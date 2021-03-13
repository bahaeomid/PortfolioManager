################################################################################################################

#Install required packages from CRAN
#ListofPackages= c('shiny','shinythemes','PerformanceAnalytics','stringr',
#                   'psych','boot','corrplot','tseries','quadprog','googleVis','quantmod','TTR')
# NewPackages= ListofPackages[!(ListofPackages %in% installed.packages()[,'Package'])]
# if(length(NewPackages)>0) install.packages(NewPackages, repos = "http://cran.rstudio.com/",dependencies = TRUE)

#Install required packages from R-forge
# ListofPackages2= c('quantmod','TTR')
# NewPackages2= ListofPackages2[!(ListofPackages2 %in% installed.packages()[,'Package'])]
# if(length(NewPackages2)>0) install.packages(NewPackages2,repos = "http://R-Forge.R-project.org")

#Load required packages
#lapply(ListofPackages,require,character.only=TRUE)

#Load source code, by Eric.Zivot, required to carry out portfolio computations
source("portfolio_noshorts.R")

#Load required libraries. Need to call seperately due to shinyapps limited capability
library('shiny')
library('shinythemes')
library('PerformanceAnalytics')
library('stringr')
library('psych')
library('boot')
library('corrplot')
library('tseries')
library('quadprog')
library('googleVis')
library('quantmod')
library('TTR')

####################################################################################################################

#Get the list of all tickers available from TTR package. used for all tickers' dropdown menus in ui.R
tickersList <- stockSymbols(quiet=TRUE)[1:2] #only first two columns containing the symbols and company
tickersList['Ticker Symbol'] <- paste(tickersList[,1],tickersList[,2],sep=' ..........') #Combine first two columns

#List of all technical indicators used in the indicators selectizeinput in ui.R
choices <- list('BBands','ROC','CLV','CMO','EMA','EMV','MACD',
                'RSI','TDI','WPR','ADX','ATR','CCI','CMF','SMA',
                'DEMA','DPO','MFI','SAR','ZigZag')

#Define a helper function to download histrical prices based on the user's input. used in server.R
downloadSym <- function(symbol, envir=parent.frame() , srcInput='yahoo', dateFrom, dateTo) {
    quiet <- lapply(symbol, function(x) {
        if (is.null(envir[[x]])) { #Download data if ticker doesn't exist in the environment
            envir[[x]] <- getSymbols(x, auto.assign = FALSE,src = srcInput,from=dateFrom,to=dateTo)
        }
        else if(abs(difftime(start(envir[[x]]),dateFrom,units='days'))>5 | abs(difftime(end(envir[[x]]),dateTo,units='days'))>5 | attr(envir[[x]],'src')!=srcInput){ #If ticker exists in the environment, Download data only if one or more of these conditions are true
            envir[[x]] <- getSymbols(x, auto.assign = FALSE,src = srcInput,from=dateFrom,to=dateTo)
        }
    })
}

#Define a helper function to process the user ticker inputs
fixTicker <- function(x){
    subInput <- sub('[\\.]{10}[a-zA-Z].*','',x)
    trimInput <- str_trim(subInput)
    upperCase <- toupper(trimInput)
    dup <- duplicated(upperCase)
    upperCase[!dup] 
}

#Define a helper function to construct the TA argument in chartSeries p1 used in server.R
addX <- function(input='Vo'){ #default is adding volume
    sapply(input,function(x) paste0('add',x,'()'))
}

#Customize ChartSeries theme globally. used in server.R
myTheme <- chartTheme("white")
myTheme$dn.col <- "pink"
myTheme$up.col <- "lightgreen"
myTheme$border <- "lightgray"

#Define a helper function to convert the original prices fetched, to daily, monthly or weekly depending on the user's input. 
Myto.period <- function(input, envir=parent.frame(),ticker){
    switch(
        input,
        'Monthly'={lapply(ticker,function(x) {prices <- to.monthly(envir[[x]]); colnames(prices) <- colnames(envir[[x]]); return(prices)})},
        'Weekly'={lapply(ticker,function(x) {prices <- to.weekly(envir[[x]]); colnames(prices) <- colnames(envir[[x]]); return(prices)})},
        'Daily'={lapply(ticker,function(x) {prices <- to.daily(envir[[x]]); colnames(prices) <- colnames(envir[[x]]); return(prices)})}
    )  
}

#Define a helper function to calculate returns based on user's inputs
Myrets <- function(origPrices,ticker ) {
    
    listofRets <- lapply(origPrices,function(x) CalculateReturns(Ad(x),method='compound')) #cc Returns based on adjusted close prices
    mergedRets <- do.call(merge,listofRets)
    colnames(mergedRets) <- ticker  #housekeeping
    return(mergedRets)
}

#Helper functions for computing bootstrap statistics in server.R
mean.boot <- function(x, idx) {mean(x[idx],na.rm=T)}
sd.boot <- function(x, idx) {sd(x[idx],na.rm=T)}
VaR.boot <- function(x, idx, p) { exp(quantile(x[idx],na.rm=T,probs = p))-1 } #Assume $1 intitial wealth
median.boot <- function(x, idx) {median(x[idx],na.rm=T)}
skew.boot <- function(x, idx) {skewness(x[idx],na.rm=T)}
kurt.boot <- function(x, idx) {kurtosis(x[idx],na.rm=T)} #Excess Kurtosis (PerformanceAnalytics)
sharpe.boot <- function(x, idx) {mean(x[idx],na.rm=T)/sd(x[idx],na.rm=T)}
rho.boot <- function(x.mat, idx) {cor(x.mat[idx,],use='pairwise.complete.obs')[1,2]} #correlation between two variables

#Define a helper function to compute bootstrap statistics
Myboot <- function(inputBoot,inputPerc,inputCor,returns,R=999){
    switch(inputBoot,
           'Mean'= apply(returns,2, function(x) boot(x,mean.boot,R)),
           'Median'= apply(returns,2, function(x) boot(x,median.boot,R)),
           'Std.Deviation'= apply(returns,2, function(x) boot(x,sd.boot,R)),
           'Skewness'= apply(returns,2, function(x) boot(x,skew.boot,R)),
           'Kurtosis'= apply(returns,2, function(x) boot(x,kurt.boot,R)),
           'Sharpe Ratio'= apply(returns,2, function(x) boot(x,sharpe.boot,R)),
           'Value-at-Risk'= apply(returns,2, function(x) boot(x,VaR.boot,p=as.numeric(inputPerc),R)),
           'Correlation' = {l <- list(boot(returns[,inputCor],rho.boot,R)); 
                            names(l) <- paste0(names(returns[,inputCor])[1],',',names(returns[,inputCor])[2]) ; l} #Storing boot result in a list, to attain consistency with other parameters being defined above
    )   
}

#Define a helper function to plot the auto-correlation function
Myacf <- function(returns){
    ret.mat <- coredata(returns)
    acf(ret.mat,lag.max=10,na.action=na.omit,main=colnames(ret.mat))
}

#Define helper function for Myroll function below
sharpe.roll <- function(x) {mean(x,na.rm=T)/sd(x,na.rm=T)}
VaR.roll <- function(x, p) {exp(quantile(x,na.rm=T,probs = p))-1 } #Assume $1 initial wealth
cor.roll <- function(x) {cor(x)[1,2]}


#Define a helper function to compute rolling statistics
Myroll <- function(inputRoll,inputPerc=NULL,inputCor=NULL,returns,inputWin,align='right'){
    switch(inputRoll,
           'Mean'= rollapply(returns,FUN=mean,width=inputWin,align=align,by.column = T),
           'Median'= rollapply(returns,FUN=median ,width=inputWin,align=align,by.column = T),
           'Std.Deviation'= rollapply(returns,FUN=sd ,width=inputWin,align=align,by.column = T) ,
           'Skewness'= rollapply(returns,FUN=skewness ,width=inputWin,align=align,by.column = T) ,
           'Kurtosis'= rollapply(returns,FUN=kurtosis ,width=inputWin,align=align,by.column = T),
           'Sharpe Ratio'= rollapply(returns,FUN=sharpe.roll ,width=inputWin,align=align,by.column = T),
           'Value-at-Risk'= rollapply(returns,FUN=VaR.roll,p=inputPerc, width=inputWin,align=align,by.column = T) ,
           'Correlation' = {
               tmp <- rollapply(returns[,inputCor],FUN=cor.roll ,width=inputWin,align=align,by.column = F)
               names(tmp) <- paste0('(',inputCor[1],',',inputCor[2],')')
               return(tmp)
           }
    )   
}

#Define a helper function to compute pairwise correlation test
Mypaircor <- function(returns,input){
    t <- cor.test(returns[,input[1]],returns[,input[2]])
    t$data.name <- paste0(input[1],', ',input[2])
    textplot(capture.output(t),halign='left')
}

#Define a helper function to compute t.test for mean
Mymeantest <- function(returns,inputAlt,inputNum){
    t <- lapply(returns, function(x) {tmp <- t.test(x,alternative =inputAlt ,mu =inputNum ); tmp$data.name <- names(x);return(tmp)})
    lapply(t, function(x) {textplot(capture.output(x),halign='left')})
}

#Define a helper function to carry out computations for hypothesis testing (only two of the options, mean and correlation)
Myhyp <- function(inputHyp,returns,inputHypCor,inputHypNum,inputHypMean){
    switch(inputHyp,
           'Mean'= Mymeantest(returns = returns,inputAlt = inputHypMean,inputNum = inputHypNum),
           'Pairwise Correlation'=Mypaircor(returns,inputHypCor)
    )  
    
}

#Define helper functions to compute expected return, std. deviation, and covariance matrix of assets (input to portfolio theory)
retEst <- function(returns){apply(returns,2,mean,na.rm=T) }
sdEst <- function(returns){apply(returns,2,sd,na.rm=T) }
covmatEst <- function(returns) { var(returns, na.rm=T)}

#Define a helper function to show asset labels on the Risk-Reward plot
Mylabels <- function(ers,sds){
    lapply(1:length(ers), function(x) text(sds[x], ers[x], labels=names(ers)[x], pos=2,cex=0.5) )
}

#Define a helper function to make a summary table for assets within portfolio's
#this function does not deal with optimal portfolio on CAL
assetsTable <- function(portsList){ #portsList has to be a named ordered list of portfolio's (Global,Tangency,Efficient)
    
    #Extract name of assets and fetch last prices from Yahoo!
    assets <- names(portsList[[1]]$weights)
    quotes <- suppressWarnings(getQuote(assets, what=yahooQF("Last Trade (Price Only)"))$Last)
    
    #if the 3rd portfolio (i.e. Efficient portfolio) is not provided, exclude its weights
    if(sum(sapply(portsList,is.null))>0) {
        weights <- data.frame(lapply(portsList[1:2],function(x) x$weights))
        df <- data.frame(assets,quotes,weights)
    }
    
    #Otherwise extract weights from all portfolio's
    else {
        weights <- data.frame(lapply(portsList,function(x) x$weights))
        df <- data.frame(assets,quotes,weights)
    }
    
    #if the 3rd portfolio (i.e. Efficient portfolio) is not provided, exclude its name from column names
    if(sum(sapply(portsList,is.null))>0) {
        names(df)[1:4] <- c('Asset','Last Price','Global Min. Port. Weights','Tangency Port. Weights') 
    }
    
    #Otherwise name all columns
    else {
        names(df)[1:5] <- c('Asset','Last Price','Global Min. Port. Weights','Tangency Port. Weights','Efficient Port. Weights') 
        
    }
    
    #Housekeeping
    rownames(df) <- NULL
    
    return(df)
}

#Define a helper function to compute weights for the optimal portfolio on CAL
optimalW <- function(assetstable, CALweights){
    tanW <- assetstable[c(1,7)]   #extract tangency portfolio weights from assetsTable
    tanW[,2] <- tanW[,2]*CALweights['Tangency Portfolio']
    tanW[,1] <- as.character(tanW[,1])  #housekeeping to allow rbind in the next step
    tanW <- rbind(tanW,c('Risk-free Asset',CALweights['Risk-free Asset']))
    tanW[,2] <- as.numeric(tanW[,2]) #housekeeping
    names(tanW)[2] <- 'Optimal Port. Weights'
    return(tanW)
}

#Define a helper function to construct a summary table for portfolio performance
portsTable <- function(portsList, rf){   #portsList is an ordered list of portfolio's (Global, Tangency, Efficient/Optimal)
    
    ports <- portsList
    
    #If 3rd portfolio is not provided, do computations for the first 2 portfolio's only
    if(sum(sapply(ports,is.null))>0) {
        ports <- ports[-3]
        names(ports) <- c('Global Min','Tangency') #housekeeping
        means <- sapply(ports,function(x) x$er)
        g.means <- exp(means)-1
        sds <- sapply(ports,function(x) x$sd)
        sharpes <- (means-rf)/sds
        probs <- c(0.01,0.05)
        VaRs <-data.frame(sapply(probs,function(x) means+sds*qnorm(x))) #assuming normal distribution and $1 initial wealth
        names(VaRs) <- c('1% VaR','5% VaR')
        df <- data.frame(ER=means,Geom.ER=g.means,SD=sds,Sharpe=sharpes,VaRs,check.names=F)
    }
    #otherwise do computations for all 3 portfolio's
    else{
        names(ports) <- c('Global Min','Tangency','placeholder') #housekeeping
        means <- sapply(ports,function(x) x$er)
        g.means <- exp(means)-1
        sds <- sapply(ports,function(x) x$sd)
        sharpes <- (means-rf)/sds
        probs <- c(0.01,0.05)
        VaRs <-data.frame(sapply(probs,function(x) means+sds*qnorm(x)))   #assuming normal distribution and $1 initial wealth
        names(VaRs) <- c('1% VaR','5% VaR')
        df <- data.frame(ER=means,Geom.ER=g.means,SD=sds,Sharpe=sharpes,VaRs,check.names=F) 
    }
    
    #Housekeeping
    df <- cbind(Portfolio=rownames(df),df)
    df[,1]<- as.character(df[,1])
    rownames(df) <- NULL 
    return(df)
}

#Define a helper function to annualize the portfolio performance
Myannual <- function(portstable,rf,inputPrices,marketRets){
    freq <- switch(inputPrices,
                   'Monthly'=12, 
                   'Weekly'=52,
                   'Daily'=252)  
    
    rf.a <- freq*rf
    means.a <- freq*(portstable$ER)
    g.means.a <- exp(means.a)-1
    sd.a <- sqrt(freq)*portstable$SD
    sharpe.a <- (means.a-rf.a)/sd.a
    var1.a <- means.a+sd.a*qnorm(0.01)
    var5.a <- means.a+sd.a*qnorm(0.05)
    portstable[2:6] <- data.frame(means.a,g.means.a,sd.a,var1.a,var5.a)
    portstable[12] <- sharpe.a
    alpha.a <- freq*portstable$alpha
    portstable[7] <- alpha.a
    betaSE.a <- sqrt(freq)*portstable$SE.beta
    portstable[9] <- betaSE.a
    sigma.e.a <- sqrt(freq)*portstable$Sigma.e
    portstable[11] <- sigma.e.a
    treynor.a <- (means.a-rf.a)/portstable$beta
    portstable[14] <- treynor.a
    mktER.a <- freq*mean(marketRets,na.rm=T)  #market return annualized
    mktSD.a <- sqrt(freq)*sd(marketRets,na.rm=T) #market sd annualized
    m2.a <- ((means.a - rf.a)*(mktSD.a/sd.a))-(mktER.a-rf.a)
    portstable[15] <- m2.a
    mktSharpe.a <- (mktER.a-rf.a)/mktSD.a
    portstable[13] <- mktSharpe.a
    
    
    return(portstable)
}

#Define helper function to compute global min portfolio bootstrap statistics
gmin.port.boot <- function(returns, n.boot=999, inputShorts){
    
    ret.mat <- returns
    n.obs = nrow(ret.mat)
    mu.gmin.boot = matrix(0, n.boot, 1) #to store values
    sd.gmin.boot = matrix(0, n.boot, 1) #to store values
    w.gmin.boot = matrix(0, n.boot, ncol(ret.mat)) #to store values
    colnames(mu.gmin.boot) =  "Global Min. mean"
    colnames(sd.gmin.boot) = "Global Min. sd"
    colnames(w.gmin.boot) = colnames(ret.mat)
    
    for (i in 1:n.boot) {
        boot.idx = sample(n.obs, replace=TRUE)
        ret.boot = ret.mat[boot.idx, ] 
        mu.boot = colMeans(ret.boot, na.rm=T)
        cov.boot = var(ret.boot,na.rm=T) 
        gmin.boot = globalMin.portfolio(mu.boot, cov.boot,shorts = inputShorts)
        mu.gmin.boot[i, ] = gmin.boot$er
        sd.gmin.boot[i, ] = gmin.boot$sd
        w.gmin.boot[i, ] = gmin.boot$weights
    }
    output <- list(mu.gmin.boot, sd.gmin.boot,w.gmin.boot)
    return(output)
}

#Define helper function to compute efficient portfolio bootstrap statistics
effport.boot <- function(returns, n.boot=999, inputTarget,inputShorts){
    
    ret.mat <- returns
    n.obs = nrow(ret.mat)
    mu.eff.boot = matrix(0, n.boot, 1) #to store values
    sd.eff.boot = matrix(0, n.boot, 1) #to store values
    w.eff.boot = matrix(0, n.boot, ncol(ret.mat)) #to store values
    colnames(mu.eff.boot) =  "Efficient Port. mean"
    colnames(sd.eff.boot) = "Efficient Port.sd"
    colnames(w.eff.boot) = colnames(ret.mat)
    
    for (i in 1:n.boot) {
        boot.idx = sample(n.obs, replace=TRUE)
        ret.boot = ret.mat[boot.idx, ] 
        mu.boot = colMeans(ret.boot, na.rm=T)
        cov.boot = var(ret.boot,na.rm=T) 
        eff.boot = efficient.portfolio(mu.boot,cov.boot,target.return = inputTarget,shorts = inputShorts)
        mu.eff.boot[i, ] = eff.boot$er
        sd.eff.boot[i, ] = eff.boot$sd
        w.eff.boot[i, ] = eff.boot$weights
    }
    output <- list(mu.eff.boot, sd.eff.boot,w.eff.boot)
    return(output)
}

#Define a helper function to bootstrap efficient frontier
frontier.boot <- function(returns, n.boot=999, inputShorts){
    
    ret.mat <- returns
    n.obs = nrow(ret.mat)
    ef.list = list()
    
    for (i in 1:n.boot) {
        boot.idx = sample(n.obs, replace=TRUE)
        ret.boot = ret.mat[boot.idx, ] 
        mu.boot = colMeans(ret.boot,na.rm=T)
        cov.boot = var(ret.boot, na.rm=T) 
        ef.boot = efficient.frontier(mu.boot, cov.boot,alpha.min = 0,alpha.max = 1,nport = 10,shorts = inputShorts)
        ef.list[[i]] = ef.boot
    }
    
    return(ef.list)
    
}

#Define a helper function for global min portfolio rolling statistics
rollGmin <- function(returns, inputShorts) {
    mu.hat = colMeans(returns, na.rm=T)
    cov.hat = var(returns, na.rm=T)
    gmin = globalMin.portfolio(er=mu.hat,cov.mat=cov.hat,shorts = inputShorts)
    output = c(gmin$er,gmin$sd,gmin$weights)
    names(output)[1:2] = c("er","sd")
    return(output)
}

#Compute the rolling global min portfolio statistics using rollGmin helper function defined above
Myroll.gmin <- function(returns,inputWin, inputShorts){
    rollapply(returns,width=inputWin, by.column=F,align="right", FUN=rollGmin,inputShorts=inputShorts )   
}

#Define a helper function for efficient portfolio rolling statistics
rollEff = function(returns,inputTarget, inputShorts) {
    mu.hat = colMeans(returns,na.rm=T)
    cov.hat = var(returns, na.rm=T)
    eport = efficient.portfolio(er=mu.hat,cov.mat=cov.hat, target.return=inputTarget,shorts=inputShorts)
    output = c(eport$er,eport$sd,eport$weights)
    names(output)[1:2] = c("er","sd")
    return(output)
}

#Compute the rolling efficient portfolio statistics using rollEff function helper defined above
Myroll.eff <- function(returns,inputWin,inputTarget,inputShorts){
    rollapply(returns,width=inputWin,by.column=F,align="right", FUN=rollEff,inputTarget=inputTarget,inputShorts=inputShorts)  
}

#Define a helper function to provide bootstrap summary for brute force bootstrap
Mybootsummary <- function(portOrig=NULL,portBoot=NULL,inputBootPort, RegBoot,Fit,Fit.tstat=NULL,assets=T,global=T,eff=T,tan=T,opt=T){
    
    orig <- switch(inputBootPort,
                   '1'='er',
                   '2'='sd',
                   '3'='weights',
                   '4'='alpha',
                   '5'='beta',
                   '6'='R2',
                   '7'='alpha.tstat'
    )
    if(inputBootPort %in% c(1,2)){
        bootOut <- portBoot[[inputBootPort]]
        bias = mean(bootOut,na.rm=T) - portOrig[[orig]]
        se = sd(bootOut,na.rm=T)
        ci = c(portOrig[[orig]]-2*se, portOrig[[orig]]+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=portOrig[[orig]],Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==3){
        bootOut <- portBoot[[inputBootPort]]
        bias = colMeans(bootOut,na.rm=T) - portOrig[[orig]]
        se = apply(bootOut, 2, sd,na.rm=T)
        ci = rbind(portOrig[[orig]]-2*se,portOrig[[orig]]+2*se)
        rownames(ci) = c('Lower 95%','Upper 95%')
        output <- do.call(rbind,list(Original=portOrig[[orig]],Bias=bias,SE=se,ci))
        output <- round(output,5)
    }
    else if(inputBootPort==4 & global & !eff & !assets & !tan & !opt){
        bootOut <- RegBoot$alpha[,'Global Min']
        orig.alpha <- subset(Fit[,'alpha'],Fit$Portfolio=='Global Min')
        bias = mean(bootOut,na.rm=T) - orig.alpha
        se = sd(bootOut,na.rm=T)
        ci = c(orig.alpha-2*se, orig.alpha+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.alpha,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==4 & eff & !global & !assets & !tan & !opt){
        bootOut <- RegBoot$alpha[,'Efficient']
        orig.alpha <- subset(Fit[,'alpha'],Fit$Portfolio=='Efficient')
        bias = mean(bootOut,na.rm=T) - orig.alpha
        se = sd(bootOut,na.rm=T)
        ci = c(orig.alpha-2*se, orig.alpha+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.alpha,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==5 & global & !eff & !assets & !tan & !opt){
        bootOut <- RegBoot$beta[,'Global Min']
        orig.beta <- subset(Fit[,'beta'],Fit$Portfolio=='Global Min')
        bias = mean(bootOut,na.rm=T) - orig.beta
        se = sd(bootOut,na.rm=T)
        ci = c(orig.beta-2*se, orig.beta+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.beta,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==5 & eff & !global & !assets & !tan & !opt){
        bootOut <- RegBoot$beta[,'Efficient']
        orig.beta <- subset(Fit[,'beta'],Fit$Portfolio=='Efficient')
        bias = mean(bootOut,na.rm=T) - orig.beta
        se = sd(bootOut,na.rm=T)
        ci = c(orig.beta-2*se, orig.beta+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.beta,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==6 & global & !eff & !assets & !tan & !opt){
        bootOut <- RegBoot$R2[,'Global Min']
        orig.R2 <- subset(Fit[,'R2'],Fit$Portfolio=='Global Min')
        bias = mean(bootOut,na.rm=T) - orig.R2
        se = sd(bootOut,na.rm=T)
        ci = c(orig.R2-2*se, orig.R2+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.R2,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==6 & eff & !global & !assets & !tan & !opt){
        bootOut <- RegBoot$R2[,'Efficient']
        orig.R2 <- subset(Fit[,'R2'],Fit$Portfolio=='Efficient')
        bias = mean(bootOut,na.rm=T) - orig.R2
        se = sd(bootOut,na.rm=T)
        ci = c(orig.R2-2*se, orig.R2+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.R2,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==7 & global & !eff & !assets & !tan & !opt){
        bootOut <- RegBoot$alpha.tstat[,'Global Min']
        orig.tstat <- subset(Fit.tstat[,'alpha.tstat'],Fit.tstat$Portfolio=='Global Min')
        bias = mean(bootOut,na.rm=T) - orig.tstat
        se = sd(bootOut,na.rm=T)
        ci = c(orig.tstat-2*se, orig.tstat+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.tstat,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==7 & eff & !global & !assets & !tan & !opt){
        bootOut <- RegBoot$alpha.tstat[,'Efficient']
        orig.tstat <- subset(Fit.tstat[,'alpha.tstat'],Fit.tstat$Portfolio=='Efficient')
        bias = mean(bootOut,na.rm=T) - orig.tstat
        se = sd(bootOut,na.rm=T)
        ci = c(orig.tstat-2*se, orig.tstat+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.tstat,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    
    else if(inputBootPort==4 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$alpha[,'Tangency']
        orig.alpha <- subset(Fit[,'alpha'],Fit$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.alpha
        se = sd(bootOut,na.rm=T)
        ci = c(orig.alpha-2*se, orig.alpha+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.alpha,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==4 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$alpha[,'Tangency']
        orig.alpha <- subset(Fit[,'alpha'],Fit$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.alpha
        se = sd(bootOut,na.rm=T)
        ci = c(orig.alpha-2*se, orig.alpha+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.alpha,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==5 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$beta[,'Tangency']
        orig.beta <- subset(Fit[,'beta'],Fit$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.beta
        se = sd(bootOut,na.rm=T)
        ci = c(orig.beta-2*se, orig.beta+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.beta,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==5 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$beta[,'Tangency']
        orig.beta <- subset(Fit[,'beta'],Fit$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.beta
        se = sd(bootOut,na.rm=T)
        ci = c(orig.beta-2*se, orig.beta+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.beta,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==6 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$R2[,'Tangency']
        orig.R2 <- subset(Fit[,'R2'],Fit$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.R2
        se = sd(bootOut,na.rm=T)
        ci = c(orig.R2-2*se, orig.R2+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.R2,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==6 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$R2[,'Tangency']
        orig.R2 <- subset(Fit[,'R2'],Fit$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.R2
        se = sd(bootOut,na.rm=T)
        ci = c(orig.R2-2*se, orig.R2+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.R2,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==7 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$alpha.tstat[,'Tangency']
        orig.tstat <- subset(Fit.tstat[,'alpha.tstat'],Fit.tstat$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.tstat
        se = sd(bootOut,na.rm=T)
        ci = c(orig.tstat-2*se, orig.tstat+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.tstat,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==7 & tan & !assets & !global & !eff & !opt){
        bootOut <- RegBoot$alpha.tstat[,'Tangency']
        orig.tstat <- subset(Fit.tstat[,'alpha.tstat'],Fit.tstat$Portfolio=='Tangency')
        bias = mean(bootOut,na.rm=T) - orig.tstat
        se = sd(bootOut,na.rm=T)
        ci = c(orig.tstat-2*se, orig.tstat+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.tstat,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==4 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$alpha[,'Optimal']
        orig.alpha <- subset(Fit[,'alpha'],Fit$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.alpha
        se = sd(bootOut,na.rm=T)
        ci = c(orig.alpha-2*se, orig.alpha+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.alpha,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==4 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$alpha[,'Optimal']
        orig.alpha <- subset(Fit[,'alpha'],Fit$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.alpha
        se = sd(bootOut,na.rm=T)
        ci = c(orig.alpha-2*se, orig.alpha+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.alpha,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==5 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$beta[,'Optimal']
        orig.beta <- subset(Fit[,'beta'],Fit$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.beta
        se = sd(bootOut,na.rm=T)
        ci = c(orig.beta-2*se, orig.beta+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.beta,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==5 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$beta[,'Optimal']
        orig.beta <- subset(Fit[,'beta'],Fit$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.beta
        se = sd(bootOut,na.rm=T)
        ci = c(orig.beta-2*se, orig.beta+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.beta,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==6 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$R2[,'Optimal']
        orig.R2 <- subset(Fit[,'R2'],Fit$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.R2
        se = sd(bootOut,na.rm=T)
        ci = c(orig.R2-2*se, orig.R2+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.R2,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==6 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$R2[,'Optimal']
        orig.R2 <- subset(Fit[,'R2'],Fit$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.R2
        se = sd(bootOut,na.rm=T)
        ci = c(orig.R2-2*se, orig.R2+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.R2,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==7 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$alpha.tstat[,'Optimal']
        orig.tstat <- subset(Fit.tstat[,'alpha.tstat'],Fit.tstat$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.tstat
        se = sd(bootOut,na.rm=T)
        ci = c(orig.tstat-2*se, orig.tstat+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.tstat,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==7 & opt & !tan & !assets & !global & !eff){
        bootOut <- RegBoot$alpha.tstat[,'Optimal']
        orig.tstat <- subset(Fit.tstat[,'alpha.tstat'],Fit.tstat$Portfolio=='Optimal')
        bias = mean(bootOut,na.rm=T) - orig.tstat
        se = sd(bootOut,na.rm=T)
        ci = c(orig.tstat-2*se, orig.tstat+2*se)
        names(ci) <- c('Lower 95%','Upper 95%')
        output <- c(Original=orig.tstat,Bias=bias,SE=se,ci)
        output <- round(output,5)
    }
    else if(inputBootPort==4 & assets & !tan & !global & !eff & !opt){
        bootOut <- RegBoot$alpha
        bias = colMeans(bootOut,na.rm=T) - Fit$alpha
        se = apply(bootOut, 2, sd,na.rm=T)
        ci = rbind(Fit$alpha-2*se,Fit$alpha+2*se)
        rownames(ci) = c('Lower 95%','Upper 95%')
        output <- do.call(rbind,list(Original=Fit$alpha,Bias=bias,SE=se,ci))
        output <- round(output,5)
    }
    else if(inputBootPort==5 & assets & !tan & !global & !eff & !opt){
        bootOut <- RegBoot$beta
        bias = colMeans(bootOut,na.rm=T) - Fit$beta
        se = apply(bootOut, 2, sd,na.rm=T)
        ci = rbind(Fit$beta-2*se,Fit$beta+2*se)
        rownames(ci) = c('Lower 95%','Upper 95%')
        output <- do.call(rbind,list(Original=Fit$beta,Bias=bias,SE=se,ci))
        output <- round(output,5)
    }
    else if(inputBootPort==6 & assets & !tan & !global & !eff & !opt){
        bootOut <- RegBoot$R2
        bias = colMeans(bootOut,na.rm=T) - Fit$R2
        se = apply(bootOut, 2, sd,na.rm=T)
        ci = rbind(Fit$R2-2*se,Fit$R2+2*se)
        rownames(ci) = c('Lower 95%','Upper 95%')
        output <- do.call(rbind,list(Original=Fit$R2,Bias=bias,SE=se,ci))
        output <- round(output,5)
    }
    else if(inputBootPort==7 & assets & !tan & !global & !eff & !opt){
        bootOut <- RegBoot$alpha.tstat
        bias = colMeans(bootOut,na.rm=T) - Fit.tstat$alpha.tstat
        se = apply(bootOut, 2, sd,na.rm=T)
        ci = rbind(Fit.tstat$alpha.tstat-2*se,Fit.tstat$alpha.tstat+2*se)
        rownames(ci) = c('Lower 95%','Upper 95%')
        output <- do.call(rbind,list(Original=Fit.tstat$alpha.tstat,Bias=bias,SE=se,ci))
        output <- round(output,5)
    }
    
    return(output)
}

#Define a helper function to perform least squares estimation for SIM
Mylsfit <- function(assetRets, marketRets,rf){
    #make numbr of rows equal and then remove NA's
    tmp <- merge(assetRets,marketRets)-rf  #excess returns
    tmp <- tmp[complete.cases(tmp),]   #housekeeping
    
    assetsR <- tmp[,-ncol(tmp)]
    names(assetsR) <- names(assetRets)   #housekeeping
    mktRets <- tmp[,ncol(tmp)]
    names(mktRets) <- names(marketRets)    #housekeeping
    
    list.master <- lapply(assetsR,function(x){
        fit <- lm(x~mktRets)
        summ <- summary(fit)
        t.stats <- coef(summ)[1,3]  #t-value for alpha
        names(t.stats) <- 'alpha.tstat'
        coeffs <- fit$coefficients
        names(coeffs) <- c('alpha','beta')
        resids <- fit$residuals
        r2 <- summ$r.squared
        sigma <- summ$sigma
        se.beta <- summ$coefficients[2,2]
        tmp= c(coeffs,SE.beta=se.beta,R2=r2,Sigma.e=sigma)
        list <- list(summary=round(tmp,4),residuals=resids,tstats=t.stats)
    })
    
    #store as dataframes
    summary.df <- data.frame(lapply(list.master,function(x) x$summary),check.names = F)
    resids.df <- data.frame(lapply(list.master,function(x) x$residuals),check.names = F)
    tstats.df <- data.frame(lapply(list.master,function(x) x$tstats),check.names = F)
    
    #store dataframes in a list
    list.output <- list(summary=summary.df,residuals=resids.df,tstats=tstats.df)
    
    return(list.output)
    
}

#helper function to compute covariances via SIM assumption
SIMcovmat <- function(beta.df,fitResids,marketRets){
    
    sig2.market <- as.numeric(var(marketRets,na.rm=T))
    beta.m <- as.matrix(beta.df)
    crossprod <- t(beta.m) %*% beta.m
    residVars <- apply(fitResids,2,var,na.rm=T)
    D.mat <- diag(residVars)
    cov.market <- sig2.market*crossprod
    cov.si <- cov.market + D.mat
    
    return(cov.si)
}

#helper function to extract summary components of lsfit 
lsAssetsT <- function(lsfit.summary.df){
    df <- data.frame((t(lsfit.summary.df)))
    df <- cbind(Asset=rownames(df),df)
    rownames(df) <- NULL
    return(df)
}

#helper function to calculate portfolio period returns to estimate CAPM parameters later
portRets <- function(assetstable,optimalweights,returns,rf){
    
    #extract rf weight from optimalweights dataframe
    if(!is.null(optimalweights)) {
        #extract weight of the risk-free asset
        xf <- optimalweights[nrow(optimalweights),2] 
        
        #merge weights into one dataframe
        merged <- merge.data.frame(assetstable,optimalweights,sort=F)[-c(2:5)]
    }
    else {
        merged <- assetstable [-c(2:5)]
    }
    
    #calculate portfolio period returns based on the weights 
    rets <- data.frame(lapply(merged[-1],function(x) {
        data.frame(sapply(1:nrow(returns),function(y){x %*% t(returns[y,]) }) )
    }))
    
    if(length(merged)>4){
        #add the risk-free returns to the optimal portfolio
        rets[,4]<- rets[,4]+ xf*rf
        #housekeeping
        names(rets) <- c('Global Min','Tangency','Efficient','Optimal')
    }
    else if(length(merged)>3 & !is.null(optimalweights)){
        #add the risk-free returns to the optimal portfolio
        rets[,3]<- rets[,3]+ xf*rf
        #housekeeping
        names(rets) <- c('Global Min','Tangency','Optimal')
    }
    else if(length(merged)>3 & is.null(optimalweights)){
        #housekeeping
        names(rets) <- c('Global Min','Tangency','Efficient')
    }
    else{
        #housekeeping
        names(rets) <- c('Global Min','Tangency')
    }
    
    #zoo object
    rets.z <- xts(rets,index(returns))
    
    return(rets.z)
}

#helper function to create a summary table for additional ratio's for porftolio performance, based on least square fit on portRets 
lsPortsT <- function(lsfit.summary.df){
    df <- data.frame((t(lsfit.summary.df)))
    df <- cbind(Portfolio=rownames(df),df)
    df[,1] <- as.character(df[,1])
    rownames(df) <- NULL
    
    return(df)
}

#Define helper function to compute assets and portfolio bootstrap regression parameters (alpha,beta,R2)
reg.boot <- function(assetRets, marketRets, n.boot=999, rf){
    
    #make numbr of rows equal and then remove NA's
    tmp <- merge(assetRets,marketRets)-rf  #excess returns
    tmp <- tmp[complete.cases(tmp),]   #housekeeping
    
    assetsR <- tmp[,-ncol(tmp)]
    names(assetsR) <- names(assetRets)   #housekeeping
    mktRets <- tmp[,ncol(tmp)]
    names(mktRets) <- names(marketRets)    #housekeeping
    
    n.obs = nrow(assetsR)
    alpha.boot = matrix(0, n.boot, ncol(assetsR)) #to store alpha values
    beta.boot = matrix(0, n.boot, ncol(assetsR)) #to store beta values
    R2.boot = matrix(0, n.boot, ncol(assetsR)) #to store R2 values
    alpha.tstat.boot = matrix(0, n.boot, ncol(assetsR)) # to store alpha t-stat values
    colnames(alpha.boot) =  colnames(assetsR) #housekeeping
    colnames(beta.boot) = colnames(assetsR)  #housekeeping
    colnames(R2.boot) = colnames(assetsR)    #housekeeping
    colnames(alpha.tstat.boot) = colnames(assetsR)    #housekeeping
    
    for (i in 1:n.boot) {
        
        boot.idx = sample(n.obs, replace=TRUE)  #Create a random set of indices 
        assetsR.boot = assetsR[boot.idx, ]  #Reshuffle original asset returns
        mktRets.boot = mktRets[boot.idx, ]   #Reshuffle original market returns
        
        #Create a list to store
        list.master <- lapply(assetsR.boot,function(x){
            fit <- lm(x~mktRets.boot)
            summ <- summary(fit)
            coeffs <- fit$coefficients
            names(coeffs) <- c('alpha','beta')
            r2 <- summ$r.squared
            alpha.tstat <- coef(summ)[1,3]
            tmp <- c(coeffs,R2=r2,alpha.tstat)
        })
        
        #Convert list.master to a dataframe and then to a matrix
        tmp.df <- data.frame(lapply(list.master,function(x) x),check.names = F)
        tmp.mat <- as.matrix(tmp.df)
        
        #Store bootstrap results into the matrices defined above
        alpha.boot[i, ] = tmp.mat[1,]
        beta.boot[i, ] = tmp.mat[2,]
        R2.boot[i, ] = tmp.mat[3,]
        alpha.tstat.boot[i,] = tmp.mat[4,]
    }
    
    #Store all bootstrap results in a single list
    output <- list(alpha=alpha.boot, beta=beta.boot,R2=R2.boot,alpha.tstat=alpha.tstat.boot)
    return(output)
}

#Helper function to compute rolling regression parameters for returns
roll.reg <- function(rets,mktrets,rf,param=c('Alpha','Beta','R-Squared'), inputWin){
    for (i in param){
        chart.RollingRegression(Ra = rets,Rb = mktrets,width = inputWin ,Rf = rf,attribute = i,legend.loc='topleft',auto.grid=F,na.pad=F)
    }
}

