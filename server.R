#Load all common non-reactive functions via global.R
source('global.R',local=TRUE)

shinyServer(function(input,output,session){
    
    ################1st Tab Computations BEGIN HERE##########################################
    ########Historical Prices#########
    
    #use fixTicker function from global.R to process the 1st ticker entered by the user (i.e symInput)
    ticker <- reactive({
        fixTicker(input$symInput)
    })
    
    #Downloading histrical prices, based on symInput input, and store it in myEnv environment. 
    #If invalid ticker is entered, spit out an error using tryCatch
    #downloadSym from global.R is used to facilitate downloading the prices
    myEnv <- new.env() #Define a new environment
    syms <- reactive({
        suppressWarnings(
            tryCatch(downloadSym(ticker(),envir = myEnv,srcInput =input$src,dateFrom = input$dfrom,dateTo = input$dto)
                     , error=function(e) 'error')
        )
    })
    
    #Calculate prices for ticker() based on Prices input (per0). the default prices fetched are daily
    #Myto.period helper function is used from global.R   
    to.period0 <- reactive({
        if(syms()=='error') validate(need(syms()!='error','Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.')) #to provoke updating the data
        else {
            Myto.period(input$per0,myEnv,ticker()) 
        }
    })
    
    #Construct TA argument for chartSeries in p1 below.
    #addX helper function is used from global.R
    ta <- reactive({
        if(input$v==0) {volume <- NULL} else {volume <- addX()}  #Volume checkbox input for chartSeries in p1
        if(input$indicators=='') {tech <- NULL} else {tech <- addX(input$indicators)}  #Technical indicator input for chartSeries in p1
        
        #dealing with different scenarios
        if(is.null(volume) & is.null(tech)) 'NULL'
        else if(is.null(tech)) volume
        else if(is.null(volume)) tech
        else paste0(tech,';',volume)
        
    })
    
    #type option for chartSeries in p1 below. 
    #Gives the user the option to display the chart in different formats (line, point, etc)
    type <- reactive({
        input$type
    })
    
    #Send p1, historical prices, to ui.R
    output$p1 <- renderPlot({
        if(input$a1==0) return() #Chart's displayed when button is clicked only. if not clicked, do nothing.
        else{
            isolate({
                
                if(syms()=='error') validate(need(syms()!='error','Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.')) #Call syms() to fetch prices. also validate input
                else chartSeries(to.period0()[[1]],theme = myTheme,type=type(), TA = ta(),name = paste0(ticker(),' ',input$per0,' Prices')) #using reactive functions defined earlier to construct the plot
                
                
            })
            
        }
        
        
    })
    
    #Let user export data in csv format
    output$exp1 = downloadHandler(
        filename = 'export.csv',
        content = function(file) {
            write.csv(to.period0()[[1]],file,row.names=index(to.period0()[[1]]))
        })
    
    ########Historical Returns#########
    
    #By default, under per input (Prices) in the 1st tab, populate the previously selected input in per0
    #also for date inputs dfrom1 and dto1, populate the previously selected dates under dfrom and dto
    observe({updateSelectInput(session,'per',selected=input$per0)})   
    observe({updateDateInput(session,'dfrom1',value=input$dfrom)})
    observe({updateDateInput(session,'dto1',value=input$dto)})
    
    #Process the new tickers entered by the user(based on addNew input). 
    #this function deals with the additional tickers entered by the user
    #fixTicker helper function from global.R is used
    newTicker <- reactive({
        fixTicker(input$addNew) 
    })
    
    #Downloading histrical prices, based on addNew input, and store it in myEnv environment. 
    #If invalid ticker is entered, spit out an error using tryCatch
    #downloadSym from global.R is used to facilitate downloading the prices
    newSyms <- reactive({
        suppressWarnings(
            tryCatch(downloadSym(newTicker(),envir = myEnv,dateFrom = input$dfrom1,dateTo = input$dto1)
                     , error=function(e) 'error')
        )
    })
    
    #Calculate prices for newTicker() based on Prices input (per). the default prices fetched are daily
    #Myto.period helper function is used from global.R 
    newTo.period <- reactive({
        if('error' %in% newSyms()) validate(need(!('error' %in% newSyms()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.')) #to provoke updating the data
        else {
            Myto.period(input$per,myEnv,newTicker())
        }
        
    })
    
    #Calculate Returns for charts p2 and p3 below, based on user's inputs.
    #Myrets helper function used from global.R
    newRets <- reactive({
        if('error' %in% newSyms()) validate(need(!('error' %in% newSyms()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.'))
        else {
            Myrets(newTo.period(),newTicker())
        }
    })
    
    #Send p2 to ui.R when a2 button is clicked
    output$p2 <- renderPlot({
        
        if(input$a2>0) isolate({  #triggered when button is clicked only
            
            chart.TimeSeries(newRets(),main=paste0(input$per,' Continuously Compounded Returns'),legend.loc='topleft',type=input$type2)
            #for a single ticker, plot mean and sd lines
            if(ncol(newRets())==1) {
                abline(h=c(mean(newRets(),na.rm=T),mean(newRets(),na.rm=T)+sd(newRets(),na.rm=T),mean(newRets(),na.rm=T)-sd(newRets(),na.rm=T)),col=c('red','blue','blue'),lty=2,lwd=c(2,1,1))
                legend(x='bottomright',legend=c('mean','mean+/-sd'),bty='n',lty='dashed',col=c('red','blue'))
            }
        })
        
    })
    
    #Send p3 to ui.R
    output$p3 <- renderPlot({
        
        if(input$a4>0) isolate({    #triggered when button is clicked only
            
            chart.CumReturns(exp(newRets())-1 ,wealth.index = T, lwd=2, main='Growth of $1',legend.loc='topleft') #Note that cc returns are convereted to simple returns for this plot to make sense
        })
        
    })
    
    output$p3.1 <- renderPlot({
        
        if(input$a3.1>0) isolate({    #triggered when button is clicked only
            
            chart.Drawdown(exp(newRets())-1 ,lwd=2, main='Drawdowns',legend.loc='bottomleft') #Note that cc returns are convereted to simple returns for this plot to make sense
        })
        
    })
    
    output$p3.2 <- renderPlot({
        
        if(input$a3.2>0) isolate({    #triggered when button is clicked only
            chart.Boxplot(newRets()) 
        })
        
    })
    
    ################1st Tab Computations END HERE##########################################    
    
    ################2nd Tab Computations BEGIN HERE##########################################    
    
    #By default, under addNew2 input in the 2nd Tab, pick the tickers previously selected under addNew inputs in the 1st Tab
    #Similarly populate the Dates, Prices, and Return (i.e. dfrom2, dto2, and per2) previously selected in the 1st tab
    observe({updateSelectizeInput(session,'addNew2',selected=input$addNew,server=T)})
    observe({updateDateInput(session,'dfrom2',value=input$dfrom1)})
    observe({updateDateInput(session,'dto2',value=input$dto1)})
    observe({updateSelectInput(session,'per2',selected=input$per)})
    
    ##use fixTicker function from global.R to process the tickers entered by the user in the 2nd tab (i.e addNew2). 
    #Identical to newTicker() in 1st tab calculations above
    newTicker2 <- reactive({
        fixTicker(input$addNew2)
    })
    
    #Fetch histrical prices from Yahoo! for tickers in newTicker2() .
    #Identical to newSyms() in 1st tab calculations above
    newSyms2 <- reactive({
        suppressWarnings(        
            tryCatch(downloadSym(newTicker2(),envir = myEnv,dateFrom = input$dfrom2,dateTo = input$dto2)
                     , error=function(e) 'error')
        )
    })
    
    #Calculate prices for newTicker2() based on Prices input (per2). the default prices fetched are daily
    #Identical to to.period() in 1st tab calculations above
    to.period2 <- reactive({
        if('error' %in% newSyms2()) validate(need(!('error' %in% newSyms2()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.')) #to provoke updating the data
        else {
            Myto.period(input$per2,myEnv,newTicker2())
        }
    })
    
    #Calculate Returns to calculate basic statistics later 
    #Identical to newRets() in 1st tab calculations above
    newRets2 <- reactive({
        if('error' %in% newSyms2()) validate(need(!('error' %in% newSyms2()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.'))
        else {
            Myrets(to.period2(),newTicker2())
        }
    })
    
    #Compute sample VaR based on perc2 inputs
    VaRs <- reactive({
        
        quants <- apply(newRets2(),2,function(x) quantile(x,na.rm=T,probs = as.numeric(input$perc2)))  #Calculating quantile from historical returns distribution (should be close to qnorm if actual distribution is normal)
        
        VaR <- exp(quants)-1 #Calculate VaR for cc returns (assume $1 initial wealth)
        
        if (length(input$perc2)>1) return(t(VaR))    #Housekeeping
        else return(data.frame(VaR))
        
    })
    
    
    #Compute basic sample statistics for all tickers entered (based on addNew2 input)
    basic <- reactive({
        summary <- describe(newRets2(),skew=F)[-c(1,6,7,11)] #Remove unnecessary columns (vars,trimmed,mad,se)
        geom.mean <- t(mean.geometric(exp(newRets2())-1))  #Compute the geometic mean for the equivalent simple returns. this number should be close to the airthmetic mean
        skew.PerAn <- t(skewness(newRets2())) #Use PerformanceAnalytics package to calculate skeweness 
        kurt.PerAn <- t(kurtosis(newRets2())) #Use PerformanceAnalytics package to calculate excess kurtosis
        sharpe.PerAn <- data.frame(SharpeRatio(newRets2())[1,])   #Use PerformanceAnalytics package to calculate Sharpe Ratio. Only extract first row which contains the sharpe ratio based on standard deviation
        merged <- do.call(cbind,list(summary, geom.mean,skew.PerAn, kurt.PerAn, sharpe.PerAn)) #Bind geom.mean, skew, kurt, and sharpe variables above to summary
        names(merged)[8:11] <- c('geometric mean','skew','excess kurtosis','sharpe')   #Housekeeping 
        merged <- merged[c(1,2,8,3:7,9:11)] #Housekeeping (rearrange columns)
        merged <- if(nrow(VaRs())==0) merged else cbind(merged,VaRs()) #Bind VaRs() computed earlier to the merged variable above
        if (length(merged)==12) {names(merged)[12]<-paste0(as.numeric(input$perc2)*100,'%')}     #Housekeeping
        return(merged)
    })
    
    #Send t1, table of basic sample statistics for all tickers entered to ui.R (based on addNew2 input)
    output$t1 <- renderGvis({
        if(input$a5==0) return()
        else {
            isolate({gvisTable(cbind(asset=rownames(basic()),round(basic(),3)),
                               options=list(
                                   page='enable',
                                   height='automatic',
                                   width='automatic'
                               )
            )})
        }
    })
    
    #Compute correlations for all tickers picked in addNew2 input
    correlations <- reactive({
        cor(newRets2(),use = 'pairwise.complete.obs') #to account for missing data
    })
    
    #Send p4 to ui.R when button a6 clicked 
    #the output of the correlations reactive function above
    output$p4 <- renderPlot({
        if(input$a6==0) return()
        else {
            isolate({
                par(oma=c(0,0,2,0))
                corrplot(correlations(),order='alphabet',type = 'lower',diag = FALSE,addCoef.col = T)
                title(main=paste0('Sample Pairwise Correlation Matrix\n ','(',input$per2, ' Continuously Compounded Returns)'),outer=T)
            })
        }
    }, height=function() { session$clientData$output_p4_width * 0.80 })
    
    #View pairwise scatterplots for all tickers pciked as per addNew2. the user may download the output
    output$d2scatter = downloadHandler(
        filename = 'scatter.pdf',
        content = function(file) {
            pdf(file=file,width=17,height=11)  #open device
            quiet <- pairs.panels(coredata(newRets2()),ellipses = F,lm = T,rug = F,hist.col = 'slateblue1')
            dev.off() #close device
        })
    
    ################2nd Tab Computations END HERE########################################## 
    
    ################3rd Tab Computations BEGIN HERE##########################################
    
    #By default, under addNew3 input in the 3rd tab, populate the tickers previously selected under addNew2 inputs in the 2nd tab
    #Similarly populate the Dates, Prices, and Return (i.e. dfrom3, dto3, and per3) previously selected in the 2nd tab
    observe({updateSelectizeInput(session,'addNew3',selected=input$addNew2,server=T)})
    observe({updateDateInput(session,'dfrom3',value=input$dfrom2)})
    observe({updateDateInput(session,'dto3',value=input$dto2)})
    observe({updateSelectInput(session,'per3',selected=input$per2)})
    
    ##use fixTicker function from global.R to process the tickers entered by the user in the 3rd tab (i.e addNew3). 
    #Identical to newTicker2() in 2nd tab calculations above
    newTicker3 <- reactive({
        fixTicker(input$addNew3)
    })
    
    #Fetch histrical prices from Yahoo! for tickers in newTicker3() .
    #Identical to newSyms2() in 1st tab calculations above
    newSyms3 <- reactive({
        suppressWarnings(        
            tryCatch(downloadSym(newTicker3(),envir = myEnv,dateFrom = input$dfrom3,dateTo = input$dto3)
                     , error=function(e) 'error')
        )
    })
    
    
    #Calculate prices for newTicker3() based on Prices input (per3). the default prices fetched are daily
    #Identical to to.period2() in 2nd tab calculations above
    to.period3 <- reactive({
        if('error' %in% newSyms3()) validate(need(!('error' %in% newSyms3()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.')) #to provoke updating the data
        else {
            Myto.period(input$per3,myEnv,newTicker3())
        }
    })
    
    #Calculate Returns for to.period3() 
    #Identical to newRets2() in 1st tab calculations above
    newRets3 <- reactive({
        if('error' %in% newSyms3()) validate(need(!('error' %in% newSyms3()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.'))
        else {
            Myrets(to.period3(),newTicker3())
        }
    })
    
    #Display a new dropdown when the user picks Correlation in boot2 input
    output$cor2 <- renderUI({ if(input$boot2=='Correlation') selectizeInput('cor2','Pick a pair at a time',choices=names(newRets3()),multiple=T,options = list(maxItems = 2)) else return() })
    
    #Display a new dropdown when the user picks VaR in boo2 input
    output$perc2.2 <- renderUI({if(input$boot2=='Value-at-Risk') selectInput('perc2.2','VaR Percent',choices = c('1%'=0.01,'5%'=0.05)) else return() })
    
    #Calculate boostrap statistics based on boot2 input. Use helper functions defined in global.R
    boots <- reactive({
        Myboot(input$boot2,input$perc2.2,input$cor2,newRets3())
    })
    
    #Calculate bootstrap confidence interval for boots() computed above
    boots.ci <- reactive({
        lapply(boots(),function(x) boot.ci(x,conf=0.95, type=c('norm','perc')))
    })
    
    #View the output of boots() and boots.ci() reactive functions above in pdf. The user can also save the output
    output$d2boot = downloadHandler(
        filename = 'boot.pdf',
        content = function(file) {
            pdf(file=file)  #open device
            quiet<-lapply(1:length(boots()),function(x) {par(oma=c(7,2,3,1),mar=c(0,2,2,1));
                                                         if(input$boot2=='Correlation') {pairs.panels(coredata(newRets3()[,input$cor2]),ellipses = F,lm = T, 
                                                                                                      hist.col = 'slateblue1', rug = F);title(main='4-Panel Correlation Summary (Sample Returns)',outer= T)};
                                                         plot(boots()[[x]]);title(main=paste0('Bootstrap ',input$boot2,' Distribution','(',names(boots())[x],')'),
                                                                                  sub = paste0(input$per3,' Continuously Compounded Returns'),outer=T);
                                                         par(mfrow=c(2,1));textplot(capture.output(boots()[x]),halign='left');
                                                         textplot(capture.output(boots.ci()[x]),halign='left');
                                                         title(main=paste0('Bootstrap Summary and Confidence Interval','(',names(boots())[x],')'),
                                                               sub = paste0(input$per3,' Continuously Compounded Returns'),outer=T)})
            dev.off() #close device
        })
    
    #Display a new dropdown when the user picks VaR in roll input
    output$perc3 <- renderUI({ if(input$roll=='Value-at-Risk') selectInput('perc3','VaR Percent',choices = c('1%'=0.01,'5%'=0.05,'10%'=0.1)) else return() })
    
    #Display a new dropdown when the user picks Correlation in roll input
    output$cor3 <- renderUI({ if(input$roll=='Correlation') selectizeInput('cor3','Pick a pair at a time',choices=names(newRets3()),multiple=T,options = list(maxItems = 2)) else return() })
    
    #Compute rolling statistics based on user's inputs
    #Myroll helper function is used from global.R
    rolling <- reactive({
        if('error' %in% newSyms3()) validate(need(!('error' %in% newSyms3()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.'))
        else {
            Myroll(inputRoll=input$roll,inputCor = input$cor3,inputPerc = as.numeric(input$perc3),returns=newRets3(),inputWin=input$win)  
        }
        
    })
    
    #Plot the output of rolling(). The user can also save the output
    output$d3roll = downloadHandler(
        filename = 'rolling.pdf',
        content = function(file) {
            pdf(file=file)  #open device
            quiet <- lapply(1:ncol(rolling()), function(x) {
                if(input$roll %in% c('Mean','Std.Deviation')){
                    par(lwd=2)
                    plot(newRets3()[,x],main=paste0(input$win,' ',sub('ly','',input$per3),' Rolling ',input$roll,'s for ',names(newRets3()[,x]),'\n(Continuously Compounded Returns)'),auto.grid = F)
                    par(lwd=1)
                    lines(rolling()[,x],col='red')
                    if(input$roll=='Mean') {abline(h=mean(coredata(newRets3()[,x]),na.rm=T),col='blue')} else {abline(h=sd(coredata(newRets3()[,x]),na.rm=T),col='blue')}
                    legend('bottomleft',legend=c(paste0('Rolling ',input$roll,'s'),paste0(names(newRets3()[,x]),' Returns'),ifelse(input$roll=='Mean','Sample Mean','Sample Std.Deviation')),col=c('red','black','blue'),lty=1,bty='n')
                }
                else{
                    par(lwd=2)
                    plot(rolling()[,x],main=paste0(input$win,' ',sub('ly','',input$per3),' Rolling ',input$roll,'s for ',names(rolling()[,x]),'\n(Continuously Compounded Returns)'),auto.grid = F)
                    
                }
            })
            dev.off() #close device
        })
    
    #Display two new dropdown menus when the user picks Mean in hyp input
    output$hypMean <- renderUI({ if(input$hyp=='Mean') selectInput('hypMean','Alternative Hypothesis',choices = c('Greater than'='g','Less than'='l','Two Sided'='t')) else return() })
    output$hypNum <- renderUI({ if(input$hyp=='Mean') numericInput('hypNum','Value',value = 0) else return() })
    
    #Display a new dropdown when the user picks Mean in hyp input
    output$hypCor <- renderUI({ if(input$hyp=='Pairwise Correlation') selectizeInput('hypCor','Pick a pair at a time',choices=names(newRets3()),multiple=T,options = list(maxItems = 2)) else return() })
    
    #Reactive function to carry out the hypothesis testing computations in tab3
    hyp <- reactive({
        Myhyp(inputHyp=input$hyp,inputHypCor=input$hypCor,returns=newRets3(),inputHypMean = input$hypMean,inputHypNum = input$hypNum)
    })
    
    #Use Myhyp along with Myacf helper functions from global.R to plot the output below. The user can also save the output
    output$d3hyp = downloadHandler(
        filename = 'hypothesisTesting.pdf',
        content = function(file) {
            pdf(file=file,paper=if(input$hyp=='Pairwise Correlation') {'executive'} else if(input$hyp=='Normality') {'usr'} else{'special'})  #open device
            if(input$hyp=='Serial Correlation'){
                par(mfrow=c(2,2),oma=c(0,0,3,0))
                quiet <- lapply(newRets3(), function(x) {
                    Myacf(x)
                    title(main=paste0('Sample Autocorrelation Function\n (',input$per3,' Continuously Compounded Returns)'),outer=T)
                })
                par(mfrow=c(1,1))
            }
            else if(input$hyp=='Normality'){
                par(mfrow=c(2,2),mar=c(4,4,3,4),oma=c(0,0,2,0))
                quiet <- lapply(newRets3(), function(x){
                    hist(x,main='', probability=T, col="slateblue1")
                    points(density(x,na.rm=T),col='orange',type='l',lwd=2,ylab='') #density plot
                    qqnorm(coredata(x), col="slateblue1",main='')
                    qqline(x)
                    boxplot(coredata(x),outchar=T,col="slateblue1")
                    textplot(str_trim(capture.output(jarque.bera.test(x[complete.cases(x)]))[-4]),halign = 'center',valign = 'center')
                    title(main=paste0('Normality Test for ',names(x),'\n(',input$per3,' Continuously Compounded Returns)'),outer=T)                    
                })
                par(mfrow=c(1,1))
            }
            else {hyp()}
            dev.off() #close device
        })
    
    ################3rd Tab Computations END HERE##########################################
    ################4th Tab Computations BEGIN HERE##########################################
    
    #By default, under addNew4 input in the 4th tab, populate the tickers previously selected under addNew3 inputs in the 3rd tab
    #Similarly populate the Dates, Prices, and Return (i.e. dfrom4, dto4, and per4) previously selected in the 3rd tab
    observe({updateSelectizeInput(session,'addNew4',selected=input$addNew3,server=T)})
    observe({updateDateInput(session,'dfrom4',value=input$dfrom3)})
    observe({updateDateInput(session,'dto4',value=input$dto3)})
    observe({updateSelectInput(session,'per4',selected=input$per3)})
    
    ##use fixTicker function from global.R to process the tickers entered by the user in the 4th tab (i.e addNew4). 
    #Identical to newTicker3() in 3rd tab calculations above
    newTicker4 <- reactive({
        fixTicker(input$addNew4)
    })
    
    #Fetch histrical prices from Yahoo! for tickers in newTicker4() .
    newSyms4 <- reactive({
        suppressWarnings(        
            tryCatch(downloadSym(newTicker4(),envir = myEnv,dateFrom = input$dfrom4,dateTo = input$dto4)
                     , error=function(e) 'error')
        )
    })    
    
    #Calculate prices for newTicker4() based on Prices input (per4). the default prices fetched are daily
    #Identical to to.period3() in 3rd tab calculations above
    to.period4 <- reactive({
        if('error' %in% newSyms4()) validate(need(!('error' %in% newSyms4()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.')) #to provoke updating the data
        else {
            Myto.period(input$per4,myEnv,newTicker4())
        }
    })    
    
    #Calculate Returns for to.period4() 
    newRets4 <- reactive({
        if('error' %in% newSyms4()) validate(need(!('error' %in% newSyms4()),'Invalid Input! Either ticker symbol is not valid for the source selected and/or dates are not selected appropriately.'))
        else {
            Myrets(to.period4(),newTicker4())
        }
    })    
    
    #Fetch histrical Market prices from Yahoo! 
    mktEnv <- new.env() #Define a new environment to store market data
    mktTicker <- reactive({fixTicker(input$indSym)})
    mktSym <- reactive({
        suppressWarnings(        
            tryCatch( downloadSym(mktTicker(),envir = mktEnv,dateFrom = input$dfrom4,dateTo= input$dto4)
                      , error=function(e) 'error')
        )
    })
    
    #Calculate prices for Market based on Prices input (per4). the default prices fetched are daily
    #Identical to to.period4() above
    mktto.period <- reactive({
        if(mktSym()=='error') validate(need(!(mktSym()=='error'),'Invalid Market Index!')) #to provoke updating the data
        else {
            Myto.period(input$per4,mktEnv,mktTicker())
        }
    })
    
    #Calculate Returns for mktto.period() 
    mktRets <- reactive({
        if(mktSym()=='error') validate(need(!(mktSym()=='error'),'Invalid Market Index!')) #to provoke updating the data
        else {
            Myrets(mktto.period(),mktTicker())
        }
    })
    
    #Reactive function to compute linear regression of returns in newRets4() on market returns mktRets()
    #Mylsfit helper function used from global.R
    ls.fit <- reactive({
        Mylsfit(assetRets = newRets4(),marketRets = mktRets(),rf = input$rf4)
    })    
    
    #Reactive function to yield the expected return vector based on historical returns calculated in newRets4()
    #retEst helper function is used from global.R
    ret.Est <- reactive({  
        retEst(newRets4())
    })
    
    #Reactive function to yield the sd vector based on historical returns calculated in newRets4()
    #sdEst helper function is used from global.R
    sd.Est <- reactive({
        sdEst(newRets4())
    })
    
    #Reactive function to estimate the covariance matrix for returns in newRets4() via SIM
    #SIMcovmat helper function used from global.R
    sim.covmat <- reactive({
        SIMcovmat(beta.df = ls.fit()$summary[2,],fitResids = ls.fit()$residuals,marketRets = mktRets())
    })
    
    #Reactive function to yield the covariance matrix for newRets4(). Either historical or via SIM, depending on the user's selection
    #covmatEst helper function used from global.R
    covmat.Est <- reactive({
        if(input$sim4==0){
            covmatEst(newRets4())
        }
        else {
            sim.covmat()
        }
    })    
    
    #Reactive function to compute the efficient frontier based on ret.Est() and covmat.Est()
    frontier <- reactive({
        efficient.frontier(er = ret.Est(), cov.mat = covmat.Est(), alpha.min=0, alpha.max=1, nport=20, shorts=input$shorts4)
    })
    
    #Reactive function to compute the global minimum portfolio based on ret.Est() and covmat.Est()
    gminPort <- reactive({
        globalMin.portfolio(er = ret.Est(),cov.mat = covmat.Est(),shorts = input$shorts4)
    })
    
    #Reactive function to compute the tangency portfolio based on ret.Est() and covmat.Est()
    tanPort <- reactive({
        tangency.portfolio(er = ret.Est(),cov.mat = covmat.Est(),risk.free = input$rf4,shorts = input$shorts4)
    })
    
    #Reactive function to compute the tangency + risk-free asset portfolio 
    cal <- reactive({
        if(input$on4=='Capital Allocation Line'){
            if(input$target4=='Target period expected return'){
                mu.obj <- input$val4
                x.tan <- (mu.obj-input$rf4)/(tanPort()$er - input$rf4)
                sd.cal <- x.tan*tanPort()$sd
                
                #return list of weights, sd, and mu
                returnList <- list(NULL,NULL,NULL)
                names(returnList) <- c('er','sd','weights')
                returnList$er <- mu.obj
                returnList$sd <- sd.cal
                returnList$weights <- c(x.tan,1-x.tan)
                names(returnList$weights) <- c('Tangency Portfolio','Risk-free Asset')
                
                return(returnList)  
            }
            else if(input$target4=='Target period std. deviation'){
                sd.obj <- input$val4
                x.tan <- sd.obj/tanPort()$sd
                mu.cal <- input$rf4 + x.tan*(tanPort()$er - input$rf4)
                
                #return list of weights, sd, and mu
                returnList <- list(NULL,NULL,NULL)
                names(returnList) <- c('er','sd','weights')
                returnList$er <- mu.cal
                returnList$sd <- sd.obj
                returnList$weights <- c(x.tan,1-x.tan)
                names(returnList$weights) <- c('Tangency Portfolio','Risk-free Asset')
                
                return(returnList)  
            }
        }      
    })    
    
    #Reactive function to compute the efficient portfolio subject to a target return
    effPort <- reactive({
        if(input$on4=='Efficient Frontier'){
            if(input$target4=='Target period std. deviation') {
                validate(need(input$target4!='Target period std. deviation','Selection invalid! please enter a tagert return instead.'))
            }
            else if(input$target4=='Target period expected return'){
                
                efficient.portfolio(er = ret.Est(),cov.mat = covmat.Est(),target.return = input$val4,shorts = input$shorts4)  
            }
            
        }     
    })
    
    #Reactive values to keep track of buttons front4 and opt4
    #also used to control x and y limits
    buttons <- reactiveValues()
    buttons$last <- 'initial'
    
    #Observers to reset the value of reactive values that track buttons front4 and opt4
    #also used to control x and y limits
    observe({if(input$front4>0) buttons$last <- 'front'})
    observe({if(input$opt4>0) buttons$last <- 'opt'})
    
    #Define x and y limits of the Risk-Reward Plot
    x.lim <- reactive({
        optNotClicked <- if(input$on4=='Capital Allocation Line') {
            ifelse(buttons$last!='opt',0,cal()$sd)
        } 
        else if(input$on4=='Efficient Frontier'){
            ifelse(buttons$last!='opt',0,effPort()$sd)
        }        
        
        range(c(0,sd.Est(),frontier()$sd,tanPort()$sd,optNotClicked))
    })
    
    y.lim <- reactive({
        optNotClicked <- if(input$on4=='Capital Allocation Line') {
            ifelse(buttons$last!='opt',0,cal()$er)
        } 
        else if(input$on4=='Efficient Frontier'){
            ifelse(buttons$last!='opt',0,effPort()$er)
        } 
        range(c(0,ret.Est(),frontier()$er,tanPort()$er,optNotClicked))
    })
    
    #Send p5 to ui.R
    output$p5 <- renderPlot({
        
        if(input$front4>0 & buttons$last=='front')
            isolate({
                
                #Reset plot
                plot.new()
                
                #Plot efficient frontier
                plot(frontier()$sd,frontier()$er, col="blue", pch=16,type='b',xlim=x.lim(),ylim=y.lim(),xlab='',ylab='')
                
                #Add asset labels
                Mylabels(ret.Est(),sd.Est())
                
                #Add global min and tangency portfolio's to the plot
                points(gminPort()$sd, gminPort()$er, col="green", pch=16, cex=3)
                points(tanPort()$sd, tanPort()$er, col="red", pch=16, cex=2)
                
                #Add the cal line
                tangent = (tanPort()$er - input$rf4)/tanPort()$sd
                abline(a=input$rf4, b=tangent, col="purple", lwd=2)
                
                #Add title, axes lables, and legend
                title(main='Risk/Reward Plot',xlab = 'Risk (SD)',ylab='Reward (ER)')                
                legend(x='topleft',bty='n',legend=c('Efficient Frontier','CAL','Global Min','Tangency'),pch=c(16,NA,16,16),lty=c(4,1,0,0) ,col=c('blue','purple','green','red')) 
            })
        
        if(input$opt4>0 & buttons$last=='opt')
            isolate({
                
                #Reset plot
                plot.new()
                
                #Plot efficient frontier
                plot(frontier()$sd,frontier()$er, col="blue", pch=16,type='b',xlim=x.lim(),ylim=y.lim(),xlab='',ylab='')
                
                #Add asset labels
                Mylabels(ret.Est(),sd.Est())
                
                #Add global min and tangency portfolio's to the plot
                points(gminPort()$sd, gminPort()$er, col="green", pch=16, cex=2)
                points(tanPort()$sd, tanPort()$er, col="red", pch=16, cex=2)
                
                #Add the cal line
                tangent = (tanPort()$er - input$rf4)/tanPort()$sd
                abline(a=input$rf4, b=tangent, col="purple", lwd=2) 
                
                if(input$on4=='Capital Allocation Line'){
                    points(cal()$sd,cal()$er,col='orange',pch=18,cex=3)
                }
                else if(input$on4=='Efficient Frontier'){
                    points(effPort()$sd,effPort()$er,col='orange',pch=18,cex=3)
                }
                
                #Add title, axes lables, and legend
                title(main='Risk/Reward Plot',xlab = 'Risk (SD)',ylab='Reward (ER)')                
                legend(x='topleft',bty='n',legend=c('Efficient Frontier','CAL','Global Min','Tangency',ifelse(input$on4=='Efficient Frontier','Efficient','Optimal')),pch=c(16,NA,16,16,18),lty=c(4,1,0,0,0) ,col=c('blue','purple','green','red','orange'))
                
            })         
        
    })
    
    #Construct a summary table to extract components of ls.fit() above, for assets
    lsAssets <- reactive({
        lsAssetsT(lsfit.summary.df = ls.fit()$summary)[-c(2,6)] #exclude alpha values and sigma values
    })
    
    #Same as above. only used for Mybootsummary function later, to include alpha values excluded above
    lsAssets2 <- reactive({
        lsAssetsT(lsfit.summary.df = ls.fit()$summary)
    })
    
    #Only used for Mybootsummary and output$hypTest5 functions later, to include alpha.tstat values 
    lsAssets.tstat <- reactive({
        lsAssetsT(lsfit.summary.df = ls.fit()$tstats)
    })
    
    #Construct a summary table for assets used in gminPort, tanPort, and effPort
    #assetsTable helper function used from global.R
    assetsW <- reactive({
        thirdPort <- if(buttons$last=='opt' & input$on4=='Efficient Frontier') effPort() else NULL 
        allPorts <- list(Global=gminPort(),Tangency=tanPort(),thirdPort)
        if(!is.null(thirdPort)) names(allPorts)[3] <- 'Efficient Port. Weights'
        output <- assetsTable(portsList = allPorts )  #portsList is a named ordered list of gminPort, tanPort, and effPort constructed above.
        
        #merge in the betas summary from lsAssets() above
        output <- merge.data.frame(output,lsAssets(),sort = F)
        
        #housekeeping
        if(length(output)==7){
            output <- output[c(1:2,5:7,3:4)]
        }
        else{
            output <- output[c(1:2,6:8,3:5)]
        }
        return(output)
    })
    
    #Reactive function to store the weights of the optimal portfolio on CAL
    #optimalW helper function used form global.R
    optW <- reactive({
        if(input$on4=='Capital Allocation Line'){
            optimalW(assetstable = assetsW(),CALweights = cal()$weights)
        } 
    }) 
    
    #Reactive function to combine the two tables assetsW and optW together
    #in order to display in t3 below 
    mergedW <- reactive({
        
        if(buttons$last=='opt' & input$on4=='Capital Allocation Line'){
            merge.data.frame(assetsW(),optW(),all=T)
        }
        else {
            assetsW()
        }
        
    })    
    
    #Calculate portfolio period returns based on calculated weights
    portPeriodRets <- reactive({
        portRets(assetstable = assetsW(),optimalweights = optW(),returns = newRets4(),rf = input$rf4)
    })     
    
    #Reactive function to compute linear regression of portfolio returns in portPeriodRets() on market returns mktRets()
    #Mylsfit helper function used from global.R
    ls.fit.port <- reactive({
        Mylsfit(assetRets = portPeriodRets(),marketRets = mktRets(),rf = input$rf4)
    })    
    
    #Construct a summary table to extract components of least squares fit, for period portfolio returns
    lsPorts <- reactive({
        lsPortsT(ls.fit.port()$summary)
    })
    
    #only used for Mybootsummary() and output$hypTest5 functions later to exctract alpha.tstat values
    lsPorts.tstat <- reactive({
        lsPortsT(ls.fit.port()$tstats)
    })
    
    #Construct a summary table for portfolio performance
    #portsTable helper function used from global.R
    portsT <- reactive({
        
        thirdPort <- {
            if(buttons$last=='opt') {
                if(input$on4=='Efficient Frontier') effPort()
                else if (input$on4=='Capital Allocation Line') cal()
            }
            else NULL 
        }
        
        allPorts <- list(gminPort(),tanPort(),thirdPort)
        output <- portsTable(portsList = allPorts,rf = input$rf4 )  #portsList is an ordered list of all portfolio's constructed above.
        
        #housekeeping: add the name of the third portfolio, depending on the user's inputs
        if(!is.null(thirdPort)) output[3,1] <- ifelse(input$on4=='Efficient Frontier','Efficient','Optimal')
        
        output <- merge.data.frame(output,lsPorts(),sort=F)
        
        #Calculate Treynor ratio
        output$Treynor <- (output$ER - input$rf4)/output$beta
        
        #Calculate M2 
        mktSD <- sd(mktRets(),na.rm=T)
        mktER <- mean(mktRets(),na.rm=T)
        output$M2 <- ((output$ER - input$rf4)*(mktSD/output$SD))-(mktER-input$rf4)
        
        #Calculate market sharpe
        output$MarketSharpe <- (mktER-input$rf4)/mktSD
        
        #housekeeping
        output <- output[c(1:4,6:12,5,15,13:14)]
        
        return(output)
        
    })
    
    #Reactive function to annualize the portfolio performance data in portsT
    #Myannual helper function used from global.R
    annualize <- reactive({
        Myannual(portstable = portsT(),rf = input$rf4,inputPrices = input$per4,marketRets = mktRets())
    })
    
    #Send portfolio results table, t2, to ui.R
    output$t2 <- renderGvis({
        if(input$front4>0 | input$opt4>0){
            isolate({
                if(input$annual!=0) {
                    gvisTable(annualize(),options=list(width='100%'),
                              formats = list('ER'='#,###.##',
                                             'Geom.ER'='#,###.##',
                                             'SD'='#,###.##',
                                             '1% VaR'='#,###.##',
                                             '5% VaR'='#,###.##',
                                             'alpha'='#,###.##',
                                             'beta'='#,###.##',
                                             'SE.beta'='#,###.##',
                                             'R2'='#,###.##',
                                             'Sigma.e'='#,###.##',
                                             'Sharpe'='#,###.##',
                                             'MarketSharpe'='#,###.##',
                                             'Treynor'='#,###.##',
                                             'M2'='#,###.##'
                              )
                    )
                }
                else {
                    gvisTable(portsT(),options=list(width='100%'),
                              formats = list('ER'='#,###.####',
                                             'Geom.ER'='#,###.####',
                                             'SD'='#,###.####',
                                             '1% VaR'='#,###.####',
                                             '5% VaR'='#,###.####',
                                             'alpha'='#,###.####',
                                             'beta'='#,###.####',
                                             'SE.beta'='#,###.####',
                                             'R2'='#,###.###',
                                             'Sigma.e'='#,###.###',
                                             'Sharpe'='#,###.###',
                                             'MarketSharpe'='#,###.###',
                                             'Treynor'='#,###.###',
                                             'M2'='#,###.###'
                              )   
                    )
                }
            })
        }
        
    })
    
    #Define formats for t3 below
    myFormats <- reactive({
        if(length(mergedW())>7 & buttons$last=='opt' & input$on4=='Efficient Frontier'){
            formats = list('Last Price'='#,###.##',
                           'beta'='#,###.###',
                           'SE.beta'='#,###.###',
                           'R2'='#,###.###',
                           'Global Min. Port. Weights'='#,###.##',
                           'Tangency Port. Weights'='#,###.##',
                           'Efficient Port. Weights'='#,###.##'
            )  
            
        } 
        else if(length(mergedW())>7 & buttons$last=='opt' & input$on4=='Capital Allocation Line') {
            formats = list('Last Price'='#,###.##',
                           'beta'='#,###.###',
                           'SE.beta'='#,###.###',
                           'R2'='#,###.###',
                           'Global Min. Port. Weights'='#,###.##',
                           'Tangency Port. Weights'='#,###.##',
                           'Optimal Port. Weights'='#,###.##'
            )  
        }
        else {
            formats = list('Last Price'='#,###.##',
                           'beta'='#,###.###',
                           'SE.beta'='#,###.###',
                           'R2'='#,###.###',
                           'Global Min. Port. Weights'='#,###.##',
                           'Tangency Port. Weights'='#,###.##'           
            ) 
        }
    })
    
    #Send asset allocation results table, t3, to ui.R
    output$t3 <- renderGvis({
        if(input$front4>0 | input$opt4>0){
            isolate({
                gvisTable(mergedW(),
                          formats = myFormats(),
                          options=list(
                              page='enable',
                              height='automatic',
                              width='automatic'
                          )
                )
            })
        }
        
    })
    
    #Display t2 title when it is viewed
    output$t2title <-renderUI({
        if(input$front4>0 | input$opt4>0){
            isolate({
                tmp <- paste0(ifelse(input$annual!=0,'Annualized',input$per4),' Portfolio Performance')
                p(strong(tmp))
            })
        }
        
    })
    
    #Display t3 title when it is viewed
    output$t3title <-renderUI({
        if(input$front4>0 | input$opt4>0){
            isolate({
                p(strong('Asset Allocation'))
            })
        }
        
    })
    
    #Send asset allocation chart, col1, to ui.R
    output$col1 <- renderGvis({
        if(input$front4>0 | input$opt4>0){
            isolate({
                gvisColumnChart(mergedW()[-c(2:5)],
                                options=list(
                                    legend='top',height=250
                                )
                )
            })
        }
        
    })
    
    #Display relative performance chart for all portfolio's
    output$p6 <- renderPlot({
        if(input$front4>0 | input$opt4>0){
            isolate({
                chart.RelativePerformance(exp(portPeriodRets())-1,exp(mktRets())-1,legend.loc = 'topleft',main='Growth of $1') 
            })
        }  
    })
    
    #Display return distribution comparison chart for all portfolio's
    output$p7 <- renderPlot({
        if(input$front4>0 | input$opt4>0){
            isolate({
                chart.Boxplot(merge(mktRets(),portPeriodRets())) 
            })
        }  
    })
    
    ################4th Tab Computations END HERE##########################################    
    
    ################5th Tab Computations BEGIN HERE##########################################    
    
    
    #Display a new dropdown menu when the user picks global min or efficient portfolio under boot5 input
    output$boot5port <- renderUI({ if(input$boot5 %in% c('Global Minimum Portfolio','Efficient Portfolio')) selectInput('boot5port','Parameter',choices = c('ER'=1,'SD'=2,'Weights'=3,'Alpha'=4,'Beta'=5,'R2'=6,'Alpha t-stat'=7)) else return() })
    
    #Display a new dropdown menu when the user picks Assets under boot5 input
    output$boot5assets <- renderUI({ if(input$boot5 %in% c('Assets','Tangency Portfolio','Optimal Portfolio')) selectInput('boot5assets','Parameter',choices = c('Alpha'=4,'Beta'=5,'R2'=6,'Alpha t-stat'=7)) else return() })
    
    #Reactive function to bootstrap efficient frontier
    #frontier.boot helper function used from global.R
    frontierBoot <- reactive({
        frontier.boot(returns=newRets4(),inputShorts = input$shorts4)
    })
    
    #Reactive function to bootstrap global min portfolio parameters
    #gmin.port.boot helper function used from global.R
    gminBoot <- reactive({
        gmin.port.boot(returns = newRets4(),inputShorts = input$shorts4)
    })
    
    #Reactive function to bootstrap efficient portfolio(with target return) parameters
    #effport.boot helper function used from global.R
    effBoot <- reactive({
        if(input$target4=='Target period expected return' & input$on4=='Efficient Frontier'){
            effport.boot(returns = newRets4(),inputTarget = input$val4,inputShorts = input$shorts4)
        }
    })
    
    #Reactive function to compute portfolio bootstrap regression parameters
    port.reg.boot <- reactive({
        reg.boot(assetRets = portPeriodRets(),marketRets = mktRets(),rf = input$rf4)
    })
    
    #Reactive function to compute bootstrap regression parameters for individual assets within the portfolio
    asset.reg.boot <- reactive({
        reg.boot(assetRets = newRets4(),marketRets = mktRets(),rf = input$rf4)
    })
    
    #Reactive function to output the summary of bootstrap portfolio for ER and SD only
    bootSummary <- reactive({
        if(input$boot5=='Global Minimum Portfolio'){
            Mybootsummary(portOrig = gminPort(),portBoot = gminBoot(),inputBootPort = as.numeric(input$boot5port),RegBoot = port.reg.boot(),Fit = lsPorts(),Fit.tstat = lsPorts.tstat(), assets=F, global = T , tan=F,eff = F,opt = F)
        }
        else if(input$boot5=='Tangency Portfolio'){
            Mybootsummary(inputBootPort = as.numeric(input$boot5assets),RegBoot = port.reg.boot(),Fit = lsPorts(), assets=F,Fit.tstat = lsPorts.tstat(),global = F,eff = F, tan=T, opt=F)
        }
        else if(input$boot5=='Efficient Portfolio'){
            Mybootsummary(portOrig = effPort(),portBoot = effBoot(),inputBootPort = as.numeric(input$boot5port),RegBoot = port.reg.boot(),Fit = lsPorts(),Fit.tstat = lsPorts.tstat(), assets=F, global = F,eff = T, tan=F,opt = F)
        }
        else if(input$boot5=='Optimal Portfolio'){
            Mybootsummary(inputBootPort = as.numeric(input$boot5assets),RegBoot = port.reg.boot(),Fit = lsPorts(),Fit.tstat = lsPorts.tstat(), assets=F,global = F,eff = F, tan=F, opt=T)
        }
        else if(input$boot5=='Assets'){
            Mybootsummary(inputBootPort = as.numeric(input$boot5assets),RegBoot = asset.reg.boot(),Fit = lsAssets2(),Fit.tstat = lsAssets.tstat(),assets = T,global = F,eff = F,tan=F,opt = F)
        }
    })
    
    #Display the bootstrap portfolio parameters computed above in pdf
    output$d5boot = downloadHandler(
        filename = 'boot.pdf',
        content = function(file) {
            pdf(file=file)  #open device
            if(input$boot5=='Portfolio Frontier'){
                plot(frontier()$sd,frontier()$er, col="blue", pch=16,type='b',xlim=x.lim(),ylim=y.lim(),xlab='',ylab='')
                quiet <- lapply(1:length(frontierBoot()), function(x)  {
                    points(frontierBoot()[[x]]$sd, frontierBoot()[[x]]$er, type="l",col=rgb(1, 0, 0, 0.05))
                })
                title(main='Bootstrapping Efficient Frontier',xlab = 'Risk (SD)',ylab='Reward (ER)')                
                legend(x='topleft',bty='n',legend=c('Sample Frontier','Bootstrap Frontier'),pch=c(16,NA),lty=c(4,1) ,col=c('blue',rgb(1, 0, 0,0.5)))  
            }
            else if(input$boot5=='Global Minimum Portfolio'){
                par(mfrow=c(2,2),mar=c(4,4,3,4),oma=c(0,0,2,0))
                quiet <- if(as.numeric(input$boot5port) %in% c(1,2)){
                    x <- gminBoot()[[as.numeric(input$boot5port)]]
                    hist(x, col="slateblue1",probability = T,main=paste0(ifelse(as.numeric(input$boot5port)==1,'ER','SD'),' Distribution'),xlab='')
                    points(density(x,na.rm=T),col='orange',type='l',lwd=2,ylab='')
                    qqnorm(x, col="slateblue1", pch=16)
                    qqline(x)
                    plot(gminBoot()[[2]], gminBoot()[[1]],pch=16,col=rgb(0,0,1,0.4),xlab='Risk (SD)',ylab='Reward (ER)',main='Global Minimum Risk/Reward Cloud',xlim=x.lim(),ylim=y.lim())
                    textplot(capture.output(bootSummary()),valign = 'top')
                    title(main='Bootstrap Summary')
                    title(main=paste0('Bootstrapping ',ifelse(as.numeric(input$boot5port)==1,'ER','SD'),' of ',input$boot5),outer=T)
                }
                else if(as.numeric(input$boot5port)==3){
                    x <- gminBoot()[[as.numeric(input$boot5port)]]
                    quiet <- lapply(1:ncol(x), function(y) {
                        hist(x[,y], col="slateblue1",probability = T,main=paste0(colnames(x)[y],' Weights Distribution'),xlab='')
                        points(density(x[,y],na.rm=T),col='orange',type='l',lwd=2,ylab='')
                        qqnorm(x[,y], col="slateblue1", pch=16)
                        qqline(x[,y])
                        title(main=paste0('Bootstrapping weights of ',input$boot5),outer=T)
                    })
                    par(mfrow=c(1,1))
                    quiet <- textplot(capture.output(bootSummary()))
                    title(main='Bootstrap Summary')
                    #stacked bar chart for weights (first 20 ports)
                    tmp.w.boot = x[1:20, ]
                    tmp.mu.boot = gminBoot()[[1]][1:20, ]
                    sort.idx = order(tmp.mu.boot)
                    chart.StackedBar(tmp.w.boot[sort.idx,], xaxis.labels=round(tmp.mu.boot[sort.idx],3), xlab='Portfolio ER',
                                     ylab="Weights", main='Asset Allocation (First 20 Portfolio\'s)')
                }
                else if(as.numeric(input$boot5port) %in% c(4,5,6,7)){
                    layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
                    y <- ifelse(as.numeric(input$boot5port)==4,'Alpha',ifelse(as.numeric(input$boot5port)==5,'Beta',ifelse(as.numeric(input$boot5port)==6,'R2','Alpha.tstat')))
                    x <- port.reg.boot()[[ifelse(y!='R2',tolower(y),y)]][,'Global Min']
                    quiet <- {
                        hist(x, col="slateblue1",probability = T,main=paste0(y,' Distribution'),xlab='')
                        points(density(x,na.rm=T),col='orange',type='l',lwd=2,ylab='')
                        qqnorm(x, col="slateblue1", pch=16)
                        qqline(x)
                        textplot(capture.output(bootSummary()))
                        title(main=paste0('Bootstrapping ',y,' of ',input$boot5),outer=T)
                    }                 
                    
                }
            }
            else if(input$boot5=='Efficient Portfolio'){
                par(mfrow=c(2,2),mar=c(4,4,3,4),oma=c(0,0,2,0))
                quiet <- if(as.numeric(input$boot5port) %in% c(1,2)){
                    x <- effBoot()[[as.numeric(input$boot5port)]]
                    hist(x, col="slateblue1",probability = T,main=paste0(ifelse(as.numeric(input$boot5port)==1,'ER','SD'),' Distribution'),xlab='')
                    points(density(x,na.rm=T),col='orange',type='l',lwd=2,ylab='')
                    qqnorm(x, col="slateblue1", pch=16)
                    qqline(x)
                    plot(effBoot()[[2]], effBoot()[[1]],pch=16,col=rgb(0,0,1,0.4),xlab='Risk (SD)',ylab='Reward (ER)',main='Efficient Portfolio Risk/Reward Cloud',,xlim=x.lim(),ylim=y.lim())
                    textplot(capture.output(bootSummary()),valign = 'top')
                    title(main='Bootstrap Summary')
                    title(main=paste0('Bootstrapping ',ifelse(as.numeric(input$boot5port)==1,'ER','SD'),' of ',input$boot5),outer=T)
                }
                else if(as.numeric(input$boot5port)==3){
                    x <- effBoot()[[as.numeric(input$boot5port)]]
                    quiet <- lapply(1:ncol(x), function(y) {
                        hist(x[,y], col="slateblue1",probability = T,main=paste0(colnames(x)[y],' Weights Distribution'),xlab='')
                        points(density(x[,y],na.rm=T),col='orange',type='l',lwd=2,ylab='')
                        qqnorm(x[,y], col="slateblue1", pch=16)
                        qqline(x[,y])
                        title(main=paste0('Bootstrapping weights of ',input$boot5),outer=T)
                    })
                    par(mfrow=c(1,1))
                    quiet <- textplot(capture.output(bootSummary()))
                    title(main='Bootstrap Summary')
                    #stacked bar chart for weights (first 20 ports)
                    tmp.w.boot = x[1:20, ]
                    tmp.mu.boot = effBoot()[[1]][1:20, ]
                    sort.idx = order(tmp.mu.boot)
                    chart.StackedBar(tmp.w.boot[sort.idx,], xaxis.labels=round(tmp.mu.boot[sort.idx],2),xlab='Portfolio ER', 
                                     ylab="Weights", main='Asset Allocation (First 20 Portfolio\'s)')
                }
                else if(as.numeric(input$boot5port) %in% c(4,5,6,7)){
                    layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
                    y <- ifelse(as.numeric(input$boot5port)==4,'Alpha',ifelse(as.numeric(input$boot5port)==5,'Beta',ifelse(as.numeric(input$boot5port)==6,'R2','Alpha.tstat')))
                    x <- port.reg.boot()[[ifelse(y!='R2',tolower(y),y)]][,'Efficient']
                    quiet <- {
                        hist(x, col="slateblue1",probability = T,main=paste0(y,' Distribution'),xlab='')
                        points(density(x,na.rm=T),col='orange',type='l',lwd=2,ylab='')
                        qqnorm(x, col="slateblue1", pch=16)
                        qqline(x)
                        textplot(capture.output(bootSummary()))
                        title(main=paste0('Bootstrapping ',y,' of ',input$boot5),outer=T)
                    }                 
                    
                }
            }
            else if(input$boot5=='Tangency Portfolio'){
                par(mar=c(4,4,3,4),oma=c(0,0,2,0))
                if(as.numeric(input$boot5assets) %in% c(4,5,6,7)){
                    layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
                    y <- ifelse(as.numeric(input$boot5assets)==4,'Alpha',ifelse(as.numeric(input$boot5assets)==5,'Beta',ifelse(as.numeric(input$boot5assets)==6,'R2','Alpha.tstat')))
                    x <- port.reg.boot()[[ifelse(y!='R2',tolower(y),y)]][,'Tangency']
                    quiet <- {
                        hist(x, col="slateblue1",probability = T,main=paste0(y,' Distribution'),xlab='')
                        points(density(x,na.rm=T),col='orange',type='l',lwd=2,ylab='')
                        qqnorm(x, col="slateblue1", pch=16)
                        qqline(x)
                        textplot(capture.output(bootSummary()))
                        title(main=paste0('Bootstrapping ',y,' of ',input$boot5),outer=T)
                    }                 
                    
                }
                
            }
            else if(input$boot5=='Optimal Portfolio'){
                par(mar=c(4,4,3,4),oma=c(0,0,2,0))
                if(as.numeric(input$boot5assets) %in% c(4,5,6,7)){
                    layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
                    y <- ifelse(as.numeric(input$boot5assets)==4,'Alpha',ifelse(as.numeric(input$boot5assets)==5,'Beta',ifelse(as.numeric(input$boot5assets)==6,'R2','Alpha.tstat')))
                    x <- port.reg.boot()[[ifelse(y!='R2',tolower(y),y)]][,'Optimal']
                    quiet <- {
                        hist(x, col="slateblue1",probability = T,main=paste0(y,' Distribution'),xlab='')
                        points(density(x,na.rm=T),col='orange',type='l',lwd=2,ylab='')
                        qqnorm(x, col="slateblue1", pch=16)
                        qqline(x)
                        textplot(capture.output(bootSummary()))
                        title(main=paste0('Bootstrapping ',y,' of ',input$boot5),outer=T)
                    }                 
                    
                }
                
            }
            else if(input$boot5=='Assets'){
                par(mfrow=c(2,2),mar=c(4,4,3,4),oma=c(0,0,2,0))
                y <- ifelse(as.numeric(input$boot5assets)==4,'Alpha',ifelse(as.numeric(input$boot5assets)==5,'Beta',ifelse(as.numeric(input$boot5assets)==6,'R2','Alpha.tstat')))
                x <- asset.reg.boot()[[ifelse(y!='R2',tolower(y),y)]]
                quiet <- lapply(1:ncol(x), function(z) {
                    hist(x[,z], col="slateblue1",probability = T,main=paste0(colnames(x)[z],' Distribution'),xlab='')
                    points(density(x[,z],na.rm=T),col='orange',type='l',lwd=2,ylab='')
                    qqnorm(x[,z], col="slateblue1", pch=16)
                    qqline(x[,z])
                    title(main=paste0('Bootstrapping ', y,' of portfolio ',input$boot5),outer=T)
                })
                par(mfrow=c(1,1))
                quiet <- textplot(capture.output(bootSummary()))
                title(main='Bootstrap Summary') 
            }
            
            dev.off() #close device
        })
    
    #Reactive function to compute rolling statistics for glonal min portfolio
    #Myroll.gmin helper function used from global.R
    rollGmin <- reactive({
        Myroll.gmin(returns = newRets4(),inputWin = input$win5,inputShorts = input$shorts4)
    })
    
    #Reactive function to compute rolling statistics for efficient portfolio
    #Myroll.eff helper function used from global.R
    rollEff <- reactive({
        Myroll.eff(returns = newRets4(),inputWin = input$win5,inputTarget = input$val4,inputShorts = input$shorts4)
    })
    
    #Reactive function to plot rolling regression parameters for portfolio returns
    #roll.reg helper function used from global.R
    roll.reg.port <- reactive({
        if(input$roll5=='Global Minimum Portfolio'){
            roll.reg(rets = portPeriodRets()[,'Global Min'],mktrets = mktRets(),rf = input$rf4,inputWin = input$win5)
        }
        else if(input$roll5=='Tangency Portfolio'){
            roll.reg(rets = portPeriodRets()[,'Tangency'],mktrets = mktRets(),rf = input$rf4,inputWin = input$win5)
        }
        else if(input$roll5=='Efficient Portfolio'){
            roll.reg(rets = portPeriodRets()[,'Efficient'],mktrets = mktRets(),rf = input$rf4,inputWin = input$win5)
        }
        else if(input$roll5=='Optimal Portfolio'){
            roll.reg(rets = portPeriodRets()[,'Optimal'],mktrets = mktRets(),rf = input$rf4,inputWin = input$win5)
        }
    })
    
    #Reactive function to plot rolling regression parameters for asset returns
    #roll.reg helper function used from global.R
    roll.reg.asset <- reactive({
        roll.reg(rets = newRets4(),mktrets = mktRets(),rf = input$rf4,inputWin = input$win5)
    })
    
    #Display the rolling portfolio parameters computed above in pdf
    output$d5roll = downloadHandler(
        filename = 'roll.pdf',
        content = function(file) {
            pdf(file=file,width=17,height=11)  #open device
            if(input$roll5=='Global Minimum Portfolio'){
                quiet<-lapply(1:2,function(x) {
                    plot(rollGmin()[,x],auto.grid=F,main=paste0('Rolling ',input$win5,'-',sub('ly','',input$per4),' ',toupper(names(rollGmin()[,x]))))
                    abline(h=gminPort()[[x+1]],col='blue')
                    legend('bottomleft',legend=c(paste0('Rolling ',toupper(names(rollGmin()[,x]))),paste0(input$roll5,' ',toupper(names(rollGmin()[,x])))),col=c('black','blue'),lty=1,bty='n')
                })
                quiet2 <- chart.TimeSeries(rollGmin()[,3:ncol(rollGmin())],legend.loc='topleft',main=paste0('Rolling ',input$win5,'-',sub('ly','',input$per4),' Weights'),auto.grid = F)
                quiet3 <- roll.reg.port()
            }
            else if(input$roll5=='Efficient Portfolio'){
                try(
                    quiet<-lapply(1:2,function(x) {
                        plot(rollEff()[,x],auto.grid=F,main=paste0('Rolling ', input$win5,'-',sub('ly','',input$per4),' ',toupper(names(rollEff()[,x]))))
                        abline(h=effPort()[[x+1]],col='blue')
                        legend('bottomleft',legend=c(paste0('Rolling ',toupper(names(rollEff()[,x]))),paste0(input$roll5,' ',toupper(names(rollEff()[,x])))),col=c('black','blue'),lty=1,bty='n')
                    })
                )
                try(
                    quiet2 <- chart.TimeSeries(rollEff()[,3:ncol(rollEff())],main=paste0('Rolling ', input$win5,'-',sub('ly','',input$per4),' Weights'),auto.grid = F,legend.loc='topleft')
                )
                quiet3 <- roll.reg.port()
            }
            else if(input$roll5=='Tangency Portfolio'){
                quiet <- roll.reg.port()
            }
            else if(input$roll5=='Optimal Portfolio'){
                quiet <- roll.reg.port()
            }
            else if(input$roll5=='Assets'){
                quiet <- roll.reg.asset()
            }
            dev.off() #close device
        })
    
    output$hypTest5 = downloadHandler(
        filename = 'hypothesistesting.pdf',
        content = function(file) {
            pdf(file=file)  #open device
            if(input$hyp5=='Alpha=0'){
                #housekeeping
                x <- cbind(lsAssets.tstat()[1],round(lsAssets.tstat()[2],3))
                y <- cbind(lsPorts.tstat()[1],round(lsPorts.tstat()[2],3))
                #plot
                textplot(capture.output(x))
                title(main='t-stat values for assets (H0: alpha=0)')
                textplot(capture.output(y))
                title(main='t-stat values for portfolio\'s (H0: alpha=0)')
            }
            else if(input$hyp5=='Normality of Residuals'){
                res.port <- ls.fit.port()$residuals
                res.asset <- ls.fit()$residuals
                
                textplot('Portfolio\'s')
                layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
                par(mar=c(4,4,3,4),oma=c(0,0,2,0))
                quiet <- lapply(1:length(res.port), function(x){
                    hist(res.port[,x],main='', probability=T, col="slateblue1",xlab='')
                    points(density(res.port[,x],na.rm=T),col='orange',type='l',lwd=2,ylab='') #density plot
                    qqnorm(coredata(res.port[,x]), col="slateblue1",main='')
                    qqline(res.port[,x])
                    plot(res.port[,x],pch=16,xlab='',ylab='Return',main='Residuals plot')
                    abline(h=0,col='red')
                    title(main=paste0('Normality Test for ',names(res.port)[x],' Port. Regression Residuals'),outer=T)                    
                })
                
                par(mfrow=c(1,1))
                textplot('Assets')
                layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
                par(mar=c(4,4,3,4),oma=c(0,0,2,0))
                quiet <- lapply(1:length(res.asset), function(x){
                    hist(res.asset[,x],main='', probability=T, col="slateblue1",xlab='')
                    points(density(res.asset[,x],na.rm=T),col='orange',type='l',lwd=2,ylab='') #density plot
                    qqnorm(coredata(res.asset[,x]), col="slateblue1",main='')
                    qqline(res.asset[,x])
                    plot(res.asset[,x],pch=16,xlab='',ylab='Return',main='Residuals plot')
                    abline(h=0,col='red')
                    title(main=paste0('Normality Test for ',names(res.asset)[x],' Regression Residuals'),outer=T)                    
                })
                
            }
            dev.off() #close device
        })
    
    
    
    ################5th Tab Computations END HERE##########################################    
    
    
})
