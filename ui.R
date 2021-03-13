shinyUI(
    
    #Navigation menu
    navbarPage('Portfolio Manager',theme=shinytheme("flatly"),
               
               
               #1st tab: use column-based layout
               
               #1st tab: Historical Prices sub-tab
               tabPanel('Prices',icon=icon('bar-chart'),
                        fluidPage(
                            
                            #Add Google Analytics code
                            tags$head(includeScript("googleAnalytics.js")),
                            
                            #Change the color and font of the validation errors
                            tags$head(tags$style(HTML("
                                        .shiny-output-error-validation {
                                        color: red;
                                        font-size:18px;
                                        font-family: Tahoma;
                                        }
                                        "))),
                            
                            #Add required JS libraries for datatables
                            tagList(
                                singleton(tags$head(tags$script(src='//cdn.datatables.net/1.10.4/js/jquery.dataTables.min.js',type='text/javascript'))),
                                singleton(tags$head(tags$script(src='//code.jquery.com/jquery-1.11.1.min.js',type='text/javascript')))
                            ),
                            
                            fluidRow(
                                column(4,
                                       wellPanel(
                                           h3('Historical Prices'),
                                           selectInput('src','Source',choices=c('Yahoo!'='yahoo','Google'='google','FRED'='FRED')),
                                           selectizeInput('symInput','Ticker Symbol',choices=tickersList['Ticker Symbol'], selected=tickersList[417,'Ticker Symbol'], options = list(create = TRUE,width='100%')),
                                           selectInput('per0','Prices',choices=c('Monthly','Weekly','Daily'))
                                       )
                                ),
                                column(4,
                                       wellPanel(
                                           h3(' '),
                                           dateInput('dfrom','Start Date',value = '2007-01-01'),
                                           dateInput('dto','End Date')
                                       )
                                ),
                                column(4,
                                       wellPanel(
                                           selectInput('type','Chart Type',choices = c('CandleSticks'='candlesticks','Line'='line')),
                                           selectizeInput('indicators','Technical Indicator',choices=choices,options=list(onInitialize = I('function() { this.setValue(""); }'),placeholder='None')),
                                           checkboxInput('v',strong('Show Volume')),
                                           actionButton('a1','Go!'),
                                           br(),
                                           br(),
                                           p(strong('Export Prices')),
                                           downloadButton('exp1')
                                       )
                                )
                            ),
                            fluidRow(
                                plotOutput('p1',height = 800)
                            )   
                        )
               ),
               
               #1st tab: Historical Returns sub-tab
               tabPanel('Returns',icon=icon('line-chart'),
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(width=3,
                                             h3('Historical Returns'),
                                             helpText('Note: This section uses Yahoo! as the source.'),
                                             selectizeInput('addNew','Ticker Symbol(s)',choices=tickersList['Ticker Symbol'], options = list(create = TRUE,onInitialize = I('function() { this.setValue(""); }')),width='100%',multiple=T),
                                             selectInput('per','Prices',choices=c('Monthly','Weekly','Daily')),
                                             dateInput('dfrom1','Start Date'),
                                             dateInput('dto1','End Date'),
                                             selectInput('type2','Chart Type',choices=c('Lines'='l','Points'='p','Both'='b','Vertical Lines'='h')),
                                             p(strong('Plot historical returns')),
                                             actionButton('a2','Go!'),
                                             br(),
                                             br(),
                                             p(strong('Plot growth of $1 invested')),
                                             actionButton('a4','Go!'),
                                             br(),
                                             br(),
                                             p(strong('Plot drawdowns')),
                                             actionButton('a3.1','Go!'),
                                             br(),
                                             br(),
                                             p(strong('Compare return distributions')),
                                             actionButton('a3.2','Go!')
                                ),
                                mainPanel(width=9,
                                          plotOutput('p2'),
                                          plotOutput('p3'),
                                          plotOutput('p3.1'),
                                          plotOutput('p3.2',height=500,width=800)
                                )
                            )
                        )
               ),
               
               #2nd tab: use sidebar layout
               tabPanel('Study Stocks',icon=icon('search'),
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(width=3,                            
                                             h3('Sample Statistics'),
                                             selectizeInput('addNew2','Ticker Symbol(s)',choices=NULL, options = list(create = TRUE,onInitialize = I('function() { this.setValue(""); }')),width='100%',multiple=T),
                                             dateInput('dfrom2','Start Date'),
                                             dateInput('dto2','End Date'),
                                             selectInput('per2','Prices',choices=c('Monthly','Weekly','Daily')),
                                             selectInput('perc2','VaR Percent (may pick multiple)',choices = c('1%'=0.01,'5%'=0.05),multiple=T),
                                             p(strong('Describe selected assets')),
                                             actionButton('a5','Go!'),
                                             br(),
                                             br(),
                                             p(strong('Plot sample pairwise correlation matrix')),
                                             actionButton('a6',label = 'Go!'),
                                             br(),
                                             br(),
                                             p(strong('View pairwise scatter plots')),
                                             downloadButton('d2scatter','View') 
                                ),
                                mainPanel(width=9,
                                          htmlOutput('t1'),
                                          br(),
                                          br(),
                                          plotOutput('p4',height="auto")
                                )
                            )
                        )
                        
               ),
               
               #3rd tab: use column-based layout
               tabPanel('Assess Stocks',icon=icon('exclamation'),
                        fluidPage(
                            fluidRow(
                                column(3,
                                       wellPanel(
                                           h3('Inputs'),
                                           helpText('Note: This section uses Yahoo! as the source.'),
                                           selectizeInput('addNew3','Ticker Symbol(s)',choices=NULL, options = list(create = TRUE,onInitialize = I('function() { this.setValue(""); }')),width='100%',multiple=T),
                                           dateInput('dfrom3','Start Date'),
                                           dateInput('dto3','End Date'),
                                           selectInput('per3','Prices',choices=c('Monthly','Weekly','Daily'))                                       )
                                ),
                                column(3,
                                       wellPanel(
                                           h3('Bootstrap Statistics'),
                                           selectInput('boot2','Statistic',choices=c('Mean','Median','Std.Deviation','Skewness','Kurtosis','Value-at-Risk','Sharpe Ratio','Correlation')),
                                           uiOutput('perc2.2'),
                                           uiOutput('cor2'),
                                           downloadButton('d2boot',label = 'View')
                                       )
                                ),
                                column(3,
                                       wellPanel(
                                           h3('Rolling Statistics'),
                                           selectInput('roll','Statistic',choices=c('Mean','Median','Std.Deviation','Skewness','Kurtosis','Value-at-Risk','Sharpe Ratio','Correlation')),
                                           uiOutput('perc3'),
                                           uiOutput('cor3'),
                                           numericInput('win','Window/Width',value = 24,min = 1),
                                           downloadButton('d3roll','View')
                                           
                                       )
                                ),
                                column(3,
                                       wellPanel(
                                           h3('Hypothesis Testing'),
                                           selectInput('hyp','Test',choices=c('Normality','Mean','Serial Correlation','Pairwise Correlation')),
                                           uiOutput('hypMean'),
                                           uiOutput('hypNum'),
                                           uiOutput('hypCor'),
                                           downloadButton('d3hyp','View')                                           
                                       )    
                                )
                            )
                        )
               ),
               
               #4th tab: use sidebar layout
               tabPanel('Build Portfolio',icon=icon('magic'),
                        fluidPage(
                            sidebarLayout(
                                sidebarPanel(width=3,
                                             h3('Portfolio Inputs'),
                                             helpText('Note: This section uses Yahoo! as the source.'),
                                             selectizeInput('addNew4','Ticker Symbol(s)',choices=NULL, options = list(create = TRUE,onInitialize = I('function() { this.setValue(""); }')),width='100%',multiple=T),
                                             selectizeInput('indSym','Index Symbol',choices='^GSPC',selected='^GSPC', options = list(create = TRUE)),
                                             dateInput('dfrom4','Start Date'),
                                             dateInput('dto4','End Date'),
                                             selectInput('per4','Prices',choices=c('Monthly','Weekly','Daily')),
                                             numericInput('rf4','Period risk-free interest rate',value = 0 ,step = 0.001),
                                             checkboxInput('shorts4','Allow short sales'),
                                             checkboxInput('annual','Annualize portfolio performance'),
                                             checkboxInput('sim4','Estimate covariances via SIM',value = T),
                                             p(strong('View Risk/Reward plot and portfolio data')),
                                             actionButton('front4','Go!'),
                                             h3('Portfolio Optimizer'),
                                             selectInput('target4','Objective',choices = c('Target period expected return','Target period std. deviation')),
                                             numericInput('val4','Enter period expected value',value = 0,step=0.01),
                                             selectInput('on4','Construct portfolio on the',choices = c('Efficient Frontier','Capital Allocation Line')),
                                             p(strong('Update Risk/Reward plot and portfolio data')),
                                             actionButton('opt4','Go!')
                                ),
                                
                                mainPanel(width=9,
                                          plotOutput('p5',height=500),
                                          br(),
                                          br(),
                                          uiOutput('t2title'),
                                          htmlOutput('t2'),
                                          br(),
                                          uiOutput('t3title'),
                                          htmlOutput('t3'),
                                          br(),
                                          htmlOutput('col1'),
                                          plotOutput('p6',height=500),
                                          plotOutput('p7',height=500)
                                )
                                
                            )
                        )           
               ),
               
               tabPanel('Assess Portfolio',icon=icon('exclamation'),
                        fluidPage(
                            fluidRow(
                                column(4,
                                       wellPanel(
                                           h3('Bootstrap Statistics'),
                                           helpText(strong('Note: You have to build your portfolio under Build Portfolio tab first.')),
                                           selectInput('boot5',label = 'Selection',choices=c('Portfolio Frontier','Global Minimum Portfolio','Tangency Portfolio','Efficient Portfolio','Optimal Portfolio','Assets')),
                                           uiOutput('boot5port'),
                                           uiOutput('boot5assets'),
                                           downloadButton('d5boot','View')
                                       )
                                ),
                                column(4,
                                       wellPanel(
                                           h3('Rolling Statistics'),
                                           helpText(strong('Note: You have to build your portfolio under Build Portfolio tab first.')),
                                           selectInput('roll5',label = 'Selection',choices=c('Global Minimum Portfolio','Tangency Portfolio','Efficient Portfolio','Optimal Portfolio','Assets')),
                                           numericInput('win5','Window/Width',value = 24,min = 1),
                                           downloadButton('d5roll','View') 
                                       )
                                ),
                                column(4,
                                       wellPanel(
                                           h3('Hypothesis Testing'),
                                           helpText(strong('Note: You have to build your portfolio under Build Portfolio tab first.')),
                                           selectInput('hyp5','Test',choices=c('Alpha=0','Normality of Residuals')),
                                           downloadButton('hypTest5','View') 
                                           
                                       )
                                )
                            )
                        )
               )           
    )      
    
)


