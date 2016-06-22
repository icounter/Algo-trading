library(shiny)
library(shinysky)
library(shinyAce)
options(warn=-1)
modes <- getAceModes()
themes <- getAceThemes()
MAXIMUM_LOSS<<--99999999999999
trans_generaed<<-0
displayList<-c("expo","power","log","combo")
shinyUI(fluidPage(
  img(src="logo.jpg", height = 50, width = 200),
#   titlePanel("Mean-Variance Framework and Exponential Utility Function"),    
#   helpText("Note:This is only a samll example to show the consistency of exponential utility function and mean-variance framework"),
#   helpText("Set the assumption step by step,number of samples means how many simulation paths you would use when using exponential utility" 
#            ),
#   fluidRow(
#     column(1, 
#            numericInput("assets_num", 
#                         label = h3("Assets Number"), 
#                         value = 4)),
#     column(2, 
#            numericInput("lambda", label = h3("Lambda"),
#                         value = 0.5)),
#     column(2,textInput("asset_ret", label = h3("Return of each asset"), value = "0.08,0.04,0.04,0.0045")),
#     column(2,textInput("asset_var", label = h3("Variance of each asset"), value="0.007225,0.000625,0.000625,0")),
#     column(2,textInput("asset_corr", label = h3("correlation of each asset"), value = "0.6,0.6,0,0.6,0,0")),
#     column(2, sliderInput("sample_number", label = h3("number of samples"),min = 10000, max = 1000000,step=10000, value =100000)),
# 
#     column(2,actionButton("go1","start to compute"))
#   ),hr(),fluidRow( 
#     plotOutput("ggplot")),
#  hr(),
  titlePanel("Real scenario considering transaction cost and set up Bayesian Net"),
  hr(),  
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv')),
      helpText("Upload your input csv file here and it will automatically update your assumptions"),
      helpText("You can also update your assumtions by hand"),
      tags$hr(),
      checkboxInput('header', 'Header', TRUE),
      radioButtons('sep', 'Separator',
                   c(Comma=',',
                     Semicolon=';',
                     Tab='\t'),
                   ','),
      radioButtons('quote', 'Quote',
                   c(None='',
                     'Double Quote'='"',
                     'Single Quote'="'"),
                   '"')
    ),
    mainPanel(
      tableOutput('contents')
    )
  ),
  titlePanel("1.Setup normal return part of assets"),

  hr(),
  fluidRow(
    column(1, 
           numericInput("assets_num1", 
                        label = h3("Assets Number"), 
                        value = 4)),  helpText("Note:All the below inputs are comma delimited"),
    column(2,textInput("asset_name1", label = h3("Names of each Asset"), value = "CLO,senior_loan,NPL1stpay,cash")),
    column(2,textInput("asset_ret1", label = h3("Return of each asset"), value = "0.08,0.04,0.04,0.0045")),
    column(2,textInput("asset_vol1", label = h3("Volatility of each asset"), value="0.085,0.025,0.025,0")),
    column(2,textInput("asset_corr1", label = h3("correlation of each asset"), value = "0.6,0.6,0,0.6,0,0"))
   
),
hr(),
titlePanel("2.Setup Extreme part of assets"),
helpText("Note:Define it before you want you want to do assets optimization"),
sidebarLayout(
  sidebarPanel(textInput("extreme_stress_loss", label = h3("Extreme Loss of each Asset"), value = "-.2,-0.03,-0.03,0.0065"),
               helpText("Note:This is the return of each assests when extreme event happens"),
               textInput("affect_relation", label = h3("Affect Relationship between assets"), value ="Hidden1;CLO+Hidden1;senior_loan+Hidden1;NPL1stpay+Hidden1;cash"),
               helpText("Define Structure of your BN,semicomma delimited.and A+B means A is affected by B"),
               actionButton("go6","Generate Bayesian Net plot"),
               helpText("outplot the structure of BN at the right panel"),
               textInput("prob_table", label = h3("Probability table"), value ="10,90;80,20,10,90;80,20,10,90;80,20,10,90;0,100"),
               helpText("Define conditional probability table for each node,semicomma delimited"),
         actionButton("go5","Show BN conditional Table"),
         helpText("When you have moret than 10 assets,PLEASE DONT TOUCH IT!")
         ),
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_bayesian"),  DT::dataTableOutput("hotable2"))),
    helpText("In the plot,A->B means A will affect  B"),
    helpText("Joint probability table shows the probability value in each scenario,1 means  extreme events will happen and 0 means extreme events won't happen")
            )
),
hr(),
titlePanel("3.Set Transaction Cost & Finance cost"),
sidebarLayout(
  sidebarPanel(
    numericInput("principal1", label = h3("Principal($M)"), value = 2669),
    helpText("How much money we hold right now"),
    textInput("w_now", label = h3("Present Portofolio Position"), value = "0,0,0,0"),
    helpText("present portofolio position,comma delimited"),
    textInput("w1", label = h3("Tentative Portofolio Position"), value = "0,0.25,0.25,0"),
    helpText("The portofolio position you would like to change to,click compute transaction cost to see how much you should pay for position change,comma delimited"),
    numericInput("con_number", label = h3("Number of transaction cost condition nodes"), value = 1),
    textInput("condtion_name", label = h3("Interval Spread"), value = "!=0"),
    actionButton("go4","generate transcation cost Matrix"),
    numericInput("con_number2", label = h3("Number of finance cost condition nodes"), value = 1),
    textInput("condtion_name2", label = h3("Interval Spread"), value = "!=0"),
    actionButton("go7","generate finance cost Matrix"),
    helpText("You can make your piecewise linear assumption here,input the number of your intervals")
    
    ),
  mainPanel(
    tabPanel("trans_cost"
             ,h4("Transaction Cost matrix")
             ,div(class="well container-fluid"
                  ,hotable("hotable1")
             )),  
    tabPanel("finan_cost" ,h4("Finance Cost matrix")
               ,div(class="well container-fluid"
               ,hotable("hotable5")
               )),
    titlePanel("How much you will spend on transaction cost and finance cost"), 
    actionButton("go3","start to compute transaction cost and finance cost"),
    textOutput("tentative_transcost")
  )
),
hr(),
titlePanel("4.Select Utility Fucntion"),
sidebarLayout(
  sidebarPanel(
    selectInput("uti", label = h3("Utility Function"),choices = list("Exponential Utility" = 'expo', "Power Utility" = 'power', "Log Utility" = 'log',"Risk seeking combination"='combo'), selected = 'expo'),
    helpText("Four kinds of utility functions to choose from:power,log,expo and comb."),
    conditionalPanel("input.uti!='combo'",
    numericInput("lambda1",label = h3("Input Lambda"),  value = 5),
#    numericInput("mid",  label = h3("Mid return(e.g. 0)"), step=0.01,value = 0),
#    numericInput("mini",  label = h3("downside return(e.g. -0.03)"), step=0.01,value = -0.07),
#    numericInput("maxi",  label = h3("upside return(e.g. 0.10)"), step=0.01,value = 0.07),
#    numericInput("prob",  label = h3("probability to fail"), step=0.01,value = 0.2210),
   helpText("This is a two-scenario selection question:(1) you can get the mid return with 100%;(2) with a probability of p to get downside lose and (1-p) to win upside return."),
   helpText("Make these two scenarios indifference to you")
    ),
   conditionalPanel("input.uti=='combo'",
                  numericInput("x_extreme",label = h3("extreme loss tolerance"),step=0.01,value = -0.3),
     numericInput("x_downturning",label = h3("downside turning return"),step=0.01,value = -0.05),
     numericInput("x_mid2",label = h3("mid return between downside and upside return"),step=0.01,value = 0),
     numericInput("x_upturning",label = h3("upside turning return"),step=0.01,value = 0.05),
   numericInput("prob2",label = h3("prob to get downside return"),step=0.01,value = 0.8),
   helpText("This assumes when a PM has a small negative return or a small positive return, he will be risk seeking to get more positive return."),
   helpText("downside turning return means before you lose this much he is risk seeking, after that he is risk averse"),
   helpText("upside turning return means before you win this much he is risk seeking, after that he is risk averse"),
   helpText("Mid return and probability to fail is the same idea as other three functions,to make yourself indifference between these two scenarios"))
   
   ),  
    mainPanel(
      conditionalPanel("input.uti=='combo'",hr(), 
                       sliderInput("x_range2", label = h3("Show utility in the range"), min = -1, max = 1, step=0.01,value = c(-0.3, 0.2)),
                       plotOutput("utility_out2")),
      conditionalPanel("input.uti!='combo'",
#       titlePanel("Based on your input suggested lambda is:"), 
#       textOutput("tentative_lambda"),
#       hr(),
      titlePanel("Reference plot for lambda:"), 
helpText("By changing the range of negative return you can change the display range of right plot"),
helpText("Left plot is the plot for (1) zero return,(2) equal return and loss in the other scenario and the probability you set to lose when makes this an indifference choice"),
helpText("Right plot is the plot for (1) zero return,(2) equal probability of win and lose,if knowing how much you will lose in the bad scenario how much you will need to win to compensate for it"),
   sliderInput("neg_ret_ran", label = h3("select the range of negative return"), min = -0.2, max = 0, step=0.01,value = -0.11),
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("plot_ut_zero_equal"),plotOutput("plot_ut_zero_equal_2")),
      hr(),
sliderInput("x_range", label = h3("Show utility in the range"), min = -1, max = 1, step=0.01,value = c(-0.3, 0.2)),
      plotOutput("utility_out"))
)),
hr(),
titlePanel("5.Set bounds & subjective value"),
fluidRow(
  helpText("The lower bound of your weights,can't short then make lower bound to be a zero vector"),
  column(2, textInput("lower_bound", 
                      label = h3("Lower bounds for assets"), 
                      value = "0,0,0,0")),
  helpText("The upper bound of your weights,if you can't do leverage then make upper bound to be a vector of one"),
  column(2,textInput("upper_bounds", label = h3("Upper bounds for assets"), value = "100,100,100,100")),
  column(2, sliderInput("sample_number1", label = h3("number of samples"),min = 10000, max = 1000000,step=10000, value =50000)),
  helpText("Subjective Value parameter is the probability you assigned that in the next interval at least one extreme event will happen"),
  column(2, sliderInput("subjective_k", label = h3("Subjective Value"),min = 0.0, max =1.0,step=0.01, value =0.01)),
  column(2, sliderInput("maxeval", label = h3("maximum evaluation rounds"),min = 0, max =2000,step=100, value =1000)),
  helpText("maximum evaluation rounds controls the speed for you to find the optimized solution")
),hr(),
fluidRow(
  column(6, aceEditor("convexity_bounds", value="##input convexity bounds here(hint format is g(x)>=0)
#eval_g0 <- function(w1, w_now1=w_now,beta11=beta1,rand21=rand2,loss11=loss1,pro_dict1=pro_dict,k1=k,trans_cost1=trans_cost,principal11=principal1)
#{
#  h<-numeric(3)
#  h[1]<-(1.09*0.0065+0.0408-0.0065-0.0215)*w1[1]+(0.0465-0.0065-0.015)*w1[2]+(0.0429-0.0065-0.0175)*w1[3]-0.09+1/3*w1[4]
#  h[3]<-1-w1[1]-w1[4]
#  h[2]<-2-sum(w1)
#  return(h)
#}")),
  column(6,aceEditor("linear_bounds", value="##input linear bounds here
#eval_h0 <- function(w1, w_now1=w_now,beta11=beta1,rand21=rand2,loss11=loss1,pro_dict1=pro_dict,k1=k,trans_cost1=trans_cost,principal11=principal1)
#{
#  return(sum(w1)-1)
#}")),
  column(2,actionButton("go2","start to compute"))
  ),
hr()
,fluidRow(plotOutput("opt_weights"),hr(),tableOutput("tablle")),
hr(),
titlePanel("6.Optimize weights table"),
sidebarLayout(
  sidebarPanel(textInput("lambdas", label = h3("lambdas you are interested in"), value = "5,10"),
textInput("ks", label = h3("probabilities of at least one extreme events to happen"), value = "0.2,0.5"),
helpText("Above are the lambdas and subjective_ks you are interested in,you may input multiple lambdas and ks,comma delimited"),
  checkboxGroupInput("uti2", label = h3("Utility function typies you are interested in"), 
                               choices = list("Exponential Utility" = 'expo', "Power Utility" = 'power', "Log Utility" = 'log',"Risk seeking combination"='combo'), selected = 'expo'),
  conditionalPanel(condition="input.uti2.indexOf('combo')>-1",hr(),
                   helpText("Below is just for COMBO function"),
                   numericInput("x_extreme2",label = h3("extreme loss tolerance"),step=0.01,value = -0.3),
                   numericInput("x_downturning2",label = h3("downside turning return"),step=0.01,value = -0.05),
                   numericInput("x_mid22",label = h3("mid return between downside and upside return"),step=0.01,value = 0),
                   numericInput("x_upturning2",label = h3("upside turning return"),step=0.01,value = 0.05),
                   numericInput("prob22",label = h3("prob to get downside return"),step=0.01,value = 0.8)
                   ),hr(),
  sliderInput("sample_number2", label = h3("number of samples to generate"),min = 10000, max = 1000000,step=10000, value =50000),
  actionButton("go9","click here to show tables"),
  radioButtons("filetype", "File type:",choices = c("csv", "tsv")),
 downloadButton('downloadData','Download'),
 helpText("This download function is only available when open in browser")
  ),mainPanel(
    titlePanel("The optimized weights of the assets are:"),
    conditionalPanel(condition="input.uti2.indexOf('expo')>-1||input.uti2.indexOf('log')>-1||input.uti2.indexOf('power')>-1",tableOutput("hotable3")),
    conditionalPanel(condition="input.uti2.indexOf('combo')>-1",hr(),titlePanel("Using Combination Utility function:"),tableOutput("hotable4"))
))

)
)