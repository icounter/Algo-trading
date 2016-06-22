# install.packages(c("ggplot2","gRbase","MASS","gRain","copula","psych","nloptr","rpsychi","plyr","shinysky","DT","rootSolve"))
# install.packages("devtools")
# library(devtools)
# install_github("shinyAce", "trestletech")
# devtools::install_github("AnalytixWare/ShinySky")
library(ggplot2)
library(MASS)
#library(gRbase)
library(gRain)
#library(LambertW)
library(copula)
library(psych)
library(nloptr)
library(rpsychi)
library(plyr)
library(shinysky)
library(DT)
library(rootSolve)
library(stringr)
source("script.R")
# server.R
shinyServer(
  function(input, output,session) {
    output$contents <- renderTable({
      inFile <- input$file1
      if(is.null(inFile)){
        return(NULL)
      }else{
      input_data<<-read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      }
    })
   observe({
     if(!is.null(input$file1)){
    
     updateNumericInput(session,"assets_num1",value=nrow(input_data))
     name<-as.character(input_data[,1])
     for(i in 1:length(name)){
       name[i]<-paste0(strsplit(name[i]," ")[[1]],collapse = "")
     }
     updateTextInput(session,"asset_name1",value=paste0(name,collapse = ","))
     updateTextInput(session,"asset_vol1",value=paste0(input_data[,5],collapse = ","))
     updateTextInput(session,"asset_ret1",value=paste0(input_data[,3],collapse = ","))
     updateTextInput(session,"extreme_stress_loss",value=paste0(input_data[,4],collapse = ","))
     N<-nrow(input_data)
     a=c()
     for( i in 1:(N-1)){
       a<-c(a,as.numeric(input_data[i,(6+i):(5+N)]))
     }
     updateTextInput(session,"asset_corr1",value=paste0(a,collapse = ","))
     updateTextInput(session,"w_now",value=paste0(input_data[,2],collapse = ","))
     updateTextInput(session,"w1",value=paste0(input_data[,2],collapse = ","))
     updateTextInput(session,"lower_bound",value=paste0(input_data[,which(colnames(input_data)=="lower_bounds")],collapse = ","))
     updateTextInput(session,"upper_bounds",value=paste0(input_data[,which(colnames(input_data)=="upper_bounds")],collapse = ","))
     bn<-as.character(input_data[,which(colnames(input_data)=='BN')])
     bn_hidden<-as.character(input_data[,which(colnames(input_data)=='BN_hidden')])
     BN<-paste0(bn_hidden[which(nchar(bn_hidden)!=0)],collapse = ";")
     BN2<-paste0(name[which(nchar(bn)!=0)],"+",bn[which(nchar(bn)!=0)])
     BN<-paste0(BN,";",paste0(BN2,collapse = ";"),";")
     BN<-paste0(BN,name[which(nchar(bn)==0)],collapse = ";")
     updateTextInput(session,"affect_relation",value=BN)
     con=as.character(input_data[,which(colnames(input_data)=='cond')])
     con_hidden=as.character(input_data[,which(colnames(input_data)=='cond_Hidden')])
     condd<-paste0(con_hidden[which(nchar(con_hidden)!=0)],collapse=";")
     condd<-paste0(condd,";",paste0(con[which(nchar(bn)!=0)],collapse = ";"),";")
     condd<-paste0(condd,con[which(nchar(bn)==0)],collapse = ";")
     updateTextInput(session,"prob_table",value=condd)
       trans_generaed<<-1
       N1<-which(colnames(input_data)=="lower_bounds")
       dddd<-input_data[,(N+6):(N1-1)]
       N2<-input_data[2,6]
       N3<-N1-N-6-N2
       trans_cost1<-data.frame((matrix(0,nrow=N,ncol=N2+1)))
       finan_cost1<-data.frame((matrix(0,nrow=N,ncol=N3+1)))
       if(N2==1){
         string1<-rep(name,1)
         string1=paste0(string1,"_trans_cost")
         trans_cost1[,1]<-string1
         trans_cost1[,2]=dddd[,1]
         # trans_cost1[(N+1):(2*N),2:(N2+1)]=dddd[,(N2+1):(2*N2)]
         colnames(trans_cost1)<-c("Asset_name","!=0")
       }else{
         string1<-rep(name,1)
         string1=paste0(string1,"_trans_cost")
         # string1[(N+1):(2*N)]=paste0(string1[(N+1):(2*N)],"_finance_cost")
         trans_cost1[,1]<-string1
         trans_cost1[,2:(N2+1)]=dddd[,1:N2]
         # trans_cost1[(N+1):(2*N),2:(N2+1)]=dddd[,(N2+1):(2*N2)]
         eq<-colnames(dddd)[1:N2]
         for(i in 1:N2){
           eq[i]=str_replace_all(eq[i],"and","&")
           eq[i]=str_replace_all(eq[i],"s","<")
           eq[i]=str_replace_all(eq[i],"l",">")
         }
         colnames(trans_cost1)<-c("Asset_name",eq)
       }
       if(N3==1){
         string1<-rep(name,1)
         string1=paste0(string1,"_finance_cost")
         finan_cost1[,1]<-string1
         finan_cost1[,2]=dddd[,(N2+1)]
         # trans_cost1[(N+1):(2*N),2:(N2+1)]=dddd[,(N2+1):(2*N2)]
         colnames(finan_cost1)<-c("Asset_name","!=0")
       }else{
         string1<-rep(name,1)
         string1=paste0(string1,"_finance_cost")
         # string1[(N+1):(2*N)]=paste0(string1[(N+1):(2*N)],"_finance_cost")
         finan_cost1[,1]<-string1
         finan_cost1[,2:(N3+1)]=dddd[,(N2+1):(N3+N2)]
         # trans_cost1[(N+1):(2*N),2:(N2+1)]=dddd[,(N2+1):(2*N2)]
         eq<-colnames(dddd)[(N2+1):(N2+N3)]
         for(i in 1:N3){
           eq[i]=str_replace_all(eq[i],"and","&")
           eq[i]=str_replace_all(eq[i],"s","<")
           eq[i]=str_replace_all(eq[i],"l",">")
         }
         colnames(finan_cost1)<-c("Asset_name",eq)
       }
      trans_cost1<<-trans_cost1
      finan_cost1<<-finan_cost1
    }   
   })
    weights1<-eventReactive(input$go1,{
   call_scenario_no_cost_no_BN(input$assets_num,input$lambda,input$asset_ret,input$asset_var,input$asset_corr,input$sample_number)
      
    })
    output$ggplot <- renderPlot({
      weights<-weights1()
       ggplot(data=weights, aes(x=asset_name, y=weight, fill=methods)) +
        geom_bar(stat="identity", position=position_dodge())+
         geom_text(aes(label=round(weight,digits = 4)), vjust=1.6,
                   position = position_dodge(0.9), size=5)
      })
    con_tabe<-eventReactive(input$go5,{
      bayesian_matrix1<-data.frame(t(bayesian_matrix1))
      bayesian_matrix1[,1]<-sprintf("%.6f",bayesian_matrix1[,1])
      bayesian_matrix1[,2:ncol(bayesian_matrix1)]<-as.data.frame(lapply(bayesian_matrix1[,2:ncol(bayesian_matrix1)], format_num))
      return(bayesian_matrix1)
    })
    format_num <- function(col) {
      if (is.numeric(col))
        sprintf('%1.0f', col)
      else
        col
    }
    
    output$hotable2 <- DT::renderDataTable(
      DT::datatable(con_tabe(), options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
      #colnames(bayesian_matrix2)
    )))
    
    plot_bay<-eventReactive(input$go6,{
      cond_tab=build_cond(affect_relation=input$affect_relation,prob_table=input$prob_table)
      plist<-compileCPT(cond_tab)
      net<-grain(plist) 
      cond_tab=build_cond(affect_relation=input$affect_relation,prob_table=input$prob_table)
      asset_name1=strsplit(input$asset_name1,",")[[1]]
      bayesian_matrix1<<-bayesian_matrix(cond_tab,asset_name=asset_name1)
      iplot(net)
    })
    output$plot_bayesian<-renderPlot(
      {
        return(plot_bay())
      }
    )
    format_num2 <- function(col) {
      if (is.numeric(col))
        sprintf('%.4f', col)
      else
        col
    }
    finan_cost2<-eventReactive(input$go7,{
      if(input$assets_num1==4&input$con_number2==1&trans_generaed==0){
        finan_cost1<-data.frame((matrix(0,nrow=input$assets_num1,ncol=input$con_number2+1)))
        col_names<-strsplit(input$condtion_name2,",")[[1]]
        colnames(finan_cost1)<-c("Asset_name",col_names)
        asset_name<-strsplit(input$asset_name1,",")[[1]]
        string2<-rep(asset_name,1)
        string2[1:input$assets_num1]=paste0(string2[1:input$assets_num1],"_finance_cost")
        finan_cost1[,1]<-string2
        finan_cost1[1,2]<-c(-0.028)
        finan_cost1[2,2]<-c(-0.0215)
        finan_cost1[3,2]<-c(-0.024)
        finan_cost1[4,2]<-c(-0.0065)
        finan_cost<<-finan_cost1
        return(finan_cost)
    }else if(trans_generaed!=0){
      finan_cost<<-finan_cost1
      return(finan_cost)}else{
        finan_cost1<-data.frame((matrix(0,nrow=input$assets_num1,ncol=input$con_number2+1)))
        col_names<-strsplit(input$condtion_name2,",")[[1]]
        colnames(finan_cost1)<-c("Asset_Name",col_names)
        asset_name<-strsplit(input$asset_name1,",")[[1]]
        string2<-rep(asset_name,1)
        string2[1:input$assets_num1]=paste0(string2[1:input$assets_num1],"_finance_cost")
        finan_cost1[,1]<-string1
        finan_cost<<-finan_cost1
        return(finan_cost1)
      }
      }) 
    output$hotable5 <- renderHotable({
        finan_cost<-finan_cost2()
      }, readOnly = FALSE)
    observe({
      df <- hot.to.df(input$hotable5)
      #        a<-colnames(df)
      #        df<-as.data.frame(lapply(df, format_num2))
      #        colnames(df)<-a
      #       #df1<-formattable(df,digits=6,format='f')
      print(df)
    })

    trans_cost2<-eventReactive(input$go4,{
     if(input$assets_num1==4&input$con_number==1&trans_generaed==0){
       trans_cost1<-data.frame((matrix(0,nrow=input$assets_num1,ncol=input$con_number+1)))
       col_names<-strsplit(input$condtion_name,",")[[1]]
       colnames(trans_cost1)<-c("Asset_name",col_names)
       asset_name<-strsplit(input$asset_name1,",")[[1]]
       string1<-rep(asset_name,1)
       string1[1:input$assets_num1]=paste0(string1[1:input$assets_num1],"_trans_cost")
       trans_cost1[,1]<-string1
       trans_cost1[1,2]<-c(0)
       trans_cost1[2,2]<-c(0)
       trans_cost1[3,2]<-c(0)
       trans_cost1[4,2]<-c(0)
       trans_cost<<-(trans_cost1)
       return(trans_cost)
     }else if(trans_generaed!=0){
     trans_cost<<-trans_cost1
     return(trans_cost)
     }else{
      trans_cost1<-data.frame((matrix(0,nrow=input$assets_num1,ncol=input$con_number+1)))
     col_names<-strsplit(input$condtion_name,",")[[1]]
     colnames(trans_cost1)<-c("Asset_Name",col_names)
     asset_name<-strsplit(input$asset_name1,",")[[1]]
     string1<-rep(asset_name,1)
     string1[1:input$assets_num1]=paste0(string1[1:input$assets_num1],"_trans_cost")
     trans_cost1[,1]<-string1
     trans_cost<<-trans_cost1
     return(trans_cost)
       
     }
    })
    
    # hotable
    output$hotable1 <- renderHotable({
      trans_cost<-trans_cost2()
    }, readOnly = FALSE)
    observe({
      df <- hot.to.df(input$hotable1)
#        a<-colnames(df)
#        df<-as.data.frame(lapply(df, format_num2))
#        colnames(df)<-a
#       #df1<-formattable(df,digits=6,format='f')
      print(df)
    })
    tentative_trans<-eventReactive(input$go3,{
        w_now<-strsplit(input$w_now,",")[[1]]
      w_now<-as.double(w_now)
      w1<-strsplit(input$w1,",")[[1]]
      w1<-as.double(w1)
      principal1<-(input$principal1)
      trans_cost<-hot.to.df(input$hotable1)
      finan_cost<-hot.to.df(input$hotable5)
      cost(w_now,w_1=w1,trans_cost,finan_cost,principal1)
    })
    output$tentative_transcost<-renderText({ 
      tentative_trans()
    })
#     output$tentative_lambda<-renderText({
#       b<-calculate_l(input$uti,input$mid,input$mini,input$maxi,input$prob)
#     })
    output$plot_ut_zero_equal<-renderPlot({
      ret<-seq(from=0,to=0.15,by=0.01)
      lam<-c(input$lambda1)
      gene<-data.frame(matrix(0,nrow=(length(lam)*length(ret)),ncol=3))
      colnames(gene)<-c("lambda","ret","probability")
      if(input$uti=='expo'){
        for(i in 1:length(lam)){
          for(j in 1:length(ret)){
            if(j==1){
              gene[((i-1)*length(ret)+j),]=c(lam[i],ret[j],0.5)
            }else{
              b=(1-exp(-lam[i]*ret[j]))/(exp(lam[i]*ret[j])-exp(-lam[i]*ret[j]))
              gene[((i-1)*length(ret)+j),]=c(lam[i],ret[j],b)
            }
          }
        }
      }else if(input$uti=='power'){
        for(i in 1:length(lam)){
          for(j in 1:length(ret)){
            if(j==1){
              gene[((i-1)*length(ret)+j),]=c(lam[i],ret[j],0.5)
            }else{
              b=((1+ret[j])^(1-lam[i])-1)/((1+ret[j])^(1-lam[i])-(1-ret[j])^(1-lam[i]))
              gene[((i-1)*length(ret)+j),]=c(lam[i],ret[j],b)
            }
          }
        }
        gene=na.omit(gene)
      }else{
        for(i in 1:length(lam)){
          for(j in 1:length(ret)){
            if(j==1){
              gene[((i-1)*length(ret)+j),]=c(lam[i],ret[j],0.5)
            }else{
              b=log(1+lam[i]*ret[j])/(log(1+lam[i]*ret[j])-log(1-lam[i]*ret[j]))
              gene[((i-1)*length(ret)+j),]=c(lam[i],ret[j],b)
            }
          }
        }
        gene=na.omit(gene)
      }
      gene[,3]<-format_num2(gene[,3])
      gene$probability<-1-as.double(gene$probability)
      ggplot(data=gene,aes(x=ret,y=probability,group=lambda,colour=lambda))+geom_line()+geom_text(aes(label=probability), size = 3,check_overlap = TRUE)+scale_y_continuous(breaks = round(seq(min(gene$probability), max(gene$probability), by = 0.03),5))+scale_x_continuous(breaks = round(seq(min(gene$ret), max(gene$ret), by = 0.01),5)) +xlab("Three-Month Return +/-")+ylab("probability of Win")
    })
    output$plot_ut_zero_equal_2<-renderPlot({
      neg_ret<-seq(from=0,to=input$neg_ret_ran,by=-0.01)
      lam<-c(input$lambda1)
      gene<-data.frame(matrix(0,nrow=(length(lam)*length(neg_ret)),ncol=3))
      colnames(gene)<-c("lambda","neg_ret","pos_ret")
      if(input$uti=='expo'){
        for(i in 1:length(lam)){
          for(j in 1:length(neg_ret)){
            if(j==1){
              gene[((i-1)*length(neg_ret)+j),]=c(lam[i],neg_ret[j],0)
            }else{
              b=log(2-exp(-lam[i]*neg_ret[j]))/(-lam[i])
              gene[((i-1)*length(neg_ret)+j),]=c(lam[i],neg_ret[j],b)
            }
          }
        }
        gene=na.omit(gene)
      }else if(input$uti=='power'){
        for(i in 1:length(lam)){
          for(j in 1:length(neg_ret)){
            if(j==1){
              gene[((i-1)*length(neg_ret)+j),]=c(lam[i],neg_ret[j],0)
            }else{
              b=(2-(1+neg_ret[j])^(1-lam[i]))^(1/(1-lam[i]))-1
              gene[((i-1)*length(neg_ret)+j),]=c(lam[i],neg_ret[j],b)
            }
          }
        }
        gene=na.omit(gene)
      }else{
        for(i in 1:length(lam)){
          for(j in 1:length(neg_ret)){
            if(j==1){
              gene[((i-1)*length(neg_ret)+j),]=c(lam[i],neg_ret[j],0)
            }else{
              b=(1/(1+lam[i]*neg_ret[j])-1)/lam[i]
              gene[((i-1)*length(neg_ret)+j),]=c(lam[i],neg_ret[j],b)
            }
          }
        }
        gene=na.omit(gene)
      }
      
      gene[,3]<-format_num2(gene[,3])
      gene$pos_ret<-as.double(gene$pos_ret)
      gene$neg_ret<-as.double(gene$neg_ret)
      ggplot(data=gene,aes(x=neg_ret,y=pos_ret,group=lambda,colour=lambda))+geom_line()+geom_text(aes(label=pos_ret), size = 3,check_overlap = TRUE)+xlab("Return in Bad Scenario")+ylab("Return in good Scenario")+scale_x_reverse(breaks = round(seq(from=min(as.double(gene$neg_ret)), to=max(as.double(gene$neg_ret)), by = 0.01),100))+scale_y_continuous(breaks = round(seq(from=min(as.double(gene$pos_ret)), to=max(as.double(gene$pos_ret)), by = 0.03),100))
    })
    output$utility_out2<-renderPlot({
      res11<-utility_solve(x_extreme =input$x_extreme,x_downturning =input$x_downturning,x_mid = input$x_mid2,x_upturning = input$x_upturning,prob2 = input$prob2)
      ggplot(data.frame(x=input$x_range2),aes(x))+stat_function(fun=vutility,args=list(x_extreme1=x_extreme,x_downturning1=x_downturning,k_21=k_2,k_11=k_1,x_11=x_1),geom='line',aes(colour="combination utility"))+xlab("return")+ylab("utility value")
    })
    output$utility_out<-renderPlot({
      if(input$uti=='log'){
        ggplot(data.frame(x=input$x_range),aes(x))+stat_function(fun=function(x)log(1+(input$lambda1*x)),geom='line',aes(colour="log utility"))+xlab("return")+ylab("utility value")
      }
      else if(input$uti=='expo'){
        ggplot(data.frame(x=input$x_range),aes(x))+stat_function(fun=function(x)((1-exp(-input$lambda1*x))/input$lambda1),geom='line',aes(colour="exponential utility"))+xlab("return")+ylab("utility value")
      }
      else{
        ggplot(data.frame(x=input$x_range),aes(x))+stat_function(fun=function(x)(1/(1-input$lambda1)*((1+x)^(1-input$lambda1)-1)),geom='line',aes(colour="exponential utility"))+xlab("return")+ylab("utility value")
      }
    })
    weights2<-eventReactive(input$go2,{
      if(input$uti!="combo"){
        call_scenario_cost_BN(input$uti,input$assets_num1,input$lambda1,input$asset_ret1,input$asset_vol1,
                            input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                            input$principal1,hot.to.df(input$hotable1),hot.to.df(input$hotable5),input$w_now,input$lower_bound,input$upper_bounds,input$subjective_k,
                            input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval)}else{
        call_scenario_cost_BN2(input$assets_num1,input$x_extreme,input$x_downturning,input$x_mid2,input$x_upturning,input$prob2,input$asset_ret1,input$asset_vol1,
                                                    input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                                                    input$principal1,hot.to.df(input$hotable1),hot.to.df(input$hotable5),input$w_now,input$lower_bound,input$upper_bounds,input$subjective_k,
                                                    input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval)
                              
      }
      })
      #method,assets_num,lambda,asset_ret,asset_vol,asset_corr,sample_number,extreme_stress_loss,cond_table=NULL,principal,
    #trans_cost,w_now,constraints,bounds,k,asset_name1=NULL
    output$opt_weights<-renderPlot({
      weights3<<-weights2()
      weights3$asset_name<-as.character(weights3$asset_name)
      ggplot(data=weights3, aes(x=asset_name, y=weights, fill=method)) +
        geom_bar(stat="identity", position=position_dodge())+
        geom_text(aes(label=round(weights,digits = 4),x=asset_name), vjust=1.6, size=5)
    })
    format_num8 <- function(col) {
      if (is.numeric(col))
        sprintf('%.4f', col)
      else
        col
    }
    output$tablle<-renderTable({
      weights3<<-weights2()
       ww_1<-weights3$weights[-length(weights3$weights)]
       mat=cost2(w_now,ww_1,trans_cost,finan_cost,principal1)
       colnames(mat)<-c("trans_cost","finance_cost")
       mat<-data.frame(mat)
       name<-as.character(weights3$asset_name)
       mat<-cbind(name[-length(name)],mat)
       colnames(mat)[1]="name"
       mat[,2:ncol(mat)]<-as.data.frame(lapply(mat[,2:ncol(mat)], format_num8))
       return(mat)
     }, readOnly = TRUE)

    get_weights_matrix<-eventReactive(input$go9,{
      if(length(which(input$uti2=='combo'))==0){
                weights_matrix<<-call_scenario_cost_BN_matrix(input$uti2,input$assets_num1,input$lambdas,input$asset_ret1,input$asset_vol1,
                              input$asset_corr1,input$sample_number2,input$extreme_stress_loss,pro_dict,
                              input$principal1,hot.to.df(input$hotable1),hot.to.df(input$hotable5),input$w_now,input$lower_bound,input$upper_bounds,input$ks,
                              input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval) 
                weights_matrix<<-data.frame(weights_matrix)
                  return(weights_matrix)
      }else if(length(which(input$uti2!='combo'))==0){
        weights_matrix5<<-call_scenario_cost_BN_matrix2(input$assets_num1,input$x_extreme2,input$x_downturning2,input$x_mid22,input$x_upturning2,input$prob22,input$asset_ret1,input$asset_vol1,
                                                        input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                                                        input$principal1,hot.to.df(input$hotable1),hot.to.df(input$hotable5),input$w_now,input$lower_bound,input$upper_bounds,input$ks,
                                                        input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval)
        weights_matrix5<<-data.frame(weights_matrix5)
        return(weights_matrix5)
      }else{
        uti2<-input$uti2[-which(input$uti2=='combo')]
        weights_matrix<<-call_scenario_cost_BN_matrix(uti2,input$assets_num1,input$lambdas,input$asset_ret1,input$asset_vol1,
                                                      input$asset_corr1,input$sample_number2,input$extreme_stress_loss,pro_dict,
                                                      input$principal1,hot.to.df(input$hotable1),hot.to.df(input$hotable5),input$w_now,input$lower_bound,input$upper_bounds,input$ks,
                                                      input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval) 
        weights_matrix5<<-call_scenario_cost_BN_matrix2(input$assets_num1,input$x_extreme2,input$x_downturning2,input$x_mid22,input$x_upturning2,input$prob22,input$asset_ret1,input$asset_vol1,
                                                  input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                                                  input$principal1,hot.to.df(input$hotable1),hot.to.df(input$hotable5),input$w_now,input$lower_bound,input$upper_bounds,input$ks,
                                                  input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval)
      
        }

      weights_matrix<<-data.frame(weights_matrix)
      weights_matrix5<<-data.frame(weights_matrix5)
      return(weights_matrix)
    })
    format_num4 <- function(col) {
      if (is.numeric(col))
        sprintf('%.2f%%', 100*col)
      else
        col
    }
    output$hotable3 <- renderTable({
      weights_matrix2<-get_weights_matrix()
      weights_matrix2[,4:ncol(weights_matrix2)]<-as.data.frame(lapply(weights_matrix2[,4:ncol(weights_matrix2)], format_num4))
    return(weights_matrix2)
      }, readOnly = TRUE)
    output$downloadData <- downloadHandler(
      filename = function() {
        paste('output', input$filetype, sep = ".")
      },
      content <- function(file) {
        sep <- switch(input$filetype, "csv" = ",", "tsv" = "\t")
        write.table(weights_matrix, file, sep = sep,
                    row.names = FALSE)
      }
    )
    output$hotable4 <- renderTable({
      weights_matrix2<-get_weights_matrix()
      if(length(which(input$uti2=='combo'))!=0){
        weights_matrix2<-weights_matrix5
      }
      weights_matrix2[,7:ncol(weights_matrix2)]<-as.data.frame(lapply(weights_matrix2[,7:ncol(weights_matrix2)], format_num4))
      return(weights_matrix2)
    }, readOnly = TRUE)
  }
)