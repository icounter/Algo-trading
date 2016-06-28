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
library(graph)
library(Rgraphviz)
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
       N1<-which(colnames(input_data)=="haircut")
       N4<-which(colnames(input_data)=="transaction_cost")
       dddd<-input_data[,(N4+1):(N1-1)]
       N6<-which(colnames(dddd)=="finance_cost")
       dddd<-dddd[,-N6]
       N3<-(ncol(dddd)+1)-N6
       N2<-(ncol(dddd))-N3
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
           eq[i]=str_replace_all(eq[i],"t","")
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
           eq[i]=str_replace_all(eq[i],"f","")
         }
         colnames(finan_cost1)<-c("Asset_name",eq)
       }
       haircut<<-input_data[,which(colnames(input_data)=="haircut")]
       real_finance_weight<<-input_data[,which(colnames(input_data)=="real_finance_weight")]
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
      rescale<-rep(0,nrow(bayesian_matrix1))
      if(bayesian_matrix1[1,1]!=1){
      rescale[1]<-1-input$tentative_k
      rescale[2:length(rescale)]<-bayesian_matrix1[2:nrow(bayesian_matrix1),1]/(1-bayesian_matrix1[1,1])*input$tentative_k
      }
      bayesian_matrix1<-cbind(bayesian_matrix1,rescale)
      colnames(bayesian_matrix1)[ncol(bayesian_matrix1)]<-"Rescaled prob"
      bayesian_matrix1<-bayesian_matrix1[,c(c(1,ncol(bayesian_matrix1)),seq(2,ncol(bayesian_matrix1)-1))]
      bayesian_matrix1[,1]<-sprintf("%.6f",bayesian_matrix1[,1])
      bayesian_matrix1[,2]<-sprintf("%.6f",bayesian_matrix1[,2])
      bayesian_matrix1[,3:ncol(bayesian_matrix1)]<-as.data.frame(lapply(bayesian_matrix1[,3:ncol(bayesian_matrix1)], format_num))
      return(bayesian_matrix1)
    })
    generate_bayesian_cor<-eventReactive(input$go5,{
      pro_dict3<-as.matrix(t(bayesian_matrix1))
      pro_dict3<-as.matrix(pro_dict)
      pro_dict_prob<-pro_dict3[1,]
      pro_dict_matrix<-pro_dict3[2:nrow(pro_dict3),]
      name<-rownames(pro_dict_matrix)
      N<-length(name)
      M<-50000
      name<-rownames(pro_dict_matrix)
      N<-length(name)
      asset_ret<-as.double(strsplit(input$asset_ret1,",")[[1]])
      asset_corr<-as.double(strsplit(input$asset_corr1,",")[[1]])
      asset_vol1<-as.double(strsplit(input$asset_vol1,",")[[1]])
      asset_var<-(asset_vol1)^2
      asset_cor<-matrix(1,N,N)
      asset_cor[upper.tri(asset_cor)]=asset_corr
      asset_cor[lower.tri(asset_cor)]=t(asset_cor)[lower.tri(asset_cor)]
      asset_cov<-r2cov(sd =sqrt(asset_var),R = asset_cor)
      margins_r<-rep("norm",N)
      paramMargins_r <- list()
      extreme_stress_loss<-as.double(strsplit(input$extreme_stress_loss,",")[[1]])
      for(i in 1:N){
        paramMargins_r[[length(paramMargins_r)+1]] <- list(mean =asset_ret[i], sd =sqrt(asset_var[i]))
      }
      rand2<-generate_N_rand(N,asset_corr,margins_r,paramMargins_r,M)
      order<-as.integer(M*pro_dict_prob/sum(pro_dict_prob))
      for(i in 1:length(order)){
        if(i==1){
          rand2[1:order[i],]<-t(((1-pro_dict_matrix[,i])*t(rand2[1:order[i],])))
          rand2[1:order[i],]<-t(apply(rand2[1:order[i],],1,function(x) x+as.vector(pro_dict_matrix[,i]*extreme_stress_loss)))
        }else{
          rand2[(sum(order[1:(i-1)])+1):(sum(order[1:i])),]<-t(((1-pro_dict_matrix[,i])*t(rand2[(sum(order[1:(i-1)])+1):(sum(order[1:i])),])))
          rand2[(sum(order[1:(i-1)])+1):(sum(order[1:i])),]<-t(apply(rand2[(sum(order[1:(i-1)])+1):(sum(order[1:i])),],1,function(x) x+as.vector(pro_dict_matrix[,i]*extreme_stress_loss)))
        }
      }
      pro_dict_matrix<-rand2
      cor<-cor(pro_dict_matrix)
      for(i in 1:ncol(cor)){
        for(j in 1:nrow(cor)){
          if(is.na(cor[i,j]))  cor[i,j]=0
        }
          
      }
      cor<-data.frame(cor)
      colnames(cor)<-name
      cor<-as.data.frame(lapply(cor, format_num9))
      cor<-cbind(name,cor)
      colnames(cor)[1]<-c("correlation matrix")
      return(cor)
    })
    
    generate_extreme_cor<-eventReactive(input$go5,{
      pro_dict3<-as.matrix(t(bayesian_matrix1))
      pro_dict3<-as.matrix(pro_dict)
      pro_dict3<-pro_dict3[,-1]
      pro_dict_prob<-pro_dict3[1,]
      pro_dict_matrix<-pro_dict3[2:nrow(pro_dict3),]
      M<-50000
      name<-rownames(pro_dict_matrix)
      N<-length(name)
      asset_ret<-as.double(strsplit(input$asset_ret1,",")[[1]])
      asset_corr<-as.double(strsplit(input$asset_corr1,",")[[1]])
      asset_vol1<-as.double(strsplit(input$asset_vol1,",")[[1]])
      asset_var<-(asset_vol1)^2
      asset_cor<-matrix(1,N,N)
      asset_cor[upper.tri(asset_cor)]=asset_corr
      asset_cor[lower.tri(asset_cor)]<-t(asset_cor)[lower.tri(asset_cor)]
      asset_cov<-r2cov(sd =sqrt(asset_var),R = asset_cor)
      margins_r<-rep("norm",N)
      paramMargins_r <- list()
      extreme_stress_loss<-as.double(strsplit(input$extreme_stress_loss,",")[[1]])
      for(i in 1:N){
        paramMargins_r[[length(paramMargins_r)+1]] <- list(mean =asset_ret[i], sd =sqrt(asset_var[i]))
      }
      rand<<-generate_N_rand(N,asset_corr,margins_r,paramMargins_r,M)
      order<-as.integer(M*pro_dict_prob/sum(pro_dict_prob))
      for(i in 1:length(order)){
        if(i==1){
          rand[1:order[i],]<-t(((1-pro_dict_matrix[,i])*t(rand[1:order[i],])))
          rand[1:order[i],]<-t(apply(rand[1:order[i],],1,function(x) x+as.vector(pro_dict_matrix[,i]*extreme_stress_loss)))
        }else{
          rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),]<-t(((1-pro_dict_matrix[,i])*t(rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),])))
          rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),]<-t(apply(rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),],1,function(x) x+as.vector(pro_dict_matrix[,i]*extreme_stress_loss)))
        }
      }
      pro_dict_matrix<-rand
      cor<-diag(N)
      cor<-cor(pro_dict_matrix)
      for(i in 1:ncol(cor)){
        for(j in 1:nrow(cor)){
          if(is.na(cor[i,j]))  cor[i,j]=0
        }
        
      }
      cor<-data.frame(cor)
      colnames(cor)<-name
      cor<-as.data.frame(lapply(cor, format_num9))
      cor<-cbind(name,cor)
      colnames(cor)[1]<-c("correlation matrix")
      return(cor)
    })
    
    generate_bayesian_cor2<-eventReactive(input$go5,{
      pro_dict3<-as.matrix(t(bayesian_matrix1))
      rescale<-rep(0,nrow(pro_dict3))
      if(pro_dict3[1,1]!=1){
        rescale[1]<-1-input$tentative_k
        rescale[2:length(rescale)]<-pro_dict3[2:nrow(pro_dict3),1]/(1-pro_dict3[1,1])*input$tentative_k
      }
      pro_dict3[,1]<-rescale
      pro_dict3<-t(pro_dict3)
      pro_dict_prob<-pro_dict3[1,]
      pro_dict_matrix<-pro_dict3[2:nrow(pro_dict3),]
      name<-rownames(pro_dict_matrix)
      N<-length(name)
      M<-50000
      asset_ret<-as.double(strsplit(input$asset_ret1,",")[[1]])
      asset_corr<-as.double(strsplit(input$asset_corr1,",")[[1]])
      asset_vol1<-as.double(strsplit(input$asset_vol1,",")[[1]])
      asset_var<-(asset_vol1)^2
      asset_cor<-matrix(1,N,N)
      asset_cor[upper.tri(asset_cor)]=asset_corr
      asset_cor[lower.tri(asset_cor)]<-t(asset_cor)[lower.tri(asset_cor)]
      asset_cov<-r2cov(sd =sqrt(asset_var),R = asset_cor)
      margins_r<-rep("norm",N)
      paramMargins_r <- list()
      extreme_stress_loss<-as.double(strsplit(input$extreme_stress_loss,",")[[1]])
      for(i in 1:N){
        paramMargins_r[[length(paramMargins_r)+1]] <- list(mean =asset_ret[i], sd =sqrt(asset_var[i]))
      }
      rand<-generate_N_rand(N,asset_corr,margins_r,paramMargins_r,M)
      order<-as.integer(M*pro_dict_prob/sum(pro_dict_prob))
      for(i in 1:length(order)){
        if(i==1&&order[i]!=0){
          rand[1:order[i],]<-t(((1-pro_dict_matrix[,i])*t(rand[1:order[i],])))
          rand[1:order[i],]<-t(apply(rand[1:order[i],],1,function(x) x+as.vector(pro_dict_matrix[,i]*extreme_stress_loss)))
        }else if(order[i]!=0){
          rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),]<-t(((1-pro_dict_matrix[,i])*t(rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),])))
          rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),]<-t(apply(rand[(sum(order[1:(i-1)])+1):(sum(order[1:i])),],1,function(x) x+as.vector(pro_dict_matrix[,i]*extreme_stress_loss)))
        }
      }
      pro_dict_matrix<-rand
      cor<-cor(pro_dict_matrix)
      for(i in 1:ncol(cor)){
        for(j in 1:nrow(cor)){
          if(is.na(cor[i,j]))  cor[i,j]=0
        }
        
      }
      cor<-data.frame(cor)
      colnames(cor)<-name
      cor<-as.data.frame(lapply(cor, format_num9))
      cor<-cbind(name,cor)
      colnames(cor)[1]<-c("correlation matrix")
      return(cor)
    })
    generate_normal_cor<-eventReactive(input$go5,{
      N<-as.double(input$assets_num1)
      asset_corr1<-as.double(strsplit(input$asset_corr1,",")[[1]])
      asset_cor<-matrix(1,N,N)
      k=1
      for(i in 1:(N-1)){
        for(j in (i+1):(N)){
          asset_cor[i,j]=asset_corr1[k]
          k=k+1
        }
      }
      for(i in 1:(N-1)){
        for(j in (i+1):N){
          asset_cor[j,i]=asset_cor[i,j]
        }
      }
      name<-strsplit(input$asset_name1,",")[[1]]
      asset_cor<-data.frame(asset_cor)
      colnames(asset_cor)<-name
      asset_cor[,1:ncol(asset_cor)]<-as.data.frame(lapply(asset_cor[,1:ncol(asset_cor)], format_num9))
      asset_cor<-cbind(name,asset_cor)
      colnames(asset_cor)[1]<-c("correlation matrix")
      
      return(asset_cor)
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
    output$bayesian_matrix_cor_normal <- DT::renderDataTable(
      DT::datatable(generate_normal_cor(), options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
        #colnames(bayesian_matrix2)
      )))
    output$bayesian_matrix_cor_extreme <- DT::renderDataTable(
      DT::datatable(generate_extreme_cor(), options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
        #colnames(bayesian_matrix2)
      )))
    output$bayesian_matrix_cor <- DT::renderDataTable(
      DT::datatable(generate_bayesian_cor(), options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
        #colnames(bayesian_matrix2)
      )))
    output$bayesian_matrix_cor2 <- DT::renderDataTable(
      DT::datatable(generate_bayesian_cor2(), options = list(
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
      plot(net)
    })
    output$plot_bayesian<-renderPlot(
      {
        return(plot_bay())
      }
    )
    format_num2 <- function(col) {
      if (is.numeric(col))
        sprintf('%.3f', col)
      else
        col
    }
    finan_cost2<-eventReactive(input$go7,{
      if((input$assets_num1==4)&(trans_generaed==0)){
        condtion_name2='!=0'
        con_number2=1
        finan_cost1<-data.frame((matrix(0,nrow=input$assets_num1,ncol=con_number2+1)))
        col_names<-strsplit(condtion_name2,",")[[1]]
        colnames(finan_cost1)<-c("Asset_name",col_names)
        asset_name<-strsplit(input$asset_name1,",")[[1]]
        string2<-rep(asset_name,1)
        string2[1:input$assets_num1]=paste0(string2[1:input$assets_num1],"_finance_cost")
        finan_cost1[,1]<-string2
        finan_cost1[1,2]<-c(-0.028)
        finan_cost1[2,2]<-c(-0.0215)
        finan_cost1[3,2]<-c(-0.024)
        finan_cost1[4,2]<-c(-0.0065)
        finan_cost<<-(finan_cost1)
        return(finan_cost)
      }else{
      finan_cost<<-(finan_cost1)
      return(finan_cost)}
      }) 
    output$hotable5 <- DT::renderDataTable(
      DT::datatable(finan_cost2(), options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15
        #colnames(bayesian_matrix2)
      )))
    trans_cost2<-eventReactive(input$go4,{
     if((input$assets_num1==4)&(trans_generaed==0)){
       con_number=1
       condtion_name="!=0"
       trans_cost1<-data.frame((matrix(0,nrow=input$assets_num1,ncol=con_number+1)))
       col_names<-strsplit(condtion_name,",")[[1]]
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
     }else{
     trans_cost<<-(trans_cost1)
     return(trans_cost)
     }
    })
    
    # hotable
    output$hotable1 <- DT::renderDataTable(
      DT::datatable(trans_cost2(),options = list(
        lengthMenu = list(c(5, 15, -1), c('5', '15', 'All')),
        pageLength = 15)))

    tentative_trans<-eventReactive(input$go3,{
        w_now<-strsplit(input$w_now,",")[[1]]
      w_now<-as.double(w_now)
      w1<-strsplit(input$w1,",")[[1]]
      w1<-as.double(w1)
      principal1<-(input$principal1)
      trans_cost<-trans_cost
      finan_cost<-finan_cost
      haircut<<-haircut
      real_finance_weight<<-real_finance_weight
      cost2(w_now,w_1=w1,trans_cost,finan_cost,haircut,real_finance_weight,principal1)
    })
    output$tentative_transcost<-renderTable({ 
      weig<-tentative_trans()
      colnames(weig)<-c("trans_cost(bps)","finance_cost(bps)","finance_weihts","haircut","allocated_weights","initial_Weights","weights_changed")
      weig<-data.frame(weig)
      name<-strsplit(input$asset_name1,",")[[1]]
      weig<-cbind(name,weig)
      colnames(weig)[1]="name"
      weig[,2:(ncol(weig)-5)]<-as.data.frame(lapply(weig[,2:(ncol(weig)-5)], format_num8))
      weig[,(ncol(weig)-4):(ncol(weig))]<-as.data.frame(lapply(weig[,(ncol(weig)-4):(ncol(weig))], format_num9))
      return(weig)
    }, readOnly = TRUE)
  
    
    
    
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
      ggplot(data=gene,aes(x=ret,y=probability,group=lambda,colour=lambda))+geom_line()+geom_text(aes(label=probability), size = 4,check_overlap = TRUE)+scale_y_continuous(breaks = round(seq(min(gene$probability), max(gene$probability), by = 0.03),5))+scale_x_continuous(breaks = round(seq(min(gene$ret), max(gene$ret), by = 0.01),5)) +xlab("Three-Month Return +/-")+ylab("probability of Win")
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
      ggplot(data=gene,aes(x=neg_ret,y=pos_ret,group=lambda,colour=lambda))+geom_line()+geom_text(aes(label=pos_ret), size = 4,check_overlap = TRUE)+xlab("Return in Bad Scenario")+ylab("Return in good Scenario")+scale_x_reverse(breaks = round(seq(from=min(as.double(gene$neg_ret)), to=max(as.double(gene$neg_ret)), by = 0.01),100))+scale_y_continuous(breaks = round(seq(from=min(as.double(gene$pos_ret)), to=max(as.double(gene$pos_ret)), by = 0.03),100))
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
        ggplot(data.frame(x=input$x_range),aes(x))+stat_function(fun=function(x)(1/(1-input$lambda1)*((1+x)^(1-input$lambda1)-1)),geom='line',aes(colour="power utility"))+xlab("return")+ylab("utility value")
      }
    })
    weights2<-eventReactive(input$go2,{
      if(input$uti!="combo"){
        call_scenario_cost_BN(input$uti,input$assets_num1,input$lambda1,input$asset_ret1,input$asset_vol1,
                            input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                            input$principal1,trans_cost,finan_cost,haircut,real_finance_weight,input$w_now,input$lower_bound,input$upper_bounds,input$subjective_k,
                            input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval)}else{
        call_scenario_cost_BN2(input$assets_num1,input$x_extreme,input$x_downturning,input$x_mid2,input$x_upturning,input$prob2,input$asset_ret1,input$asset_vol1,
                                                    input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                                                    input$principal1,trans_cost,finan_cost,haircut,real_finance_weight,input$w_now,input$lower_bound,input$upper_bounds,input$subjective_k,
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
        sprintf('%.2f', 10000*col)
      else
        col
    }
    format_num9 <- function(col) {
      if (is.numeric(col))
        sprintf('%.2f%%', 100*col)
      else
        col
    }
    output$tablle<-renderTable({
      weights3<<-weights2()
       ww_1<-weights3$weights[-length(weights3$weights)]
       mat=cost2(w_now,ww_1,trans_cost,finan_cost,haircut,real_finance_weight,principal1)
       colnames(mat)<-c("trans_cost(bps)","finance_cost(bps)","finance_weihts","haircut","allocated_weights","initial_Weights","weights_changed")
       mat<-data.frame(mat)
       name<-as.character(weights3$asset_name)
       mat<-cbind(name[-length(name)],mat)
       colnames(mat)[1]="name"
       mat[,2:(ncol(mat)-5)]<-as.data.frame(lapply(mat[,2:(ncol(mat)-5)], format_num8))
       mat[,(ncol(mat)-4):(ncol(mat))]<-as.data.frame(lapply(mat[,(ncol(mat)-4):(ncol(mat))], format_num9))
       return(mat)
     }, readOnly = TRUE)

    get_weights_matrix<-eventReactive(input$go9,{
      if(length(which(input$uti2=='combo'))==0){
                weights_matrix<<-call_scenario_cost_BN_matrix(input$uti2,input$assets_num1,input$lambdas,input$asset_ret1,input$asset_vol1,
                              input$asset_corr1,input$sample_number2,input$extreme_stress_loss,pro_dict,
                              input$principal1,trans_cost,finan_cost,haircut,real_finance_weight,input$w_now,input$lower_bound,input$upper_bounds,input$ks,
                              input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval) 
                weights_matrix<<-data.frame(weights_matrix)
                  return(weights_matrix)
      }else if(length(which(input$uti2!='combo'))==0){
        weights_matrix5<<-call_scenario_cost_BN_matrix2(input$assets_num1,input$x_extreme2,input$x_downturning2,input$x_mid22,input$x_upturning2,input$prob22,input$asset_ret1,input$asset_vol1,
                                                        input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                                                        input$principal1,trans_cost,finan_cost,haircut,real_finance_weight,input$w_now,input$lower_bound,input$upper_bounds,input$ks,
                                                        input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval)
        weights_matrix5<<-data.frame(weights_matrix5)
        return(weights_matrix5)
      }else{
        uti2<-input$uti2[-which(input$uti2=='combo')]
        weights_matrix<<-call_scenario_cost_BN_matrix(uti2,input$assets_num1,input$lambdas,input$asset_ret1,input$asset_vol1,
                                                      input$asset_corr1,input$sample_number2,input$extreme_stress_loss,pro_dict,
                                                      input$principal1,trans_cost,finan_cost,haircut,real_finance_weight,input$w_now,input$lower_bound,input$upper_bounds,input$ks,
                                                      input$convexity_bounds,input$linear_bounds,input$asset_name1,input$maxeval) 
        weights_matrix5<<-call_scenario_cost_BN_matrix2(input$assets_num1,input$x_extreme2,input$x_downturning2,input$x_mid22,input$x_upturning2,input$prob22,input$asset_ret1,input$asset_vol1,
                                                  input$asset_corr1,input$sample_number1,input$extreme_stress_loss,pro_dict,
                                                  input$principal1,trans_cost,finan_cost,haircut,real_finance_weight,real_finance_weight,input$w_now,input$lower_bound,input$upper_bounds,input$ks,
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
    Outvar<-reactive({
      vars<-strsplit(input$asset_name1,",")[[1]]
      vars<-c(vars,"optimized_total")
      vars<-as.list(vars)
      return(vars)
    })
    output$variables=renderUI({
      selectInput('see_return','Asset Name',Outvar())
    })
    output$variables2=renderUI({
      checkboxGroupInput('see_return2','Multiple assets comparison',Outvar(),selected = 'optimized_total')
    })
 output$returnplot<-renderPlot({
   weights3<-weights2()
   w_1<-weights3$weights[-length(weights3$weights)] 
   name<-as.character(weights3$asset_name[-length(weights3$weights)])
   tcost<-cost(w_now,w_1,trans_cost,finan_cost,haircut,real_finance_weight,principal1)
   rand3<-rand2%*%w_1+tcost
   rand3<-cbind(rand3,rand2)
   rand3<-data.frame(rand3)
   colnames(rand3)<-c("optimized_total",name)
   rand5<-c()
   M=which(input$see_return==colnames(rand3))
#    if(input$see_parts=='normal'){
#      p2<-ggplot(rand3, aes(x=rand3[,M])) +
#        stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand3)[M])+ylab("Cumulative Density Function")
#      #+scale_x_continuous(breaks = round(seq(min(rand3[,M]), max(rand3[,M]), by = 0.1),5))
#      return(p2)
#    }else{
     u<-rep(1,nrow(pro_dict2)-1)
     uu<-diag(length(w_1))
     pro_dict3<-t(t(pro_dict2)[2:ncol(pro_dict2),2:nrow(pro_dict2)]%*%(loss1*w_1))+tcost+loss1[length(loss1)]*w_1[length(w_1)]
     pro_dict_loss_matrix<-data.frame(t(pro_dict2)[2:ncol(pro_dict2),2:nrow(pro_dict2)]%*%(loss1*uu))
     rand4=rand3[,M]
     if(M==1){
       if(input$subjective_k!=1){
      Number<-as.integer((input$subjective_k)/((1-input$subjective_k)*(1-pro_dict2$p0[1]))*nrow(rand3)*pro_dict2[1,1+as.double(str_replace_all(colnames(unique(pro_dict3)),"p",""))])
      ecdf_gene<-rep(unique(pro_dict3),Number)
      rand4<-data.frame(c(rand4,ecdf_gene))
      colnames(rand4)<-colnames(rand3)[M]
      p2<-ggplot(rand4, aes(x=rand4)) +
        stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand4))+ylab("Return Cumulative Density Function")+scale_y_continuous(breaks = round(seq(0,1, by = 0.05),5))+scale_x_continuous(labels=scales::percent,breaks =  seq(from=round(100*input$x_range_return[1])/100,to=input$x_range_return[2], by = 0.05))+geom_vline(xintercept = 0,colour=1)+coord_cartesian(xlim=input$x_range_return)
      for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.01)){
        p2=p2+geom_vline(xintercept = i,colour=1,linetype="dotted")
      }
      for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.05)){
        p2=p2+geom_vline(xintercept = i,colour=1)
      }
      return(p2)}else{
        Number<-as.integer(nrow(rand3)*pro_dict2[1,1+as.double(str_replace_all(colnames(unique(pro_dict3)),"p",""))])
        ecdf_gene<-rep(unique(pro_dict3),Number)
        rand4<-data.frame(ecdf_gene)
        colnames(rand4)<-colnames(rand3)[M]
        p2<-ggplot(rand4, aes(x=rand4)) +
          stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand4))+ylab("Return Cumulative Density Function")+scale_y_continuous(breaks = round(seq(0,1, by = 0.05),5))+scale_x_continuous(labels=scales::percent,breaks =  seq(from=round(100*input$x_range_return[1])/100,to=input$x_range_return[2], by = 0.05))+geom_vline(xintercept = 0,colour=1)+coord_cartesian(xlim=input$x_range_return)
        for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.01)){
          p2=p2+geom_vline(xintercept = i,colour=1,linetype="dotted")
        }
        for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.05)){
          p2=p2+geom_vline(xintercept = i,colour=1)
        }
        return(p2)
      }}else if(M!=(length(name)+1)){
       if(input$subjective_k!=1){
       set<-unique(pro_dict_loss_matrix[,M-1])
       number<-set[which(set!=0)]
       Number<-as.integer((input$subjective_k)/((1-input$subjective_k)*(1-pro_dict2$p0[1]))*nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M-1])!=0),]),"p",""))]))
       Number1<-as.integer((input$subjective_k)/((1-input$subjective_k)*(1-pro_dict2$p0[1]))*nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M-1])==0),]),"p",""))]))
       Number<-as.integer(nrow(rand3)/(nrow(rand3)+Number1)*Number)
       ecdf_gene<-rep(number,Number)
      rand4<-data.frame(c(rand4,ecdf_gene))
      colnames(rand4)<-colnames(rand3)[M]
       p2<-ggplot(rand4, aes(x=rand4)) +stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand4))+ylab("Return Cumulative Density Function")+scale_y_continuous(breaks = round(seq(0,1, by = 0.05),5))+scale_x_continuous(labels=scales::percent,breaks =  seq(from=round(100*input$x_range_return[1])/100,to=input$x_range_return[2], by = 0.05))+geom_vline(xintercept = 0,colour=1)+coord_cartesian(xlim=input$x_range_return)
       for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.01)){
         p2=p2+geom_vline(xintercept = i,colour=1,linetype="dotted")
       }
       for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.05)){
         p2=p2+geom_vline(xintercept = i,colour=1)
       }
       return(p2)}else{
         set<-unique(pro_dict_loss_matrix[,M-1])
         number<-set[which(set!=0)]
         Number<-as.integer(nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M-1])!=0),]),"p",""))]))
         Number2<-as.integer(nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M-1])==0),]),"p",""))]))
         Number<-as.integer(nrow(rand3)/Number2*Number)
         ecdf_gene<-rep(number,Number)
         rand4<-data.frame(c(rand4,ecdf_gene))
         colnames(rand4)<-colnames(rand3)[M]
         p2<-ggplot(rand4, aes(x=rand4)) +stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand4))+ylab("Return Cumulative Density Function")+scale_y_continuous(breaks = round(seq(0,1, by = 0.05),5))+scale_x_continuous(labels=scales::percent,breaks =  seq(from=round(100*input$x_range_return[1])/100,to=input$x_range_return[2], by = 0.05))+geom_vline(xintercept = 0,colour=1)+coord_cartesian(xlim=input$x_range_return)
         for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.01)){
           p2=p2+geom_vline(xintercept = i,colour=1,linetype="dotted")
         }
         for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.05)){
           p2=p2+geom_vline(xintercept = i,colour=1)
         }
         return(p2)}
     }else{
       rand4<-rand3[,M]
        p2<-ggplot(rand3, aes(x=rand4)) +
         stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand3)[M])+ylab("Return Cumulative Density Function")+scale_y_continuous(breaks = round(seq(0,1, by = 0.05),5))+geom_vline(xintercept = 0,colour=1)+scale_x_continuous(labels=scales::percent,breaks = seq(from=round(100*input$x_range_return[1])/100,to=input$x_range_return[2], by = 0.05))+coord_cartesian(xlim=input$x_range_return)
        for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.01)){
          p2=p2+geom_vline(xintercept = i,colour=1,linetype="dotted")
        }
        for(i in seq(from=input$x_range_return[1],to=input$x_range_return[2], by = 0.05)){
          p2=p2+geom_vline(xintercept = i,colour=1)
        }
       return(p2)
     }
   })
 output$returnplot2<-renderPlot({
   weights3<-weights2()
   w_1<-weights3$weights[-length(weights3$weights)] 
   name<-as.character(weights3$asset_name[-length(weights3$weights)])
   tcost<-cost(w_now,w_1,trans_cost,finan_cost,haircut,real_finance_weight,principal1)
   rand3<-rand2%*%w_1+tcost
   rand3<-cbind(rand3,rand2)
   rand3<-data.frame(rand3)
   colnames(rand3)<-c("optimized_total",name)
   M<-0
   for(i in 1:length(input$see_return2)){
     M=c(M,which(colnames(rand3)==input$see_return2[i]))
   }
   M<-M[-1]
   u<-rep(1,nrow(pro_dict2)-1)
   uu<-diag(length(w_1))
   pro_dict3<-t(t(pro_dict2)[2:ncol(pro_dict2),2:nrow(pro_dict2)]%*%(loss1*w_1))+tcost+loss1[length(loss1)]*w_1[length(w_1)]
   pro_dict_loss_matrix<-data.frame(t(pro_dict2)[2:ncol(pro_dict2),2:nrow(pro_dict2)]%*%(loss1*uu))
   rand5<-c()
   #p2<-ggplot(rand4, aes(x=rand4))+stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand4))+ylab("Cumulative Density Function")
   for(jk in 1:length(M)){
     rand4=rand3[,M[jk]]
     if(input$subjective_k!=1){
       if(M[jk]==1){
         Number<-as.integer((input$subjective_k)/((1-input$subjective_k)*(1-pro_dict2$p0[1]))*nrow(rand3)*pro_dict2[1,1+as.double(str_replace_all(colnames(unique(pro_dict3)),"p",""))])
         ecdf_gene<-rep(unique(pro_dict3),Number)
         rand4<-data.frame(c(rand4,ecdf_gene))
         colnames(rand4)<-colnames(rand3)[M[jk]]
       }else if(M[jk]!=(length(name)+1)){
         set<-unique(pro_dict_loss_matrix[,M[jk]-1])
         number<-set[which(set!=0)]
         Number<-as.integer((input$subjective_k)/((1-input$subjective_k)*(1-pro_dict2$p0[1]))*nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M[jk]-1])!=0),]),"p",""))]))
         Number1<-as.integer((input$subjective_k)/((1-input$subjective_k)*(1-pro_dict2$p0[1]))*nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M[jk]-1])==0),]),"p",""))]))
         Number<-as.integer(nrow(rand3)/(nrow(rand3)+Number1)*Number)
         ecdf_gene<-rep(number,Number)
         rand4<-data.frame(c(rand4,ecdf_gene))
         colnames(rand4)<-colnames(rand3)[M[jk]]}else{          
           rand4<-data.frame(rand4)
           colnames(rand4)<-c('cash')}
       if(jk==1){
         rand5<-as.vector(as.matrix(rand4))
         name_level_list<-c(colnames(rand4))
         length_level_list<-c(length(rand5))
       }else{
         rand5<-c(as.vector(rand5),as.vector(as.matrix(rand4)))
         name_level_list<-c(name_level_list,colnames(rand4))
         length_level_list<-c(length_level_list,length(as.vector(as.matrix(rand4))))
       }}else{
       if(M[jk]==1){
         Number<-as.integer(nrow(rand3)*pro_dict2[1,1+as.double(str_replace_all(colnames(unique(pro_dict3)),"p",""))])
         ecdf_gene<-rep(unique(pro_dict3),Number)
         rand4<-data.frame(ecdf_gene)
         colnames(rand4)<-colnames(rand3)[M[jk]]
       }else if(M[jk]!=(length(name)+1)){
         set<-unique(pro_dict_loss_matrix[,M[jk]-1])
         number<-set[which(set!=0)]
         Number<-as.integer(nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M[jk]-1])!=0),]),"p",""))]))
         Number2<-as.integer(nrow(rand3)*sum(pro_dict2[1,1+as.double(str_replace_all(rownames(pro_dict_loss_matrix[which((pro_dict_loss_matrix[,M[jk]-1])==0),]),"p",""))]))
         Number<-as.integer(nrow(rand3)/Number2*Number)
         ecdf_gene<-rep(number,Number)
         rand4<-data.frame(c(rand4,ecdf_gene))
         }else{
           rand4<-data.frame(rand4)
           colnames(rand4)<-c('cash')}
       if(jk==1){
         rand5<-as.vector(as.matrix(rand4))
         name_level_list<-c(colnames(rand4))
         length_level_list<-c(length(rand5))
       }else{
         rand5<-c(as.vector(rand5),as.vector(as.matrix(rand4)))
         name_level_list<-c(name_level_list,colnames(rand4))
         length_level_list<-c(length_level_list,length(as.vector(as.matrix(rand4))))
       }
       }}
   gl=as.factor(rep(name_level_list,length_level_list))
   df<-data.frame(x=rand5,assets=gl)
   if(length(M)==1&M==(length(name)+1)){
     p2<-ggplot(df, aes(x,colour=assets)) +stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand4))+ylab("Return Cumulative Density Function")+scale_y_continuous(breaks = round(seq(0,1, by = 0.05),5))+geom_vline(xintercept = 0,colour=1)+scale_x_continuous(labels=scales::percent,breaks = seq(from=round(100*input$x_range_return2[1])/100,to=input$x_range_return2[2], by = 0.05))+coord_cartesian(xlim=input$x_range_return2)
     for(i in seq(from=input$x_range_return2[1],to=input$x_range_return2[2], by = 0.01)){
       p2=p2+geom_vline(xintercept = i,colour=1,linetype="dotted")
     }
     for(i in seq(from=input$x_range_return2[1],to=input$x_range_return2[2], by = 0.05)){
       p2=p2+geom_vline(xintercept = i,colour=1)
     }
   }else{
   p2<-ggplot(df, aes(x,colour=assets)) +stat_ecdf(geom="step")+theme_bw()+xlab(colnames(rand4))+ylab("Return Cumulative Density Function")+scale_y_continuous(breaks = round(seq(0,1, by = 0.05),5))+scale_x_continuous(labels=scales::percent,breaks = seq(from=round(100*input$x_range_return2[1])/100,to=input$x_range_return2[2], by = 0.05))+geom_vline(xintercept = 0,colour=1)+coord_cartesian(xlim=input$x_range_return2)
   for(i in seq(from=input$x_range_return2[1],to=input$x_range_return2[2], by = 0.01)){
     p2=p2+geom_vline(xintercept = i,colour=1,linetype="dotted")
   }
   for(i in seq(from=input$x_range_return2[1],to=input$x_range_return2[2], by = 0.05)){
     p2=p2+geom_vline(xintercept = i,colour=1)
   }
   }
   return(p2)
   })
   
 
 
 
 
  }
)
