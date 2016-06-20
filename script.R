##############################################################compare mean-variance wrt utility
####################first scenario no transcation cost no bayesian net######################################################
############################################################################################################################
mean_variance<-function(w,mu,A,cov){
  rett<-w%*%mu-1/2*A*w%*%cov%*%w
  return(-rett)
}
h_eq<-function(w){
  return(sum(w)-1)
}
####using this to generate N random variables
generate_N_rand<-function(N_p,corr,margins_r,paramMargins_r,N){
  #set.seed(100)
  myCop <- normalCopula(param=corr, dim = N_p, dispstr = "un")
  myMvd <- mvdc(copula=myCop, margins=margins_r,
                paramMargins=paramMargins_r )
  Z2 <- rmvdc(myMvd, N)
  #colnames(Z2) <-parameter_name
  return(Z2)
}
expo_u<-function(x,lambda){
  u<- -exp(-lambda*x)/lambda
  return(u)
}
get_expo_u<-function(ret,lambda){
  return(expo_u(ret,lambda))
}
expo_find<-function(w1,rand2,lambda,sample_number){
  
  exp_expo_ut<-mean(get_expo_u(rand2 %*% w1,lambda))
  return(log(-exp_expo_ut))
}
scenario_no_cost_no_BN<-function(assets_num,lambda,asset_ret,asset_var,asset_corr,sample_number){
  ####input variable:
  ###
  ###
  ##
  #########
  N=assets_num
  A=lambda
  mu=asset_ret
  asset_cor<-matrix(1,N,N)
  asset_cor[upper.tri(asset_cor)]=asset_corr
  asset_cor[lower.tri(asset_cor)]=t(asset_cor)[lower.tri(asset_cor)]
  asset_cov<-r2cov(sd =sqrt(asset_var),R = asset_cor)
  init_val=rep(1/N,N)
  lower1<-rep(0,N)
  upper1<-rep(1,N)
  heqjac.hs100<<-function(x) nl.jacobian(x, h_eq, heps = 1e-2) 
  res1=slsqp(init_val,mean_variance, lower = lower1, upper =upper1,
             hin =NULL , heq = h_eq ,heqjac=heqjac.hs100,
             nl.info = FALSE, control = list(xtol_rel = 1e-10, check_derivatives = TRUE),mu=mu,A=A,cov=asset_cov)
  #parameter_name=c("CLO","CMBS")
  #####define what's the copula and marginal distr
  margins_r<-rep("norm",N)
  paramMargins_r <- list()
  # Now the new experiments
  for(i in 1:N){
    paramMargins_r[[length(paramMargins_r)+1]] <- list(mean =asset_ret[i], sd =sqrt(asset_var[i]))
  }
  N_sample<-sample_number
  Z<-generate_N_rand(N,asset_corr,margins_r,paramMargins_r,N_sample)
#   res2=slsqp(init_val,expo_find, lower = lower1, upper =upper1,
#              hin =NULL , heq = h_eq, 
#              nl.info = FALSE, control = list(xtol_rel = 1e-20,ftol_abs=0,maxeval = 10000, check_derivatives = TRUE),rand2=Z,lambda=A,sample_number=sample_number)
  #expo_find.hs100<<-function(x) nl.jacobian(x, expo_find, heps = 1e-2) 
   res2=slsqp(x0=init_val,fn=expo_find,heq=h_eq,heqjac=heqjac.hs100,lower=lower1,upper=upper1,rand2=Z,lambda=A,sample_number=sample_number)
#  
#  system.time(replicate(10,auglag(init_val,mean_variance, lower = lower1, upper =upper1,
#             hin =NULL , heq = h_eq, 
#             nl.info = FALSE, control = list(xtol_rel = 1e-10, check_derivatives = TRUE),mu=mu,A=A,cov=asset_cov)))
#  
#  system.time(replicate(1,auglag(x0=init_val,fn=expo_find,heq=h_eq,heqjac=heqjac.hs100,lower=lower1,localsolver = c("LBFGS"),upper=upper1,rand2=Z,lambda=A,sample_number=sample_number)
#  ))
#   system.time(replicate(1,auglag(init_val,expo_find, lower = lower1, upper =upper1,
#                                              hin =NULL , heq = h_eq,localsolver = c("LBFGS"), 
#                                              nl.info = FALSE, control = list(xtol_rel = 1e-20,ftol_abs=0,maxeval = 10000, check_derivatives = TRUE),rand2=Z,lambda=A,sample_number=sample_number)
#   ))
#   system.time(replicate(1,LBFGS(init_val,expo_find, lower = lower1, upper =upper1,
#                                  hin =NULL , heq = h_eq,
#                                  nl.info = FALSE, control = list(xtol_rel = 1e-20,ftol_abs=0,maxeval = 10000, check_derivatives = TRUE),rand2=Z,lambda=A,sample_number=sample_number)
#   ))
  
#  system.time(replicate(1,auglag(x0=init_val,fn=expo_find,heq=h_eq,heqjac=heqjac.hs100,lower=lower1,localsolver = c("MMA"),upper=upper1,rand2=Z,lambda=A,sample_number=sample_number)
#  ))
#  res2<-auglag(init_val,expo_find, lower = lower1, upper =upper1,
#              hin =NULL , heq = h_eq, 
#              nl.info = FALSE, control = list(xtol_rel = 1e-20,ftol_abs=0,maxeval = 10000, check_derivatives = TRUE),rand2=Z,lambda=A,sample_number=sample_number)
#  res3<-auglag(x0=init_val,fn=expo_find,heq=h_eq,heqjac=heqjac.hs100,lower=lower1,localsolver = c("LBFGS"),upper=upper1,rand2=Z,lambda=A,sample_number=sample_number)
# 
#  
#  system.time(replicate(1,auglag(x0=init_val,fn=expo_find,heq=h_eq,heqjac=heqjac.hs100,lower=lower1,localsolver = c("auglag"),upper=upper1,rand2=Z,lambda=A,sample_number=sample_number)
#                          ))
 
 
  weights_mean=res1$par
  weights_exp=res2$par
  asset_name1=rep("asset_",N)
  for(i in 1:N){
    asset_name1[i]=paste0(asset_name1[i],i)
  }
  methods1=c(rep("mean_variance",N),rep("exponential utility",N))
  dimen<-2*length(weights_mean)
  weights1<<-data.frame(
    methods=methods1,
    asset_name=rep(asset_name1,2),
    weight=c(weights_mean,weights_exp))
  return(weights1)
}

call_scenario_no_cost_no_BN<-function(assets_num,lambda,asset_ret,asset_var,asset_corr,sample_number){
  asset_ret=as.double(unlist(strsplit(asset_ret,',')))
  asset_var=as.double(unlist(strsplit(asset_var,',')))
  asset_corr=as.double(unlist(strsplit(asset_corr,',')))
  weights=scenario_no_cost_no_BN(assets_num,lambda,asset_ret,asset_var,asset_corr,sample_number)
  return(weights)
}

build_cond<-function(affect_relation,prob_table){
  affect<-strsplit(affect_relation,";")[[1]]
  table<-strsplit(prob_table,";")[[1]]
  yn<-c("1","0")
  N=length(affect)
  cond_table=list()
  for(i in 1:N){
    bb<-paste0("~",affect[i])
    aa<-paste0("c(",table[i],")")
    cond_table[[i]]<-cptable(eval(parse(text=bb)),values=eval(parse(text=aa)),levels=yn)
  }
  return(cond_table)
}
cost<-function(w_now,w_1,trans_cost,principal1=2293){
  ##cash position at MM
  N<-length(w_now)
  N_con<-ncol(trans_cost)
  if(length(N_con)==0) {
    return(0)
  }else{
    con<-colnames(trans_cost)[2:N_con]
    delta_w<-(w_1-w_now)*as.double(principal1)
    
    ret_sum<-0
    for(j in 1:N){
      for(i in 1:(N_con-1)){
        if(i==1||i==(N_con-1)) {
          aa<-paste0(abs(delta_w[j]),con[i])
          if(eval(parse(text=aa))){
            ret_sum=ret_sum+trans_cost[j,i+1]*abs(w_now[j]-w_1[j])
            
          }}
        else{
          aa<-paste0(abs(delta_w[j]),con[i],abs(delta_w[j]))
          if(eval(parse(text=aa))){
            ret_sum=ret_sum+trans_cost[j,i+1]*abs(w_now[j]-w_1[j])
            
          }}
      }
    }
    leverage=sum(w_1[1:(length(w_1))])-1
    i=0
    trans_cost2=trans_cost
    while(leverage>0&&i<=length(w_1)){
      a=which.max(trans_cost2[(N+1):(2*N-i),2])
      i=i+1
      if((leverage-w_1[a])<=0){
        ret_sum=ret_sum+trans_cost2[N+a,2]*leverage
        leverage=-1
      }else{
        ret_sum=ret_sum+trans_cost2[N+a,2]*w_1[a]
        leverage=leverage-w_1[a]
      }
      w_1=w_1[-a]
      trans_cost2=trans_cost[-(N+a),]
    }
    return(ret_sum)
  }
}
power_uility<-function(x,beta){
  u<-1/(1-beta)*(x^(1-beta)-1)
  return(u)
}
log_uility<-function(x){
  if(x<0){
    return(MAXIMUM_LOSS)
  }else{
  u<-log(x)
  return(u)
  }
}
expo_utility<-function(x,lambda){
  u<- (1-exp(-lambda*x))/lambda
  return(u)
}
get_expo_ut<-function(ret,beta,w_now,w1,tcost){
  return(expo_utility(ret+tcost,beta))
}
get_power_ut<-function(ret,beta,w_now,w1,tcost){
  return(power_uility(1+ret+tcost,beta))
}
get_log_ut<-function(ret,beta,w_now,w1,tcost){
  return(log_uility(1+beta*(ret+tcost)))
}
get_extreme_expo_uti<<-function(pro_dict,loss,beta,w_now,w,tcost,mu){
  rett<-0
  u<-rep(1,nrow(pro_dict)-1)
  rett<-sum(pro_dict[1,2:ncol(pro_dict)]*expo_utility((t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)]%*%(loss*w)+(u-t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)])%*%(mu*w))+tcost,beta))
  return(rett)
}
get_extreme_power_uti<<-function(pro_dict,loss,beta,w_now,w,tcost,mu){
  rett<-0
  u<-rep(1,nrow(pro_dict)-1)
  rett<-sum(pro_dict[1,2:ncol(pro_dict)]*power_uility((1+t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)]%*%(loss*w)+(u-t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)])%*%(mu*w))+tcost,beta))
#   for(i in 2:ncol(pro_dict)){
#     rett<-rett+pro_dict[1,i]*power_uility((1+sum(pro_dict[2:nrow(pro_dict),i]*w*loss+(u-pro_dict[2:nrow(pro_dict),i])*w*mu)+tcost),beta)
#   }
  return(rett)
}
get_extreme_log_uti<<-function(pro_dict,loss,beta,w_now,w,tcost,mu){
  rett<-0
  u<-rep(1,nrow(pro_dict)-1)
  rett<-sum(pro_dict[1,2:ncol(pro_dict)]*log_uility(1+beta*(t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)]%*%(loss*w)+(u-t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)])%*%(mu*w)+tcost)))
#   for(i in 2:ncol(pro_dict)){
#     rett<-rett+pro_dict[1,i]*log_uility((1+beta*(sum(pro_dict[2:nrow(pro_dict),i]*w*loss+(u-pro_dict[2:nrow(pro_dict),i])*w*mu)+tcost)))
#   }
  return(rett)
}

power_find_w<-function(w1,w_now,beta1,trans_cost,principal1,rand2,loss1,pro_dict,k,mu){
  tcost<-cost(w_now,w1,trans_cost,principal1)
  exp_pow_ut<-(1-k)*mean(na.omit(get_power_ut(rand2 %*% w1,beta1,w_now,w1,tcost)))+(k/(1-pro_dict$p0[1]))*get_extreme_power_uti(pro_dict,loss1,beta1,w_now,w1,tcost,mu)
  return(-exp_pow_ut)
}

log_find_w<-function(w1,w_now,beta1,trans_cost,principal1,rand2,loss1,pro_dict,k,mu){
  tcost<-cost(w_now,w1,trans_cost,principal1)
  exp_log_ut<-(1-k)*mean(na.omit(get_log_ut(rand2 %*% w1,beta1,w_now,w1,tcost)))+k/(1-pro_dict$p0[1])*get_extreme_log_uti(pro_dict,loss1,beta1,w_now,w1,tcost,mu)
  return(-exp_log_ut)  
}
expo_find_w<-function(w1,w_now,beta1,trans_cost,principal1,rand2,loss1,pro_dict,k,mu){
  tcost<-cost(w_now,w1,trans_cost,principal1)
  exp_expo_ut<-(1-k)*mean(na.omit(get_expo_ut(rand2 %*% w1,beta1,w_now,w1,tcost)))+k/(1-pro_dict$p0[1])*get_extreme_expo_uti(pro_dict,loss1,beta1,w_now,w1,tcost,mu)
  return(-exp_expo_ut)
}
function_expo<-function(x){
  return(exp(-x*mid1)-prob1*exp(-x*min1)-(1-prob1)*exp(-x*max1))
}
function_power<-function(x){
  return((1+mid1)^(1-x)-prob1*(1+min1)^(1-x)-(1-prob1)*(1+max1)^(1-x))
}
function_log<-function(x){
  return(log(1+x*mid1)-prob1*log(1+x*min1)-(1-prob1)*log(1+x*max1))
}


calculate_l<-function(method,mid,min,max,prob){
  mid1<<-mid
  min1<<-min
  max1<<-max
  prob1<<-prob
  if(method=='log'){
    a<-uniroot.all(function_log,c(0,30))
    return(a[which(a>0)])
  }else if(method=='expo'){
   a<-uniroot.all(function_expo,c(0,30))
   return(a[which(a>0)])
  }else{
    a<-uniroot.all(function_power,c(0,30))
    return(a[which(a>1.00001)])
  }
}

# input$uti,input$assets_num1,input$lambda1,input$asset_ret1,input$asset_var1,
# input$asset_corr1,input$sample_number1,input$extreme_stress_loss,input$affect_relation,input$prob_table,
# input$principal,hot.to.df(input$hotable1),input$w_now,input$lower_bound,input$upper_bounds,input$subjective_k,
# input$convexity_bounds,input$linear_bounds,input$asset_name1
stringsplit<-function(string){
  b<-seq(1,2)
  a<-strsplit(string,NULL)[[1]]
  ll<-(length(a)+1)/2
  for(i in 1:ll){
    b[i]<-as.numeric(a[2*i-1])
  }
  return(b)
}
bayesian_matrix<-function(cond_table=NULL,asset_name){
  if(length(cond_table)==0){
    yn<-c("1","0")
    c<-cptable(~CLO,values=c(4,96),levels=yn)
    cm<-cptable(~CMBS|CLO,values = c(4,6,7,93),levels=yn)
    rm.cm<-cptable(~RMBS|CMBS,values=c(3,97,4,96),levels=yn)
    n=3
    cond_table=list(c,cm,rm.cm)
  }
  plist <- compileCPT(cond_table)
  net <- grain(plist)
  bbb<-querygrain(net,nodes=asset_name, type="joint")
  ddd1<-data.frame(bbb)
  n=length(asset_name)
  pro_dict<-matrix(0,nrow=2^n,ncol=n+1)
  k<-1
  for(i in 1:ncol(ddd1)){
    for(j in 1:2){
      string<-paste0(row.names(ddd1)[j],".",substr(colnames(ddd1)[i],2,nchar(colnames(ddd1)[i])))
      value<-ddd1[j,i]
      pro_dict[k,1]=value
      pro_dict[k,2:(n+1)]=stringsplit(string)
      k=k+1 
    }
  }
  pro_dict<-data.frame(pro_dict)
  joint_table<-aggregate((pro_dict[,1]), as.list(pro_dict[,2:(n+1)]), FUN = sum)
  joint_table=t(joint_table[,c(n+1,seq(1,n))])
  colname=c()
  for(i in seq(0,2^n-1)){
    colname[i+1]<-paste0("p",i)
  }
  colnames(joint_table)<-colname
  rownames(joint_table)<-c("probability",asset_name)
  pro_dict<-data.frame(joint_table)
  pro_dict<-pro_dict[,which(pro_dict[1,]!=0)]
  pro_dict<<-pro_dict
  return(pro_dict)
}
scenario_cost_BN<-function(method,assets_num,lambda,asset_ret,asset_var,asset_corr,sample_number,extreme_stress_loss,pro_dict,principal1,
                           trans_cost,w_now,lower_bounds,upper_bounds,subjective_k,
                           convexity_bounds,linear_bounds,asset_name1=NULL,maxeval1){
  ##
  #
  ##trans_cost matrix like
  N=assets_num
  A=lambda
  mu<<-asset_ret
  asset_cor<-matrix(1,N,N)
  asset_cor[upper.tri(asset_cor)]=asset_corr
  asset_cor[lower.tri(asset_cor)]=t(asset_cor)[lower.tri(asset_cor)]
  asset_cov<-r2cov(sd =sqrt(asset_var),R = asset_cor)
  if(length(asset_name1)==0){
    asset_name1=rep("asset_",N)
    for(i in 1:N){
      asset_name1[i]=paste0(asset_name1[i],i)
    }
  }
  margins_r<-rep("norm",N)
  paramMargins_r <- list()
  # Now the new experiments
  for(i in 1:N){
    paramMargins_r[[length(paramMargins_r)+1]] <- list(mean =asset_ret[i], sd =sqrt(asset_var[i]))
  }
  N_sample<-sample_number
  k<<-subjective_k
  rand2<<-generate_N_rand(N,asset_corr,margins_r,paramMargins_r,N_sample)
  if(assets_num>=6){
    pro_dict1<<-pro_dict[,which(pro_dict[1,]>0.0001)]
  }else{
    pro_dict1<<-pro_dict
  }
  loss1<<-extreme_stress_loss
  trans_cost<<-trans_cost
  w_now<<-w_now
  w1<<-w_now
  eval_g0 <<- convexity_bounds
  eval_h0 <<- linear_bounds
  lower_bound<<-lower_bounds
  upper_bound<<-upper_bounds
  if(is.null(eval_g0)){
    hinjac.hs100<<-NULL
  }else{
    hinjac.hs100 <<- function(x) nl.jacobian(x, eval_g0, heps = 1e-2)
  }
  
  if(is.null(eval_h0)){
    heqjac.hs100<<-NULL
  }else{
    heqjac.hs100<<-function(x) nl.jacobian(x, eval_h0, heps = 1e-2)
  }
  if(assets_num>6){
    tol=1e-4
    maxeval1 = maxeval1
    check_derivatives1=FALSE
  }else{
    tol<-1e-8
    maxeval1 = 1000
    check_derivatives1=TRUE
  }
  if(method=='power'){
    # auglag()
    w1<-slsqp(x0=w1, power_find_w, lower = lower_bound, upper =upper_bound,
              hin =eval_g0 ,hinjac = hinjac.hs100, heq = eval_h0, heqjac = heqjac.hs100,
              nl.info = FALSE, control = list(xtol_rel = tol,maxeval=maxeval1,check_derivatives = check_derivatives1),w_now=w_now,beta1=lambda,
              trans_cost=trans_cost,principal1=principal1,rand2=rand2,loss1=loss1,pro_dict=pro_dict1,k=k,mu=mu)
  }else if(method=='log'){
    w1<-slsqp(w1, log_find_w, lower = lower_bound, upper =upper_bound,
              hin =eval_g0 ,hinjac = hinjac.hs100, heq = eval_h0, heqjac = heqjac.hs100,
              nl.info = FALSE, control = list(xtol_rel = tol,maxeval=maxeval1,check_derivatives = check_derivatives1),w_now=w_now,beta1=lambda,
              trans_cost=trans_cost,principal1=principal1,rand2=rand2,loss1=loss1,pro_dict=pro_dict1,k=k,mu=mu )
  }else if(method=='expo'){
    w1<-slsqp(w1, expo_find_w, lower = lower_bound, upper =upper_bound,
              hin =eval_g0 ,hinjac = hinjac.hs100, heq = eval_h0, heqjac = heqjac.hs100,
              nl.info = FALSE, control = list(xtol_rel = tol,maxeval=maxeval1,check_derivatives = check_derivatives1),w_now=w_now,beta1=lambda,
              trans_cost=trans_cost,principal1=principal1,rand2=rand2,loss1=loss1,pro_dict=pro_dict1,k=k,mu=mu )
  }
  w1<-w1$par
  if(sum(w1)>1){
    w1[N+1]=sum(w1)-1
  }else{
    w1[N+1]=0
    w1[N]=w1[N]+1-sum(w1)
  }
  method<-rep(method,N+1)  
  weights2<<-data.frame(
    method=method,
    asset_name=c(asset_name1,"borrow"),
    weights=w1
  )
  return(weights2)
}

call_scenario_cost_BN<-function(uti,assets_num1,lambda1,asset_ret1,asset_var1,
                                asset_corr1,sample_number1,extreme_stress_loss,pro_dict,
                                principal1,trans_cost,w_now,lower_bounds,upper_bounds,subjective_k,
                                convexity_bounds,linear_bounds,asset_name1,maxeval1){
  uti<-uti
  assets_num1<-as.double(assets_num1)
  lambda1<-lambda1
  asset_ret1<-as.double(strsplit(asset_ret1,",")[[1]])
  asset_var1<-as.double(strsplit(asset_var1,",")[[1]])
  asset_corr1<-as.double(strsplit(asset_corr1,",")[[1]])
  sample_number1<-as.double(sample_number1)
  extreme_stress_loss<-as.double(strsplit(extreme_stress_loss,",")[[1]])
  # cond_table<-build_cond(affect_relation,prob_table)
  principal1<<-as.double(principal1)
  w_now<-as.double(strsplit(w_now,",")[[1]])
  lower_bound<-as.double(strsplit(lower_bounds,",")[[1]])
  upper_bound<-as.double(strsplit(upper_bounds,",")[[1]])
  k<-as.double(subjective_k)
  if(length(convexity_bounds)==0){
    convexity_bounds=NULL
  }else{
    convexity_bounds<-eval(parse(text=convexity_bounds))}
  if(length(linear_bounds)==0){
    linear_bounds=NULL
  }else{
    linear_bounds<-eval(parse(text=linear_bounds))}
  asset_name1<-strsplit(asset_name1,",")[[1]]
  weights=scenario_cost_BN(uti,assets_num1,lambda1,asset_ret1,asset_var1,
                           asset_corr1,sample_number1,extreme_stress_loss,pro_dict,
                           principal1,trans_cost,w_now,lower_bound,upper_bound,k,
                           convexity_bounds,linear_bounds,asset_name1,maxeval1)
  return(weights)
}
function_logistic<-function(x){
  return(1/(1+exp(-x*(x_mid-x_1)))-prob2/(1+exp(-x*(x_downturning-x_1)))-(1-prob2)/(1+exp(-x*(x_upturning-x_1))))
}
utility<-function(x){
  return((x<x_downturning)*(l+AA*log(1+k_2*x))+(x>=x_downturning)*(1/(1+exp(-k_1*(x-x_1)))))
}
utility_solve<-function(x_extreme,x_downturning,x_upturning,x_mid,prob2){
  x_extreme<<-x_extreme
  x_downturning<<-x_downturning
  x_upturning<<-x_upturning
  x_mid<<-x_mid
  prob2<<-prob2
  x_1<<-x_upturning
  k_1<<-uniroot.all(function_logistic,c(0,100))[-1]
  k_2<<--1/x_extreme
  AA<<-k_1*exp(-k_1*(x_downturning-x_1))/((1+exp(-k_1*(x_downturning-x_1)))^2)*(1+k_2*x_downturning)/k_2
  l<<-1/(1+exp(-k_1*(x_downturning-x_1)))-AA*log(1+k_2*x_downturning)
  result<-c(x_downturning,x_1,k_1,k_2,l,AA)
  return(result)
}
combo_uility<-function(x,x_downturning,AA,l,k_2,k_1,x_1){
  return((x<x_downturning)*(l+AA*log(1+k_2*x))+(x>=x_downturning)*(1/(1+exp(-k_1*(x-x_1)))))
}

get_combo_ut<-function(ret,x_downturning,AA,l,k_2,k_1,x_1,w_now,w1,tcost){
  return(combo_uility(ret+tcost,x_downturning,AA,l,k_2,k_1,x_1))
}
get_extreme_combo_uti<<-function(pro_dict,loss,x_downturning,AA,l,k_2,k_1,x_1,w_now,w,tcost,mu){
  rett<-0
  u<-rep(1,nrow(pro_dict)-1)
  rett<-sum(pro_dict[1,2:ncol(pro_dict)]*combo_uility((1+t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)]%*%(loss*w)+(u-t(pro_dict)[2:ncol(pro_dict),2:nrow(pro_dict)])%*%(mu*w))+tcost,x_downturning,AA,l,k_2,k_1,x_1))
#   for(i in 2:ncol(pro_dict)){
#     rett<-rett+pro_dict[1,i]*combo_uility((1+sum(pro_dict[2:nrow(pro_dict),i]*w*loss+(u-pro_dict[2:nrow(pro_dict),i])*w*mu)+tcost),x_downturning,AA,l,k_2,k_1,x_1)
#   }
  return(rett)
}
combo_find_w<-function(w1,w_now,x_downturning,AA,l,k_2,k_1,x_1,trans_cost,principal1,rand2,loss1,pro_dict,k,mu){
  tcost<-cost(w_now,w1,trans_cost,principal1)
  exp_pow_ut<-(1-k)*mean(na.omit(get_combo_ut(rand2 %*% w1,x_downturning,AA,l,k_2,k_1,x_1,w_now,w1,tcost)))+(k/(1-pro_dict$p0[1]))*get_extreme_combo_uti(pro_dict,loss1,x_downturning,AA,l,k_2,k_1,x_1,w_now,w1,tcost,mu)
  return(-exp_pow_ut)
}
scenario_cost_BN2<-function(assets_num,x_1,k_1,k_2,AA,l,x_downturning,asset_ret,asset_var,asset_corr,sample_number,extreme_stress_loss,pro_dict,principal1,
                           trans_cost,w_now,lower_bounds,upper_bounds,subjective_k,
                           convexity_bounds,linear_bounds,asset_name1=NULL,maxeval1){
  ##
  #
  ##trans_cost matrix like
  method="combo"
  N=assets_num
  mu<<-asset_ret
  asset_cor<-matrix(1,N,N)
  asset_cor[upper.tri(asset_cor)]=asset_corr
  asset_cor[lower.tri(asset_cor)]=t(asset_cor)[lower.tri(asset_cor)]
  asset_cov<-r2cov(sd =sqrt(asset_var),R = asset_cor)
  if(length(asset_name1)==0){
    asset_name1=rep("asset_",N)
    for(i in 1:N){
      asset_name1[i]=paste0(asset_name1[i],i)
    }
  }
  margins_r<-rep("norm",N)
  paramMargins_r <- list()
  # Now the new experiments
  for(i in 1:N){
    paramMargins_r[[length(paramMargins_r)+1]] <- list(mean =asset_ret[i], sd =sqrt(asset_var[i]))
  }
  N_sample<-sample_number
  k<<-subjective_k
  rand2<<-generate_N_rand(N,asset_corr,margins_r,paramMargins_r,N_sample)
  if(assets_num>=6){
    pro_dict1<<-pro_dict[,which(pro_dict[1,]>0.0001)]
  }else{
    pro_dict1<<-pro_dict
  }
  loss1<<-extreme_stress_loss
  trans_cost<<-trans_cost
  w_now<<-w_now
  w1<<-w_now
  eval_g0 <<- convexity_bounds
  eval_h0 <<- linear_bounds
  lower_bound<<-lower_bounds
  upper_bound<<-upper_bounds
  if(is.null(eval_g0)){
    hinjac.hs100<<-NULL
  }else{
    hinjac.hs100 <<- function(x) nl.jacobian(x, eval_g0, heps = 1e-2)
  }
  if(is.null(eval_h0)){
    heqjac.hs100<<-NULL
  }else{
    heqjac.hs100<<-function(x) nl.jacobian(x, eval_h0, heps = 1e-2)
  }
  if(assets_num>6){
    tol=1e-4
    maxeval1 = maxeval1
    check_derivatives1=FALSE
  }else{

    tol<-1e-8
    maxeval1 <- maxeval1
    check_derivatives1=TRUE
  }
 w1<-auglag(w1, combo_find_w, lower = lower_bound, upper =upper_bound,
              hin =eval_g0 ,hinjac = hinjac.hs100, heq = eval_h0, heqjac = heqjac.hs100,
              nl.info = FALSE, control = list(xtol_rel = tol,maxeval=maxeval1,check_derivatives = check_derivatives1),x_downturning=x_downturning,AA=AA,k_2=k_2,k_1=k_1,x_1=x_1,l=l
           ,w_now=w_now, trans_cost=trans_cost,principal1=principal1,rand2=rand2,loss1=loss1,pro_dict=pro_dict1,k=k,mu=mu )
  w1<-w1$par
  if(sum(w1)>1){
    w1[N+1]=sum(w1)-1
  }else{
    w1[N+1]=0
    w1[N]=w1[N]+1-sum(w1)
  }
  method<-rep(method,N+1)  
  weights2<<-data.frame(
    method=method,
    asset_name=c(asset_name1,"borrow"),
    weights=w1
  )
  return(weights2)
}
call_scenario_cost_BN2<-function(assets_num1,x_extreme,x_downturning,x_mid2,x_upturning,prob2,asset_ret1,asset_var1,
                                 asset_corr1,sample_number1,extreme_stress_loss,pro_dict,
                                 principal1,trans_cost,w_now,lower_bounds,upper_bounds,subjective_k,
                                 convexity_bounds,linear_bounds,asset_name1,maxeval1){
  assets_num1<-as.double(assets_num1)
  x_extreme<<-x_extreme
  x_downturning<<-x_downturning
  x_upturning<<-x_upturning
  x_mid<<-x_mid2
  prob2<<-prob2
  x_1<<-x_upturning
  k_1<<-uniroot.all(function_logistic,c(0,100))[-1]
  k_2<<--1/x_extreme
  AA<<-k_1*exp(-k_1*(x_downturning-x_1))/((1+exp(-k_1*(x_downturning-x_1)))^2)*(1+k_2*x_downturning)/k_2
  l<<-1/(1+exp(-k_1*(x_downturning-x_1)))-AA*log(1+k_2*x_downturning)
  asset_ret1<-as.double(strsplit(asset_ret1,",")[[1]])
  asset_var1<-as.double(strsplit(asset_var1,",")[[1]])
  asset_corr1<-as.double(strsplit(asset_corr1,",")[[1]])
  sample_number1<-as.double(sample_number1)
  extreme_stress_loss<-as.double(strsplit(extreme_stress_loss,",")[[1]])
  # cond_table<-build_cond(affect_relation,prob_table)
  principal1<<-as.double(principal1)
  w_now<-as.double(strsplit(w_now,",")[[1]])
  lower_bound<-as.double(strsplit(lower_bounds,",")[[1]])
  upper_bound<-as.double(strsplit(upper_bounds,",")[[1]])
  k<-as.double(subjective_k)
  if(length(convexity_bounds)==0){
    convexity_bounds=NULL
  }else{
    convexity_bounds<-eval(parse(text=convexity_bounds))}
  if(length(linear_bounds)==0){
    linear_bounds=NULL
  }else{
    linear_bounds<-eval(parse(text=linear_bounds))}
  asset_name1<-strsplit(asset_name1,",")[[1]]
  weights=scenario_cost_BN2(assets_num1,x_1,k_1,k_2,AA,l,x_downturning,asset_ret1,asset_var1,
                           asset_corr1,sample_number1,extreme_stress_loss,pro_dict,
                           principal1,trans_cost,w_now,lower_bound,upper_bound,k,
                           convexity_bounds,linear_bounds,asset_name1,maxeval1)
  return(weights)
}
scenario_cost_BN_matrix<-function(method,assets_num,lambdas,asset_ret,asset_var,asset_corr,sample_number,extreme_stress_loss,pro_dict,principal1,
                           trans_cost,w_now,lower_bounds,upper_bounds,subjective_ks,
                           convexity_bounds,linear_bounds,asset_name1=NULL,maxeval1){
  ##
  #
  ##trans_cost matrix like
  N=assets_num
  # A=lambda
  mu<<-asset_ret
  asset_cor<-matrix(1,N,N)
  asset_cor[upper.tri(asset_cor)]=asset_corr
  asset_cor[lower.tri(asset_cor)]=t(asset_cor)[lower.tri(asset_cor)]
  asset_cov<-r2cov(sd =sqrt(asset_var),R = asset_cor)
  if(length(asset_name1)==0){
    asset_name1=rep("asset_",N)
    for(i in 1:N){
      asset_name1[i]=paste0(asset_name1[i],i)
    }
  }
  margins_r<-rep("norm",N)
  paramMargins_r <- list()
  # Now the new experiments
  for(i in 1:N){
    paramMargins_r[[length(paramMargins_r)+1]] <- list(mean =asset_ret[i], sd =sqrt(asset_var[i]))
  }
  N_sample<-sample_number
  k<<-subjective_k
  rand2<<-generate_N_rand(N,asset_corr,margins_r,paramMargins_r,N_sample)
  if(assets_num>=6){
    pro_dict1<<-pro_dict[,which(pro_dict[1,]>0.0001)]
  }else{
    pro_dict1<<-pro_dict
  }
  loss1<<-extreme_stress_loss
  trans_cost<<-trans_cost
  w_now<<-w_now
  w1<<-w_now
  eval_g0 <<- convexity_bounds
  eval_h0 <<- linear_bounds
  lower_bound<<-lower_bounds
  upper_bound<<-upper_bounds
  if(is.null(eval_g0)){
    hinjac.hs100<<-NULL
  }else{
    hinjac.hs100 <<- function(x) nl.jacobian(x, eval_g0, heps = 1e-2)
  }
  
  if(is.null(eval_h0)){
    heqjac.hs100<<-NULL
  }else{
    heqjac.hs100<<-function(x) nl.jacobian(x, eval_h0, heps = 1e-2)
  }
  if(assets_num>6){
    tol=1e-4
    maxeval1 = maxeval1
    check_derivatives1=FALSE
  }else{
    tol<-1e-8
    maxeval1 = 1000
    check_derivatives1=TRUE
  }
  for(kk in 1:length(method)){
    if(method[kk]=='power'){
      for(i in 1:length(lambdas)){
        for(j in 1:length(subjective_ks)){
          w_temp<-slsqp(w1, power_find_w, lower = lower_bound, upper =upper_bound,
                        hin =eval_g0 ,hinjac = hinjac.hs100, heq = eval_h0, heqjac = heqjac.hs100,
                        nl.info = FALSE, control = list(xtol_rel = tol,maxeval=maxeval1,check_derivatives = check_derivatives1),w_now=w_now,beta1=lambdas[i],
                        trans_cost=trans_cost,principal1=principal1,rand2=rand2,loss1=loss1,pro_dict=pro_dict1,k=subjective_ks[j],mu=mu )
          w_temp<-w_temp$par
          if(sum(w_temp)>1){
            w_temp[N+1]=sum(w_temp)-1
          }else{
            w_temp[N+1]=0
            w_temp[N]=w_temp[N]+1-sum(w_temp)
          }
          if(kk==1&&i==1&&j==1){
            w_temp<-c(lambdas[i],subjective_ks[j],w_temp)
            weights_matrix<-data.frame(t(w_temp))
            colnames(weights_matrix)<-c("lambda","k",asset_name1,"borrow")
          }else{
            w_temp<-c(lambdas[i],subjective_ks[j],w_temp)
            weights_matrix<-rbind(weights_matrix,w_temp)
          }
        }
      }
    }else if(method[kk]=='log'){
      for(i in 1:length(lambdas)){
        for(j in 1:length(subjective_ks)){
          w_temp<-slsqp(w1, log_find_w, lower = lower_bound, upper =upper_bound,
                        hin =eval_g0 ,hinjac = hinjac.hs100, heq = eval_h0, heqjac = heqjac.hs100,
                        nl.info = FALSE, control = list(xtol_rel = tol,maxeval=maxeval1,check_derivatives = check_derivatives1),w_now=w_now,beta1=lambdas[i],
                        trans_cost=trans_cost,principal1=principal1,rand2=rand2,loss1=loss1,pro_dict=pro_dict1,k=subjective_ks[j],mu=mu )
          w_temp<-w_temp$par
          if(sum(w_temp)>1){
            w_temp[N+1]=sum(w_temp)-1
          }else{
            w_temp[N+1]=0
            w_temp[N]=w_temp[N]+1-sum(w_temp)
          }
          if(kk==1&&i==1&&j==1){
            w_temp<-c(lambdas[i],subjective_ks[j],w_temp)
            weights_matrix<-data.frame(t(w_temp))
            colnames(weights_matrix)<-c("lambda","k",asset_name1,"borrow")
          }else{
            w_temp<-c(lambdas[i],subjective_ks[j],w_temp)
            weights_matrix<-rbind(weights_matrix,w_temp)
          }
        }
      }
      
    }else if(method[kk]=='expo'){
      for(i in 1:length(lambdas)){
        for(j in 1:length(subjective_ks)){
          w_temp<-slsqp(w1, expo_find_w, lower = lower_bound, upper =upper_bound,
                        hin =eval_g0 ,hinjac = hinjac.hs100, heq = eval_h0, heqjac = heqjac.hs100,
                        nl.info = FALSE, control = list(xtol_rel = tol,maxeval=maxeval1,check_derivatives = check_derivatives1),w_now=w_now,beta1=lambdas[i],
                        trans_cost=trans_cost,principal1=principal1,rand2=rand2,loss1=loss1,pro_dict=pro_dict1,k=subjective_ks[j],mu=mu )
          w_temp<-w_temp$par
          if(sum(w_temp)>1){
            w_temp[N+1]=sum(w_temp)-1
          }else{
            w_temp[N+1]=0
            w_temp[N]=w_temp[N]+1-sum(w_temp)
          }
          if(kk==1&&i==1&&j==1){
            w_temp<-c(lambdas[i],subjective_ks[j],w_temp)
            weights_matrix<-data.frame(t(w_temp))
            colnames(weights_matrix)<-c("lambda","k",asset_name1,"borrow")
          }else{
            w_temp<-c(lambdas[i],subjective_ks[j],w_temp)
            weights_matrix<-rbind(weights_matrix,w_temp)
          }
        }
      }  }
  }
  methods<-c()
  for(kk in 1:length(method)){
    methods<-c(methods,rep(method[kk],length(lambdas)*length(subjective_ks)))
  }
  weights_matrix<-cbind(methods,weights_matrix)
  return(weights_matrix)
}
call_scenario_cost_BN_matrix<-function(uti2,assets_num1,lambdas,asset_ret1,asset_var1,
                             asset_corr1,sample_number2,extreme_stress_loss,pro_dict,
                             principal1,trans_cost,w_now,lower_bounds,upper_bounds,ks,
                             convexity_bounds,linear_bounds,asset_name1,maxeval1){
  uti<-uti2
  assets_num1<-as.double(assets_num1)
  asset_ret1<-as.double(strsplit(asset_ret1,",")[[1]])
  asset_var1<-as.double(strsplit(asset_var1,",")[[1]])
  asset_corr1<-as.double(strsplit(asset_corr1,",")[[1]])
  sample_number2<-as.double(sample_number2)
  extreme_stress_loss<-as.double(strsplit(extreme_stress_loss,",")[[1]])
  # cond_table<-build_cond(affect_relation,prob_table)
  principal1<<-as.double(principal1)
  w_now<-as.double(strsplit(w_now,",")[[1]])
  lower_bound<-as.double(strsplit(lower_bounds,",")[[1]])
  upper_bound<-as.double(strsplit(upper_bounds,",")[[1]])
  lambdas=as.double(strsplit(lambdas,",")[[1]])
  ks<-as.double(strsplit(ks,",")[[1]])
  if(length(convexity_bounds)==0){
    convexity_bounds=NULL
  }else{
    convexity_bounds<-eval(parse(text=convexity_bounds))}
  if(length(linear_bounds)==0){
    linear_bounds=NULL
  }else{
    linear_bounds<-eval(parse(text=linear_bounds))}
  asset_name1<-strsplit(asset_name1,",")[[1]]
  weights=scenario_cost_BN_matrix(uti,assets_num1,lambdas,asset_ret1,asset_var1,
                           asset_corr1,sample_number2,extreme_stress_loss,pro_dict,
                           principal1,trans_cost,w_now,lower_bound,upper_bound,ks,
                           convexity_bounds,linear_bounds,asset_name1,maxeval1)
  return(weights)
}

