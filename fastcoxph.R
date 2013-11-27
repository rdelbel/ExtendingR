#uncomment if you need to install packages
#install.packages("survival","rbenchmark")
 require(survival)
 require(rbenchmark)
 fcoxph<-function(time,status,x,strata=NULL){
   n=length(time)
   if(class(x)!="matrix") x=matrix(x)
   if(!is.null(strata)){
     sorted = order(strata, time)
     newstrata=as.integer(c(1 * (diff(as.numeric(strata[sorted])) !=0), 1)) 
   }else{
     sorted=order(time)
     newstrata=rep(0L,n)
   }
   .Call("coxfit6", 20L, time[sorted],   as.integer(status[sorted]),    x[sorted,,drop=F],rep(0,n),rep(1,n), newstrata,1L, 1e-09,  1.818989e-12,rep(0,ncol(x)), 1L)
 }
 rm(lung) #just incase
 
 #Make status 0,1
 lung[,3]=lung[,3]-1
 #fill in NA for demonstration
 lung[is.na(lung[,7]),7]=80
 #Look at the output
 fcoxph(lung[,2],lung[,3],cbind(lung[,4],lung[,7]),lung[,5])
 #imat is a vector of the  variance-covariance matrix
 #Need to be careful about special cases
 #1) If the model does not converge $flag==1000
 #2) If the model is singular in a covariate the corresponding $coef==0
 #3) If the model fails to converge in a specific beta the corresponding abs(beta)>15
 #4) Sometimes a coef can be NaN
 
#return HR, CI, and p-value of first coef
nicefcoxph<-function(time,status,x,strata=NULL){
  n=length(time)
  if(class(x)!="matrix") x=matrix(x)
  if(!is.null(strata)){
    sorted = order(strata, time)
    newstrata=as.integer(c(1 * (diff(as.numeric(strata[sorted])) !=0), 1)) 
  }else{
    sorted=order(time)
    newstrata=rep(0L,n)
  }
  model=.Call("coxfit6", 20L, time[sorted],   as.integer(status[sorted]),    x[sorted,,drop=F],rep(0,n),rep(1,n),newstrata, 
        1L, 1e-09,  1.818989e-12,rep(0,ncol(x)), 1L)
  
  c(exp(c(model$coef[1],model$coef[1]-qnorm(.975)*sqrt(model$imat[1]),model$coef[1]+qnorm(.975)*sqrt(model$imat[1]))),1-pchisq(model$coef[1]^2/model$imat[1],1))
}

 #Assume data is already sorted
sfcoxph<-function(time,status,x,strata=NULL){
  n=length(time)
  if(class(x)!="matrix") x=matrix(x)
  if(!is.null(strata)){
    newstrata<-as.integer(c(1 * (diff(as.numeric(strata)) !=0), 1)) 
  }else{    
    newstrata=as.integer(rep(0,n))
  }
  .Call("coxfit6", 20L, time, as.integer(status), 
        x,rep(0,n),rep(1,n),newstrata, 
        1L,1e-09, 1.818989e-12, rep(0,ncol(x)), 1L)
}

#Assume data is already sorted and return HR CI and p-value 
snicefcoxph<-function(time,status,x,strata=NULL){
  n=length(time)
  if(class(x)!="matrix") x=matrix(x)
  if(!is.null(strata)){
    newstrata<-as.integer(c(1 * (diff(as.numeric(strata)) !=0), 1)) 
  }else{    
    newstrata=as.integer(rep(0,n))
  }
  model=.Call("coxfit6", 20L, time, as.integer(status), 
        x,rep(0,n),rep(1,n),newstrata, 
        1L,1e-09, 1.818989e-12, rep(0,ncol(x)), 1L)  
  
  c(exp(c(model$coef[1],model$coef[1]-qnorm(.975)*sqrt(model$imat[1]),model$coef[1]+qnorm(.975)*sqrt(model$imat[1]))),1-pchisq(model$coef[1]^2/model$imat[1],1))
}
#presort the data for the functions that assume sorted input data
slung=lung[order(lung[,5],lung[,2]),]

#Get the estimate, 95% CI and pvalue using coxph directly
 coxphout<-function(data){
   model=summary(coxph(Surv(data[,2],data[,3])~data[,4]+strata(data[,5])))
   c(model$conf.int[-2],model$coefficients[5])}

#benchmark the different methods. So fast.
benchmark(coxphout(lung),
          coxphout(slung),
          nicefcoxph(lung[,2],lung[,3],lung[,4],lung[,5]),
          snicefcoxph(slung[,2],slung[,3],slung[,4],slung[,5]),order="relative",replications=1000)

 #Are they equal? 
all.equal(nicefcoxph(lung[,2],lung[,3],lung[,4],lung[,5]),
          snicefcoxph(slung[,2],slung[,3],slung[,4],slung[,5]),
          coxphout(lung)) #Nice
