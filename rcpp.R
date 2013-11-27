#install.packages('rbenchmark)
require(rbenchmark)
set.seed(89726341)
#We write fib in a stupid way to show how expensive function calls are
fibR<-function(n){  
  if (n==0) return(0)
  if (n==1) return (1)
  return (fibR(n-1)+fibR(n-2))
}

# CPP function for reference
# int fibCpp(const int x) {
#   if (x==0) return 0;
#   if (x==1) return 1;
#   return fibCpp(x-1) + fibCpp (x-2);
# }

## The functions will be called 3 times
benchmark(fibR(1),fibCpp(1),replications=100000)

##Function calls are expensive in R. This will call fibr 2,692,537 times.
### fibCpp is also called the same number of times.
benchmark(fibCpp(30),fibR(30),replications=1)

##any is expensive in R (evaluates all elements even if the ifrst is true)
x<-rep(-1,1000000)
benchmark(any(x<0),cppany(x),replications=1000)

#CPP function for reference
# bool cppany(Rcpp::NumericVector V){
#   return is_true(any(V<0));
# }


#almost anything is expensive in R, even vectorized functions like ifelse...
x<-sample(100000)
y<-sample(100000)
benchmark(ifelse(x+y>100000,0,1),cppifelse(x,y))

#CPP function for reference
# IntegerVector cppifelse(NumericVector V1,NumericVector V2){
#   return ifelse(V1+V2>100000,0,1);
# }


#What isnt expensive in R? Built in numerical methods can be fast
x1<-matrix(rnorm(50000),ncol=100)
x2<-t(x1)
benchmark(x1%*%x2,acppmm(x1,x2))

# CPP function for reference
# mat acppmm(mat X, mat Y){
#   return X*Y;
# }


#But sometimes they are a bit slower than C++
x<-matrix(rnorm(1000000),nrow=1000)
benchmark(acppinv(x),solve(x),replications=1)

# CPP function for reference
# mat acppinv(mat X){
#   return inv(X);
# }

#lets do something really stupid and write our own matrix multiplication function
# This is to show when R is /really/ slow. Accessing elements by index is a disaster.

mrmm<-function(x1,x2){
  d11=nrow(x1)
  d12=ncol(x1)
  d22=ncol(x2) 
  X<-matrix(0,nrow=d11,ncol=d22)
  for(i in 1:d11){
    for(j in 1:d11){      
      for(a in 1:d12){
        X[i,j]=X[i,j]+x1[i,a]*x2[a,j]
      }
      
    }
  }
  return (X)
}

#CPP code for reference
# mat macppmm(mat X1,mat X2){
#   int d11=X1.n_rows;
#   int d12=X1.n_cols;
#   int d22=X2.n_cols; 
#   arma::mat X(d11,d22);
#   for(int i=0; i<d11;++i){
#     for(int j=0;j<d11;++j){      
#       for(int a=0;a<d12;++a){
#         X(i,j)=X(i,j)+X1(i,a)*X2(a,j);
#       }
#       
#     }
#   }
#   return X;
# }

#oops
benchmark(x1%*%x2,macppmm(x1,x2),mrmm(x1,x2),replications=1)