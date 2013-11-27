//comments are after unless it is // [[Rcpp::export]] which is not a comment
// the space between // and [[]] is in  // [[Rcpp::export]
//press the source botton on the top right of this pane to combine R and C++


//always copy the next 3 lines
#include <RcppArmadillo.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]

// If you uncomment the next three lines then you dont have to write
// Rcpp::, arma::, or std::. You will still need Rcpp:: in the 
// // [[Rcpp:: ]] part

//using namespace Rcpp;
//using namespace arma;
//using namespace std;


// Before every function we want to export we write
// // [[Rcpp::export]]

// We can work with integers without any special conversion
// [[Rcpp::export]]
int fibCpp(const int x) {
  if (x==0) return 0;
  if (x==1) return 1;
  return fibCpp(x-1) + fibCpp (x-2);
}


// Lets just use rcpp to integrate R and C++

// convert to C++
//We have to start with a Rcpp::NumericVector and convert it to a std::vector<double> with Rcpp::as<>
// [[Rcpp::export]]
double cpp1(Rcpp::NumericVector R1){
  std::vector<double> C1 = Rcpp::as<std::vector<double> >(R1);
  //now C1 is a 'normal' C++ vector<double>. We can use fancy C++ methods on it.
   return std::accumulate(C1.begin(),C1.end(),0);  
}



// [[Rcpp::export]]
Rcpp::NumericVector cpp2(Rcpp::NumericVector R1){
  std::vector<double> C1 = Rcpp::as<std::vector<double> >(R1);
  //now C1 is a 'normal' C++ vector<double>. We can use fancy C++ methods on it.
   C1[0]=99;
   //R1 does not change
   //The vector we use as input does not change
   return R1;
}
// Lets integrate R and C

// [[Rcpp::export]]
Rcpp::NumericVector c1(Rcpp::NumericVector R1){
// Again we have to start with Rcpp::NumericVector and convert it do a double* object
  double* C1 = R1.begin();
  C1[0]=99;
  //Changing C1 changed R1!
  //It also changed the vector we used as input...
  //This might be what we want or might not... be careful!
  return R1;
}

// Here we use Rcpp objects so we write code in an R-like way.
// We need to return IntegerVector since we are returning integers.
// [[Rcpp::export]]
Rcpp::IntegerVector cppifelse(Rcpp::NumericVector V1, Rcpp::NumericVector V2){
  return ifelse(V1+V2>100000,0,1);
}

// To convert to bool we need to call is_true() first.
// This is because a boolean in R has 3 values: T, F, NA
// and bool only has 2: T, F. Here we only care about T or F
// So we call is_true() and NA will become F.
// [[Rcpp::export]]
bool cppany(Rcpp::NumericVector V){
  return is_true(any(V<0));
}


//Here We call arma matricies. Notice we dont have to convert the R matricies
// [[Rcpp::export]]
arma::mat acppmm(arma::mat X, arma::mat Y){
  return X*Y;
}

// [[Rcpp::export]]
arma::mat acppinv(arma::mat X){
  return inv(X);
}

// Manual arma matrix multiplication
// [[Rcpp::export]]
arma::mat macppmm(arma::mat X1, arma::mat X2){
  int d11=X1.n_rows;
  int d12=X1.n_cols;
  int d22=X2.n_cols; 
  arma::mat X(d11,d22);
  for(int i=0; i<d11;++i){
    for(int j=0;j<d11;++j){      
      for(int a=0;a<d12;++a){
        X(i,j)=X(i,j)+X1(i,a)*X2(a,j);
      }
      
    }
  }
  return X;
}





