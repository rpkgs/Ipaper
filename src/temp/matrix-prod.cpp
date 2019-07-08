// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]
#include <RcppArmadillo.h>
#include <RcppEigen.h>

// https://stackoverflow.com/questions/35923787/fast-large-matrix-multiplication-in-r
// using namespace Rcpp;

//' @name matmult_
//' @title Fast Matrix Multiplication 
//' @rdname matmult_

//' @rdname matmult_
//' @export
// [[Rcpp::export]]
SEXP matmult_arma(arma::mat A, arma::mat B){
    arma::mat C = A * B;
    return Rcpp::wrap(C);
}


//' @rdname matmult_
//' @export
// [[Rcpp::export]]
SEXP matmult_eigen(Eigen::MatrixXd A, Eigen::MatrixXd B){
    Eigen::MatrixXd C = A * B;    
    return Rcpp::wrap(C);
}


//' @rdname matmult_
//' @export
// [[Rcpp::export]]
SEXP matmult_eigenMap(const Eigen::Map<Eigen::MatrixXd> A, Eigen::Map<Eigen::MatrixXd> B){
    Eigen::MatrixXd C = A * B;
    return Rcpp::wrap(C);
}
