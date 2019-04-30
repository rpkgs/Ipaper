#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp; 
using namespace arma;

// [[Rcpp::export]]
SEXP split_array(NumericVector x) {
  Rcpp::NumericVector xx(x);
  
  IntegerVector dims = x.attr("dim");
  arma::cube Q(x.begin(), dims[0], dims[1], dims[2], false);
  
  int nrow = dims[0], ncol = dims[1], nslice = dims[2];
  List res(nrow*ncol);
  
  for (int i = 0; i < nrow; i++){
    for (int j = 0; j < ncol; j++){
        arma::cube mem_ex = Q.tube(i, j);
        arma::colvec col_vec(mem_ex.memptr(), mem_ex.n_elem, 1, false);
        // arma::colvec colvec = Q(arma::span(i), arma::span(j), arma::span::all);
        // Rcout << colvec << endl;
        res[i*ncol + j] = col_vec;
    }
  }
  return wrap(res);
}

/*** R
# timesTwo(42)
# n = 1e7; x <- array(rnorm(n*3*3), dim = c(3, 3, n))
# y <- split_array(x)
*/


// SEXP cube_means(Rcpp::NumericVector vx) {
//   Rcpp::NumericVector xx(vx);
//   IntegerVector x_dims = vx.attr("dim");
//   
//   arma::cube Q(vx.begin(), x_dims[0], x_dims[1], x_dims[2], false);
//   
//   int i = 0, j = 0;
//   arma::cube mem_ex = Q.tube(i, j);
//   arma::colvec col_vec(mem_ex.memptr(), mem_ex.n_elem, 1, false);
//   
//   // NumericVector v = vx(1, 1, _);
//   // arma::mat result(x.n_cols, x.n_slices);
//   // for (unsigned int i = 0; i < x.n_slices; i++) {
//   //   result.col(i) = arma::conv_to<arma::colvec>::from(arma::mean(x.slice(i)));  
//   // }
//   
//   return wrap(col_vec);
// }

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically 
// run after the compilation.
//

