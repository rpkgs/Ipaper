#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP senslope(const NumericVector & x) {
    NumericVector xx(x);
    // return x * 2;
    int n = xx.size();
    int len = (n * n - n) / 2;
    // Rcout << len << std::endl;

    NumericVector V = NumericVector(len, NA_REAL); // int V[len];

    int k = 0;
    for (int j = 1; j < n; j++) {
        for (int i = 0; i < j; i++) {
            V[k] = (xx[j] - xx[i]) / (j - i);
            k++;
            // Rcout << i << "," << j << std::endl;
            // Rcout << "k = " << k << ": " << V[k] << std::endl;
        }
    }
    // Rcout << V << std::endl;
    double slope = Rcpp::median(V, true); //rm_na is true
    // Rcout << slope << std::endl;
    return wrap(slope);
}

int sign(double x) {
    return x > 0 ? 1 : (x == 0 ? 0 : -1);
}

// [[Rcpp::export]]
int Sf(NumericVector x) {
    int n = x.size();
    int S = 0;

    for (int j = 1; j < n; j++) {
        for (int i = 0; i < j; i++) {
            S += sign(x[j] - x[i]);
        }
    }
    return S;
}

/*** R
# x <- rnorm(50)
# library(microbenchmark)
# # library(Rcpp)
# # microbenchmark(senslope(x), times=100)
# 
# source("E:/GitHub/Vegetation/TP_phenology/R/mainfunc/main_TREND.R", encoding = "utf-8")
# x <- rnorm(32)
# microbenchmark(mkTrend(x),
#                mkTrend.rcpp(x), times=100)
*/

// SEXP senslope(const NumericVector& x);
// RcppExport SEXP sourceCpp_1_senslope(SEXP xSEXP) {
//   BEGIN_RCPP
//   Rcpp::RObject rcpp_result_gen;
//   Rcpp::RNGScope rcpp_rngScope_gen;
//   Rcpp::traits::input_parameter< const NumericVector& >::type x(xSEXP);
//   rcpp_result_gen = Rcpp::wrap(senslope(x));
//   return rcpp_result_gen;
//   END_RCPP
// }
// // Sf
// int Sf(NumericVector x);
// RcppExport SEXP sourceCpp_1_Sf(SEXP xSEXP) {
//   BEGIN_RCPP
//   Rcpp::RObject rcpp_result_gen;
//   Rcpp::RNGScope rcpp_rngScope_gen;
//   Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
//   rcpp_result_gen = Rcpp::wrap(Sf(x));
//   return rcpp_result_gen;
//   END_RCPP
// }