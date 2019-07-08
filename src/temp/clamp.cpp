#include <Rcpp.h>
using namespace Rcpp;

// template <typename T>
// void clamp_min(T& x, T a) {
//     if (x < a) x = a;
//     // if (b != NULL) {
//     //     if (x > b) x = b;
//     // }
// }
// // //' @rdname clamp
// // // [[Rcpp::export]]
// // template<typename T>
// // void clamp_min(T& x, T a) {
// //     if (x < a) x = a;
// //     // if (x > b) x = b;
// // }
// // 
// // //' @rdname clamp
// // // [[Rcpp::export]]
// // template<typename T>
// // void clamp_max(T& x, T b) {
// //     // if (x < a) x = a;
// //     if (x > b) x = b;
// // }
// // template<typename T>

//' @export
// [[Rcpp::export]]
void clamp_min2(NumericMatrix xx, double a, bool na_rm = true) {
    // int n = x.length();
    // NumericMatrix xx(x);
    for (int i = 0; i < xx.nrow(); i++) {
        for (int j = 0; j < xx.ncol(); j++) {
            // bool b = xx(i, j) < a;
            if (xx(i, j) < a) {
                // Rcout << xx(i, j) << b <<std::endl;
                xx(i, j) = a;
            }
        }
    }
    
    if (na_rm) {
        for (int i = 0; i < xx.nrow(); i++) {
            for (int j = 0; j < xx.ncol(); j++) {
                if (!Rcpp::traits::is_finite<REALSXP>(xx(i, j))) {
                    xx(i, j) = a;
                }
            }
        }
    }
}

/*** R
{
    x <- matrix(1:16, 4, 4)*1.0
    x[4] <- NA
    clamp_min2(x, 4, TRUE)
    x
}
# library(microbenchmark)
# # library(Rcpp)
# # microbenchmark(senslope(x), times=100)
# 
# source("E:/GitHub/Vegetation/TP_phenology/R/mainfunc/main_TREND.R", encoding = "utf-8")
# x <- rnorm(32)
# microbenchmark(mkTrend(x),
#                mkTrend.rcpp(x), times=100)
*/
