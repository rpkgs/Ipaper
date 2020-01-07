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

// [[Rcpp::export]]
NumericVector varS(NumericVector x, NumericVector rof, int S)
{
    int n = x.size();
    // int S = 0;
    double cte = 2.0/(n * (n - 1) * (n - 2));
    double ess = 0;
    for (int i =1; i <= n-1; i++) {
        ess = ess + (n - i) * (n - i - 1) * (n - i - 2) * rof[i-1];
    }

    double essf = 1 + ess * cte;
    double var_S = n * (n - 1) * (2 * n + 5)/18;

    NumericVector aux = unique(x);
    int m = aux.size();
    if (m < n) {
        int tie;
        for (int i = 0; i < m; i++) {
            tie = 0;
            for (int j = 0; j < n; j++) {
                if (x[j] == aux[i]) tie++;
            }
            if (tie > 1)
                var_S = var_S - tie * (tie - 1) * (2 * tie + 5) / 18;
        }
    }

    double VS = var_S * essf;
    double z, z0;
    if (S == 0) {
        z = 0;
        z0 = 0;
    } else if (S > 0) {
        z  = (S - 1) / sqrt(VS);
        z0 = (S - 1) / sqrt(var_S);
    } else {
        z = (S + 1) / sqrt(VS);
        z0 = (S + 1) / sqrt(var_S);
    }

    return NumericVector::create(
        _["essf"] = essf,
        _["var.S"] = var_S,
        _["z0"] = z0,
        _["z"] = z
    );
    // double VS = var_S * essf;    
    // return S;
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
