#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame ai_df(NumericVector year_c, NumericVector month_c, NumericVector day_c, NumericVector mat_months) {
  
  int index_length mat_months.size();
  
  return DataFrame::create(
    Named("year_m") = year_c
  );
}