#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector bootstrap(IntegerVector iter, IntegerVector term, IntegerVector bs_group, NumericVector yield, NumericVector price, NumericVector dcf, int r_count){
  
  int current_iter = 0;
  int current_group = 0;
  int chapter_start = 0;
  
  for (int i = 0; i < r_count; i++){
    
    if(iter[i] != current_iter){
      current_iter = iter[i];
      dcf[i] = 0;
    }else{
    
      if(bs_group[i] != current_group){
        current_group = bs_group[i];
        chapter_start = i;
      }
    
      double prior_dcf = 0;
      
      if(i > chapter_start){
        for (int s = chapter_start; s < i; s++){
          prior_dcf = prior_dcf + dcf[s] * yield[i]/2 * 100;
        }
      }
      
      dcf[i] = (price[i] - prior_dcf)/(100 + 100*yield[i]/2);
      
    }
  }
  return dcf;
}