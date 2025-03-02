#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame ai_df(IntegerVector year_c, IntegerVector month_c, IntegerVector day_c, IntegerVector mat_months, LogicalVector c) {
  
  int index_length = mat_months.size();
  
  NumericVector year_d1 = index_length;
  NumericVector year_d2 = index_length;
  NumericVector month_d1 = index_length;
  NumericVector month_d2 = index_length;
  NumericVector day_d1 = index_length;
  NumericVector day_d2 = index_length;
  
  std::unordered_set<int> thirty_one = {1, 3, 5, 7, 8, 10, 12};
  
  for(int i = 0; i < index_length; i++){
    
    int m_next = ((mat_months[i]-1) % 6) + 1;
    
    int m2 = month_c[i] + m_next;
    int y2 = year_c[i];
    
    if(m2 > 12){
      y2 = y2++;
      m2 = m2 - 12;
    }
    
    int m1 = m2 - 6;
    int y1 = y2;
    
    if(m1 < 1){
      y1 = y1 - 1;
      m1 = m1 + 12;
    }
    
    int d = day_c[i];
    int d1 = d;
    int d2 = d;
    
    if(c[i]){
      
      if(thirty_one.count(m1)){
        d1 = 31;
      }else{
        d1 = 30;
      }
      
      if(thirty_one.count(m2)){
        d2 = 31;
      }else{
        d2 = 30;
      }
    }else{
      d1 = d;
      d2 = d;
    }
    
    if(m1 == 2){
      int bound = (y1 % 4) > 0;
      if(d1 > (29 - bound)){
        d1 = 29 - bound;
      }
    }
    
    if(m2 == 2){
      int bound = (y2 % 4) > 0;
      if(d2 > (29 - bound)){
        d2 = 29 - bound;
      }
    }
    
    year_d1[i] = y1;
    year_d2[i] = y2;
    month_d1[i] = m1;
    month_d2[i] = m2;
    day_d1[i] = d1;
    day_d2[i] = d2;
    
  }
  
  return DataFrame::create(
    Named("term") = mat_months,
    Named("year_d1") = year_d1,
    Named("month_d1") = month_d1,
    Named("day_d1") = day_d1,
    Named("year_d2") = year_d2,
    Named("month_d2") = month_d2,
    Named("day_d2") = day_d2
  );
}