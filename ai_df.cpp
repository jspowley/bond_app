#include <Rcpp.h>
#include <iostream>
#include <chrono>

using namespace Rcpp;

// [[Rcpp::export]]
DataFrame ai_df(DateVector date_c, IntegerVector mat_months, LogicalVector c) {
  
  int index_length = mat_months.size();
  
  DateVector date_1 = index_length;
  DateVector date_2 = index_length;
  DateVector maturity = index_length;
  IntegerVector dtm = index_length;
  IntegerVector days_in = index_length;
  IntegerVector days_through = index_length;
  DoubleVector ai = index_length;
  
  //NumericVector year_d1 = index_length;
  //NumericVector year_d2 = index_length;
  //NumericVector month_d1 = index_length;
  //NumericVector month_d2 = index_length;
  //NumericVector day_d1 = index_length;
  //NumericVector day_d2 = index_length;
  
  //NumericVector year_dtm = index_length;
  //NumericVector month_dtm = index_length;
  //NumericVector day_dtm = index_length;
  
  std::unordered_set<int> thirty_one = {1, 3, 5, 7, 8, 10, 12};
  
  for(int i = 0; i < index_length; i++){
    
    Date current_date = date_c[i];
    int d_c = current_date.getDay();
    int m_c = current_date.getMonth();
    int y_c = current_date.getYear();
    
    int m_next = ((mat_months[i]-1) % 6) + 1;
    
    int m2 = m_c + m_next;
    int y2 = y_c;
    
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
    
    int d = d_c;
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
    
    //year_d1[i] = y1;
    //year_d2[i] = y2;
    //month_d1[i] = m1;
    //month_d2[i] = m2;
    //day_d1[i] = d1;
    //day_d2[i] = d2;
    
    date_1[i] = Date(y1, m1, d1);
    date_2[i] = Date(y2, m2, d2);
    
    int dtm_next (((mat_months[i]-1) % 12) + 1);
    int m_dtm (m_c + dtm_next);
    int y_dtm (y_c);
    
    if(m_dtm > 12){
      y_dtm = y_dtm + ((m_dtm - 1) / 12);
      m_dtm = ((m_dtm - 1) % 12) + 1;
    }
    
    int d_dtm (d_c);
    
    if(c[i]){
      if(thirty_one.count(m_dtm)){
        d_dtm = 31;
      }else{
        d_dtm = 30;
      }
    }else{
      d_dtm = d;
    }
    
    if(m_dtm == 2){
      int bound = (y_dtm % 4) > 0;
      if(d_dtm > (29 - bound)){
        d_dtm = 29 - bound;
      }
    }
    
    //year_dtm[i] = y_dtm;
    //month_dtm[i] = m_dtm;
    //day_dtm[i] = d_dtm;
    
    maturity[i] = Date(y_dtm, m_dtm, d_dtm);
    
    Date dtm_1 = current_date;
    Date dtm_2 = maturity[i];
    dtm[i] = dtm_2 - dtm_1;
    
    Date days_in_1 = date_1[i];
    Date days_in_2 = date_2[i];
    days_in[i] = days_in_2 - days_in_1;

    Date days_through_1 = date_1[i];
    Date days_through_2 = current_date;
    days_through[i] = days_through_2 - days_through_1;
    
    double dt = days_through[i];
    double di = days_in[i];
    
    ai[i] = dt / di;
    
  }
  
  return DataFrame::create(
    //Named("term") = mat_months,
    //Named("year_d1") = year_d1,
    //Named("month_d1") = month_d1,
    //Named("day_d1") = day_d1,
    //Named("year_d2") = year_d2,
    //Named("month_d2") = month_d2,
    //Named("day_d2") = day_d2,
    //Named("year_dtm") = year_dtm,
    //Named("month_dtm") = month_dtm,
    //Named("day_dtm") = day_dtm
    Named("date_1") = date_1,
    Named("date_2") = date_2,
    Named("maturity") = maturity,
    Named("dtm") = dtm,
    Named("days_in") = days_in,
    Named("days_through") = days_through,
    Named("ai") = ai
    
  );
}