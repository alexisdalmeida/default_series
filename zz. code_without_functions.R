


order_bank<- function( df, target_date){
  df_t<- as.data.frame(t(df))
  date_vect<- index(df)
  col<-  which(  date_vect == target_date) 
  df_t_rank<- df_t[  order(-df_t[,col]) , ]
  df_final<- t(df_t_rank)
  df_final<- as.xts( df_final, order.by = date_vect  )
  return( df_final)
}


