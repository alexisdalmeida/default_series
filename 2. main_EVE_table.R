path1<- "~/R/IRRBB_Proj/data/"
#path1<- "//CBFS/BsedCommon/Risk_Analytics/04. Data Analytics/6. R Project/Data_analytics_Proj"
file<- "ZZ_FX_rates_bankwise.csv"
path2<- paste( path1, file, sep=""  )
fx_rate <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)

bank_list<- c('ADCB', 'ADIB', 'AJMB', 'CBD', 'CBI', 'CITI', 'DIB', 'FAB', 'ENBD',
              'HSBC', 'Mashreq', 'NBF', 'NBQ', 'RAK', 'UAB' )
bank_count<- 9
#bank_list<- c('ADCB', 'ADIB')

# for PDF
path10<- "~/R/IRRBB_Proj/output/"
file<- "A&L profile.pdf"
path12<- paste( path10, file, sep=""  )
pdf(file = path12)
par(mfrow=c(2,1))

# ========= Loop through bank ===================

for (bank_count in 1:length(bank_list)){
  
  bank_name<- bank_list[bank_count]
  
  # === Loan file ===
  file<- paste(bank_name,'_EVE_data.csv', sep="")
  path2<- paste( path1, file, sep=""  )
  data <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
  data<- replace( data, is.na(data), 0) # replace empty value by 0
  
  total_row<- nrow(data)
  total_col<- ncol(data)
  
  # get fx for the bank
  fx_table<- data.frame( fx_rate$FX, fx_rate[,bank_name]); names(fx_table)<- c('FX', bank_name)

  label_vect<- rep("", 21)
  check_vect<- rep("", 21)
  check_count <- 1
    
  # ============ check 1 ==============
  label_vect[1]<-  'outstaning = sum of CF per line' 
  temp<- ""
  # check all rows
  for (irow in 1:total_row){
    s1<- data$Oustanding_Balance[irow]
    s2<- apply(data[,10:total_col],1, sum)[irow]
    #s2<- sum(data[ irow,10:total_col])
    #check at 100 AED tolerance
    if( s1 != 0){if(  abs(s1 - s2) > 100  ){temp<-c(temp, FALSE)}else{temp<-c(temp, TRUE)}}
  }
  # if there is one FALSE in the vector then report FALSE 
  check_vect[check_count]<- if( sum(grepl(FALSE,temp))>0){FALSE}else{TRUE} 
  
  
 # =========== check 2 ==========
  check_count <- check_count+1
  label_vect[check_count]<- 'Assets = Liabilities + Equity (BS level)'
  s1 = sum( data$Oustanding_Balance[ data$GL_Level_1 == 'Assets' ] )
  s2 = sum( data$Oustanding_Balance[ data$GL_Level_1 == 'Liabilities' ] )
  s3 = sum( data$Oustanding_Balance[ data$GL_Level_1 == 'Equity' ] )
  #check at 2% tolerance
  check_vect[check_count]<- if( abs(s1/(s2+s3) - 1) > 0.02 ){ FALSE} else {TRUE}  


  # =========== check 3 =========
  check_count <- check_count+1
  label_vect[check_count]<- 'Assets = Liabilities + Equity (ccy level)'
  # find the number of ccies
  ccy_vect<- unique(data$Instrument_currency)
  # loop through ccies
 
  for (i in 1:length(ccy_vect)){
    # get fx for the bank
    fx<- fx_table[ fx_table[,1]==ccy_vect[i] , 2 ]
    s1 = sum( data$Oustanding_Balance[ data$GL_Level_1 == 'Assets' & data$Instrument_currency == ccy_vect[i]  & data$Scenario=='Base'] ) * fx
    s2 = sum( data$Oustanding_Balance[ data$GL_Level_1 == 'Liabilities'& data$Instrument_currency == ccy_vect[i]  & data$Scenario=='Base' ]) * fx
    s3 = sum( data$Oustanding_Balance[ data$GL_Level_1 == 'Equity' & data$Instrument_currency == ccy_vect[i] & data$Scenario=='Base' ] ) * fx
    s4 = sum( data$Oustanding_Balance[ data$GL_Level_1 == 'Off_BS' & data$Instrument_currency == ccy_vect[i] & data$Scenario=='Base'  ] ) * fx
    if( i ==1){ ss1<- s1  }else{ss1<- ss1+s1}
    if( i ==1){ ss2<- s2  }else{ss2<- ss2+s2}
    if( i ==1){ ss3<- s3  }else{ss3<- ss3+s3}
    if( i ==1){ ss4<- s4  }else{ss4<- ss4+s4}
  } 
  check_vect[check_count]<- if( abs( (ss1-ss2-ss3+ss4)/ss1 ) > .02  ){ FALSE}else{ TRUE}
  Total_assets<- ss1
  
  
  # =========== check 4 ======== 
  check_count <- check_count+1
  label_vect[check_count]<- 'sign of liabilities should be positive, per line'
  # find the number of ccies
  ccy_vect<- unique(data$Instrument_currency)
  temp<- ""
  for (irow in 1:total_row){
    if( data$GL_Level_1[irow] == 'Liabilities'){ 
      s1<- data$Oustanding_Balance[irow]
      if(s1<0 ){temp<-c(temp, FALSE)}else{temp<-c(temp, TRUE)}
    }
  }
  
  # if there is one FALSE in the vector then report FALSE 
  check_vect[check_count]<- if( sum(grepl(FALSE,temp))>0){FALSE}else{TRUE} 
  
  
  # =========== check 5 ========
  check_count <- check_count+1
  label_vect[check_count]<-  'cash with central banks should be O/N to 1Y' 
  # create subset
  data_sub<- data[ data$GL_Level_2 == 'Cash_and_balances_with_central_banks' , ]
  s1<- sum(data_sub[ , 16:( ncol(data)-1) ])
  check_vect[check_count]<- if( s1>0){FALSE}else{TRUE} 
  
  # =========== check 6 ========
  check_count <- check_count+1
  label_vect[check_count]<-  'cash and FVTPL should be non rate sensitive' 
  # create subset
  data_sub<- data[ data$GL_Level_3 == 'CASH AND CASH ITEMS' |
                   data$GL_Level_3 == 'POSITIVE FAIR VALUE OF FX CASH' |  
                   data$GL_Level_3 == 'NEGATIVE FAIR VALUE OF FX CASH' |
                   data$GL_Level_3 == 'INVESTEMENTS - FVTPL' , ] 
  s1<- sum(data_sub[ , 10:( ncol(data)-1) ]) # sum of all cf
  check_vect[check_count]<- if( s1>0){FALSE}else{TRUE} 

  
  # =========== check 7  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'due to/from banks should be rate sensitive and t<2y' 
  # create subset
  data_sub<- data[ data$GL_Level_2 == 'Due_from_banks_and_financial_institutions' |
                   data$GL_Level_2 == 'Due_to_Banks' , ] 
  #s1<- sum(data_sub$Non_rate_sensitive) # sum of non rate sensitive
  s2<- sum(data_sub[ , 18:( ncol(data)-1) ]) # sum of cf beyon 2 y
    
  # check if the CF are zero
  check_vect[check_count]<- if( s1>0){FALSE}else{TRUE} 

  
  # =========== check 8  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'Derivatives should be non sensitive' 
  # create subset
  data_sub<- data[ data$GL_Level_2 == 'Derivatives_Financial_Assets' |
                   data$GL_Level_2 == 'Derivatives_Financial_Liabilities' , ] 
  s1<- sum( data_sub[ , 10:( ncol(data)-1) ]) # sum of all cf
  
  # check if the CF are zero
  check_vect[check_count]<- if( s1>0){FALSE}else{TRUE} 
  
  
  # =========== check 9  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'If GL name says fixed rate, then the rate type should be fixed' 
  # loop through rows
  temp<- ""
  for (irow in 1:total_row){
    if( grepl('FIXED RATE' , data$GL_Level_3[irow]) == TRUE  ){ # if fixed rate 
        temp<- if( data$Rate.type[irow] != 'Fixed' |   data$Rate.type[irow] != 'Admin'  ){c(temp, TRUE)}else{temp<-c(temp, FALSE)}
    }
  }
  # if there is one FALSE in the vector then report FALSE 
  check_vect[check_count]<- if( sum(grepl(FALSE,temp))>0){FALSE}else{TRUE}
  
  
  # =========== check 10  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'If GL name says floating rate, then the rate type should be floating' 
  # loop through rows
  temp<- ""
  for (irow in 1:total_row){
    if( grepl('FLOATING' , data$GL_Level_3[irow]) == TRUE  ){ # if fixed rate 
      temp<- if( data$Rate.type[irow] != 'Floating'){c(temp, FALSE)}else{temp<-c(temp, TRUE)}
    }
  }
  # if there is one FALSE in the vector then report FALSE 
  check_vect[check_count]<- if( sum(grepl(FALSE,temp))>0){FALSE}else{TRUE} 

  
  # =========== check 11  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'If GL name says floating rate, then CF should be <=1y' 
  # loop through rows
  temp<- ""
  for (irow in 1:total_row){
    if( grepl('FLOATING' , data$GL_Level_3[irow]) == TRUE  ){ # if fixed rate 
      s <- sum( data[irow,  17:( ncol(data)-1)]) # sum of all cf beyond 1 Y
      temp<- if( s>0){c(temp, FALSE)}else{temp<-c(temp, TRUE)}
    }
  }
  # if there is one FALSE in the vector then report FALSE 
  check_vect[check_count]<- if( sum(grepl(FALSE,temp))>0){FALSE}else{TRUE} 
  
  # =========== check 12  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'Securities should all be rate sensitive' 
  # create subset
  data_sub<- data[ data$GL_Level_3 == 'AMORTISED COST DEBT SECURITIES - FIXED RATE' |
                   data$GL_Level_3 == 'FVOCI DEBT SECURITIES - FIXED RATE' , ] 
  s1<- sum( data_sub$Non_rate_sensitive) # sum of all cf
  # check if the CF are zero
  check_vect[check_count]<- if( s1>0){FALSE}else{TRUE} 
  
  
  # =========== check 13  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'Credit cards exposure should < 3y' 
  # create subset
  data_sub<- data[ data$GL_Level_3 == 'CREDIT CARD RECEIVABLES' , ]
  s1<- sum( data_sub[ , 19:( ncol(data)-1) ]) # sum of all cf from 3y onward
  # check if the CF are zero
  check_vect[check_count]<- if( s1>100){FALSE}else{TRUE} 
  
  
  # =========== check 14  ========
  check_count <- check_count+1
  label_vect[check_count]<-  '9Y to 20+ year sum of Asset CF should not be >10% of outstanding' 
  # create subset
  data_sub<- data[ data$GL_Level_1 == 'Assets' & data$Scenario == 'Base', ]
  
  s1<- sum( data_sub[ , 25:( ncol(data)-1) ]) # sum of all cf from 9y onward
  s2<- sum(data_sub$Oustanding_Balance)
  # check if the CF are zero
  check_vect[check_count]<- if( abs(s1/s2) > 0.1){FALSE}else{TRUE} 
  
  #=== BAR PLOT ==========
    d<- apply(data_sub[ , 10:( ncol(data_sub)-1) ], 2, sum   ) 
    c<- as.numeric( 1-cumsum(d)/sum(d))
    title<- paste( bank_name,': % Assets > tenor T', sep=" " )
    x<- barplot(c, names.arg = names(d), cex.names=.5, main=title)
    text(x=x, y= c+.03, label=as.character(round(c*100)))

    
  # =========== check 15  ========
  check_count <- check_count+1
  label_vect[check_count]<-  '9Y to 20+ year sum of Liabs CF should not be >10% of outstanding' 
  # create subset
  data_sub<- data[ data$GL_Level_1 == 'Liabilities' & data$Scenario == 'Base', ]
  
  s1<- sum( data_sub[ , 25:( ncol(data)-1) ]) # sum of all cf from 9y onward
  s2<- sum(data_sub$Oustanding_Balance)
  # check if the CF are zero
  check_vect[check_count]<- if( abs(s1/s2) > 0.1){FALSE}else{TRUE} 
  
  #=== BAR PLOT ==========
  d<- apply(data_sub[ , 10:( ncol(data_sub)-1) ], 2, sum   ) 
  c<- as.numeric( 1-cumsum(d)/sum(d))
  title<- paste( bank_name,': % Liabilities > tenor T', sep=" " )
  x<- barplot(c, names.arg = names(d), cex.names=.5, main=title)
  text(x=x, y= c+.03, label=as.character(round(c*100)))
  
  
  # =========== check 16  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'CASA not greater than 5y' 
  # create subset
  data_sub<- data[ data$GL_Level_3 == 'RETAIL CURRENT ACCOUNTS' |
                   data$GL_Level_3 == 'RETAIL SAVINGS' |
                   data$GL_Level_3 == 'RETAIL ISLAMIC SAVINGS' |
                   data$GL_Level_3 == 'RETAIL ISLAMIC CURRENT ACCOUNTS' , ]
  
  s1<- sum( data_sub[ , 21:( ncol(data)-1) ]) # sum of all cf from 5y onward
  # check if the CF are zero
  check_vect[check_count]<- if( s1>100){FALSE}else{TRUE}  # 100 AED margin or error
  
  
  # =========== check 17  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'NPL should be positive' 
  # create subset
  data_sub<- data[ data$GL_Level_2 == 'Loans_and_Advances_Non_Performing' , ]
  
  # loop through rows
  temp<- ""
  for (irow in 1:nrow(data_sub)){
    s<- sum( data_sub[ irow, 11:( ncol(data)-1) ]) # sum of all cf 
    temp<- if( s< -100){c(temp, FALSE)}else{temp<-c(temp, TRUE)}
  }
  
  # if there is one FALSE in the vector then report FALSE 
  check_vect[check_count]<- if( sum(grepl(FALSE,temp))>0){FALSE}else{TRUE} 

  
  
  # =========== check 18  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'NPL should be split in 3 buckets' 
  # create subset
  data_sub<- data[ data$GL_Level_2 == 'Loans_and_Advances_Non_Performing' &
                   data$Scenario == 'Base' , ]
  
  s1<- sapply( data_sub[ , 11:( ncol(data)-1)], 2, FUN = sum) # sum of all cf 
  # check the number of col with value > 100
  n<- sum(s1 > 100, na.rm=TRUE)
  check_vect[check_count]<- if( n<3){FALSE}else{TRUE} 
  
  
  # =========== check 19  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'CF diff across scenarios base, up, down' 
  # create subset
  data_sub_base<- data[ data$GL_Level_2 == 'Loans_and_Advances_Performing' & data$Scenario == 'Base' , ]
  s1<- apply( data_sub[ , 11:( ncol(data)-1)], 2, FUN = sum)
  
  data_up<- data[ data$GL_Level_2 == 'Loans_and_Advances_Performing' & data$Scenario == 'Parallel_up' , ]
  s2<- apply( data_sub[ , 11:( ncol(data)-1)], 2, FUN = sum)
  
  data_down<- data[ data$GL_Level_2 == 'Loans_and_Advances_Performing' & data$Scenario == 'Parallel_down' , ]
  s3<- apply( data_sub[ , 11:( ncol(data)-1)], 2, FUN = sum)
    
  # check the number of col with value > 100
  d1<- sum(s2-s1) ; d2<- sum(s3-s1)   # diff between scenarios
  check_vect[check_count]<- if( d1< 100 |  d2< 100 ){FALSE}else{TRUE}  # 100 AED margin
  
  
  # =========== check 20  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'equity should be rate non sensitive' 
  # create subset
  data_sub<- data[ data$GL_Level_1 == 'Equity' , ] 
  #s1<- sum(data_sub$Non_rate_sensitive) # sum of non rate sensitive
  s1<- sum(data_sub$Oustanding_Balance)
  s2<- sum(data_sub$Non_rate_sensitive)
  
  # compare outstanding and non-sensitive
  check_vect[check_count]<- if( abs(s2-s2) > 100 ){FALSE}else{TRUE} 

  
  # =========== check 21  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'Derivatives should be rate  non-sensitive' 
  # create subset
  data_sub<- data[ data$GL_Level_2 == 'Derivatives_Financial_Assets' |
                   data$GL_Level_2 == 'Derivatives_Financial_Liabilities' , ] 
  #s1<- sum(data_sub$Non_rate_sensitive) # sum of non rate sensitive
  s1<- sum(data_sub$Oustanding_Balance)
  s2<- sum(data_sub$Non_rate_sensitive)
  
  # compare outstanding and non-sensitive
  check_vect[check_count]<- if( abs(s2-s1) > 100 ){FALSE}else{TRUE} 
  
   
  # =========== check 22  ========
  check_count <- check_count+1
  label_vect[check_count]<-  'Hedges: no less than 80% match' 
  # create subset
  if( sum( data$GL_Level_3 == 'DERIVATIVES - RECEIVING LEG (BS HEDGE)') !=0 ) {
    #data_sub_receiving<- data[ data$GL_Level_3 == 'DERIVATIVES - RECEIVING LEG (BS HEDGE)' & data$Scenario =='Base',]
    #data_sub_paying<- data[data$GL_Level_3 == 'DERIVATIVES - PAYING LEG (BS HEDGE)'  & data$Scenario =='Base' ,]
    
    data_sub<- data[data$GL_Level_1 == 'Off_BS'  & data$Scenario =='Base' ,]
    
    # Sum the hedges converted in AED
    for (i in 1:length(ccy_vect)){
      # get fx for the bank
      fx<- fx_table[ fx_table[,1]==ccy_vect[i] , 2 ]
      s1 = sum( data_sub$Oustanding_Balance[ data_sub$Instrument_currency == ccy_vect[i]   ]  ) * fx
      if( i ==1){ ss1<- s1  }else{ss1<- ss1+s1}
    } 
    #s1<- sum(data_sub_receiving$Oustanding_Balance)
    #s2<- sum(data_sub_paying$Oustanding_Balance)
    #m<- max(c(s1,s2)) ; n<- min(s1, s2)
    
    check_vect[check_count]<-if( abs(ss1)/Total_assets > 0.02){FALSE}else{TRUE} 
    
  }else{check_vect[check_count]<- 'No hedges'}
    
    
  # creation of a table
  res_table<- data.frame( label_vect, check_vect)
  
  if( bank_count == 1) {res_out<- res_table}else{res_out<- cbind(res_out,check_vect)}
}

dev.off() 

# Add names
names(res_out)<- c('Check', bank_list)
# Export to Excel
path10<- "~/R/IRRBB_Proj/output/"
file<- "Data_checks_all_banks.csv"
path11<- paste( path3, file, sep=""  )
write.csv(res_out, path11)


#file<- "test.pdf"
#path11<- paste( path3, file, sep=""  )
#pdf(file = path11)
  #plot(data$X3M_to_6M)
#dev.off()


# =====================  Extract raw data for all banks ===============

for (bank_count in 1:length(bank_list)){
  
  bank_name<- bank_list[bank_count]
  
  # === Loan file ===
  file<- paste(bank_name,'_EVE_data.csv', sep="")
  path2<- paste( path1, file, sep=""  )
  data <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
  data<- replace( data, is.na(data), 0) # replace empty value by 0
  
  total_row<- nrow(data)
  total_col<- ncol(data)
  
  # get fx for the bank
  fx_table<- data.frame( fx_rate$FX, fx_rate[,bank_name]); names(fx_table)<- c('FX', bank_name)
  
 
  data_numeric<- data[,9:ncol(data)]   # = get the numeric part only
  data_text<- data[,1:8]  # = get the text part only
  n<- match( data$Instrument_currency, fx_table[,1]) # get the position of the fx rate in the fx table
  fx_vect<- sapply( n, function(x)  fx_table [ x,2]) # obtain the corresponding fx value
  data_numeric_fx<- apply( data_numeric,2, function(x) x* fx_vect  )
  # check
  #data.frame( data_numeric[ ,1] , data_numeric_fx[ ,1], fx_vect)
  data_bank<- cbind( bank_name, data_text, data_numeric_fx)
  
 
  if( bank_count ==1){all_table<- data_bank}else{ all_table<- rbind( all_table,data_bank )}
  
}


# Export to Excel
path11<- "~/R/IRRBB_Proj/output/"
file<- "Row_Data_all_banks.csv"
path12<- paste( path11, file, sep=""  )
write.csv(all_table, path12)

