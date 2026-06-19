

#library(quantmod)
#library(car)
#library(xts)

#library(factoextra)
#library(devtools)
#library(ggbiplot)

#library(dplyr)
#library(reshape)
#library(data.table)

path1<- "~/R/IRRBB_Proj/data/"
#path1<- "//CBFS/BsedCommon/Risk_Analytics/04. Data Analytics/6. R Project/Data_analytics_Proj"

# == Load files that are generic across banks ===========

# ==== load Full tenors
file<- "ZZ_Full_discount_tenors.csv"
path2<- paste( path1, file, sep=""  )
full_tenor_original <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
# ==== load Basel buckets
file<- "ZZ_Basel_discount_tenors.csv"
path2<- paste( path1, file, sep=""  )
basel_tenor_original <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
# ==== load FX rates
file<- "ZZ_FX_rates_bankwise.csv"
path2<- paste( path1, file, sep=""  )
fx_rate <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
# ==== load shocks
file<- "ZZ_IR_shocks.csv"
path6<- paste( path1, file, sep=""  )
ir_shocks <- read.csv(path6, stringsAsFactors=FALSE, check.names = TRUE)

bank_list<- c('ADCB', 'ADIB', 'AJMB', 'CBD', 'CBI', 'CITI', 'DIB', 'FAB', 'ENBD',
              'HSBC', 'Mashreq', 'NBF', 'NBQ', 'RAK', 'UAB' )
bank_count<- 3


# ========= Loop through bank ===================

for (bank_count in 1:length(bank_list)){
  
  bank_name<- bank_list[bank_count]
  basel_tenor<-basel_tenor_original
  full_tenor<-full_tenor_original
  
  # == Load files that are specific to each bank ===========
  # ==== load EVE data
  #file<- "DIB_EVE_data.csv"
  file<- paste(bank_name,'_EVE_data.csv', sep="")
  path2<- paste( path1, file, sep=""  )
  EVE_data <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
  # ==== load Curve
  #file<- "DIB_discount_curve.csv"
  file<- paste(bank_name,'_discount_curve.csv', sep="")
  path2<- paste( path1, file, sep=""  )
  disc <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)# ==== load EVE data
  
  # get the discount curves
  source("code/discount_curve_function.R")
  disc_curves<- get_disc_curve( basel_tenor=basel_tenor, full_tenor=full_tenor, disc=disc)
  basel_tenor_with_curves<- disc_curves[[1]]
  full_tenor_with_curves<- disc_curves[[2]]
  
  #==  visual check
  ccy<- 'AED'
  title = paste( bank_name, ccy, sep=" ")
  plot(basel_tenor_with_curves[, 'Year'], basel_tenor_with_curves[, ccy], type='l', col='red', main = title)
  lines(  x= full_tenor[,'Year' ]  , y=full_tenor_with_curves[, ccy], col='black')
  ccy<- 'USD'
  title = paste( bank_name, ccy, sep=" ")
  plot(basel_tenor_with_curves[, 'Year'], basel_tenor_with_curves[, ccy], type='l', col='red', main = title)
  lines(  x= full_tenor[,'Year' ]  , y=full_tenor_with_curves[, ccy],col='black')

  # ======== Extract the necessary columns from the CF file
  source("code/DCF_profile_function.R")
  cf<- get_cf(EVE_data)
  
  # define shocks
  scen<- c("Base","Parallel_up","Parallel_down")
  shock_table<- ir_shocks[1:4] #we keep only the shocks for the three scenarios base/up/down
  
  # get fx for the bank
  fx_table<- data.frame( fx_rate$FX, fx_rate[,bank_name]); names(fx_table)<- c('FX', bank_name)
  
  # get DCF profiles
  dcf_profile<- get_dcf_profile( basel_tenor = basel_tenor_with_curves, cf, shock_table)

  # get EVE 
  source("code/compute_EVE_function.R")
  EVE_table<- compute_EVE(dcf_profile, fx_table )
  
  #Combined profiles and EVE results
  EVE_and_dcf_table<- rbind(dcf_profile, EVE_table)
  
  # Add the bank name
  Bank<- as.data.frame(rep(bank_name, nrow(EVE_and_dcf_table))); names(Bank)<- 'Bank'
  EVE_and_dcf_table<- cbind(Bank, EVE_and_dcf_table)
  rownames(EVE_and_dcf_table)<- NULL
  
  # Combined banks
  if(bank_count==1){
      final_table<- EVE_and_dcf_table
  }else{
      final_table<- rbind(final_table, EVE_and_dcf_table)
  }
  
  #barplot(as.numeric(EVE_table[26,6:ncol(EVE_table)]))
  
}

#write.table(dcf_profile, "clipboard", sep="\t")
#write.table(final_table, "clipboard", sep="\t")
#rownames(EVE_out)<- NULL
write.table(EVE_out, "clipboard", sep="\t")

# Export to Excel
path3<- "~/R/IRRBB_Proj/output/"
file<- "EVE_Result_table_all_banks.csv"
path4<- paste( path3, file, sep=""  )
write.csv(final_table, path4)

