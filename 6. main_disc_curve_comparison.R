

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
file<- "ZZ_Basel_income_tenors.csv"
path2<- paste( path1, file, sep=""  )
basel_tenor_original <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
# ==== load FX rates
file<- "ZZ_FX_rates_bankwise.csv"
path2<- paste( path1, file, sep=""  )
fx_rate <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
# ==== load shocks
# The ir shocks are defined in a grid.
# They will all be used in conjunction with the 'Base' CF scenario
file<- "ZZ_IR_shocks_for_EAR.csv"
path6<- paste( path1, file, sep=""  )
ir_shocks <- read.csv(path6, stringsAsFactors=FALSE, check.names = TRUE)


bank_list<- c('ADCB', 'ADIB', 'AJMB', 'CBD', 'CBI', 'CITI', 'DIB', 'FAB', 
              'HSBC', 'Mashreq', 'NBF', 'NBQ', 'RAK', 'UAB' )
bank_count<- 1


# *****************************************
# ====== EAR profile =====

# === loop through bank

for (bank_count in 1:length(bank_list)){
  
  bank_name<- bank_list[bank_count]
  basel_tenor<-basel_tenor_original
  full_tenor<-full_tenor_original
  
  # == Load files that are specific to each bank ===========
  # ==== load EAR data

  file<- paste(bank_name,'_EAR_data.csv', sep="")
  path2<- paste( path1, file, sep=""  )
  EAR_data <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
  # ==== load Curve
  file<- paste(bank_name,'_discount_curve.csv', sep="")
  path2<- paste( path1, file, sep=""  )
  disc <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
  
  # get the discount curves
  source("code/discount_curve_function.R")
  disc_curves<- get_disc_curve( basel_tenor=basel_tenor, full_tenor=full_tenor, disc=disc)
  basel_tenor_with_curves<- disc_curves[[1]]
  full_tenor_with_curves<- disc_curves[[2]]
  
  # Extract the necessary columns from the data
  source("code/income_profile_function.R")
  cf<- get_cf(EAR_data)
  cf<- cf[ cf$scen=='Base' , ] # keep only the base cash flows
  
  # =========== Get Base EAR ============
  
  shock_name<- paste('s', 21, sep="")  # zero shock
  shock_table<- data.frame( ir_shocks$Basel_bucket, ir_shocks[shock_name ] )
  names(shock_table)[2]<- 'Base'   # we use the base cf scenario to input shocks
  
  # get Inc profiles
  income_profile<- get_income_profile( basel_tenor = basel_tenor_with_curves, cf, shock_table)
  
  # get fx for the bank
  fx_table<- data.frame( fx_rate$FX, fx_rate[,bank_name]); names(fx_table)<- c('FX', bank_name)
  
  # === get starting unstressed EAR
  source("code/compute_EAR_function.R")
  EAR_table<- compute_EAR( result_out = income_profile, fx_table )
  index_scen<- EAR_table$Scenario == 'Base'
  index_ccy<- EAR_table$Currency =='Aggregated'
  index_cf<- EAR_table$CF_type =='Repriced'
  index_gl<- EAR_table$GL1 =='EAR_bank_level'
  EAR_0<- EAR_table[index_scen & index_ccy & index_gl & index_cf, ]$Sum_CF[1]
  
  # ==== loop through shocks and recompute EVE each time
  
  for (j in 1: (   ncol(ir_shocks)-4 ) ){ # the shocks start at col 4
    
    shock_name<- paste('s', j, sep="") 
    shock_table<- data.frame( ir_shocks$Basel_bucket, ir_shocks[shock_name ] )
    names(shock_table)[2]<- 'Base'   # we use the base cf scenario to input shocks
    
    # get Inc profiles
    income_profile<- get_income_profile( basel_tenor = basel_tenor_with_curves, cf, shock_table)
    
    # get stressed EVE 
    EAR_table<- compute_EAR(income_profile, fx_table )
    index_scen<- EAR_table$Scenario == 'Base'
    index_ccy<- EAR_table$Currency =='Aggregated'
    index_cf<- EAR_table$CF_type =='Repriced'
    index_gl<- EAR_table$GL1 =='EAR_bank_level'
    EAR_stress<- EAR_table[index_scen & index_ccy & index_gl & index_cf, ]$Sum_CF[1]
    
    dEAR_abs<- EAR_stress - EAR_0
    dEAR_rel<- dEAR_abs / abs(EAR_0)
    
    # build a table
    if( j==1){ EAR_level<- EAR_stress }else{ EAR_level<-  c( EAR_level, EAR_stress) }
    if( j==1){ dEAR_abs_table<- dEAR_abs }else{ dEAR_abs_table<-  c( dEAR_abs_table, dEAR_abs) }
    if( j==1){ dEAR_rel_table<- dEAR_rel }else{ dEAR_rel_table<-  c( dEAR_rel_table, dEAR_rel) }
    
    
  }
  
  shock_vect<- as.numeric( ir_shocks[1,5:ncol(ir_shocks)] )
  plot( shock_vect, dEAR_abs_table/1000000  , type = 'l')
  plot( shock_vect, dEAR_rel_table           , type = 'l')
  
  # combine EVE and dEVE in a single table
  combined<- data.frame( shock_vect, EAR_level, dEAR_abs_table, dEAR_rel_table  )
  # add labels
  h1<- paste(bank_name,'EAR', sep="_" )
  h2<- paste(bank_name,'dEAR_abs', sep="_" )
  h3<- paste(bank_name,'dEAR_rel', sep="_" )
  names(combined)<- c('Shock', h1,h2,h3 )
  
  #combine across banks
  if( bank_count==1){ res<- combined }else{ res<-  cbind( res, combined[,2:4]) }
}

# === Plotting on the chart
# create a small table with only relative values 
pos<- grepl( 'rel', names(res)) 
index<- which(pos %in% TRUE)
dEVE<- data.frame( res[,index]  )
max_val<- max(dEVE) ; min_val<- min(dEVE)

plot(shock_vect/100, dEVE[,1], type='l', col=1,
     ylim = c(min_val,max_val), main = "Relative EAR change per bank", 
     xlab = "% IR shock", ylab = "EAR % change")

lines(shock_vect/100,dEVE[,2], type='l', col= 'green')
lines(shock_vect/100,dEVE[,3], type='l', col= 'red')
lines(shock_vect/100,dEVE[,4], type='l', col='blue')

legend(x = "topright",          # Position
       legend = bank_list,  # Legend texts
       lty = 1,           # Line types
       col = c(1,'green','red', 'blue')) 

#write.table(dcf_profile, "clipboard", sep="\t")
#write.table(final_table, "clipboard", sep="\t")
#rownames(EAR_out)<- NULL
#write.table(res, "clipboard", sep="\t")

# Export to Excel
path7<- "~/R/IRRBB_Proj/output/"
file<- "EAR_IR_profile_all_banks.csv"
path8<- paste( path3, file, sep=""  )
write.csv(res, path8)

