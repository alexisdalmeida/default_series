path1<- "~/R/IRRBB_Proj/data/"
#path1<- "//CBFS/BsedCommon/Risk_Analytics/04. Data Analytics/6. R Project/Data_analytics_Proj"


bank_list<- c('ADCB', 'ADIB', 'AJMB', 'CBD', 'CBI', 'CITI', 'DIB', 'FAB', 'ENBD',
              'HSBC', 'Mashreq', 'NBF', 'NBQ', 'RAK', 'UAB' )
bank_count<- 9
#bank_list<- c('ADCB',


# ==== load Full tenors
file<- "ZZ_Full_discount_tenors.csv"
path2<- paste( path1, file, sep=""  )
full_tenor_original <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)
# ==== load Basel buckets
file<- "ZZ_Basel_discount_tenors.csv"
path2<- paste( path1, file, sep=""  )
basel_tenor_original <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)


# ====== Get discount cureves for all banks

for (bank_count in 1:length(bank_list)){
  
  bank_name<- bank_list[bank_count]
  basel_tenor<-basel_tenor_original
  full_tenor<-full_tenor_original
  
  # == Load files that are specific to each bank ===========
  
  # ==== load Curve
  file<- paste(bank_name,'_discount_curve.csv', sep="")
  path2<- paste( path1, file, sep=""  )
  disc <- read.csv(path2, stringsAsFactors=FALSE, check.names = TRUE)# 
  
  # get the discount curves
  source("code/discount_curve_function.R")
  disc_curves<- get_disc_curve( basel_tenor=basel_tenor, full_tenor=full_tenor, disc=disc)
  basel_tenor_with_curves<- disc_curves[[1]]
  
  # create a table for one bank with bank name in as header
  temp<- basel_tenor_with_curves[ ,c(-3,-4) ]
  n<- names(temp)
  n[3:length(n) ]<- paste( bank_name , n[3:length(n) ] , sep="_" )
  names(temp)<- n
  
  if( bank_count==1){
    tab_out<- temp
  }else{
    tab_out<- cbind( tab_out, temp[ ,c(-1,-2) ]  )
  }
  
}


path3<- "~/R/IRRBB_Proj/output/"
file<- "Basel_tenor_disc_all_banks.csv"
path4<- paste( path3, file, sep=""  )
write.csv(tab_out, path4)


