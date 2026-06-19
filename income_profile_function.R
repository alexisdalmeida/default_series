

# *****************************************************************************
# ======= Discount curve: map the Basel tenor onto the full tenor

get_disc_curve<- function( basel_tenor, full_tenor, disc ){

  # Create an empty column, only once
  basel_tenor[,'lower_bound']<- 0
  basel_tenor[,'upper_bound']<- 0
  
  # loop through currencies
  for (ccy_count in 1:nrow(disc)){
    ccy<- disc[ccy_count,1] # get the ccy name
    full_curve<- as.numeric( disc[ccy_count,][-1]) #get the curve
    
    if( sum( full_curve!=0) ) {   
      
      # Add the ccy in the table basel_tenor. Empty col. A new ccy wil added at each loop   
      basel_tenor[ , ccy]<- 0  
      # Add the ccy and curve in the table full_tenor
      full_tenor[,ccy]<- full_curve
      
      # loop through Basel tenors and find the matching tenor in 'full_tenor'
      for (basel_count in 1:nrow(basel_tenor)){    
        # Get the target basel tenor
        target_tenor<- basel_tenor[basel_count,2]
        
        # loop through the full tenors
        for(i in 1:nrow(full_tenor)){
          # Compute the difference in years
          diff_1 =  target_tenor - full_tenor[i,2]
          diff_2 =  target_tenor - full_tenor[i+1,2]
          
          # If the basel tenor is in the middle then allocate the upper and lower bounds
          if( diff_1>=0 & diff_2<0 ){ 
            t1<- full_tenor[i,2]
            t2<- full_tenor[i+1,2]
            r1<- full_tenor[i, ccy]
            r2<- full_tenor[i+1, ccy]
            basel_tenor[basel_count,'lower_bound']<- t1
            basel_tenor[basel_count,'upper_bound']<- t2
            # Interpolate the rate
            disc_rate<- r1 + (r2-r1)/(t2-t1) * (target_tenor - t1)
            basel_tenor[ basel_count, ccy]<- disc_rate
            
          }    # if statement loop
        }   #full tenor loop
      } #basel tenor loop
    }  # if statement loop to check if the ccy is empty
  } #ccy loop
  
  return(list(basel_tenor, full_tenor))

}