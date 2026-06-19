


# ********************************************

# ======= Create a data frame with the necessary dimension



get_cf<- function(EVE_data){
    cf<- cbind( EVE_data[ ,'Scenario'],  
                EVE_data[ ,'Instrument_currency'], 
                EVE_data[ ,'GL_Level_1'],
                EVE_data[ , 10:ncol(EVE_data)] )
    cf<- replace( cf, is.na(cf), 0) # replace empty value by 0
    names(cf)[1:3]<- c('scen', 'ccy', 'gl1' )
    return(cf)
}

    
# *****************************************************************************
# ========= Loop through the data and compute DCF ===========================
       
#get_dcf_profile( basel_tenor, cf, shock_table )

get_dcf_profile<- function( basel_tenor, cf, shock_table){      
    
    # create vectors with the required dimensions
    ccy_vect<- unique( cf[,'ccy'])
    scen_vect<- unique( cf[,'scen'])
    gl_vect<- unique( cf[,'gl1'])
  
    # ==== create a recipient
    basel_tenor_shocked<- basel_tenor
    # Create a big table with loop through dimensions 
    i<- 0
    for (scen_count in 1:length(scen_vect)){   # =========== Scenario loop
      scen<- scen_vect[scen_count] # get scenario
      
      # ******* Add shock
      s<- shock_table[scen][[1]] # vectorise
      basel_tenor_shocked[ , 5:ncol( basel_tenor) ] <- apply( basel_tenor[,5:ncol( basel_tenor) ],2, function(x) x+s )
      
  
      for (ccy_count in 1:length(ccy_vect)){   # =========== ccy loop 
        ccy<- ccy_vect[ccy_count]  # get ccy. No conversion
        
        for (gl_count in 1:length(gl_vect)){   # =========== GL loop  
          gl<- gl_vect[gl_count]  # get gl
          
          # CALL FUNCTION get the profile given all the inputs scen, ccy and gl
          cf_profile<- get_disc_cf(cf=cf, basel_tenor=basel_tenor_shocked, scen=scen, ccy=ccy, gl=gl  )
          
          #== Creation of a large result table
          i<- i+1 # counter within the gl loop
          if(i==1){ # at the first loop, the table is created
            result_out<- cf_profile
          }else{ 
            result_out<- rbind( result_out, cf_profile) # then added
          }
          
        } # === GL loop
        
      }# ====  Ccy loop
      
    } # === scen loop
    
    return(result_out)
} 



# *****************************************************************************
# ========= Function that computes DCF : the output is two vectors with associated dimensions label ===========================
#scen<- 'Base' ; ccy<- 'AED' ; gl<-'Assets'  

get_disc_cf<- function( cf, basel_tenor, scen, ccy, gl ){
  
  cf_select<- cf[ cf[,'scen'] == scen 
                & cf[,'ccy' ] == ccy 
                & cf[,'gl1' ] == gl, ]
  
  # ccies available in the basel table
  ccy_list<- names(basel_tenor)[5:ncol(basel_tenor) ]
  if( ccy %in% ccy_list ){ ccy_for_disc= ccy }else{ccy_for_disc = 'AED' }
  
  # get the parameters for the computation
  disc_rate<- basel_tenor[  , ccy_for_disc]
  disc_time<- basel_tenor[  , 'Year']
  disc_factor<- exp( - disc_rate/100 * disc_time)
  
  # Compute CF and discount
  N<-  cf_select[ , 4: ( ncol(cf_select)-1) ] # extract just the CF without non interest rate sensitive 
  original_cf<- apply(N, 2, sum) # sum the rows
  names(original_cf)<- basel_tenor[ ,'Basel_bucket' ] # update the name
  disc_cf <- original_cf  * disc_factor # compute DCF
  table_out<- as.data.frame( rbind(original_cf, disc_cf) )# collate
  
  # Sum CF
  Sum_CF<- c( sum(table_out[1,])  ,  sum(table_out[2,]) )
  # Labels
  Scenario<- c(scen, scen) ; Currency<- c(ccy, ccy) ; GL1<- c(gl, gl)
  CF_type <-  c('Original', 'Discounted' )
  #Combine
  table_out<- cbind( Scenario, Currency,  GL1 , CF_type, Sum_CF, table_out)
  
  return(table_out)
}
# =========================

