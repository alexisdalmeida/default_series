
#result_out<- dcf_profile  # for debugging

compute_EAR<- function(result_out, fx_table ){

    #============= EAR construction loop
    ccy_vect<- unique( result_out[,'Currency'])
    scen_vect<- unique( result_out[,'Scenario'])
    gl_vect<- unique( result_out[,'GL1'])  
  
    i<- 0
    for (scen_count in 1:length(scen_vect)){   # =========== Scenario loop ==============  
      scen<- scen_vect[scen_count] # get scenario
      
      for (ccy_count in 1:length(ccy_vect)){   # =========== ccy loop ============== 
        ccy<- ccy_vect[ccy_count]  # get ccy
        fx<- fx_table[ fx_table[,1]==ccy , 2 ]
        
        index_scen<- result_out$Scenario == scen
        index_ccy<- result_out$Currency == ccy
        index_asset<- result_out$GL1 == 'Assets' ; index_liab<- result_out$GL1 == 'Liabilities' 
        index_eq <- result_out$GL1 == 'Equity' ; index_offbs<- result_out$GL1 == 'Off_BS'
        
       # EAR_ccy_level<- result_out[ index_scen & index_ccy & index_asset, 5:ncol(result_out)  ] -
                       # result_out[ index_scen & index_ccy & index_liab, 5:ncol(result_out)   ] -
                      #  result_out[ index_scen & index_ccy & index_eq,  5:ncol(result_out)    ] +
                       # if( TRUE %in% index_offbs){  result_out[ index_scen & index_ccy & index_offbs, 5:ncol(result_out) ] }else{0}
                      
        assets<- result_out[ index_scen & index_ccy & index_asset, 5:ncol(result_out)  ]
        liabs<-  result_out[ index_scen & index_ccy & index_liab, 5:ncol(result_out)   ]
        eq<-  result_out[ index_scen & index_ccy & index_eq, 5:ncol(result_out)   ]
        offbs<-  result_out[ index_scen & index_ccy & index_offbs, 5:ncol(result_out)   ] 
        
        eq<- if( TRUE %in% index_eq){ eq }else{assets*0}
        offbs<- if( TRUE %in% index_offbs){ offbs }else{assets*0}
        
        EAR_ccy_level<- assets - liabs - eq + offbs
        #if( TRUE %in% index_eq){ eq }else{0} + if( TRUE %in% index_offbs){ offbs }else{0}
      
        # ======= Creation of a big table to APPEND EAR per currencies
        # ==== Add the dimensions
        Scenario<- c(scen, scen) ; Currency<- c(ccy, ccy)      # Get Dimensions
        GL1<- c('EAR_ccy_level', 'EAR_ccy_level') ; CF_type <-  c('Original', 'Repriced' )
        EAR_ccy_level<- cbind( Scenario, Currency,  GL1 , CF_type, EAR_ccy_level )
        
        i<- i+1 
        if(i==1){ # at the first loop, the big table is created
          EAR_out<- EAR_ccy_level
        }else{ 
          EAR_out<- rbind( EAR_out, EAR_ccy_level)  # then added
        }
        
        
        # ======= Creation of a smaller table to SUM EAR across currencies
        if(ccy_count==1){ # at the first loop, the table is created
          EAR_bank_level<-  EAR_ccy_level[ ,5:ncol(result_out)]  * fx
        }else{ 
          EAR_bank_level<- EAR_bank_level +  EAR_ccy_level[ ,5:ncol(result_out)]  * fx   # then added
        } 
        
        
        # ===== creation of a smaller table to SUM Assets, Liabs, Offbs across currencies
        if(ccy_count==1){ # at the first loop, the table is created
          assets_bank_level<- assets  * fx
          liabs_bank_level<- liabs  * fx
          eq_bank_level<-   if( TRUE %in% index_eq){ eq * fx }else{assets_bank_level*0} 
          offbs_bank_level<- if( TRUE %in% index_offbs){ offbs * fx }else{assets_bank_level*0}
         
        }else{ 
          assets_bank_level<- assets_bank_level + assets  * fx  
          liabs_bank_level<-  liabs_bank_level +  liabs  * fx
          eq_bank_level<-    if( TRUE %in% index_eq){ eq_bank_level + eq * fx }else{assets_bank_level*0}
          offbs_bank_level<-if( TRUE %in% index_offbs){ offbs_bank_level + offbs * fx }else{assets_bank_level*0}
        } 
        
      }# ======================  Ccy loop
      
      #=== Combined the EAR across ccies
      #Add the dimensions to the EAR_bank level table
      Currency<- c('Aggregated', 'Aggregated') ; GL1<- c('EAR_bank_level', 'EAR_bank_level')
      EAR_bank_level<- cbind( Scenario, Currency,  GL1 , CF_type, EAR_bank_level )
      #Combine with the result table at currency level
      EAR_out<- rbind( EAR_out, EAR_bank_level) 
      
      #=== Combined the col Assets, etc. across ccies
      #Add the dimensions to the EAR_bank level table
      Currency<- c('Aggregated', 'Aggregated')
      GL1<- c('Assets', 'Assets')
      assets_bank_level<- cbind( Scenario, Currency,  GL1 , CF_type, assets_bank_level )
      GL1<- c('Liabilities', 'Liabilities')
      liabs_bank_level<- cbind( Scenario, Currency,  GL1 , CF_type, liabs_bank_level )
      GL1<- c('Equity', 'Equity')
      eq_bank_level<- cbind( Scenario, Currency,  GL1 , CF_type, eq_bank_level )
      GL1<- c('Off_BS', 'Off_BS')
      offbs_bank_level<- cbind( Scenario, Currency,  GL1 , CF_type, offbs_bank_level )
      
      
      #Combine with the result table at currency level
      EAR_out<- rbind( EAR_out, assets_bank_level, liabs_bank_level, eq_bank_level, offbs_bank_level, EAR_bank_level)
    
      
    } # ============= end of scen loop
    
    
    #======== Compute the change in EAR 
    index_EAR_bank<- EAR_out$GL1 == 'EAR_bank_level'
    index_cf_type<- EAR_out$CF_type == 'Repriced'
    index_base<- EAR_out$Scenario == scen_vect[1]
    index_up<- EAR_out$Scenario == scen_vect[2] 
    index_down<- EAR_out$Scenario == scen_vect[3] 
    
    dEAR_up<- EAR_out[ index_EAR_bank & index_up & index_cf_type , 5:ncol(result_out)    ] -
              EAR_out[ index_EAR_bank & index_base & index_cf_type , 5:ncol(result_out)  ] 
  
    dEAR_down<- EAR_out[ index_EAR_bank & index_down & index_cf_type , 5:ncol(result_out)    ] -
                EAR_out[ index_EAR_bank & index_base & index_cf_type, 5:ncol(result_out)  ]
    
    #Add the dimensions to the dEAR table
    
    Scenario<- c(scen_vect[2]); Currency<- c('Aggregated') ; GL1<- c('EAR_bank_level_change') ; CF_type<- c('Repriced')
    dEAR_up <- cbind( Scenario, Currency,  GL1 , CF_type, dEAR_up )[1,]
    
    Scenario<- c(scen_vect[3]); Currency<- c('Aggregated') ; GL1<- c('EAR_bank_level_change') ; CF_type<- c('Repriced')
    dEAR_down <- cbind( Scenario, Currency,  GL1 , CF_type, dEAR_down )[1,]
    
    # == Add it to the table
    EAR_out<- rbind(EAR_out, dEAR_up, dEAR_down )
       
    return(EAR_out)
    
}
