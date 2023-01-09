# Patriots center turn findings

team_to_analyze <- 'NE'

#create data frame for important variables in Patriots center turn model
centerturnimp_rf <- varImp(get(paste0('centerturnmodels_',team_to_analyze)))

centerturn_rf <- data.frame(centerturnimp_rf$importance[1])
centerturn_rf <- centerturn_rf %>% mutate(Feature=rownames(centerturn_rf))
centerturn_rf <- remove_rownames(centerturn_rf)
centerturnimportance <- centerturn_rf %>% select(Feature,everything())


#since the features were one hot encoded, need to identify the column value for the factors, and separate the value from the column name
datacolumns <- colnames(odata)

datacolumn_name <- vector(length=nrow(centerturnimportance))
datacolumn_value <- vector(length=nrow(centerturnimportance))

for (i in 1:nrow(centerturnimportance)){
  
  for(c in 1:length(colnames(odata))){
    
    check <- str_detect(centerturnimportance[i,1],datacolumns[c])
    if(check==TRUE){
      datacolumn_name[i] <- str_extract(centerturnimportance[i,1],datacolumns[c])
      datacolumn_value[i] <- str_replace(centerturnimportance[i,1],datacolumn_name[i],'')
    }
    
  }
}

centerturnimportance <- 
  data.frame(cbind(
    Column=datacolumn_name,
    ColumnValue=datacolumn_value,
    centerturnimportance
  ))



centerturnfindings <- list()
centerturnfindings_pct <- list()
centerturnfindings_ID <- list()

cntr <- 0


# using variable importance, create readable findings of each teams model, then store the results
# THIS WILL ONLY SUMMARIZE FEATURES THAT ARE FACTORS, NOT NUMERIC FEATURES  
for (r in 1:nrow(centerturnimportance)){
  total <- nrow(offense_hot %>% filter(timeToSnap==0 & test_weeks==0) %>% filter(get(centerturnimportance$Column[r])==centerturnimportance$ColumnValue[r] & (centerTurn=='right' | centerTurn=='left')))
  
  right <- nrow(offense_hot %>% filter(timeToSnap==0 & test_weeks==0) %>% filter(get(centerturnimportance$Column[r])==centerturnimportance$ColumnValue[r] & centerTurn=='right'))
  
  left <- nrow(offense_hot %>% filter(timeToSnap==0 & test_weeks==0) %>% filter(get(centerturnimportance$Column[r])==centerturnimportance$ColumnValue[r] & centerTurn=='left'))
  
  centerturnupper <- 0.70
  
  if(total >= 3 & right/total >= centerturnupper & centerturnimportance$ColumnValue[r]!='NA' & centerturnimportance$ColumnValue[r]!= 'UNKNOWN'){
    cntr <- cntr+1
    
    centerturnfindings_pct[cntr] <- right/total
    
    centerturnfindings[cntr] <- paste0(team_to_analyze,': ',ifelse(is.na(round(right/total,2)*100)==TRUE,0,round(right/total,2)*100),'% (',right,'/',total,') ','right, when ', centerturnimportance$Column[r], ' = ', centerturnimportance$ColumnValue[r])
    centerturnfindings_ID[[cntr]] <- offense_hot %>% filter(test_weeks==0) %>% filter(get(centerturnimportance$Column[r])==centerturnimportance$ColumnValue[r]) %>% select(id_all_frame) %>% unlist %>% unname
  }
  
  if(total >= 3 & left/total >= centerturnupper & centerturnimportance$ColumnValue[r]!='NA' & centerturnimportance$ColumnValue[r]!= 'UNKNOWN'){
    cntr <- cntr+1
    
    centerturnfindings_pct[cntr] <- left/total
    
    centerturnfindings[cntr] <- paste0(team_to_analyze,': ',ifelse(is.na(round(left/total,2)*100)==TRUE,0,round(left/total,2)*100),'% (',left,'/',total,') ','left, when ', centerturnimportance$Column[r], ' = ', centerturnimportance$ColumnValue[r])
    centerturnfindings_ID[[cntr]] <- offense_hot %>% filter(test_weeks==0) %>% filter(get(centerturnimportance$Column[r])==centerturnimportance$ColumnValue[r]) %>% select(id_all_frame) %>% unlist %>% unname
  }
  
}

if(length(centerturnfindings)>0){
  
  tempfindings <- centerturnfindings
  tempplays <- centerturnfindings_ID
  
  sortorder <- order(unlist(centerturnfindings_pct),decreasing = TRUE)
  cntr <- 0
  for(i in sortorder){
    cntr <- cntr + 1
    centerturnfindings[cntr] <- tempfindings[i]
    centerturnfindings_ID[cntr] <- tempplays[i]
  }
}

# Patriots center turn findings for factor variables
centerturnfindings

# play IDs for center turn findings, if you want to plot a play
centerturnfindings_ID


#examine numerical features separately
varImp(centerturnmodels_NE)

offense_train %>% filter(inWideBox_LminusR<0 & timeToSnap==0) %>% count(centerTurn)

offense_train %>% filter(onLOS_inside_LminusR<0 & timeToSnap==0) %>% count(centerTurn)

offense_train %>% filter(onLOS_outside_LminusR<0 & timeToSnap==0) %>% count(centerTurn)

offense_train %>% filter(inside_2ndLevel_LminusR>0 & timeToSnap==0) %>% count(centerTurn)


