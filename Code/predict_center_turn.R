# CREATE CENTER TURN MODELS
# (MUST RUN create_features.R FIRST)
# (MUST LOAD PACKAGES FROM create_features.R FIRST)

# only include teams that had 20 or more relevant snaps in weeks 6-8
teams <- plays %>% filter(week>=6) %>% count(possessionTeam) %>% filter(n>=20) %>% select(possessionTeam) %>% unlist %>% unname


#create lists to store findings, and columns to store predictions
maxacc_rf <- list()
maxkap_rf <- list()
varimp_rf <- list()

weeks$centerturn_pred <- NA
weeks$centerturn_pred_testdata <- NA
weeks$centerturn_pred_right <- NA
weeks$centerturn_pred_right_testdata <- NA
weeks$centerturn_pred_left <- NA
weeks$centerturn_pred_left_testdata <- NA


# CREATE MODELS 
for(t in 1:length(teams)){


# only include pre-snap data, the offensive center, and teams that are in the "teams" variable
offense <- weeks %>% filter(frameId<=snap_frame &
                              team==teams[t] & 
                              off_def=='O' & 
                              position=='C')

# test data for Chargers is different, because I wanted to include a specific week 2 play in test data
if(teams[t]=='LAC'){
  offense$test_weeks <- ifelse(offense$gameId %in% (plays %>% filter(possessionTeam==teams[t] & (week >= 6)) %>% select(gameId) %>% unname %>% unlist),1,0)
  offense$test_weeks <- ifelse(offense$id=='2021091911_1143',1,offense$test_weeks)
} else {
  offense$test_weeks <- ifelse(offense$gameId %in% (plays %>% filter(possessionTeam==teams[t] & week >= 6) %>% select(gameId) %>% unname %>% unlist),1,0)
}



#select features for model
offense <- offense %>% 
  select(test_weeks,
         centerTurn,
         onLOS_Agap_left,
         onLOS_Agap_right,
         passStrength,
         TEs_right,
         TEs_left,
         RBs_right,
         RBs_left,
         hash,
         rightBoxOverload,
         leftBoxOverload,
         rightLOSOverload,
         leftLOSOverload,
         inWideBox_LminusR,
         onLOS_inside_LminusR,
         onLOS_outside_LminusR,
         outside_2ndLevel_LminusR,
         inside_2ndLevel_LminusR,
         timeToSnap,
         id_all_frame)

  
#create dummy variables and perform one hot encoding
dummy <- dummyVars(centerTurn~.,data=offense %>% select(-id_all_frame,-test_weeks))
offense_hot <- data.frame(predict(dummy,newdata=offense))
offense_hot <- cbind(id_all_frame=offense$id_all_frame,
                     centerTurn=offense$centerTurn,
                     test_weeks=offense$test_weeks,
                     offense_hot)

#do not coerce these variables to factors
numeric_features <- grep('LminusR',names(offense_hot))
numeric_features2 <- grep('timeToSnap',names(offense_hot))

#coerce all variables to factors, except for the ones above
offense_hot_factors <- data.frame(lapply(offense_hot[,-c(numeric_features,numeric_features2)],as.factor))

#bind the non-factor variables to the factor variables
offense_hot <- cbind(offense_hot_factors,offense_hot[,c(numeric_features,numeric_features2)])

#create train and test sets
offense_train <- offense_hot %>% filter(test_weeks!=1) %>% select(-test_weeks)
offense_test <- offense_hot %>% filter(test_weeks==1) %>% select(-test_weeks)

#store training data in this dataset, so we can keep track of the id_all_frame in the other dataset
#this is the data for model training
odata <- offense_train %>% select(-id_all_frame)


#identify columns with only one unique value
n <- ncol(odata)
remove_col <- vector(length=ncol(odata))
for (i in 1:n){
  if (n_distinct(odata[,i]) == 1){
    remove_col[i] <- i
  }
}

#remove 0's from remove_col
remove_col <- remove_col[remove_col!=0]


#remove columns with only 1 unique value
if(length(remove_col)>0){
  odata <- subset(odata,select=-remove_col)
}


#create train control
#using out of bag, since these are random forest models
centerturnctrl <-  
  trainControl(method='oob',
               savePredictions = 'all')

# set seed for reproducibility
set.seed(111)

#train models
centerturnmodels <- 
  train(centerTurn~.,
        data=odata,
        trControl = centerturnctrl,
        method='rf',
        tuneGrid=data.frame(mtry=2),
        metric='Accuracy'
  )

#store accuracy, kappa, and important variables
maxacc_rf[t] <- max(centerturnmodels$results$Accuracy)
maxkap_rf[t] <- max(centerturnmodels$results$Kappa)
varimp_rf[t] <- paste(rownames(varImp(centerturnmodels)$importance %>% arrange(-Overall))[1:5],collapse=', ')


#create object for each model
assign(paste0('centerturnmodels_',teams[t]),centerturnmodels)

#save models (adjust path for your own folder)
# saveRDS(get(paste0('centerturnmodels_',teams[t])),
        # paste0(savepath,'model_rds\\','centerturnmodels_',teams[t]))

#create column in offense_hot for training data predictions
offense_hot$centerturn_pred <- predict(centerturnmodels,offense_hot %>% select(names(odata)))
offense_hot$centerturn_pred_right <- predict(centerturnmodels,offense_hot %>% select(names(odata)),type='prob')$right
offense_hot$centerturn_pred_left <- predict(centerturnmodels,offense_hot %>% select(names(odata)),type='prob')$left

#create column in offense_hot for training data predictions
weeks$centerturn_pred <- ifelse(weeks$id_all_frame %in% (offense_hot %>% select(id_all_frame) %>% unname %>% unlist),
                               vlookup(lookup_value = weeks$id_all_frame,
                                       dict = offense_hot %>% select(id_all_frame,centerturn_pred),
                                       result_column = 2,
                                       lookup_column = 1),
                               weeks$centerturn_pred)

#create columns in "weeks" for training data predictions, using offense_hot
weeks$centerturn_pred_right <- ifelse(weeks$id_all_frame %in% (offense_hot %>% select(id_all_frame) %>% unname %>% unlist),
                                        vlookup(lookup_value = weeks$id_all_frame,
                                                dict = offense_hot %>% select(id_all_frame,centerturn_pred_right),
                                                result_column = 2,
                                                lookup_column = 1),
                                        weeks$centerturn_pred_right)

weeks$centerturn_pred_left <- ifelse(weeks$id_all_frame %in% (offense_hot %>% select(id_all_frame) %>% unname %>% unlist),
                                        vlookup(lookup_value = weeks$id_all_frame,
                                                dict = offense_hot %>% select(id_all_frame,centerturn_pred_left),
                                                result_column = 2,
                                                lookup_column = 1),
                                        weeks$centerturn_pred_left)



#create column in offense_hot for test data predictions
offense_test$centerturn_pred <- predict(centerturnmodels,offense_test %>% select(names(odata)))
offense_test$centerturn_pred_right <- predict(centerturnmodels,offense_test %>% select(names(odata)),type='prob')$right
offense_test$centerturn_pred_left <- predict(centerturnmodels,offense_test %>% select(names(odata)),type='prob')$left

#create columns in "weeks" for test data predictions, using offense_hot
weeks$centerturn_pred_testdata <- ifelse(weeks$id_all_frame %in% (offense_test %>% select(id_all_frame) %>% unname %>% unlist),
                                        vlookup(lookup_value = weeks$id_all_frame,
                                                dict = offense_test %>% select(id_all_frame,centerturn_pred),
                                                result_column = 2,
                                                lookup_column = 1),
                                        weeks$centerturn_pred_testdata)

weeks$centerturn_pred_right_testdata <- ifelse(weeks$id_all_frame %in% (offense_test %>% select(id_all_frame) %>% unname %>% unlist),
                                                 vlookup(lookup_value = weeks$id_all_frame,
                                                         dict = offense_test %>% select(id_all_frame,centerturn_pred_right),
                                                         result_column = 2,
                                                         lookup_column = 1),
                                                 weeks$centerturn_pred_right_testdata)

weeks$centerturn_pred_left_testdata <- ifelse(weeks$id_all_frame %in% (offense_test %>% select(id_all_frame) %>% unname %>% unlist),
                                                 vlookup(lookup_value = weeks$id_all_frame,
                                                         dict = offense_test %>% select(id_all_frame,centerturn_pred_left),
                                                         result_column = 2,
                                                         lookup_column = 1),
                                                 weeks$centerturn_pred_left_testdata)

}


# 1 = left, and 2 = right
weeks$centerturn_pred <- ifelse(weeks$centerturn_pred==1,'left',
                                ifelse(weeks$centerturn_pred==2,'right',weeks$centerturn_pred))

weeks$centerturn_pred_testdata <- ifelse(weeks$centerturn_pred_testdata==1,'left',
                                         ifelse(weeks$centerturn_pred_testdata==2,'right',weeks$centerturn_pred_testdata))





#data frame with teams, maximum kappa, and top 5 variables of importance
k_imp_center <- data.frame(Team=teams,
                    Accuracy_Model=(maxacc_rf %>% unlist)[1:length(teams)],
                    # Kappa_Model=maxkap_rf %>% unlist,
                    Importance=(varimp_rf %>% unlist)[1:length(teams)])


k_imp_center$Accuracy_TestData <- vlookup(lookup_value = k_imp_center$Team,
                                   dict = data.frame(weeks %>% filter(off_def=='O' & position=='C') %>% 
                                                       filter(is.na(centerturn_pred_testdata)==F) %>% 
                                                       select(team,centerTurn,centerturn_pred_testdata) %>% 
                                                       mutate(acc=ifelse(centerTurn==centerturn_pred_testdata,1,0)) %>%
                                                       group_by(team) %>% 
                                                       summarize(acc_test=sum(acc)/length(acc))),
                                   result_column = 2,
                                   lookup_column = 1)

k_imp_center <- k_imp_center %>% arrange(-Accuracy_Model) %>% select(Team,
                                        # Kappa_Model,
                                        Accuracy_Model,Accuracy_TestData,Importance)
k_imp_center <- k_imp_center %>% mutate(Accuracy_Model=paste0(round(Accuracy_Model,2)*100,'%'),
                          Accuracy_TestData=paste0(round(Accuracy_TestData,2)*100,'%')
)

# data frame without variables of importance (easier to read)
k_imp_center1 <- setNames(k_imp_center %>% select(-Importance),c('Team',
                                                                 # 'Kappa (Validation)',
                                                                 'Accuracy (Validation)','Accuracy (Test Data)'))


# create a flextable object for k_imp_center1
centerTurnResults <- 
  k_imp_center1 %>% 
  regulartable() %>% 
  width(j=c(2,3),width=2.5) %>%
  fontsize(size=12,part='all') %>% 
  bg(i=even(as.numeric(rownames(k_imp_center1)))==TRUE,bg='gray80',part='body') %>% 
  bg(i=odd(as.numeric(rownames(k_imp_center1)))==TRUE,bg='gray95',part='body') %>%
  bg(part='header',bg='dodgerblue') %>% color(color='white',part='header') %>% 
  align(align = 'center',part='all') %>% 
  align(j=c(1),align = 'left',part='all')

