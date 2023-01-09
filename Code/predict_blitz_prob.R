# CREATE BLITZ PROBABILITY MODELS
# (MUST RUN create_features.R FIRST)
# (MUST LOAD PACKAGES FROM create_features.R FIRST)

# only include teams that had 20 or more relevant snaps in weeks 6-8
#do not include the Cowboys: I ran a separate analysis on them
teams <- plays %>% filter(week>=6) %>% count(defensiveTeam) %>% filter(n>=20) %>% select(defensiveTeam) %>% unlist %>% unname

# add DAL if they were not included
if(sum(str_detect(teams,'DAL'))==0){
  teams <- c('DAL',teams)
}


#create lists to store findings, and columns to store predictions
maxacc_rf_blitz <- list()
maxkap_rf_blitz <- list()
varimp_rf_blitz <- list()

weeks$blitzprob_pred <- NA
weeks$blitzprob_pred_testdata <- NA
weeks$blitzprob_pred_Coverage <- NA
weeks$blitzprob_pred_Coverage_testdata <- NA
weeks$blitzprob_pred_PassRush <- NA
weeks$blitzprob_pred_PassRush_testdata <- NA



# CREATE MODELS 
for(t in 1:length(teams)){
  

# only include pre-snap data, defensive players, and teams that are in the "teams" variable
defense <- weeks %>% filter(frameId<=snap_frame &
                                 team==teams[t] & 
                                 off_def=='D')


# test data for Cowboys is different, because I wanted to include a specific week 2 play in test data
if(teams[t]=='DAL'){
  defense$test_weeks <- ifelse(defense$gameId %in% (plays %>% filter(defensiveTeam==teams[t] & (week >= 6)) %>% select(gameId) %>% unname %>% unlist),1,0)
  defense$test_weeks <- ifelse(defense$id=='2021091911_1143',1,defense$test_weeks)
} else {
  defense$test_weeks <- ifelse(defense$gameId %in% (plays %>% filter(defensiveTeam==teams[t] & week >= 6) %>% select(gameId) %>% unname %>% unlist)[1],1,0)
}

#identify defensive players moving to/away from LOS
# 1 = moving towards ball, -1 = moving away
defense$movingTowardsBall <- ifelse(defense$playDirection=='left' & defense$y<defense$ball & defense$dir>270 & defense$dir<360,1,
                                       ifelse(defense$playDirection=='left' & defense$y>defense$ball & defense$dir>180 & defense$dir<270,1,
                                              ifelse(defense$playDirection=='right' & defense$y>defense$ball & defense$dir>90 & defense$dir<180,1,
                                                     ifelse(defense$playDirection=='right' & defense$y<defense$ball & defense$dir>0 & defense$dir<90,1,-1))))


#multiply speed by movingTowardsBall to give "velocity"
defense$velocity <- defense$s * defense$movingTowardsBall

# distance from LOS, if defender is in wide box
# 10 times the distance from the LOS, if the defender is 15+ yards away from the ball (distance from the football along the short axis)
# 4 times the distance from the LOS if the defender is outside the wide box, but within 15 yards of the ball
# 10 & 4 are not scientifically proven factors. more work should be completed to hone in on more appropriate values
defense$adjDistanceFromLOS <- ifelse(defense$inWideBox==1, abs(defense$x-defense$LOS),
                                     ifelse(abs(defense$y-defense$ball)>=15,10*abs(defense$x-defense$LOS),
                                            4*abs(defense$x-defense$LOS)))



#select features for model
defense <- defense %>% 
  select(role,
         timeToSnap,
         velocity,
         adjDistanceFromLOS,
         nflId,
         defenderGap,
         inWideBox_inside_left,
         inWideBox_outside_left,
         inWideBox_inside_right,
         inWideBox_outside_right,
         onLOS_Agap_left,
         onLOS_Agap_right,
         onLOS_inside_left,
         onLOS_outside_left,
         onLOS_inside_right,
         onLOS_outside_right,
         passStrength,
         TEs_right,
         TEs_left,
         RBs_right,
         RBs_left,
         test_weeks,
         id_all_frame)






#create dummy variables and perform one hot encoding
dummy <- dummyVars(role~.,data=defense %>% select(-id_all_frame,-test_weeks))
defense_hot <- data.frame(predict(dummy,newdata=defense))
defense_hot <- cbind(id_all_frame=defense$id_all_frame,
                        role=defense$role,
                       test_weeks=defense$test_weeks,
                       defense_hot)

#do not coerce these variables to factors
numeric_features <- grep('velocity',names(defense_hot))
numeric_features2 <- grep('condensed',names(defense_hot))
numeric_features3 <- grep('adjDist',names(defense_hot))
numeric_features4 <- grep('timeToSnap',names(defense_hot))

#coerce all variables to factors, except for the ones above
defense_hot_factors <- data.frame(lapply(defense_hot[,-c(numeric_features,numeric_features2,numeric_features3,numeric_features4)],as.factor))

#bind the non-factor variables to the factor variables
defense_hot <- cbind(defense_hot_factors,defense_hot[,c(numeric_features,numeric_features2,numeric_features3,numeric_features4)])


#create train and test sets
defense_train <- defense_hot %>% filter(test_weeks!=1) %>% select(-test_weeks)
defense_test <- defense_hot %>% filter(test_weeks==1) %>% select(-test_weeks)

#store training data in this dataset, so we can keep track of the id_all_frame in the other dataset
#this is the data for model training
ddata <- defense_train %>% select(-id_all_frame)


#identify columns with only one unique value
n <- ncol(ddata)
remove_col <- vector(length=ncol(ddata))
for (i in 1:n){
  if (n_distinct(ddata[,i]) == 1){
    remove_col[i] <- i
  }
}

#remove 0's from remove_col
remove_col <- remove_col[remove_col!=0]


#remove columns with only 1 unique value
if(length(remove_col)>0){
  ddata <- subset(ddata,select=-remove_col)
}


#create train control
#using out of bag, since these are random forest models
blitzprobctrl <-  
  trainControl(method='oob',
               savePredictions = 'all')

# set seed for reproducibility
set.seed(111)

#train models
blitzprobmodels <- 
  train(role~.,
        data=ddata,
        trControl = blitzprobctrl,
        method='rf',
        tuneGrid=data.frame(mtry=c(2,5,10)),
        metric='Kappa'
  )

#store accuracy, kappa, and important variables
maxacc_rf_blitz[t] <- max(blitzprobmodels$results$Accuracy)
maxkap_rf_blitz[t] <- max(blitzprobmodels$results$Kappa)
varimp_rf_blitz[t] <- paste(rownames(varImp(blitzprobmodels)$importance %>% arrange(-Overall))[1:5],collapse=', ')



#create object for each model
assign(paste0('blitzprobmodels_',teams[t]),blitzprobmodels)

#save models (adjust path for your own folder)
# saveRDS(get(paste0('blitzprobmodels_',teams[t])),
#         paste0(savepath,'model_rds\\','blitzprobmodels_',teams[t]))





#create column in defense_hot for training data predictions
defense_hot$blitzprob_pred <- predict(blitzprobmodels,defense_hot %>% select(names(ddata)))
defense_hot$blitzprob_pred_Coverage <- predict(blitzprobmodels,defense_hot %>% select(names(ddata)),type='prob')$Coverage
defense_hot$blitzprob_pred_PassRush <- predict(blitzprobmodels,defense_hot %>% select(names(ddata)),type='prob')$`Pass Rush`

#create columns in "weeks" for training data predictions, using defense_hot
weeks$blitzprob_pred <- ifelse(is.na(weeks$blitzprob_pred)==T,
                                vlookup(lookup_value = weeks$id_all_frame,
                                        dict = defense_hot %>% select(id_all_frame,blitzprob_pred),
                                        result_column = 2,
                                        lookup_column = 1),
                                weeks$blitzprob_pred)

weeks$blitzprob_pred_Coverage <- ifelse(is.na(weeks$blitzprob_pred_Coverage)==T,
                                      vlookup(lookup_value = weeks$id_all_frame,
                                              dict = defense_hot %>% select(id_all_frame,blitzprob_pred_Coverage),
                                              result_column = 2,
                                              lookup_column = 1),
                                      weeks$blitzprob_pred_Coverage)

weeks$blitzprob_pred_PassRush <- ifelse(is.na(weeks$blitzprob_pred_PassRush)==T,
                                     vlookup(lookup_value = weeks$id_all_frame,
                                             dict = defense_hot %>% select(id_all_frame,blitzprob_pred_PassRush),
                                             result_column = 2,
                                             lookup_column = 1),
                                     weeks$blitzprob_pred_PassRush)



#create column in defense_test for test data predictions
defense_test$blitzprob_pred <- predict(blitzprobmodels,defense_test %>% select(names(ddata)))
defense_test$blitzprob_pred_Coverage <- predict(blitzprobmodels,defense_test %>% select(names(ddata)),type='prob')$Coverage
defense_test$blitzprob_pred_PassRush <- predict(blitzprobmodels,defense_test %>% select(names(ddata)),type='prob')$`Pass Rush`


#create columns in "weeks" for test data predictions, using defense_test
weeks$blitzprob_pred_testdata <- ifelse(is.na(weeks$blitzprob_pred_testdata)==T,
                                         vlookup(lookup_value = weeks$id_all_frame,
                                                 dict = defense_test %>% select(id_all_frame,blitzprob_pred),
                                                 result_column = 2,
                                                 lookup_column = 1),
                                         weeks$blitzprob_pred_testdata)

weeks$blitzprob_pred_Coverage_testdata <- ifelse(is.na(weeks$blitzprob_pred_Coverage_testdata)==T,
                                               vlookup(lookup_value = weeks$id_all_frame,
                                                       dict = defense_test %>% select(id_all_frame,blitzprob_pred_Coverage),
                                                       result_column = 2,
                                                       lookup_column = 1),
                                               weeks$blitzprob_pred_Coverage_testdata)

weeks$blitzprob_pred_PassRush_testdata <- ifelse(is.na(weeks$blitzprob_pred_PassRush_testdata)==T,
                                              vlookup(lookup_value = weeks$id_all_frame,
                                                      dict = defense_test %>% select(id_all_frame,blitzprob_pred_PassRush),
                                                      result_column = 2,
                                                      lookup_column = 1),
                                              weeks$blitzprob_pred_PassRush_testdata)

#fill in post snap probabilities
#use final prediction frame (time == snap_time) to fill out the remainder of the play
weeks$blitzprob_pred_PassRush <- 
  ifelse(weeks$team==teams[t] & weeks$off_def=='D' & weeks$time > weeks$snap_time,
         vlookup(lookup_value = weeks$id_all,
                 dict = weeks %>% filter(team==teams[t] & off_def=='D' & event=='ball_snap') %>% select(id_all,blitzprob_pred_PassRush),
                 result_column = 2,
                 lookup_column = 1),
         weeks$blitzprob_pred_PassRush)


}


# 1 = coverage, and 2 = pass rush
weeks$blitzprob_pred <- ifelse(weeks$blitzprob_pred==1,'Coverage',
                               ifelse(weeks$blitzprob_pred==2,'Pass Rush',weeks$blitzprob_pred))

weeks$blitzprob_pred_testdata <- ifelse(weeks$blitzprob_pred_testdata==1,'Coverage',
                                        ifelse(weeks$blitzprob_pred_testdata==2,'Pass Rush',weeks$blitzprob_pred_testdata))








#data frame with teams, kappa, accuracy, and top 5 variables of importance
k_imp_blitz <- data.frame(Team=teams,
                    Accuracy_Model=(maxacc_rf_blitz %>% unlist)[1:length(teams)],
                    Kappa_Model=(maxkap_rf_blitz %>% unlist)[1:length(teams)],
                    Importance=(varimp_rf_blitz %>% unlist)[1:length(teams)]) %>%
  arrange(-Kappa_Model)

k_imp_blitz$Accuracy_TestData <- vlookup(lookup_value = k_imp_blitz$Team,
                                   dict = data.frame(weeks %>% filter(off_def=='D') %>% 
                                                       filter(is.na(blitzprob_pred_testdata)==F) %>% 
                                                       select(team,role,blitzprob_pred_testdata) %>% 
                                                       mutate(acc=ifelse(role==blitzprob_pred_testdata,1,0)) %>%
                                                       group_by(team) %>% 
                                                       summarize(acc_test=sum(acc)/length(acc)) %>%
                                                       arrange(-acc_test)),
                                   result_column = 2,
                                   lookup_column = 1)

k_imp_blitz <- k_imp_blitz %>% select(Team,Kappa_Model,Accuracy_Model,Accuracy_TestData,Importance)
k_imp_blitz <- k_imp_blitz %>% mutate(Accuracy_Model=paste0(round(Accuracy_Model,2)*100,'%'),
                          Kappa_Model=paste0(round(Kappa_Model,2)*100,'%'),
                          Accuracy_TestData=paste0(round(Accuracy_TestData,2)*100,'%')
)


# data frame without variables of importance (easier to read)
k_imp_blitz1 <- setNames(k_imp_blitz %>% select(-Importance),c('Team','Kappa (Validation)','Accuracy (Validation)','Accuracy (Test Data)'))

# create a flextable object for k_imp_blitz1
blitzResults <- 
  k_imp_blitz1 %>% 
  regulartable() %>% 
  width(j=c(2,3,4),width=2.5) %>%
  fontsize(size=12,part='all') %>% 
  bg(i=even(as.numeric(rownames(k_imp_blitz1)))==TRUE,bg='gray80',part='body') %>% 
  bg(i=odd(as.numeric(rownames(k_imp_blitz1)))==TRUE,bg='gray95',part='body') %>%
  bg(part='header',bg='dodgerblue') %>% color(color='white',part='header') %>% 
  align(align = 'center',part='all') %>% 
  align(j=c(1),align = 'left',part='all')
