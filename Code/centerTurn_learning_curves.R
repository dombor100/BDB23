# learning curves for center turn models
# (MUST RUN create_features.R FIRST)
# (MUST LOAD PACKAGES FROM create_features.R FIRST)


team_to_analyze <- 'NE'
team_display <- 'New England Patriots'


# only include pre-snap data, the offensive center, and teams that are in the "teams" variable
offense <- weeks %>% filter(frameId<=snap_frame &
                              team==team_to_analyze &
                              off_def=='O' & 
                              position=='C')


# test data for Chargers is different, because I wanted to include a specific week 2 play in test data
if(team_to_analyze=='LAC'){
  offense$test_weeks <- ifelse(offense$gameId %in% (plays %>% filter(possessionTeam==team_to_analyze & (week >= 6)) %>% select(gameId) %>% unname %>% unlist),1,0)
  offense$test_weeks <- ifelse(offense$id=='2021091911_1143',1,offense$test_weeks)
} else {
  offense$test_weeks <- ifelse(offense$gameId %in% (plays %>% filter(possessionTeam==team_to_analyze & week >= 6) %>% select(gameId) %>% unname %>% unlist),1,0)
}


#select features for model
offense <- offense %>% 
  select(test_weeks,
         centerTurn,
         onLOS_Agap_left_forOffense,
         onLOS_Agap_right_forOffense,
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


#create function for pre processing

preproc <- function(data_to_pre_process){
  
  #create dummy variables and perform one hot encoding
  dummy <- dummyVars(centerTurn~.,data=data_to_pre_process %>% select(-id_all_frame,-test_weeks))
  data_hot <- data.frame(predict(dummy,newdata=data_to_pre_process))
  data_hot <- cbind(id_all_frame=data_to_pre_process$id_all_frame,
                    centerTurn=data_to_pre_process$centerTurn,
                    test_weeks=data_to_pre_process$test_weeks,
                    data_hot)
  
  #do not coerce these variables to factors
  numeric_features <- grep('LminusR',names(data_hot))
  numeric_features2 <- grep('timeToSnap',names(data_hot))
  
  #coerce all variables to factors, except for the ones above
  data_hot_factors <- data.frame(lapply(data_hot[,-c(numeric_features,numeric_features2)],as.factor))
  
  #bind the non-factor variables to the factor variables
  out <- cbind(data_hot_factors,data_hot[,c(numeric_features,numeric_features2)])
  
  #identify columns with only one unique value
  n <- ncol(out)
  remove_col <- vector(length=ncol(out))
  for (i in 1:n){
    if (n_distinct(out[,i]) == 1){
      remove_col[i] <- i
    }
  }
  
  #remove 0's from remove_col
  remove_col <- remove_col[remove_col!=0]
  
  
  #remove columns with only 1 unique value
  if(length(remove_col)>0){
    out <- subset(out,select=-remove_col)
  }
  
  return(out)
  
}



#create train and test sets
#ideally, should split before pre processing,
  #but I was not concerned with data leakage since there was no centering or scaling,
  #and the factor sets should NOT change between train and test data
offense_train <- preproc(offense) %>% filter(test_weeks!=1) %>% select(-test_weeks)
offense_test <- preproc(offense) %>% filter(test_weeks==1) %>% select(-test_weeks)

#store training data in this dataset, so we can keep track of the id_all_frame in the other dataset
#this is the data for model training
odata <- offense_train %>% select(-id_all_frame)



#create train control
#using out of bag, since these are random forest models
centerturnctrl <-  
  trainControl(method='boot632', #caret won't save predictions with method='oob'
               classProbs = TRUE,
               savePredictions = TRUE)

#set seed for reproduction
set.seed(111)


#create empty data frames to store data for ggplot
trainCurve <- data.frame(`Training Set Size` = seq(nrow(odata)-nrow(odata)*0.9,nrow(odata),nrow(odata)*0.1),
                       Accuracy = seq(nrow(odata)-nrow(odata)*0.9,nrow(odata),nrow(odata)*0.1),
                       Type = 'Train Accuracy')

testCurve <- data.frame(`Training Set Size` = seq(nrow(odata)-nrow(odata)*0.9,nrow(odata),nrow(odata)*0.1),
                         Accuracy = seq(nrow(odata)-nrow(odata)*0.9,nrow(odata),nrow(odata)*0.1),
                         Type = 'Test Accuracy')




cntr <- 0
# loop over training examples
for (i in seq(nrow(odata)-nrow(odata)*0.9,nrow(odata),nrow(odata)*0.1)) {
  
  cntr <- cntr+1
  
  
  trainCurve$`Training Set Size`[cntr] <- i
  testCurve$`Training Set Size`[cntr] <- i
  

  # train learning algorithm with size i
  
  
  mod <-
    train(centerTurn~.,
        data=odata[1:i,],
        trControl = centerturnctrl,
        method='rf',
        tuneGrid=data.frame(mtry=2),
        metric='Accuracy'
        )

  trainCurve$Accuracy[cntr] <- (mod$results %>% select(Accuracy) %>% arrange(-Accuracy) %>% unname %>% unlist)[1]

  
  # predict test data
  prediction <- predict(mod, newdata = offense_test %>% select(-centerTurn))
    accuracy <- postResample(prediction, offense_test %>% select(centerTurn) %>% unname %>% unlist)
  testCurve$Accuracy[cntr] <- accuracy[1]
}

#bind trainCurve and testCurve together for ggplot
learnCurve <- rbind(trainCurve,testCurve)


p <- ggplot(learnCurve,aes(x = `Training Set Size`, y = Accuracy))+
  
  #add team logo
  geom_image(aes(x=nrow(odata)*0.9, y=0.55), image=nflfastR::teams_colors_logos %>% filter(team_abbr==team_to_analyze) %>% select(team_logo_espn) %>% unlist %>% unname, size=0.2)+
  
  #smooth lines using loess
  geom_smooth(aes(color=Type), method = loess, span = .8, fill=NA) +
  
  #title
  ggtitle(paste0(team_display,' Center Turn Prediction Learning Curves'))+
  
  #theme options
  theme(plot.title = element_text(size=10,hjust = 0.5,face='bold'),
        panel.background = element_rect(fill = 'black',
                                        color = 'black'),
        axis.text=element_text(size=8),
        axis.title=element_text(size=9,face='bold'),
        legend.text=element_text(size=8),
        legend.title = element_text(size = 9),
        panel.grid.major = element_line(color='gray30'),
        panel.grid.minor = element_line(color='gray30')
  )+
  
  #vertical window
  ylim(0.5,1) +
  
  #change legend size
  guides(color = guide_legend(override.aes = list(size=10)))

ggsave(paste0(savepath,'centerTurn_learningCurves.png'),plot = p)



# ROC
library(pROC)

pred <- mod$pred$right
obs <- ifelse(mod$pred$obs=='right',1,0)

roc_obj <- roc(obs,pred)
rp <- ggroc(roc_obj)

ggsave(paste0(savepath,'centerTurn_ROC.png'),plot = rp)


