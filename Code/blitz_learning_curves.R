# learning curves for blitz models
# (MUST RUN create_features.R FIRST)
# (MUST LOAD PACKAGES FROM create_features.R FIRST)


team_to_analyze <- 'DAL'
team_display <- 'Dallas Cowboys'



# only include pre-snap data, defensive players, and teams that are in the "teams" variable
defense <- weeks %>% filter(frameId<=snap_frame &
                              team==team_to_analyze & 
                              off_def=='D')


# test data for Cowboys is different, because I wanted to include a specific week 2 play in test data
if(team_to_analyze=='DAL'){
  defense$test_weeks <- ifelse(defense$gameId %in% (plays %>% filter(defensiveTeam==team_to_analyze & (week >= 6)) %>% select(gameId) %>% unname %>% unlist),1,0)
  defense$test_weeks <- ifelse(defense$id=='2021091911_1143',1,defense$test_weeks)
} else {
  defense$test_weeks <- ifelse(defense$gameId %in% (plays %>% filter(defensiveTeam==team_to_analyze & week >= 6) %>% select(gameId) %>% unname %>% unlist)[1],1,0)
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


# set seed for reproduction 
set.seed(111)



#create empty data frames to store data for ggplot
trainCurve <- data.frame(`Training Set Size` = c(20,seq(nrow(ddata)-nrow(ddata)*0.9,nrow(ddata),nrow(ddata)*0.1)),
                       Kappa = c(20,seq(nrow(ddata)-nrow(ddata)*0.9,nrow(ddata),nrow(ddata)*0.1)),
                       Type = 'Train Kappa')

testCurve <- data.frame(`Training Set Size` = c(20,seq(nrow(ddata)-nrow(ddata)*0.9,nrow(ddata),nrow(ddata)*0.1)),
                         Kappa = c(20,seq(nrow(ddata)-nrow(ddata)*0.9,nrow(ddata),nrow(ddata)*0.1)),
                         Type = 'Test Kappa')





cntr <- 0
# loop over training examples
# I added 20 in the beginning, so you can really see the improvements given more data
for (i in c(20,seq(nrow(ddata)-nrow(ddata)*0.9,nrow(ddata),nrow(ddata)*0.1))) {
  
  cntr <- cntr+1
  
  
  trainCurve$`Training Set Size`[cntr] <- i
  testCurve$`Training Set Size`[cntr] <- i
  

  # train learning algorithm with size i
  
  
  mod <-
    train(role~.,
          data=ddata[1:i,],
          trControl = blitzprobctrl,
          method='rf',
          tuneGrid=data.frame(mtry=c(2,5,10)),
          metric='Kappa'
    )
  
  
  #training kappa values
  trainCurve$Kappa[cntr] <- (mod$results %>% select(Kappa) %>% arrange(-Kappa) %>% unname %>% unlist)[1]

  
  
  # predict test data
  prediction <- predict(mod, newdata = defense_test %>% select(-role))
    kappa <- postResample(prediction, defense_test %>% select(role) %>% unname %>% unlist)
  testCurve$Kappa[cntr] <- kappa[1]
}

#bind trainCurve and testCurve together for ggplot
learnCurve <- rbind(trainCurve,testCurve)

p <- 
  ggplot(learnCurve,aes(x = `Training Set Size`, y = Kappa))+
  
  #add team logo
  geom_image(aes(x=nrow(ddata)*0.9, y=0.1), image=nflfastR::teams_colors_logos %>% filter(team_abbr==team_to_analyze) %>% select(team_logo_espn) %>% unlist %>% unname, size=0.2)+
  
  #smooth lines using loess
  geom_smooth(aes(color=Type), method = loess, span = .8, fill=NA) +
  
  #title
  ggtitle(paste0(team_display,' Blitz Prediction Learning Curves'))+
  
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
  ylim(0,1) +
  
  #change legend size
  guides(color = guide_legend(override.aes = list(size=10)))


ggsave(paste0(savepath,'blitz_learningCurves.png'),plot = p)
