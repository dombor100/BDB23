# The game plan recommendatio in my report shows there is an 80% probability that the center turn is to the RIGHT
# This is the prediction used for that assessment

#create data frame to predict center turn
recommendation <- data.frame(passStrengthbalanced = 0,
                             passStrengthleft = 0,
                             passStrengthright = 1,
                             TEs_right = 0,
                             TEs_left = 0,
                             RBs_right = 0,
                             RBs_left = 1,
                             hashL=1,
                             hashLM=1,
                             hashM=1,
                             hashR=1,
                             hashRM=1,
                             rightBoxOverload=0,
                             leftBoxOverload=0,
                             rightLOSOverload=1,
                             leftLOSOverload=0,
                             inWideBox_LminusR=0,
                             onLOS_inside_LminusR=-1,
                             onLOS_outside_LminusR=0,
                             outside_2ndLevel_LminusR=0,
                             inside_2ndLevel_LminusR=1,
                             timeToSnap=0
                             )

#do not coerce these features to factors
numeric_features <- grep('LminusR',names(recommendation))
numeric_features2 <- grep('timeToSnap',names(recommendation))

#coerce all variables to factors, except for the ones above
recommendation_factors <- data.frame(lapply(recommendation[,-c(numeric_features,numeric_features2)],as.factor))

#bind the non-factor variables to the factor variables
recommendation <- cbind(recommendation_factors,recommendation[,c(numeric_features,numeric_features2)])

#make the prediction
predict(centerturnmodels_NE,recommendation,type='prob')
