# THIS FILE WILL RUN MY ENTIRE ANALYSIS
# Please save all .R files to the directory you specify in "savepath" below
# Please save all data from the Big Data Bowl 23 website to the directory you specify in "savepath" below
# Big Data Bowl 23 website = https://www.kaggle.com/competitions/nfl-big-data-bowl-2023/data

#specify your directory (notice "\\" at the end of the string)
savepath <- 'D:\\Projects\\BigDataBowl23\\final\\'

#create all features for models
source(paste0(savepath,'create_features.R'))

#create center turn models
source(paste0(savepath,'predict_center_turn.R'))

#create blitz models
source(paste0(savepath,'predict_blitz_prob.R'))

#create center turn model learning curves
source(paste0(savepath,'centerTurn_learning_curves.R'))

#create blitz model learning curves
source(paste0(savepath,'blitz_learning_curves.R'))

#create animations
source(paste0(savepath,'animations.R'))

#analyze Micah Parsons
source(paste0(savepath,'micahparsons.R'))

#recommendation for game plan task
source(paste0(savepath,'recommendation.R'))

#analyze Patriots center turn tendencies
source(paste0(savepath,'pats_centerturn_findings.R'))

#create defensive front density plots
source(paste0(savepath,'defensive_front_density.R'))

#save "weeks" data set (takes up ~64 MB)
# saveRDS(weeks,paste0(savepath,'weeks.rds'))


paste('micahparsons.R, recommendation.R, and pats_centerturn_findings.R all print to the console.  Please run these files and examine the outputs separately')





