# THIS FILE WILL RUN MY ENTIRE ANALYSIS
# Please save all .R files to the directory you specify in "savepath" below
# Please save all data from the Big Data Bowl 23 website to the directory you specify in "savepath" below
# Big Data Bowl 23 website = https://www.kaggle.com/competitions/nfl-big-data-bowl-2023/data

# when I ran these models, I used a computer with:
  # 32 GB RAM, Intel i7-11850H processor
  # Windows 10, Version 10.0

start_time <- Sys.time()

#specify your directory (notice "\\" at the end of the string)
savepath <- '\\BDB23-main\\Code\\'

#or use this
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
savepath <- paste0(getwd(),'/')

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


#variable importance plots for Kaggle Winning Model Documentation
#New England center turn prediction model
p <- ggplot(data=varImp(readRDS(paste0(savepath,'centerturnmodels_NE.rds'))),top = 10)+
  geom_col(col='dodgerblue',fill='dodgerblue')+
  ggtitle('Variable Importance - Center Turn Prediction Models - New England Patriots')+
  theme(plot.title = element_text(size=16,face='bold',hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face='bold'))
ggsave(paste0(savepath,'varImp_centerTurn_NE.png'),plot = p)

#partial dependence plot for Kaggle Winning Model Documentation
#Dallas blitz prediction model - adjDistanceFromLOS feature
dal_data <- readRDS(paste0(savepath,'blitzprobmodels_DAL_DATA.rds'))
plot_data <- data.frame(adjDistanceFromLOS=seq(1,10,0.5),
                        AvgBlitzProbability=seq(1,10,0.5))
cntr <- 0
for(i in seq(1,10,0.5)){
  cntr <- cntr+1
  dal_data$adjDistanceFromLOS <- i
  pred <- mean(predict(readRDS(paste0(savepath,'blitzprobmodels_DAL.rds')),dal_data,type='prob')$`Pass Rush`)
  plot_data$AvgBlitzProbability[cntr] <- pred
}

pp <- ggplot(plot_data)+
  geom_smooth(aes(x=adjDistanceFromLOS,y=AvgBlitzProbability),method='loess',fill=NA)+
  ylim(0,1)+
  ggtitle('Partial Dependence - adjDistanceFromLOS - Blitz Prediction Models - Dallas Cowboys')+
  theme(plot.title = element_text(size=16,face='bold',hjust = 0.5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=14,face='bold'))
ggsave(paste0(savepath,'partialPlot_blitzProb_DAL_adjDist.png'),plot = pp)



end_time <- Sys.time()

end_time-start_time
