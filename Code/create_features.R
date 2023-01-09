#CREATE FEATURES FOR ANALYSIS

gc()

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

unregister_dopar()

pacman::p_load(pacman,caret,caretEnsemble,expss,purrr,stringr,dplyr,tibble,ggplot2,doParallel,gganimate,nflfastR,ggimage,gifski,grid,flextable,gtools,zoo)

cores=detectCores()
registerDoParallel(cores=cores)

#store files locally, and point to their location with "savepath"
# savepath <- paste0('D:\\Projects\\BigDataBowl23\\final\\')

#load data
games <- read.csv(paste0(savepath,'games.csv'))
pffScoutingData <- read.csv(paste0(savepath,'pffScoutingData.csv'))
players <- read.csv(paste0(savepath,'players.csv'))
plays <- read.csv(paste0(savepath,'plays.csv'))


#data frame for all weeks
weeks <- rbind(read.csv(paste0(savepath,'week1.csv')),
               read.csv(paste0(savepath,'week2.csv')),
               read.csv(paste0(savepath,'week3.csv')),
               read.csv(paste0(savepath,'week4.csv')),
               read.csv(paste0(savepath,'week5.csv')),
               read.csv(paste0(savepath,'week6.csv')),
               read.csv(paste0(savepath,'week7.csv')),
               read.csv(paste0(savepath,'week8.csv')))


#create unique identifier for gameId + playId
pffScoutingData$id <- paste0(pffScoutingData$gameId,'_',pffScoutingData$playId)
plays$id <- paste0(plays$gameId,'_',plays$playId)
weeks$id <- paste0(weeks$gameId,'_',weeks$playId)

#create unique identifier for gameId + playId + nflId
pffScoutingData$id_all <- paste0(pffScoutingData$gameId,'_',pffScoutingData$playId,'_',pffScoutingData$nflId)
weeks$id_all <- paste0(weeks$gameId,'_',weeks$playId,'_',weeks$nflId)

#create unique identifier for gameId + playId + nflId + frameId
weeks$id_all_frame <- paste0(weeks$id_all,'_',weeks$frameId)

#create unique identifier for gameId + playId + frameId (without nflId)
weeks$id_frame <- paste0(weeks$id,'___',weeks$frameId)



#only use shotgun plays
weeks <- weeks %>% filter(id %in% (plays %>% filter(offenseFormation=='SHOTGUN') %>% select(id) %>% unname %>% unlist))

#change time format
weeks$time <- as.POSIXct(weeks$time,format='%Y-%m-%dT%H:%M:%OS')


#create variable for time_elapsed
#and remove plays less than 3 seconds in length
weeks$time_elapsed <- vlookup(lookup_value = weeks$id,
                              dict = weeks %>% 
                                select(id,time) %>% 
                                group_by(id) %>%
                                filter(time %in% c(max(time),min(time))) %>% mutate(time_elapsed=max(time)-min(time)) %>% 
                                select(id,time_elapsed),
                              result_column = 2,
                              lookup_column = 1)

weeks <- weeks %>% filter(time_elapsed>=3)

#simplify snap event
#this removes plays without a snap event, and converts automated snap events to manual, for data aggregation purposes
manual_ball_snap <- weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% select(id)
auto_ball_snap <- weeks %>% filter(is.na(nflId)==T & event=='autoevent_ballsnap' & !id %in% manual_ball_snap$id) %>% select(id)

ids <- rbind(manual_ball_snap,auto_ball_snap)

manual_ball_snap <- NULL
auto_ball_snap <- NULL

weeks <- weeks %>% filter(id %in% ids$id)

ids <- NULL

weeks <- weeks %>% mutate(event=ifelse(id %in% auto_ball_snap & event=='autoevent_ballsnap','ball_snap',event))

weeks$event <- ifelse(weeks$event=='autoevent_ballsnap','None',weeks$event)



#create column in "weeks" that shows player position lined up
weeks$position <- vlookup(lookup_value = weeks$id_all,
                          dict = pffScoutingData %>% select(id_all,pff_positionLinedUp),
                          result_column = 2,
                          lookup_column = 1)

#create "general" position names
weeks$position_general <- ifelse(weeks$position %in% c('LWR','RWR'),'WR',
                                 ifelse(weeks$position %in% c('SLiWR','SLoWR','SRiWR','SRoWR','SRWR','SLWR'),'WR',
                                        ifelse(weeks$position %in% c('TE-iL','TE-iR','TE-L','TE-R','TE-oL','TE-oR'),'Y',
                                               ifelse(weeks$position %in% c('HB','HB-R','HB-L'),'H',
                                                      ifelse(weeks$position %in% c('FB','FB-L','FB-R','FB-iR','FB-iL'),'F',
                                                             ifelse(weeks$position=='QB','Q',
                                                                    ifelse(weeks$position %in% c('LG','RG'),'G',
                                                                           ifelse(weeks$position %in% c('LT','RT'),'T',
                                                                                  ifelse(weeks$position %in% c('LILB','LLB','LOLB','MLB','RILB','RLB','ROLB'),'B',
                                                                                         ifelse(weeks$position %in% c('DRT','DLT','NT','NLT','NRT'),'T',
                                                                                                ifelse(weeks$position %in% c('LE','RE','LEO','REO'),'E',
                                                                                                       ifelse(weeks$position %in% c('RCB','LCB','SCBiL','SCBiR','SCBoL','SCBoR','SCBR','SCBL'),'C',
                                                                                                              ifelse(weeks$position %in% c('SS','SSL','SSR','FS','FSL','FSR'),'$',
                                                                                                                     weeks$position)))))))))))))

#create column in "weeks" showing if player is on defense or offense
weeks$off_def <- ifelse(weeks$position %in% c('C','FB','FB-L','FB-R','FB-iR','FB-iL','HB','HB-R','HB-L',
                                              'LG','LT','LWR','QB','RG','RT','RWR',
                                              'SLiWR','SLoWR','SRiWR','SRoWR','SRWR','SLWR',
                                              'TE-iL','TE-iR','TE-L','TE-oL','TE-oR','TE-R'),'O',
                        ifelse(is.na(weeks$position)==F,'D','ball'))

#create "basic" position names (less specific than position_general)
weeks$position_basic <- ifelse(weeks$off_def=='O' & weeks$position_general %in% c('T','G','C'),'OL',
                               ifelse(weeks$off_def=='O' & weeks$position_general %in% c('H','F','Y','S','O','Q'),'OS',
                                      ifelse(weeks$off_def=='D','D',weeks$position_general)))




#location of the ball (pre snap) between the hashes (y coordinate in player tracking data, where nflID == NA)
weeks$ball <- vlookup(lookup_value = weeks$id,
                      dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% select(id,y),
                      result_column = 2,
                      lookup_column = 1)


#absolute location (out of 120) of the line of scrimmage (used the center to locate the LOS, since the "ball" location was not consistent)
weeks$LOS <- vlookup(lookup_value = weeks$id,
                     dict = weeks %>% filter(position=='C' & event=='ball_snap') %>% select(id,x),
                     result_column = 2,
                     lookup_column = 1)




# determine hash
# L/R = within 1 yard of the left/right hash
# M = within 2 yards of exact MOF
# LM = between left hash and 2 yards from middle
# RM = between right hash and 2 yards from middle


leftDir_leftHash <- (160/3)/2 - 3.083333
leftDir_leftMiddle <- (160/3)/2 - 1
leftDir_rightMiddle <- (160/3)/2 + 1
leftDir_rightHash <- (160/3)/2 + 3.083333

rightDir_leftHash <- (160/3)/2 + 3.083333
rightDir_leftMiddle <- (160/3)/2 + 1
rightDir_rightMiddle <- (160/3)/2 - 1
rightDir_rightHash <- (160/3)/2 - 3.083333


weeks$hash <- ifelse(weeks$playDirection=='left' & weeks$ball<=leftDir_leftHash,'L',
                     ifelse(weeks$playDirection=='left' & between(weeks$ball,leftDir_leftHash,leftDir_leftMiddle),'LM',
                            ifelse(weeks$playDirection=='left' & between(weeks$ball,leftDir_leftMiddle,leftDir_rightMiddle),'M',
                                   ifelse(weeks$playDirection=='left' & between(weeks$ball,leftDir_rightMiddle,leftDir_rightHash),'RM',
                                          ifelse(weeks$playDirection=='left' & weeks$ball>=leftDir_rightHash,'R',
                                                 ifelse(weeks$playDirection=='right' & weeks$ball>=rightDir_leftHash,'L',
                                                        ifelse(weeks$playDirection=='right' & between(weeks$ball,rightDir_leftMiddle,rightDir_leftHash),'LM',
                                                               ifelse(weeks$playDirection=='right' & between(weeks$ball,rightDir_rightMiddle,rightDir_leftMiddle),'M',
                                                                      ifelse(weeks$playDirection=='right' & between(weeks$ball,rightDir_rightHash,rightDir_rightMiddle),'RM',
                                                                             ifelse(weeks$playDirection=='right' & weeks$ball<=rightDir_rightHash,'R',''))))))))))


#time of snap
weeks$snap_time <- vlookup(lookup_value = weeks$id,
                           dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% select(id,time),
                           result_column = 2,
                           lookup_column = 1)

#time to snap
weeks$timeToSnap <- ifelse(weeks$time<=weeks$snap_time,weeks$snap_time-weeks$time,NA)

#frame of snap
weeks$snap_frame <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% select(id,frameId),
                            result_column = 2,
                            lookup_column = 1)

#1 second after snap
weeks$snap_plus1 <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% mutate(t1=time+1) %>% select(id,t1),
                            result_column = 2,
                            lookup_column = 1)

#1.5 seconds after snap
weeks$snap_plus1.5 <- vlookup(lookup_value = weeks$id,
                              dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% mutate(t1.5=time+1.5) %>% select(id,t1.5),
                              result_column = 2,
                              lookup_column = 1)

#2 seconds after snap
weeks$snap_plus2 <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% mutate(t2=time+2) %>% select(id,t2),
                            result_column = 2,
                            lookup_column = 1)

#2.5 seconds after snap
weeks$snap_plus2.5 <- vlookup(lookup_value = weeks$id,
                              dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% mutate(t2.5=time+2.5) %>% select(id,t2.5),
                              result_column = 2,
                              lookup_column = 1)

#3 seconds after snap
weeks$snap_plus3 <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(is.na(nflId)==T & event=='ball_snap') %>% mutate(t3=time+3) %>% select(id,t3),
                            result_column = 2,
                            lookup_column = 1)



weeks$snap1 <- ifelse(weeks$snap_plus1==weeks$time,1,0)
weeks$snap1.5 <- ifelse(weeks$snap_plus1.5==weeks$time,1,0)
weeks$snap2 <- ifelse(weeks$snap_plus2==weeks$time,1,0)
weeks$snap2.5 <- ifelse(weeks$snap_plus2.5==weeks$time,1,0)
weeks$snap3 <- ifelse(weeks$snap_plus3==weeks$time,1,0)


#create column in "weeks" that shows player "role"
weeks$role <- vlookup(lookup_value = weeks$id_all,
                      dict = pffScoutingData %>% select(id_all,pff_role),
                      result_column = 2,
                      lookup_column = 1)


#create column in "weeks" that shows pass pro qty
weeks$passProQty <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% group_by(id) %>% filter(event=='ball_snap' & role=='Pass Block') %>% count(role) %>% select(id,n),
                            result_column = 2,
                            lookup_column = 1)

# ONLY ANALYZE 5 AND 6 MAN PASS PRO
weeks <- weeks %>% filter(passProQty %in% c(5,6))


# total pass rush quantity on each play

weeks$passRushQty <-
  vlookup(lookup_value = weeks$id,
          dict = weeks %>% 
            filter(event=='ball_snap') %>% 
            mutate(rush=ifelse(role=='Pass Rush',1,0)) %>%
            group_by(id) %>%
            summarize(sum(rush,na.rm=T)),
          result_column = 2,
          lookup_column = 1)


# ONLY ANALYZE PLAYS WITH 5 OFFENSIVE LINEMEN
plays_without_5_OL <- weeks %>% 
  filter(position %in% c('LT','LG','C','RG','RT') & event=='ball_snap') %>% 
  group_by(id) %>% 
  mutate(count=max(row_number())) %>% 
  filter(count<5) %>% 
  ungroup %>% 
  select(id) %>%
  unname %>%
  unlist

weeks <- weeks %>% filter(!id %in% plays_without_5_OL)

plays_without_5_OL <- NULL


#create column in "weeks" that shows the offensive formation, not including offensive line or QB
weeks$formation <- vlookup(lookup_value = weeks$id_frame,
                           dict = weeks %>% filter(off_def=='O' & !position %in% c('QB','LT','LG','C','RG','RT')) %>% group_by(id_frame) %>% arrange(position) %>% summarize(paste(position,collapse=',')),
                           result_column = 2,
                           lookup_column = 1)







# DO NOT ANALYZE PLAYS WITH 2 BACKS OR 2 TEs
weeks$RBqty <- str_count(weeks$formation,'HB-L|FB-L|HB-R|FB-R')
weeks$TEqty <- str_count(weeks$formation,'TE-L|TE-R|TE-iL|TE-iR|TE-oL|TE-oR')

weeks <- weeks %>% filter(RBqty<=1 & TEqty<=1)



#create column for number of TEs
weeks$TEs_left <- vlookup(lookup_value = weeks$id,
                          dict = pffScoutingData %>% 
                            group_by(id) %>% 
                            summarize(pos=paste(pff_positionLinedUp,collapse=',')) %>% 
                            mutate(TEs=str_count(pos,'TE-.{0,1}L')) %>% 
                            select(id,TEs,pos),
                          result_column = 2,
                          lookup_column = 1)

weeks$TEs_right <- vlookup(lookup_value = weeks$id,
                           dict = pffScoutingData %>% 
                             group_by(id) %>% 
                             summarize(pos=paste(pff_positionLinedUp,collapse=',')) %>% 
                             mutate(TEs=str_count(pos,'TE-.{0,1}R')) %>% 
                             select(id,TEs,pos),
                           result_column = 2,
                           lookup_column = 1)




#create column for number of backs
weeks$RBs_left <- vlookup(lookup_value = weeks$id,
                          dict = pffScoutingData %>% 
                            group_by(id) %>% 
                            summarize(pos=paste(pff_positionLinedUp,collapse=',')) %>% 
                            mutate(RBs=str_count(pos,'HB-.{0,1}L|FB-.{0,1}L')) %>% 
                            select(id,RBs,pos),
                          result_column = 2,
                          lookup_column = 1)

weeks$RBs_right <- vlookup(lookup_value = weeks$id,
                           dict = pffScoutingData %>% 
                             group_by(id) %>% 
                             summarize(pos=paste(pff_positionLinedUp,collapse=',')) %>% 
                             mutate(RBs=str_count(pos,'HB-.{0,1}R|FB-.{0,1}R')) %>% 
                             select(id,RBs,pos),
                           result_column = 2,
                           lookup_column = 1)


#create column for number of WRs
weeks$WRs_left <- vlookup(lookup_value = weeks$id,
                          dict = pffScoutingData %>% 
                            group_by(id) %>% 
                            summarize(pos=paste(pff_positionLinedUp,collapse=',')) %>% 
                            mutate(RBs=str_count(pos,'L.{0,1}WR')) %>% 
                            select(id,RBs,pos),
                          result_column = 2,
                          lookup_column = 1)

weeks$WRs_right <- vlookup(lookup_value = weeks$id,
                           dict = pffScoutingData %>% 
                             group_by(id) %>% 
                             summarize(pos=paste(pff_positionLinedUp,collapse=',')) %>% 
                             mutate(RBs=str_count(pos,'R.{0,1}WR')) %>% 
                             select(id,RBs,pos),
                           result_column = 2,
                           lookup_column = 1)




#create column in "weeks" that shows the center location, in the left/right direction
weeks$centerlocation <- vlookup(lookup_value = weeks$id,
                                dict = weeks %>% filter(position=='C') %>% select(id,y),
                                result_column = 2,
                                lookup_column = 1)

#create column in "weeks" that shows the LG location, in the left/right direction
weeks$LGlocation <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(position=='LG') %>% select(id,y),
                            result_column = 2,
                            lookup_column = 1)

#create column in "weeks" that shows the RG location, in the left/right direction
weeks$RGlocation <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(position=='RG') %>% select(id,y),
                            result_column = 2,
                            lookup_column = 1)

#create column in "weeks" that shows the LT location, in the left/right direction
weeks$LTlocation <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(position=='LT') %>% select(id,y),
                            result_column = 2,
                            lookup_column = 1)

#create column in "weeks" that shows the RT location, in the left/right direction
weeks$RTlocation <- vlookup(lookup_value = weeks$id,
                            dict = weeks %>% filter(position=='RT') %>% select(id,y),
                            result_column = 2,
                            lookup_column = 1)

#create column in "weeks" that shows the left TE location, in the left/right direction
weeks$LTElocation <- vlookup(lookup_value = weeks$id,
                             dict = weeks %>% filter(position=='TE-L') %>% select(id,y),
                             result_column = 2,
                             lookup_column = 1)
weeks$LTElocation <- ifelse(is.na(weeks$LTElocation)==T,weeks$LTlocation,weeks$LTElocation)

#create column in "weeks" that shows the right TE location, in the left/right direction
weeks$RTElocation <- vlookup(lookup_value = weeks$id,
                             dict = weeks %>% filter(position=='TE-R') %>% select(id,y),
                             result_column = 2,
                             lookup_column = 1)
weeks$RTElocation <- ifelse(is.na(weeks$RTElocation)==T,weeks$RTlocation,weeks$RTElocation)



#create column in "weeks" that shows the RB location at the snap
weeks$RBlocation_snap <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(str_detect(position,'HB-L|FB-L|HB-R|FB-R') & event=='ball_snap') %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the RB location at 1 second after the snap
weeks$RBlocation_1sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(str_detect(position,'HB-L|FB-L|HB-R|FB-R') & snap1==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the RB location at 2 seconds after the snap
weeks$RBlocation_2sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(str_detect(position,'HB-L|FB-L|HB-R|FB-R') & snap2==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)





#create column in "weeks" that shows the LT location at the snap
weeks$LTlocation_snap <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='LT' & event=='ball_snap') %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the LT location at 1 second after the snap
weeks$LTlocation_1sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='LT' & snap1==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the LT location at 1.5 seconds after the snap
weeks$LTlocation_1.5sec <- vlookup(lookup_value = weeks$id,
                                   dict = weeks %>% filter(position=='LT' & snap1.5==1) %>% select(id,y),
                                   result_column = 2,
                                   lookup_column = 1)

#create column in "weeks" that shows the LT location at 2 seconds after the snap, for comparison
weeks$LTlocation_2sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='LT' & snap2==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)




#create column in "weeks" that shows the RT location at the snap
weeks$RTlocation_snap <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='RT' & event=='ball_snap') %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the RT location at 1 second after the snap
weeks$RTlocation_1sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='RT' & snap1==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the RT location at 1.5 seconds after the snap
weeks$RTlocation_1.5sec <- vlookup(lookup_value = weeks$id,
                                   dict = weeks %>% filter(position=='RT' & snap1.5==1) %>% select(id,y),
                                   result_column = 2,
                                   lookup_column = 1)

#create column in "weeks" that shows the RT location at 2 seconds after the snap
weeks$RTlocation_2sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='RT' & snap2==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)






#create column in "weeks" that shows the left TE location at the snap
weeks$LTElocation_snap <- vlookup(lookup_value = weeks$id,
                                  dict = weeks %>% filter(position=='TE-L' & event=='ball_snap') %>% select(id,y),
                                  result_column = 2,
                                  lookup_column = 1)
weeks$LTElocation_snap <- ifelse(is.na(weeks$LTElocation_snap)==T,weeks$LTlocation_snap,weeks$LTElocation_snap)

#create column in "weeks" that shows the left TE location after 1 sec
weeks$LTElocation_1sec <- vlookup(lookup_value = weeks$id,
                                  dict = weeks %>% filter(position=='TE-L' & snap1==1 & role=='Pass Block') %>% select(id,y),
                                  result_column = 2,
                                  lookup_column = 1)
weeks$LTElocation_1sec <- ifelse(is.na(weeks$LTElocation_1sec)==T,weeks$LTlocation_1sec,weeks$LTElocation_1sec)



#create column in "weeks" that shows the right TE location at the snap
weeks$RTElocation_snap <- vlookup(lookup_value = weeks$id,
                                  dict = weeks %>% filter(position=='TE-R' & event=='ball_snap') %>% select(id,y),
                                  result_column = 2,
                                  lookup_column = 1)
weeks$RTElocation_snap <- ifelse(is.na(weeks$RTElocation_snap)==T,weeks$RTlocation_snap,weeks$RTElocation_snap)

#create column in "weeks" that shows the right TE location after 1 sec
weeks$RTElocation_1sec <- vlookup(lookup_value = weeks$id,
                                  dict = weeks %>% filter(position=='TE-R' & snap1==1 & role=='Pass Block') %>% select(id,y),
                                  result_column = 2,
                                  lookup_column = 1)
weeks$RTElocation_1sec <- ifelse(is.na(weeks$RTElocation_1sec)==T,weeks$RTlocation_1sec,weeks$RTElocation_1sec)












#create column in "weeks" that shows the center location at the snap, in the left/right direction
weeks$centerlocation_snap <- vlookup(lookup_value = weeks$id,
                                     dict = weeks %>% filter(position=='C' & event=='ball_snap') %>% select(id,y),
                                     result_column = 2,
                                     lookup_column = 1)


#create column in "weeks" that shows the center location at 1 second after the snap
weeks$centerlocation_1sec <- vlookup(lookup_value = weeks$id,
                                     dict = weeks %>% filter(position=='C' & snap1==1) %>% select(id,y),
                                     result_column = 2,
                                     lookup_column = 1)

#create column in "weeks" that shows the center location at 1.5 second after the snap
weeks$centerlocation_1.5sec <- vlookup(lookup_value = weeks$id,
                                       dict = weeks %>% filter(position=='C' & snap1.5==1) %>% select(id,y),
                                       result_column = 2,
                                       lookup_column = 1)


#create column in "weeks" that shows the center location at 2 second after the snap
weeks$centerlocation_2sec <- vlookup(lookup_value = weeks$id,
                                     dict = weeks %>% filter(position=='C' & snap2==1) %>% select(id,y),
                                     result_column = 2,
                                     lookup_column = 1)






#create column in "weeks" that shows the QB location at the snap, in the left/right direction
weeks$QBlocation_snap <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='QB' & event=='ball_snap') %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)


#create column in "weeks" that shows the QB location at 1 second after the snap
weeks$QBlocation_1sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='QB' & snap1==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)


#create column in "weeks" that shows the QB location at 2 seconds after the snap
weeks$QBlocation_2sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='QB' & snap2==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)


#create column in "weeks" that shows the QB location at 3 seconds after the snap
weeks$QBlocation_3sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='QB' & snap3==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)








#create column in "weeks" that shows the RG location at the snap, in the left/right direction
weeks$RGlocation_snap <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='RG' & event=='ball_snap') %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)


#create column in "weeks" that shows the RG location at 2 seconds after the snap
weeks$RGlocation_1sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='RG' & snap1==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the RG location at 1.5 seconds after the snap
weeks$RGlocation_1.5sec <- vlookup(lookup_value = weeks$id,
                                   dict = weeks %>% filter(position=='RG' & snap1.5==1) %>% select(id,y),
                                   result_column = 2,
                                   lookup_column = 1)





#create column in "weeks" that shows the LG location at the snap, in the left/right direction
weeks$LGlocation_snap <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='LG' & event=='ball_snap') %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)


#create column in "weeks" that shows the LG location at 1 seconds after the snap
weeks$LGlocation_1sec <- vlookup(lookup_value = weeks$id,
                                 dict = weeks %>% filter(position=='LG' & snap1==1) %>% select(id,y),
                                 result_column = 2,
                                 lookup_column = 1)

#create column in "weeks" that shows the LG location at 1.5 seconds after the snap
weeks$LGlocation_1.5sec <- vlookup(lookup_value = weeks$id,
                                   dict = weeks %>% filter(position=='LG' & snap1.5==1) %>% select(id,y),
                                   result_column = 2,
                                   lookup_column = 1)







# create column for CONDENSED FORMATIONS (Condensed = WR lined up within 5 yards of the EMOL (tackle or TE) on his side)


weeks$isCondensed_left <- ifelse(weeks$playDirection=='right' &
                                   weeks$position %in% c('LWR','SLWR','SLiWR','SLoWR') & 
                                   ((weeks$y>=weeks$LTlocation_snap+5 & 
                                       weeks$y>=weeks$LTElocation_snap+5) | 
                                      (weeks$y<=weeks$RTlocation_snap-5 & 
                                         weeks$y<=weeks$RTElocation_snap-5)),
                                 0,
                                 ifelse(weeks$playDirection=='left' &
                                          weeks$position %in% c('LWR','SLWR','SLiWR','SLoWR') & 
                                          ((weeks$y<=weeks$LTlocation_snap-5 & 
                                              weeks$y<=weeks$LTElocation_snap-5) | 
                                             (weeks$y>=weeks$RTlocation_snap+5 & 
                                                weeks$y>=weeks$RTElocation_snap+5)),
                                        0,
                                        ifelse(!weeks$position %in% c('LWR','SLWR','SLiWR','SLoWR'),
                                               0,1)
                                 ))


weeks$isCondensed_right <- ifelse(weeks$playDirection=='right' &
                                    weeks$position %in% c('RWR','SRWR','SRiWR','SRoWR') & 
                                    ((weeks$y>=weeks$LTlocation_snap+5 & 
                                        weeks$y>=weeks$LTElocation_snap+5) | 
                                       (weeks$y<=weeks$RTlocation_snap-5 & 
                                          weeks$y<=weeks$RTElocation_snap-5)),
                                  0,
                                  ifelse(weeks$playDirection=='left' &
                                           weeks$position %in% c('RWR','SRWR','SRiWR','SRoWR') & 
                                           ((weeks$y<=weeks$LTlocation_snap-5 & 
                                               weeks$y<=weeks$LTElocation_snap-5) | 
                                              (weeks$y>=weeks$RTlocation_snap+5 & 
                                                 weeks$y>=weeks$RTElocation_snap+5)),
                                         0,
                                         ifelse(!weeks$position %in% c('RWR','SRWR','SRiWR','SRoWR'),
                                                0,1)
                                  ))

weeks$isCondensed <- weeks$isCondensed_left + weeks$isCondensed_right




# DO NOT INCLUDE TE or offensive linemen that cross the center after the snap

off_cross <- 
  rbind(
    weeks %>% 
      filter(playDirection=='right' & position %in% c('TE-L','TE-iL','TE-oL','LG','LT','SLWR','SLiWR','LWR') & snap2==1) %>% 
      group_by(id) %>% 
      mutate(location_at_2sec=ifelse(y<=centerlocation_2sec & x<=LOS,'cross','')) %>% 
      ungroup() %>% 
      filter(location_at_2sec=='cross') %>% 
      select(id)
    ,
    weeks %>% 
      filter(playDirection=='right' & position %in% c('TE-R','TE-iR','TE-oR','RG','RT','SRWR','SRiWR','RWR') & snap2==1) %>% 
      group_by(id) %>% 
      mutate(location_at_2sec=ifelse(y>=centerlocation_2sec & x<=LOS,'cross','')) %>% 
      ungroup() %>% 
      filter(location_at_2sec=='cross') %>% 
      select(id)
    ,
    weeks %>% 
      filter(playDirection=='left' & position %in% c('TE-L','TE-iL','TE-oL','LG','LT','SLWR','SLiWR','LWR') & snap2==1) %>% 
      group_by(id) %>% 
      mutate(location_at_2sec=ifelse(y>=centerlocation_2sec & x>=LOS,'cross','')) %>% 
      ungroup() %>% 
      filter(location_at_2sec=='cross') %>% 
      select(id)
    ,
    weeks %>% 
      filter(playDirection=='left' & position %in% c('TE-R','TE-iR','TE-oR','RG','RT','SRWR','SRiWR','RWR') & snap2==1) %>% 
      group_by(id) %>% 
      mutate(location_at_2sec=ifelse(y<=centerlocation_2sec & x>=LOS,'cross','')) %>% 
      ungroup() %>% 
      filter(location_at_2sec=='cross') %>% 
      select(id)
  ) %>% unname %>% unlist


weeks <- weeks %>% filter(!id %in% off_cross)
off_cross <- NULL


# do not include TE pass protection
TE_pass_pro_plays <- weeks %>% filter(str_detect(position,'TE')==T & role=='Pass Block') %>% select(id) %>% unname %>% unlist
weeks <- weeks %>% filter(!id %in% TE_pass_pro_plays)


# determine pass strength
pass_strength <- 
  weeks %>% 
  filter(off_def=='O' & !position %in% c('LT','LG','C','RG','RT','QB')) %>%
  group_by(id) %>% 
  summarize(pos_list=paste(position,collapse=',')) %>% 
  mutate(WR_left=str_count(pos_list,'LWR|SLWR|SLiWR|SLoWR'),
         WR_right=str_count(pos_list,'RWR|SRWR|SRiWR|SRoWR'),
         TE_left=str_count(pos_list,'TE-L|TE-iL|TE-oL'),
         TE_right=str_count(pos_list,'TE-R|TE-iR|TE-oR'),
         RB_left=str_count(pos_list,'HB-L|FB-L|FB-iL|FB-oL'),
         RB_right=str_count(pos_list,'HB-R|FB-R|FB-iR|FB-oR')
  ) %>%
  mutate(strength=ifelse(WR_left>WR_right,'left',
                         ifelse(WR_left<WR_right,'right',
                                ifelse(WR_left==WR_right & TE_left>TE_right,'left',
                                       ifelse(WR_left==WR_right & TE_left<TE_right,'right','balanced'))))) %>% 
  select(id,strength)


weeks$passStrength <- vlookup(lookup_value = weeks$id,
                              dict = pass_strength,
                              result_column = 2,
                              lookup_column = 1)

pass_strength <- NULL




# determine offensive linemen turns
# example: center turn is LEFT if center location at 1 second and 1.5 seconds is to the LEFT of his location at the snap
# also, there is 1/4 yard buffer for the center, only.
# Meaning, if the center stays within the 1/2 yard window (1/4 yard to the left and 1/4 yard to the right), 
# there is no center turn recorded

weeks$centerTurn <- ifelse(weeks$playDirection=='left' & weeks$centerlocation_1sec<(weeks$ball-0.25) & weeks$centerlocation_1.5sec<(weeks$ball-0.25),'left',
                           ifelse(weeks$playDirection=='left' & weeks$centerlocation_1sec>(weeks$ball+0.25) & weeks$centerlocation_1.5sec>(weeks$ball+0.25),'right',
                                  ifelse(weeks$playDirection=='right' & weeks$centerlocation_1sec<(weeks$ball-0.25) & weeks$centerlocation_1.5sec<(weeks$ball-0.25),'right',
                                         ifelse(weeks$playDirection=='right' & weeks$centerlocation_1sec>(weeks$ball+0.25) & weeks$centerlocation_1.5sec>(weeks$ball+0.25),'left','na'))))
weeks$centerTurn <- ifelse(is.na(weeks$centerTurn) | weeks$centerTurn=='na','na',weeks$centerTurn)



weeks$RGturn <- ifelse(weeks$playDirection=='left' & weeks$RGlocation_1sec<weeks$RGlocation_snap & weeks$RGlocation_1.5sec<weeks$RGlocation_snap,'left',
                       ifelse(weeks$playDirection=='left' & weeks$RGlocation_1sec>weeks$RGlocation_snap & weeks$RGlocation_1.5sec>weeks$RGlocation_snap,'right',
                              ifelse(weeks$playDirection=='right' & weeks$RGlocation_1sec<weeks$RGlocation_snap & weeks$RGlocation_1.5sec<weeks$RGlocation_snap,'right',
                                     ifelse(weeks$playDirection=='right' & weeks$RGlocation_1sec>weeks$RGlocation_snap & weeks$RGlocation_1.5sec>weeks$RGlocation_snap,'left','na'))))
weeks$RGturn <- ifelse(is.na(weeks$RGturn) | weeks$RGturn=='na','na',weeks$RGturn)




weeks$LGturn <- ifelse(weeks$playDirection=='left' & weeks$LGlocation_1sec<weeks$LGlocation_snap & weeks$LGlocation_1.5sec<weeks$LGlocation_snap,'left',
                       ifelse(weeks$playDirection=='left' & weeks$LGlocation_1sec>weeks$LGlocation_snap & weeks$LGlocation_1.5sec>weeks$LGlocation_snap,'right',
                              ifelse(weeks$playDirection=='right' & weeks$LGlocation_1sec<weeks$LGlocation_snap & weeks$LGlocation_1.5sec<weeks$LGlocation_snap,'right',
                                     ifelse(weeks$playDirection=='right' & weeks$LGlocation_1sec>weeks$LGlocation_snap & weeks$LGlocation_1.5sec>weeks$LGlocation_snap,'left','na'))))
weeks$LGturn <- ifelse(is.na(weeks$LGturn) | weeks$LGturn=='na','na',weeks$LGturn)




weeks$RTturn <- ifelse(weeks$playDirection=='left' & weeks$RTlocation_1sec<weeks$RTlocation_snap & weeks$RTlocation_1.5sec<weeks$RTlocation_snap,'left',
                       ifelse(weeks$playDirection=='left' & weeks$RTlocation_1sec>weeks$RTlocation_snap & weeks$RTlocation_1.5sec>weeks$RTlocation_snap,'right',
                              ifelse(weeks$playDirection=='right' & weeks$RTlocation_1sec<weeks$RTlocation_snap & weeks$RTlocation_1.5sec<weeks$RTlocation_snap,'right',
                                     ifelse(weeks$playDirection=='right' & weeks$RTlocation_1sec>weeks$RTlocation_snap & weeks$RTlocation_1.5sec>weeks$RTlocation_snap,'left','na'))))
weeks$RTturn <- ifelse(is.na(weeks$RTturn) | weeks$RTturn=='na','na',weeks$RTturn)




weeks$LTturn <- ifelse(weeks$playDirection=='left' & weeks$LTlocation_1sec<weeks$LTlocation_snap & weeks$LTlocation_1.5sec<weeks$LTlocation_snap,'left',
                       ifelse(weeks$playDirection=='left' & weeks$LTlocation_1sec>weeks$LTlocation_snap & weeks$LTlocation_1.5sec>weeks$LTlocation_snap,'right',
                              ifelse(weeks$playDirection=='right' & weeks$LTlocation_1sec<weeks$LTlocation_snap & weeks$LTlocation_1.5sec<weeks$LTlocation_snap,'right',
                                     ifelse(weeks$playDirection=='right' & weeks$LTlocation_1sec>weeks$LTlocation_snap & weeks$LTlocation_1.5sec>weeks$LTlocation_snap,'left','na'))))
weeks$LTturn <- ifelse(is.na(weeks$LTturn) | weeks$LTturn=='na','na',weeks$LTturn)



# only include clear turns by the center

weeks <- weeks %>% filter(centerTurn!='na')


# sprintout detection

weeks$QB_sprint <- ifelse(weeks$playDirection=='left' & 
                            weeks$QBlocation_2sec<weeks$LGlocation_snap & 
                            weeks$QBlocation_3sec<weeks$LTlocation_snap & 
                            weeks$centerTurn=='left' & 
                            weeks$RGturn=='left' & 
                            weeks$RTturn=='left' & 
                            weeks$LGturn=='left' & 
                            weeks$LTturn == 'left',
                          'left',
                          ifelse(weeks$playDirection=='left' & 
                                   weeks$QBlocation_2sec>weeks$RGlocation_snap & 
                                   weeks$QBlocation_3sec>weeks$RTlocation_snap & 
                                   weeks$centerTurn=='right' & 
                                   weeks$RGturn=='right' & 
                                   weeks$RTturn=='right' & 
                                   weeks$LGturn=='right' & 
                                   weeks$LTturn == 'right',
                                 'right',
                                 ifelse(weeks$playDirection=='right' & 
                                          weeks$QBlocation_2sec<weeks$RGlocation_snap & 
                                          weeks$QBlocation_3sec<weeks$RTlocation_snap & 
                                          weeks$centerTurn=='right' & 
                                          weeks$RGturn=='right' & 
                                          weeks$RTturn=='right' & 
                                          weeks$LGturn=='right' & 
                                          weeks$LTturn == 'right',
                                        'right',
                                        ifelse(weeks$playDirection=='right' & 
                                                 weeks$QBlocation_2sec>weeks$LGlocation_snap & 
                                                 weeks$QBlocation_3sec>weeks$LTlocation_snap & 
                                                 weeks$centerTurn=='left' & 
                                                 weeks$RGturn=='left' & 
                                                 weeks$RTturn=='left' & 
                                                 weeks$LGturn=='left' & 
                                                 weeks$LTturn == 'left',
                                               'left',''))))
weeks$QB_sprint <- ifelse(is.na(weeks$QB_sprint) | weeks$QB_sprint=='','',weeks$QB_sprint)

#remove sprintout
weeks <- weeks %>% filter(QB_sprint=='')
weeks$QB_sprint <- NULL









# all right/left designations in this section are from the offense's perspective

# Goal: identify the gap each defender is lined up in, pre snap
# first, locate each gap


weeks$A_gap_start <- vlookup(lookup_value = weeks$id,
                         dict = weeks %>% filter(position=='C') %>% select(id,y),
                         result_column = 2,
                         lookup_column = 1)

weeks$A_gap_right_end <- vlookup(lookup_value = weeks$id,
                               dict = weeks %>% filter(position=='RG') %>% select(id,y),
                               result_column = 2,
                               lookup_column = 1)

weeks$A_gap_left_end <- vlookup(lookup_value = weeks$id,
                              dict = weeks %>% filter(position=='LG') %>% select(id,y),
                              result_column = 2,
                              lookup_column = 1)

weeks$B_gap_right_start <- weeks$A_gap_right_end
weeks$B_gap_left_start <- weeks$A_gap_left_end

weeks$B_gap_right_end <- vlookup(lookup_value = weeks$id,
                               dict = weeks %>% filter(position=='RT') %>% select(id,y),
                               result_column = 2,
                               lookup_column = 1)

weeks$B_gap_left_end <- vlookup(lookup_value = weeks$id,
                              dict = weeks %>% filter(position=='LT') %>% select(id,y),
                              result_column = 2,
                              lookup_column = 1)

weeks$C_gap_right_start <- weeks$B_gap_right_end
weeks$C_gap_left_start <- weeks$B_gap_left_end

weeks$C_gap_right_end <- ifelse(weeks$TEs_right==1,
                              vlookup(lookup_value = weeks$id,
                                      dict = weeks %>% filter(position=='TE-R') %>% select(id,y),
                                      result_column = 2,
                                      lookup_column = 1),NA)

weeks$C_gap_left_end <- ifelse(weeks$TEs_left==1,
                             vlookup(lookup_value = weeks$id,
                                     dict = weeks %>% filter(position=='TE-L') %>% select(id,y),
                                     result_column = 2,
                                     lookup_column = 1),NA)

# identify defender gap

weeks$defenderGap <- ifelse(weeks$y > weeks$A_gap_start & weeks$y < weeks$A_gap_left_end,'A Gap Left','')
weeks$defenderGap <- ifelse(weeks$y < weeks$A_gap_start & weeks$y > weeks$A_gap_left_end,'A Gap Left',weeks$defenderGap)
weeks$defenderGap <- ifelse(weeks$y == weeks$A_gap_start,'Head Up on Center',weeks$defenderGap)
weeks$defenderGap <- ifelse(weeks$y > weeks$A_gap_start & weeks$y < weeks$A_gap_right_end,'A Gap Right',weeks$defenderGap)
weeks$defenderGap <- ifelse(weeks$y < weeks$A_gap_start & weeks$y > weeks$A_gap_right_end,'A Gap Right',weeks$defenderGap)

weeks$defenderGap <- ifelse(is.na(weeks$B_gap_right_start)==F & is.na(weeks$B_gap_right_end)==F & weeks$y < weeks$B_gap_right_start & weeks$y > weeks$B_gap_right_end,'B Gap Right',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$B_gap_right_start)==F & is.na(weeks$B_gap_right_end)==F & weeks$y > weeks$B_gap_right_start & weeks$y < weeks$B_gap_right_end,'B Gap Right',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$B_gap_right_start)==F & weeks$y == weeks$B_gap_right_start,'Head Up on RG',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$B_gap_left_start)==F & is.na(weeks$B_gap_left_end)==F & weeks$y < weeks$B_gap_left_start & weeks$y > weeks$B_gap_left_end,'B Gap Left',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$B_gap_left_start)==F & is.na(weeks$B_gap_left_end)==F & weeks$y > weeks$B_gap_left_start & weeks$y < weeks$B_gap_left_end,'B Gap Left',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$B_gap_left_start)==F & weeks$y == weeks$B_gap_left_start,'Head Up on LG',weeks$defenderGap)

weeks$defenderGap <- ifelse(is.na(weeks$C_gap_right_start)==F & is.na(weeks$C_gap_right_end)==F & weeks$y < weeks$C_gap_right_start & weeks$y > weeks$C_gap_right_end,'C Gap Right',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$C_gap_right_start)==F & is.na(weeks$C_gap_right_end)==F & weeks$y > weeks$C_gap_right_start & weeks$y < weeks$C_gap_right_end,'C Gap Right',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$C_gap_right_start)==F & weeks$y == weeks$C_gap_right_start,'Head Up on RT',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$C_gap_left_start)==F & is.na(weeks$C_gap_left_end)==F & weeks$y < weeks$C_gap_left_start & weeks$y > weeks$C_gap_left_end,'C Gap Left',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$C_gap_left_start)==F & is.na(weeks$C_gap_left_end)==F & weeks$y > weeks$C_gap_left_start & weeks$y < weeks$C_gap_left_end,'C Gap Left',weeks$defenderGap)
weeks$defenderGap <- ifelse(is.na(weeks$C_gap_left_start)==F & weeks$y == weeks$C_gap_left_start,'Head Up on LT',weeks$defenderGap)

weeks$defenderGap <- ifelse(weeks$defenderGap=='' & weeks$playDirection=='left' & weeks$y<weeks$ball,'Left Open Gap',
                                   ifelse(weeks$defenderGap=='' & weeks$playDirection=='left' & weeks$y>weeks$ball,'Right Open Gap',
                                          ifelse(weeks$defenderGap=='' & weeks$playDirection=='right' & weeks$y<weeks$ball,'Right Open Gap',
                                                 ifelse(weeks$defenderGap=='' & weeks$playDirection=='right' & weeks$y>weeks$ball,'Left Open Gap',
                                                        weeks$defenderGap))))





#find defenders within 6 yards of the LOS and within 5 yards of the outermost core player (TE or tackle) on either side

weeks$inWideBox_right <- ifelse(weeks$time<=weeks$snap_time & 
                                  weeks$playDirection=='right' &
                                  weeks$off_def=='D' & 
                                  weeks$x<=weeks$LOS+6 &
                                  weeks$y<weeks$centerlocation & 
                                  weeks$y>=weeks$RTlocation-5 & 
                                  weeks$y>=weeks$RTElocation-5,
                                1,
                                ifelse(weeks$time<=weeks$snap_time &
                                         weeks$playDirection=='left' &
                                         weeks$off_def=='D' &
                                         weeks$x>=weeks$LOS-6 &
                                         weeks$y>weeks$centerlocation & 
                                         weeks$y<=weeks$RTlocation+5 & 
                                         weeks$y<=weeks$RTElocation+5,
                                       1,
                                       0
                                ))



weeks$inWideBox_left <- ifelse(weeks$time<=weeks$snap_time & 
                                 weeks$playDirection=='right' &
                                 weeks$off_def=='D' & 
                                 weeks$x<=weeks$LOS+6 &
                                 weeks$y<=weeks$LTlocation+5 & 
                                 weeks$y<=weeks$LTElocation+5 & 
                                 weeks$y>weeks$centerlocation,
                               1,
                               ifelse(weeks$time<=weeks$snap_time &
                                        weeks$playDirection=='left' &
                                        weeks$off_def=='D' &
                                        weeks$x>=weeks$LOS-6 &
                                        weeks$y>=weeks$LTlocation-5 & 
                                        weeks$y>=weeks$LTElocation-5 & 
                                        weeks$y<weeks$centerlocation,
                                      1,
                                      0
                               ))




# find defenders located directly over top of the center

weeks$inWideBox_mid <- ifelse(weeks$time<=weeks$snap_time & 
                                weeks$off_def=='D' & 
                                weeks$x<=weeks$LOS+6 &
                                weeks$y==weeks$centerlocation,
                              1,0)

weeks$inWideBox <- weeks$inWideBox_right + weeks$inWideBox_left + weeks$inWideBox_mid


#find defenders in the wide box and inside the core

weeks$inWideBox_inside_right <- ifelse(weeks$time<=weeks$snap_time & 
                                         weeks$playDirection=='right' &
                                         weeks$off_def=='D' & 
                                         weeks$x<=weeks$LOS+6 &
                                         weeks$y<weeks$centerlocation & 
                                         weeks$y>=weeks$RTElocation,
                                       1,
                                       ifelse(weeks$time<=weeks$snap_time &
                                                weeks$playDirection=='left' &
                                                weeks$off_def=='D' &
                                                weeks$x>=weeks$LOS-6 &
                                                weeks$y>weeks$centerlocation & 
                                                weeks$y<=weeks$RTElocation,
                                              1,
                                              0
                                       ))



weeks$inWideBox_inside_left <- ifelse(weeks$time<=weeks$snap_time & 
                                        weeks$playDirection=='right' &
                                        weeks$off_def=='D' & 
                                        weeks$x<=weeks$LOS+6 &
                                        weeks$y<=weeks$LTElocation & 
                                        weeks$y>weeks$centerlocation,
                                      1,
                                      ifelse(weeks$time<=weeks$snap_time &
                                               weeks$playDirection=='left' &
                                               weeks$off_def=='D' &
                                               weeks$x>=weeks$LOS-6 &
                                               weeks$y>=weeks$LTElocation & 
                                               weeks$y<weeks$centerlocation,
                                             1,
                                             0
                                      ))






#find defenders in the wide box and within 5 yards outside the core

weeks$inWideBox_outside_right <- ifelse(weeks$time<=weeks$snap_time & 
                                          weeks$playDirection=='right' &
                                          weeks$off_def=='D' & 
                                          weeks$x<=weeks$LOS+6 &
                                          weeks$y<weeks$RTlocation & 
                                          weeks$y<weeks$RTElocation & 
                                          weeks$y>=weeks$RTlocation-5 & 
                                          weeks$y>=weeks$RTElocation-5,
                                        1,
                                        ifelse(weeks$time<=weeks$snap_time &
                                                 weeks$playDirection=='left' &
                                                 weeks$off_def=='D' &
                                                 weeks$x>=weeks$LOS-6 &
                                                 weeks$y>weeks$RTlocation & 
                                                 weeks$y>weeks$RTElocation & 
                                                 weeks$y<=weeks$RTlocation+5 & 
                                                 weeks$y<=weeks$RTElocation+5,
                                               1,
                                               0
                                        ))



weeks$inWideBox_outside_left <- ifelse(weeks$time<=weeks$snap_time & 
                                         weeks$playDirection=='right' &
                                         weeks$off_def=='D' & 
                                         weeks$x<=weeks$LOS+6 &
                                         weeks$y<=weeks$LTlocation+5 & 
                                         weeks$y<=weeks$LTElocation+5 & 
                                         weeks$y>weeks$LTElocation & 
                                         weeks$y>weeks$LTElocation,
                                       1,
                                       ifelse(weeks$time<=weeks$snap_time &
                                                weeks$playDirection=='left' &
                                                weeks$off_def=='D' &
                                                weeks$x>=weeks$LOS-6 &
                                                weeks$y>=weeks$LTlocation-5 & 
                                                weeks$y>=weeks$LTElocation-5 & 
                                                weeks$y<weeks$LTlocation & 
                                                weeks$y<weeks$LTElocation,
                                              1,
                                              0
                                       ))






#find defenders within 2 yards of the LOS and within 5 yards of the outermost core player on either side

weeks$onLOS_right <- ifelse(weeks$time<=weeks$snap_time & 
                              weeks$playDirection=='right' &
                              weeks$off_def=='D' & 
                              ((weeks$x<=weeks$LOS+2 &
                                  weeks$y>=weeks$RTlocation-5 & 
                                  weeks$y>=weeks$RTElocation-5) | 
                                 weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','LEO','RE','REO')) & 
                              weeks$y<weeks$centerlocation,
                            1,
                            ifelse(weeks$time<=weeks$snap_time &
                                     weeks$playDirection=='left' &
                                     weeks$off_def=='D' &
                                     weeks$x>=weeks$LOS-2 &
                                     weeks$y>weeks$centerlocation & 
                                     weeks$y<=weeks$RTlocation+5 & 
                                     weeks$y<=weeks$RTElocation+5,
                                   1,
                                   0
                            ))



weeks$onLOS_left <- ifelse(weeks$time<=weeks$snap_time & 
                             weeks$playDirection=='right' &
                             weeks$off_def=='D' & 
                             ((weeks$x<=weeks$LOS+2 & 
                                 weeks$y<=weeks$LTlocation+5 & 
                                 weeks$y<=weeks$LTElocation+5) | 
                                weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','LEO','RE','REO')) & 
                             weeks$y>weeks$centerlocation,
                           1,
                           ifelse(weeks$time<=weeks$snap_time &
                                    weeks$playDirection=='left' &
                                    weeks$off_def=='D' &
                                    weeks$x>=weeks$LOS-2 &
                                    weeks$y>=weeks$LTlocation-5 & 
                                    weeks$y>=weeks$LTElocation-5 & 
                                    weeks$y<weeks$centerlocation,
                                  1,
                                  0
                           ))




# find players head up on the center and on the LOS

weeks$onLOS_mid <- ifelse(weeks$time<=weeks$snap_time & 
                            weeks$off_def=='D' & 
                            (weeks$x<=weeks$LOS+2 | 
                               weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','LEO','RE','REO')) & 
                            weeks$y==weeks$centerlocation,
                          1,0)

weeks$onLOS <- weeks$onLOS_right + weeks$onLOS_left + weeks$onLOS_mid








#find defenders within 2 yards of the LOS and lined up in the A gap

weeks$onLOS_Agap_right <- ifelse(weeks$time<=weeks$snap_time & 
                                   weeks$onLOS==1 & 
                                   weeks$defenderGap=='A Gap Right',
                                 1,0)



weeks$onLOS_Agap_left <- ifelse(weeks$time<=weeks$snap_time & 
                                  weeks$onLOS==1 & 
                                  weeks$defenderGap=='A Gap Left',
                                1,0)

#find defensive linemen within 2 yards of the LOS and lined up in the A gap

weeks$onLOS_Agap_right_DL <- ifelse(weeks$time<=weeks$snap_time & 
                                      weeks$onLOS_Agap_right==1 & 
                                      weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO'),
                                    1,0)


weeks$onLOS_Agap_left_DL <- ifelse(weeks$time<=weeks$snap_time & 
                                     weeks$onLOS_Agap_left==1 & 
                                     weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO'),
                                   1,0)

#find linebackers within 2 yards of the LOS and lined up in the A gap

weeks$onLOS_Agap_left_LB <- ifelse(weeks$onLOS_Agap_left==1 & weeks$onLOS_Agap_left_DL==0,1,0)
weeks$onLOS_Agap_right_LB <- ifelse(weeks$onLOS_Agap_right==1 & weeks$onLOS_Agap_right_DL==0,1,0)









#find defenders within 2 yards of the LOS and inside the core

weeks$onLOS_inside_right <- ifelse(weeks$time<=weeks$snap_time & 
                                     weeks$playDirection=='right' &
                                     weeks$off_def=='D' & 
                                     (weeks$x<=weeks$LOS+2 | 
                                        weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                     weeks$y<weeks$centerlocation_snap & 
                                     weeks$y>=weeks$RTElocation_snap,
                                   1,
                                   ifelse(weeks$time<=weeks$snap_time &
                                            weeks$playDirection=='left' &
                                            weeks$off_def=='D' &
                                            (weeks$x>=weeks$LOS-2 | 
                                               weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                            weeks$y>weeks$centerlocation_snap & 
                                            weeks$y<=weeks$RTElocation_snap,
                                          1,
                                          0
                                   ))



weeks$onLOS_inside_left <- ifelse(weeks$time<=weeks$snap_time & 
                                    weeks$playDirection=='right' &
                                    weeks$off_def=='D' & 
                                    (weeks$x<=weeks$LOS+2 | 
                                       weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                    weeks$y<=weeks$LTElocation_snap & 
                                    weeks$y>weeks$centerlocation_snap,
                                  1,
                                  ifelse(weeks$time<=weeks$snap_time &
                                           weeks$playDirection=='left' &
                                           weeks$off_def=='D' &
                                           (weeks$x>=weeks$LOS-2 | 
                                              weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                           weeks$y>=weeks$LTElocation_snap & 
                                           weeks$y<weeks$centerlocation_snap,
                                         1,
                                         0
                                  ))






#find defenders within 2 yards of the LOS and within 5 yards outside the core

weeks$onLOS_outside_right <- ifelse(weeks$time<=weeks$snap_time & 
                                      weeks$playDirection=='right' &
                                      weeks$off_def=='D' & 
                                      ((weeks$x<=weeks$LOS+2 & 
                                          weeks$y>=weeks$RTlocation_snap-5 & 
                                          weeks$y>=weeks$RTElocation_snap-5) | 
                                         weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                      weeks$y<weeks$RTlocation_snap & 
                                      weeks$y<weeks$RTElocation_snap,
                                    1,
                                    ifelse(weeks$time<=weeks$snap_time &
                                             weeks$playDirection=='left' &
                                             weeks$off_def=='D' &
                                             ((weeks$x>=weeks$LOS-2 & 
                                                 weeks$y<=weeks$RTlocation_snap+5 & 
                                                 weeks$y<=weeks$RTElocation_snap+5) | 
                                                weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                             weeks$y>weeks$RTlocation_snap & 
                                             weeks$y>weeks$RTElocation_snap,
                                           1,
                                           0
                                    ))



weeks$onLOS_outside_left <- ifelse(weeks$time<=weeks$snap_time & 
                                     weeks$playDirection=='right' &
                                     weeks$off_def=='D' & 
                                     ((weeks$x<=weeks$LOS+2 &
                                         weeks$y<=weeks$LTlocation_snap+5 & 
                                         weeks$y<=weeks$LTElocation_snap+5) | 
                                        weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                     weeks$y>weeks$LTElocation_snap & 
                                     weeks$y>weeks$LTElocation_snap,
                                   1,
                                   ifelse(weeks$time<=weeks$snap_time &
                                            weeks$playDirection=='left' &
                                            weeks$off_def=='D' &
                                            ((weeks$x>=weeks$LOS-2 &
                                                weeks$y>=weeks$LTlocation_snap-5 & 
                                                weeks$y>=weeks$LTElocation_snap-5) | 
                                               weeks$position %in% c('DRT','DLT','NT','NLT','NRT','LE','RE','LEO','REO')) &
                                            weeks$y<weeks$LTlocation_snap & 
                                            weeks$y<weeks$LTElocation_snap,
                                          1,
                                          0
                                   ))










#find defender location in the onLOS feature after 1 second

weeks$onLOS_right_1sec <- ifelse(weeks$snap1==1 & 
                                   weeks$playDirection=='right' &
                                   weeks$off_def=='D' & 
                                   weeks$x<=weeks$LOS+2 &
                                   weeks$y<weeks$centerlocation_1sec & 
                                   weeks$y>=weeks$RTlocation_1sec-5 & 
                                   weeks$y>=weeks$RTElocation_1sec-5,
                                 1,
                                 ifelse(weeks$snap1==1 &
                                          weeks$playDirection=='left' &
                                          weeks$off_def=='D' &
                                          weeks$x>=weeks$LOS-2 &
                                          weeks$y>weeks$centerlocation_1sec & 
                                          weeks$y<=weeks$RTlocation_1sec+5 & 
                                          weeks$y<=weeks$RTElocation_1sec+5,
                                        1,
                                        0
                                 ))




#create feature for even or odd front
weeks$front <- vlookup(lookup_value = weeks$id_frame,
                       dict = weeks %>% 
                         group_by(id_frame) %>% 
                         summarize(pos=position,collapse=',') %>% 
                         mutate(down=ifelse(str_count(pos,'LE|RE|LEO|REO|DLT|DRT|NT|NLT|NRT')==3,'Odd',
                                                   ifelse(str_count(pos,'LE|RE|LEO|REO|DLT|DRT|NT|NLT|NRT')==4,'Even',''))) %>% 
                         select(id_frame,down),
                       result_column = 2,
                       lookup_column = 1)





#Goal: identify defender gap after 1 second, post-snap

#first, locate the gaps

weeks$A_gap_1_1sec <- vlookup(lookup_value = weeks$id,
                              dict = weeks %>% filter(snap1==1 & role=='Pass Block' & position=='C') %>% select(id,y),
                              result_column = 2,
                              lookup_column = 1)

weeks$A_gap_right_2_1sec <- vlookup(lookup_value = weeks$id,
                                    dict = weeks %>% filter(snap1==1 & role=='Pass Block' & position=='RG') %>% select(id,y),
                                    result_column = 2,
                                    lookup_column = 1)

weeks$A_gap_left_2_1sec <- vlookup(lookup_value = weeks$id,
                                   dict = weeks %>% filter(snap1==1 & role=='Pass Block' & position=='LG') %>% select(id,y),
                                   result_column = 2,
                                   lookup_column = 1)

weeks$B_gap_right_1_1sec <- weeks$A_gap_right_2_1sec
weeks$B_gap_left_1_1sec <- weeks$A_gap_left_2_1sec

weeks$B_gap_right_2_1sec <- vlookup(lookup_value = weeks$id,
                                    dict = weeks %>% filter(snap1==1 & role=='Pass Block' & position=='RT') %>% select(id,y),
                                    result_column = 2,
                                    lookup_column = 1)

weeks$B_gap_left_2_1sec <- vlookup(lookup_value = weeks$id,
                                   dict = weeks %>% filter(snap1==1 & role=='Pass Block' & position=='LT') %>% select(id,y),
                                   result_column = 2,
                                   lookup_column = 1)

weeks$C_gap_right_1_1sec <- weeks$B_gap_right_2_1sec
weeks$C_gap_left_1_1sec <- weeks$B_gap_left_2_1sec

weeks$C_gap_right_2_1sec <- ifelse(weeks$TEs_right==1,
                                   vlookup(lookup_value = weeks$id,
                                           dict = weeks %>% filter(snap1==1 & position=='TE-R' & role=='Pass Block') %>% select(id,y),
                                           result_column = 2,
                                           lookup_column = 1),NA)

weeks$C_gap_left_2_1sec <- ifelse(weeks$TEs_left==1,
                                  vlookup(lookup_value = weeks$id,
                                          dict = weeks %>% filter(snap1==1 & position=='TE-L' & role=='Pass Block') %>% select(id,y),
                                          result_column = 2,
                                          lookup_column = 1),NA)



weeks$y_1sec <- vlookup(lookup_value = weeks$id_all,
                        dict = weeks %>% filter(snap1==1) %>% select(id_all,y),
                        result_column = 2,
                        lookup_column = 1)

# find defender gap after 1 second, post-snap

weeks$defenderGap_1_sec <- ifelse(weeks$y_1sec > weeks$A_gap_1_1sec & weeks$y_1sec < weeks$A_gap_left_2_1sec,'A Gap Left','')
weeks$defenderGap_1_sec <- ifelse(weeks$y_1sec < weeks$A_gap_1_1sec & weeks$y_1sec > weeks$A_gap_left_2_1sec,'A Gap Left',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(weeks$y_1sec == weeks$A_gap_1_1sec,'Head Up on Center',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(weeks$y_1sec > weeks$A_gap_1_1sec & weeks$y_1sec < weeks$A_gap_right_2_1sec,'A Gap Right',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(weeks$y_1sec < weeks$A_gap_1_1sec & weeks$y_1sec > weeks$A_gap_right_2_1sec,'A Gap Right',weeks$defenderGap_1_sec)

weeks$defenderGap_1_sec <- ifelse(is.na(weeks$B_gap_right_1_1sec)==F & is.na(weeks$B_gap_right_2_1sec)==F & weeks$y_1sec < weeks$B_gap_right_1_1sec & weeks$y_1sec > weeks$B_gap_right_2_1sec,'B Gap Right',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$B_gap_right_1_1sec)==F & is.na(weeks$B_gap_right_2_1sec)==F & weeks$y_1sec > weeks$B_gap_right_1_1sec & weeks$y_1sec < weeks$B_gap_right_2_1sec,'B Gap Right',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$B_gap_right_1_1sec)==F & weeks$y_1sec == weeks$B_gap_right_1_1sec,'Head Up on RG',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$B_gap_left_1_1sec)==F & is.na(weeks$B_gap_left_2_1sec)==F & weeks$y_1sec < weeks$B_gap_left_1_1sec & weeks$y_1sec > weeks$B_gap_left_2_1sec,'B Gap Left',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$B_gap_left_1_1sec)==F & is.na(weeks$B_gap_left_2_1sec)==F & weeks$y_1sec > weeks$B_gap_left_1_1sec & weeks$y_1sec < weeks$B_gap_left_2_1sec,'B Gap Left',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$B_gap_left_1_1sec)==F & weeks$y_1sec == weeks$B_gap_left_1_1sec,'Head Up on LG',weeks$defenderGap_1_sec)

weeks$defenderGap_1_sec <- ifelse(is.na(weeks$C_gap_right_1_1sec)==F & is.na(weeks$C_gap_right_2_1sec)==F & weeks$y_1sec < weeks$C_gap_right_1_1sec & weeks$y_1sec > weeks$C_gap_right_2_1sec,'C Gap Right',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$C_gap_right_1_1sec)==F & is.na(weeks$C_gap_right_2_1sec)==F & weeks$y_1sec > weeks$C_gap_right_1_1sec & weeks$y_1sec < weeks$C_gap_right_2_1sec,'C Gap Right',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$C_gap_right_1_1sec)==F & weeks$y_1sec == weeks$C_gap_right_1_1sec,'Head Up on RT',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$C_gap_left_1_1sec)==F & is.na(weeks$C_gap_left_2_1sec)==F & weeks$y_1sec < weeks$C_gap_left_1_1sec & weeks$y_1sec > weeks$C_gap_left_2_1sec,'C Gap Left',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$C_gap_left_1_1sec)==F & is.na(weeks$C_gap_left_2_1sec)==F & weeks$y_1sec > weeks$C_gap_left_1_1sec & weeks$y_1sec < weeks$C_gap_left_2_1sec,'C Gap Left',weeks$defenderGap_1_sec)
weeks$defenderGap_1_sec <- ifelse(is.na(weeks$C_gap_left_1_1sec)==F & weeks$y_1sec == weeks$C_gap_left_1_1sec,'Head Up on LT',weeks$defenderGap_1_sec)

weeks$defenderGap_1_sec <- ifelse(weeks$defenderGap_1_sec=='' & weeks$playDirection=='left' & weeks$y_1sec<weeks$ball,'Left Open Gap',
                                  ifelse(weeks$defenderGap_1_sec=='' & weeks$playDirection=='left' & weeks$y_1sec>weeks$ball,'Right Open Gap',
                                         ifelse(weeks$defenderGap_1_sec=='' & weeks$playDirection=='right' & weeks$y_1sec<weeks$ball,'Right Open Gap',
                                                ifelse(weeks$defenderGap_1_sec=='' & weeks$playDirection=='right' & weeks$y_1sec>weeks$ball,'Left Open Gap',
                                                       weeks$defenderGap_1_sec))))

weeks$defenderGap_1_sec <- ifelse(weeks$role=='Coverage','Coverage',weeks$defenderGap_1_sec)






#count players in defensive box on right/left, and compare with offensive linemen and TEs on right/left
#if defensive count minus offensive count >=1, then consider it an overload
weeks$rightBoxOverload <- vlookup(lookup_value = weeks$id_frame,
                                  dict = weeks %>% 
                                    group_by(id_frame) %>% 
                                    summarize(cond_right=sum(isCondensed_right),
                                              cond_left=sum(isCondensed_left),
                                              TEs_right=first(TEs_right),
                                              rightBoxCount=sum(inWideBox_right)) %>% 
                                    mutate(rightBoxOverload=ifelse(rightBoxCount-(cond_right+TEs_right+2.5)>=1,1,0)),
                                  result_column = 6,
                                  lookup_column = 1)


weeks$leftBoxOverload <- vlookup(lookup_value = weeks$id_frame,
                                 dict = weeks %>% 
                                   group_by(id_frame) %>% 
                                   summarize(cond_right=sum(isCondensed_right),
                                             cond_left=sum(isCondensed_left),
                                             TEs_left=first(TEs_left),
                                             leftBoxCount=sum(inWideBox_left)) %>% 
                                   mutate(leftBoxOverload=ifelse(leftBoxCount-(cond_left+TEs_left+2.5)>=1,1,0)),
                                 result_column = 6,
                                 lookup_column = 1)


#count players on defensive LOS on right/left, and compare with offensive linemen and TEs on right/left
#if defensive count minus offensive count >=1, then consider it an overload
weeks$rightLOSOverload <- vlookup(lookup_value = weeks$id_frame,
                                  dict = weeks %>% 
                                    group_by(id_frame) %>% 
                                    summarize(TEs_right=first(TEs_right),
                                              rightLOSCount=sum(onLOS_right)) %>% 
                                    mutate(rightLOSOverload=ifelse(rightLOSCount-(TEs_right+2.5)>=1,1,0)),
                                  result_column = 4,
                                  lookup_column = 1)


weeks$leftLOSOverload <- vlookup(lookup_value = weeks$id_frame,
                                 dict = weeks %>% 
                                   group_by(id_frame) %>% 
                                   summarize(TEs_left=first(TEs_left),
                                             leftLOSCount=sum(inWideBox_left)) %>% 
                                   mutate(leftLOSOverload=ifelse(leftLOSCount-(TEs_left+2.5)>=1,1,0)),
                                 result_column = 4,
                                 lookup_column = 1)



#left wide box players minus right wide box players
weeks$inWideBox_LminusR <- vlookup(lookup_value = weeks$id_frame,
                                   dict = weeks %>% 
                                     group_by(id_frame) %>% 
                                     summarize(rightBoxCount=sum(inWideBox_right),
                                               leftBoxCount=sum(inWideBox_left)) %>% 
                                     mutate(LminusR=leftBoxCount-rightBoxCount),
                                   result_column = 4,
                                   lookup_column = 1)

#left LOS players minus right LOS players
weeks$onLOS_LminusR <- vlookup(lookup_value = weeks$id_frame,
                               dict = weeks %>% 
                                 group_by(id_frame) %>% 
                                 summarize(rightLOSCount=sum(onLOS_right),
                                           leftLOSCount=sum(onLOS_left)) %>% 
                                 mutate(LminusR=leftLOSCount-rightLOSCount),
                               result_column = 4,
                               lookup_column = 1)

#left inside LOS players minus right inside LOS players
weeks$onLOS_inside_LminusR <- vlookup(lookup_value = weeks$id_frame,
                                      dict = weeks %>% 
                                        group_by(id_frame) %>% 
                                        summarize(rightLOSCount=sum(onLOS_inside_right),
                                                  leftLOSCount=sum(onLOS_inside_left)) %>% 
                                        mutate(LminusR=leftLOSCount-rightLOSCount),
                                      result_column = 4,
                                      lookup_column = 1)

#left outside LOS players minus right outside LOS players
weeks$onLOS_outside_LminusR <- vlookup(lookup_value = weeks$id_frame,
                                       dict = weeks %>% 
                                         group_by(id_frame) %>% 
                                         summarize(rightLOSCount=sum(onLOS_outside_right),
                                                   leftLOSCount=sum(onLOS_outside_left)) %>% 
                                         mutate(LminusR=leftLOSCount-rightLOSCount),
                                       result_column = 4,
                                       lookup_column = 1)


#second level players lined up outside the core, who do not have a receiver to cover
#subtract left minus right
weeks$outside_2ndLevel_LminusR <- vlookup(lookup_value = weeks$id_frame,
                                          dict = weeks %>% 
                                            group_by(id_frame) %>% 
                                            summarize(cond_right=sum(isCondensed_right),
                                                      cond_left=sum(isCondensed_left),
                                                      TEs_left=first(TEs_left),
                                                      TEs_right=first(TEs_right),
                                                      outside_left=inWideBox_outside_left-onLOS_outside_left,
                                                      outside_right=inWideBox_outside_right-onLOS_outside_right) %>% 
                                            mutate(leftDiff=ifelse(outside_left<0,0,outside_left) - cond_left - TEs_left,
                                                   rightDiff=ifelse(outside_right<0,0,outside_right) - cond_right - TEs_right) %>% 
                                            mutate(LminusR=ifelse(leftDiff<0,0,leftDiff)-ifelse(rightDiff<0,0,rightDiff)),
                                          result_column = 10,
                                          lookup_column = 1)






#second level players lined up inside the core
#subtract left minus right
weeks$inside_2ndLevel_LminusR <- vlookup(lookup_value = weeks$id_frame,
                                         dict = weeks %>% 
                                           group_by(id_frame) %>% 
                                           summarize(inside_left=inWideBox_inside_left-onLOS_inside_left,
                                                     inside_right=inWideBox_inside_right-onLOS_inside_right) %>% 
                                           mutate(LminusR=ifelse(inside_left<0,0,inside_left)-ifelse(inside_right<0,0,inside_right)),
                                         result_column = 4,
                                         lookup_column = 1)







#second level players, in general
#subtract left minus right
weeks$secondLevel_LminusR <- vlookup(lookup_value = weeks$id_frame,
                                     dict = weeks %>% 
                                       group_by(id_frame) %>% 
                                       summarize(left=inWideBox_left-onLOS_left,
                                                 right=inWideBox_right-onLOS_right) %>% 
                                       mutate(LminusR=ifelse(left<0,0,left)-ifelse(right<0,0,right)),
                                     result_column = 4,
                                     lookup_column = 1)



# # CURRENTLY NOT USED IN MODEL DEVELOPMENT
# ####################################################################################################
# ####################################################################################################
# ####################################################################################################
# #Goal: which defender is in each gap?
# 
# 
# #a string of gaps occupied by LOS players
# weeks$defenderGap_LOS_string <- vlookup(lookup_value = weeks$id_frame,
#                                         dict = weeks %>% filter(onLOS==1) %>%
#                                           group_by(id_frame) %>%
#                                           summarize(gaps=paste(defenderGap,collapse=',')),
#                                         result_column = 2,
#                                         lookup_column = 1)
# 
# #identify if a LOS player is occupying each gap
# weeks$AgapR <- str_count(weeks$defenderGap_LOS_string,'A Gap Right')
# weeks$AgapL <- str_count(weeks$defenderGap_LOS_string,'A Gap Left')
# 
# weeks$BgapR <- str_count(weeks$defenderGap_LOS_string,'B Gap Right')
# weeks$BgapL <- str_count(weeks$defenderGap_LOS_string,'B Gap Left')
# 
# weeks$CgapR <- str_count(weeks$defenderGap_LOS_string,'C Gap Right')
# weeks$CgapL <- str_count(weeks$defenderGap_LOS_string,'C Gap Left')
# 
# weeks$OpengapR <- str_count(weeks$defenderGap_LOS_string,'Right Open Gap')
# weeks$OpengapL <- str_count(weeks$defenderGap_LOS_string,'Left Open Gap')
# 
# 
# #use features created above to create a description of the defensive front
# #"frontdesc" shows the quantity of players that are lined up in each gap
# weeks$frontdesc <- paste(weeks$OpengapL,
#                          weeks$CgapL,
#                          weeks$BgapL,
#                          weeks$AgapL,
#                          ';',
#                          weeks$AgapR,
#                          weeks$BgapR,
#                          weeks$CgapR,
#                          weeks$OpengapR)
# 
# #"frontdesc_basic" displays 1 if there is ANY quantity of players in that gap
# weeks$frontdesc_basic <- str_replace_all(weeks$frontdesc,'[2-9]','1')
# 
# ####################################################################################################
# ####################################################################################################
# ####################################################################################################




# add "gameId" and "week" to "plays"


plays <- left_join(x=plays,
                   y=games %>% select(gameId,week),
                   by='gameId')
# 
# 
# plays <- 
#   right_join(
#     x=plays,
#     y=weeks %>%
#       group_by(id) %>% 
#       filter(event=='ball_snap' & is.na(nflId)==T) %>% 
#       select(formation,
#              RBqty,
#              TEqty,
#              passStrength,
#              front,
#              insideBlitzQty,
#              insideBlitzQty_manSide,
#              insideBlitzQty_zoneSide,
#              outsideBlitzQty,
#              outsideBlitzQty_manSide,
#              outsideBlitzQty_zoneSide,
#              outsideBlitzQty_passStrength,
#              passRushQty),
#     by = 'id')
# 
# 
# plays <- 
#   right_join(
#     x=plays,
#     y=weeks %>%
#       group_by(id) %>% 
#       filter(event=='ball_snap') %>% 
#       summarize(
#         isCondensed_left=sum(isCondensed_left),
#         isCondensed_right=sum(isCondensed_right),
#         centerTurn=first(centerTurn),
#         RGturn=first(RGturn),
#         LGturn=first(LGturn),
#         RTturn=first(RTturn),
#         LTturn=first(LTturn),
#         defenderGap=paste(defenderGap,collapse=','),
#         onLOS=sum(onLOS),
#         onLOS_inside_left=sum(onLOS_inside_left),
#         onLOS_inside_right=sum(onLOS_inside_right),
#         onLOS_Agap_left=sum(onLOS_Agap_left),
#         onLOS_Agap_left_DL=sum(onLOS_Agap_left_DL),
#         onLOS_Agap_left_LB=sum(onLOS_Agap_left_LB),
#         onLOS_Agap_right=sum(onLOS_Agap_right),
#         onLOS_Agap_right_DL=sum(onLOS_Agap_right_DL),
#         onLOS_Agap_right_LB=sum(onLOS_Agap_right_LB),
#         onLOS_outside_left=sum(onLOS_outside_left),
#         onLOS_outside_right=sum(onLOS_outside_right),
#         onLOS_right=sum(onLOS_right),
#         onLOS_left=sum(onLOS_left),
#         hash=first(hash),
#         TEs_left=first(TEs_left),
#         TEs_right=first(TEs_right),
#         RBs_right=first(RBs_right),
#         RBs_left=first(RBs_left),
#         WRs_right=first(WRs_right),
#         WRs_left=first(WRs_left),
#         inWideBox_right=sum(inWideBox_right),
#         inWideBox_left=sum(inWideBox_left),
#         inWideBox_outside_left=sum(inWideBox_outside_left),
#         inWideBox_outside_right=sum(inWideBox_outside_right),
#         inWideBox_inside_left=sum(inWideBox_inside_left),
#         inWideBox_inside_right=sum(inWideBox_inside_right)
#       ),
#     by='id')
# 
# 
# 
# plays <- 
#   left_join(
#     x=plays,
#     y=pffScoutingData %>% 
#       group_by(id) %>% 
#       mutate(pff_hurry=ifelse(pff_hurry=='',NA,pff_hurry),
#              pff_hit=ifelse(pff_hit=='',NA,pff_hit),
#              pff_sack=ifelse(pff_sack=='',NA,pff_sack),
#              hurry_players=ifelse(pff_hurry==1,nflId,''),
#              hit_players=ifelse(pff_hit==1,nflId,''),
#              sack_players=ifelse(pff_sack==1,nflId,'')) %>% 
#       summarize(hurry=sum(pff_hurry,na.rm=T),
#                 hit=sum(pff_hit,na.rm=T),
#                 sack=sum(pff_sack,na.rm=T),
#                 hurry_players=paste(na.omit(hurry_players),collapse=','),
#                 hit_players=paste(na.omit(hit_players),collapse=','),
#                 sack_players=paste(na.omit(sack_players),collapse=',')) %>%
#       select(id,hurry,hit,sack,hurry_players,hit_players,sack_players),
#     by='id'
#   )
# 
# 
# #sum hits, hurries, and sacks into one metric
# 
# plays$hurry_hit_sack <- ifelse(plays$hurry+plays$hit+plays$sack>=1,1,0)
# 
# 
