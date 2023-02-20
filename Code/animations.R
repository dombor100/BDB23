#CREATE ANIMATIONS
#MUST RUN create_features.R, predict_blitz_prob.R, and predict_center_turn.R FIRST
#MUST LOAD PACKAGES FROM create_features.R


# cowboys vs chargers week 2 (example of pre-snap movement)
id1 <- '2021091911_1143'

# cowboys vs panthers week 4 (for gameplan)
id2 <- '2021100303_3219'

# cowboys vs patriots week 6 (game)
id3 <- '2021101710_1202'


# run all 6 animations above and save to "savepath" directory
ids <- c(id1,id2,id3)




for(j in 1:length(ids)){
  
  play <- ids[j]
  
  #create description variables for each play
  offense <- weeks %>% filter(id==play & off_def=='O') %>% summarize(first(team)) %>% unlist %>% unname
  defense <- weeks %>% filter(id==play & off_def=='D') %>% summarize(first(team)) %>% unlist %>% unname
  week <- plays %>% filter(id==play) %>% mutate(w=paste0('Week ',week,': ',offense,' (O) vs ',defense,' (D)')) %>% select(w) %>% unlist %>% unname
  dd <- plays %>% filter(id==play) %>% mutate(dd=paste0(down,ifelse(down==1,'st',ifelse(down==2,'nd',ifelse(down==3,'rd',ifelse(down==4,'th','')))),' & ',yardsToGo)) %>% select(dd) %>% unlist %>% unname
  hometeam <- games %>% filter(gameId==(weeks %>% filter(id==play) %>% select(gameId) %>% unlist %>% unname)[1]) %>% select(homeTeamAbbr) %>% unlist %>% unname
  awayteam <- games %>% filter(gameId==(weeks %>% filter(id==play) %>% select(gameId) %>% unlist %>% unname)[1]) %>% select(visitorTeamAbbr) %>% unlist %>% unname
  homescore <- plays %>% filter(id==play) %>% select(preSnapHomeScore) %>% unlist %>% unname
  awayscore <- plays %>% filter(id==play) %>% select(preSnapVisitorScore) %>% unlist %>% unname
  score <- paste0(ifelse(max(homescore,awayscore)==homescore,hometeam,awayteam),' is winning: ',max(homescore,awayscore),' - ',min(homescore,awayscore))
  clock <- plays %>% filter(id==play) %>% mutate(c=paste0('Q',quarter,': ',gameClock)) %>% select(c) %>% unlist %>% unname
  desc <- plays %>% filter(id==play) %>% select(playDescription)
  
  #number of frames
  nFrames <- max(weeks %>% filter(id==play) %>% select(frameId))
  
  #data to use for this animation (adjust x and y coordinates for play direction & identify labels for each player)
  data_snap <- weeks %>% filter(id==play & frameId<=nFrames) %>% mutate(x=ifelse(playDirection=='left',120-x,x),
                                                                        y=ifelse(playDirection=='left',160/3-y,y),
                                                                        label=ifelse(off_def=='O',position_general,ifelse(off_def=='D',
                                                                                                                  paste0(round(blitzprob_pred_PassRush,2)*100,'%'),'')))
  
  #relative line of scrimmage
  LOS_rel <- paste0('LOS = ',round(plays %>% filter(id==data_snap$id[1]) %>% select(yardlineNumber) %>% unname %>% unlist,2))
  
  #location at snap for each player
  data_snap$snapx <- vlookup(lookup_value = data_snap$nflId,
                             dict = data_snap %>% filter(time==snap_time) %>% select(nflId,x),
                             result_column = 2,
                             lookup_column = 1)
  data_snap$snapy <- vlookup(lookup_value = data_snap$nflId,
                             dict = data_snap %>% filter(time==snap_time) %>% select(nflId,y),
                             result_column = 2,
                             lookup_column = 1)
  
  
  #location of each player for pass rusher paths
  #(also included offensive line, in case I wanted to plot their path too)
  #however, I chose to plot the exact path of the pass rusher, and only a representation of the offensive line movement
  
  xlist <- list()
  ylist <- list()
  
  for(i in 1:14){
    xlist[[i]] <- vlookup(lookup_value = data_snap$nflId,
                          dict = data_snap %>% filter(frameId==snap_frame+i) %>% select(nflId,x),
                          result_column = 2,
                          lookup_column = 1) 
    ylist[[i]] <- vlookup(lookup_value = data_snap$nflId,
                          dict = data_snap %>% filter(frameId==snap_frame+i) %>% select(nflId,y),
                          result_column = 2,
                          lookup_column = 1)
  }
  
  data_snap$snapx <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,data_snap$snapx,NA)
  data_snap$x1 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[1]],NA)
  data_snap$x2 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[2]],NA)
  data_snap$x3 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[3]],NA)
  data_snap$x4 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[4]],NA)
  data_snap$x5 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[5]],NA)
  data_snap$x6 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[6]],NA)
  data_snap$x7 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[7]],NA)
  data_snap$x8 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[8]],NA)
  data_snap$x9 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[9]],NA)
  data_snap$x10 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[10]],NA)
  data_snap$x11 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[11]],NA)
  data_snap$x12 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[12]],NA)
  data_snap$x13 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[13]],NA)
  data_snap$x14 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,xlist[[14]],NA)
  
  data_snap$snapy <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,data_snap$snapy,NA)
  data_snap$y1 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[1]],NA)
  data_snap$y2 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[2]],NA)
  data_snap$y3 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[3]],NA)
  data_snap$y4 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[4]],NA)
  data_snap$y5 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[5]],NA)
  data_snap$y6 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[6]],NA)
  data_snap$y7 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[7]],NA)
  data_snap$y8 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[8]],NA)
  data_snap$y9 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[9]],NA)
  data_snap$y10 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[10]],NA)
  data_snap$y11 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[11]],NA)
  data_snap$y12 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[12]],NA)
  data_snap$y13 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[13]],NA)
  data_snap$y14 <- ifelse((data_snap$role=='Pass Rush' | data_snap$position_basic=='OL') & data_snap$frameId==data_snap$snap_frame,ylist[[14]],NA)
  
  
  
  #pre snap data
  data_pre_snap <- data_snap %>% filter(frameId<snap_frame)
  
  #post snap data
  data_post_snap <- data_snap %>% filter(frameId>snap_frame) %>% mutate(frameId=frameId+14)
  
  #data at snap
  data_at_snap <- data_snap %>% filter(frameId==snap_frame)
  
  #create data frame for ggplot
  #this is how I forced the animation to pause at the snap
  #I simply duplicated the frame to force it to remain on the screen
  data_snap <- rbind(data_pre_snap %>% mutate(freeze=0),
                     data_at_snap %>% mutate(freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+1,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+2,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+3,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+4,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+5,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+6,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+7,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+8,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+9,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+10,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+11,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+12,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+13,freeze=1),
                     data_at_snap %>% mutate(frameId=frameId+14,freeze=1),
                     data_post_snap %>% mutate(freeze=2)) %>% arrange(frameId)
  
  #replace NAs with final value, pre-snap, using na.locf from the zoo package
  #na.locf (last observation carried forward)
  data_snap$centerturn_pred <- na.locf(data_snap$centerturn_pred,na.rm = F)
  data_snap$centerturn_pred <- na.locf(data_snap$centerturn_pred,fromLast = T)
  
  data_snap$centerturn_pred_right <- na.locf(data_snap$centerturn_pred_right,na.rm = F)
  data_snap$centerturn_pred_right <- na.locf(data_snap$centerturn_pred_right,fromLast = T)
  
  data_snap$centerturn_pred_left <- na.locf(data_snap$centerturn_pred_left,na.rm = F)
  data_snap$centerturn_pred_left <- na.locf(data_snap$centerturn_pred_left,fromLast = T)

  
  #manipulate data for bottom left text in animations, which shows play details, defensive structure, and predictions
  more_features <- cbind(
    data_snap %>% 
      filter(frameId<=snap_frame & off_def=='D') %>% 
      mutate(qty=ifelse(blitzprob_pred_PassRush>0.5,1,0)) %>% 
      group_by(frameId) %>% summarize(wb=sum(inWideBox),
                                      wbl=sum(inWideBox_left),
                                      wbr=sum(inWideBox_right),
                                      los=sum(onLOS),
                                      losr=sum(onLOS_right),
                                      losl=sum(onLOS_left),
                                      rbo=ifelse(first(rightBoxOverload)==1,'YES','NO'),
                                      lbo=ifelse(first(leftBoxOverload)==1,'YES','NO'),
                                      pass_rush_qty_pred=sum(qty)),
    data_snap %>% 
      filter(frameId<=snap_frame & off_def=='O') %>% 
      mutate(c=ifelse(centerturn_pred=='right',paste0(round(centerturn_pred_right,2)*100,'% Right'),
                      ifelse(centerturn_pred=='left',paste0(round(centerturn_pred_left,2)*100,'% Left'),NA))) %>% 
      group_by(frameId) %>% summarize(centerturn_pred_pct=first(c)) %>% 
      ungroup %>% select(centerturn_pred_pct)
  )
  
  data_snap <- 
    full_join(
      x=data_snap,
      y=more_features,
      by='frameId'
    )
  
  
  
  #replace NAs with most recent observed value (final value pre-snap)
  data_snap$wb <- na.locf(data_snap$wb)
  data_snap$wbl <- na.locf(data_snap$wbl)
  data_snap$wbr <- na.locf(data_snap$wbr)
  data_snap$los <- na.locf(data_snap$los)
  data_snap$losr <- na.locf(data_snap$losr)
  data_snap$losl <- na.locf(data_snap$losl)
  data_snap$rbo <- na.locf(data_snap$rbo)
  data_snap$lbo <- na.locf(data_snap$lbo)
  data_snap$pass_rush_qty_pred <- na.locf(data_snap$pass_rush_qty_pred)
  data_snap$centerturn_pred_pct <- na.locf(data_snap$centerturn_pred_pct)
  
  #replace NA values with 0
  data_snap$timeToSnap <- ifelse(is.na(data_snap$timeToSnap)==T,0,data_snap$timeToSnap)
  
  #absolute line of scrimmage
  LOS_abs <- ifelse(data_snap$playDirection[1]=='left',120-data_snap$LOS[1],data_snap$LOS[1])
  
  #maximum vertical position on play
  # ymax <- max(data_snap$x)
  
  #I'm currently only displaying 15 yards past the LOS, since we are only concerned with front play
  ymax <- LOS_abs+15
  

  #create values for football field representation
  smallest_yardLine <- ceiling((LOS_abs-15)/5)*5
  smallest_number <- ceiling((LOS_abs-15)/10)*10
  
  fiveYardLines <- data.frame(y=seq(smallest_yardLine,ymax,5)) %>% mutate(yend=y,x=53.3333,xend=0)
  rightHash <- data.frame(y=seq(smallest_yardLine,ymax,1)) %>% mutate(yend=y,x=29.75+0.25,xend=29.75-0.25)
  leftHash <- data.frame(y=seq(smallest_yardLine,ymax,1)) %>% mutate(yend=y,x=23.58333+0.25,xend=23.58333-0.25)
  left_SL_hash <- data.frame(y=seq(smallest_yardLine,ymax,1)) %>% mutate(yend=y,x=0.1,xend=0.1+0.5)
  right_SL_hash <- data.frame(y=seq(smallest_yardLine,ymax,1)) %>% mutate(yend=y,x=53.2333,xend=53.2333-0.5)
  sidelines <- data.frame(x=c(0,53.3333)) %>% mutate(xend=x,y=smallest_yardLine,yend=ymax)
  
  rightNumbers <- data.frame(y=seq(smallest_number,ymax,10)) %>% mutate(x=26.666667+13,label=seq(smallest_number,ymax,10),angle=270)
  leftNumbers <- data.frame(y=seq(smallest_number,ymax,10)) %>% mutate(x=26.666667-13,label=seq(smallest_number,ymax,10),angle=90)
  
  rightNumbers <- rightNumbers %>% mutate(label=label-10) %>% mutate(label=ifelse(label>50,100-label,label))
  leftNumbers <- leftNumbers %>% mutate(label=label-10) %>% mutate(label=ifelse(label>50,100-label,label))
  
  
  
  
  
  p <- 
    ggplot(data=data_snap)+
    
    #since the offense is going "up" and not "right"
    scale_x_reverse() +
    
    #theme options
    theme(legend.position='none',
          plot.title = element_text(hjust = 0.5,size=20),
          plot.subtitle = element_text(hjust=0.5,size=18),
          plot.tag=element_blank(),
          plot.caption=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length = unit(0, 'mm'),
          panel.background = element_rect(fill = 'black',
                                          color = 'black'),
    )+

    #create football field
    geom_segment(data=fiveYardLines,aes(x=x,xend=xend,y=y,yend=yend),color='white')+
    geom_segment(data=leftHash,aes(x=x,xend=xend,y=y,yend=yend),color='white')+
    geom_segment(data=rightHash,aes(x=x,xend=xend,y=y,yend=yend),color='white')+
    geom_segment(data=left_SL_hash,aes(x=x,xend=xend,y=y,yend=yend),color='white')+
    geom_segment(data=right_SL_hash,aes(x=x,xend=xend,y=y,yend=yend),color='white')+
    geom_segment(data=sidelines,aes(x=x,xend=xend,y=y,yend=yend),color='white',size=4)+
    geom_text(data=rightNumbers,aes(x=x,y=y,label=label,angle=angle),color='white',size=10)+
    geom_text(data=leftNumbers,aes(x=x,y=y,label=label,angle=angle),color='white',size=10)+
    
    #offensive team logo
    geom_image(aes(x=5,y=smallest_yardLine+3,image=nflfastR::teams_colors_logos %>% filter(team_abbr==offense) %>% select(team_logo_espn) %>% unlist %>% unname),size=0.15)+
    
    #defensive team logo
    geom_image(aes(x=5,y=ymax-3,image=nflfastR::teams_colors_logos %>% filter(team_abbr==defense) %>% select(team_logo_espn) %>% unlist %>% unname),size=0.15)+
    
    
    #offensive player points for animation
    geom_point(data=data_snap %>% filter(off_def!='D'),
               aes(x=y,y=x,shape='21',group=nflId),
               color=ifelse((data_snap %>% filter(off_def!='D') %>% select(off_def) %>% unname %>% unlist)=='ball','darkorange',
                            ifelse((data_snap %>% filter(off_def!='D') %>% select(off_def) %>% unname %>% unlist)=='O','gray','white')),
               fill=ifelse((data_snap %>% filter(off_def!='D') %>% select(off_def) %>% unname %>% unlist)=='ball','darkorange',
                           ifelse((data_snap %>% filter(off_def!='D') %>% select(off_def) %>% unname %>% unlist)=='O','gray','white')),
               alpha=ifelse((data_snap %>% filter(off_def!='D') %>% select(off_def) %>% unname %>% unlist)=='OL',1,0.4), size=15) +
    
    #Micah Parsons spotlight
    geom_point(data=data_snap %>% filter(nflId=='53441'),
               aes(x=y,y=x,shape='21',group=nflId),alpha=0.5,size=20,color='yellow',fill='yellow') +
    
    #defensive player points for animation
    geom_point(data=data_snap %>% filter(off_def=='D'),
               aes(x=y,y=x,shape='21',group=nflId,color=blitzprob_pred_PassRush,fill=blitzprob_pred_PassRush),size=15) + 
    
    scale_color_gradient(low='blue3',high='red') + 
    
    
    
    
    # #center turn prediction
    geom_segment(data=data_snap %>% filter(position=='C' & freeze==0),aes(x=y,xend=y,y=x-0.5,yend=x-2),size=1.5,color='gray') +
    geom_segment(data=data_snap %>% filter(position=='C' & freeze==0),aes(x=y,xend=ifelse(centerturn_pred=='left',y+1,y-1),y=x-2,yend=x-2),size=1.5,arrow = arrow(length = unit(0.25, "cm")),color='gray') +

    geom_text(data=data_snap %>% filter(position=='C' & freeze==0),aes(x=y,y=x-2.5,label=centerturn_pred_pct),
              size=6,hjust='center',color='white')+
  
    #actual center turn representation      
    geom_segment(data=data_snap %>% filter(position=='C' & freeze==1),aes(x=snapy,xend=snapy,y=snapx-0.5,yend=snapx-2.5),color='dodgerblue',size=2) + 
    geom_segment(data=data_snap %>% filter(position=='C' & freeze==1),aes(x=snapy,xend=ifelse(centerTurn=='left',snapy+1.5,snapy-1.5),y=snapx-2.5,yend=snapx-2.5),color='dodgerblue',size=2,arrow = arrow(length = unit(0.5, "cm"))) + 
    
    #actual RG turn representation      
    geom_segment(data=data_snap %>% filter(position=='RG' & freeze==1),aes(x=snapy,xend=snapy,y=snapx-0.5,yend=snapx-2.5),color=ifelse(data_snap$RGturn[1]=='na','gray',ifelse(data_snap$RGturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2) + 
    geom_segment(data=data_snap %>% filter(position=='RG' & freeze==1),aes(x=snapy,xend=ifelse(RGturn=='na',snapy,ifelse(RGturn=='left',snapy+1.5,snapy-1.5)),y=snapx-2.5,yend=snapx-ifelse(RGturn=='na',2.6,2.5)),color=ifelse(data_snap$RGturn[1]=='na','gray',ifelse(data_snap$RGturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2,arrow = arrow(length = unit(0.5, "cm"))) + 
    
    #actual LG turn representation      
    geom_segment(data=data_snap %>% filter(position=='LG' & freeze==1),aes(x=snapy,xend=snapy,y=snapx-0.5,yend=snapx-2.5),color=ifelse(data_snap$LGturn[1]=='na','gray',ifelse(data_snap$LGturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2) + 
    geom_segment(data=data_snap %>% filter(position=='LG' & freeze==1),aes(x=snapy,xend=ifelse(LGturn=='na',snapy,ifelse(LGturn=='left',snapy+1.5,snapy-1.5)),y=snapx-2.5,yend=snapx-ifelse(LGturn=='na',2.6,2.5)),color=ifelse(data_snap$LGturn[1]=='na','gray',ifelse(data_snap$LGturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2,arrow = arrow(length = unit(0.5, "cm"))) + 
    
    #actual RT turn representation      
    geom_segment(data=data_snap %>% filter(position=='RT' & freeze==1),aes(x=snapy,xend=snapy,y=snapx-0.5,yend=snapx-2.5),color=ifelse(data_snap$RTturn[1]=='na','gray',ifelse(data_snap$RTturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2) + 
    geom_segment(data=data_snap %>% filter(position=='RT' & freeze==1),aes(x=snapy,xend=ifelse(RTturn=='na',snapy,ifelse(RTturn=='left',snapy+1.5,snapy-1.5)),y=snapx-2.5,yend=snapx-ifelse(RTturn=='na',2.6,2.5)),color=ifelse(data_snap$RTturn[1]=='na','gray',ifelse(data_snap$RTturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2,arrow = arrow(length = unit(0.5, "cm"))) + 
    
    #actual LT turn representation      
    geom_segment(data=data_snap %>% filter(position=='LT' & freeze==1),aes(x=snapy,xend=snapy,y=snapx-0.5,yend=snapx-2.5),color=ifelse(data_snap$LTturn[1]=='na','gray',ifelse(data_snap$LTturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2) + 
    geom_segment(data=data_snap %>% filter(position=='LT' & freeze==1),aes(x=snapy,xend=ifelse(LTturn=='na',snapy,ifelse(LTturn=='left',snapy+1.5,snapy-1.5)),y=snapx-2.5,yend=snapx-ifelse(LTturn=='na',2.6,2.5)),color=ifelse(data_snap$LTturn[1]=='na','gray',ifelse(data_snap$LTturn[1]==data_snap$centerTurn[1],'dodgerblue','forestgreen')),size=2,arrow = arrow(length = unit(0.5, "cm"))) + 
    
    #bottom left text for play details, defensive structure, and predictions
    geom_label(aes(x=160/3+1,y=smallest_yardLine+4,label=paste0('Time to Snap = ',round(timeToSnap,1),'\n',
                                                                  'Defenders in Wide Box = ',wb,'\n',
                                                                  # 'In Wide Box Left = ',wbl,'\n',
                                                                  # 'In Wide Box Right = ',wbr,'\n',
                                                                  'Defenders on LOS = ',los,'\n',
                                                                  # 'On LOS Right = ',losr,'\n',
                                                                  # 'On LOS Left = ',losl,'\n',
                                                                  'Right Box Overload? = ',rbo,'\n',
                                                                  'Left Box Overload? = ',lbo,'\n',
                                                                  'Pass Rush Qty Prediction = ',pass_rush_qty_pred,'\n',
                                                                  'Center Turn Prediction = ',centerturn_pred_pct,'\n',
                                                                  'Actual Center Turn = ',ifelse(centerTurn=='right','Right','Left'))),
               hjust='left',size=7)+
    
    
    #pass rusher paths
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=snapy,xend=y1,y=snapx,yend=x1),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y1,xend=y2,y=x1,yend=x2),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y2,xend=y3,y=x2,yend=x3),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y3,xend=y4,y=x3,yend=x4),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y4,xend=y5,y=x4,yend=x5),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y5,xend=y6,y=x5,yend=x6),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y6,xend=y7,y=x6,yend=x7),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y7,xend=y8,y=x7,yend=x8),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y8,xend=y9,y=x8,yend=x9),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y9,xend=y10,y=x9,yend=x10),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y10,xend=y11,y=x10,yend=x11),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y11,xend=y12,y=x11,yend=x12),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y12,xend=y13,y=x12,yend=x13),color='red',size=1.5) + 
    geom_segment(data=data_snap %>% filter(role=='Pass Rush' & freeze==1),aes(x=y13,xend=y14,y=x13,yend=x14),color='red',size=1.5,arrow = arrow(length = unit(0.5, "cm"))) + 
    
    # offense - position_general labels
    geom_text(aes(x = y, y = x, label = ifelse(off_def=='O',label,NA)),
              colour = "white", 
              vjust = 0.36, size = 5.5, fontface='bold') + 
    
    # defense = blitz probability
    geom_text(aes(x = y, y = x, label = ifelse(off_def=='D',label,NA)),
              colour = "white", 
              vjust = 0.36, size = 5, fontface='bold') + 
    
    # defender gaps
    geom_text(aes(x = y, y = x+1.2, label = ifelse(inWideBox==1,ifelse(str_detect(defenderGap,'Open')==T,'Open',str_extract(defenderGap,'.')),NA)),
              colour = "white", 
              vjust = 0.36, size = 5.5, fontface= 'bold') + 
    
    #title
    ggtitle(label=desc,
            subtitle=paste0('{round(frame/(nFrames+14),2)*100}% Complete')) +
    
    #top left text showing general play information
    geom_label(aes(x=160/3+1,y=ymax-2,label=paste0(week,'\n',score,'\n',clock,'\n',dd,'\n',LOS_rel)),hjust='left',size=7)+
    
    ylim(LOS_abs-15,LOS_abs+15)+

    #animate
    transition_time(frameId)  +
    ease_aes('linear')
  
  #save animation
  anim_save(paste0(savepath,'animation',j,".gif"),
            animate(p, width = 1200, height = 850,
                    fps = 10, nframe = nFrames+14,
                    renderer = gifski_renderer()))
  
  
  
}
