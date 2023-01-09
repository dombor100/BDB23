#CREATE DENSITY PLOTS FOR 5 MAN FRONTS
#MUST RUN create_features.R, predict_blitz_prob.R, and predict_center_turn.R FIRST
#MUST LOAD PACKAGES FROM create_features.R

#filter: at frame of snap, defenders in wide box, offensive linemen, QB
#filter: no "balanced" defensive looks
#mutate: adjust x and y coordinates for play direction
#mutate: normalize to hash = M
#mutate: create variable for LOS vs 2nd level defenders
data_density <- weeks %>% 
  filter(frameId==snap_frame & 
           ((off_def=='D' & onLOS==1) | 
              position_basic=='OL'| position=='QB')) %>% 
  mutate(h_adj=160/3/2 - ball) %>% 
  mutate(v_adj=60 - LOS) %>% 
  mutate(x=ifelse(playDirection=='left',120-(x+v_adj),x+v_adj),
         y=ifelse(playDirection=='left',160/3-(y+h_adj),y+h_adj))

  
data_density$totalDL <- vlookup(lookup_value = data_density$id,
                                dict = data_density %>% group_by(id) %>% summarize(o=sum(onLOS)) %>% select(id,o),
                                result_column = 2,
                                lookup_column = 1)

data_density <- data_density %>% filter(totalDL==5)
  
  
  p_right <- 
    ggplot()+
    
    #since the offense is going "up" and not "right"
    scale_x_reverse() +
    
    #density plot (defensive players)
    stat_density_2d(data=data_density %>% filter(centerTurn=='right' & off_def=='D'),
                    aes(fill= ..density.., x=y, y=x),
                    # fill='red',
                    geom = 'raster',
                    contour=F,
                    h=0.3) + 
    
    #colors for density plot
    scale_fill_gradientn(colors=c('black','dodgerblue4','dodgerblue','firebrick','red','purple','white')) +
      
      #offensive line and QB
      geom_point(data=data_density %>% filter(centerTurn=='right' & off_def=='O') %>% group_by(position) %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y,y=x,shape='21'),
                 color='white',
                 fill='white',
                 alpha=0.4,
                 size=25) +
      
      #offensive position label
      geom_text(data=data_density %>% filter(centerTurn=='right' & off_def=='O') %>% group_by(position) %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y,y=x,label=position)) +

    xlim(35,17.5) +
    ylim(50,70) + 
    theme(panel.background = element_rect(fill = 'black',
                                          color = 'black'),
          legend.position='none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length = unit(0, 'mm'),
          plot.title = element_text(hjust = 0.5,size=20),
          )+
    ggtitle('Center Turn = RIGHT')+
    geom_segment(data=data_density %>% filter(centerTurn=='left' & off_def=='O' & position=='C') %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y,xend=y,y=x-1,yend=x-2),size=3,color='gray') +
    geom_segment(data=data_density %>% filter(centerTurn=='left' & off_def=='O' & position=='C') %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y,xend=y-2,y=x-2,yend=x-2),size=3,arrow = arrow(length = unit(0.5, "cm")),color='gray') +
    
    #point to shade/2i
    geom_segment(data=data_density %>% filter(centerTurn=='left' & off_def=='O' & position=='C') %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y-1,xend=y-1,y=x+7,yend=x+2),size=3,arrow = arrow(length = unit(0.5, "cm")),color='blue')
    
  
  
  
  p_left <- 
    ggplot()+
    
    #since the offense is going "up" and not "right"
    scale_x_reverse() +
    
    #density plot (defensive players)
    stat_density_2d(data=data_density %>% filter(centerTurn=='left' & off_def=='D'),
                    aes(fill= ..density.., x=y, y=x),
                    # fill='red',
                    geom = 'raster',
                    contour=F,
                    h=0.3) + 
    
    #colors for density plot
    scale_fill_gradientn(colors=c('black','dodgerblue4','dodgerblue','firebrick','red','purple','white')) +
    
    #offensive line and QB
    geom_point(data=data_density %>% filter(centerTurn=='left' & off_def=='O') %>% group_by(position) %>% summarize(x=mean(x),y=mean(y)),
               aes(x=y,y=x,shape='21'),
               color='white',
               fill='white',
               alpha=0.4,
               size=25) +
    
    #offensive position label
    geom_text(data=data_density %>% filter(centerTurn=='left' & off_def=='O') %>% group_by(position) %>% summarize(x=mean(x),y=mean(y)),
              aes(x=y,y=x,label=position)) +
    
    xlim(35,17.5) +
    ylim(50,70) + 
    theme(panel.background = element_rect(fill = 'black',
                                          color = 'black'),
          legend.position='none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.length = unit(0, 'mm'),
          plot.title = element_text(hjust = 0.5,size=20)
    )+
    
    #title
    ggtitle('Center Turn = LEFT')+
    
    #show center turn direction
    geom_segment(data=data_density %>% filter(centerTurn=='left' & off_def=='O' & position=='C') %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y,xend=y,y=x-1,yend=x-2),size=3,color='gray') +
    geom_segment(data=data_density %>% filter(centerTurn=='left' & off_def=='O' & position=='C') %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y,xend=y+2,y=x-2,yend=x-2),size=3,arrow = arrow(length = unit(0.5, "cm")),color='gray')+
    
    #point to shade/2i
    geom_segment(data=data_density %>% filter(centerTurn=='left' & off_def=='O' & position=='C') %>% summarize(x=mean(x),y=mean(y)),
                 aes(x=y+1,xend=y+1,y=x+7,yend=x+2),size=3,arrow = arrow(length = unit(0.5, "cm")),color='blue')
  
    
  library(patchwork)
  
  p_left + p_right