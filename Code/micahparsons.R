
#players that have a mix of coverage and pass rush plays
#Micah Parsons is very versatile, and has the most hits, hurries, and sacks, by far

mix <- 
data.frame(weeks %>% 
             group_by(nflId) %>%
             mutate(c=ifelse(role=='Coverage',1,0)) %>% 
             summarize(pct=sum(c)/length(c)) %>% 
             filter(pct>0.4 & pct<0.6))

mix$name <- vlookup(lookup_value = mix$nflId,
                    dict = players %>% select(nflId,displayName),
                    result_column = 2,
                    lookup_column = 1)

mix <- 
left_join(
  x=mix,
  y=pffScoutingData %>% 
    filter(nflId %in% mix$nflId) %>% 
    group_by(nflId) %>% 
    mutate(hhs=ifelse(pff_hit==1 | pff_hurry==1 | pff_sack==1,1,0)) %>% 
    summarize(hhs=sum(hhs)),
  by='nflId'
)


mix <- mix %>% arrange(-hhs)
mix


# Micah Parsons top 10 LOW probability blitzes weeks 1-5, that resulted in a hit, hurry, or sack

weeks %>% filter(id %in% 
                          (pffScoutingData %>% 
                             mutate(hhs=ifelse(pff_hit==1 | pff_hurry==1 | pff_sack==1,1,0)) %>% 
                             filter(nflId=='53441' & hhs==1 & 
                                      id %in% 
                                      (plays %>% filter(defensiveTeam=='DAL' & week<=5) %>% count(id) %>% select(id) %>% unlist %>% unname)) %>% 
                             select(id) %>% 
                             unlist %>% 
                             unname)) %>% 
  filter(nflId=='53441' & frameId<=snap_frame) %>% 
  select(id,blitzprob_pred_PassRush,role) %>% 
  group_by(id) %>% 
  filter(role=='Pass Rush') %>% 
  summarize(min=min(blitzprob_pred_PassRush)) %>% 
  arrange(min) %>% 
  top_n(-10,min) %>% 
  select(id) %>% 
  unname %>% 
  unlist


# Micah Parsons average blitz probability when lined up in the wide box, inside-left or inside-right, and NOT on the LOS

weeks %>% filter(id %in%
  (pffScoutingData %>% 
     filter(id %in% 
              (plays %>% filter(defensiveTeam=='DAL' & week<=5) %>% count(id) %>% select(id) %>% unlist %>% unname)) %>% 
     select(id) %>% 
     unlist %>% 
     unname)) %>% 
  filter(nflId=='53441' & (inWideBox_inside_right==1 | inWideBox_inside_left==1) & onLOS==0 & frameId==snap_frame) %>% 
  summarize(mean(blitzprob_pred_PassRush))


# Micah Parsons average blitz probability, overall

weeks %>% filter(id %in%
                   (pffScoutingData %>% 
                      filter(id %in% 
                               (plays %>% filter(defensiveTeam=='DAL' & week<=5) %>% count(id) %>% select(id) %>% unlist %>% unname)) %>% 
                      select(id) %>% 
                      unlist %>% 
                      unname)) %>% 
  filter(nflId=='53441' & frameId==snap_frame) %>% 
  summarize(mean(blitzprob_pred_PassRush))




