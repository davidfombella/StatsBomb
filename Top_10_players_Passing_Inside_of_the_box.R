# RUSSIA 2018 WORLD CUP

# Top 10 Players: Passing from inside of the boc

#Importar Librerias 
require(StatsBombR)
require(tidyverse)

# Load Free events from statbombs API
events <- StatsBombR::StatsBombFreeEvents()


# get minutes played information
minutes.played <-StatsBombR::get.minutesplayed(events)



# create a summary of total minutes per player
minutes.played.per.player <- minutes.played %>%
   group_by(player.name) %>%
   summarise(total.minutes.played=sum(minutes.played))
   
# create a subset of passes inside the box and from World Cup 2018
# wcup competition id 43
pass.from.inside.box <- events %>%
   separate(location,c("spare.location","x","y"))%>%
   mutate(x=as.numeric(as.character(x)),y=as.numeric(as.character(y)))%>%
   filter(competition_id==43 & type.name=="Pass" & between(x,102,120) &between(y,18,62))%>%
   group_by(player.name) %>%  summarise(passes.from.inside.box=n())

#add the total minutes played information to the pass subset dataframe
pass.from.inside.box <-merge(pass.from.inside.box ,minutes.played.per.player,by="player.name")


# filter out players less than 270 minutes and convert to per 90 minutes played
pass.from.inside.box %>%
   group_by(player.name)%>%
   filter(total.minutes.played > 270) %>%
   summarise(passes.from.inside.box.90=(passes.from.inside.box / total.minutes.played)*90) %>%
   arrange(-passes.from.inside.box.90) %>%
   top_n(10)


# Lo guardamos en un dataframe
top10.passes.players.inside.box <-pass.from.inside.box %>%
   group_by(player.name)%>%
   filter(total.minutes.played > 270) %>%
   summarise(passes.from.inside.box.90=(passes.from.inside.box / total.minutes.played)*90) %>%
   arrange(-passes.from.inside.box.90) %>%
   top_n(10)



require(ggplot2)

###############
# Plot it
###############

ggplot(top10.passes.players.inside.box,aes(x=fct_reorder(player.name, passes.from.inside.box.90),
                                           y=passes.from.inside.box.90))+
  geom_bar(stat="identity")+
  coord_flip()+
  geom_text(aes(label=round(passes.from.inside.box.90,2)), vjust=1,hjust=2, color="white", size=4)


#############################################
## WC EVENTS ##
#############################################


wc_events <- events %>%
  filter(competition_id==43)

wc_events_spain <- events %>%
  filter(team.name=="Spain",competition_id==43)


wc_events_spain_shots <- events %>%
  filter(team.name=="Spain",competition_id==43,type.name=="Shot")



df<-wc_events_spain_shots %>%  
  separate(location,c("location","x","y"))%>%
  mutate(x=as.numeric(as.character(x)),y=as.numeric(as.character(y)))%>%
  separate(shot.end_location,c("shot.end_location","x_end","y_end"))%>%
  mutate(x_end=as.numeric(as.character(x_end)),y_end=as.numeric(as.character(y_end)))



# All shots
ggplot()+
  geom_segment(aes(x = x, y = y, xend = x_end, yend = y_end, colour = shot.outcome.name),
               arrow = arrow(length = unit(0.02, "npc")),data = df)


# Goals

df %>%
  filter(shot.outcome.name=="Goal")%>%
ggplot()+
  geom_segment(aes(x = x, y = y, xend = x_end, yend = y_end, colour = shot.outcome.name),
               arrow = arrow(length = unit(0.02, "npc")))
