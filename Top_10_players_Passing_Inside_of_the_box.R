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
  geom_text(aes(label=round(passes.from.inside.box.90,2)), vjust=1,hjust=2, color="white", size=4)+
  labs(title="Top 10 Players by Passes Inside Box (StatsBomb)",
       x ="Player", y = "Passes Inside Box per 90")+
   theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=12, face="bold"),
    axis.text.x = element_text(color="black", size=10, face="bold"),
    axis.title.y = element_text(color="#993333", size=12, face="bold"),
    axis.text.y = element_text(color="black", size=10, face="bold")
  )


#############################################
## WC EVENTS  SPAIN  43 is World cup##
#############################################
library(ggsoccer)



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
               arrow = arrow(length = unit(0.02, "npc")),data = df)+
  xlim(0, 120)+ ylim(0, 80)

############################
# Goals and Saved
############################

df %>%
  filter(shot.outcome.name %in% c("Goal","Saved"))%>%
ggplot()+
  annotate_pitch(x_scale = 1.2,  y_scale = 0.8, colour = "white",  fill = "green4")+
  geom_segment(aes(x = x, y = y, xend = x_end, yend = y_end, colour = shot.outcome.name),
               arrow = arrow(length = unit(0.02, "npc")))+
 xlim(80, 120)+ ylim(0, 80)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
        )

############################
# Goals all with  right foot
############################

df %>%
  filter(shot.outcome.name %in% c("Goal"))%>%
  ggplot()+
  annotate_pitch(x_scale = 1.2,  y_scale = 0.8, colour = "white",  fill = "green4")+
  geom_segment(aes(x = x, y = y, xend = x_end, yend = y_end, colour = shot.body_part.name),
               arrow = arrow(length = unit(0.02, "npc")))+
  xlim(90, 120)+ ylim(0, 80)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )




############################
# Goals by type
############################

# Y axis inverted   y = 80-y


df %>%
  filter(shot.outcome.name %in% c("Goal"))%>%
  ggplot()+
  annotate_pitch(x_scale = 1.2,  y_scale = 0.8, colour = "white",  fill = "green4")+
  geom_segment(aes(x = x, y = 80-y, xend = x_end, yend =80- y_end, colour = shot.type.name),size=1,
               arrow = arrow(length = unit(0.02, "npc")))+
   xlim(0, 120)+ ylim(0, 80)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank()
  )
