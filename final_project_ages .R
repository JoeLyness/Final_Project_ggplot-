#packages 
library(tidyverse)
pacman::p_load('dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png', 
               'grid', 'ggpubr', 'scales',
               'bbplot')
library(stringr)
library(svglite)
library(RColorBrewer)
library(pdftools)
library(tm)
library(rvest)
library(rlist)
library(stringi)
library(htmltab)
library(forcats)
library(ggrepel)

#ended up not using this as was given better data but this is an example 
#of a quick and easy wikipedia scraper - in this case it just pulls some 
#Prem attendences 

url = 'https://www.worldfootball.net/attendance/eng-premier-league-2018-2019/1/'
url1 = 'https://en.wikipedia.org/wiki/2018%E2%80%9319_Premier_League'
attendence_DF = url %>%
  read_html() %>%
  html_node(xpath = '//*[@id="site"]/div[2]/div[1]/div/div[3]/div/table') %>%
  html_table(fill = TRUE)


final_table_DF = url1 %>%
  read_html() %>%
  html_node(xpath = '//*[@id="mw-content-text"]/div/table[5]') %>%
  html_table(fill = TRUE)

final_table_DF <- final_table_DF[,-11]

attendence_DF1 <- attendence_DF[,-2]


#Visualisations for the piece


#load data 
all_memb_ages <- read_csv("all_member_ages.csv")
memb_class <- read_csv("class_parties.csv")




#ggplot showing how easy it is to prototype various vis 
#this is far too busy and doesn't give a clear view of what I want 
ggplot(all_memb_ages,
       aes(x=party,
           y=value,
           fill=as.factor(ages)))+
  geom_bar(stat="identity")+
  bbc_style()


#This is a way better way of doing it - varidis_d is also really effective 
ggplot(all_memb_ages,
       aes(x=party,
           y=value,
           fill=as.factor(ages)))+
  geom_bar(stat="identity",position="dodge")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  bbc_style()+
  scale_fill_viridis_d(direction = -1) +
  labs(title = "Party membership age breakdown",
       subtitle = "Data from Party Members Project") +
  theme(legend.position = "top", 
        legend.justification = "left") +
  guides(fill = guide_legend(reverse = TRUE))

#quick export 
  +
  ggsave("chart_test.svg")


#use filter to look at specific parties  
tory_ages <- all_memb_ages %>% 
  filter(party=="Conservatives")

some_ages <- all_memb_ages %>% 
  filter(party=="Conservatives"|party=="Labour"|party=="Lib Dems")
                                      


#breakdown of tory membership ages
#this is the most dramatic and really shows how old most tories are - but is it unfair?
#they're not the only unrepresentitive party 
ggplot(tory_ages,
       aes(x=ages,
           y=value,
           fill=as.factor(value)))+
         geom_bar(stat = "identity")+
         scale_y_continuous(labels = function(x) paste0(x, "%"))+
         bbc_style()+
         scale_fill_viridis_d(direction = -1)+
         labs(title = "Elderly are a majority amongst tories",
              subtitle = "Percentage breakdown of Conservative members age groups",
              caption = "Data from Party Members Project")+
         theme(legend.position = "top", 
               legend.justification = "left",
               plot.caption = element_text(face = "italic", family = "Helvetica")) +
          coord_flip() +
          ylim(c(0,40))+
         guides(fill = guide_legend(reverse = TRUE))+
  ggsave("Tories_ages.svg")


#this could be a better way of doing it? - still showing the extent of tories - but gives
#the reader something to compare it to - extended ylim to add clarity 
ggplot(some_ages,
       aes(x=party,
           y=value,
           fill=as.factor(ages)))+
  geom_bar(stat="identity",position="dodge")+
  bbc_style()+
  scale_fill_viridis_d(direction = -1) +
  labs(title = "Young people are a minority of members",
       subtitle = "Percentage breakdown of members ages across the three main parties",
       caption = "Data from Party Members Project") +
  theme(legend.position = "top", 
        legend.justification = "left",
        plot.caption = element_text(face = "italic", family = "Helvetica")) +
  ylim(c(0,40))+
  guides(fill = guide_legend(reverse = TRUE))+
  ggsave("three_party_split.svg")

#try it flipped - this looks rubbish 
ggplot(some_ages,
       aes(x=party,
           y=value,
           fill=as.factor(ages)))+
  geom_bar(stat="identity",position="dodge")+
  bbc_style()+
  scale_fill_viridis_d(direction = -1) +
  labs(title = "Party membership age breakdown",
       subtitle = "Data from Party Members Project") +
  theme(legend.position = "top", 
        legend.justification = "left") +
  coord_flip()+
  guides(fill = guide_legend(reverse = TRUE))




#pull in poling data 
members_brexit_thoughts<- read_csv("tory_members_brexit_intentions.csv")

view(members_brexit_thoughts)


#dumbell to show members are a lot more into brexit 

ggplot(members_brexit_thoughts, aes(x = `members`, xend = `voters`, y = question)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 4,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
    theme(legend.position = "top", 
        legend.justification = "left")+
  bbc_style()+
  labs(title="Party members passionate about Brexit",
       subtitle="Conservative voters' views on Brexit compared to members'")+
   xlim(c(0,100))

#uses stringr function to wrap qusetion titles 
members_brexit_thoughts$questionnew = str_wrap(members_brexit_thoughts$question, width = 30)

#and re-try + some new labels to make sure everyone understands 
ggplot(members_brexit_thoughts, aes(x = `members`, xend = `voters`, y = questionnew)) + 
  geom_dumbbell(colour = "#dddddd",
                size = 4,
                colour_x = "#FDE725",
                colour_xend = "#440154") +
  geom_text(aes(label=paste0("(",`voters`,"% voters, ",`members`,"% members)"),hjust=-0.1, vjust=-0.8),family = "Helvetica")+
  bbc_style()+
  labs(title="Party members are more passionate about Brexit",
       subtitle="A comparisson of members and voters opinions on several issues",
       caption = "Data from Party Members Project")+
  xlim(c(0,100))+
  theme(plot.caption = element_text(face = "italic"))+
  ggsave("final_test_dumbbell.jpg")




#having a look at waffle package as a quick way to make some infographic things to 
#accompany text - '1 in 50 people are party members' etc.

library(waffle)
library(hrbrthemes)
library(waffle)
library(tidyverse)

        #not sure about these

iron(
  waffle(c("Members" = 1, "Non-Members" = 49), rows = 5, keep = FALSE)
)            

  

#Now I need to show that they're older and posher than most the public.
posh_gender <- read_csv("posh_gender.csv")

ggplot(data=posh_gender,
       aes(x=`ABC1 %`, y=`Female %`, color=Party))+
  geom_point(size=4)+
  bbc_style()+
  expand_limits(x = 0, y = 0, xend = 100, yend = 100)+
  labs(title="Posh and Male",
       subtitle="")

#add an ab line to help read it 
ggplot(data=posh_gender,
       aes(x=`ABC1 %`, y=`Female %`, color=Party))+
  geom_point(size=6)+
  bbc_style()+
  expand_limits(x = 0, y = 0, xend = 100, yend = 100)+
  geom_abline(intercept=0, col="light gray")+
  labs(title="Posh and Male",
       subtitle="")


#try some ways to make it more clear - decided to add captions to ensure that the chart was fully explained 
#originally as a prototype I did this in ggplot but after seeing how well they could be done I felt 
#no need to do it in illustrator - I also made some other adjustments to the BBC theme based on some product
#feedback in an attempt to make sure it could be easily understood by a wide audience 
ggplot(data=posh_gender,
       aes(x=`ABC1 %`, y=`Female %`, colour=Party))+
  geom_point(size=5)+
  scale_color_manual(values = c("#440154","#29788D", "#79D251", "#FDE725"))+
  bbc_style()+
  scale_fill_manual(values=cbPalette)+
  expand_limits(x = 0, y = 0, xend = 100, yend = 100)+
  geom_abline(intercept=0, col="black")+
  labs(title="Middle class and majority male",
       subtitle="The percentage of party members who are female and from an\nABC1 social grouping",
       x = "% of members in ABC1 social grouping",
       y = "% of female members",
       caption = "Data from Party Members Project")+
  scale_y_continuous(labels = function(x) paste0(x, "%"))+
  scale_x_continuous(labels = function(x) paste0(x, "%"))+
  geom_label(aes(x = 5, y = 75, label = "The population is almost a 50/50 split"), 
             hjust = 0, 
             vjust = 0.5, 
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  geom_curve(aes(x = 15, y = 70, xend = 54, yend = 50), 
             colour = "#555555", 
             size=0.5, 
             curvature = 0.5,
             arrow = arrow(length = unit(0.03, "npc")))+
  geom_label(aes(x = 32, y = 20, label = "To varying degrees political parties \nfind themselves a distance \nfrom the trend"), 
             hjust = 0, 
             vjust = 0.5, 
             lineheight = 0.8,
             colour = "#555555", 
             fill = "white", 
             label.size = NA, 
             family="Helvetica", 
             size = 6) +
  geom_curve(aes(x = 50, y = 29, xend = 74, yend = 45), 
             colour = "#555555", 
             size=0.5, 
             curvature = -0.2,
             arrow = arrow(length = unit(0.03, "npc")))+
  theme(panel.grid.major.x = element_line(color="#cbcbcb"), 
        panel.grid.major.y=element_line(color="#cbcbcb"),
        panel.grid.minor = element_line(color = "#D3D3D3",
                                        size = 0.2),
        axis.title = element_text(size = 18, family="Helvetica"),
        plot.caption = element_text(face = "italic", family="Helvetica"))+
  ggsave("posh_male_final.svg")



