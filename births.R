library(tidyverse)
library(magrittr)
#reading in the data
births <- rKenyaCensus::V4_T2.40[-c(1:3),]%>%
  select(-c(2,5,6))%>%
  arrange(desc(Percent_Notified))

births[c(1:23),]%>%
  pivot_longer(cols = c(2,3),values_to = 'Count',
                                          names_to = "Births")%>%
  ggplot(aes(reorder(County,Count),Count,fill=Births))+
  geom_bar(stat = 'identity',position = position_dodge(width = 1))+
  labs(title = 'Kenya Birth Numbers across counties',
       subtitle = 'First 23 counties ordered by notification rate')+
  xlab('County')+ylab('Number of births')+
  geom_text(aes(label=Count),position=position_dodge(1),size=3.2,hjust=0,vjust=.4)+
  scale_fill_brewer(palette = 'Dark2',type = 'qual')+
  scale_y_continuous(labels = scales::comma,expand = c(0,0),limits = c(0,530000))+
  theme(legend.position = c(.8,.6))+
  coord_flip()

births[-c(1:23),]%>%
  pivot_longer(cols = c(2,3),values_to = 'Count',
               names_to = "Births")%>%
  ggplot(aes(reorder(County,Count),Count,fill=Births))+
  geom_bar(stat = 'identity',position = position_dodge(width = 1))+
  labs(title = 'Kenya Birth Numbers across counties',
       subtitle = 'Next 23 counties ordered by notification rate')+
  xlab('County')+ylab('Number of births')+
  geom_text(aes(label=Count),position=position_dodge(1),size=3.2,hjust=0,vjust=.4)+
  scale_fill_brewer(palette = 'Dark2',type = 'qual')+
  scale_y_continuous(labels = scales::comma,expand = c(0,0),limits = c(0,200000))+
  theme(legend.position = c(.8,.6))+
  coord_flip()
