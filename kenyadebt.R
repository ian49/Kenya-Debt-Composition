####Kenya Debt composition####
library(tidyverse)
debt <- read.csv("../cbkdebt.csv",sep=",",header = T)
colnames(debt)[1] <- "Year"
for(i in 3:5){
  debt[,i] <- as.numeric(lapply(debt[,i],gsub,pattern=',',replacement=''))
}
clean_debt <- debt%>%
  filter(Month=="December"|Month=="June"&Year=="2020")%>%
  group_by(Year,Month)%>%
  select(-2)%>%
  pivot_longer(.,cols=c(Domestic.Debt,External.Debt,Total),names_to="Type",
            values_to="Amount")
#Plot A
ggplot(clean_debt,aes(x=Year,y=Amount,fill=Type))+
  geom_bar(stat="identity",position=position_dodge(width = .9))+
  scale_fill_brewer(type = 'qual',palette = 6,labels=c("Domestic","External","Total"))+
  geom_text(aes(label=round(Amount/100000,1)),vjust=-.4,hjust=.4,size=3,
            color="black",position = position_dodge(1))+
  scale_y_continuous(labels = paste(seq(0,70,10)),
                     breaks =seq(0,7000000,1000000))+
  labs(title="Kenya Debt Composition,09/`99-06/`20",y="Amount,Ksh Billion '00",
       caption ="Compiled by @mutwiriian\nSource:Central Bank of Kenya")+
  theme(legend.direction = "horizontal",legend.position = c(0.4,.9),
        legend.title =element_blank(),
        plot.caption = element_text(size = 10,
        margin =margin(t=5),hjust = .1))

#Plot B
ggplot(clean_debt,aes(Year,Amount,fill=Type))+
  geom_col(position = 'dodge',width = .9)+
  scale_fill_brewer(type = 'qual',palette=6,labels=c('Domestic','External','Total'))+
  labs(title ='Kenya Debt Composition by Type 09/`99-06/`20',x='Year',y="Amount,Ksh Billion '00",
       caption = 'Compiled by @mutwiriian\nSource:Central Bank of Kenya')+
  geom_text(aes(label=round(Amount/100000,0)),position = position_dodge(.8),
            vjust=-0.2,hjust=.5,size=3.5)+
  scale_y_continuous(breaks = seq(0,7000000,1000000),
                     labels = paste(seq(0,70,10)))+
  theme(legend.direction = 'horizontal',
        legend.title =element_blank(),legend.position = c(0.4,0.9),
        plot.caption = element_text(size = 10,margin=margin(t=10),hjust=.1))
