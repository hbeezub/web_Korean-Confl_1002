#Deaths by birth year
brthyr<-read.csv('KoreanConflict.csv',header=TRUE,stringsAsFactors=FALSE)

sum(str_detect(brthyr$BIRTH_YEAR,"\\d{4}"))

#group by incident dates
#to count in dplyr n() is for count
df<-brthyr%>%
  filter(str_detect(brthyr$BIRTH_YEAR,"\\d{4}")==TRUE)%>%
  group_by(BIRTH_YEAR)%>%
  summarize(num_deaths=n())%>%
  select(BIRTH_YEAR,num_deaths)


ggplot()+
  geom_line(data=df, aes(x=BIRTH_YEAR,y=num_deaths,group=1),stat="identity")+
  geom_point(data=df,aes(x=BIRTH_YEAR,y=num_deaths),stat="identity")+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Birth Year")+
  ylab("Number of Deaths")+
  ggtitle("Korean Conflict Deaths by BirthYear")

