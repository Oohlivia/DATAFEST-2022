

library(tidyverse)
player1 <- read_csv("player-6427031.csv",guess_max=2106600)
main <- read_csv("logs.csv", guess_max = 2106600)


category <- unique(main_data$event_category)
last <- main_data$event_category[1]
time <- main_data$event_time_dbl[1]
v<- c()
df<-c()
for (i in 2:length(main_data$event_category)) {
  current<- main_data$event_category[i]
  if(current!=last){
    #v<-c(v,time)
    df<-rbind(df,c(main_data$player_id[i-1] ,last, time))
  }
  last <- current
  time <- main_data$event_time_dbl[i]
}
df
dfdf <- data.frame(playerID = df[,1], category = df[,2], cumu_time=as.numeric(df[,3]))
dfdf



total_player <-dfdf %>% 
  group_by(playerID) %>% 
  count()
total_player$playerID


individual<-function(d_f){
  category <- unique(d_f$category)
  last <- d_f$category[1]
  time <- d_f$cumu_time[1]
  v<- c()
  df<-c()
  for (i in 2:length(d_f$category)) {
    current<- d_f$category[i]
    if(current!=last){
      #v<-c(v,time)
      df<-rbind(df,c(d_f$playerID[1] ,last, time))
    }
    last <- current
    time <- d_f$cumu_time[i]
  }
  #df<-data.frame(playerID = df[,1], category = df[,2], cumu_time=as.numeric(df[,3]))
  df <- data_frame(playerID = df[,1],category = df[,2], time_spend=diff(c(0,as.numeric(df[,3]))))
  output<-df %>% 
    group_by(category) %>% 
    summarise(sum = sum(time_spend))
  
  #output <- df %>% 
  # group_by(playerID, category) %>% 
  #summarise(time_spend = diff(c(0,df[,3])))
  cbind(ID=df$playerID[1],output)
}


a <-c()
#begin <- individual(dfdf[dfdf$playerID==total_player$playerID[1],])
for(i in 1:length(total_player$playerID)){
  input <- individual(dfdf[dfdf$playerID==total_player$playerID[i],])
  a<-rbind(a, input)
}
a # 166 player's time-spend on each event category 

answer <- a %>% group_by(category) %>% 
  summarise(total=sum(sum)) %>% 
  arrange(desc(total))


player_session <- read_csv("player_session_time_version3.csv", guess_max = 2106600)
colnames(player_session) <- c("ID", "Session", "Activity Category", "Total")

average_time_by_session <- player_session %>% 
  group_by(Session, `Activity Category`) %>% 
  summarise(Average = sum(Total)/166)

  
p <- ggplot(average_time_by_session, aes(x = Session, y = Average ))
p

p + geom_point(aes(color = `Activity Category`)) + 
  scale_colour_manual(values = rainbow(11))

p + geom_point( aes(color = `Activity Category`)) + 
  scale_colour_manual(values = rainbow(11)) +
  ylab("Average Time (s)") +
  ggtitle("Average Player Time by Session") +
  scale_x_discrete(labels= 1:9)
