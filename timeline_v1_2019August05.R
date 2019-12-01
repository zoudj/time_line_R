# Michael Jordan's timeline
# created by JZ, Aug. 05, 2019

getwd()
library(xlsx)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
MJ<- read.xlsx("timeline_Michael_Jordan.xlsx", sheetIndex = 1)
str(MJ)

df<- MJ %>%
  mutate_if(is.factor,as.character) %>%
  mutate(Start=ifelse(grepl("[[:space:]]*[Bb]{1}[Cc]{1}", Starting), paste("-", Starting, sep = ""), Starting), End=ifelse(grepl("[[:space:]]*[Bb]{1}[Cc]{1}", Ending), paste("-", Ending, sep = ""), Ending)) %>%
  mutate(Start=gsub("[[:space:]]*[Bb]{1}[Cc]{1}", "", Start), End=gsub("[[:space:]]*[Bb]{1}[Cc]{1}", "", End)) %>%
  mutate(Start=gsub("[[:space:]]*[Aa]{1}[Dd]{1}", "", Start), End=gsub("[[:space:]]*[Aa]{1}[Dd]{1}", "", End)) %>%
  mutate(Start= as.integer(Start), End= as.integer(End)) %>%
  mutate(End= ifelse(is.na(End), Start, End))%>%
  mutate(Start= ifelse(is.na(Start), End, Start)) %>%
  filter(!is.na(Start) & !is.na(End))%>%
  mutate(Range= End - Start)
  
str(df)
maximum<- max(df$Start,df$End)
minimum<- min(df$Start,df$End)
time_range<- maximum - minimum

year_merged<- c(df$Start, df$End) %>%
  unique()%>%
  sort()%>%
  {data.frame(Year=.)}%>%
  mutate(YearStr= ifelse(Year<0,paste(abs(Year),"BC", sep = ""), as.character(Year)))

str(year_merged)

x_range<- 10
y_range<- (maximum- minimum)*1.15
ratio<- x_range/y_range

df <- df%>%
  mutate(label_pos_x= Range * ratio/3) %>%
  mutate(label_pos_x= ifelse(label_pos_x> 0.2, 0.2, label_pos_x)) %>%
  mutate(label_pos_y= (Start+End)/2)%>%
  mutate(para_a= -label_pos_x/(Start-label_pos_y)^2)

# add empty rows to the dataframe
# no need
for(i in minimum:maximum){
  if(!(i %in% df$Start)){
    df[nrow(df)+1,] <- NA
    df$Start[nrow(df)]<- i
  }
}

df<- df %>%
  mutate(Event= ifelse(is.na(Event), "   ", Event)) %>%
  arrange(Start) %>%
  mutate(End= ifelse(is.na(End), Start, End))%>%
  mutate(Start= ifelse(is.na(Start), End, Start)) %>%
  mutate(Range= End - Start)%>%
  mutate(label_pos_x= Range * ratio/ (-3))%>%
  mutate(label_pos_x= ifelse(label_pos_x< -0.2, -0.2, label_pos_x)) %>%
  mutate(label_pos_y= (Start+End)/2)%>%
  mutate(para_a= -label_pos_x/(Start-label_pos_y)^2)


x_vector<- vector()
y_vector<- vector()
index_vector<- vector()
for(i in 1:nrow(df)){
  if(df$Start[i] == df$End[i]){
    next
  } else {
    y_vector<- c(y_vector, seq(df$Start[i], df$End[i],length.out = 11))
    index_vector<- c(index_vector, rep(df$Event[i], 11 ))
    x_temp<- sapply(seq(df$Start[i], df$End[i],length.out = 11), function(y){df$para_a[i]*(y-df$label_pos_y[i])^2 + df$label_pos_x[i]})
    x_vector<- c(x_vector, x_temp)
  }
  
}
df_curve<- data.frame("curveX"= x_vector, "curveY"= y_vector, "index"= index_vector)%>%
  na.omit()

TimeLine<- geom_segment(data = NULL, aes(x = 3, y = minimum-5, xend = 3, yend = maximum+5), lineend = "round", linejoin = "bevel", size = 1, arrow = arrow(length = unit(0.1, "inches")), inherit.aes = F) 

p1<- ggplot(data = df,aes(x=3, y= Start))+ theme(panel.background = element_rect(fill="white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ geom_point(data = df, aes(x= 3, y= Start))+ geom_point(data = df, aes(x= 3, y= End))

# force (default=1), force of repulsion between overlapping text labels
# min.segment.length = 0.5 (default)
p1 + TimeLine+ xlim(-5, 5)+  geom_text(data= year_merged,aes(x=3, y= Year, label= YearStr, color= 'darkblue'), show.legend = F ,hjust = 0, nudge_x = 0.05, inherit.aes = F)+ geom_text_repel(data=df, aes(x=label_pos_x+3 ,y= label_pos_y,label= Event, color= Event), show.legend = F,min.segment.length = 0, force = 0.1, hjust = 1,nudge_x = -0.1,direction = "y", inherit.aes = F)+ geom_path(data = df_curve, aes(x=curveX+3, y=curveY, group= index, color= index), show.legend = F, inherit.aes = F) + scale_y_reverse()

ggsave("tl_imperial_dynasties_2019August05_v7.png", width = 12, height = 18, units = "in")


# time marks on the left of timeline, texts on the right
TimeLine2<- geom_segment(data = NULL, aes(x = -3, y = minimum-5, xend = -3, yend = maximum+5), lineend = "round", linejoin = "bevel", size = 1, arrow = arrow(length = unit(0.1, "inches")), inherit.aes = F)

p2<- ggplot(data = df,aes(x=-3, y= Start))+ theme(panel.background = element_rect(fill="white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ geom_point(data = df, aes(x= -3, y= Start))+ geom_point(data = df, aes(x= -3, y= End))

p2 + TimeLine2+ xlim(-5, 5)+  geom_text(data= year_merged,aes(x=-3, y= Year, label= YearStr, color= 'darkblue'), show.legend = F ,hjust = 1, nudge_x = -0.1, inherit.aes = F)+ geom_text_repel(data=df, aes(x=label_pos_x-3 ,y= label_pos_y,label= Event, color= Event), show.legend = F,min.segment.length = 0, force = 0.1, hjust = 0,nudge_x = 0.1,direction = "y", inherit.aes = F)+ geom_path(data = df_curve, aes(x=curveX-3, y=curveY, group= index, color= index), show.legend = F, inherit.aes = F) + scale_y_reverse()
