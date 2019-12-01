#Created by DZ, July 28, 2019.
#to plot a time line from a dataset
getwd()
library(xlsx)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)
dynasties<- read.xlsx("Imperial_dynasties_of_China.xlsx", sheetIndex = 1)
str(dynasties)

df<- dynasties %>%
  mutate_if(is.factor,as.character) %>%#convert column type to character 
  mutate(Start=ifelse(grepl("[[:space:]]*[Bb]{1}[Cc]{1}", Starting), paste("-", Starting, sep = ""), Starting), End=ifelse(grepl("[[:space:]]*[Bb]{1}[Cc]{1}", Ending), paste("-", Ending, sep = ""), Ending)) %>%#add a negative sign to years with "BC" in Starting and Ending columns
  mutate(Start=gsub("[[:space:]]*[Bb]{1}[Cc]{1}", "", Start), End=gsub("[[:space:]]*[Bb]{1}[Cc]{1}", "", End)) %>%#remove "BC" if it is in the string
  mutate(Start=gsub("[[:space:]]*[Aa]{1}[Dd]{1}", "", Start), End=gsub("[[:space:]]*[Aa]{1}[Dd]{1}", "", End)) %>%#remove "AD" if it is in the string
  mutate(Start= as.integer(Start), End= as.integer(End)) %>%#convert string variables into integer variables
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
########################################
test<- c("a  BC","a Bc","a    bC","a  bc","aBC", "a  b")
#to test whether a pattern exists in a string
grepl("[[:space:]]*[Bb]{1}[Cc]{1}", test)
#to replace a pattern with another string
gsub("[[:space:]]*[Bb]{1}[Cc]{1}", "", test)
#to add a character to the beginning of the string
ifelse(grepl("[[:space:]]*[Bb]{1}[Cc]{1}", test), paste("-", test, sep = ""), test)
########################################
# merge data in Starting and Ending columns to avoid duplicate
test2<- c(df$Starting, df$Ending) %>%
  unique()%>%
  sort()%>%
  {data.frame(Year=.)}
########################################

# plot the time axis and remove all other decorations
# the time line is horizontal
p<- ggplot(data = NULL,aes(x=range(minimum,maximum),y= range(-5, 5)))+ geom_hline(yintercept=0)+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())
#p+ geom_blank()
p+ geom_point(data = df, aes(x=Start,y=0),color= "green")+ geom_point(data = df, aes(x=End,y=0), color= "brown")+ geom_curve(aes(x = Start, y = 0, xend = End, yend = 0, label= Dynasty), data = df, curvature = -0.5, angle = 90)

p1<- ggplot(data = df,aes(x=Start,y= 0,label= Dynasty))+ geom_hline(yintercept=0)+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())

p1+ geom_point(color = "red") +
  geom_text_repel(
    nudge_y      = 0.05,
    direction    = "x",
    angle        = 0,
    vjust        = 0,
    segment.size = 0.2
  ) +
  xlim(-230, 1920) +
  ylim(-1, 1) 

#ggrepel examples
# https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html

# the time line is vertical
p2<- ggplot(data = df,aes(x=0, y= Start,label= Dynasty))+ geom_vline(xintercept=0)+ theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ geom_point(data = df, aes(x= 0, y= Start),color= "green")+ geom_point(data = df, aes(x= 0, y= End), color= "brown")+ geom_curve(aes(x = 0, y= Start, xend = 0, yend= End), data = df, curvature = -0.5, angle = 90)

p2 + xlim(-3, 3) + geom_point(color = "red") +
  scale_y_continuous(position = "left") +
  geom_text_repel(
    nudge_x      = -0.35,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2
  )

p2 + xlim(-3, 3)+ geom_text(check_overlap = TRUE) # check_overlap may cause some texts do not show in the plot
p2 + xlim(-3, 3)+ geom_text(hjust = 0, nudge_x = 0.05)
p2 + xlim(-3, 3)+ geom_text(hjust = 1, nudge_x = -0.05)

p2 + xlim(-3, 3)+ geom_label()
p2 + xlim(-3, 3)  +
  geom_text_repel(
    nudge_x      = -0.35,
    direction    = "y",
    hjust        = 1,
    segment.size = 0.2
  )

p3<- ggplot(data = df,aes(x=0, y= Start))+ theme(panel.background = element_rect(fill="white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ geom_point(data = df, aes(x= 0, y= Start),color= "green")+ geom_point(data = df, aes(x= 0, y= End), color= "brown")+ geom_curve(aes(x = 0, y= Start, xend = 0, yend= End), data = df, curvature = -0.5, angle = 90)

TimeLine<- geom_segment(data = NULL, aes(x = 0, y = minimum*1.15, xend = 0, yend = maximum*1.15), lineend = "round", linejoin = "bevel", size = 1, arrow = arrow(length = unit(0.1, "inches")), inherit.aes = F) 


# re-specify x, y, and label in aes()
p3+ TimeLine+ xlim(-3, 3)+ geom_text(data=df, aes(x=-0.1 ,y= (End + Start)/2,label= Dynasty),hjust = 1, nudge_x = -0.05, inherit.aes = F) + geom_text_repel(data= year_merged,aes(x=0, y= Year, label= YearStr),hjust = 0, nudge_x = 0.2, inherit.aes = F, direction= "y") 

# remove background grey area
theme(panel.background = element_rect(fill, colour, size, linetype, color))
# gremove background grid
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggsave("tl_imperial_dynasties_2019August05_v4.png", width = 8.5, height = 11, units = "in")

# try direct labels
# this doesn't work
p4<- ggplot(data = df,aes(x=0, y= Start))+ geom_vline(xintercept = 0)+ theme(panel.background = element_rect(fill="white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ geom_curve(aes(x = 0, y= Start, xend = 0, yend= End), data = df, curvature = -0.5, angle = 90)
p4

p4+ geom_dl(aes(label=Dynasty),method="top.points")


###################################
x_range<- 6
# it seems there's an issue
y_range<- (maximum- minimum)*1.15/x_range
ratio<- x_range/y_range

df <- df%>%
  mutate(label_pos_x= Range * ratio/ (-3))%>%
  mutate(label_pos_x= ifelse(label_pos_x< -0.2, -0.2, label_pos_x)) %>%
  mutate(label_pos_y= (Start+End)/2)%>%
  mutate(para_a= -label_pos_x/(Start-label_pos_y)^2)

p3+ TimeLine+ xlim(-3, 3)+ geom_text(data=df, aes(x=label_pos_x ,y= label_pos_y,label= Dynasty),hjust = 1, nudge_x = -0.01, inherit.aes = F) + geom_text_repel(data= year_merged,aes(x=0, y= Year, label= YearStr),hjust = 0, nudge_x = 0.2, inherit.aes = F, direction= "y") 


# parabola formula
# x=a*(y-k)^2+ h, vertex:(h,k)
# a= -h/(y1-k)^2 or a= -h/(y2-k)^2

lapply(seq(df$Start[14], df$End[14]), function(y){df$para_a[14]*(y-df$label_pos_y[14])^2 + df$label_pos_x[14]})


# generate curve points for each segment of dynasty
x_vector<- vector()
y_vector<- vector()
index_vector<- vector()
for(i in 1:nrow(df)){
  y_vector<- c(y_vector, seq(df$Start[i], df$End[i]))
  index_vector<- c(index_vector, rep(df$Dynasty[i], length(seq(df$Start[i], df$End[i])) ))
  x_temp<- sapply(seq(df$Start[i], df$End[i]), function(y){df$para_a[i]*(y-df$label_pos_y[i])^2 + df$label_pos_x[i]})
  x_vector<- c(x_vector, x_temp)
}
df_curve<- data.frame("curveX"= x_vector, "curveY"= y_vector, "index"= index_vector)

p5<- ggplot(data = df,aes(x=0, y= Start))+ theme(panel.background = element_rect(fill="white"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(), axis.title.y=element_blank(),axis.text.y=element_blank(),axis.ticks.y=element_blank())+ geom_point(data = df, aes(x= 0, y= Start))+ geom_point(data = df, aes(x= 0, y= End))

# use geom_path() rather than geom_line()
# geom_path() connects the observations in the order in which they appear in the data. 
# geom_line() connects them in order of the variable on the x axis.


# extract colors
colors<-hue_pal()(length(df$Dynasty))
colScale <- scale_colour_manual(values =colors )

colScale

p5 + TimeLine+ xlim(-3, 3)+  geom_text_repel(data= year_merged,aes(x=0, y= Year, label= YearStr, color= 'darkblue'), show.legend = F ,hjust = 0, nudge_x = 0.2, inherit.aes = F, direction= "y")+ geom_text(data=df, aes(x=label_pos_x ,y= label_pos_y,label= Dynasty, color= Dynasty), show.legend = F,hjust = 1, nudge_x = -0.01, inherit.aes = F)+ geom_path(data = df_curve, aes(x=curveX, y=curveY, group= index, color= index), show.legend = F, inherit.aes = F) + scale_y_reverse()
