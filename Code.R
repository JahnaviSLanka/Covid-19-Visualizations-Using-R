rm(list=ls())

#Covid-19 Cases in Hubei province and other top provinces in CHINA

library(plotly)

data <- read.csv('C:/Users/jahna/Downloads/DV_FinalProject/DV_Data_Code/hubei.csv')
data

fig <-plot_ly(data = data, 
              x = ~Day, 
              y = ~obsvalue,
              color = ~Cases
) %>% 
  layout(legend = list(x=0.9,y=0.95)) %>% 
  layout(
    title = "Case History of Coronavirus in Hubei Province (China)",
    xaxis = list(title = "Dates",
                 categoryorder = "array",
                 categoryarray = ~Day),
    yaxis = list(title = "Number of Cases per day")
  )
fig


data2 <- read.csv('C:/Users/jahna/Downloads/DV_FinalProject/DV_Data_Code/province.csv')
data2

fig2 <-plot_ly(data = data2, 
               x = ~Day, 
               y = ~Confirmed,
               color = ~Province,
               colors= c('#E31A1C', '#33A02C', '#1F78B4', '#FB9A99')
) %>% 
  layout(legend = list(x=0.9,y=0.95)) %>% 
  layout(
    title = "Confirmed Case History of Coronavirus in Guangdong, Henan, Hunan and	Zhejiang Provinces (China)",
    xaxis = list(title = "Dates",
                 categoryorder = "array",
                 categoryarray = ~Day),
    yaxis = list(title = "Number of Cases per day")
  )
fig2


#Showing part to whole relation using Donut Chart for Confirmed, Recovered and Death Cases in CHINA


library(ggplot2)

data <- data.frame(
  category=c("Confirmed", "Deaths", "Recovered"),
  count=c(82052,  3339, 77575)
)
data$fraction = data$count / sum(data$count)

data$ymax = cumsum(data$fraction)
data$ymin = c(0, head(data$ymax, n=-1))

data$category <- factor(data$category, levels = c("Confirmed", "Deaths", "Recovered"))

p1 = ggplot(data, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  coord_polar(theta="y") + geom_rect(color='white') +
  xlim(c(2, 4))

plot<-p1 +  scale_fill_brewer("cases") +
  theme(axis.text.x=element_blank()) + theme(legend.position=c(.5, .5)) + ggtitle("Covid-19 Cases in China") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) + theme(axis.ticks=element_blank()) +
  theme(legend.title = element_text(size=10, face="bold")) +
  theme(legend.text = element_text(size = 10, face = "bold"))
plot
plot +  geom_label(aes(label=paste(round(fraction*100,2),"%"),x=3.5,y=(ymin+ymax)/2),inherit.aes = TRUE, show.legend = FALSE)


---------------------------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------------------------

#Top 10 countries effected by Covid-19, showing confirmed, recovered and deaths

library(dplyr)
library(lubridate)
library(readxl)

Daataset <- read_xlsx("C:/Users/jahna/Downloads/DV_FinalProject/DV_Data_Code/covid.xlsx")
Dataset

library(ggplot2)
library(ggthemes)
library(ggrepel)
options(scipen=999)

Confirmed <- Dataset %>% mutate(label = if_else(Date == max(Date), as.character(Country), NA_character_)) %>%
  ggplot(aes(x = Date, y = Confirmed, group = Country, colour = Country)) + 
  geom_line() + labs(title="COVID-19 - Confirmed Cases for Top 10 Countries") + theme_clean() +
  geom_label_repel(aes(label = label), nudge_x = 0, nudge_y = 0, 
                   na.rm = TRUE, show.legend = FALSE, label.size = 0.25, force=2,
                   segment.alpha = NULL)
Confirmed

Deaths <- Dataset %>%
  mutate(label = if_else(Date == max(Date), as.character(Country), NA_character_)) %>%
  ggplot(aes(x = Date, y = Death, group = Country, colour = Country)) + 
  geom_line() + labs(title="COVID-19 - Deaths for Top 10 Countries") + theme_clean() +
  geom_label_repel(aes(label = label), nudge_x = 0, nudge_y = 0, 
                   na.rm = TRUE, show.legend = FALSE, label.size = 0.25, force=2,
                   segment.alpha = NULL)
Deaths

Recovered <- Dataset %>%
  mutate(label = if_else(Date == max(Date), as.character(Country), NA_character_)) %>%
  ggplot(aes(x = Date, y = Recovered, group = Country, colour = Country)) + 
  geom_line() + labs(title="COVID-19 - Recovered Cases for Top 10 Countries") + theme_clean() +
  geom_label_repel(aes(label = label), nudge_x = 0, nudge_y = 0, 
                   na.rm = TRUE, show.legend = FALSE, label.size = 0.25, force=2, 
                   segment.alpha = NULL)
Recovered

--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------


library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)
library(ggthemes)
library(plotly)
library(readxl)

world <- map_data("world")

data <- read_excel('C:/Users/jahna/Downloads/DV_FinalProject/DV_Data_Code/world_updated.xlsx')

ggplot() + geom_polygon(data = world, aes(x=long, y = lat, group = group),alpha=0.3)+theme_void()

data <- data %>% arrange(confirmed) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate( mytext=paste("Country: ", name, "\n", "Confirmed: ", confirmed, sep=""))

confirmed <-   data %>% ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group),fill='light grey', color='grey',alpha=0.6) +
  geom_point(aes(x=long, y=lat, size=confirmed, text=mytext), color='dark blue',alpha= 0.3) +
  scale_alpha_continuous(trans="log") +  scale_size_continuous(range=c(1,15)) + 
  ggtitle("Covid 19 Confirmed Cases of World") + 
  theme_void() +theme(legend.position = "none")

confirmed <- ggplotly(p, tooltip="text")
confirmed


data_r <- data %>% arrange(recovered) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate( mytext=paste("Country: ", name, "\n", "Recovered: ", recovered, sep=""))

recovered <-   data_r %>% ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group),fill='light grey', color='grey',alpha=0.6) +
  geom_point(aes(x=long, y=lat, size=recovered, text=mytext), color='dark green',alpha= 0.3)+
  scale_alpha_continuous(trans="log") +  scale_size_continuous(range=c(1,15)) + 
  ggtitle("Covid 19 Recoverd Cases of World") + 
  theme_void() +theme(legend.position = "none")

recovered <- ggplotly(p_r, tooltip="text")
recovered


data_d <- data %>% arrange(deaths) %>%
  mutate( name=factor(name, unique(name))) %>%
  mutate( mytext=paste("Country: ", name, "\n", "deaths: ", deaths, sep=""))

deaths <-   data_d %>% ggplot() +
  geom_polygon(data = world, aes(x=long, y = lat, group = group),fill='light grey', color='grey',alpha=0.6) +
  geom_point(aes(x=long, y=lat, size=deaths, text=mytext), color='red',alpha= 0.3)+
  scale_alpha_continuous(trans="log") +  scale_size_continuous(range=c(1,15)) + 
  ggtitle("Covid 19 Death Cases of World") + 
  theme_void() +theme(legend.position = "none")

deaths <- ggplotly(p_d, tooltip="text")
deaths





















