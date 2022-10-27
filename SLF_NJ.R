##Spotted Lanternfly Data
##Alexis O'Callahan

# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tidyverse)
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

#Convert to factors
#NJ_CountyData_SLF is df
NJ_CountyData_SLF$State = as.factor(NJ_CountyData_SLF$State)
NJ_CountyData_SLF$County = as.factor(NJ_CountyData_SLF$County)
NJ_CountyData_SLF$DateBeg1 = as.Date(NJ_CountyData_SLF$DateBeg, "%m/%d/%y")


#Visualize time series by county across day since first sighting of SLF
#all of New Jersey
p <- ggplot(NJ_CountyData_SLF, aes(x=DateBeg1, y=PerCapitaPop, color=County)) +
  geom_line() + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  scale_x_date(limit=c(as.Date("2019-11-01"),as.Date("2021-01-11"))) +
  ylim(0,0.0001)

p

p <- ggplot(NJ_CountyData_SLF, aes(x=DOY, y=PerCapitaPop, color=Year)) +
  geom_line() + 
  xlab("") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylim(0,0.0001) + stat_smooth(method="loess", span=0.1, se=TRUE, aes(fill=PerCapitaPop), alpha=0.3) +
  theme_bw()


#find average observation of SLF in NJ over all years based on what day of year it is (DOY);;

NJ_CountyData_SLF_avgYr <- NJ_CountyData_SLF %>% 
 dplyr::group_by(DOY) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

NJ_AvgDOY = NJ_CountyData_SLF_avgYr[c(1,2,4,7)]

#standard deviation
NJ_CountyData_SLF_sdYr <- NJ_CountyData_SLF %>% 
  dplyr::group_by(DOY) %>%
  summarise_if(is.numeric, sd, na.rm = TRUE)

NJ_AvgDOY2 <- left_join(NJ_AvgDOY, NJ_CountyData_SLF,
                                 by = "DOY", 
                                 suffix = c("", "avg")) 


p <- ggplot(data = NJ_CountyData_SLF, aes(x=DOY, y=Count)) + 
  geom_point(aes(x=DOY, y=Count, color=as.factor(Year), 
                 size=0.8, alpha=0.4)) + 
  scale_color_brewer(palette="Spectral") +
  xlab("Day of Year") +
  stat_summary(geom = "line", fun = mean) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) +
  ylim(0,105)  + guides(colour = guide_legend(override.aes = list(size=10))) +
  theme_bw() + guides(alpha = "none") + guides(size = "none")#0.002 #LOOKS GOOD


p2 = p + geom_line(data = NJ_CountyData_SLF_avgYr, aes(x=DOY, y=Count),
                   color="violet") 

#density
library(viridis)
library(RColorBrewer)
install.packages("Polychrome")
library(Polychrome)
Glasbey = glasbey.colors(21)
swatch(Glasbey)

c25 <- c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
  "darkturquoise", "green1", "yellow4", "yellow3",
  "darkorange4", "brown"
)
nb.cols <- 21
mycolors <- colorRampPalette(brewer.pal(8, "Dark2"))(nb.cols)

d <- ggplot(NJ_CountyData_SLF, aes(x=DOY, fill=County)) +
  geom_density(alpha=0.4) 
d2 <- d + scale_fill_manual(values=c25) #this works! #cumberland early peak; 
#cape may later peak, middlesex early rise, salem early peak+ocean,

#Plot over the last 4 years:
e <- ggplot(NJ_CountyData_SLF, aes(x=DaysSinceFirstSight, fill=County)) +
  geom_density(alpha=0.4) 
e2 <- e + scale_fill_manual(values=c25)
  
#stacked density plot:
f <- ggplot(NJ_CountyData_SLF, aes(x=DaysSinceFirstSight, fill=County)) +
  geom_density(adjust=1.5, position="fill") +
  theme_ipsum()

#small multiple density plots:
# Using Small multiple
g <- ggplot(data=NJ_CountyData_SLF, aes(x=DaysSinceFirstSight, group=County, fill=County)) +
  geom_density(adjust=1.5) +
  theme_ipsum() +
  facet_wrap(~County) +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    axis.ticks.x=element_blank()
  ) #looks good!

#Plot per capita #calculate
#NJ_A
#df[df$Group != "A",]


#Separate by invaders


