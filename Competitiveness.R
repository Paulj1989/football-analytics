#COMPETITIVENESS
library(tidyverse)
library(hrbrthemes)
library(ggthemes)
library(extrafont)
library(ggpubr)
library(ggthemr)
library(ggsci)


time3 <- read_csv("~/Documents/GitHub Directory/ftw/time3.csv")
time3$year <- as.factor(time3$year)

time <- time3 %>% 
  filter(year == '14/15' | year == '15/16' | year == '16/17' | year == '17/18' | year == '18/19')

#Plot distributions

ggthemr('greyscale', layout = "scientific", text_size = 25)

facet_pts <- ggplot(time, aes(pts, color = year))+
  geom_density(alpha=0.6, size = 0.8)+
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        legend.box.spacing = unit(0.005, "cm"),
        legend.key.size = unit(0.7, "cm"))+
  xlim(16,100)+
  scale_color_lancet()+
  labs(title = NULL, y = NULL, x = "Points") + facet_wrap(~country, ncol = 3)

facet_pts

#Subset

germany <- filter(time3, country == "Bundesliga")
england <- filter(time3, country == "Premier League")
spain <- filter(time3, country == "La Liga")

#Gini 


(sum(ineq(eng_2019$pts), ineq(eng_2018$pts), ineq(eng_2017$pts),
     ineq(eng_2016$pts), ineq(eng_2015$pts), ineq(eng_2014$pts),
     ineq(eng_2013$pts), ineq(eng_2012$pts), ineq(eng_2011$pts)))/9

(sum(ineq(ger_2019$pts), ineq(ger_2018$pts), ineq(ger_2017$pts),
     ineq(ger_2016$pts), ineq(ger_2015$pts), ineq(ger_2014$pts),
     ineq(ger_2013$pts), ineq(ger_2012$pts), ineq(ger_2011$pts)))/9

(sum(ineq(esp_2019$pts), ineq(esp_2018$pts), ineq(esp_2017$pts),
     ineq(esp_2016$pts), ineq(esp_2015$pts), ineq(esp_2014$pts),
     ineq(esp_2013$pts), ineq(esp_2012$pts), ineq(esp_2011$pts)))/9

#Concentration index

(sum(conc(eng_2019$pts), conc(eng_2018$pts), conc(eng_2017$pts),
     conc(eng_2016$pts), conc(eng_2015$pts), conc(eng_2014$pts),
     conc(eng_2013$pts), conc(eng_2012$pts), conc(eng_2011$pts)))/9

(sum(conc(ger_2019$pts), conc(ger_2018$pts), conc(ger_2017$pts),
     conc(ger_2016$pts), conc(ger_2015$pts), conc(ger_2014$pts),
     conc(ger_2013$pts), conc(ger_2012$pts), conc(ger_2011$pts)))/9

(sum(conc(esp_2019$pts), conc(esp_2018$pts), conc(esp_2017$pts),
     conc(esp_2016$pts), conc(esp_2015$pts), conc(esp_2014$pts),
     conc(esp_2013$pts), conc(esp_2012$pts), conc(esp_2011$pts)))/9

#Variances

(sum(sd(eng_2019$pts), sd(eng_2018$pts), sd(eng_2017$pts),
     sd(eng_2016$pts), sd(eng_2015$pts), sd(eng_2014$pts),
     sd(eng_2013$pts), sd(eng_2012$pts), sd(eng_2011$pts)))/9

(sum(sd(ger_2019$pts), sd(ger_2018$pts), sd(ger_2017$pts),
     sd(ger_2016$pts), sd(ger_2015$pts), sd(ger_2014$pts),
     sd(ger_2013$pts), sd(ger_2012$pts), sd(ger_2011$pts)))/9

(sum(sd(esp_2019$pts), sd(esp_2018$pts), sd(esp_2017$pts),
     sd(esp_2016$pts), sd(esp_2015$pts), sd(esp_2014$pts),
     sd(esp_2013$pts), sd(esp_2012$pts), sd(esp_2011$pts)))/9

#HHI

HHIlb20 <-1/20
HHIlb18 <- 1/18

HHIub20 <- 2*(2*20-1)
HHIub20 <- HHIub20/1140

HHIub18 <- 2*(2*18-1)
HHIub18 <- HHIub18/918

#Germany 


#England

engHHI <- sum((conc(eng_2019$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2018$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2017$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2016$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2015$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2014$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2013$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2012$pts) - HHIlb20)/(HHIub20 - HHIlb20),
           (conc(eng_2011$pts) - HHIlb20)/(HHIub20 - HHIlb20))/9

gerHHI <- sum((conc(ger_2019$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2018$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2017$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2016$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2015$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2014$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2013$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2012$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2011$pts) - HHIlb18)/(HHIub18 - HHIlb18))/9

espHHI <- sum((conc(esp_2019$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2018$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2017$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2016$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2015$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2014$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2013$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2012$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2011$pts) - HHIlb20)/(HHIub20 - HHIlb20))/9

gerHHI

#ASD

#England

nteams <- 20
max.points <- 114
england$p.i <- england$pts/max.points

eng_2019 <- subset(england, year == "18/19")
eng_2018 <- subset(england, year == "17/18")
eng_2017 <- subset(england, year == "16/17")
eng_2016 <- subset(england, year == "15/16")
eng_2015 <- subset(england, year == "14/15")
eng_2014 <- subset(england, year == "13/14")
eng_2013 <- subset(england, year == "12/13")
eng_2012 <- subset(england, year == "11/12")
eng_2011 <- subset(england, year == "10/11")

#2019

mpr <- sum(eng_2019$p.i/nteams)
eng_2019$sump <- eng_2019$p.i - mpr
eng_2019$sump <- (eng_2019$sump)^2
sum <- sum(eng_2019$sump)
ASD19 <- sqrt(sum)
CV19 <- ASD19/mpr

#2018

mpr <- sum(eng_2018$p.i/nteams)
eng_2018$sump <- eng_2018$p.i - mpr
eng_2018$sump <- (eng_2018$sump)^2
sum <- sum(eng_2018$sump)
ASD18 <- sqrt(sum)
CV18 <- ASD18/mpr

#2017

mpr <- sum(eng_2017$p.i/nteams)
eng_2017$sump <- eng_2017$p.i - mpr
eng_2017$sump <- (eng_2017$sump)^2
sum <- sum(eng_2017$sump)
ASD17 <- sqrt(sum)
CV17 <- ASD17/mpr

#2016

mpr <- sum(eng_2016$p.i/nteams)
eng_2016$sump <- eng_2016$p.i - mpr
eng_2016$sump <- (eng_2016$sump)^2
sum <- sum(eng_2016$sump)
ASD16 <- sqrt(sum)
CV16 <- ASD16/mpr

#2015

mpr <- sum(eng_2015$p.i/nteams)
eng_2015$sump <- eng_2015$p.i - mpr
eng_2015$sump <- (eng_2015$sump)^2
sum <- sum(eng_2015$sump)
ASD15 <- sqrt(sum)
CV15 <- ASD15/mpr

#2014

mpr <- sum(eng_2014$p.i/nteams)
eng_2014$sump <- eng_2014$p.i - mpr
eng_2014$sump <- (eng_2014$sump)^2
sum <- sum(eng_2014$sump)
ASD14 <- sqrt(sum)
CV14 <- ASD14/mpr

#2013

mpr <- sum(eng_2013$p.i/nteams)
eng_2013$sump <- eng_2013$p.i - mpr
eng_2013$sump <- (eng_2013$sump)^2
sum <- sum(eng_2013$sump)
ASD13 <- sqrt(sum)
CV13 <- ASD13/mpr

#2012

mpr <- sum(eng_2012$p.i/nteams)
eng_2012$sump <- eng_2012$p.i - mpr
eng_2012$sump <- (eng_2012$sump)^2
sum <- sum(eng_2012$sump)
ASD12 <- sqrt(sum)
CV12 <- ASD12/mpr

#2011

mpr <- sum(eng_2011$p.i/nteams)
eng_2011$sump <- eng_2011$p.i - mpr
eng_2011$sump <- (eng_2011$sump)^2
sum <- sum(eng_2011$sump)
ASD11 <- sqrt(sum)
CV11 <- ASD11/mpr


#TOTAL

engASD <- round(data.frame(ASD11, ASD12, ASD13, ASD14 ,ASD15,ASD16,ASD17,ASD18,ASD19), digits =2)
engCV <- round(data.frame(CV11, CV12, CV13, CV14, CV15, CV16, CV17, CV18, CV19), digits =2)

#Germany

nteams <- 18
max.points <- 102
germany$p.i <- germany$pts/max.points

ger_2019 <- subset(germany, year == "18/19")
ger_2018 <- subset(germany, year == "17/18")
ger_2017 <- subset(germany, year == "16/17")
ger_2016 <- subset(germany, year == "15/16")
ger_2015 <- subset(germany, year == "14/15")
ger_2014 <- subset(germany, year == "13/14")
ger_2013 <- subset(germany, year == "12/13")
ger_2012 <- subset(germany, year == "11/12")
ger_2011 <- subset(germany, year == "10/11")

#2019

mpr <- sum(ger_2019$p.i/nteams)
ger_2019$sump <- ger_2019$p.i - mpr
ger_2019$sump <- (ger_2019$sump)^2
sum <- sum(ger_2019$sump)
ASD19 <- sqrt(sum)
CV19 <- ASD19/mpr

#2018

mpr <- sum(ger_2018$p.i/nteams)
ger_2018$sump <- ger_2018$p.i - mpr
ger_2018$sump <- (ger_2018$sump)^2
sum <- sum(ger_2018$sump)
ASD18 <- sqrt(sum)
CV18 <- ASD18/mpr

#2017

mpr <- sum(ger_2017$p.i/nteams)
ger_2017$sump <- ger_2017$p.i - mpr
ger_2017$sump <- (ger_2017$sump)^2
sum <- sum(ger_2017$sump)
ASD17 <- sqrt(sum)
CV17 <- ASD17/mpr

#2016

mpr <- sum(ger_2016$p.i/nteams)
ger_2016$sump <- ger_2016$p.i - mpr
ger_2016$sump <- (ger_2016$sump)^2
sum <- sum(ger_2016$sump)
ASD16 <- sqrt(sum)
CV16 <- ASD16/mpr

#2015

mpr <- sum(ger_2015$p.i/nteams)
ger_2015$sump <- ger_2015$p.i - mpr
ger_2015$sump <- (ger_2015$sump)^2
sum <- sum(ger_2015$sump)
ASD15 <- sqrt(sum)
CV15 <- ASD15/mpr

#2014

mpr <- sum(ger_2014$p.i/nteams)
ger_2014$sump <- ger_2014$p.i - mpr
ger_2014$sump <- (ger_2014$sump)^2
sum <- sum(ger_2014$sump)
ASD14 <- sqrt(sum)
CV14 <- ASD14/mpr

#2013

mpr <- sum(ger_2013$p.i/nteams)
ger_2013$sump <- ger_2013$p.i - mpr
ger_2013$sump <- (ger_2013$sump)^2
sum <- sum(ger_2013$sump)
ASD13 <- sqrt(sum)
CV13 <- ASD13/mpr

#2012

mpr <- sum(ger_2012$p.i/nteams)
ger_2012$sump <- ger_2012$p.i - mpr
ger_2012$sump <- (ger_2012$sump)^2
sum <- sum(ger_2012$sump)
ASD12 <- sqrt(sum)
CV12 <- ASD12/mpr

#2011

mpr <- sum(ger_2011$p.i/nteams)
ger_2011$sump <- ger_2011$p.i - mpr
ger_2011$sump <- (ger_2011$sump)^2
sum <- sum(ger_2011$sump)
ASD11 <- sqrt(sum)
CV11 <- ASD11/mpr


#TOTAL

gerASD <- round(data.frame(ASD11, ASD12, ASD13, ASD14, ASD15,ASD16,ASD17,ASD18,ASD19), digits =2)
gerCV <- round(data.frame(CV11, CV12, CV13, CV14, CV15,CV16,CV17,CV18,CV19), digits =2)


#Spain

nteams <- 20
max.points <- 114
spain$p.i <- spain$pts/max.points

esp_2019 <- subset(spain, year == "18/19")
esp_2018 <- subset(spain, year == "17/18")
esp_2017 <- subset(spain, year == "16/17")
esp_2016 <- subset(spain, year == "15/16")
esp_2015 <- subset(spain, year == "14/15")
esp_2014 <- subset(spain, year == "13/14")
esp_2013 <- subset(spain, year == "12/13")
esp_2012 <- subset(spain, year == "11/12")
esp_2011 <- subset(spain, year == "10/11")
#2019

mpr <- sum(esp_2019$p.i/nteams)
esp_2019$sump <- esp_2019$p.i - mpr
esp_2019$sump <- (esp_2019$sump)^2
sum <- sum(esp_2019$sump)
ASD19 <- sqrt(sum)
CV19 <- ASD19/mpr

#2018

mpr <- sum(esp_2018$p.i/nteams)
esp_2018$sump <- esp_2018$p.i - mpr
esp_2018$sump <- (esp_2018$sump)^2
sum <- sum(esp_2018$sump)
ASD18 <- sqrt(sum)
CV18 <- ASD18/mpr

#2017

mpr <- sum(esp_2017$p.i/nteams)
esp_2017$sump <- esp_2017$p.i - mpr
esp_2017$sump <- (esp_2017$sump)^2
sum <- sum(esp_2017$sump)
ASD17 <- sqrt(sum)
CV17 <- ASD17/mpr

#2016

mpr <- sum(esp_2016$p.i/nteams)
esp_2016$sump <- esp_2016$p.i - mpr
esp_2016$sump <- (esp_2016$sump)^2
sum <- sum(esp_2016$sump)
ASD16 <- sqrt(sum)
CV16 <- ASD16/mpr

#2015

mpr <- sum(esp_2015$p.i/nteams)
esp_2015$sump <- esp_2015$p.i - mpr
esp_2015$sump <- (esp_2015$sump)^2
sum <- sum(esp_2015$sump)
ASD15 <- sqrt(sum)
CV15 <- ASD15/mpr

#2014

mpr <- sum(esp_2014$p.i/nteams)
esp_2014$sump <- esp_2014$p.i - mpr
esp_2014$sump <- (esp_2014$sump)^2
sum <- sum(esp_2014$sump)
ASD14 <- sqrt(sum)
CV14 <- ASD14/mpr

#2013

mpr <- sum(esp_2013$p.i/nteams)
esp_2013$sump <- esp_2013$p.i - mpr
esp_2013$sump <- (esp_2013$sump)^2
sum <- sum(esp_2013$sump)
ASD13 <- sqrt(sum)
CV13 <- ASD13/mpr

#2012

mpr <- sum(esp_2012$p.i/nteams)
esp_2012$sump <- esp_2012$p.i - mpr
esp_2012$sump <- (esp_2012$sump)^2
sum <- sum(esp_2012$sump)
ASD12 <- sqrt(sum)
CV12 <- ASD12/mpr

#2011

mpr <- sum(esp_2011$p.i/nteams)
esp_2011$sump <- esp_2011$p.i - mpr
esp_2011$sump <- (esp_2011$sump)^2
sum <- sum(esp_2011$sump)
ASD11 <- sqrt(sum)
CV11 <- ASD11/mpr


#TOTAL
espASD <- round(data.frame(ASD11, ASD12, ASD13, ASD14, ASD15,ASD16,ASD17,ASD18,ASD19), digits =2)
espCV <- round(data.frame(CV11, CV12, CV13, CV14, CV15,CV16,CV17,CV18,CV19), digits =2)

#ALL

TOTAL_ASD <- rbind(engASD, espASD, gerASD)
TOTAL_ASD <- as_tibble(TOTAL_ASD)


TOTAL_ASD <- TOTAL_ASD %>%
  dplyr::rename('18/19' = ASD19, '17/18' = ASD18, '16/17' = ASD17,'15/16' = ASD16,
                '14/15' = ASD15, '13/14' = ASD14, '12/13' = ASD13, '11/12' = ASD12,
                '10/11' = ASD11)


TOTAL_ASD$League <- c("Premier League","La Liga", "Bundesliga")

TOTAL_ASD <- TOTAL_ASD %>%
  select('League', '10/11', '11/12', '12/13', '13/14', '14/15',
         '15/16', '16/17', '17/18', '18/19')


TOTAL_CV <- rbind(engCV, espCV, gerCV)
TOTAL_CV <- as_tibble(TOTAL_CV)

TOTAL_CV <- TOTAL_CV %>%
  dplyr::rename('18/19' = CV19, '17/18' = CV18, '16/17' = CV17,'15/16' = CV16,
                '14/15' = CV15, '13/14' = CV14, '12/13' = CV13, '11/12' = CV12,
                '10/11' = CV11)


TOTAL_CV$League <- c("Premier League","La Liga", "Bundesliga")

TOTAL_CV <- TOTAL_CV %>%
  select('League', '10/11', '11/12', '12/13', '13/14', '14/15',
         '15/16', '16/17', '17/18', '18/19')


#PLOT
ggthemr('greyscale', layout = "scientific", text_size = 25)
N <- TOTAL_ASD %>% 
  gather('10/11', '11/12', '12/13', '13/14', '14/15',
         '15/16', '16/17', '17/18', '18/19', key = "Season", value = "ASD")

asd_plot <- ggplot(data=N, aes(x=Season, y=as.numeric(ASD), group=League, color=League)) +
  geom_line(size = 1, alpha = 1)+
  geom_point(size=4, alpha = 1)+xlab("Year")+ylab("ASD")+
  scale_color_lancet()+
  theme(legend.box.spacing = unit(0.005, "cm"), legend.key.size = unit(0.7, "cm"))+
  labs(color = NULL)
asd_plot

N2 <- TOTAL_CV %>% 
  gather('10/11', '11/12', '12/13', '13/14', '14/15',
         '15/16', '16/17', '17/18', '18/19', key = "Season", value = "CV")

cv_plot <- ggplot(data=N2, aes(x=Season, y=as.numeric(CV), group=League, color=League)) +
  geom_line(size = 1, alpha = 1)+
  geom_point(size=4, alpha = 1)+xlab("Year")+ylab("CV")+
  scale_color_lancet()+
  labs(color = "League")
ggarrange(asd_plot,cv_plot, common.legend = TRUE, legend="bottom")

