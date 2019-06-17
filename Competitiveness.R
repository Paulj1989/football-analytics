#COMPETITIVENESS

#Gini 

ineq(england$pts)
ineq(germany$pts)
ineq(spain$pts)

#Concentration index

conc(england$pts)
conc(germany$pts)
conc(spain$pts)

#Variances

sd(england$pts)
sd(germany$pts)
sd(spain$pts)

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
           (conc(eng_2015$pts) - HHIlb20)/(HHIub20 - HHIlb20))/5

gerHHI <- sum((conc(ger_2019$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2018$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2017$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2016$pts) - HHIlb18)/(HHIub18 - HHIlb18),
              (conc(ger_2015$pts) - HHIlb18)/(HHIub18 - HHIlb18))/5

espHHI <- sum((conc(esp_2019$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2018$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2017$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2016$pts) - HHIlb20)/(HHIub20 - HHIlb20),
              (conc(esp_2015$pts) - HHIlb20)/(HHIub20 - HHIlb20))/5

espHHI

#ASD

#England

nteams <- 20
max.points <- 114
england$p.i <- england$pts/max.points

eng_2019 <- subset(england, year == "2019")
eng_2018 <- subset(england, year == "2018")
eng_2017 <- subset(england, year == "2017")
eng_2016 <- subset(england, year == "2016")
eng_2015 <- subset(england, year == "2015")

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

#TOTAL

engASD <- round(data.frame(ASD15,ASD16,ASD17,ASD18,ASD19), digits =2)
engCV <- round(data.frame(CV15,CV16,CV17,CV18,CV19), digits =2)

#Germany

nteams <- 18
max.points <- 102
germany$p.i <- germany$pts/max.points

ger_2019 <- subset(germany, year == "2019")
ger_2018 <- subset(germany, year == "2018")
ger_2017 <- subset(germany, year == "2017")
ger_2016 <- subset(germany, year == "2016")
ger_2015 <- subset(germany, year == "2015")

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

#TOTAL

gerASD <- round(data.frame(ASD15,ASD16,ASD17,ASD18,ASD19), digits =2)
gerCV <- round(data.frame(CV15,CV16,CV17,CV18,CV19), digits =2)


#Spain

nteams <- 20
max.points <- 114
spain$p.i <- spain$pts/max.points

esp_2019 <- subset(spain, year == "2019")
esp_2018 <- subset(spain, year == "2018")
esp_2017 <- subset(spain, year == "2017")
esp_2016 <- subset(spain, year == "2016")
esp_2015 <- subset(spain, year == "2015")

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

#TOTAL
espASD <- round(data.frame(ASD15,ASD16,ASD17,ASD18,ASD19), digits =2)
espCV <- round(data.frame(CV15,CV16,CV17,CV18,CV19), digits =2)

#ALL

TOTAL_ASD <- rbind(engASD, espASD, gerASD)

TOTAL_ASD <- TOTAL_ASD %>% rename('2015' = ASD15)
TOTAL_ASD <- TOTAL_ASD %>% rename('2016' = ASD16)
TOTAL_ASD <- TOTAL_ASD %>% rename('2017' = ASD17)
TOTAL_ASD <- TOTAL_ASD %>% rename('2018' = ASD18)
TOTAL_ASD <- TOTAL_ASD %>% rename('2019' = ASD19)

TOTAL_ASD$League <- c("Premier League","La Liga", "Bundesliga")

TOTAL_ASD = TOTAL_ASD[, c(6, 1:5)]

TOTAL_CV <- rbind(engCV, espCV, gerCV)

TOTAL_CV$League <- c("Premier League","La Liga", "Bundesliga")
TOTAL_CV = TOTAL_CV[, c(6, 1:5)]
names(TOTAL_CV) <- c("League", "2015", "2016", "2017", "2018", "2019")


#PLOT
install.packages("reshape")
library(reshape)
N <-melt(rbind(TOTAL_ASD))

asd_plot <- ggplot(data=N, aes(x=variable, y=as.numeric(value), group=League, color=League)) +
  geom_line(size = 1, alpha = 0.8)+
  geom_point(size=4, alpha = 0.6)+xlab("Year")+ylab("ASD")+
  theme_ipsum(base_size = 20, axis_title_size = 20)+
  theme(legend.box.spacing = unit(0.005, "cm"), legend.key.size = unit(0.7, "cm"))+
  labs(color = "League")+
  scale_color_brewer(name = NULL, palette = "Set1")
asd_plot

N2 <-melt(rbind(TOTAL_CV))

cv_plot <- ggplot(data=N2, aes(x=variable, y=as.numeric(value), group=League, color=League)) +
  geom_line(size = 1, alpha = 0.6)+
  geom_point(size=4, alpha = 0.6)+xlab("Year")+ylab("CV")+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(color = "League")+
  scale_color_brewer(name = NULL, palette = "Set1")

ggarrange(asd_plot,cv_plot, common.legend = TRUE, legend="bottom")

#ISD

isd <- 0.5/34
isd <- isd^0.5
isd
#RSD

rsd <- read.csv("~/Documents/FTW/rsd.csv")
N3 <-melt(rbind(rsd))

rsd_plot <- ggplot(data=N3, aes(x=variable, y=as.numeric(value), group=League, color=League)) +
  geom_line(size = 1, alpha = 0.6)+
  geom_point(size=4, alpha = 0.6)+xlab("Year")+ylab("RSD")+
  theme_ipsum(base_size = 18, axis_title_size = 18)+
  labs(color = "League")+
  scale_color_brewer(name = NULL, palette = "Set1")


ggarrange(asd_plot,cv_plot, rsd_plot, common.legend = TRUE, legend="bottom")
