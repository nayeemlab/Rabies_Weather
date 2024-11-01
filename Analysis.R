library(MASS)
require(foreign)
require(maptools)
library(tidyverse)
library(betareg)
library(car)
library(gapminder)
library(dplyr)
library(ggplot2)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(psych) # for descriptive analysis
library(forecast)
library(lmtest)
library(tseries)
library(forecast)
library(MASS)
library(tseries)
library(forecast)
library(lubridate)
library(ggplot2)
library(zoo)
library(Rcpp)
library(prophet)
library(data.table)
library(dplyr)
library(psych)
require(MASS) # to access Animals data sets
require(scales) # to access break formatting functions
library(mgcv)
library(GGally)
library(mgcv)
library(visreg)

setwd('E:\\ResearchProject\\Sumon Bhai\\Rabies Weather')
Rabies <- read.csv("Rabies_Weather_Data.csv")

#Rabies Case
YearwiseRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$Year), FUN=sum)
YearwiseRabies$Category
YearwiseRabies$x

colnames(YearwiseRabies) <- c("Year", "RabiesCase")
describe(YearwiseRabies$RabiesCase)

BeforeInitiative <- YearwiseRabies[which(YearwiseRabies$Year<='2013'),]
describe(BeforeInitiative$RabiesCase)

AfterInitiative <- YearwiseRabies[which(YearwiseRabies$Year>'2013'),]
describe(AfterInitiative$RabiesCase)

t.test(BeforeInitiative$RabiesCase, AfterInitiative$RabiesCase,  
       alternative = "two.sided", 
       var.equal = FALSE)


#ARV
YearwiseARV <- aggregate(Rabies$ARV, by=list(Category=Rabies$Year), FUN=sum)
YearwiseARV$Category
YearwiseARV$x

colnames(YearwiseARV) <- c("Year", "ARV")
describe(YearwiseARV$ARV)

BeforeInitiative <- YearwiseARV[which(YearwiseARV$Year<='2013'),]
describe(BeforeInitiative$ARV)

AfterInitiative <- YearwiseARV[which(YearwiseARV$Year>'2013'),]
describe(AfterInitiative$ARV)

t.test(BeforeInitiative$ARV, AfterInitiative$ARV,  
       alternative = "two.sided", 
       var.equal = FALSE)



#MDV
YearwiseMDV <- aggregate(Rabies$MDV, by=list(Category=Rabies$Year), FUN=sum)
YearwiseMDV$Category
YearwiseMDV$x

colnames(YearwiseMDV) <- c("Year", "MDV")
describe(YearwiseMDV$MDV)

BeforeInitiative <- YearwiseMDV[which(YearwiseMDV$Year<='2013'),]
describe(BeforeInitiative$MDV)

AfterInitiative <- YearwiseMDV[which(YearwiseMDV$Year>'2013'),]
describe(AfterInitiative$MDV)

t.test(BeforeInitiative$MDV, AfterInitiative$MDV,  
       alternative = "two.sided", 
       var.equal = FALSE)



#Rainfall
YearwiseRainfall <- aggregate(Rabies$Rainfall, by=list(Category=Rabies$Year), FUN=sum)
YearwiseRainfall$Category
YearwiseRainfall$x

colnames(YearwiseRainfall) <- c("Year", "Rainfall")
describe(YearwiseRainfall$Rainfall)

BeforeInitiative <- YearwiseRainfall[which(YearwiseRainfall$Year<='2013'),]
describe(BeforeInitiative$Rainfall)

AfterInitiative <- YearwiseRainfall[which(YearwiseRainfall$Year>'2013'),]
describe(AfterInitiative$Rainfall)

t.test(BeforeInitiative$Rainfall, AfterInitiative$Rainfall,  
       alternative = "two.sided", 
       var.equal = FALSE)




#Temp
YearwiseAvgT <- aggregate(Rabies$AvgT, by=list(Category=Rabies$Year), FUN=mean)
YearwiseAvgT$Category
YearwiseAvgT$x

colnames(YearwiseAvgT) <- c("Year", "AvgT")
describe(YearwiseAvgT$AvgT)

BeforeInitiative <- YearwiseAvgT[which(YearwiseAvgT$Year<='2013'),]
describe(BeforeInitiative$AvgT)

AfterInitiative <- YearwiseAvgT[which(YearwiseAvgT$Year>'2013'),]
describe(AfterInitiative$AvgT)

t.test(BeforeInitiative$AvgT, AfterInitiative$AvgT,  
       alternative = "two.sided", 
       var.equal = FALSE)




#Pre-monsoon
PreMonsoon <- Rabies[which(Rabies$Season=='Pre-monsoon'),]
PreMonsoon$Season

#Descriptive PreMonsoon
describe(PreMonsoon$RabiesCase)
describe(PreMonsoon$ARV)
describe(PreMonsoon$MDV)
describe(PreMonsoon$Rainfall)
describe(PreMonsoon$AvgT)


#Subset Monsoon
Monsoon <- Rabies[which(Rabies$Season=='Rainy'),]
Monsoon$Season

#Descriptive (2006-2013)
describe(Monsoon$RabiesCase)
describe(Monsoon$ARV)
describe(Monsoon$MDV)
describe(Monsoon$Rainfall)
describe(Monsoon$AvgT)




#Subset Monsoon
Winter <- Rabies[which(Rabies$Season=='Winter'),]
Winter$Season

#Descriptive Winter
describe(Winter$RabiesCase)
describe(Winter$ARV)
describe(Winter$MDV)
describe(Winter$Rainfall)
describe(Winter$AvgT)


NROW(BeforeInitiative$RabiesCase)
NROW(AfterInitiative$RabiesCase)

summary(aov(Rabies$RabiesCase ~ Rabies$Season))

summary(aov(Rabies$ARV ~ Rabies$Season))

summary(aov(Rabies$MDV ~ Rabies$Season))

summary(aov(Rabies$Rainfall ~ Rabies$Season))

summary(aov(Rabies$AvgT ~ Rabies$Season))


library(ggpubr)
library(rstatix)
bxp <- ggboxplot(Rabies, x = "Season", y = "RabiesCase", fill = "Season", 
                 palette = c("#00AFBB", "#E7B800", "#FC4E07")) + scale_x_discrete(limits = c("Monsoon", "Pre-monsoon", "Winter"))



stat.test <- Rabies %>% t_test(RabiesCase ~ Season)
stat.test

# Box plot
stat.test <- stat.test %>% add_xy_position(x = "Season")
bxp + stat_pvalue_manual(stat.test, label = "p.adj.signif")


bxp <- bxp + 
  stat_pvalue_manual(
    stat.test, label = "p.adj.signif", tip.length = 0.01,
    bracket.shorten = 0.05
  )  + theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15)) + ylab("Rabies Cases")

bxp

tiff("bxp.tiff", units="in", width=8, height=6, res=300)
gridExtra::grid.arrange(bxp)
dev.off()




library(ggpubr)
library(ggplot2)
library(extrafont)
library(GGally)
library(MASS)

RabiesCorr <- cbind(Rabies$Rainfall, Rabies$AvgT, Rabies$ARV, Rabies$MDV, Rabies$RabiesCase)

colnames(RabiesCorr) <- c("Rainfall", "Tempurature", "ARV", "MDV", "Rabies Cases")

ggpairs(data.frame(RabiesCorr))

y <- lm(Rabies$RabiesCase ~ Rabies$Rainfall)
summary(y)
y <- lm(Rabies$RabiesCase ~ Rabies$AvgT)
summary(y)
y <- lm(Rabies$RabiesCase ~ Rabies$ARV)
summary(y)
y <- lm(Rabies$RabiesCase ~ Rabies$MDV)
summary(y)


y <- lm(Rabies$MDV ~ Rabies$Rainfall)
summary(y)
y <- lm(Rabies$MDV ~ Rabies$AvgT)
summary(y)
y <- lm(Rabies$MDV ~ Rabies$ARV)
summary(y)


y <- lm(Rabies$ARV ~ Rabies$Rainfall)
summary(y)
y <- lm(Rabies$ARV ~ Rabies$AvgT)
summary(y)

y <- lm(Rabies$AvgT ~ Rabies$Rainfall)
summary(y)


library(GGally)
CorrPlot <- ggpairs(data.frame(RabiesCorr),  
                    lower = list(continuous = "smooth"))
CorrPlot

#Yearly Data
YearwiseRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$Year), FUN=sum)
YearwiseRabies$Category
YearwiseRabies$x
YearwiseARV <- aggregate(Rabies$ARV, by=list(Category=Rabies$Year), FUN=sum)
YearwiseARV$x
YearwiseMDV <- aggregate(Rabies$MDV, by=list(Category=Rabies$Year), FUN=sum)
YearwiseMDV$x

library(tidyverse)
df2<-data.frame(Years = c("2006", "2007", "2008", "2009", "2010", "2011", "2012", 
                             "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                             "2020", "2021", "2022", "2023", "2024"),
                value = c(167, 166, 165, 164, 104, 109,  82,  82, 106,  83,  66,  80,  60,  57,  26,  
                          38,  47,  47,  28))


options(scipen = 999) ## To disable scientific notation
YRC <- ggplot(df2, aes(x = Years, y = value))+ geom_col(fill = "#0073C2FF")+
  theme_bw()+
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.position = c(0.2, 0.8),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+ylab("Rabies Cases")

YRC

#Monthly Data
MonthwiseRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$Month), FUN=mean)
MonthwiseRabies$Category
MonthwiseRabies$x

colnames(MonthwiseRabies) <- c("Months", "RabiesCase")

Rabies$ARVNoMiss <- replace(Rabies$ARV, is.na(Rabies$ARV),0)
MonthwiseARV <- aggregate(Rabies$ARVNoMiss, by=list(Category=Rabies$Month), FUN=mean)
MonthwiseARV$x
colnames(MonthwiseARV) <- c("Months", "ARV")

Rabies$MDVNoMiss <- replace(Rabies$MDV, is.na(Rabies$MDV),0)
MonthwiseMDV <- aggregate(Rabies$MDVNoMiss, by=list(Category=Rabies$Month), FUN=mean)
MonthwiseMDV$x
colnames(MonthwiseMDV) <- c("Months", "MDV")

library(tidyverse)

df2<-data.frame(Months = c(seq(1,12)),
                value = c(9.58, 8.68, 5.95, 7.63,  5.16, 6.05, 5.16, 6.68, 7.21, 8.50, 8.61, 10.50))


options(scipen = 999) ## To disable scientific notation
MRC <- ggplot(df2, aes(x = Months, y = value))+geom_col(fill = "#0073C2FF")+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec")) + ylab("Rabies Cases")

MRC

tiff("barplotYearMonth.tiff", units="in", width=10, height=12, res=300)
gridExtra::grid.arrange(YRC, MRC, nrow=2, ncol=1)
dev.off()

df2 <- data.frame(Vaccines=rep(c("ARV", "MDV"), each=13),
                  Years=rep(c(YearwiseARV$Year[6:18]),2),
                  Numbers=c(YearwiseARV$ARV[6:18],YearwiseMDV$MDV[6:18]))

# Change the colors manually
p <- ggplot(data=df2, aes(x=factor(Years), y=Numbers/100000, fill=Vaccines)) +
  geom_bar(position="dodge", stat="identity")+
  theme_minimal()+    theme_bw()+
  theme(legend.title=element_blank(),
        legend.text = element_text(size=15),
        legend.position = c(0.2, 0.8),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+ xlab("Years") + ylab("Vaccines Used (in 100,000)")
options(scipen=0)
# Use custom colors
p + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
p<- p + scale_fill_brewer(palette="Dark2") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p

df2 <- data.frame(Vaccines=rep(c("ARV", "MDV"), each=12),
                  Months=rep(c(MonthwiseARV$Months),2),
                  Numbers=c(MonthwiseARV$ARV,
                            MonthwiseMDV$MDV))

options(scipen=0)
# Change the colors manually
q <- ggplot(data=df2, aes(x=Months, y=Numbers/100000, fill=Vaccines)) +
  geom_bar(position="dodge", stat="identity")+
  
  theme_minimal() + theme_bw() +
  theme(legend.title=element_blank(),
        legend.text = element_text(size=15),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+ylab("Rabies Cases") + ylab("Vaccines Used (in 100,000)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))
# Use custom colors
q + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
q <- q + scale_fill_brewer(palette="Dark2")+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
q


tiff("barplotARVMDV.tiff", units="in", width=10, height=12, res=300)
gridExtra::grid.arrange(p, q, nrow=2, ncol=1)
dev.off()







# Data

library(tidyverse)

df1<-data.frame(TYPE = c(rep("ARV", 2), rep("MDV", 2)),
                Initiative = rep(c(seq(1,2)),2),
                value = c(108866, 237021.4,
                          30872, 237172.9))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 2)),
                Initiative = c(seq(1,2)),
                value = c(129.88,  58.00))


options(scipen = 999) ## To disable scientific notation
barplotInter <- ggplot() + 
  geom_col(data = df1, aes(x = Initiative , y = value, fill = TYPE), position = position_dodge()) +
  scale_fill_manual("", values = c("ARV" = "#FFD580", "MDV" = "skyblue"))+
  geom_point(data = df2, aes(x = Initiative, y = value*1000,  group = TYPE, col = TYPE)) + 
  geom_line(data = df2, aes(x = Initiative, y = value*1000, group = TYPE, col = TYPE)) +
  scale_color_manual("", values = c("Annual Mean Rabies Cases" = "#FF7F7F"))+
  scale_y_continuous(name = "Annual Mean Vaccines",
                     sec.axis = sec_axis(trans = ~.*1/1000, name="Annual Mean Rabies Cases"))+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+
  scale_x_discrete(limits = c("1", "2"),
                   labels = c("Before", "After"))


barplotInter




#Seasonal Data
RabiesPremonsson <- Rabies[Rabies$Season == "Pre-monsoon",]


SeasonalRabies <- aggregate(RabiesPremonsson$RabiesCase, by=list(Category=RabiesPremonsson$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "RabiesCase")
describe(SeasonalRabies$RabiesCase)

SeasonalRabies <- aggregate(RabiesPremonsson$ARV, by=list(Category=RabiesPremonsson$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "ARV")
describe(SeasonalRabies$ARV)

SeasonalRabies <- aggregate(RabiesPremonsson$MDV, by=list(Category=RabiesPremonsson$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "MDV")
describe(SeasonalRabies$MDV)


#Seasonal Data
RabiesMonsson <- Rabies[Rabies$Season == "Monsoon",]


SeasonalRabies <- aggregate(RabiesMonsson$RabiesCase, by=list(Category=RabiesMonsson$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "RabiesCase")
describe(SeasonalRabies$RabiesCase)

SeasonalRabies <- aggregate(RabiesMonsson$ARV, by=list(Category=RabiesMonsson$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "ARV")
describe(SeasonalRabies$ARV)

SeasonalRabies <- aggregate(RabiesMonsson$MDV, by=list(Category=RabiesMonsson$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "MDV")
describe(SeasonalRabies$MDV)


#Seasonal Data
RabiesWinter <- Rabies[Rabies$Season == "Winter",]


SeasonalRabies <- aggregate(RabiesWinter$RabiesCase, by=list(Category=RabiesWinter$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "RabiesCase")
describe(SeasonalRabies$RabiesCase)

SeasonalRabies <- aggregate(RabiesWinter$ARV, by=list(Category=RabiesWinter$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "ARV")
describe(SeasonalRabies$ARV)

SeasonalRabies <- aggregate(RabiesWinter$MDV, by=list(Category=RabiesWinter$Year), FUN=sum)

colnames(SeasonalRabies) <- c("Year", "MDV")
describe(SeasonalRabies$MDV)



library(tidyverse)

df1<-data.frame(TYPE = c(rep("ARV", 3), rep("MDV", 3)),
                Seasons = rep(c(seq(1,3)),2),
                value = c(50185.62, 81668.69,  75592.77,
                          75646.67, 58858.50, 68284.25))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 3)),
                Seasons = c(seq(1,3)),
                value = c(18.74, 33.16, 36.37))


options(scipen = 999) ## To disable scientific notation
barplotSeasons <- ggplot() + 
  geom_col(data = df1, aes(x = Seasons, y = value, fill = TYPE), position = position_dodge()) +
  scale_fill_manual("", values = c("ARV" = "#FFD580", "MDV" = "skyblue"))+
  geom_point(data = df2, aes(x = Seasons, y = value*1000,  group = TYPE, col = TYPE)) + 
  geom_line(data = df2, aes(x = Seasons, y = value*1000, group = TYPE, col = TYPE)) +
  scale_color_manual("", values = c("Annual Mean Rabies Cases" = "#FF7F7F"))+
  scale_y_continuous(name = "Annual Mean Vaccines",
                     sec.axis = sec_axis(trans = ~.*1/1000, name="Annual Mean Rabies Cases"))+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+
  scale_x_discrete(limits = c("1", "2", "3"),
                   labels = c("Pre-monsoon", "Rainy", "Winter"))
barplotSeasons

tiff("barplotSeasons.tiff", units="in", width=8, height=12, res=300)
gridExtra::grid.arrange(barplotSeasons, barplotInter)
dev.off()








#Menn kendal
library(Kendall)
library(trend)

#Rabies Cases
myts <- ts(YearwiseRabies$x)

library(trend)
MannKendall(myts)
sens.slope(myts, conf.level = 0.95)


#ARV
myts <- ts(YearwiseARV$x)

library(trend)
MannKendall(myts)
mytsnomiss <- replace(myts, is.na(myts),0)
sens.slope(mytsnomiss, conf.level = 0.95)


#MDV
myts <- ts(YearwiseMDV$x)

library(trend)
MannKendall(myts)
mytsnomiss <- replace(myts, is.na(myts),0)
sens.slope(mytsnomiss, conf.level = 0.95)






#######Count GLM

## Dengue - BD data ##
rm(list=ls())
library(MASS)
library(tscount)
library(glmmTMB)
library(DHARMa)
library(performance)
library(scales)
Rabies$Season
options(scipen = 999)

setwd('E:\\ResearchProject\\Sumon Bhai\\Rabies Weather')
Rabies <- read.csv("Rabies_Weather_Data.csv")

fitglm <- glm(RabiesCase ~ MDV + Rainfall + AvgT + relevel(factor(Rabies$Season), ref = "Pre-monsoon"), data=Rabies, 
              family=poisson(link = "log"))


library(car)
summary(fitglm)
round(exp(fitglm$coefficients),6)
round(exp(confint(fitglm)),6)

options(scipen = 999)
performance::performance(fit)

