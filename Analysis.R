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

#Yearwise Rabies Case
YearwiseRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$Year), FUN=sum)
YearwiseRabies$Category
YearwiseRabies$x

colnames(YearwiseRabies) <- c("Year", "RabiesCase")
describe(YearwiseRabies$RabiesCase)

BeforeInitiativeRabies <- YearwiseRabies[which(YearwiseRabies$Year<='2013'),]
describe(BeforeInitiativeRabies$RabiesCase)

AfterInitiativeRabies <- YearwiseRabies[which(YearwiseRabies$Year>'2013'),]
describe(AfterInitiativeRabies$RabiesCase)

t.test(BeforeInitiativeRabies$RabiesCase, AfterInitiativeRabies$RabiesCase,  
       alternative = "two.sided", 
       var.equal = FALSE)


#ARV
YearwiseARV <- aggregate(Rabies$ARV[61:216], by=list(Category=Rabies$Year[61:216]), FUN=sum)
YearwiseARV$Category
YearwiseARV$x

colnames(YearwiseARV) <- c("Year", "ARV")
describe(YearwiseARV$ARV)

BeforeInitiativeARV <- YearwiseARV[which(YearwiseARV$Year<='2013'),]
describe(BeforeInitiativeARV$ARV)

AfterInitiativeARV <- YearwiseARV[which(YearwiseARV$Year>'2013'),]
describe(AfterInitiativeARV$ARV)

t.test(BeforeInitiativeARV$ARV, AfterInitiativeARV$ARV,  
       alternative = "two.sided", 
       var.equal = FALSE)



#MDV
YearwiseMDV <- aggregate(Rabies$MDV[61:216], by=list(Category=Rabies$Year[61:216]), FUN=sum)
YearwiseMDV$Category
YearwiseMDV$x

colnames(YearwiseMDV) <- c("Year", "MDV")
describe(YearwiseMDV$MDV)

BeforeInitiativeMDV <- YearwiseMDV[which(YearwiseMDV$Year<='2013'),]
describe(BeforeInitiativeMDV$MDV)

AfterInitiativeMDV <- YearwiseMDV[which(YearwiseMDV$Year>'2013'),]
describe(AfterInitiativeMDV$MDV)

t.test(BeforeInitiativeMDV$MDV, AfterInitiativeMDV$MDV,  
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




#Monthly Data Rabies
MonthwiseRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$Month), FUN=mean)
MonthwiseRabies$Category
MonthwiseRabies$x
colnames(MonthwiseRabies) <- c("Months", "RabiesCase")
describe(MonthwiseRabies$RabiesCase)

#Subset Pre-monsoon
PreMonsoonRabies <- Rabies[which(Rabies$Season =='Pre-monsoon'),]
PreMonsoonRabies$Season
#Descriptive PreMonsoon
describe(PreMonsoonRabies$RabiesCase)

#Subset Monsoon
MonsoonRabies <- Rabies[which(Rabies$Season=='Monsoon'),]
MonsoonRabies$Season
#Descriptive (2006-2013)
describe(MonsoonRabies$RabiesCase)

#Subset Winter
WinterRabies <- Rabies[which(Rabies$Season=='Winter'),]
WinterRabies$Season
#Descriptive Winter
describe(WinterRabies$RabiesCase)



#Monthly Data ARV
MonthwiseARV <- aggregate(Rabies$ARV[61:216], by=list(Category=Rabies$Month[61:216]), FUN=mean)
MonthwiseARV$Category
MonthwiseARV$x
colnames(MonthwiseARV) <- c("Months", "ARV")
describe(MonthwiseARV$ARV)

#Subset Pre-monsoon
PreMonsoonARV <- Rabies[which(Rabies$Season[61:216]=='Pre-monsoon'),]
PreMonsoonARV$Season
#Descriptive PreMonsoon
describe(PreMonsoonARV$ARV)

#Subset Monsoon
MonsoonARV <- Rabies[which(Rabies$Season[61:216]=='Monsoon'),]
MonsoonARV$Season
#Descriptive (2006-2013)
describe(MonsoonARV$ARV)

#Subset Winter
WinterARV <- Rabies[which(Rabies$Season=='Winter'),]
WinterARV$Season
#Descriptive Winter
describe(WinterARV$ARV)

#Monthly Data MDV
MonthwiseMDV <- aggregate(Rabies$MDV[61:216], by=list(Category=Rabies$Month[61:216]), FUN=mean)
MonthwiseMDV$Category
MonthwiseMDV$x
colnames(MonthwiseMDV) <- c("Months", "MDV")
describe(MonthwiseMDV$MDV)

#Subset Pre-monsoon
PreMonsoonMDV <- Rabies[which(Rabies$Season[61:216]=='Pre-monsoon'),]
PreMonsoonMDV$Season
#Descriptive PreMonsoon
describe(PreMonsoonMDV$MDV)

#Subset Monsoon
MonsoonMDV <- Rabies[which(Rabies$Season[61:216]=='Monsoon'),]
MonsoonMDV$Season
#Descriptive (2006-2013)
describe(MonsoonMDV$MDV)

#Subset Winter
WinterMDV <- Rabies[which(Rabies$Season[61:216]=='Winter'),]
WinterMDV$Season
#Descriptive Winter
describe(WinterMDV$MDV)


#Monthly Data Rainfall
MonthwiseRainfall <- aggregate(Rabies$Rainfall, by=list(Category=Rabies$Month), FUN=mean)
MonthwiseRainfall$Category
MonthwiseRainfall$x
colnames(MonthwiseRainfall) <- c("Months", "Rainfall")
describe(MonthwiseRainfall$Rainfall)

#Subset Pre-monsoon
PreMonsoon <- Rabies[which(Rabies$Season=='Pre-monsoon'),]
PreMonsoon$Season
#Descriptive PreMonsoon
describe(PreMonsoon$Rainfall)

#Subset Monsoon
MonsoonRainfall <- Rabies[which(Rabies$Season=='Monsoon'),]
MonsoonRainfall$Season
#Descriptive (2006-2013)
describe(MonsoonRainfall$Rainfall)

#Subset Winter
WinterRainfall <- Rabies[which(Rabies$Season=='Winter'),]
WinterRainfall$Season
#Descriptive Winter
describe(WinterRainfall$Rainfall)

#Monthly Data Temperature
MonthwiseAvgT <- aggregate(Rabies$AvgT, by=list(Category=Rabies$Month), FUN=mean)
MonthwiseAvgT$Category
MonthwiseAvgT$x
colnames(MonthwiseAvgT) <- c("Months", "AvgT")
describe(MonthwiseAvgT$AvgT)

#Subset Pre-monsoon
PreMonsoonAvgT <- Rabies[which(Rabies$Season=='Pre-monsoon'),]
PreMonsoonAvgT$Season
#Descriptive PreMonsoon
describe(PreMonsoonAvgT$AvgT)

#Subset Monsoon
MonsoonAvgT <- Rabies[which(Rabies$Season=='Monsoon'),]
MonsoonAvgT$Season
#Descriptive (2006-2013)
describe(MonsoonAvgT$AvgT)

#Subset Winter
WinterAvgT <- Rabies[which(Rabies$Season=='Winter'),]
WinterAvgT$Season
#Descriptive Winter
describe(WinterAvgT$AvgT)

summary(aov(Rabies$RabiesCase ~ Rabies$Season))
summary(aov(Rabies$ARV[61:216] ~ Rabies$Season[61:216]))
summary(aov(Rabies$MDV[61:216] ~ Rabies$Season[61:216]))
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

RabiesCorr <- cbind(Rabies$Rainfall[61:216], Rabies$AvgT[61:216], Rabies$ARV[61:216], Rabies$MDV[61:216], Rabies$RabiesCase[61:216])

colnames(RabiesCorr) <- c("Rainfall", "Tempurature", "ARV", "MDV", "Rabies Cases")

ggpairs(data.frame(RabiesCorr))

library(GGally)
CorrPlot <- ggpairs(data.frame(RabiesCorr),  
                    lower = list(continuous = "smooth"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
CorrPlot

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





#Yearly Rabies Barplot

library(tidyverse)
df2<-data.frame(Years = YearwiseRabies$Year,
                value = c(YearwiseRabies$RabiesCase))


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

#Monthly Rabies Barplot

library(tidyverse)

df2<-data.frame(Months = MonthwiseRabies$Months,
                value = MonthwiseRabies$RabiesCase)


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





#Yearly MDV ARV Barplot

df2 <- data.frame(Vaccines=rep(c("ARV", "MDV"), each=13),
                  Years=rep(c(YearwiseARV$Year),2),
                  Numbers=c(YearwiseARV$ARV,YearwiseMDV$MDV))

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







#Barplot by initiative and season

library(tidyverse)
mean(BeforeInitiativeARV$ARV)
df1<-data.frame(TYPE = c(rep("ARV", 2), rep("MDV", 2)),
                Initiative = rep(c(seq(1,2)),2),
                value = c(3402.0625, 19751.78333,
                          677.3854167, 19764.40833))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 2)),
                Initiative = c(seq(1,2)),
                value = c(10.82291667, 4.945736434))


options(scipen = 999) ## To disable scientific notation
barplotInter <- ggplot() + 
  geom_col(data = df1, aes(x = Initiative , y = value, fill = TYPE), position = position_dodge()) +
  scale_fill_manual("", values = c("ARV" = "#FFD580", "MDV" = "skyblue"))+
  geom_point(data = df2, aes(x = Initiative, y = value*1000,  group = TYPE, col = TYPE)) + 
  geom_line(data = df2, aes(x = Initiative, y = value*1000, group = TYPE, col = TYPE)) +
  scale_color_manual("", values = c("Annual Mean Rabies Cases" = "#FF7F7F"))+
  scale_y_continuous(name = "Annual Mean Vaccines",
                     sec.axis = sec_axis(trans = ~.*1/1000, name="Annual Mean Rabies Cases"))+
  xlab("Intervention Phase")+
  theme_bw()+
  theme(
    plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, face = "bold"),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15),
    legend.position = "none")+
  scale_x_discrete(limits = c("1", "2"),
                   labels = c("Before", "After"))


barplotInter



mean(PreMonsoonARV$ARV)
library(tidyverse)

df1<-data.frame(TYPE = c(rep("ARV", 3), rep("MDV", 3)),
                Seasons = rep(c(seq(1,3)),2),
                value = c(12081.72222, 11796.58889,  13648.69444,
                          16810.37037, 7847.8,  11426.33333))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 3)),
                Seasons = c(seq(1,3)),
                value = c(6.245614035, 6.70212766,  9.337837838))


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
  theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=0.5, face = "bold"),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15),
        legend.position = c(0.9, 0.87))+
  scale_x_discrete(limits = c("1", "2", "3"),
                   labels = c("Pre-monsoon", "Rainy", "Winter"))
barplotSeasons

tiff("barplotSeasons.tiff", units="in", width=8, height=10, res=300)
gridExtra::grid.arrange(barplotSeasons, barplotInter)
dev.off()

#Menn kendal
library(Kendall)
library(trend)

#Rabies Cases
myts <- ts(YearwiseRabies$RabiesCase)

library(trend)
MannKendall(myts)
sens.slope(myts, conf.level = 0.95)


#ARV
myts <- ts(YearwiseARV$ARV)

library(trend)
MannKendall(myts)
mytsnomiss <- replace(myts, is.na(myts),0)
sens.slope(mytsnomiss, conf.level = 0.95)


#MDV
myts <- ts(YearwiseMDV$MDV)

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
options(scipen = 999)

setwd('E:\\ResearchProject\\Sumon Bhai\\Rabies Weather')
Rabies <- read.csv("Rabies_Weather_Data.csv")


fitglm <- glm(RabiesCase ~ MDV + Rainfall + AvgT + Season, data=BeforeInitiative, 
              family=poisson(link = "log"))


library(car)
summary(fitglm)
round(exp(fitglm$coefficients),6)
round(exp(confint(fitglm)),6)

options(scipen = 999)
performance::performance(fitglm)




fitglm <- glm(RabiesCase ~ MDV + Rainfall + AvgT + Season, data=AfterInitiative, 
              family=poisson(link = "log"))


library(car)
summary(fitglm)
round(exp(fitglm$coefficients),6)
round(exp(confint(fitglm)),6)

options(scipen = 999)
performance::performance(fitglm)





fitglm <- glm(RabiesCase ~ MDV + Rainfall + AvgT + Season, data=Rabies, 
              family=poisson(link = "log"))


library(car)
summary(fitglm)
round(exp(fitglm$coefficients),6)
round(exp(confint(fitglm)),6)

options(scipen = 999)
performance::performance(fitglm)

