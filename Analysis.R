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

#Descriptive Total
describe(Rabies$RabiesCase)
describe(Rabies$ARV)
describe(Rabies$MDV)
describe(Rabies$Rainfall)
describe(Rabies$AvgT)

#Subset (2006-2013)
BeforeInitiative <- Rabies[which(Rabies$Year<='2013'),]
BeforeInitiative$Year

#Descriptive (2006-2013)
describe(BeforeInitiative$RabiesCase)
describe(BeforeInitiative$ARV)
describe(BeforeInitiative$MDV)
describe(BeforeInitiative$Rainfall)
describe(BeforeInitiative$AvgT)


#Subset (2014-2024)
AfterInitiative <- Rabies[which(Rabies$Year>'2013'),]
AfterInitiative$Year

#Descriptive (2006-2013)
describe(AfterInitiative$RabiesCase)
describe(AfterInitiative$ARV)
describe(AfterInitiative$MDV)
describe(AfterInitiative$Rainfall)
describe(AfterInitiative$AvgT)

NROW(BeforeInitiative$RabiesCase)
NROW(AfterInitiative$RabiesCase)

t.test(BeforeInitiative$RabiesCase, AfterInitiative$RabiesCase,  
       alternative = "two.sided", 
       var.equal = FALSE)

t.test(BeforeInitiative$ARV, AfterInitiative$ARV,  
       alternative = "two.sided", 
       var.equal = FALSE)

t.test(BeforeInitiative$MDV, AfterInitiative$MDV,  
       alternative = "two.sided", 
       var.equal = FALSE)

t.test(BeforeInitiative$Rainfall, AfterInitiative$Rainfall,  
       alternative = "two.sided", 
       var.equal = FALSE)

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
library(ggplot2)
library(extrafont)
library(GGally)
library(MASS)

RabiesCorr <- cbind(Rabies$Rainfall, Rabies$AvgT, Rabies$ARV, Rabies$MDV, Rabies$RabiesCase)

colnames(RabiesCorr) <- c("Rainfall", "Tempurature", "ARV", "MDV", "Rabies Cases")

ggpairs(data.frame(RabiesCorr))

#Yearly Data
YearwiseRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$Year), FUN=sum)
YearwiseRabies$Category
YearwiseRabies$x
YearwiseARV <- aggregate(Rabies$ARV, by=list(Category=Rabies$Year), FUN=sum)
YearwiseARV$x
YearwiseMDV <- aggregate(Rabies$MDV, by=list(Category=Rabies$Year), FUN=sum)
YearwiseMDV$x

library(tidyverse)

df1<-data.frame(TYPE = c(rep("ARV", 19), rep("MDV", 19)),
                      Years = rep(c("2006", "2007", "2008", "2009", "2010", "2011", "2012", 
                                   "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                                   "2020", "2021", "2022", "2023", "2024"),2),
                      value = c(NA, NA, NA, NA, NA, 29994, 129444, 167160, 197551, 226459, 251617,
                                240414, 240376, 246710, 194950, 238369, 275412, 258356, NA,
                                NA,     NA,     NA,     NA,     NA,     NA,  45655,  16089,  26318, 135269,  
                                70303,  40420, 365316, 625208, 212562, 288837, 341486, 266010,     NA))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 19)),
                Years = c("2006", "2007", "2008", "2009", "2010", "2011", "2012", 
                             "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                             "2020", "2021", "2022", "2023", "2024"),
                value = c(167, 166, 165, 164, 104, 109,  82,  82, 106,  83,  66,  80,  60,  57,  26,  
                          38,  47,  47,  28))


options(scipen = 999) ## To disable scientific notation
barplotYear <- ggplot() + 
  geom_col(data = df1, aes(x = Years, y = value, fill = TYPE), position = position_dodge()) +
  scale_fill_manual("", values = c("ARV" = "#FFD580", "MDV" = "skyblue"))+
  geom_point(data = df2, aes(x = Years, y = value*1000,  group = TYPE, col = TYPE)) + 
  geom_line(data = df2, aes(x = Years, y = value*1000, group = TYPE, col = TYPE)) +
  scale_color_manual("", values = c("Rabies Cases" = "#FF7F7F"))+
  scale_y_continuous(name = "Number of Vaccines",
                     sec.axis = sec_axis(trans = ~.*1/1000, name="Rabies Cases"))+
  theme_bw()+
  theme(legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        legend.position = c(0.2, 0.8),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+
  labs(caption = "★ = up to September' 2024")


#Monthly Data
MonthwiseRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$Month), FUN=sum)
MonthwiseRabies$Category
MonthwiseRabies$x
Rabies$ARVNoMiss <- replace(Rabies$ARV, is.na(Rabies$ARV),0)
MonthwiseARV <- aggregate(Rabies$ARVNoMiss, by=list(Category=Rabies$Month), FUN=sum)
MonthwiseARV$x
Rabies$MDVNoMiss <- replace(Rabies$MDV, is.na(Rabies$MDV),0)
MonthwiseMDV <- aggregate(Rabies$MDVNoMiss, by=list(Category=Rabies$Month), FUN=sum)
MonthwiseMDV$x

library(tidyverse)

df1<-data.frame(TYPE = c(rep("ARV", 12), rep("MDV", 12)),
                Months = rep(c(seq(1,12)),2),
                value = c(237055, 217692, 236523, 207038, 208852, 204955, 185731, 201060, 218737, 
                          251210, 268275, 259684,
                          178219, 363121, 302754, 295280, 309726, 299713,  96499,  
                          65006,  63543, 181541, 253079, 28277))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 12)),
                Months = c(seq(1,12)),
                value = c(182, 165, 113, 145,  98, 115,  98, 127, 137, 153, 155, 189))


options(scipen = 999) ## To disable scientific notation
barplotMonth <- ggplot() + 
  geom_col(data = df1, aes(x = Months, y = value, fill = TYPE), position = position_dodge()) +
  scale_fill_manual("", values = c("ARV" = "#FFD580", "MDV" = "skyblue"))+
  geom_point(data = df2, aes(x = Months, y = value*1000,  group = TYPE, col = TYPE)) + 
  geom_line(data = df2, aes(x = Months, y = value*1000, group = TYPE, col = TYPE)) +
  scale_color_manual("", values = c("Rabies Cases" = "#FF7F7F"))+
  scale_y_continuous(name = "Number of Vaccines",
                     sec.axis = sec_axis(trans = ~.*1/1000, name="Rabies Cases"))+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+
  labs(caption = "★ = up to September' 2024")+
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))


tiff("barplotYearMonth.tiff", units="in", width=10, height=12, res=300)
gridExtra::grid.arrange(barplotYear, barplotMonth, nrow=2, ncol=1)
dev.off()

mean(Rabies$GrowthFactor_CbP)
sd(Rabies$GrowthFactor_CbP)

describe.by(Rabies$GrowthFactor_CbP, Rabies$Season)

library(Rmisc)
CIs <- group.CI(Rabies$Gfexp ~ Rabies$Month, data=Rabies, ci = 0.95)
CIs
mean(CIs$`Rabies$Gfexp.mean`)
sd(CIs$`Rabies$Gfexp.mean`)
my.data <- data.frame(time     = c(1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12),
                      means    = c(CIs$`Rabies$Gfexp.mean`),
                      lowerCI  = c(CIs$`Rabies$Gfexp.lower`),
                      upperCI  = c(CIs$`Rabies$Gfexp.upper`),
                      scenario = rep(c("Mean monthly growth factor"), each=3))


c <- ggplot(my.data, aes(x = factor(time), y = means, group = scenario))+
  geom_line(aes(colour = scenario), size =1)+
  geom_line(aes(y = lowerCI, colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  geom_line(aes(y = upperCI, colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  scale_colour_manual(values = c('Mean monthly growth factor' = 'black',
                                 '95% Confidence interval' = 'black'),
                      breaks = c( 'Mean monthly growth factor', '95% Confidence interval'))+  ylab("Monthly growth factor") + 
  xlab("Months") + ggtitle("") +  theme_bw()+ geom_hline(yintercept=1, linetype="dashed", 
                                                         color = "black", size=1)+
  theme_bw()+
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18),
         legend.position = c(0.2, 0.9),
         text = element_text(size = 18)) +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))

c



colnames(YearwiseRabies) <- c("Year", "Rabies Cases")
YearwiseRabies$Gf <- YearwiseRabies$`Rabies Cases`/lag(YearwiseRabies$`Rabies Cases`)
YearwiseRabies$Gflog <- log(YearwiseRabies$Gf)
YearwiseRabies$Gfexp <- exp(YearwiseRabies$Gflog)

library(Rmisc)
CIs <- group.CI(Rabies$Gfexp ~ Rabies$Year, data=Rabies, ci = 0.95)
CIs
mean(CIs$`Rabies$Gfexp.mean`)
sd(CIs$`Rabies$Gfexp.mean`)
my.data <- data.frame(time     = rep(c("2006", "2007", "2008", "2009", "2010", "2011", "2012", 
                                   "2013", "2014", "2015", "2016", "2017", "2018", "2019", 
                                   "2020", "2021", "2022", "2023", "2024"),3),
                      means    = c(CIs$`Rabies$Gfexp.mean`),
                      lowerCI  = c(CIs$`Rabies$Gfexp.lower`),
                      upperCI  = c(CIs$`Rabies$Gfexp.upper`),
                      scenario = rep(c("Mean yearly growth factor"), each=3))


d <- ggplot(my.data, aes(x = factor(time), y = means, group = scenario))+
  geom_line(aes(colour = scenario), size =1)+
  geom_line(aes(y = lowerCI, colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  geom_line(aes(y = upperCI, colour = paste(scenario, '95% Confidence interval')),
            linetype = 'dashed')+
  scale_colour_manual(values = c('Mean yearly growth factor' = 'black',
                                 '95% Confidence interval' = 'black'),
                      breaks = c( 'Mean yearly growth factor', '95% Confidence interval'))+  ylab("Yearly growth factor") + 
  xlab("Years") + ggtitle("") +  theme_bw()+ geom_hline(yintercept=1, linetype="dashed", 
                                                         color = "black", size=1)+
  theme_bw()+
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18),
         legend.position = c(0.2, 0.9),
         text = element_text(size = 18),
         axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))

d


tiff("GF.tiff", units="in", width=12, height=16, res=300)
gridExtra::grid.arrange(c,d)
dev.off()




# Data
InterRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$InterventionNum), FUN=sum)
InterRabies$Category
InterRabies$x
Rabies$ARVNoMiss <- replace(Rabies$ARV, is.na(Rabies$ARV),0)
InterARV <- aggregate(Rabies$ARVNoMiss, by=list(Category=Rabies$InterventionNum), FUN=sum)
InterARV$x
Rabies$MDVNoMiss <- replace(Rabies$MDV, is.na(Rabies$MDV),0)
InterMDV <- aggregate(Rabies$MDVNoMiss, by=list(Category=Rabies$InterventionNum), FUN=sum)
InterMDV$x

library(tidyverse)

df1<-data.frame(TYPE = c(rep("ARV", 2), rep("MDV", 2)),
                Initiative = rep(c(seq(1,2)),2),
                value = c(326598, 2370214,
                          65029, 2371729))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 2)),
                Initiative = c(seq(1,2)),
                value = c(1039,  638))


options(scipen = 999) ## To disable scientific notation
barplotInter <- ggplot() + 
  geom_col(data = df1, aes(x = Initiative , y = value, fill = TYPE), position = position_dodge()) +
  scale_fill_manual("", values = c("ARV" = "#FFD580", "MDV" = "skyblue"))+
  geom_point(data = df2, aes(x = Initiative, y = value*1000,  group = TYPE, col = TYPE)) + 
  geom_line(data = df2, aes(x = Initiative, y = value*1000, group = TYPE, col = TYPE)) +
  scale_color_manual("", values = c("Rabies Cases" = "#FF7F7F"))+
  scale_y_continuous(name = "Number of Vaccines",
                     sec.axis = sec_axis(trans = ~.*1/1000, name="Rabies Cases"))+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+
  labs(caption = "★ = up to September' 2024")+
  scale_x_discrete(limits = c("1", "2"),
                   labels = c("Before", "After"))







#Seasonal Data
SeasonalRabies <- aggregate(Rabies$RabiesCase, by=list(Category=Rabies$SeasonNum), FUN=sum)
SeasonalRabies$Category
SeasonalRabies$x
Rabies$ARVNoMiss <- replace(Rabies$ARV, is.na(Rabies$ARV),0)
SeasonalARV <- aggregate(Rabies$ARVNoMiss, by=list(Category=Rabies$SeasonNum), FUN=sum)
SeasonalARV$x
Rabies$MDVNoMiss <- replace(Rabies$MDV, is.na(Rabies$MDV),0)
SeasonalMDV <- aggregate(Rabies$MDVNoMiss, by=list(Category=Rabies$SeasonNum), FUN=sum)
SeasonalMDV$x

library(tidyverse)

df1<-data.frame(TYPE = c(rep("ARV", 3), rep("MDV", 3)),
                Seasons = rep(c(seq(1,3)),2),
                value = c(652413, 1061693,  982706,
                          907760, 706302, 822696))

df2<-data.frame(TYPE = c(rep("Rabies Cases", 3)),
                Seasons = c(seq(1,3)),
                value = c(356, 630, 691))


options(scipen = 999) ## To disable scientific notation
barplotSeasons <- ggplot() + 
  geom_col(data = df1, aes(x = Seasons, y = value, fill = TYPE), position = position_dodge()) +
  scale_fill_manual("", values = c("ARV" = "#FFD580", "MDV" = "skyblue"))+
  geom_point(data = df2, aes(x = Seasons, y = value*1000,  group = TYPE, col = TYPE)) + 
  geom_line(data = df2, aes(x = Seasons, y = value*1000, group = TYPE, col = TYPE)) +
  scale_color_manual("", values = c("Rabies Cases" = "#FF7F7F"))+
  scale_y_continuous(name = "Number of Vaccines",
                     sec.axis = sec_axis(trans = ~.*1/1000, name="Rabies Cases"))+
  theme_bw()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5),
        text=element_text(size=15),
        axis.text.y = element_text(hjust = 0.5),
        axis.title.x = element_text(hjust = 0.5,size=15))+
  labs(caption = "★ = up to September' 2024")+
  scale_x_discrete(limits = c("1", "2", "3"),
                   labels = c("Pre-monsoon", "Rainy", "Winter"))


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
BeforeInitiative$Season
options(scipen = 999)

setwd('E:\\ResearchProject\\Sumon Bhai\\Rabies Weather')
Rabies <- read.csv("Rabies_Weather_Data.csv")

fitglm <- glm(RabiesCase ~ ARV + MDV + Rainfall + AvgT + Season, data=Rabies, 
              family=poisson(link = "log"))


library(car)
summary(fitglm)
round(exp(fitglm$coefficients),6)
round(exp(confint(fitglm)),6)

options(scipen = 999)
performance::performance(fit)






fitglm <- glm(RabiesCase ~ ARV + MDV + Rainfall + AvgT + Season, data=BeforeInitiative, 
              family=poisson(link = "log"))


library(car)
summary(fitglm)
round(exp(fitglm$coefficients),6)
round(exp(confint(fitglm)),6)

options(scipen = 999)
performance::performance(fit)




fitglm <- glm(RabiesCase ~ ARV + MDV + Rainfall + AvgT + Season, data=AfterInitiative, 
              family=poisson(link = "log"))


library(car)
summary(fitglm)
round(exp(fitglm$coefficients),6)
round(exp(confint(fitglm)),6)

options(scipen = 999)
performance::performance(fit)



