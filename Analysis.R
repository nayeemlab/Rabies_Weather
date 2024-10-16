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
YearwiseARV <- aggregate(Rabies$ARV, by=list(Category=Rabies$Year), FUN=sum)
YearwiseMDV <- aggregate(Rabies$MDV, by=list(Category=Rabies$Year), FUN=sum)



data <- data.frame('ARV_MDV' = c(rep(c("ARV", "MDV"),19)),
                   'Year' = c("2006", "2007","2008", "2009","2010", "2011",
                              "2012", "2013","2014", "2015","2016", "2017",
                              "2018", "2019","2020", "2021",,"2022", "2023","2024"),
                   '' = c(34.1, 65.9,
                              32.0, 68.0,
                              0.0, 0.0,
                              36.6, 63.4,
                              39.2, 60.8),
                   'n' = c(723, 1395,
                           587, 1247,
                           0, 0,
                           545, 943,
                           439, 681)) %>%
  
  mutate(Ano_do_Nascimento = as.numeric(as.character(Ano_do_Nascimento)),
         perc = ifelse(Tipo_de_Parto == 'Vaginal', perc, NA)) # drop unwanted percentages here


# this is the factor we will use to scale the secondary y-axis
your_factor <- max(data$n) / 100


# plot bars
p <- ggplot(data, aes(x = Ano_do_Nascimento, y = n, fill = Tipo_de_Parto)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = c('Cesário' = 'blue4', 'Vaginal' = 'blue')) +
  
  # add percentages
  geom_point(aes(x = Ano_do_Nascimento, y = perc*your_factor),
             color = 'orange') +
  geom_line(aes(x = Ano_do_Nascimento, y = perc*your_factor),
            color = 'orange') +
  
  # add secondary y-axis
  scale_y_continuous("n", 
                     sec.axis = sec_axis(~./your_factor, name = "perc")) 

# calculate the range of the secondary y-axis
y2_range <- ggplot_build(p)$layout$panel_params[[1]]$y.range / your_factor

# add the points to the plotly object
ggplotly(p) %>%
  
  # add the secondary axis back in
  add_trace(x=~Ano_do_Nascimento, y=~perc, yaxis="y2",
            data=data, showlegend=FALSE, inherit=FALSE, mode = "markers") %>%
  layout(yaxis2 = list(overlaying = "y", side = "right", title = "perc", range = y2_range))



describe.by(Rabies$RC, Rabies$Year)


YearwiseRC <- aggregate(Rabies$RC, by=list(Category=Dengue$Year), FUN=sum)
YearwiseRC
describe(YearwiseRC)
summary(YearwiseRC$x)

YearwiseAvgT <- aggregate(Dengue$AvgT, by=list(Category=Dengue$Year), FUN=mean)
YearwiseAvgT
YearwiseRainfall <- aggregate(Dengue$Rainfall, by=list(Category=Dengue$Year), FUN=sum)
YearwiseRainfall

colnames(YearwiseRC) <- c("Year","RC")
YearwiseRC

df2 <- data.frame(Dengue=c("Cases"),
                  Years=c(YearwiseRC$Year),
                  Numbers=c(YearwiseRC$RC))

# Change the colors manually
p <- ggplot(data=df2, aes(x=Years, y=Numbers, fill=Dengue)) + 
  geom_bar(position="dodge", stat="identity")+
  theme_minimal()+  theme_bw() +
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.8, 0.9),
         text = element_text(size = 25))

# Use custom colors
p + scale_fill_manual(values=c('#999999','#E69F00'))
# Use brewer color palettes
p<- p + scale_fill_brewer(palette="Dark2") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p


monthwiseRC <- aggregate(Dengue$RC, by=list(Category=Dengue$Month), FUN=mean)
monthwiseRC

df2 <- data.frame(Dengue=c("Cases"),
                  Months=c(monthwiseRC$Category),
                  Numbers=c(monthwiseRC$x))


# Change the colors manually
q <- ggplot(data=df2, aes(x=Months, y=Numbers, fill=Dengue)) +
  geom_bar(position="dodge", stat="identity")+
  
  theme_minimal() + theme_bw() +
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.15, 0.94),
         text = element_text(size = 25)) +

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

tiff("DCDDM.tiff", units="in", width=12, height=13, res=300)

library(cowplot)

gridExtra::grid.arrange(plot_grid(p, q, labels = "AUTO", ncol = 1, nrow = 2))
dev.off()


fYearwiseRC <- YearwiseRC[which(YearwiseRC$Year<='2013'), ]

mean(fYearwiseRC$RC, na.rm = T)
sd(fYearwiseRC$RC, na.rm = T)
summary(fYearwiseRC$RC, na.rm = T)

sYearwiseRC <- YearwiseRC[which(YearwiseRC$Year>'2013'), ]

mean(sYearwiseRC$RC, na.rm = T)
sd(sYearwiseRC$RC, na.rm = T)
summary(sYearwiseRC$RC, na.rm = T)

fYearwiseAvgT <- YearwiseAvgT[which(YearwiseAvgT$Category<='2013'), ]

mean(fYearwiseAvgT$x, na.rm = T)
sd(fYearwiseAvgT$x, na.rm = T)

sYearwiseAvgT <- YearwiseAvgT[which(YearwiseAvgT$Category>'2013'), ]

mean(sYearwiseAvgT$x, na.rm = T)
sd(sYearwiseAvgT$x, na.rm = T)

fYearwiseRainfall <- YearwiseRainfall[which(YearwiseRainfall$Category<='2013'), ]

mean(fYearwiseRainfall$x, na.rm = T)
sd(fYearwiseRainfall$x, na.rm = T)

sYearwiseRainfall <- YearwiseRainfall[which(YearwiseRainfall$Category>'2013'), ]

mean(sYearwiseRainfall$x, na.rm = T)
sd(sYearwiseRainfall$x, na.rm = T)


fmonthwise <- Dengue[which(Dengue$Year<='2013'),]
NROW(fmonthwise)

x <- ggplot(fmonthwise, aes(x=as.factor(Month), y=Rainfall)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) + 
  ylab("Monthly rainfall (mm)") + xlab("") + ggtitle("Monthly rainfall Dhaka, Bangladesh (2000-2010)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                         axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5)
  )
x

fmonthwise <- Dengue[which(Dengue$Year>'2013'), ]

y <- ggplot(fmonthwise, aes(x=as.factor(Month), y=Rainfall)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) + 
  ylab("Monthly rainfall (mm)") + xlab("") + ggtitle("Monthly rainfall Dhaka, Bangladesh (2011-2022)") +
  scale_x_discrete(limits = c("1", "2", "3", 
                              "4", "5", "6", 
                              "7", "8", "9", 
                              "10", "11", "12"),
                   labels = c("Jan", "Feb", 
                              "Mar", "Apr", "May", 
                              "Jun", "Jul",
                              "Aug", "Sep", 
                              "Oct", "Nov", "Dec"))+theme(axis.text=element_text(size=12,face="bold"),
                                                          axis.title=element_text(size=14,face="bold"))+
  theme(plot.title = element_text(size = 16, face = "bold",hjust = 0.5)
  )
y

tiff("box.tiff", units="in", width=12, height=12, res=300)
gridExtra::grid.arrange(x,y)
dev.off()


mean(fmonthwise$RC, na.rm = T)
sd(fmonthwise$RC, na.rm = T)

mean(fmonthwise$AvgT, na.rm = T)
sd(fmonthwise$AvgT, na.rm = T)

mean(fmonthwise$Rainfall, na.rm = T)
sd(fmonthwise$Rainfall, na.rm = T)

smonthwise <- Dengue[which(Dengue$Year>'2013'), ]
NROW(smonthwise$RC)

mean(smonthwise$RC, na.rm = T)
sd(smonthwise$RC, na.rm = T)

mean(smonthwise$AvgT, na.rm = T)
sd(smonthwise$AvgT, na.rm = T)

mean(smonthwise$Rainfall, na.rm = T)
sd(smonthwise$Rainfall, na.rm = T)

t.test(fYearwiseDC$DC[1:132], sYearwiseDC$DC[1:132], paired = TRUE, alternative = "two.sided")
t.test(fYearwiseDD$DD[1:132], sYearwiseDD$DD[1:132], paired = TRUE, alternative = "two.sided")
t.test(fYearwiseAvgT$x[1:132], sYearwiseAvgT$x[1:132], paired = TRUE, alternative = "two.sided")
t.test(fYearwiseRainfall$x[1:132], sYearwiseRainfall$x[1:132], paired = TRUE, alternative = "two.sided")

# monthwiseRainfall <- Dengue[which(Dengue$Year<='2013'),]
# fmonthwiseRainfallsm <- monthwiseRainfall[monthwiseRainfall$Month<7 |monthwiseRainfall$Month>10,]
# smonthwiseRainfallsm <- monthwiseRainfall[monthwiseRainfall$Month>=7 |monthwiseRainfall$Month<=10,]
# 
# mean(fmonthwiseRainfallsm$Rainfall)
# sd(fmonthwiseRainfallsm$Rainfall)
# 
# mean(smonthwiseRainfallsm$Rainfall)
# sd(smonthwiseRainfallsm$Rainfall)
# 
# monthwiseRainfalll <- Dengue[which(Dengue$Year>'2010'), ]
# fmonthwiseRainfallsml <- monthwiseRainfalll[monthwiseRainfalll$Month<7 |monthwiseRainfalll$Month>10,]
# smonthwiseRainfallsml <- monthwiseRainfalll[monthwiseRainfalll$Month>=7 |monthwiseRainfalll$Month<=10,]
# 
# mean(fmonthwiseRainfallsml$Rainfall)
# sd(fmonthwiseRainfallsml$Rainfall)
# 
# mean(smonthwiseRainfallsml$Rainfall)
# sd(smonthwiseRainfallsml$Rainfall)
# 
# 
# t.test(fmonthwiseRainfallsm$Rainfall, fmonthwiseRainfallsml$Rainfall[1:88], paired = TRUE, alternative = "two.sided")
# t.test(smonthwiseRainfallsm$Rainfall, smonthwiseRainfallsml$Rainfall[1:132], paired = TRUE, alternative = "two.sided")
# 
# t.test(fmonthwiseRainfallsm$Rainfall, smonthwiseRainfallsm$Rainfall[1:88], paired = TRUE, alternative = "two.sided")
# t.test(fmonthwiseRainfallsml$Rainfall, smonthwiseRainfallsml$Rainfall[1:96], paired = TRUE, alternative = "two.sided")
# NROW(smonthwiseRainfallsml$Rainfall)

monthwise <- aggregate(Dengue$RC, by=list(Category=Dengue$Month), FUN=mean)
monthwise

monthwise <- aggregate(Dengue$RC, by=list(Category=Dengue$Month), FUN=min)
monthwise

monthwise <- aggregate(Dengue$RC, by=list(Category=Dengue$Month), FUN=sd)
monthwise

monthwise <- aggregate(Dengue$RC, by=list(Category=Dengue$Month), FUN=max)
monthwise

#Monthly
theme_set(theme_classic())

NROW(Dengue$RC)

DengueTS <- ts(Dengue$RC, frequency=12, start=c(2006,1), end=c(2024,9))

# Plot
a <- ggseasonplot(DengueTS)+ geom_line(size=1) + 
  theme_bw()  + xlab("Months") + ylab("Number of Rabies cases") + ggtitle("") +  
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 25), legend.position = c(0.2, 0.8),
         text = element_text(size = 25)) 

a

tiff("DC.tiff", units="in", width=18, height=12, res=300)
gridExtra::grid.arrange(a)
dev.off()


#t <- (Dengue$DC +1)/(lag(Dengue$DC)+1)
t <- Dengue$Gfexp
options(scipen=999)
DengueGF <- ts(t[2:225], frequency=12, start=c(2006,2), end=c(2024,9))
# Plot
b <- ggseasonplot(DengueGF) + geom_line(size=1) + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                                              labels = trans_format("log10", math_format(10^.x))) + 
  theme_bw()  + xlab("Months") + ylab("Monthly growth factor") + ggtitle("") +   geom_hline(yintercept=1, linetype="dashed", 
                                                                                           color = "black", size=1)+
  theme( legend.title=element_blank(),
         legend.text = element_text(color = "Black", size = 18), legend.position = c(0.1, 0.75),
         text = element_text(size = 18) ) 

b

#GF
#Dengue$Values_GF <- log((Dengue$DC +1)/(lag(Dengue$DC)+1))

#DengueTSlog <- ts(Dengue$Values_GF, frequency=12, start=c(2000,1), end=c(2022,12))

DengueTSlog <- ts(Dengue$Gfexp, frequency=12, start=c(2006,1), end=c(2024,9))

# Dengue_mean <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Month), FUN=mean, na.rm=T)
# Dengue_mean
# mean(Dengue_mean$x)
# sd(Dengue_mean$x)
# 
# Dengue_sd <- aggregate(Dengue$Values_GF, by=list(Category=Dengue$Month), FUN=SD, na.rm=T)
# Dengue_sd
# mean(Dengue_sd$x)
#  
# margin <- qt(0.975,df=11-1)*Dengue_sd$x / sqrt(11)
# 
# Dengue_sd$lower.ci <- Dengue_mean$x - margin
# Dengue_sd$lower.ci
# mean(Dengue_sd$lower.ci)
# Dengue_sd$upper.ci = Dengue_mean$x + margin
# Dengue_sd$upper.ci
# mean(Dengue_sd$upper.ci)
library(Rmisc)
CIs <- group.CI(Dengue$Gfexp ~ Dengue$Month, data=Dengue, ci = 0.95)
CIs
mean(CIs$`Dengue$Gfexp.mean`)
sd(CIs$`Dengue$Gfexp.mean`)
my.data <- data.frame(time     = c(1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12,
                                   1, 2,3, 4,5, 6,
                                   7, 8,9, 10,11, 12),
                      means    = c(CIs$`Dengue$Gfexp.mean`),
                      lowerCI  = c(CIs$`Dengue$Gfexp.lower`),
                      upperCI  = c(CIs$`Dengue$Gfexp.upper`),
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


tiff("GF.tiff", units="in", width=12, height=16, res=300)
gridExtra::grid.arrange(c,b)
dev.off()




#ARIMA

YearWiseCase <- aggregate(Dengue$RC, by=list(Category=Dengue$Year), FUN=sum)
YearWiseCase

DengueTS <- ts(YearWiseCase$x, start=c(2000))

auto.arima(DengueTS)

Fit<-Arima(DengueTS,order=c(0,1,0),lambda=0 )
summary(Fit)

fcast <- forecast(Fit, h=10)
library(ggfortify)
z <- autoplot(fcast, size = 2) +
  xlab("Years") + ylab("Number of dengue cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
  fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
  theme( legend.text = element_text(color = "Black", size = 40),
         text = element_text(size = 40))
z




#######Count GLM

## Dengue - BD data ##
rm(list=ls())
library(MASS)
library(tscount)
dendat <- read.csv("Rabies_Weather_Data.csv", header=T)  
dim(dendat)
head(dendat)
names(dendat)
#dendat <- dendat[c(37:276),] # discarding the set of missing values #

fitglm <- glm(RC ~ AvgT + Rainfall + ARV + dendat$MDV, data=dendat, family=poisson(link = "log"))
summary(fitglm)
#stepAIC(fitglm)
cat("IRR for AvgT. = ", exp(fitglm$coefficients[2]))
cat("IRR for Lag1Rainfall = ", exp(fitglm$coefficients[3]*100))
cat("IRR for Lag2Rainfall = ", exp(fitglm$coefficients[4]*100))

confint(fitglm)
exp(confint(fitglm)[2,1:2])
exp(confint(fitglm)[3,1:2]*100)
exp(confint(fitglm)[4,1:2]*100)


## Analysis using tscount package ##
attach(dendat)  
xcov = cbind(AvgT, Rainfall, Lag1AvgT, Lag2AvgT, Lag1Rainfall, Lag2Rainfall, AvgT*Rainfall)
fittsglm <- tsglm(DC, xreg=xcov, link = "log", distr = "poisson")
summary(fittsglm)

summary(fittsglm)[5]$coefficient[,1]/summary(fittsglm)[5]$coefficient[,2]

exp(summary(fittsglm)[5]$coefficient[,1])


summary(fit_pois)
coeftest(fit_pois)

exp(fit_pois$coefficients)

round(exp(confint(fit_pois)),3)

#Menn kendal
library(Kendall)
library(trend)

myts <- ts(YearWiseCase$x)
t.test(YearWiseCase$x)$"conf.int"
mean(YearWiseCase$x)

library(trend)
MannKendall(myts)
sens.slope(myts, conf.level = 0.95)
sea.sens.slope(myts)
