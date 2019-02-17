rm(list=ls())
setwd("C:/Users/jse022/Dropbox/Documents/Doktorgrad UiT/Artikkel arbeidsplass")
library(readr)
library(tidyverse)
d <- read.csv("startup.csv", encoding = "UTF-16")
glimpse(d)
# 1. Data management ----
#Taking out all municipalities with less than 2000 inabitants
g <- d %>% filter(year==2006)
g <- g %>% arrange(desc(innby))
g[1:15,"kommune"] 
df <- g %>% select("kommune", "innby")
print(df, n=116)
g$innbycat <- cut(g$innby, c(0,5000,10000,25000,850000), labels=c("Smallest (<5000)", "Small (5-10000)", "Medium (10-25000)", "Large (>25000)"))
table(g$innbycat)
g$loginnby <- log(g$innby)
summary(g$loginnby)
g <- g %>% select(kommune, innbycat, loginnby)
g$innbycat <- relevel(g$innbycat, ref= 1)
d <- merge(d, g, by="kommune")

glimpse(d)
d <- d %>%group_by(kommune) %>% mutate(stemployees = (antall_ansatte/innby)*100) 
d <- d %>%group_by(kommune) %>% mutate(no_uni =stemployees-knowpercent)
d <- d %>% filter(year>2005)
d$finance <- ifelse(d$year==2008 | d$year==2009 ,1,0)
unique(d$trend)
table(d$finance)
d$loginnby <- log(d$innby)
d <- d %>% arrange(trend)
d <- d %>% arrange(komnr)

# 2. Analysis ----
# Model building - startup----
glimpse(d)
library(nlme)
library(sjstats)
library(lme4)
library(stargazer)
library(forecast)
mod0 <- lme(startup ~ 1, random= ~1|kommune, data=d, na.action=na.omit)
summary(mod0)
mod_tom <- lmer(startup ~ 1 + (1|kommune), data=d)
summary(mod_tom)
icc(mod_tom)
mod1 <- lme(startup ~ 1+ trend, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod1)
mod1b <- lme(startup ~ 1+no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod1b)
mod2 <- lme(startup ~ 1+ trend + stemployees, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod2)
d$no_uni2 <- d$no_uni^2
mod2b <- lme(startup ~ 1+ trend + no_uni+knowpercent+no_uni2, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod2b)


#Sjekker forskjeller mellom innbycat og loginnby
mod3a <- lme(startup ~ 1+ trend + stemployees+ uni + innbycat, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod3a)
mod3b <- lme(startup ~ 1+ trend + stemployees+ uni + loginnby, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod3b)
anova(mod3a, mod3b)
mod3c <- lme(startup ~ 1+ trend + stemployees + uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod3c)

mod4 <- lme(startup ~ 1+ trend + stemployees+ uni + finance + loginnby + unemployment+innby_2566, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod4)
mod5 <- lme(startup ~ 1+ trend + stemployees+ uni + finance + loginnby + unemployment+innby_2566+loginnby:stemployees, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod5)
mod6 <- lme(startup ~ 1+ trend + stemployees+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:stemployees, random= ~trend|kommune, data=d, na.action=na.omit, method="REML")
summary(mod6)

d <- d %>% group_by(kommune) %>% mutate(popgrowth = ((innby/lag(innby, 1))-1)*100) 
e <- d %>% filter(year>2006)
mod7 <- lme(popgrowth ~ 1+ trend + stemployees, random= ~trend|kommune, data=e, na.action=na.omit, method="ML")
summary(mod7)
mod8 <- lme(popgrowth ~ 1+ trend + stemployees+uni+unemployment, random= ~trend|kommune, data=e, na.action=na.omit, method="ML")
summary(mod8)
mod9 <- lme(popgrowth ~ 1+ startup, random= ~trend|kommune, data=e, na.action=na.omit, method="ML")
summary(mod9)
mod10 <- lm(e$popgrowth ~ e$startup)
summary(mod10)

ggplot( data = d, aes(x = knowpercent)) + geom_histogram(binwidth=0.21) + labs(title="") + theme(legend.position="none") 


Acf(residuals(mod5, type="normalized"))
stargazer(mod1, mod2, mod3c, mod4, mod5, type="text")
stargazer(mod5, mod6, type="text")

stargazer(mod1, mod2, mod3c,mod4, mod5, type = "html", digits =2, 
          out = "table1.html",
          title="Table 1: Multilevel regression analysis on the effect on start-ups 2006-2014", 
          covariate.labels = c("Trend", "Percentage of state employees in municipality", 
                               "University City (dummy)", "Financial crisis",
                               "Municipality population (log)",
                               "Unemployment rate", "Percentage 25-66 years"), 
          dep.var.labels = "Start-up firms per 1000 inhabitant", notes = "Standard errors in parenthesis")


stargazer(mod7, mod8, mod9, type = "html", digits =2, 
          out = "table2.html",
          title="Table 2: Multilevel regression analysis on the effect on population growth 2007-2014", 
          covariate.labels = c("Trend", "Percentage of state employees in municipality", 
                               "University City (dummy)", "Unemployment rate",
                               "Start-up firms per 1000 inhabitant"), 
          dep.var.labels = "Municipal population growth (percentage)", notes = "Standard errors in parenthesis", model.numbers          = FALSE, column.labels = c("(6)", "(7)", "(8)"))

# 3. Graphs----
glimpse(d)
gator4 <- d %>% group_by(kommune) %>% summarise(mean(startup), mean(stemployees), mean(loginnby), mean(uni), mean(innby))
gator4$startup <- gator4$`mean(startup)`
gator4$stateemployees <- gator4$`mean(stemployees)`
gator4$loginnby <- gator4$`mean(loginnby)`
gator4$uni <- gator4$`mean(uni)`
gator4$innby <- gator4$`mean(innby)`
gator4$uni <- as.factor(gator4$uni)
gator4 <- gator4[!is.na(gator4$uni),]
library(ggplot2)
figure1 <- ggplot(data = gator4, aes(x=stateemployees, y=startup, colour=uni, size =innby)) +
  geom_point(alpha=0.6) +
  geom_text(aes(label=ifelse(uni==1|stateemployees>4|startup>11.1,as.character(kommune),'')), size= 2.1, vjust=.3, hjust=-0.2) +
  labs(title="Figure1: Start-ups through relocalization of state employees?",
       subtitle="Relationship between startup firms and number of state employees in Norway 2006-2014",
       x="Yearly average - percentage of state employees in the municipality (2006-2014)", 
       y="Yearly average - new startup firms per 1000 inhabitant",
       caption = "Source: Statistics Norway and Norwegian Centre for Research Data",
       colour="University city", size="Mean population 06-14") + theme_bw()+
  theme(legend.position = "bottom", legend.background = element_rect(linetype="solid"), panel.grid.minor = element_blank()) +
  scale_size(range = c(0, 10)) +
  scale_x_continuous(breaks=c(0,5,10,15,20,25), limits = c(0,25))+scale_colour_manual(values=c("grey28", "royalblue"))
figure1

ggsave("figure1.jpg", plot=figure1, width = 9, height = 7, dpi=600)
ggsave("figure1.tiff", plot=figure1, width = 9, height = 7, dpi=300)

#4.Oversikt landsdel----

h <- d %>% group_by(landsdel) %>% summarise(sum(antall_ansatte), sum(innby), mean(stemployees))
h %>% filter(landsdel=="NA")
h <- subset(h, landsdel!="NA")
h$ansatte <- (h$`sum(antall_ansatte)`/h$`sum(innby)`)*100
h$komsnitt <- h$`mean(stemployees)`
h <- h %>% select("landsdel", "ansatte", "komsnitt")
h <- h %>% arrange(desc(ansatte))
h <- h %>% mutate(landsdel2 = factor(landsdel, landsdel)) 
h$nord <- as.factor(ifelse(h$landsdel == "Nord-Norge", "Northern Norway", "Other Region"))
pos= c("Oslo og Akershus", "Trøndelag", "Nord-Norge", "Vestlandet", "Hedmark og Oppland", "Agder og Rogaland", "Sør-Østlandet")
  
ggplot(data=h, aes(x=landsdel, y=ansatte)) +
  geom_bar(stat="identity") + scale_x_discrete(limits = pos)
figthesis1 <- ggplot(data=h, aes(x=landsdel2, y=ansatte, fill=nord)) +
  geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle=30,hjust=0.6,vjust=0.5)) + theme(legend.position = "bottom", legend.background = element_rect(linetype="solid"), panel.grid.minor = element_blank()) + labs(list(title= "State employees in Norwegian regions 2006-2014", fill="", x="Region", y="Percentage", caption="Source: NSD", subtitle="Percentage of total regional population government employeed"))
ggsave("figthesis1.jpeg", plot=figthesis1, width = 9, height = 7, dpi=250)

h <- h %>% arrange(desc(komsnitt))
h <- h %>% mutate(landsdel3 = factor(landsdel, landsdel)) 
figthesis2 <- ggplot(data=h, aes(x=landsdel3, y=komsnitt, fill=nord)) +
  geom_bar(stat="identity") + theme_bw() + theme(axis.text.x=element_text(angle=30,hjust=0.6,vjust=0.5)) + theme(legend.position = "bottom", legend.background = element_rect(linetype="solid"), panel.grid.minor = element_blank()) + labs(list(title= "State employees in Norwegian regions 2006-2014", fill="", x="Region", y="Percentage", caption="Source: NSD", subtitle="Average percentage government employeed per municipality"))
ggsave("figthesis2.jpeg", plot=figthesis2, width = 9, height = 7, dpi=250)


h2 <- d %>% filter(year==2014)
cor(h2$startup, h2$unemployment, method = "pearson", use = "complete.obs")  
h2 <- h2 %>% select("kommune", "stemployees", "landsdel")
h2 <- h2 %>% arrange(desc(stemployees))
h2

#5.Test AR----
mod1.ar1 <- update(mod1, correlation = corAR1())
mod2.ar1 <- update(mod2, correlation = corAR1())
mod3.ar1 <- update(mod3c, correlation = corAR1())
mod4.ar1 <- update(mod4, correlation = corAR1())
mod5.ar1 <- update(mod5, correlation = corAR1())
stargazer(mod1.ar1, mod2.ar1, mod3.ar1, mod4.ar1, mod5.ar1, type="text")

stargazer(mod1.ar1, mod2.ar1, mod3.ar1, mod4.ar1, mod5.ar1, type = "html", digits =2,          out = "table1.html",
          title="Table 1: Multilevel regression analysis on the effect on start-ups 2006-2014", 
          covariate.labels = c("Trend", "Percentage of state employees in municipality", 
                               "University City (dummy)", "Financial crisis",
                               "Municipality population (log)",
                               "Unemployment rate", "Percentage 25-66 years", "Percentage of state employees * Population (log)"), 
          dep.var.labels = "Start-up firms per 1000 inhabitant", notes = "Standard errors in parenthesis")

# Appendix ----
d6 <- d %>% select(kommune, startup, trend, no_uni, knowpercent, uni, loginnby, unemployment, innby_2566)
d66 <- d %>% select(kommune,innby_2566, finance)
d7 <- sapply(d66, mean, min, max, na.rm=T)
summary(d66)
stargazer(d6)
stargazer(d6, type = "html", title = "Appendix: Descriptive statistics of variables",
          align = TRUE, digits=2, out ="appendix.html")

stargazer(d6)
# Model building - innbypros----
glimpse(d)
library(nlme)
library(sjstats)
library(lme4)
library(stargazer)
library(forecast)
mod0 <- lme(innbypros ~ 1, random= ~1|kommune, data=d, na.action=na.omit)
summary(mod0)
mod_tom <- lmer(innbypros ~ 1 + (1|kommune), data=d)
summary(mod_tom)
icc(mod_tom)
mod6 <- lme(innbypros ~ 1+ trend + finance, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod1)
mod7 <- lme(innbypros ~ 1+ trend + finance + no_uni+knowpercent + uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod2)

#Sjekker forskjeller mellom innbycat og loginnby
mod8 <- lme(innbypros ~ 1+ trend + finance + no_uni+knowpercent + uni + loginnby, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod3b)
anova(mod3a, mod3b)


mod9 <- lme(innbypros ~ 1+ trend + finance + no_uni+knowpercent + uni + loginnby + loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod4)
mod10 <- lme(innbypros ~ 1+ trend + finance + no_uni+knowpercent + uni  + unemployment+innby_2566, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod10)

Acf(residuals(mod10, type="normalized"))
stargazer(mod6, mod7, mod8, mod9, mod10, type="text")


stargazer(mod1, mod2, mod3b,mod4, mod5, type = "html", digits =3, 
          out = "table1.html",
          title="Table 1: Effect on start-ups", 
          covariate.labels = c("Trend", "Financial crisis", "Percent of state employees in municipality",
                               "Percentage of university employees in municipality", 
                               "University City (dummy)", 
                               "Population in 2006 (log)",
                               "Unemployment rate", "Percent 25-66 years", 
                               "State employees * Population in 2006"))

stargazer(mod5, mod6, mod7, mod8, type="text")


summary(mod2)
stargazer(mod_tom, mod4, mod1,mod2,plmpooled1, type="text")
library(forecast)

Acf(residuals(mod4, type="normalized"))

library(plm)
plmpooled1 <- plm(startup ~ trend+antall_ansatte+pubchange+innbycat+uni, data = d, model = "pooling", index = c("kommune","year"))
summary(plmpooled1)
Acf(residuals(plmpooled1, type="normalized"))

##Tester tidslagg mm

mod7 <- lme(startup ~ 1+ trend + no_uni +knowpercent+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML", correlation=corAR1(form=~trend|kommune))
summary(mod7)
d <- d %>% arrange(year)
d <- d %>% arrange(desc(kommune))

d <- d %>%
  group_by(kommune) %>%
  mutate(startupLAG = lag(startup, 1))
d <- d %>%
  group_by(kommune) %>%
  mutate(startupLEAD = lead(startup, 1))
d <- d %>%
  group_by(kommune) %>%
  mutate(startupLEAD2 = lead(startup, 2))
d <- d %>%
  group_by(kommune) %>%
  mutate(startupLEAD3 = lead(startup, 3))


mod5LAG <- lme(startupLAG ~ 1+ trend + no_uni +knowpercent+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod5LAG)
mod5LAG2 <- lme(startupLAG2 ~ 1+ trend + no_uni +knowpercent+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod5LAG2)
mod5LAG3 <- lme(startupLAG3 ~ 1+ trend + no_uni +knowpercent+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod5LAG3)
mod5LEAD <- lme(startupLEAD ~ 1+ trend + no_uni +knowpercent+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod5LEAD)
mod5LEAD2 <- lme(startupLEAD2 ~ 1+ trend + no_uni +knowpercent+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod5LEAD2)
mod5LEAd3 <- lme(startupLEAD3 ~ 1+ trend + no_uni +knowpercent+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML")
summary(mod5LEAD3)


stargazer(mod5, mod5LEAD, type="text")

Acf(residuals(mod5, type="normalized"))

#LAGS ----
d <- d %>% arrange(year)
d <- d %>% arrange(desc(kommune))

d <- d %>%
  group_by(kommune) %>%
  mutate(knowpercentLAG = lag(knowpercent, 1))

l1 <- lme(startup ~ 1+ trend + no_uni +knowpercent+ knowpercentLAG+ uni + finance + loginnby + unemployment+innby_2566+ loginnby:no_uni, random= ~trend|kommune, data=d, na.action=na.omit, method="ML", correlation=corAR1(form=~trend|kommune))
summary(l1)
