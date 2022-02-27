#Tugas 3 TPS
kond <- read.csv("tugas3tps.csv")
str(kond)
head(kond)
for (i in 1:ncol(kond)) print(sum(is.na(kond[,i])))
View(kond)
newkond <- kond[-c(297),]
sum(is.na(newkond))
str(newkond); head(newkond)

#SRS
set.seed(1234)
s1 <- sample(1:nrow(newkond), 10)
srskond <- newkond[s1,]
summary(srskond[,"IMT"])
var(srskond[,"IMT"])
sd(srskond[,"IMT"])
serr.srs <- sd(srskond[,"IMT"])/sqrt(sum(!is.na(srskond[,"IMT"])))

#Stratified RS
library("sampling")
set.seed(1234)
strat <- strata(newkond, c("IMTCat"), size = c(3, 4, 3), method = "srswor")
stratkond <-getdata(newkond, strat)
summary(stratkond[,"IMT"])
var(stratkond[,"IMT"])
sd(stratkond[,"IMT"])
serr.strat <- sd(stratkond[,"IMT"])/sqrt(sum(!is.na(stratkond[,"IMT"])))

#Systematic S
library("sampling")
set.seed(1234)
syst <- strata(newkond, c("IMTCat"), size = c(3, 4, 3), method = "systematic", pik = newkond$IMT)
systkond <-getdata(newkond, syst)
summary(systkond[,"IMT"])
var(systkond[,"IMT"])
sd(systkond[,"IMT"])
serr.syst <- sd(systkond[,"IMT"])/sqrt(sum(!is.na(systkond[,"IMT"])))

#Population
summary(newkond[,"IMT"])
var(newkond[,"IMT"])
sd(newkond[,"IMT"])

#Test of mean difference
library("BSDA")
z.test(srskond[,"IMT"], y = newkond[,"IMT"],
       sigma.x = sd(srskond[,"IMT"]),
       sigma.y = sd(newkond[,"IMT"]),
       conf.level = .95)
t.test(srskond[,"IMT"], newkond[,"IMT"])

z.test(stratkond[,"IMT"], y = newkond[,"IMT"],
       sigma.x = sd(stratkond[,"IMT"]),
       sigma.y = sd(newkond[,"IMT"]),
       conf.level = .95)

z.test(systkond[,"IMT"], y = newkond[,"IMT"],
       sigma.x = sd(systkond[,"IMT"]),
       sigma.y = sd(newkond[,"IMT"]),
       conf.level = .95)