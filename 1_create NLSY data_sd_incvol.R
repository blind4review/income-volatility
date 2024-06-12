#NLSY "replication" analysis of Grasset et al. Income Volatility and Cognition paper using CARDIA

#Open libraries
library(tidyverse)
library(haven)
library(tidyr)
library(nnet)
library(nlme)
library(lme4)
library(optimx)
library(ggeffects)
library(cowplot)
library(table1)
library(boot)
library(nptest)
library(simpleboot)
library(dplyr)

#set working directory
setwd("/Users/xxxxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL")

#####################################################################################
##import data frames and label variables
nlsy <- read.csv("NLSYINCOMEVOL.csv")
educ <- read.csv("education.csv") #forgot xrnd educational attainment variable so merge in here
  educ<-educ[,-1]
sourceyrs<-read.csv("sourceyrs.csv") #merge in years that age 40,50 and 60 health modules completed 
  sourceyrs<-sourceyrs[,-5]
hhsize<-read.csv("hhsize.csv")
  hhsize<-hhsize[,-1]  

nlsy<-cbind(nlsy, educ, sourceyrs, hhsize)
names(nlsy)
nlsy.l<-nlsy
names(nlsy.l)<-c("CESD7.H40", "ever.hypertens.H40", "curr.hypertens.H40", "ever.diab.H40", "ever.highchol.H40", "CESD7.H50", "report.hypertens.inH40", "ever.hypertens.H50", "curr.hypertens.H50", "report.diab.inH40", "ever.diab.H50", "eversmoke.H60.1", "eversmoke.H60.2", "eversmoke.daily.H60.1", "eversmoke.daily.H60.2", "currsmoke.daily.H60", "report.lasttimesmokeday.H60", "timelastsmokedaily.H60", "CESD7.H60", "ever.hypertens.H60", "curr.hypertens.H60", "ever.diab.H60", "height.ft.H60", "height.in.H60", 
"case.ID", "firstonly.raceth", "second.raceth", "third.raceth", "fourth.raceth", "fifth.raceth", "sixth.raceth", "primary.raceth", "NLSY.raceth", "sex", 
"highestgradeattn.1990", "highestgradecompl.1990", "weight.lbs.1990", "netfamwealth.1990", "iwstatus.1990", "netfamincome.1990", "maristat.1990", "age.1990", "employed.1990", "hourswork.1990", "retired.1990", "disabled.1990", 
"highestgradeattn.1991", "highestgradecompl.1991", "iwstatus.1991", "netfamincome.1991", "maristat.1991", "employed.1991", "hourswork.1991", "retired.1991", "disabled.1991", 
"highestgradeattn.1992", "highestgradecompl.1992", "weight.lbs.1992", "CESD.appetite.1992", "CESD.troublemind.1992", "CESD.depressed.1992", "CESD.effort.1992", "CESD.sleep.1992", "CESD.sad.1992", "CESD.going.1992", "CESD20.score.1992", "missflag.CESD20.1992", "CESD7.score.1992", "netfamwealth.1992", "eversmoke.1992", "currsmoke.daily.1992", "mosincesmoked.daily.1992", "yrsincesmoked.daily.1992", "iwstatus.1992", "netfamincome.1992", "maristat.1992", "employed.1992", "hourswork.1992", "retired.1992", "disabled.1992", 
"highestgradeattn.1993", "highestgradecompl.1993", "weight.lbs.1993", "netfamwealth.1993", "iwstatus.1993", "netfamincome.1993", "maristat.1993", "employed.1993", "hourswork.1993", "retired.1993", "disabled.1993", 
"highestgradeattn.1994", "highestgradecompl.1994", "weight.lbs.1994", "CESD.appetite.1994", "CESD.troublemind.1994", "CESD.depressed.1994", "CESD.effort.1994", "CESD.sleep.1994", "CESD.sad.1994", "CESD.going.1994", "CESD7.score.1994", "netfamwealth.1994", "eversmoke1.1994", "eversmoke2.1994", "currsmoke.daily.1994", "unit.timelastsmokdaily.1994", "timelastsmokdaily.1994", "iwstatus.1994", "netfamincome.1994", "maristat.1994", "employed.1994", "hourswork.1994", "retired.1994", #did not collect disabled status in 1994?
"iwstatus.1996", "netfamincome.1996", "maristat.1996", "employed.1996", "hourswork.1996", "retired.1996", "disabled.1996", "highestgradeattn.1996", "highestgradecompl.1996", "weight.lbs.1996", "netfamwealth.1996", 
"highestgradeattn.1998", "highestgradecompl.1998", "weight.lbs.1998", "freq.lightPA.1998", "freq.vigPA.1998", "netfamwealth.1998", "eversmoke1.1998", "eversmoke2.1998", "currsmoke.daily.1998", "unit.timelastsmokdaily.1998", "timelastsmokdaily.1998", "iwstatus.1998", "netfamincome.1998", "maristat.1998", "employed.1998", "hourswork.1998", "retired.1998", "disabled.1998", 
"highestgradeattn.2000", "highestgradecompl.2000", "weight.lbs.2000", "freq.lightPA.2000", "freq.vigPA.2000", "netfamwealth.2000", "iwstatus.2000", "netfamincome.2000", "maristat.2000", "employed.2000", "hourswork.2000", "retired.2000", "disabled.2000", 
"othrace.white.2002", "othrace.black.2002", "othrace.asian.2002", "othrace.nathipi.2002", "othrace.aminaknat.2002", "othrace.other.2002", "othrace.hisplat.2002", "highestgradeattn.2002", "highestgradecompl.2002", "weight.lbs.2002", "freq.vigPA.2002", "unit.freq.vigPA.2002", "unable.vigPA.2002", "freq.lightPA.2002", "unit.freq.lightPA.2002", "unable.lightPA.2002", "iwstatus.2002", "netfamincome.2002", "maristat.2002", "employed.2002", "hourswork.2002", "retired.2002", "disabled.2002", 
"highestgradeattn.2004", "highestgradecompl.2004", "weight.lbs.2004", "freq.vigPA.2004", "unit.freq.vigPA.2004", "unable.freq.vigPA.2004", "freq.lightPA.2004", "unit.freq.lightPA.2004", "unable.freq.lightPA.2004", "netfamwealth.2004", "num.maxcreditcard.2004", "iwstatus.2004", "netfamincome.2004", "maristat.2004", "employed.2004", "hourswork.2004", "retired.2004", "disabled.2004", 
"highestgradeattn.2006", "highestgradecompl.2006", "weight.lbs.2006", "height.ft.2006", "height.in.2006", "freq.vigPA.2006", "unit.freq.vigPA.2006", "unable.freq.vigPA.2006", "freq.lightPA.2006", "unit.freq.lightPA.2006", "unable.freq.lightPA.2006", "iwstatus.2006", "netfamincome.2006", "maristat.2006", "employed.2006", "hourswork.2006", "retired.2006", "disabled.2006",
"highestgradeattn.2008", "highestgradecompl.2008", "weight.lbs.2008", "height.ft.2008", "height.in.2008", "freq.vigPA.2008", "unit.freq.vigPA.2008", "unable.freq.vigPA.2008", "freq.lightPA.2008", "unit.freq.lightPA.2008", "unable.freq.lightPA.2008", "currmeds1.bldsgr.2008", "currmeds1.bp.2008", "currmeds2.bldsgr.2008", "currmeds2.bp.2008", "eversmoke1.2008", "eversmoke2.2008", "eversmokedaily1.2008", "eversmokedaily2.2008", "currsmoke.daily.2008", "timelastsmokdaily.2008", "unit.timelastsmokdaily.2008", "netfamwealth.2008", "iwstatus.2008", "netfamincome.2008", "maristat.2008", "employed.2008", "hourswork.2008", "retired.2008", "disabled.2008", 
"highestgradeattn.2010", "highestgradecompl.2010", "weight.lbs.2010","height.ft.2010", "height.in.2010", "freq.vigPA.2010", "unit.freq.vigPA.2010", "unable.freq.vigPA.2010", "freq.lightPA.2010", "unit.freq.lightPA.2010", "unable.freq.lightPA.2010", "currmeds1.bldsgr.2010", "currmeds1.bp.2010", "currmeds2.bldsgr.2010", "currmeds2.bp.2010", "eversmoke1.2010", "eversmoke2.2010", "eversmokedaily1.2010", "eversmokedaily2.2010", "currsmoke.daily.2010", "timelastsmokdaily.2010", "unit.timelastsmokdaily.2010", "iwstatus.2010", "netfamincome.2010", "maristat.2010", "employed.2010", "hourswork.2010", "retired.2010", "disabled.2010", "numjobseverhad.2010", 
"highestgradecompl.1989", "highestgradecompl", "sourceyear.40","sourceyear.50", "sourceyear.60","death.flag",
"hhsize.1990", "hhsize.1991", "hhsize.1992", "hhsize.1993", "hhsize.1994", "hhsize.1996", "hhsize.1998", "hhsize.2000", "hhsize.2002", "hhsize.2004", "hhsize.2006", "hhsize.2008", "hhsize.2010")

names(nlsy.l)

#when cleaning can compare nlsy and nlsy.l dataframes as needed to make sure no variables was mislabeled

table(nlsy.l$sourceyear.40, exclude=NULL)
table(nlsy.l$sourceyear.50, exclude=NULL)
table(nlsy.l$sourceyear.60, exclude=NULL)
table(nlsy.l$hhsize.1990, exclude=NULL)

#clean/recode interview status variable (-4=responded at interview; >0=non-respondant)
statusvars<-grep("iwstat", names(nlsy.l), value = TRUE)
nlsy.l[, statusvars] <- lapply(nlsy.l[, statusvars], function(x) ifelse(x==-4, 1, 0))

table(nlsy.l$iwstatus.1990, exclude=NULL)

#merge in sampleID
sampleid<-read.csv("sampleID.csv")
names(sampleid)<-c("case.ID","sample.ID")
nlsy.l<-merge(nlsy.l, sampleid, by="case.ID")

table(nlsy.l$sample.ID) #use this later when restricting to 1990 baseline sample - remove oversamples later dropped from sample

#----------------------------------------------------------------------------------------------------------------------------------
##(1).Cleaning exposure variables
incomes<-grep("income", names(nlsy.l), value = TRUE)

#recode invalid/missing/non-interview values to NA
nlsy.l[,incomes] <- lapply(nlsy.l[,incomes], function(x) ifelse(x<0, NA, x))
  summary(nlsy.l$netfamincome.1990)
  summary(nlsy.l$netfamincome.2010)

#cpi for 1990 real dollars for years 1990, 1991, 1992, 1993, and every 2 years between 1994-2010
cpi1990.incvol<-c(replicate(n=12686, 1.00), replicate(n=12686, 0.96), replicate(n=12686, 0.93), replicate(n=12686, 0.90), replicate(n=12686, 0.88), replicate(n=12686, 0.83), replicate(n=12686, 0.80), replicate(n=12686, 0.76), replicate(n=12686, 0.73), replicate(n=12686, 0.69), replicate(n=12686, 0.65), replicate(n=12686, 0.61), replicate(n=12686, 0.60))

#convert income to 1990 income using https://www.usinflationcalculator.com/
newinc1990<-c("income90.1990","income90.1991", "income90.1992", "income90.1993", "income90.1994", 
              "income90.1996", "income90.1998", "income90.2000", "income90.2002", "income90.2004",
              "income90.2006", "income90.2008", "income90.2010")
nlsy.l[,newinc1990]<-nlsy.l[,incomes]*cpi1990.incvol
  summary(nlsy.l$netfamincome.1990)  
  summary(nlsy.l$income90.1990)
    #spot check 1991 and 2008
    head(nlsy.l[c(incomes[2],newinc1990[2])])
    c(20000*0.96,24200*0.96, 61000*0.96)
    head(nlsy.l[c(incomes[12],newinc1990[12])])
    c(7000*0.61,141050*0.61)

#now, equivalize these variables for household size
eqnewinc1990<-c("eqincome90.1990","eqincome90.1991", "eqincome90.1992", "eqincome90.1993", "eqincome90.1994", 
                "eqincome90.1996", "eqincome90.1998", "eqincome90.2000", "eqincome90.2002", "eqincome90.2004",
                "eqincome90.2006", "eqincome90.2008", "eqincome90.2010")
hhsize<-grep("hhsize", names(nlsy.l), value = TRUE)
nlsy.l[,hhsize]<- lapply(nlsy.l[,hhsize], function(x) ifelse(x<0, NA, x))

nlsy.l[,eqnewinc1990]<-nlsy.l[,newinc1990]/sqrt(nlsy.l[,hhsize])
summary(nlsy.l$eqincome90.1990)
  #spot check 1991 
  head(nlsy.l[c(incomes[2],newinc1990[2],hhsize[2],eqnewinc1990[2])])
  23232/sqrt(5)
  58560/sqrt(2)
  
#create income volatility measures in cleaned dataset
    #select participants who have at least 3 income measures

#----------------------------------------------------------------------------------------------------------------------------------
##(2).Cleaning outcomes
cognition <- read.csv("all_cognition.csv")
cognition <- cognition[,-c(1:4)]
  colnames(cognition)
  names(cognition)[c(1:30)] <- c("VerbalFluency.2018", "CogTestYear","selfratemem", "selfratemem.change",
                                   "WR1.completed","WR1.listnum","WR1.List1","WR1.List2","WR1.List3","WR1.List4",
                                   "BC.completed", "BC.1stattempt","BC1.yesnocorrect","BC.2ndattempt","BC2.yesnocorrect", "BC86.completed",
                                   "BC86.1stattempt","BC86.1.yesnocorrect","BC86.2ndattempt","BC86.2.yesnocorrect",
                                   "ST.100m7", "ST.93m7","ST.86m7","ST.79m7","ST.72m7", "WR2.yesno","WR2.List1","WR2.List2","WR2.List3","WR2.List4")
    
#recode negative value into NA
cognition[, 1:30] <- lapply(cognition[, 1:30], function(x) ifelse(x<0, NA, x))
    
#dist of cog exam year
round(prop.table(table(cognition$CogTestYear)),2)*100 #proportion of people who completed cognitive battery in each year (at age 50)
    
#dist of self-rated memory
table(cognition$selfratemem, exclude = NULL)
  #same coding as HRS
  #1 Excellent; 2 Very Good; 3 Good; 4 Fair; 5 Poor

#(a).clean immediate word recall    
#take which column is non-NA and assign it as the immediate word recall value
table(cognition$WR1.completed, exclude=NULL)
cognition$immedrecall <- ifelse(!is.na(cognition$WR1.List1), cognition$WR1.List1,
                                ifelse(!is.na(cognition$WR1.List2), cognition$WR1.List2,
                                       ifelse(!is.na(cognition$WR1.List3), cognition$WR1.List3, 
                                              ifelse(!is.na(cognition$WR1.List4), cognition$WR1.List4, NA))))
    
  table(cognition$immedrecall, exclude=NULL)  
  table(cognition$WR1.completed, cognition$immedrecall, useNA = "ifany")
    
#(b).clean delayed word recall
#take which column is non-NA and assign it as the delayed word recall value
cognition$delrecall <- ifelse(!is.na(cognition$WR2.List1), cognition$WR2.List1,
                              ifelse(!is.na(cognition$WR2.List2), cognition$WR2.List2,
                                     ifelse(!is.na(cognition$WR2.List3), cognition$WR2.List3, 
                                            ifelse(!is.na(cognition$WR2.List4), cognition$WR2.List4, NA))))
    
  table(cognition$WR2.yesno, exclude=NULL)
  table(cognition$delrecall, exclude = NULL)

#(c).backward counting: 1=correct, 5=incorrect, 6 wants to start over

table(cognition$BC86.1.yesnocorrect) #attempt 1
table(cognition$BC86.2.yesnocorrect) #attempt 2
table(cognition$BC1.yesnocorrect) #attempt 1
table(cognition$BC2.yesnocorrect) #attempt 2
table(cognition$BC1.yesnocorrect, cognition$BC2.yesnocorrect)
table(cognition$BC86.1.yesnocorrect, cognition$BC86.2.yesnocorrect)
    
#make backward counting variable like AV team - 1 if correct on first attempt, 0 if not
#bc 20
  cognition$bc20 <- ifelse(cognition$BC1.yesnocorrect==1,1,0)
  cognition$bc20[cognition$BC1.yesnocorrect==97]<-NA
  table(cognition$BC1.yesnocorrect, cognition$bc20, exclude=NULL)
  table(cognition$bc20)

#bc 86
  cognition$bc86 <- ifelse(cognition$BC86.1.yesnocorrect==1,1,0)
  cognition$bc86[cognition$BC86.1.yesnocorrect==97]<-NA
  table(cognition$BC86.1.yesnocorrect, cognition$bc86, exclude=NULL)
  table(cognition$bc86)
  
#CD typically averages the scores from 20 and 86 when examining BC individual assessment as outcome itself
  cognition$avgBC<-(cognition$bc20+cognition$bc86)/2
  
  #if using average, try to recover a little missing data
  cognition$avgBC.r<-ifelse(is.na(cognition$avgBC),cognition$bc20,
                             ifelse(is.na(cognition$avgBC),cognition$bc86,cognition$avgBC)) 
  table(cognition$avgBC)
  table(cognition$avgBC.r)
  hist(cognition$avgBC.r)
  
#(d). serial 7s: ST.100m7=93 ; ST.93m7=86 ; ST.86m7=79 ; ST.79m7=72 ; ST.72m7=65
#Ask the patient to take away 7 from 100. Ask them to continue subtracting 7 and continue to a total of 5 subtractions (one point is given for each correct answer to a maximum score of five points).
table(cognition$ST.100m7, useNA="ifany")
table(cognition$ST.93m7, useNA="ifany") 
table(cognition$ST.100m7-cognition$ST.93m7) 

#create serial7s in a way that makes each test independent (subtract 7 from whatever number on previous attempt was)
cognition <- cognition %>%
  mutate(
    ST.100cat = ifelse(ST.100m7==93,1,0), #first trial only counts if 93 since everyone starts at 100
    ST.93cat = ifelse(ST.100m7-ST.93m7==7,1,0), #second trial either 86 (correct answer) or a difference of 7 between answer on first trial and this trial
    ST.86cat = ifelse(ST.93m7-ST.86m7==7,1,0),
    ST.79cat = ifelse(ST.86m7-ST.79m7==7,1,0),
    ST.72cat = ifelse(ST.79m7-ST.72m7==7,1,0),
    serial7s = ST.100cat+ST.93cat+ST.86cat+ST.79cat+ST.72cat
  )

table(cognition$ST.100cat) 
table(cognition$ST.93cat) 
table(cognition$ST.86cat) 
table(cognition$ST.79cat) 
table(cognition$ST.72cat) 
table(cognition$serial7s,exclude = NULL) 

cognition$serial7s<-rowSums(cognition[,c("ST.100cat","ST.93cat","ST.86cat","ST.79cat","ST.72cat")],na.rm=TRUE)
#if NAs for all time points change back to NA instead of 0 (rowsums makes it 0)
cognition$serial7s[is.na(cognition$ST.100cat)&is.na(cognition$ST.93cat)&is.na(cognition$ST.86cat)&is.na(cognition$ST.79cat)&is.na(cognition$ST.72cat)]<-NA
  table(cognition$serial7s,exclude = NULL) #appropriate number of NAs now 

#only extract the cognition var we need
colnames(cognition)
cognition <- cognition[, c(1:4, 31:36, 42)]

#merge in data on baseline cognition
#import baseline cognition
basecog<-read.csv("baseline_cog.csv")
basecog<-basecog[,-1]  
names(basecog)<-c("AFQTpctlscore80.81","AFQTpctlscore89rev.81","AFQTpctlscore06rev.81","ASVAB.arithmath.z","ASVAB.arithmath.wtpct",
                  "ASVAB.wordpara.z","ASVAB.wordpara.wtpct","ASVAB.arith.z","ASVAB.arith.wtpct","ASVAB.word.z","ASVAB.word.wtpct",
                  "ASVAB.para.z","ASVAB.para.wtpct","ASVAB.math.z","ASVAB.math.wtpct")
basecog <- lapply(basecog, function(x) ifelse(x<0, NA, x))

cognition <- cbind(cognition, basecog)

summary(cognition$AFQTpctlscore80.81)
summary(cognition$AFQTpctlscore89rev.81)
summary(cognition$AFQTpctlscore06rev.81)/1000

#06 revised has 3 implied decimal places so divide variable by 1000
cognition$AFQTpctlscore06rev.81.r<-cognition$AFQTpctlscore06rev.81/1000
  summary(cognition$AFQTpctlscore06rev.81.r)

#create global cognition score TICS-m
#chrome-extension://efaidnbmnnnibpcajpcglclefindmkaj/https://watermark.silverchair.com/gbac138.pdf?token=AQECAHi208BE49Ooan9kkhW_Ercy7Dm3ZL_9Cf3qfKAc485ysgAAA3QwggNwBgkqhkiG9w0BBwagggNhMIIDXQIBADCCA1YGCSqGSIb3DQEHATAeBglghkgBZQMEAS4wEQQMRe9099ZIak6WnNG9AgEQgIIDJ0Y3DOd9L1wSz1Tl8wNovHS3i9bdOv9VCo2aQ2_CnFUW6z36pvsz5_bT42DzhbYOSwJ7iUufQVEmtJ8c2MWvatW9YeBdJ0FB4ekYVr7QZu3YsxaULyNAGlB93KbYwBRmbXkqgR4bn21IJGA7vODpOTR1Fkj14kqdI5WTSf4ORpRo97ltj-Augk8VjcLBLydTFpQnxg2bAuC294FlMg1gbX1YuMlZ0RjXhUGIWnNi0wHgORw5XlIEq1XF1qcd8Y_D9GwWUnEv_y7KqoseWBhgfoxunGLSJthzTBrRi6FXVzhWMwrjPB8OpwPXQmDisjsXnBYDfHZl019hTe06D0nhiw-jLuMCiKiDBzYfWyV9tmHQ4bY5HzazPdrJ0oyneg3gFpwZlfNtaNKb31oqzwZ9vLMVnNQZj9GPqetnK-NiFIsU9BVaMZ4PbZ9ZXuSkZunRS6h_t09empuGFKwbYaiaMl6KOppGW58_PMLU69RoxP5Rhn9O4DS235iRepuV1AdgPfW9fi4AZg5AwmmehfWrvQgG8ZBuFDzpBvqIw3Y8zOp2sJ8gquIM7r4rq7fm6Byp2zRssRcOfbH5y0vERSV03xhL8AlkBJnca6s8nQ417P1y3KHmaidSnj9ghQmFk7jktBtTw1lEtrVi5mfALRLAfN7_wcGZ0kGcsFNXyUtgph_yCb8JSjHICud_4sKth8t8Krp4r92LXeyD40unKT7Vgk34qyDngR2F2ybbOQ0butJahd1OAVfNiDg1FFHVqhAnSfmHCYpv00SBVHhrhnbXZ1bs5EVCKEXJiWJNXv0SJ_HtqbYLbSoUAkt-lS7K8sCX0N1cjxqfexL8P6azsQdKC7qRCLNhWLSCaDnZ41sqogCJvi0nioR6cDq0WlSdhPKOavn1NHM7LhTo4UKyrXtqMxWCxrC7PmCaOhJxUJl2O5Z8bxeo2yz0SU1bHnmhh5IX5syG4Xtt35E1OBsYoLfthHx_8qpOs4ZVBJbrVev3lmXaH5UfDAOF7f1EKbPtulvUeufD5nhSuGv2p1GfzRMRNRA-2UZVDeBjqzM6kNYULTbLopd6Kum45Q
summary(cognition$immedrecall)

#CD creates raw summary score by summing IR, DR, both BC, and s7; want a complete case sample, so no na.rm here (only sum where all tests have scores)
cognition$TICSm.score<-rowSums(cognition[,c("immedrecall","delrecall","bc20","bc86","serial7s")])
  summary(cognition$TICSm.score)
  hist(cognition$TICSm.score) #looks pretty normal

#merge cognition data in with nlsy.l dataset
nlsy.l<-cbind(nlsy.l,cognition)
  colnames(nlsy.l)   

#----------------------------------------------------------------------------------------------------------------------------------
##(3).Cleaning baseline covariates (model 1)

##age
summary(nlsy.l$age.1990, na.rm=TRUE)
  #-5 means "non-interview"
nlsy.l$age.1990[nlsy.l$age.1990<0]<-NA

#sex
table(nlsy.l$sex, exclude=NULL)

#race
table(nlsy.l$NLSY.raceth, exclude=NULL) #1=Hispanic, 2=Black, 3=non-Black, non-Hispanic

#education (at baseline i.e. 1990)
table(nlsy.l$highestgradecompl.1989, exclude=NULL)
  table(nlsy.l$highestgradeattn.1990, exclude=NULL) #lots missing here in 1990 - I think just asked to people who left school? Use 1989
  table(nlsy.l$highestgradecompl.1990, exclude=NULL) #lots missing here in 1990 - I think just asked to people who left school? Use 1989
nlsy.l$highestgradecompl.1989[nlsy.l$highestgradecompl.1989==-5|nlsy.l$highestgradecompl.1989==-3]<-NA
nlsy.l$educat89<-ifelse(nlsy.l$highestgradecompl.1989>12,1,0)
  table(nlsy.l$educat89, exclude=NULL)

#education (cumulative NLSY summary measure)
table(nlsy.l$highestgradecompl, exclude=NULL) 
nlsy.l$highestgradecompl[nlsy.l$highestgradecompl<0]<-NA
nlsy.l$educat<-ifelse(nlsy.l$highestgradecompl>12,1,0)
  table(nlsy.l$educat, exclude=NULL)

table(nlsy.l$highestgradecompl.1989, nlsy.l$highestgradecompl, exclude=NULL)
table(nlsy.l$educat89, nlsy.l$educat, exclude=NULL) #pre-exposure educational attainment

#marital status at baseline (1990)
table(nlsy.l$maristat.1990, exclude=NULL)
nlsy.l$maristat.1990[nlsy.l$maristat.1990<0]<-NA

#income at baseline (1990) #to compare: https://www.census.gov/library/publications/1991/demo/p60-174.html

summary(nlsy.l$income90.1990, exclude=NULL) #mean: 32131
summary(nlsy.l$eqincome90.1990, exclude=NULL) #mean: 20125
hist(nlsy.l$eqincome90.1990)
  #Grasset et al. paper mean income: 36405.7 (but this is after restricting to people contributing 3+ time points - likely higher than NLSY right now because of that)
  ((39681*(1780/3287))+(32253*(1108/3287))+(33326*(399/3287)))

#----------------------------------------------------------------------------------------------------------------------------------
##(4).Cleaning baseline covariates for second model - health conditions and behaviors (model 2)

##(a).bmi

grep("weight", names(nlsy.l), value = TRUE)
grep("height", names(nlsy.l), value = TRUE)

#clean weight variables (remove negative values)
weights.lbs<-grep("weight", names(nlsy.l), value = TRUE)
nlsy.l[, weights.lbs] <- lapply(nlsy.l[, weights.lbs], function(x) ifelse(x<0, NA, x))
  summary(nlsy.l$weight.lbs.1990)

#convert weights to kg
weights.kgs<-c("weight.kgs.1990","weight.kgs.1992","weight.kgs.1993","weight.kgs.1994","weight.kgs.1996",
               "weight.kgs.1998","weight.kgs.2000","weight.kgs.2002","weight.kgs.2004","weight.kgs.2006",
               "weight.kgs.2008","weight.kgs.2010")

nlsy.l[,weights.kgs]<-nlsy.l[,weights.lbs]*0.45359237
  summary(nlsy.l$weight.kgs.1990)
  
#choose height variable to use and clean
#people typically achieve height by age 18
  summary(nlsy.l$age.1990) #youngest person in 1985 would be 20
  #use 1985 exam reported height? NLSY has 1985 and then again in 2006

#merge in height in 1985
height1985<-read.csv("height1985.csv")

#convert height to m
height1985$height.in.1985<-height1985[,2]
height1985$height.m.1985<-height1985$height.in.1985*0.0254
height1985<-height1985[,-c(1,2)]

#merge 1985 height in with full dataset
nlsy.l<-cbind(nlsy.l,height1985)

#clean and convert all other heights to m
heights<-grep("height", names(nlsy.l), value = TRUE)
nlsy.l[, heights] <- lapply(nlsy.l[, heights], function(x) ifelse(x<0, NA, x))
  summary(nlsy.l$height.m.1985)

nlsy.l$height.m.2010<-((nlsy.l$height.ft.2010*12)+nlsy.l$height.in.2010)*0.0254
  summary(nlsy.l$height.m.2010) 
  
#try two different BMI measures and see how the heights compare in the final analytic dataset
nlsy.l$BMI1990.ht1985<-nlsy.l$weight.kgs.1990/(nlsy.l$height.m.1985^2) #end up using this version (pre-exposure)
nlsy.l$BMI1990.ht2010<-nlsy.l$weight.kgs.1990/(nlsy.l$height.m.2010^2)
  summary(nlsy.l$BMI1990.ht1985)
  summary(nlsy.l$BMI1990.ht2010)

#(b).blood pressure/hypertension - don't end up using (post-exposure)
#don't have these at baseline...only during follow-up
grep("hyper", names(nlsy.l), value = TRUE)
grep("bp", names(nlsy.l), value = TRUE)

#ever diagnosed by doctor with hypertension
nlsy.l$ever.hypertens.H40[nlsy.l$ever.hypertens.H40<0]<-NA
nlsy.l$ever.hypertens.H50[nlsy.l$ever.hypertens.H50<0]<-NA
nlsy.l$ever.hypertens.H60[nlsy.l$ever.hypertens.H60<0]<-NA

table(nlsy.l$ever.hypertens.H40, exclude=NULL)
table(nlsy.l$ever.hypertens.H50, exclude=NULL)
table(nlsy.l$ever.hypertens.H60, exclude=NULL)

table(nlsy.l$ever.hypertens.H40,nlsy.l$ever.hypertens.H50, exclude=NULL)

nlsy.l$everHBP12<-ifelse(nlsy.l$ever.hypertens.H40==0&nlsy.l$ever.hypertens.H50==0,0,
                    ifelse(nlsy.l$ever.hypertens.H40==1|nlsy.l$ever.hypertens.H50==1,1,NA))
nlsy.l$everHBP12[is.na(nlsy.l$ever.hypertens.H40)&nlsy.l$ever.hypertens.H50==0]<-0
nlsy.l$everHBP12[is.na(nlsy.l$ever.hypertens.H50)&nlsy.l$ever.hypertens.H40==0]<-0
nlsy.l$everhbp<-ifelse(nlsy.l$everHBP12==0&nlsy.l$ever.hypertens.H60==0,0,
                  ifelse(nlsy.l$everHBP12==1|nlsy.l$ever.hypertens.H60==1,1,NA))
nlsy.l$everhbp[is.na(nlsy.l$everHBP12)&nlsy.l$ever.hypertens.H60==0]<-0
nlsy.l$everhbp[is.na(nlsy.l$ever.hypertens.H60)&nlsy.l$everHBP12==0]<-0

table(nlsy.l$everhbp,nlsy.l$age.1990, exclude=NULL)

#(c).currently on bp medication (end of follow-up...) - don't end up using (post-exposure)
table(nlsy.l$currmeds1.bp.2010, exclude=NULL)
table(nlsy.l$currmeds2.bp.2010, exclude=NULL)
table(nlsy.l$currmeds1.bp.2010, nlsy.l$currmeds2.bp.2010, exclude=NULL)
nlsy.l$bpmeds2010<-ifelse(nlsy.l$currmeds1.bp.2010==1|nlsy.l$currmeds2.bp.2010==1,1,
                          ifelse(nlsy.l$currmeds1.bp.2010==0|nlsy.l$currmeds2.bp.2010==0,0,NA))

table(nlsy.l$bpmeds2010, exclude=NULL)

#(d).ever diagnosed by doctor with high cholesterol - don't end up using (post-exposure)
grep("chol", names(nlsy.l), value = TRUE)
nlsy.l$everhichol<-ifelse(nlsy.l$ever.highchol.H40<0,NA,nlsy.l$ever.highchol.H40)
table(nlsy.l$everhichol, exclude=NULL)

#(e). ever diagnosed by doctor with diabetes or blood sugar problem - don't end up using (post-exposure)
grep("diab", names(nlsy.l), value = TRUE)
diabetes<-grep("diab", names(nlsy.l), value = TRUE)
nlsy.l[,diabetes] <- lapply(nlsy.l[,diabetes], function(x) ifelse(x<0, NA, x))

nlsy.l$everdiab12<-ifelse(nlsy.l$ever.diab.H40==0&nlsy.l$ever.diab.H50==0,0,
                            ifelse(nlsy.l$ever.diab.H40==1|nlsy.l$ever.diab.H50==1,1,NA))
nlsy.l$everdiab12[is.na(nlsy.l$ever.diab.H40)&nlsy.l$ever.diab.H50==0]<-0
nlsy.l$everdiab12[is.na(nlsy.l$ever.diab.H50)&nlsy.l$ever.diab.H40==0]<-0
nlsy.l$everdiab<-ifelse(nlsy.l$everdiab12==0&nlsy.l$ever.diab.H60==0,0,
                          ifelse(nlsy.l$everdiab12==1|nlsy.l$ever.diab.H60==1,1,NA))
nlsy.l$everdiab[is.na(nlsy.l$everdiab12)&nlsy.l$ever.diab.H60==0]<-0
nlsy.l$everdiab[is.na(nlsy.l$ever.diab.H60)&nlsy.l$everdiab12==0]<-0

table(nlsy.l$everdiab, exclude=NULL)

#(f).physical activity (1998 earliest; could also do 2010) - don't end up using (post-exposure)
grep("PA", names(nlsy.l), value = TRUE)
exercise<-grep("PA", names(nlsy.l), value = TRUE)
nlsy.l[,exercise] <- lapply(nlsy.l[,exercise], function(x) ifelse(x<0, NA, x))

table(nlsy.l$freq.lightPA.1998, exclude=NULL)
table(nlsy.l$freq.vigPA.1998, exclude=NULL)
table(nlsy.l$freq.vigPA.2010, exclude=NULL)
table(nlsy.l$unit.freq.vigPA.2010, exclude=NULL)
table(nlsy.l$unable.freq.vigPA.2010, exclude=NULL)

nlsy.l <- nlsy.l %>%
  mutate(
    vigPA.2010 = case_when(
      freq.vigPA.2010 == 0 | freq.vigPA.2010 == 996 | unit.freq.vigPA.2010 ==5  ~ 5, #unable to do/never
      freq.vigPA.2010 >=1 & unit.freq.vigPA.2010 ==1 ~ 1,# >= everyday
      freq.vigPA.2010 >=7 & unit.freq.vigPA.2010 ==2 ~ 1,# >= everyday
      freq.vigPA.2010 >=30 & unit.freq.vigPA.2010 ==3 ~ 1,# >= everyday
      freq.vigPA.2010 >=365 & unit.freq.vigPA.2010 ==4 ~ 1,# >= everyday
      (freq.vigPA.2010 >1 & freq.vigPA.2010 <7) & unit.freq.vigPA.2010 ==2 ~ 2,# >1 times per week
      (freq.vigPA.2010 >4 & freq.vigPA.2010 <30) & unit.freq.vigPA.2010 ==3 ~ 2,# >1 times per week
      (freq.vigPA.2010 >12 & freq.vigPA.2010 <365) & unit.freq.vigPA.2010 ==4 ~ 2,# >1 times per week
      freq.vigPA.2010 ==1 & unit.freq.vigPA.2010 ==2 ~ 3,# 1 per week
      freq.vigPA.2010 ==4 & unit.freq.vigPA.2010 ==3 ~ 3,# 1 per week
      freq.vigPA.2010 ==12 & unit.freq.vigPA.2010 ==4 ~ 3,# 1 per week
      (freq.vigPA.2010 >0 & freq.vigPA.2010 <4) & unit.freq.vigPA.2010 ==3 ~ 4,# 1-3 per month (or <1 per week)
      (freq.vigPA.2010 >0 & freq.vigPA.2010 <12) & unit.freq.vigPA.2010 ==4 ~ 4),# 1-3 per month (or <1 per week)
    lightPA.2010 = case_when(
      freq.lightPA.2010 == 0 | freq.lightPA.2010 == 996 | unit.freq.lightPA.2010 ==5  ~ 5, #unable to do/never
      freq.lightPA.2010 >=1 & unit.freq.lightPA.2010 ==1 ~ 1,# >= everyday
      freq.lightPA.2010 >=7 & unit.freq.lightPA.2010 ==2 ~ 1,# >= everyday
      freq.lightPA.2010 >=30 & unit.freq.lightPA.2010 ==3 ~ 1,# >= everyday
      freq.lightPA.2010 >=365 & unit.freq.lightPA.2010 ==4 ~ 1,# >= everyday
      (freq.lightPA.2010 >1 & freq.lightPA.2010 <7) & unit.freq.lightPA.2010 ==2 ~ 2,# >1 times per week
      (freq.lightPA.2010 >4 & freq.lightPA.2010 <30) & unit.freq.lightPA.2010 ==3 ~ 2,# >1 times per week
      (freq.lightPA.2010 >12 & freq.lightPA.2010 <365) & unit.freq.lightPA.2010 ==4 ~ 2,# >1 times per week
      freq.lightPA.2010 ==1 & unit.freq.lightPA.2010 ==2 ~ 3,# 1 per week
      freq.lightPA.2010 ==4 & unit.freq.lightPA.2010 ==3 ~ 3,# 1 per week
      freq.lightPA.2010 ==12 & unit.freq.lightPA.2010 ==4 ~ 3,# 1 per week
      (freq.lightPA.2010 >0 & freq.lightPA.2010 <4) & unit.freq.lightPA.2010 ==3 ~ 4,# 1-3 per month (or <1 per week)
      (freq.lightPA.2010 >0 & freq.lightPA.2010 <12) & unit.freq.lightPA.2010 ==4 ~ 4),# 1-3 per month (or <1 per week)
  )    
table(nlsy.l$vigPA.2010, exclude=NULL)
table(nlsy.l$lightPA.2010, exclude=NULL)
  
  #adjustment for b-e barely budges results, but conceptually it's funny to adjust for things happening at outcome time point; don't adjust

#(g).smoking (1992) - a health behavior closely tied to high blood pressure
grep("smok", names(nlsy.l), value = TRUE)
smokes<-grep("smok", names(nlsy.l), value = TRUE)
nlsy.l[,smokes] <- lapply(nlsy.l[,smokes], function(x) ifelse(x<0, NA, x))

table(nlsy.l$eversmoke.1992, exclude=NULL)
table(nlsy.l$currsmoke.daily.1992, exclude=NULL)
table(nlsy.l$eversmoke.1992, nlsy.l$currsmoke.daily.1992, exclude=NULL) #0:never, 1=daily, 2=occasionally
table(nlsy.l$mosincesmoked.daily.1992)
table(nlsy.l$yrssincesmoked.daily.1992)

#create a current vs. not binary variable (probably most accurate)
nlsy.l$currsmoke.1992<-nlsy.l$currsmoke.daily.1992
nlsy.l$currsmoke.1992[nlsy.l$currsmoke.daily.1992==1|nlsy.l$currsmoke.daily.1992==2]<-1
nlsy.l$currsmoke.1992[nlsy.l$eversmoke.1992==0]<-0
  table(nlsy.l$currsmoke.1992, exclude=NULL)

#create a current, former, never smoker variable where people who are smokers who "never" smoke daily are "former" 
nlsy.l$smokestat.1992<-nlsy.l$currsmoke.daily.1992
nlsy.l$smokestat.1992[nlsy.l$currsmoke.daily.1992==1|nlsy.l$currsmoke.daily.1992==2]<-"current"
nlsy.l$smokestat.1992[nlsy.l$currsmoke.daily.1992==0]<-"former"
nlsy.l$smokestat.1992[nlsy.l$eversmoke.1992==0]<-"never"
  table(nlsy.l$smokestat.1992, exclude=NULL)
  
#(h).depression (1992)
grep("CES", names(nlsy.l), value = TRUE)
nlsy.l$CESD7.score.1992[nlsy.l$CESD7.score.1992<0]<-NA
nlsy.l$CESD20.score.1992[nlsy.l$CESD20.score.1992<0]<-NA
  summary(nlsy.l$CESD7.score.1992)
  table(nlsy.l$CESD7.score.1992, exclude=NULL)
  hist(nlsy.l$CESD7.score.1992)

#can use a score of >=8 to screen for depression: #https://www.researchgate.net/publication/234089547_Evaluating_the_seven-item_Center_for_Epidemiologic_Studies_Depression_Scale_Short-Form_A_longitudinal_US_community_study
nlsy.l$depression.cesd7.92<-ifelse(nlsy.l$CESD7.score.1992>=8,1,0)
  table(nlsy.l$depression.cesd7.92, exclude=NULL)
  prop.table(table(nlsy.l$depression.cesd7.92)) #17% screen for depression...matches the article above
nlsy.l$depression.cesd20.92<-ifelse(nlsy.l$CESD20.score.1992>=16,1,0)
  table(nlsy.l$depression.cesd20.92, exclude=NULL)
  prop.table(table(nlsy.l$depression.cesd20.92)) #this is the measure in Grasset - use this

#additional variable to consider (not in Grasset et al. but may help make up for lack of biomarker measures by capturing aspects of SES/health)  

#(i).alcohol use (1992) - a health behavior closely tied to heart disease (not in Grasset but might be good to include here since it's closer to baseline)
alc_unempl_ins<-read.csv("alcohol_unempl_insurance.csv")
names(alc_unempl_ins)
alc<-alc_unempl_ins[,-c(1:6)] 
names(alc)<-c("healthprvntwork90","healthlimittypework90","healthlimitamtwork90","healthlimitchk90",
              "famhaveinsurance90","curemplins.rr.90","prevemplins.rr.90","spcuremplins.rsp.90","prevemplins.rsp.90",
              "curemplins.spr.90","prevemplins.spr.90","spcuremplins.spsp.90","prevemplins.spsp.90",
              "weeksunempl.pyear90", "weeksunempl.slint90",
              "numdaysalc.lastmo","numdrinksavg")

#use 1992 variables and keep it simple; clean alcohol variables for now (clean others in later step)
alc[,c(16,17)] <- lapply(alc[,c(16,17)], function(x) ifelse(x<0, NA, x))
  table(alc$numdaysalc.lastmo, exclude=NULL)
  table(alc$numdrinksavg, exclude=NULL)
  table(alc$numdaysalc.lastmo, alc$numdrinksavg, exclude=NULL)

alc <- alc %>%
  mutate(
    daysdrinkpweek92 = case_when(numdaysalc.lastmo ==0 ~ 0,
                               numdaysalc.lastmo ==1 ~ 7,
                               numdaysalc.lastmo ==2 ~ 5.5,
                               numdaysalc.lastmo ==3 ~ 3.5,
                               numdaysalc.lastmo ==4 ~ 1.5,
                               numdaysalc.lastmo ==5 ~ 0.5),
    drinkspweek92 = daysdrinkpweek92*numdrinksavg
  )

alc$drinkspweek92[alc$numdaysalc.lastmo==0]<-0
table(alc$drinkspweek92, exclude=NULL)

#CDC guidelines: moderate= <=1 drink/day for women and <=2 drinks/day for men
            #heavy: 8+ drinks/week for women and 15+ drinks/week for men
#https://www.cdc.gov/alcohol/faqs.htm#:~:text=The%20Dietary%20Guidelines%20for%20Americans%20recommends%20that%20adults%20who%20choose,on%20a%20day%20for%20men.

#never, moderate, heavy - sex specific, so need to merge alc in with full dataset
names(alc)
alc<-alc[,-c(16,17)] #just keep alc vars needed

nlsy.l<-cbind(nlsy.l,alc)
colnames(nlsy.l)

table(nlsy.l$sex) #1=male; 2=female

nlsy.l <- nlsy.l %>%
  mutate(
    heavydrink = case_when(sex == 2 & drinkspweek92 == 0 ~ 0, #never
                           sex == 2 & (drinkspweek92>0&drinkspweek92<8) ~ 1, #moderate/not heavy
                           sex == 2 & drinkspweek92 >=8 ~ 2, #heavy
                           sex == 1 & drinkspweek92 == 0 ~ 0, #never
                           sex == 1 & (drinkspweek92>0&drinkspweek92<15) ~ 1, #moderate/not heavy
                           sex == 1 & drinkspweek92 >=15 ~ 2) #heavy
    )

table(nlsy.l$heavydrink, exclude=NULL)

#----------------------------------------------------------------------------------------------------------------------------------
##(5).Cleaning baseline covariates for third model - baseline income and employment status

#(a).income at baseline (cleaned above)
hist(nlsy.l$eqincome90.1990)
summary(nlsy.l$income90.1990)  
hist(nlsy.l$eqincome90.1990)
summary(nlsy.l$eqincome90.1990)  

#(b).employment status at baseline
colnames(nlsy.l)
grep("work", names(nlsy.l), value = TRUE)
empl90vars<-c("employed.1990","retired.1990","disabled.1990","hourswork.1990","weeksunempl.pyear90","weeksunempl.slint90","healthprvntwork90","healthlimittypework90","healthlimitamtwork90")
nlsy.l[,empl90vars] <- lapply(nlsy.l[,empl90vars], function(x) ifelse(x<0, NA, x))

nlsy.l$lbrf.90 <- rep("Not in LbrF", nrow(nlsy.l))
table(nlsy.l$lbrf.90, exclude = NULL)
nlsy.l$lbrf.90[nlsy.l$iwstatus.1990==0]<-NA
nlsy.l$lbrf.90[nlsy.l$employed.1990==1&nlsy.l$hourswork.1990==1] <- "Work FT"
nlsy.l$lbrf.90[nlsy.l$employed.1990==1&nlsy.l$hourswork.1990==0] <- "Work PT"
nlsy.l$lbrf.90[nlsy.l$employed.1990==0] <- "Unemployed"
nlsy.l$lbrf.90[nlsy.l$retired.1990==1] <- "Retired"
nlsy.l$lbrf.90[nlsy.l$disabled.1990==1] <- "Disabled"
  table(nlsy.l$lbrf.90, exclude = NULL)

table(nlsy.l$lbrf.90, nlsy.l$healthprvntwork90, exclude=NULL) #overlaps quite a bit with "disabled" category, so don't need to adjust for both

#(c).additional variable to consider (not in Grasset et al. but may help make up for lack of biomarker measures by capturing aspects of SES/health)  
#(c.i).employer provided health insurance (via self or spouse?)
insvars<-grep("ins", names(nlsy.l), value = TRUE)
nlsy.l[,insvars] <- lapply(nlsy.l[,insvars], function(x) ifelse(x<0, NA, x))

table(nlsy.l$famhaveinsurance90, exclude=NULL)
table(nlsy.l$famhaveinsurance90, nlsy.l$curemplins.spr.90, exclude=NULL)

nlsy.l <- nlsy.l %>%
  mutate(
    epinsurance90 = case_when(famhaveinsurance90==0 ~ 0, #has no insurance
                            (curemplins.rr.90==1|prevemplins.rr.90==1|spcuremplins.rsp.90==1|prevemplins.rsp.90==1) ~ 2, #employer-provided health insurance
                            famhaveinsurance90==1 ~ 1) #has other insurance
  )

table(nlsy.l$epinsurance90)

#c.ii (wealth - at baseline and average over time period?) - to explore as modifier, later
grep("wealth", names(nlsy.l), value = TRUE)
wealthvars<-c("netfamwealth.1990", "netfamwealth.1992", "netfamwealth.1993", 
              "netfamwealth.1994", "netfamwealth.1996", "netfamwealth.1998", 
              "netfamwealth.2000", "netfamwealth.2004", "netfamwealth.2008")
nlsy.l[,wealthvars] <- lapply(nlsy.l[,wealthvars], function(x) ifelse(x<0, NA, x))

#cpi for 1990 real dollars for years 1990, 1992, 1993, 1994, 1996, 1998, 2000, 2004, 2008
cpi1990.wealthyrs<-c(replicate(n=12686, 1.00), replicate(n=12686, 0.93), replicate(n=12686, 0.90), replicate(n=12686, 0.88), replicate(n=12686, 0.83), replicate(n=12686, 0.80), replicate(n=12686, 0.76), replicate(n=12686, 0.69), replicate(n=12686, 0.61))

newwealth1990<-c("wealth90.1990", "wealth90.1992", "wealth90.1993", "wealth90.1994", 
              "wealth90.1996", "wealth90.1998", "wealth90.2000", "wealth90.2004","wealth90.2008")

nlsy.l[,newwealth1990]<-nlsy.l[,wealthvars]*cpi1990.wealthyrs

  summary(nlsy.l$netfamwealth.2008)
  summary(nlsy.l$wealth90.2008)

#average wealth over exposure period to make low.wealth modifier variable
wealthvars01<-c("wealthmiss.1990", "wealthmiss.1992", "wealthmiss.1993", 
              "wealthmiss.1994", "wealthmiss.1996", "wealthmiss.1998", 
              "wealthmiss.2000", "wealthmiss.2004", "wealthmiss.2008")
nlsy.l[,wealthvars01] <- lapply(nlsy.l[,wealthvars], function(x) ifelse(!is.na(x), 1, 0))

  table(nlsy.l$wealthmiss.1990, exclude=NULL)
  table(nlsy.l$wealthmiss.2008, exclude=NULL) #missingness increases over time - makes sense

nlsy.l$wealthtimes<-rowSums(nlsy.l[,c("wealthmiss.1990","wealthmiss.1992","wealthmiss.1993","wealthmiss.1994","wealthmiss.1996","wealthmiss.1998","wealthmiss.2000","wealthmiss.2004","wealthmiss.2008")], na.rm=TRUE)
  table(nlsy.l$wealthtimes) #how many time points wealth information available? 0 should = NAs of mean wealth variable, below

nlsy.l$avg90wealth<-rowMeans(nlsy.l[,c("wealth90.1990","wealth90.1992","wealth90.1993","wealth90.1994","wealth90.1996","wealth90.1998","wealth90.2000","wealth90.2004","wealth90.2008")], na.rm=TRUE)
  summary(nlsy.l$avg90wealth) #average wealth of available time points with wealth information for each participant
    #2245 NA and that makes sense because 2245 people with "wealhtimes==0" meaning missing at all time points
  
#create low.wealth variable for effect modification analysis (<$2,000?)
nlsy.l$low.wealth<-ifelse(nlsy.l$avg90wealth<2000,1,0) #is 2000 the correct amount for 1990 even?
  prop.table(table(nlsy.l$low.wealth))

#c.iii assets (savings)
##may be more appropriate to look just at savings (vs total HH wealth) when looking at $2,000 low savings indicator 
savings<-read.csv("savings_r.csv") 
names(savings)<-c("case.ID", "assets.yn.90","assets.1990","assets.yn.92","assets.1992","assets.yn.93","assets.1993","assets.yn.94","assets.1994",
                  "assets.yn.96","assets.1996","assets.yn.98","assets.1998","assets.yn.00","assets.2000","assets.2000.1500plus","assets.2000.500plus","assets.2000.3500",
                  "assets.yn.04","assets.2004","assets.2004.est1","assets.2004.est2","assets.yn.08","assets.2008","assets.2008.est1","assets.2008.est2")
savings[, c(1:26)] <- lapply(savings[, c(1:26)], function(x) ifelse(x<0, NA, x))

table(savings$assets.2000,savings$assets.2000.1500plus, useNA="ifany")
table(savings$assets.2000.1500plus)
summary(savings$assets.2000[savings$assets.2000.1500plus==0])

nlsy.l<-left_join(nlsy.l,savings,by=c("case.ID"))

summary(nlsy.l$assets.2000[nlsy.l$assets.2000>3500]) #for those >3500 give mean of savings for people >3500 in sample

#recover some missing asset data using alternative measures in later years (categories etc.)
nlsy.l$assets.2000.c<-nlsy.l$assets.2000
nlsy.l$assets.2000.c[nlsy.l$assets.2000.500plus==0]<-250
nlsy.l$assets.2000.c[nlsy.l$assets.2000.500plus==1&nlsy.l$assets.2000.1500plus==0]<-1000
nlsy.l$assets.2000.c[nlsy.l$assets.1500plus==1&nlsy.l$assets.2000.3500==0]<-2500
nlsy.l$assets.2000.c[nlsy.l$assets.2000.3500==1]<-35373

summary(nlsy.l$assets.2000.c)

summary(nlsy.l$assets.2004)
summary(nlsy.l$assets.2004.c)
nlsy.l$assets.2004.c<-nlsy.l$assets.2004
nlsy.l$assets.2004.c[is.na(nlsy.l$assets.2004.c)]<-nlsy.l$assets.2004.est1[is.na(nlsy.l$assets.2004.c)]
nlsy.l$assets.2004.c[is.na(nlsy.l$assets.2004.c)]<-nlsy.l$assets.2004.est2[is.na(nlsy.l$assets.2004.c)]

summary(nlsy.l$assets.2008)
summary(nlsy.l$assets.2008.c)
nlsy.l$assets.2008.c<-nlsy.l$assets.2008
nlsy.l$assets.2008.c[is.na(nlsy.l$assets.2008.c)]<-nlsy.l$assets.2008.est1[is.na(nlsy.l$assets.2008.c)]
nlsy.l$assets.2008.c[is.na(nlsy.l$assets.2008.c)]<-nlsy.l$assets.2008.est2[is.na(nlsy.l$assets.2008.c)]

#inflation adjust assets for average assets variable (in 1990 dollars)
nlsy.l$assets.1990.90<-nlsy.l$assets.1990*1
nlsy.l$assets.1992.90<-nlsy.l$assets.1992*0.93
nlsy.l$assets.1993.90<-nlsy.l$assets.1993*0.90
nlsy.l$assets.1994.90<-nlsy.l$assets.1994*0.88
nlsy.l$assets.1996.90<-nlsy.l$assets.1996*0.83
nlsy.l$assets.1998.90<-nlsy.l$assets.1998*0.80
nlsy.l$assets.2000.90<-nlsy.l$assets.2000.c*0.76
nlsy.l$assets.2004.90<-nlsy.l$assets.2004.c*0.69
nlsy.l$assets.2008.90<-nlsy.l$assets.2008.c*0.61

#where in prior question people said "no assets" make continuous assets variable a 0 (wealth already summarizes like this acros all sources)
nlsy.l$assets.1990.90<-ifelse(nlsy.l$assets.yn.90==0,0,nlsy.l$assets.1990.90)
nlsy.l$assets.1992.90<-ifelse(nlsy.l$assets.yn.92==0,0,nlsy.l$assets.1992.90)
nlsy.l$assets.1993.90<-ifelse(nlsy.l$assets.yn.93==0,0,nlsy.l$assets.1993.90)
nlsy.l$assets.1994.90<-ifelse(nlsy.l$assets.yn.94==0,0,nlsy.l$assets.1994.90)
nlsy.l$assets.1996.90<-ifelse(nlsy.l$assets.yn.96==0,0,nlsy.l$assets.1996.90)
nlsy.l$assets.1998.90<-ifelse(nlsy.l$assets.yn.98==0,0,nlsy.l$assets.1998.90)
nlsy.l$assets.2000.90<-ifelse(nlsy.l$assets.yn.00==0,0,nlsy.l$assets.2000.90)
nlsy.l$assets.2004.90<-ifelse(nlsy.l$assets.yn.04==0,0,nlsy.l$assets.2004.90)
nlsy.l$assets.2008.90<-ifelse(nlsy.l$assets.yn.08==0,0,nlsy.l$assets.2008.90)

#alternative assets<2000 variable looks within each time period (so don't need to do inflation adjusted measure here, all is relative to that time period - did you have $2000 in assets or not in 1990, 1991, etc.)
nlsy.l$assets2000.1990<-ifelse(nlsy.l$assets.1990>2000,1,0)
  table(nlsy.l$assets2000.1990, useNA="ifany")
  table(nlsy.l$assets.yn.90, useNA="ifany")
nlsy.l$assets2000.1990<-ifelse(nlsy.l$assets.yn.90==0,0,nlsy.l$assets2000.1990)
nlsy.l$assets2000.1992<-ifelse(nlsy.l$assets.1992>2000,1,0)
nlsy.l$assets2000.1992<-ifelse(nlsy.l$assets.yn.92==0,0,nlsy.l$assets2000.1992)
nlsy.l$assets2000.1993<-ifelse(nlsy.l$assets.1993>2000,1,0)
nlsy.l$assets2000.1993<-ifelse(nlsy.l$assets.yn.93==0,0,nlsy.l$assets2000.1993)
nlsy.l$assets2000.1994<-ifelse(nlsy.l$assets.1994>2000,1,0)
nlsy.l$assets2000.1994<-ifelse(nlsy.l$assets.yn.94==0,0,nlsy.l$assets2000.1994)
nlsy.l$assets2000.1996<-ifelse(nlsy.l$assets.1996>2000,1,0)
nlsy.l$assets2000.1996<-ifelse(nlsy.l$assets.yn.96==0,0,nlsy.l$assets2000.1996)
nlsy.l$assets2000.1998<-ifelse(nlsy.l$assets.1998>2000,1,0)
nlsy.l$assets2000.1998<-ifelse(nlsy.l$assets.yn.98==0,0,nlsy.l$assets2000.1998)
nlsy.l$assets2000.2000<-ifelse(nlsy.l$assets.2000.c>2000,1,0)
nlsy.l$assets2000.2000<-ifelse(nlsy.l$assets.yn.00==0,0,nlsy.l$assets2000.2000)
nlsy.l$assets2000.2004<-ifelse(nlsy.l$assets.2004.c>2000,1,0)
nlsy.l$assets2000.2004<-ifelse(nlsy.l$assets.yn.04==0,0,nlsy.l$assets2000.2004)
nlsy.l$assets2000.2008<-ifelse(nlsy.l$assets.2008.c>2000,1,0)
nlsy.l$assets2000.2008<-ifelse(nlsy.l$assets.yn.08==0,0,nlsy.l$assets2000.2008)

#create missing indicator variable at each survey wave
nlsy.l$assetsmiss.90<-ifelse(!is.na(nlsy.l$assets.1990.90),1,0)
nlsy.l$assetsmiss.92<-ifelse(!is.na(nlsy.l$assets.1992.90),1,0)
nlsy.l$assetsmiss.93<-ifelse(!is.na(nlsy.l$assets.1993.90),1,0)
nlsy.l$assetsmiss.94<-ifelse(!is.na(nlsy.l$assets.1994.90),1,0)
nlsy.l$assetsmiss.96<-ifelse(!is.na(nlsy.l$assets.1996.90),1,0)
nlsy.l$assetsmiss.98<-ifelse(!is.na(nlsy.l$assets.1998.90),1,0)
nlsy.l$assetsmiss.00<-ifelse(!is.na(nlsy.l$assets.2000.90),1,0)
nlsy.l$assetsmiss.04<-ifelse(!is.na(nlsy.l$assets.2004.90),1,0)
nlsy.l$assetsmiss.08<-ifelse(!is.na(nlsy.l$assets.2008.90),1,0)

nlsy.l$assetstimes<-rowSums(nlsy.l[,c("assetsmiss.90","assetsmiss.92","assetsmiss.93","assetsmiss.94","assetsmiss.96","assetsmiss.98","assetsmiss.00","assetsmiss.04","assetsmiss.08")], na.rm=TRUE)
  table(nlsy.l$assetstimes) #1808 are the NAs

nlsy.l$avg90assets<-rowMeans(nlsy.l[,c("assets.1990.90","assets.1992.90","assets.1993.90","assets.1994.90","assets.1996.90","assets.1998.90","assets.2000.90","assets.2004.90","assets.2008.90")], na.rm=TRUE)
  summary(nlsy.l$avg90assets) #1808 NAs

nlsy.l$p2000assets<-rowMeans(nlsy.l[,c("assets2000.1990","assets2000.1992","assets2000.1993","assets2000.1994","assets2000.1996","assets2000.1998","assets2000.2000","assets2000.2004","assets2000.2008")], na.rm=TRUE)
  summary(nlsy.l$p2000assets)
  
nlsy.l$assets2000<-ifelse(nlsy.l$avg90assets<2000,1,0) #average assets over exposure period <2000
  prop.table(table(nlsy.l$assets2000, exclude=NULL))
nlsy.l$assets2000.c<-ifelse(nlsy.l$avg90assets==0,2,
                          ifelse(nlsy.l$avg90assets<2000,1,0))
table(nlsy.l$p2000assets, exclude=NULL)
  
nlsy.l$p2000assets.c<-ifelse(nlsy.l$p2000assets==1,"always",
                               ifelse(nlsy.l$p2000assets==0,"never","sometimes")) #average assets over exposure period <2000
  prop.table(table(nlsy.l$p2000assets.c, exclude=NULL))
  
#----------------------------------------------------------------------------------------------------------------------------------
##(6). Limiting to 1990 survey

#restrict to people who were in the interview at baseline (iwstatus.1990==1)
###(??) ask if this restriction is necessary
nlsy.1990<-nlsy.l[nlsy.l$iwstatus.1990==1,]
#n=10436 - this includes some of the special sample that was later dropped

table(nlsy.1990$sample.ID) #oversamples: poor nonBlack, nonHispanic 9, 12 (military oversamples should have already been dropped in 1984)
table(nlsy.l$iwstatus.1990, exclude=NULL)

nlsy.1990[nlsy.1990$sample.ID==9|nlsy.1990$sample.ID==12,]<-NA
nlsy.1990<-nlsy.1990[!is.na(nlsy.1990$sample.ID),] #8953

table(nlsy.1990$sample.ID, exclude=NULL)
table(nlsy.1990$CogTestYear, exclude=NULL) 

write.csv(nlsy.1990,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/nlsy.1990_sample.csv")
save(nlsy.1990, file = "/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/nlsy.1990_sample.rda")

#----------------------------------------------------------------------------------------------------------------------------------
##(7). Restricting to participants with 3 income time points

#restrict to people with 3 income time points
inc90vars<-grep("incom", names(nlsy.1990), value = TRUE)[27:39]
incomeind<-c("i.inc90","i.inc91","i.inc92","i.inc93","i.inc94","i.inc96",
             "i.inc98","i.inc00","i.inc02","i.inc04","i.inc06","i.inc08","i.inc10")

nlsy.1990[,incomeind] <- lapply(nlsy.1990[,inc90vars], function(x) ifelse(x>=0, 1, NA))
nlsy.1990[,incomeind] <- lapply(nlsy.1990[,incomeind], function(x) ifelse(is.na(x), 0, x))
  summary(nlsy.1990$income90.1990)
  table(nlsy.1990$i.inc90, exclude=NULL)
  aggregate(nlsy.1990$income90.1990, by=list(nlsy.1990$lbrf.90), FUN="mean", na.rm=TRUE)

#sum number of time points with 1 (non-missing income information)
nlsy.1990$sumincomes<-rowSums(nlsy.1990[,incomeind], na.rm=TRUE)
nlsy.1990$inc3times<-ifelse(nlsy.1990$sumincomes>=3,1,0)
  table(nlsy.1990$sumincomes)
  table(nlsy.1990$inc3times) 

#first restrict to people with baseline 1990 income
nlsyBinc<-nlsy.1990[!is.na(nlsy.1990$eqincome90.1990),] #7398

#then restrict to people with 3+ time points over exposure period
nlsy3inc<-nlsyBinc[nlsyBinc$inc3times==1,] #7257

table(nlsy3inc$sumincomes)

#----------------------------------------------------------------------------------------------------------------------------------
##(8). Creating income volatility measures

#(a.)first carry forward last observation to fill in missing adjusted/deflated income data (just to avoid NAs)
write.csv(nlsy3inc[,c("case.ID",eqnewinc1990)],"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/missing_inc_patterns.csv")
  #save file into NLSY replication folder
  #open in excel and then: 1) delete first column, 2) change "NA" to blank cells
  #select all and then click "find & select" and then "Blanks" and then select "okay"
  #now right click to delete the blank cells and when prompted allow excel to "shift cells left"
    #now all of the individual exposure trajectories are sequential with no missing time points in between
    #this means a standard deviation of % change variable can be created for each person without having to do imputation over time (e.g., if carry forward, can make the sd income volatility variable look conservative/less volatile than in truth because it adds time points of no change in between actual time points of observed income)
    #number of income drops over time will be the same regardless of approach
  #rename column headings to time1-time13 and use these when making income volatility variables using this approch  

#import cleaned income file back into R
fixmissinc<-read.csv("/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/missing_inc_patterns_fixed.csv")  
#fixmissinc<-read.csv("/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/missing_inc_patterns_fixed.csv")  

nlsy3inc<-left_join(nlsy3inc,fixmissinc,by=c("case.ID"))

#also create variables that do carry forward imputation
incimpute<-nlsy3inc %>%
  mutate(
    eqincome90.1991i = ifelse(is.na(eqincome90.1991), eqincome90.1990, eqincome90.1991),
    eqincome90.1992i = ifelse(is.na(eqincome90.1992), eqincome90.1991i, eqincome90.1992),
    eqincome90.1993i = ifelse(is.na(eqincome90.1993), eqincome90.1992i, eqincome90.1993),
    eqincome90.1994i = ifelse(is.na(eqincome90.1994), eqincome90.1993i, eqincome90.1994),
    eqincome90.1996i = ifelse(is.na(eqincome90.1996), eqincome90.1994i, eqincome90.1996),
    eqincome90.1998i = ifelse(is.na(eqincome90.1998), eqincome90.1996i, eqincome90.1998),
    eqincome90.2000i = ifelse(is.na(eqincome90.2000), eqincome90.1998i, eqincome90.2000),
    eqincome90.2002i = ifelse(is.na(eqincome90.2002), eqincome90.2000i, eqincome90.2002),
    eqincome90.2004i = ifelse(is.na(eqincome90.2004), eqincome90.2002i, eqincome90.2004),
    eqincome90.2006i = ifelse(is.na(eqincome90.2006), eqincome90.2004i, eqincome90.2006),
    eqincome90.2008i = ifelse(is.na(eqincome90.2008), eqincome90.2006i, eqincome90.2008),
    eqincome90.2010i = ifelse(is.na(eqincome90.2010), eqincome90.2008i, eqincome90.2010)
  )
  
  summary(nlsy3inc$eqincome90.2010)
  summary(incimpute$eqincome90.2010)
  summary(incimpute$eqincome90.2010i)
  
#zeros are true zeros...so, how many zeros? 
table(nlsy3inc$eqincome90.1990[nlsy3inc$eqincome90.1990==0]) #only 112 in 1990

#do x+1 conversion for all income variables? run into issues when people have consecutive 0s (denominator becomes undefined in formula) - very few have this issue...probably okay to add a really small constant to all income variables to get around this
#for variables where missing time points were removed and everything was shifted left
  incTime<-grep("Time", names(incimpute), value = TRUE) 
  incimpute[,incTime] <- lapply(incimpute[,incTime], function(x) x=x+0.0001) 
    table(nlsy3inc$eqincome90.1990[nlsy3inc$eqincome90.1990==0])
    table(incimpute$Time1[incimpute$Time1==0]) #no more zeros

#for variables where missing time points were imputed via carry forward approach
  eqnewinc1990i<-c("eqincome90.1990","eqincome90.1991i", "eqincome90.1992i", "eqincome90.1993i", "eqincome90.1994i", 
                "eqincome90.1996i", "eqincome90.1998i", "eqincome90.2000i", "eqincome90.2002i", "eqincome90.2004i",
                "eqincome90.2006i", "eqincome90.2008i", "eqincome90.2010i")
  incimpute[,eqnewinc1990i] <- lapply(incimpute[,eqnewinc1990i], function(x) x=x+0.0001)
  head(nlsy3inc$eqincome90.2010)
  head(incimpute$eqincome90.2010i)
    table(nlsy3inc$eqincome90.1990[nlsy3inc$eqincome90.1990==0]) #112, as above
    table(incimpute$eqincome90.1990[incimpute$eqincome90.1990==0]) #no more zeros
  
#(b). Income volatility measure #1: SD of percent change in income (adjusted/deflated income)

#Step 1: calculate the percentage change in deflated income between 2 consecutive surveys
#((Y_t2-Y_t1)/(0.5*(Y_t1+Y_t2)))*100

#no imputation but time points squeezed together (adjust for # time points provided in analyses)
incimpute$inc.pct9190 <- ((incimpute$Time2-incimpute$Time1)/(0.5*(incimpute$Time2+incimpute$Time1)))*100 
incimpute$inc.pct9291 <- ((incimpute$Time3-incimpute$Time2)/(0.5*(incimpute$Time3+incimpute$Time2)))*100
incimpute$inc.pct9392 <- ((incimpute$Time4-incimpute$Time3)/(0.5*(incimpute$Time4+incimpute$Time3)))*100
incimpute$inc.pct9493 <- ((incimpute$Time5-incimpute$Time4)/(0.5*(incimpute$Time5+incimpute$Time4)))*100
incimpute$inc.pct9694 <- ((incimpute$Time6-incimpute$Time5)/(0.5*(incimpute$Time6+incimpute$Time5)))*100
incimpute$inc.pct9896 <- ((incimpute$Time7-incimpute$Time6)/(0.5*(incimpute$Time7+incimpute$Time6)))*100
incimpute$inc.pct0098 <- ((incimpute$Time8-incimpute$Time7)/(0.5*(incimpute$Time8+incimpute$Time7)))*100
incimpute$inc.pct0200 <- ((incimpute$Time9-incimpute$Time8)/(0.5*(incimpute$Time9+incimpute$Time8)))*100
incimpute$inc.pct0402 <- ((incimpute$Time10-incimpute$Time9)/(0.5*(incimpute$Time10+incimpute$Time9)))*100
incimpute$inc.pct0604 <- ((incimpute$Time11-incimpute$Time10)/(0.5*(incimpute$Time11+incimpute$Time10)))*100
incimpute$inc.pct0806 <- ((incimpute$Time12-incimpute$Time11)/(0.5*(incimpute$Time12+incimpute$Time11)))*100
incimpute$inc.pct1008 <- ((incimpute$Time13-incimpute$Time12)/(0.5*(incimpute$Time13+incimpute$Time12)))*100
  summary(incimpute$inc.pct9190) 
  summary(incimpute$inc.pct1008) 

#imputation (carry forward)
incimpute$inc.pct9190i <- ((incimpute$eqincome90.1991i-incimpute$eqincome90.1990)/(0.5*(incimpute$eqincome90.1991i+incimpute$eqincome90.1990)))*100 
incimpute$inc.pct9291i <- ((incimpute$eqincome90.1992i-incimpute$eqincome90.1991i)/(0.5*(incimpute$eqincome90.1992i+incimpute$eqincome90.1991i)))*100
incimpute$inc.pct9392i <- ((incimpute$eqincome90.1993i-incimpute$eqincome90.1992i)/(0.5*(incimpute$eqincome90.1993i+incimpute$eqincome90.1992i)))*100
incimpute$inc.pct9493i <- ((incimpute$eqincome90.1994i-incimpute$eqincome90.1993i)/(0.5*(incimpute$eqincome90.1994i+incimpute$eqincome90.1993i)))*100
incimpute$inc.pct9694i <- ((incimpute$eqincome90.1996i-incimpute$eqincome90.1994i)/(0.5*(incimpute$eqincome90.1996i+incimpute$eqincome90.1994i)))*100
incimpute$inc.pct9896i <- ((incimpute$eqincome90.1998i-incimpute$eqincome90.1996i)/(0.5*(incimpute$eqincome90.1998i+incimpute$eqincome90.1996i)))*100
incimpute$inc.pct0098i <- ((incimpute$eqincome90.2000i-incimpute$eqincome90.1998i)/(0.5*(incimpute$eqincome90.2000i+incimpute$eqincome90.1998i)))*100
incimpute$inc.pct0200i <- ((incimpute$eqincome90.2002i-incimpute$eqincome90.2000i)/(0.5*(incimpute$eqincome90.2002i+incimpute$eqincome90.2000i)))*100
incimpute$inc.pct0402i <- ((incimpute$eqincome90.2004i-incimpute$eqincome90.2002i)/(0.5*(incimpute$eqincome90.2004i+incimpute$eqincome90.2002i)))*100
incimpute$inc.pct0604i <- ((incimpute$eqincome90.2006i-incimpute$eqincome90.2004i)/(0.5*(incimpute$eqincome90.2006i+incimpute$eqincome90.2004i)))*100
incimpute$inc.pct0806i <- ((incimpute$eqincome90.2008i-incimpute$eqincome90.2006i)/(0.5*(incimpute$eqincome90.2008i+incimpute$eqincome90.2006i)))*100
incimpute$inc.pct1008i <- ((incimpute$eqincome90.2010i-incimpute$eqincome90.2008i)/(0.5*(incimpute$eqincome90.2010i+incimpute$eqincome90.2008i)))*100
  summary(incimpute$inc.pct9190i)  
  summary(incimpute$inc.pct1008i)  
  

#Step 2. Compute SD of these percent changes
#without imputation
  pctchgvars<-grep("inc.pct", names(incimpute), value = TRUE)[1:12]
  incimpute$sd_pct <- apply(incimpute[, pctchgvars], 1, function(x) sd(x, na.rm = T))
    summary(incimpute$sd_pct)
    hist(incimpute$sd_pct)
    
      test<-incimpute
      test$exposure<-apply(test[,c("inc.pct9190","inc.pct9291","inc.pct9392","inc.pct9493","inc.pct9694","inc.pct9896","inc.pct0098","inc.pct0200","inc.pct0402","inc.pct0604","inc.pct0806","inc.pct1008")], MARGIN = 1, FUN=sd, na.rm=T)
      summary(test$exposure) #gives the same answer - so it is doing the interindividualsd
    
  #In Grasset looked at 1 SD of income volatility, which in their analysis was 34.5 SD of percent change
    sd(incimpute$sd_pct)   
    incimpute$sd_incvol<-incimpute$sd_pct/sd(incimpute$sd_pct)
    
      
#with imputation
  pctchgvars.i<-grep("inc.pct", names(incimpute), value = TRUE)[13:24]
  incimpute$sd_pct.i <- apply(incimpute[, pctchgvars.i], 1, function(x) sd(x, na.rm = T))
    summary(incimpute$sd_pct.i)
    hist(incimpute$sd_pct.i)
    
  #In Grasset looked at 1 SD of income volatility, which in their analysis was 34.5 SD of percent change
    sd(incimpute$sd_pct.i)   
    incimpute$sd_incvol.i<-incimpute$sd_pct.i/sd(incimpute$sd_pct.i)
  
    
#(c).Income volatility measure #2: number of income drops

#use unadjusted income (see Grasset et al. p.e1892)
#here imputation or not does not matter because outcome variable is the number of income drops...so if carry forward in between those will not count as drops until you hit the next time point in which a >25% occurs.
  #so either dataset/approach will produce same # drops per person
  
#Step 1: create mean income over time
unadjincomes<-grep("netfamin", names(incimpute), value = TRUE)
equnadjincomes<-c("eqnetfamincome.1990","eqnetfamincome.1991","eqnetfamincome.1992","eqnetfamincome.1993","eqnetfamincome.1994",
                  "eqnetfamincome.1996","eqnetfamincome.1998","eqnetfamincome.2000","eqnetfamincome.2002","eqnetfamincome.2004",
                  "eqnetfamincome.2006","eqnetfamincome.2008","eqnetfamincome.2010")
incimpute[,equnadjincomes]<-incimpute[,unadjincomes]/sqrt(incimpute[,hhsize])

  incimpute$mean_inc <- apply(incimpute[, equnadjincomes], 1, function(x) mean(x, na.rm = T)) 
  summary(incimpute$mean_inc)#No NAs because na.rm=TRUE; also then don't have carry forward values impacting mean calculation here

#carry forward as above (easier than exporting to excel and reimporting cleaned file since it would result in same thing as carry forward)
incimpute2<-incimpute %>%
  mutate(
    unadjincome.1990 = eqnetfamincome.1990,
    unadjincome.1991 = ifelse(is.na(eqnetfamincome.1991), eqnetfamincome.1990, eqnetfamincome.1991),
    unadjincome.1992 = ifelse(is.na(eqnetfamincome.1992), unadjincome.1991, eqnetfamincome.1992),
    unadjincome.1993 = ifelse(is.na(eqnetfamincome.1993), unadjincome.1992, eqnetfamincome.1993),
    unadjincome.1994 = ifelse(is.na(eqnetfamincome.1994), unadjincome.1993, eqnetfamincome.1994),
    unadjincome.1996 = ifelse(is.na(eqnetfamincome.1996), unadjincome.1994, eqnetfamincome.1996),
    unadjincome.1998 = ifelse(is.na(eqnetfamincome.1998), unadjincome.1996, eqnetfamincome.1998),
    unadjincome.2000 = ifelse(is.na(eqnetfamincome.2000), unadjincome.1998, eqnetfamincome.2000),
    unadjincome.2002 = ifelse(is.na(eqnetfamincome.2002), unadjincome.2000, eqnetfamincome.2002),
    unadjincome.2004 = ifelse(is.na(eqnetfamincome.2004), unadjincome.2002, eqnetfamincome.2004),
    unadjincome.2006 = ifelse(is.na(eqnetfamincome.2006), unadjincome.2004, eqnetfamincome.2006),
    unadjincome.2008 = ifelse(is.na(eqnetfamincome.2008), unadjincome.2006, eqnetfamincome.2008),
    unadjincome.2010 = ifelse(is.na(eqnetfamincome.2010), unadjincome.2008, eqnetfamincome.2010)
  )
    summary(incimpute$eqnetfamincome.1992)
    summary(incimpute2$unadjincome.1992)
    summary(incimpute$eqnetfamincome.2010)
    summary(incimpute2$unadjincome.2010)
  
#Step 2: create if income drop at each follow-up waves
#definition: a *decrease* of ≥25% in income compared with the income at the previous study visit
#and less than the participant’s average income from all study visits

#again, need to do x+1 conversion for all unadjusted income variables or else have issues in calculations when zero is denominator (i.e., when no change between time points and income is zero?)
unadjincs<-grep("unadjinc", names(incimpute2), value = TRUE)
incimpute2[,unadjincs] <- lapply(incimpute2[,unadjincs], function(x) x=x+0.0001)
  
incimpute2$incdrop.9190 <- ifelse(((incimpute2$unadjincome.1991-incimpute2$unadjincome.1990)/
                                (0.5*(incimpute2$unadjincome.1991+incimpute2$unadjincome.1990)) <= -0.25) & 
                               incimpute2$unadjincome.1991 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.9190, exclude = NULL)
  head(incimpute2[, c("unadjincome.1991","unadjincome.1990","incdrop.9190")],100)

incimpute2$incdrop.9291 <- ifelse(((incimpute2$unadjincome.1992-incimpute2$unadjincome.1991)/
                                     (0.5*(incimpute2$unadjincome.1992+incimpute2$unadjincome.1991)) <= -0.25) & 
                                    incimpute2$unadjincome.1992 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.9291, exclude = NULL)
  head(incimpute2[, c("unadjincome.1992","unadjincome.1991","incdrop.9291")],100)
  
incimpute2$incdrop.9392 <- ifelse(((incimpute2$unadjincome.1993-incimpute2$unadjincome.1992)/
                                       (0.5*(incimpute2$unadjincome.1993+incimpute2$unadjincome.1992)) <= -0.25) & 
                                      incimpute2$unadjincome.1993 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.9392, exclude = NULL)
  head(incimpute2[, c("unadjincome.1993","unadjincome.1992","incdrop.9392")],100)
  
incimpute2$incdrop.9493 <- ifelse(((incimpute2$unadjincome.1994-incimpute2$unadjincome.1993)/
                                     (0.5*(incimpute2$unadjincome.1994+incimpute2$unadjincome.1993)) <= -0.25) & 
                                    incimpute2$unadjincome.1994 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.9493, exclude = NULL)
  head(incimpute2[, c("unadjincome.1994","unadjincome.1993","incdrop.9493")],100)

incimpute2$incdrop.9694 <- ifelse(((incimpute2$unadjincome.1996-incimpute2$unadjincome.1994)/
                                     (0.5*(incimpute2$unadjincome.1996+incimpute2$unadjincome.1994)) <= -0.25) & 
                                    incimpute2$unadjincome.1996 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.9694, exclude = NULL)
  head(incimpute2[, c("unadjincome.1996","unadjincome.1994","incdrop.9694")],100)

incimpute2$incdrop.9896 <- ifelse(((incimpute2$unadjincome.1998-incimpute2$unadjincome.1996)/
                                     (0.5*(incimpute2$unadjincome.1998+incimpute2$unadjincome.1996)) <= -0.25) & 
                                    incimpute2$unadjincome.1998 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.9896, exclude = NULL)
  head(incimpute2[, c("unadjincome.1998","unadjincome.1996","incdrop.9896")],100)

incimpute2$incdrop.0098 <- ifelse(((incimpute2$unadjincome.2000-incimpute2$unadjincome.1998)/
                                     (0.5*(incimpute2$unadjincome.2000+incimpute2$unadjincome.1998)) <= -0.25) & 
                                    incimpute2$unadjincome.2000 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.0098, exclude = NULL)
  head(incimpute2[, c("unadjincome.2000","unadjincome.1998","incdrop.0098")],100)

incimpute2$incdrop.0200 <- ifelse(((incimpute2$unadjincome.2002-incimpute2$unadjincome.2000)/
                                       (0.5*(incimpute2$unadjincome.2002+incimpute2$unadjincome.2000)) <= -0.25) & 
                                      incimpute2$unadjincome.2002 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.0200, exclude = NULL)
  head(incimpute2[, c("unadjincome.2002","unadjincome.2000","incdrop.0200")],100)
  
incimpute2$incdrop.0402 <- ifelse(((incimpute2$unadjincome.2004-incimpute2$unadjincome.2002)/
                                       (0.5*(incimpute2$unadjincome.2004+incimpute2$unadjincome.2002)) <= -0.25) & 
                                      incimpute2$unadjincome.2004 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.0402, exclude = NULL)
  head(incimpute2[, c("unadjincome.2004","unadjincome.2002","incdrop.0402")],100)
  
incimpute2$incdrop.0604 <- ifelse(((incimpute2$unadjincome.2006-incimpute2$unadjincome.2004)/
                                       (0.5*(incimpute2$unadjincome.2006+incimpute2$unadjincome.2004)) <= -0.25) & 
                                      incimpute2$unadjincome.2006 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.0604, exclude = NULL)
  head(incimpute2[, c("unadjincome.2006","unadjincome.2004","incdrop.0604")],100)
  
incimpute2$incdrop.0806 <- ifelse(((incimpute2$unadjincome.2008-incimpute2$unadjincome.2006)/
                                       (0.5*(incimpute2$unadjincome.2008+incimpute2$unadjincome.2006)) <= -0.25) & 
                                      incimpute2$unadjincome.2008 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.0806, exclude = NULL)
  head(incimpute2[, c("unadjincome.2008","unadjincome.2006","incdrop.0806")],100)
  
incimpute2$incdrop.1008 <- ifelse(((incimpute2$unadjincome.2010-incimpute2$unadjincome.2008)/
                                       (0.5*(incimpute2$unadjincome.2010+incimpute2$unadjincome.2008)) <= -0.25) & 
                                      incimpute2$unadjincome.2010 < incimpute2$mean_inc, 1, 0)
  table(incimpute2$incdrop.1008, exclude = NULL)
  head(incimpute2[, c("unadjincome.2010","unadjincome.2008","incdrop.1008")],100)
  
#Step 3: count the number of income drop
colnames(incimpute2)
incdrops<-grep("incdrop", names(incimpute2), value = TRUE)

incimpute2$num.drops <- rowSums(incimpute2[, incdrops], na.rm = T)
  table(incimpute2$num.drops, exclude = NULL)

#categorize it: 0, 1, and >=2
incimpute2$num.drops3 <- ifelse(incimpute2$num.drops==0, "0",
                             ifelse(incimpute2$num.drops==1, "1",
                                    ifelse(incimpute2$num.drops>=2, ">=2", NA)))
incimpute2$num.drops3 <- factor(incimpute2$num.drops3, levels = c("0", "1",  ">=2"))
  table(incimpute2$num.drops3)

nlsyincvol<-incimpute2 #n=7257

#write.csv(nlsyincvol,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/nlsy.1990.3inc_sample.csv")
#save(nlsyincvol, file = "/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/nlsy.1990.3inc_sample.rda")

#----------------------------------------------------------------------------------------------------------------------------------
##(9). Restrict to people with non-missing cognitive outcome on all tests
nlsyincvol$srm10<-ifelse(nlsyincvol$selfratemem>=0,1,0)
  nlsyincvol$srm10<-ifelse(is.na(nlsyincvol$srm10),0,nlsyincvol$srm10)
  table(nlsyincvol$srm10, exclude=NULL)
nlsyincvol$iwr10<-ifelse(nlsyincvol$immedrecall>=0,1,0)
  nlsyincvol$iwr10<-ifelse(is.na(nlsyincvol$iwr10),0,nlsyincvol$iwr10)
  table(nlsyincvol$iwr10, exclude=NULL)
nlsyincvol$dwr10<-ifelse(nlsyincvol$delrecall>=0,1,0)
  nlsyincvol$dwr10<-ifelse(is.na(nlsyincvol$dwr10),0,nlsyincvol$dwr10)
  table(nlsyincvol$dwr10, exclude=NULL)
nlsyincvol$bc10<-ifelse(nlsyincvol$bc20>=0,1,0)
  nlsyincvol$bc10<-ifelse(is.na(nlsyincvol$bc10),0,nlsyincvol$bc10)
  table(nlsyincvol$bc10, exclude=NULL)
nlsyincvol$bc8610<-ifelse(nlsyincvol$bc86>=0,1,0)
  nlsyincvol$bc8610<-ifelse(is.na(nlsyincvol$bc8610),0,nlsyincvol$bc8610)
  table(nlsyincvol$bc8610, exclude=NULL)
nlsyincvol$s710<-ifelse(nlsyincvol$serial7s>=0,1,0)
  nlsyincvol$s710<-ifelse(is.na(nlsyincvol$s710),0,nlsyincvol$s710)
  table(nlsyincvol$s710, exclude=NULL)

#restrict to people with 2010+ cognitive outcomes
nlsyincvolcog10<-nlsyincvol[!is.na(nlsyincvol$CogTestYear)&nlsyincvol$CogTestYear>=2010,] 
  #3438
nlsyincvolcog<-nlsyincvolcog10[!is.na(nlsyincvolcog10$TICSm.score),] #3273 have all cognition variables
  #n=3273

#move ahead with the complete case (individuals have ALL cognitive assessments) because case about global cognition score for main analysis
 
#----------------------------------------------------------------------------------------------------------------------------------
##(10). Clean up analytic variables for descriptive tables and analyses

#(a). cut num.drops at 3 because so many more time points, can do it more fine-grained maybe?
nlsyincvolcog$num.drops4<-ifelse(nlsyincvolcog$num.drops>=3,4,nlsyincvolcog$num.drops3)
  prop.table(table(nlsyincvolcog$num.drops4))

#(b). descriptive table 1 for crude associations between covariates and exposure

#collapse and relevel variables for tables and analysis
summary(nlsyincvolcog$age.1990)
table(nlsyincvolcog$NLSY.raceth)
covariates <- nlsyincvolcog %>%
  mutate(
    female = sex,
    sex = factor(sex, levels = c(1,2), labels = c("Male", "Female")), 
    female = ifelse(sex=="Male",0,1),
    female = factor(female, levels = c(0,1), labels = c("Male", "Female")),
    raceth = factor(NLSY.raceth, levels = c(3,2,1), labels = c("Non-Hispanic, non-Black","Black","Hispanic")),
    married = case_when(maristat.1990 == 0 ~ "Never married",
                        maristat.1990 ==1 ~ "Married",
                        maristat.1990 %in% c(2,3,6) ~ "Separated/divorced/widowed"),
    married = factor(married, levels = c("Married","Never married","Separated/divorced/widowed")),
    HSeduc = factor(educat89, levels = c(1,0), labels = c(">12 years", "<=12 years")),
    everhbp = factor(everhbp, levels = c(0,1), labels = c("No", "Yes")),
    everhichol = factor(everhichol, levels = c(0,1), labels = c("No", "Yes")),
    everdiab = factor(everdiab, levels = c(0,1), labels = c("No", "Yes")),
    everhibp = factor(everhbp, levels = c(0,1), labels = c("No", "Yes")),
    bpmeds2010 = factor(bpmeds2010, levels = c(0,1), labels = c("Not currently taking", "Currently taking")),
    smokstat92 = factor(smokestat.1992, levels = c("never","former","current"), labels = c("Never","Former","Current")),
    depressed92 = factor(depression.cesd20.92, levels = c(0,1), labels = c("CESD-20 score <16", "CESD-20 score >=16")),
    vigPA.2010 = factor(vigPA.2010, levels = c(2,1,3,4,5), labels = c(">Once per week", "Everyday/>once per day", "Once per week", "1-3 times per month", "Unable to do")),
    lightPA.2010 = factor(lightPA.2010, levels = c(2,1,3,4,5), labels = c(">Once per week", "Everyday/>once per day", "Once per week", "1-3 times per month", "Unable to do")),
    empstat = factor(lbrf.90, levels = c("Work FT", "Work PT","Unemployed","Not in LbrF","Disabled"), labels=c("Works full-time", "Works part-time","Unemployed","Not in labor force","Disabled")),                
    ephealthins = factor(epinsurance90, levels = c(0,1,2), labels = c("None","Other","Employer provided")),
    heavydrink = factor(heavydrink, levels = c(1,0,2), labels = c("Light/Moderate","Never","Heavy")),
    num.drops4 = factor(num.drops4, levels = c(1,2,3,4), labels = c("No drops", "1 drop", "2 drops", "3+ drops")),
    #num.drops5 = factor(num.drops5, levels = c(1,2,3,4,5), labels = c("No drops", "1 drop", "2 drops", "3 drops", "4+ drops")),
    empstat2 = case_when(lbrf.90 == "Work FT" ~ "Works full-time",
                         lbrf.90 %in% c("Work PT","Unemployed","Not in LbrF","Disabled") ~ "Does not work full-time")
  )

table(covariates$empstat2)
table(covariates$num.drops4)
#nlsy.sex: 1=male, 2=female
#nlsy.race: 1=Hispanic, 2=Black, 3=non-Hispanic, non-Black
#marital: 0=never married, 1=married, 2=separated, 3=divorced, 5=remarried, 6=widowed 

#----------------------------------------------------------------------------------------------------------------------------------
##(11). Restrict to people with non-missing covariate data for complete case analysis

#10282023 update: remove 2010 variables from model 2; they do not make a difference if in or out and they mess up temporality (and we should get back sample size!)
#model 1: CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
#model 2: CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
#model 3: CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

prop.table(table(covariates$num.drops3)) #see if distribution of exposure changes with missing data restrictions
#0         1       >=2 
#0.2141766 0.3415826 0.4442408

summary(covariates$immedrecall) #see if distribution of outcome changes with missing data restrictions
summary(covariates$delrecall) #see if distribution of outcome changes with missing data restrictions

#--------------
#i.restrict to non-missing model 1 vars:
nlsy.m1<- covariates %>%
  filter(if_all(c("CogTestYear","AFQTpctlscore06rev.81.r","sumincomes","age.1990","sex","raceth","HSeduc","married"),  ~ !is.na(.)))
#n=3137

summary(nlsy.m1$sd_pct)
prop.table(table(nlsy.m1$num.drops3)) #see if distribution of exposure changes with missing data restrictions
#0         1       >=2 
#0.2135799 0.3414090 0.4450112

summary(nlsy.m1$immedrecall) #see if distribution of outcome changes with missing data restrictions
summary(nlsy.m1$delrecall) #see if distribution of outcome changes with missing data restrictions

#--------------
#ii.restrict to non-missing model 2 vars - see if there are any that don't really matter so can gain back some sample size
summary(nlsy.m1$heavydrink) #304 missing alcohol
table(nlsy.m1$smokestat.1992,exclude=NULL) #287 missing smoking status in 1992

nlsy.m2<-nlsy.m1 %>%
  filter(if_all(c("BMI1990.ht1985","smokestat.1992","depressed92","ephealthins","heavydrink"),  ~ !is.na(.)))
#n=2643

prop.table(table(nlsy.m2$num.drops3)) #see if distribution of exposure changes with missing data restrictions
#0         1       >=2 
#0.2183125 0.3522512 0.4294362

summary(nlsy.m2$immedrecall) #see if distribution of outcome changes with missing data restrictions
summary(nlsy.m2$delrecall) #see if distribution of outcome changes with missing data restrictions

#--------------
#iii.restrict to non-missing model 3 vars
summary(nlsy.m2$wealth90.1990) #561 additionally missing wealth....too much of a reduction?
table(nlsy.m2$lbrf.90,exclude=NULL)

nlsy.m3<-nlsy.m2 %>%
  filter(if_all(c("eqincome90.1990","empstat2"),  ~ !is.na(.)))
#n=2643

prop.table(table(nlsy.m3$num.drops3)) #see if distribution of exposure changes with missing data restrictions
#0         1       >=2 
#0.2183125 0.3522512 0.4294362 

summary(nlsy.m3$immedrecall) #see if distribution of outcome changes with missing data restrictions
summary(nlsy.m3$delrecall) #see if distribution of outcome changes with missing data restrictions

#----------------------------------------------------------------------------------------------------------------------------------
##(12). Descriptive statistics

#(a).crude associations between exposure and outcomes

#exposures
summary(nlsy.m3$sd_pct) 
  table(nlsy.m3$num.drops)
  prop.table(table(nlsy.m3$num.drops3))
  prop.table(table(nlsy.m3$num.drops4))

#are crude analyses in direction of hypothesis that more volatility --> lower scores on cog exams
summary(nlsy.m3$immedrecall) 
aggregate(nlsy.m3$immedrecall, by=list(nlsy.m3$num.drops4), FUN=mean, na.rm=TRUE)
  #yes, for immediate recall
summary(nlsy.m3$delrecall)   
aggregate(nlsy.m3$delrecall, by=list(nlsy.m3$num.drops4), FUN=mean, na.rm=TRUE)
  #yes, for delayed recall
summary(nlsy.m3$bc20)  
aggregate(nlsy.m3$bc20, by=list(nlsy.m3$num.drops4), FUN=mean, na.rm=TRUE)
  #yes, for backward counting
summary(nlsy.m3$bc86) 
aggregate(nlsy.m3$bc86, by=list(nlsy.m3$num.drops4), FUN=mean, na.rm=TRUE)
  #yes, for backward counting from 86
summary(nlsy.m3$serial7s)  
aggregate(nlsy.m3$serial7s, by=list(nlsy.m3$num.drops4), FUN=mean, na.rm=TRUE)
  #yes, for serial 7s

#Create a Descriptive Table 1
label(nlsy.m3$age.1990)  <- "Age (years)"
label(nlsy.m3$sex) <- "Sex"
label(nlsy.m3$raceth) <- "Race/ethnicity"
label(nlsy.m3$HSeduc) <- "Years of education"
label(nlsy.m3$married) <- "Marital status"
label(nlsy.m3$hhsize.1990) <- "Household size"
label(nlsy.m3$BMI1990.ht1985) <- "Body mass index"
label(nlsy.m3$everhbp) <- "Doctor ever diagnosed with high blood pressure"
label(nlsy.m3$bpmeds2010) <- "Currently taking blood pressure medication (2010)"
label(nlsy.m3$everhichol) <- "Doctor ever diagnosed with high cholesterol"
label(nlsy.m3$everdiab) <- "Doctor ever diagnosed with diabetes"
label(nlsy.m3$smokstat92) <- "Smoking status (1992)"
label(nlsy.m3$depressed92) <- "CESD-20 score (1992)"
label(nlsy.m3$vigPA.2010) <- "Participates in vigorous exercise"
label(nlsy.m3$lightPA.2010) <- "Participates in light/moderate exercise"
label(nlsy.m3$income90.1990) <- "Net family income"
label(nlsy.m3$empstat) <- "Employment status"
label(nlsy.m3$ephealthins) <- "Health insurance type"
label(nlsy.m3$wealth90.1990) <- "Family net wealth (dollars)"
label(nlsy.m3$heavydrink) <- "Drinking status"
label(nlsy.m3$AFQTpctlscore06rev.81) <- "Baseline cognition (1980)"
label(nlsy.m3$empstat2) <- "Employment status (collapsed)"

#https://www.rdocumentation.org/packages/table1/versions/1.4.3
table1.invol<-table1(~ sd_pct + AFQTpctlscore06rev.81.r + age.1990 + as.factor(sex) + as.factor(raceth) + as.factor(HSeduc) + as.factor(married) + hhsize.1990 +
                       BMI1990.ht1985 + as.factor(smokstat92) + as.factor(depressed92) + as.factor(ephealthins) + as.factor(heavydrink) + 
                       income90.1990 + as.factor(empstat) + as.factor(empstat2) + wealth90.1990 | num.drops4, data=nlsy.m3,
                       overall=c(left="Total")) #, caption=caption, footnote=footnote)

#report non-equivalized in table 1 just for descriptive purposes (because already report household size in table 1)
  #summary(nlsy.m3$eqincome90.1990*sqrt(2)) #to report in manuscript text comparing to census?
  summary(nlsy.m3$income90.1990)
  sd(nlsy.m3$income90.1990)
    aggregate(nlsy.m3$income90.1990, by=list(nlsy.m3$num.drops4), FUN=summary)
    aggregate(nlsy.m3$income90.1990, by=list(nlsy.m3$num.drops4), FUN=sd)
  summary(nlsy.m3$wealth90.1990)
  sd(nlsy.m3$wealth90.1990, na.rm=TRUE)
    aggregate(nlsy.m3$wealth90.1990, by=list(nlsy.m3$num.drops4), FUN=summary)
    aggregate(nlsy.m3$wealth90.1990, by=list(nlsy.m3$num.drops4), FUN=sd, na.rm=TRUE)

#----------------------------------------------------------------------------------------------------------------------------------
##(13). Create analytic datasets for main analyses - create outcomes
maindata<-nlsy.m3

maindata$memscore.raw<-rowSums(maindata[,c("immedrecall","delrecall")])
maindata$attnscore.raw<-rowSums(maindata[,c("bc20","bc86","serial7s")])
#outcome - raw domain and overall scores
  summary(maindata$memscore.raw)  
  summary(maindata$attnscore.raw)
  summary(maindata$TICSm.score)

#within complete case dataset create z-scored cog variables (as CD suggested)
#immediate recall z score
maindata$IR.z<-(maindata$immedrecall-mean(maindata$immedrecall))/sd(maindata$immedrecall)
  summary(maindata$IR.z)
  sd(maindata$IR.z)

#delayed recall z score
maindata$DR.z<-(maindata$delrecall-mean(maindata$delrecall))/sd(maindata$delrecall)
  sd(maindata$DR.z)
  summary(maindata$DR.z)
  
#memory domain score z score
maindata$memscore.z<-(maindata$immedrecall+maindata$delrecall)/2
maindata$memscore.z<-(maindata$memscore.z-mean(maindata$memscore.z))/sd(maindata$memscore.z)
  summary(maindata$memscore.z)
  sd(maindata$memscore.z)

#backward counting z score
  #1) first avg the bc20 and bc86 measures (done in cognition cleaning step above, when cognition data first imported)
  #then z-score that variable
maindata$BC20.z<-(maindata$bc20-mean(maindata$bc20))/sd(maindata$bc20)
maindata$BC86.z<-(maindata$bc86-mean(maindata$bc86))/sd(maindata$bc86)
maindata$BC.z<-(maindata$avgBC.r-mean(maindata$avgBC.r))/sd(maindata$avgBC.r)
sd(maindata$BC.z)
  summary(maindata$BC.z)
  hist(maindata$BC.z)

#test<-(maindata$BC20.z+maindata$BC86.z)/2
#test2<-(test-mean(test))/sd(test)

  #CD says use average of BCs then standardize when looking at BC as outcome 
  #but in attention score, CD says z-score each of the 3 components (bc20, bc86, serial7s), average, then z-score again
    #note: really doesn't make a huge difference which way (zscore avg BC and serial 7s, average, then zscore or above) but will do CD way for consistency

#serial 7s z score
maindata$s7.z<-(maindata$serial7s-mean(maindata$serial7s))/sd(maindata$serial7s)
  hist(maindata$s7.z)
  summary(maindata$s7.z)  
  sd(maindata$s7.z)  

#attention domain score z score
    #take average of averaged BC and serial 7 z scores
    #then z score that quantity
maindata$attnscore.z.CD<-rowMeans(maindata[,c("BC20.z","BC86.z","s7.z")], na.rm=TRUE)
maindata$attnscore.z<-(maindata$BC.z+maindata$s7.z)/2
maindata$attnscore.z.CD<-(maindata$attnscore.z.CD-mean(maindata$attnscore.z.CD))/sd(maindata$attnscore.z.CD)
maindata$attnscore.z<-(maindata$attnscore.z-mean(maindata$attnscore.z))/sd(maindata$attnscore.z)
  sd(maindata$attnscore.z)
  summary(maindata$attnscore.z.CD) 
    #as above, not super different, but will use CD's approach for consistency
      sum(table(maindata$attnscore.z))
      sum(table(maindata$attnscore.z.CD))
        #N is same using either approach
  
#use CD's approach for attnscore.z variable
maindata$attnscore.z<-maindata$attnscore.z.CD
  summary(maindata$attnscore.z)

#overall score, z score  
maindata$TICSm.zscore<-(maindata$memscore.z+maindata$attnscore.z)/2
maindata$TICSm.zscore<-(maindata$TICSm.zscore-mean(maindata$TICSm.zscore))/sd(maindata$TICSm.zscore)
    summary(maindata$TICSm.zscore)
    mean(maindata$TICSm.zscore)
    sd(maindata$TICSm.zscore)
    hist(maindata$TICSm.zscore) #looks pretty okay!
    hist(maindata$TICSm.score) #looks good!
    
#Distribution of main outcomes
hist(maindata$TICSm.zscore) #looks pretty okay!
hist(maindata$TICSm.score) #looks good!
hist(maindata$memscore.raw)
hist(maindata$memscore.z)
hist(maindata$attnscore.raw)
hist(maindata$attnscore.z) 

#categorical outcomes for dementia and CIND based on TICs-m
maindata$cogimpair3<-ifelse(maindata$TICSm.score>=7&maindata$TICSm.score<=11,1,
                       ifelse(maindata$TICSm.score<7,2,0))
maindata$cogimpair2<-ifelse(maindata$TICSm.score<=11,1,0)
                            
table(maindata$cogimpair2) #makes sense it would be low/rare outcome 
table(maindata$cogimpair3) #makes sense dementia would be really low here


#outcome distrbutions for manuscript
summary(maindata$memscore.raw)
summary(maindata$attnscore.raw)  
summary(maindata$TICSm.score)

summary(maindata$memscore.z)
summary(maindata$attnscore.z)  
summary(maindata$TICSm.zscore)

#----------------------------------------------------------------------------------------------------------------------------------
##(14). Main analyses

#how many people contributed N number of time points of income information in each trajectory
prop.table(table(maindata$sumincomes))*100

summary(maindata$age.1990) #age range at the start is 25-33 - med=27
summary(maindata$age.1990+10) #age range at mid-follow up is 35-43 - med=37
summary(maindata$age.1990+20) #age range at end follow-up is 45-53 - med=47
  #cover more of period of peak earning years AND coincide with recession

table(maindata$age.1990)

aggregate(maindata$TICSm.score, by=list(maindata$age.1990), FUN="mean")
aggregate(maindata$income90.1990, by=list(maindata$age.1990), FUN="mean")

#(a).TICs global cog score (raw)

hist(maindata$TICSm.score) #pretty normal - bootstrapping feels not necessary here...
hist(maindata$TICSm.zscore) #pretty normal - bootstrapping feels not necessary here...

#####
#use the SD of sd_pct in the final analytic sample:
  #even though did this above, probably better to have SD of income vol in the analytic sample
maindata$sd_incvol<-maindata$sd_pct/sd(maindata$sd_pct)
maindata$sd_incvol.i<-maindata$sd_pct.i/sd(maindata$sd_pct.i)

summary(maindata$sd_incvol)
summary(maindata$sd_incvol.i)
table(maindata$num.drops)
table(maindata$num.drops4, maindata$num.drops)

#save main data file so can import in additional syntax files for sensitivity analyses
  #write.csv(maindata,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.csv")
  #save(maindata, file = "/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.rda")
  #write.csv(maindata,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.csv")
  #save(maindata, file = "/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.rda")

#create empty dataframes to store results
incvoltable<-as.data.frame(matrix(nrow=30,ncol=5))
incvoltable[,1]<-c("rawtotscore_M1","rawtotscore_M2","rawtotscore_M3",
                   "ztotscore_M1","ztotscore_M2","ztotscore_M3",
                   "rawmemscore_M1","rawmemscore_M2","rawmemscore_M3",
                   "zmemscore_M1","zmemscore_M2","zmemscore_M3",
                   "rawattnscore_M1","rawattnscore_M2","rawattnscore_M3",
                   "zattnscore_M1","zattnscore_M2","zattnscore_M3",
                   "zimmscore_M1","zimmscore_M2","zimmscore_M3",
                   "zdelscore_M1","zdelscore_M2","zdelscore_M3",
                   "zbcscore_M1","zbcscore_M2","zbcscore_M3",
                   "zs7score_M1","zs7score_M2","zs7score_M3")
names(incvoltable)<-c("model","beta","lci","uci","pval")                   

incdroptable<-as.data.frame(matrix(nrow=90,ncol=5))
names(incdroptable)<-c("model","beta","lci","uci","pval")                   
incdroptable[,1]<-c("rawtotscore_M1_1drop","rawtotscore_M1_2drop","rawtotscore_M1_3drop",
                    "rawtotscore_M2_1drop","rawtotscore_M2_2drop","rawtotscore_M2_3drop",
                    "rawtotscore_M3_1drop,","rawtotscore_M3_2drop","rawtotscore_M3_3drop",
                    "ztotscore_M1_1drop","ztotscore_M1_2drop","ztotscore_M1_3drop",
                    "ztotscore_M2_1drop","ztotscore_M2_2drop","ztotscore_M2_3drop",
                    "ztotscore_M3_1drop,","ztotscore_M3_2drop","ztotscore_M3_3drop",
                    "rawmemscore_M1_1drop","rawmemscore_M1_2drop","rawmemscore_M1_3drop",
                    "rawmemscore_M2_1drop","rawmemscore_M2_2drop","rawmemscore_M2_3drop",
                    "rawmemscore_M3_1drop,","rawmemscore_M3_2drop","rawmemscore_M3_3drop",
                    "zmemscore_M1_1drop","zmemscore_M1_2drop","zmemscore_M1_3drop",
                    "zmemscore_M2_1drop","zmemscore_M2_2drop","zmemscore_M2_3drop",
                    "zmemscore_M3_1drop,","zmemscore_M3_2drop","zmemscore_M3_3drop",
                    "rawattnscore_M1_1drop","rawattnscore_M1_2drop","rawattnscore_M1_3drop",
                    "rawattnscore_M2_1drop","rawattnscore_M2_2drop","rawattnscore_M2_3drop",
                    "rawattnscore_M3_1drop,","rawattnscore_M3_2drop","rawattnscore_M3_3drop",
                    "zattnscore_M1_1drop","zattnscore_M1_2drop","zattnscore_M1_3drop",
                    "zattnscore_M2_1drop","zattnscore_M2_2drop","zattnscore_M2_3drop",
                    "zattnscore_M3_1drop,","zattnscore_M3_2drop","zattnscore_M3_3drop",                   
                    "zimmscore_M1_1drop","zimmscore_M1_2drop","zimmscore_M1_3drop",
                    "zimmscore_M2_1drop","zimmscore_M2_2drop","zimmscore_M2_3drop",
                    "zimmscore_M3_1drop,","zimmscore_M3_2drop","zimmscore_M3_3drop",
                    "zdelscore_M1_1drop","zdelscore_M1_2drop","zdelscore_M1_3drop",
                    "zdelscore_M2_1drop","zdelscore_M2_2drop","zdelscore_M2_3drop",
                    "zdelscore_M3_1drop,","zdelscore_M3_2drop","zdelscore_M3_3drop",
                    "zbcscore_M1_1drop","zbcscore_M1_2drop","zbcscore_M1_3drop",
                    "zbcscore_M2_1drop","zbcscore_M2_2drop","zbcscore_M2_3drop",
                    "zbcscore_M3_1drop,","zbcscore_M3_2drop","zbcscore_M3_3drop",
                    "zs7score_M1_1drop","zs7score_M1_2drop","zs7score_M1_3drop",
                    "zs7score_M2_1drop","zs7score_M2_2drop","zs7score_M2_3drop",
                    "zs7score_M3_1drop,","zs7score_M3_2drop","zs7score_M3_3drop")

#run analyses and store results                   
#income volatility (SD of pct change over time)
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(TICs.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[1,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[1,5]<-summary(model1.vol)$coefficients[2,4]
  
    #set.seed(12345)
    #lboot<-lm.boot(lm(TICs.m1, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
    #perc.lm(lboot,c(0.025, 0.975))[c(3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
    
model2.vol<-lm(TICs.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[2,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[2,5]<-summary(model2.vol)$coefficients[2,4]
  
model3.vol<-lm(TICs.m3, data=maindata)
summary(model3.vol)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[3,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[3,5]<-summary(model3.vol)$coefficients[2,4]

#####
#income drops
TICs.id.m1<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.id.m2<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.id.m3<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

table(maindata$empstat2)  

model1.drops<-lm(TICs.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[1:3,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[1:3,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[1:3,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(TICs.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[4:6,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[4:6,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[4:6,5]<-summary(model2.drops)$coefficients[c(2:4),4]
  
model3.drops<-lm(TICs.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
  summary(model3.drops)
    incdroptable[7:9,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[7:9,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[7:9,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------
#-----------------
#(b). TICs-m global score (standardized)
#####
zTICs.m1<-TICSm.zscore ~ CogTestYear + AFQTpctlscore06rev.81.r
  model1.vol<-lm(zTICs.m1,data=maindata)
  summary(model1.vol)

  aggregate(maindata$TICSm.zscore, by=list(maindata$age.1990), FUN="summary")
  aggregate(maindata$income90.1990, by=list(maindata$age.1990), FUN="mean")
  
#income volatility (SD of pct change over time)
zTICs.m1<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.m2<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.m3<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(zTICs.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[4,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[4,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(zTICs.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[5,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[5,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(zTICs.m3, data=maindata)
  summary(model3.vol)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[6,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[6,5]<-summary(model3.vol)$coefficients[2,4]

    #without adjustment for earlier-life cog
    zTICs.m3s<-TICSm.zscore ~ sd_incvol + CogTestYear + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2
    model3s.vol<-lm(zTICs.m3s, data=maindata)
    round(c(coefficients(model3s.vol)["sd_incvol"],confint(model3s.vol)["sd_incvol",]),4)
    summary(model3.vol)$coefficients[2,4]

    #model 3 changing units for income and income vol -just to compare (for internal purposes)
    maindata$sd_incvol_r<-maindata$sd_pct/10
    maindata$inc.p1000<-maindata$eqincome90.1990/10000
    zTICs.m3r<-TICSm.zscore ~ sd_pct + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2
    zTICs.m3r<-TICSm.zscore ~ sd_incvol_r + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + inc.p1000 + empstat2
    model3.vol.r<-lm(zTICs.m3r, data=maindata)
    summary(model3.vol.r)
    round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[6,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
    summary(model3.vol)$coefficients[2,4]
    incvoltable[6,5]<-summary(model3.vol)$coefficients[2,4]
    

#####
#income drops
zTICs.id.m1<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.id.m2<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.id.m3<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2
  
model1.drops<-lm(zTICs.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[10:12,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[10:12,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[10:12,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zTICs.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[13:15,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[13:15,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[13:15,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zTICs.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[16:18,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[16:18,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[16:18,5]<-summary(model3.drops)$coefficients[c(2:4),4]

    #without adjustment for baseline income
    zTICs.id.m3s<-TICSm.zscore ~ num.drops4 + CogTestYear + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2
    model3s.drops<-lm(zTICs.id.m3s, data=maindata)
    round(coefficients(model3s.drops)[2:4],3)
    
#-----------------  
#(c).memory domain score (raw)

#####
#income volatility (SD of pct change over time)
hist(maindata$memscore.raw) #fairly normal - use model-based conf intervals (e.g., model 1: -0.01046 -0.00233)
hist(maindata$memscore.z) #fairly normal - use model-based conf intervals (e.g., model 1: -0.0106 -0.00209)

mem.m1<-memscore.raw ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
mem.m2<-memscore.raw ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
mem.m3<-memscore.raw ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(mem.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[7,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[7,5]<-summary(model1.vol)$coefficients[2,4]
  
model2.vol<-lm(mem.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[8,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[8,5]<-summary(model2.vol)$coefficients[2,4]
    
model3.vol<-lm(mem.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[9,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[9,5]<-summary(model3.vol)$coefficients[2,4]

#####
#income drops
mem.id.m1<-memscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
mem.id.m2<-memscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
mem.id.m3<-memscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(mem.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[19:21,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[19:21,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[19:21,5]<-summary(model1.drops)$coefficients[c(2:4),4]
  
model2.drops<-lm(mem.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[22:24,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[22:24,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[22:24,5]<-summary(model2.drops)$coefficients[c(2:4),4]
  
model3.drops<-lm(mem.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[25:27,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[25:27,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[25:27,5]<-summary(model3.drops)$coefficients[c(2:4),4]
  
#-----------------
#-----------------
#(d). Memory domain score (standardized)
  
#####
#income volatility (SD of pct change over time)
zmem.m1<-memscore.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zmem.m2<-memscore.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zmem.m3<-memscore.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(zmem.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[10,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[10,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(zmem.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[11,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[11,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(zmem.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[12,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[12,5]<-summary(model3.vol)$coefficients[2,4]  

#####
#income drops
zmem.id.m1<-memscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zmem.id.m2<-memscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zmem.id.m3<-memscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(zmem.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[28:30,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[28:30,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[28:30,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zmem.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[31:33,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[31:33,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[31:33,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zmem.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[34:36,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[34:36,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[34:36,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------  
#-----------------  
#(e). attention domain score (raw)
  
#####
#income volatility (SD of pct change over time)
attn.m1<-attnscore.raw ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
attn.m2<-attnscore.raw ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
attn.m3<-attnscore.raw ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

hist(maindata$attnscore.raw) #model-based ci and boostrapped are very similar (just continue with model-based)
hist(maindata$attnscore.z)

model1.vol<-lm(attn.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[13,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[13,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(attn.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[14,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[14,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(attn.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[15,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[15,5]<-summary(model3.vol)$coefficients[2,4]  

  #set.seed(12345)
  #lboot<-lm.boot(lm(attn.m3, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
  #perc.lm(lboot,c(0.025, 0.975))[c(3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
  
#here, even though outcomes less normally distributed, get same answers/conclusions across all models if use model-based vs. bootstrapped CIs - linear regression pretty robust
  # model-based: -0.0045  0.0003 ** keep using this (model-based)
  #bootstrapped: -0.0046  0.0003
    
#####
#income drops
attn.id.m1<-attnscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
attn.id.m2<-attnscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
attn.id.m3<-attnscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(attn.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[37:39,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[37:39,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[37:39,5]<-summary(model1.drops)$coefficients[c(2:4),4]
  
model2.drops<-lm(attn.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[40:42,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[40:42,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[40:42,5]<-summary(model2.drops)$coefficients[c(2:4),4]
  
model3.drops<-lm(attn.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[43:45,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[43:45,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[43:45,5]<-summary(model3.drops)$coefficients[c(2:4),4]
  
#-----------------
#-----------------
#(f). attention domain score (standardized)
  
#####
#income volatility (SD of pct change over time)
zattn.m1<-attnscore.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zattn.m2<-attnscore.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zattn.m3<-attnscore.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

hist(maindata$attnscore.z)

model1.vol<-lm(zattn.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[16,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[16,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(zattn.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[17,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[17,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(zattn.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[18,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[18,5]<-summary(model3.vol)$coefficients[2,4]  

#model 3,  model-based ci: -0.0027 -0.0002 ** use model-based then, since consistent
#model 3, bootstrapped ci: -0.00274 -0.00015

#####
#income drops
zattn.id.m1<-attnscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zattn.id.m2<-attnscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zattn.id.m3<-attnscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(zattn.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[46:48,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[46:48,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[46:48,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zattn.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[49:51,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[49:51,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[49:51,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zattn.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[52:54,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[52:54,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[52:54,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------  
#-----------------  
#(g). cognitive impairment categories (logistic regression models)

#create new results table for cog impairment
cogimpair<-as.data.frame(matrix(nrow=12,ncol=5))
names(cogimpair)<-c("model","or","lci","uci","pval")
cogimpair[,1]<-c("vol_m1","vol_m2","vol_m3","1drop_m1","2drop_m1","3drop_m1","1drop_m2","2drop_m2","3drop_m2","1drop_m3","2drop_m3","3drop_m3")
####
#SD of % change income over time
sdCIND.m1<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
sdCIND.m2<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
sdCIND.m3<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-glm(sdCIND.m1, family=binomial, data=maindata)
  round(exp(coefficients(model1.drops))[2],3)
    cogimpair[1,2]<-round(exp(coefficients(model1.drops))[2],3)
  round(exp(confint(model1.drops))[2,],3)
    cogimpair[1,3:4]<-round(exp(confint(model1.drops))[2,],3)
    cogimpair[1,5]<-summary(model1.drops)$coefficients[2,4]

model2.drops<-glm(sdCIND.m2, family=binomial, data=maindata)
  round(exp(coefficients(model2.drops))[2],3)
    cogimpair[2,2]<-round(exp(coefficients(model2.drops))[2],3)
  round(exp(confint(model2.drops))[2,],3)
    cogimpair[2,3:4]<-round(exp(confint(model2.drops))[2,],3)
    cogimpair[2,5]<-summary(model2.drops)$coefficients[2,4]
    
model3.drops<-glm(sdCIND.m3, family=binomial, data=maindata)
  round(exp(coefficients(model3.drops))[2],3)
    cogimpair[3,2]<-round(exp(coefficients(model3.drops))[2],3)
  round(exp(confint(model3.drops))[2,],3)
    cogimpair[3,3:4]<-round(exp(confint(model3.drops))[2,],3)
    cogimpair[3,5]<-summary(model3.drops)$coefficients[2,4]
    
####
#number of drops  
CIND.m1<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
CIND.m2<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
CIND.m3<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-glm(CIND.m1, family=binomial, data=maindata)
  round(exp(coefficients(model1.drops))[2:4],3)
    cogimpair[4:6,2]<-round(exp(coefficients(model1.drops))[2:4],3)
  round(exp(confint(model1.drops))[2:4,],3)
    cogimpair[4:6,3:4]<-round(exp(confint(model1.drops))[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    cogimpair[4:6,5]<-summary(model1.drops)$coefficients[c(2:4),4]
  
model2.drops<-glm(CIND.m2, family=binomial, data=maindata)
  round(exp(coefficients(model2.drops))[2:4],3)
    cogimpair[7:9,2]<-round(exp(coefficients(model2.drops))[2:4],3)
  round(exp(confint(model2.drops))[2:4,],3)
    cogimpair[7:9,3:4]<-round(exp(confint(model2.drops))[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    cogimpair[7:9,5]<-summary(model2.drops)$coefficients[c(2:4),4]
    
model3.drops<-glm(CIND.m3, family=binomial, data=maindata)
  round(exp(coefficients(model3.drops))[2:4],3)
    cogimpair[10:12,2]<-round(exp(coefficients(model3.drops))[2:4],3)
  round(exp(confint(model3.drops))[2:4,],3)
    cogimpair[10:12,3:4]<-round(exp(confint(model3.drops))[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    cogimpair[10:12,5]<-summary(model3.drops)$coefficients[c(2:4),4]
    
 
################################################################################################ 
################################################################################################  
################################################################################################
#individual assessments as outcomes (sensitivity analysis)  
#z-scores so they can be compared with each other 
  
#-----------------  
#(sens i).immediate recall - raw score
hist(maindata$immedrecall) #~normally distributed - linear regression
hist(maindata$sd_incvol) #do we want to transform at all? 

# IR.m1<-IR.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# IR.m2<-IR.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# IR.m3<-IR.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

IR.m1<-immedrecall ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
IR.m2<-immedrecall ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
IR.m3<-immedrecall ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(IR.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[19,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[19,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(IR.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[20,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[20,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(IR.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[21,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[21,5]<-summary(model3.vol)$coefficients[2,4]  

#####
#income drops
# IR.id.m1<-IR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# IR.id.m2<-IR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# IR.id.m3<-IR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

#income drops
IR.id.m1<-immedrecall ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
IR.id.m2<-immedrecall ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
IR.id.m3<-immedrecall ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(IR.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[55:57,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[55:57,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[55:57,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(IR.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[58:60,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[58:60,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[58:60,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(IR.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[61:63,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[61:63,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[61:63,5]<-summary(model3.drops)$coefficients[c(2:4),4]


#-----------------
#-----------------
#(sens ii).delayed recall
summary(maindata$DR.z) #~normally distributed - linear regression
hist(maindata$DR.z) #~normally distributed - linear regression

# DR.m1<-DR.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# DR.m2<-DR.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# DR.m3<-DR.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

DR.m1<-delrecall ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
DR.m2<-delrecall ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
DR.m3<-delrecall ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(DR.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[22,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[22,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(DR.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[23,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[23,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(DR.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[24,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[24,5]<-summary(model3.vol)$coefficients[2,4]  


#####
#income drops
# DR.id.m1<-DR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# DR.id.m2<-DR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# DR.id.m3<-DR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

#income drops
DR.id.m1<-delrecall ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
DR.id.m2<-delrecall ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
DR.id.m3<-delrecall ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(DR.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[64:66,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[64:66,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[64:66,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(DR.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[67:69,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[67:69,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[67:69,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(DR.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[70:72,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[70:72,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[70:72,5]<-summary(model3.drops)$coefficients[c(2:4),4]


#-------------
#-------------
#(sens iii). backward counting from 20
summary(maindata$BC.z) #~binary and getting test 'correct' is common 
hist(maindata$BC.z) #~binary and getting test 'correct' is common 

# bc.m1<-BC.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# bc.m2<-BC.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# bc.m3<-BC.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

bc.m1<-avgBC.r ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
bc.m2<-avgBC.r ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
bc.m3<-avgBC.r ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(bc.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[25,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[25,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(bc.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[26,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[26,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(bc.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[27,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[27,5]<-summary(model3.vol)$coefficients[2,4]  


#####
#income drops
    
# bc.id.m1<-BC.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# bc.id.m2<-BC.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# bc.id.m3<-BC.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

bc.id.m1<-avgBC.r ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
bc.id.m2<-avgBC.r ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
bc.id.m3<-avgBC.r ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2


model1.drops<-lm(bc.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[73:75,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[73:75,3:4]<-round(confint(model1.drops)[2:4,],3)
    incdroptable[73:75,5]<-summary(model1.drops)$coefficients[c(2:4),4]
    #set.seed(12345)
    #lboot<-lm.boot(lm(bc.id.m1, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
    #perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
    #incdroptable[73:75,3:4]<-perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)][c(1,3,5,2,4,6)] 

model2.drops<-lm(bc.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[76:78,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[76:78,3:4]<-round(confint(model2.drops)[2:4,],3)
    incdroptable[76:78,5]<-summary(model2.drops)$coefficients[c(2:4),4]
    # set.seed(12345)
  #   lboot<-lm.boot(lm(bc.id.m2, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
  #   perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
  #   incdroptable[76:78,3:4]<-perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)][c(1,3,5,2,4,6)] 
  
model3.drops<-lm(bc.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[79:81,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[79:81,3:4]<-round(confint(model3.drops)[2:4,],3)
    incdroptable[79:81,5]<-summary(model3.drops)$coefficients[c(2:4),4]
    # set.seed(12345)
  #   lboot<-lm.boot(lm(bc.id.m3, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
  #   perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
  #   incdroptable[79:81,3:4]<-perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)][c(1,3,5,2,4,6)] 
    
#-----------------
#-----------------
#(sens iv).serial 7s
summary(maindata$s7.z) #not very normally distributed...compare model-based ci and bootstrapping
hist(maindata$s7.z) #not very normally distributed...compare model-based ci and bootstrapping
  #model-based are wider and both are non-significant (in model 3) so go with model-based
  
# s7.m1<-s7.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# s7.m2<-s7.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# s7.m3<-s7.z ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2
 
s7.m1<-serial7s ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
s7.m2<-serial7s ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
s7.m3<-serial7s ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(s7.m1,data=maindata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    incvoltable[28,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    incvoltable[28,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(s7.m2, data=maindata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    incvoltable[29,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    incvoltable[29,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(s7.m3, data=maindata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    incvoltable[30,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    incvoltable[30,5]<-summary(model3.vol)$coefficients[2,4]  

#####
#income drops
# s7.id.m1<-s7.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# s7.id.m2<-s7.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# s7.id.m3<-s7.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

s7.id.m1<-serial7s ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
s7.id.m2<-serial7s ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
s7.id.m3<-serial7s ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(s7.id.m1, data=maindata)
  round(coefficients(model1.drops)[2:4],3)
    incdroptable[82:84,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    incdroptable[82:84,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    incdroptable[82:84,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(s7.id.m2, data=maindata)
  round(coefficients(model2.drops)[2:4],3)
    incdroptable[85:87,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    incdroptable[85:87,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    incdroptable[85:87,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(s7.id.m3, data=maindata)
  round(coefficients(model3.drops)[2:4],3)
    incdroptable[88:90,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    incdroptable[88:90,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    incdroptable[88:90,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#output results table
  write.csv(incvoltable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/sd_incvol_rawindscores.csv")
  #write.csv(incvoltable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/sd_incvol_indzscores.csv")
  write.csv(incdroptable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/numdrops_rawindscores.csv")
  #write.csv(incdroptable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/numdrops_indzscores.csv")
  #write.csv(cogimpair,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/cogimpair.csv")

    
#----------------------------------------------------------------------------------------------------------------------------------
##(15). Modifier analyses
  
#see "modifiers" R script and run

#----------------------------------------------------------------------------------------------------------------------------------
##(16). Adjust for covariate summaries over exposure period (for time-varying confounders)

#married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

sensdata<-maindata

#BMI
weightinkg<-grep("weight.k", names(sensdata), value = TRUE)
BMIs<-c("BMI.1990","BMI.1992","BMI.1993","BMI.1994","BMI.1996","BMI.1998","BMI.2000","BMI.2002","BMI.2004","BMI.2006","BMI.2008","BMI.2010")
sensdata[,BMIs]<-(sensdata[,weightinkg])/(sensdata$height.m.1985^2)

#Marital status
marital<-grep("maris", names(sensdata), value = TRUE)
sensdata[, marital] <- lapply(sensdata[, marital], function(x) ifelse(x<0, NA, x))
sensdata[, marital] <- lapply(sensdata[, marital], function(x) ifelse(x==1, 1, 0))
  table(sensdata$maristat.2010, exclude=NULL)

#Employment status
table(sensdata$numjobseverhad.2010)
hrswork<-grep("hours", names(sensdata), value = TRUE)
employed<-grep("employed", names(sensdata), value = TRUE)
sensdata[,hrswork] <- lapply(sensdata[,hrswork], function(x) ifelse(x<0, NA, x))
sensdata[,employed] <- lapply(sensdata[,employed], function(x) ifelse(x<0, NA, x))
empstat2<-c("empstat2.1990","empstat2.1991","empstat2.1992","empstat2.1993","empstat2.1994","empstat2.1996","empstat2.1998","empstat2.2000","empstat2.2002","empstat2.2004","empstat2.2006","empstat2.2008","empstat2.2010")
sensdata[, empstat2]<-ifelse(sensdata[,employed]==1&sensdata[,hrswork]==1,1,0) #just need to know if worked full time in a given year for summary measure
table(sensdata$empstat2.2010, exclude=NULL)

#Smoking status
table(sensdata$currsmoke.1992, sensdata$smokestat.1992)
currsmok<-grep("currsmoke.daily", names(sensdata), value = TRUE)[-1]
eversmok<-grep("eversmoke", names(sensdata), value = TRUE)[c(5,6,8,10,14)]
smokestat<-c("smokstat92","smokstat94","smokstat98","smokstat08","smokstat10")
sensdata[, smokestat]<-ifelse(sensdata[,currsmok]==1|sensdata[,currsmok]==2,1,0)
sensdata$smokstat92<-ifelse(sensdata$eversmoke.1992==0,0,sensdata$smokstat92)
sensdata$smokstat94<-ifelse(sensdata$eversmoke1.1994==0,0,sensdata$smokstat94)
sensdata$smokstat98<-ifelse(sensdata$eversmoke1.1998==0,0,sensdata$smokstat98)
sensdata$smokstat08<-ifelse(sensdata$eversmoke1.2008==0,0,sensdata$smokstat08)
sensdata$smokstat10<-ifelse(sensdata$eversmoke1.2010==0,0,sensdata$smokstat10)

#depressive symptoms
table(sensdata$sourceyear.40)
table(sensdata$sourceyear.50)
sensdata$CESD7.98to10<-sensdata$CESD7.H40
table(sensdata$CESD7.98to10)
sensdata$CESD7.98to10<-ifelse(sensdata$sourceyear.50==2008, sensdata$CESD7.H50, sensdata$CESD7.98to10)
sensdata$CESD7.98to10<-ifelse(sensdata$sourceyear.50==2010, sensdata$CESD7.H50, sensdata$CESD7.98to10)

cesdvars<-grep("CESD7", names(sensdata), value = TRUE)[-c(1,2,3)]
  cesdvars

sensdata[,cesdvars] <- lapply(sensdata[,cesdvars], function(x) ifelse(x<0, NA, x))
sensdata[,cesdvars] <- lapply(sensdata[,cesdvars], function(x) ifelse(x>=8, 1, 0))
  table(sensdata$CESD7.98to10)

#insurance variables
anyinsurance<-read.csv("anyinsurance.csv") #merge in years that age 40,50 and 60 health modules completed 
names(anyinsurance)
names(anyinsurance)<-c("case.ID", "famhaveinsurance90", "famhaveinsurance92","famhaveinsurance93","famhaveinsurance94","famhaveinsurance96","famhaveinsurance98",
                   "famhaveinsurance00","famhaveinsurance02","famhaveinsurance04","famhaveinsurance06","famhaveinsurance08","famhaveinsurance10")
insTV<-read.csv("insuranceTV.csv") #merge in years that age 40,50 and 60 health modules completed 
names(insTV)
grep("ins", names(sensdata), value = TRUE)
names(insTV)<- c("case.ID","curemplins.rr.90","prevemplins.rr.90","spcuremplins.rsp.90","prevemplins.rsp.90",
               "curemplins.rr.92","prevemplins.rr.92","spcuremplins.rsp.92","prevemplins.rsp.92", 
               "curemplins.rr.94","prevemplins.rr.94", "curemplins.rr.96","prevemplins.rr.96",
               "curemplins.rr.98","prevemplins.rr.98", "curemplins.rr.00","prevemplins.rr.00",
               "curemplins.rr.02","curemplins.rsp.02","prevemplins.rrsp.02",
               "curemplins.rr.04","curemplins.rsp.04","prevemplins.rrsp.04",
               "curemplins.rr.06","curemplins.rsp.06","prevemplins.rrsp.06",
               "curprevemplins.rr.08","curprevemplins.rsp.08","curprevemplins.rr.10","curprevemplins.rsp.10")
instv<-grep("ins", names(insTV), value = TRUE)
insTV[,instv] <- lapply(insTV[,instv], function(x) ifelse(x<0, NA, x))

insdata<-merge(anyinsurance, insTV, by="case.ID")
insdata <- insdata %>%
    mutate(
      epinsurance.90 = case_when(famhaveinsurance90==0 ~ 0, #has no insurance
                                (curemplins.rr.90==1|prevemplins.rr.90==1|spcuremplins.rsp.90==1|prevemplins.rsp.90==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance90==1 ~ 1), #has other insurance
      epinsurance.92 = case_when(famhaveinsurance92==0 ~ 0, #has no insurance
                                (curemplins.rr.92==1|prevemplins.rr.92==1|spcuremplins.rsp.92==1|prevemplins.rsp.92==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance92==1 ~ 1), #has other insurance
      epinsurance.94 = case_when(famhaveinsurance94==0 ~ 0, #has no insurance
                                (curemplins.rr.94==1|prevemplins.rr.94==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance94==1 ~ 1), #has other insurance
      epinsurance.96 = case_when(famhaveinsurance96==0 ~ 0, #has no insurance
                                (curemplins.rr.96==1|prevemplins.rr.96==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance96==1 ~ 1), #has other insurance
      epinsurance.98 = case_when(famhaveinsurance98==0 ~ 0, #has no insurance
                                (curemplins.rr.98==1|prevemplins.rr.98==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance98==1 ~ 1), #has other insurance
      epinsurance.00 = case_when(famhaveinsurance00==0 ~ 0, #has no insurance
                                (curemplins.rr.00==1|prevemplins.rr.00==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance00==1 ~ 1), #has other insurance
      epinsurance.02 = case_when(famhaveinsurance02==0 ~ 0, #has no insurance
                                (curemplins.rr.02==1|curemplins.rsp.02==1|prevemplins.rrsp.02==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance02==1 ~ 1), #has other insurance
      epinsurance.04 = case_when(famhaveinsurance04==0 ~ 0, #has no insurance
                                (curemplins.rr.04==1|curemplins.rsp.04==1|prevemplins.rrsp.04==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance04==1 ~ 1), #has other insurance
      epinsurance.06 = case_when(famhaveinsurance06==0 ~ 0, #has no insurance
                                (curemplins.rr.06==1|curemplins.rsp.06==1|prevemplins.rrsp.06==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance06==1 ~ 1), #has other insurance
      epinsurance.08 = case_when(famhaveinsurance08==0 ~ 0, #has no insurance
                                (curprevemplins.rr.08==1|curprevemplins.rsp.08==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance08==1 ~ 1), #has other insurance
      epinsurance.10 = case_when(famhaveinsurance10==0 ~ 0, #has no insurance
                                (curprevemplins.rr.10==1|curprevemplins.rsp.10==1) ~ 2, #employer-provided health insurance
                                famhaveinsurance10==1 ~ 1) #has other insurance
    )

sensdata<-left_join(sensdata,insdata,by="case.ID")
allins<-grep("epinsurance.", names(sensdata), value = TRUE)[-1]

#heavydrinking
alcTV<-read.csv("alcoholTV.csv")  
names(alcTV)
names(alcTV)<-c("case.ID", "daysalc92", "numdrinks92", "anyalc94", "daysalc94", "numdrinks94", "anyalc02","daysalc02", "numdrinks02", 
                "anyalc06", "daysalc06", "numdrinks06", "anyalc08", "daysalc08", "numdrinks08", "anyalc10", "daysalc10", "numdrinks10")

alc<-grep("s", names(alcTV), value = TRUE)[-1]

#use 1992 variables and keep it simple; clean alcohol variables for now (clean others in later step)
alcTV[,alc] <- lapply(alcTV[,alc], function(x) ifelse(x<0, NA, x))
daysalc<-grep("days", names(alcTV), value = TRUE)[-1]
numdrink<-grep("num", names(alcTV), value = TRUE)
anydrink<-grep("any", names(alcTV), value = TRUE)

daysalc.week<-c("daysalcweek94", "daysalcweek02", "daysalcweek06", "daysalcweek08", "daysalcweek10")

alcTV[,daysalc.week] <- lapply(alcTV[,anydrink], function(x) ifelse(x==0, 0, NA))
  table(alcTV$anyalc02, exclude=NULL)
  table(alcTV$daysalcweek02, exclude=NULL)  

table(alcTV$daysalc08, exclude=NULL)  
table(alcTV$daysalc92, exclude=NULL)

alcTV$daysalcweek92<-ifelse(alcTV$daysalc92==0,0,
                        ifelse(alcTV$daysalc92==1,7,
                               ifelse(alcTV$daysalc92==2,5.5,
                                      ifelse(alcTV$daysalc92==3,3.5,
                                             ifelse(alcTV$daysalc92==4,1.5,
                                                    ifelse(alcTV$daysalc92==5,0.5,NA))))))

alcTV$daysalc94[alcTV$anyalc94==0]<-0
alcTV$daysalc02[alcTV$anyalc02==0]<-0
alcTV$daysalc06[alcTV$anyalc06==0]<-0
alcTV$daysalc08[alcTV$anyalc08==0]<-0
alcTV$daysalc10[alcTV$anyalc10==0]<-0

alcTV$numdrinks92[alcTV$daysalcweek92==0]<-0
alcTV$numdrinks94[alcTV$anyalc94==0]<-0
alcTV$numdrinks02[alcTV$anyalc02==0]<-0
alcTV$numdrinks06[alcTV$anyalc06==0]<-0
alcTV$numdrinks08[alcTV$anyalc08==0]<-0
alcTV$numdrinks10[alcTV$anyalc10==0]<-0

alcTV$daysalcweek94<-ifelse(is.na(alcTV$daysalcweek94),(alcTV$daysalc94/30)*7,0)
alcTV$daysalcweek02<-ifelse(is.na(alcTV$daysalcweek02),(alcTV$daysalc02/30)*7,0)
alcTV$daysalcweek06<-ifelse(is.na(alcTV$daysalcweek06),(alcTV$daysalc06/30)*7,0)
alcTV$daysalcweek08<-ifelse(is.na(alcTV$daysalcweek08),(alcTV$daysalc08/30)*7,0)
alcTV$daysalcweek10<-ifelse(is.na(alcTV$daysalcweek10),(alcTV$daysalc10/30)*7,0)

drinkspweek<-c("drinkpweek92","drinkpweek94","drinkpweek02","drinkpweek06","drinkpweek08","drinkpweek10")
dayspweek<-c("daysalcweek92","daysalcweek94","daysalcweek02","daysalcweek06","daysalcweek08","daysalcweek10")

alcTV[,drinkspweek]<-alcTV[,numdrink]*alcTV[,dayspweek]
  summary(alcTV$drinkpweek10)

#CDC guidelines: moderate= <=1 drink/day for women and <=2 drinks/day for men
#heavy: 8+ drinks/week for women and 15+ drinks/week for men
#https://www.cdc.gov/alcohol/faqs.htm#:~:text=The%20Dietary%20Guidelines%20for%20Americans%20recommends%20that%20adults%20who%20choose,on%20a%20day%20for%20men.
  
#never, moderate, heavy - sex specific, so need to merge alc in with full dataset
names(alcTV)
alc<-alcTV[c("case.ID","drinkpweek92","drinkpweek94","drinkpweek02","drinkpweek06","drinkpweek08","drinkpweek10")]
sensdata<-left_join(sensdata,alc,by="case.ID")

table(sensdata$sex) #1=male; 2=female
alcvars<-c("drinkpweek92","drinkpweek94","drinkpweek02","drinkpweek06","drinkpweek08","drinkpweek10")
heavydrink<-c("heavydrink92","heavydrink94","heavydrink02","heavydrink06","heavydrink08","heavydrink10")  

table(sensdata$drinkpweek02)
sensdata[,heavydrink]<-ifelse(sensdata$sex=="Female"&sensdata[,alcvars]==0,0,
                              ifelse(sensdata$sex=="Female"&sensdata[,alcvars]<8,1,
                                     ifelse(sensdata$sex=="Female"&sensdata[,alcvars]>=8,2,
                                            ifelse(sensdata$sex=="Male"&sensdata[,alcvars]==0,0,
                                                   ifelse(sensdata$sex=="Male"&sensdata[,alcvars]<15,1,
                                                          ifelse(sensdata$sex=="Male"&sensdata[,alcvars]>=15,2,NA))))))

table(sensdata$heavydrink92) #0,1,2 - do like insurance

#summary measures
sensdata$avg.BMI<-rowMeans(sensdata[,BMIs], na.rm=TRUE)
sensdata$avg.married<-rowMeans(sensdata[,marital], na.rm=TRUE)
sensdata$avg.marriedcat<-ifelse(sensdata$avg.married==1,"Always",
                                ifelse(sensdata$avg.married==0,"Never","Sometimes"))
sensdata$avg.employed<-rowMeans(sensdata[,empstat2], na.rm=TRUE)
sensdata$avg.employedcat<-ifelse(sensdata$avg.employed==1,"Always",
                                ifelse(sensdata$avg.employed==0,"Never","Sometimes"))
sensdata$avg.smoke<-rowMeans(sensdata[,smokestat], na.rm=TRUE)
sensdata$avg.smokecat<-ifelse(sensdata$avg.smoke==1,"Always",
                                 ifelse(sensdata$avg.smoke==0,"Never","Sometimes"))
sensdata$avg.depress<-rowMeans(sensdata[,cesdvars], na.rm=TRUE)
sensdata$avg.depresscat<-ifelse(sensdata$avg.depress==1,"Always",
                              ifelse(sensdata$avg.depress==0,"Never","Sometimes"))
sensdata$avg.epins<-rowMeans(sensdata[,allins], na.rm=TRUE)/2
sensdata$avg.epinscat<-ifelse(sensdata$avg.epins==1,"Always",
                                ifelse(sensdata$avg.epins==0,"Never","Sometimes"))
sensdata$avg.alc<-rowMeans(sensdata[,heavydrink], na.rm=TRUE)/2
sensdata$avg.alccat<-ifelse(sensdata$avg.alc==1,"Always",
                              ifelse(sensdata$avg.alc==0,"Never","Sometimes"))

table(sensdata$avg.employedcat, exclude=NULL)
table(sensdata$avg.marriedcat, exclude=NULL)
table(sensdata$avg.smokecat, exclude=NULL)
table(sensdata$avg.depresscat, exclude=NULL)  
table(sensdata$avg.alccat, exclude=NULL)    
table(sensdata$avg.epinscat, exclude=NULL)    
hist(sensdata$avg.epins, exclude=NULL)
hist(sensdata$avg.alc, exclude=NULL)

####run sensitivity analysis with TV summaries variables

#create empty dataframes to store results
sensTVtable<-as.data.frame(matrix(nrow=24,ncol=5))
sensTVtable[,1]<-c("rawtotscore_M1","rawtotscore_M2","rawtotscore_M3",
                   "ztotscore_M1","ztotscore_M2","ztotscore_M3",
                   "rawtotscore_M1_1drop","rawtotscore_M1_2drop","rawtotscore_M1_3drop",
                   "rawtotscore_M2_1drop","rawtotscore_M2_2drop","rawtotscore_M2_3drop",
                   "rawtotscore_M3_1drop,","rawtotscore_M3_2drop","rawtotscore_M3_3drop",
                   "ztotscore_M1_1drop","ztotscore_M1_2drop","ztotscore_M1_3drop",
                   "ztotscore_M2_1drop","ztotscore_M2_2drop","ztotscore_M2_3drop",
                   "ztotscore_M3_1drop,","ztotscore_M3_2drop","ztotscore_M3_3drop")
names(sensTVtable)<-c("model","beta","lci","uci","pval")                   

#run analyses and store results         

#income voltility sensitivity analysis with average covariates over time instead - raw score outcome
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + avg.depresscat + avg.epins + avg.alc #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + avg.depresscat + avg.epins + avg.alc + eqincome90.1990 + avg.employedcat

model1.vol<-lm(TICs.m1,data=sensdata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    sensTVtable[1,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    sensTVtable[1,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(TICs.m2, data=sensdata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    sensTVtable[2,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    sensTVtable[2,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(TICs.m3, data=sensdata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    sensTVtable[3,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    sensTVtable[3,5]<-summary(model3.vol)$coefficients[2,4]

#income volatility (SD of pct change over time) sens analysis z score outcome
zTICs.m1<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat
zTICs.m2<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + depressed92 + avg.epins + avg.alc #these can be baseline proxies for health status?
zTICs.m3<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + depressed92 + avg.epins + avg.alc + eqincome90.1990 + avg.employedcat
    
model1.vol<-lm(zTICs.m1,data=sensdata)
  round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
    sensTVtable[4,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
  summary(model1.vol)$coefficients[2,4]
    sensTVtable[4,5]<-summary(model1.vol)$coefficients[2,4]
    
model2.vol<-lm(zTICs.m2, data=sensdata)
  round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
    sensTVtable[5,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
  summary(model2.vol)$coefficients[2,4]
    sensTVtable[5,5]<-summary(model2.vol)$coefficients[2,4]
    
model3.vol<-lm(zTICs.m3, data=sensdata)
  round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
    sensTVtable[6,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
  summary(model3.vol)$coefficients[2,4]
    sensTVtable[6,5]<-summary(model3.vol)$coefficients[2,4]
    
#income drops sens analysis raw score outcome
TICs.id.m1<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat
TICs.id.m2<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + avg.depresscat + avg.epins + avg.alc #these can be baseline proxies for health status?
TICs.id.m3<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + avg.depresscat + avg.epins + avg.alc + eqincome90.1990 + avg.employedcat

model1.drops<-lm(TICs.id.m1, data=sensdata)
  round(coefficients(model1.drops)[2:4],3)
    sensTVtable[7:9,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    sensTVtable[7:9,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    sensTVtable[7:9,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(TICs.id.m2, data=sensdata)
  round(coefficients(model2.drops)[2:4],3)
    sensTVtable[10:12,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    sensTVtable[10:12,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    sensTVtable[10:12,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(TICs.id.m3, data=sensdata)
  round(coefficients(model3.drops)[2:4],3)
    sensTVtable[13:15,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    sensTVtable[13:15,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    sensTVtable[13:15,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#####
#income drops sens analysis z score outcome
zTICs.id.m1<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat
zTICs.id.m2<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + avg.depresscat + avg.epins + avg.alc #these can be baseline proxies for health status?
zTICs.id.m3<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + avg.marriedcat + avg.BMI + avg.smokecat + avg.depresscat + avg.epins + avg.alc + eqincome90.1990 + avg.employedcat

model1.drops<-lm(zTICs.id.m1, data=sensdata)
  round(coefficients(model1.drops)[2:4],3)
    sensTVtable[16:18,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
    sensTVtable[16:18,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
    sensTVtable[16:18,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zTICs.id.m2, data=sensdata)
  round(coefficients(model2.drops)[2:4],3)
    sensTVtable[19:21,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
    sensTVtable[19:21,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
    sensTVtable[19:21,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zTICs.id.m3, data=sensdata)
  round(coefficients(model3.drops)[2:4],3)
    sensTVtable[22:24,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
    sensTVtable[22:24,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
    sensTVtable[22:24,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#output results table
  #write.csv(sensTVtable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/TVcovsens_results.csv")
    
#----------------------------------------------------------------------------------------------------------------------------------
##(17). Additional sensitivity analyses
  #run modifiers (run code "modifiers")
  #run modifiers_zscore (run code "modifiers_zscore")
  #imputed exposure sens analysis (run code "imputed_exposure_sens_analysis_04012024")
  #ip weights (run code "IPW sens for generalizability")
  #NLSY weights (run code "sampling weights sens")
  
