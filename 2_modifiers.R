#run libraries from main file

load(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.rda")
sensdata<-maindata
head(sensdata)

#First look within strata of race/ethnicity and sex (as proxies for exposure to structural racism and sexism, determinants of sustained poverty)

racesexmod<-as.data.frame(matrix(nrow=33, ncol=6))
names(racesexmod)<-c("model","N","beta","lci","uci","pval")
racesexmod[,1]<-c("Black.m1","Black.m2","Black.m3","white.m1","white.m2","white.m3","Hispanic.m1","Hispanic.m2","Hispanic.m3",
                  "female.m1","female.m2","female.m3","male.m1","male.m2","male.m3",
                  "Black.female.m1","Black.female.m2","Black.female.m3","white.female.m1","white.female.m2","white.female.m3","Hispanic.female.m1","Hispanic.female.m2","Hispanic.female.m3",
                  "Black.male.m1","Black.male.m2","Black.male.m3","white.male.m1","white.male.m2","white.male.m3","Hispanic.male.m1","Hispanic.male.m2","Hispanic.male.m3")
racesexmod[1:9,2]<-table(sensdata$raceth)[c(2,2,2,1,1,1,3,3,3)]
racesexmod[10:15,2]<-table(sensdata$female)[c(2,2,2,1,1,1)]
racesexmod[16:33,2]<-table(sensdata$raceth, sensdata$female)[c(5,5,5,4,4,4,6,6,6,2,2,2,1,1,1,3,3,3)]

###Racial group membership

#income volatility (SD of pct change over time)
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

#Black
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$raceth=="Black",])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    racesexmod[1,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    racesexmod[1,6]<-summary(sens.vol1)$coefficients[2,4]

sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$raceth=="Black",])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    racesexmod[2,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    racesexmod[2,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$raceth=="Black",])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    racesexmod[3,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    racesexmod[3,6]<-summary(sens.vol3)$coefficients[2,4]


#white
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    racesexmod[4, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    racesexmod[4, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    racesexmod[5, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    racesexmod[5, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    racesexmod[6, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    racesexmod[6, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#Hispanic
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$raceth == "Hispanic", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    racesexmod[7, 3:5] <-round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    racesexmod[7, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$raceth == "Hispanic", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    racesexmod[8, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    racesexmod[8, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$raceth == "Hispanic", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    racesexmod[9, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    racesexmod[9, 6] <- summary(sens.vol3)$coefficients[2, 4]        

###Sex
    
table(sensdata$sex)
#income volatility (SD of pct change over time)
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + raceth + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

#female
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$sex=="Female",])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    racesexmod[10,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    racesexmod[10,6]<-summary(sens.vol1)$coefficients[2,4]

sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$sex=="Female",])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    racesexmod[11,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    racesexmod[11,6]<-summary(sens.vol2)$coefficients[2,4]

sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$sex=="Female",])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    racesexmod[12,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    racesexmod[12,6]<-summary(sens.vol3)$coefficients[2,4]

#male
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$sex == "Male", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    racesexmod[13, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    racesexmod[13, 6] <- summary(sens.vol1)$coefficients[2, 4]

sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$sex == "Male", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    racesexmod[14, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    racesexmod[14, 6] <- summary(sens.vol2)$coefficients[2, 4]

sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$sex == "Male", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    racesexmod[15, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    racesexmod[15, 6] <- summary(sens.vol3)$coefficients[2, 4]

###Race/ethnicity x Sex strata

#income volatility (SD of pct change over time)
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

#Black & female
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$raceth=="Black"&sensdata$sex=="Female",])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    racesexmod[16,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    racesexmod[16,6]<-summary(sens.vol1)$coefficients[2,4]

sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$raceth=="Black"&sensdata$sex=="Female",])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    racesexmod[17,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    racesexmod[17,6]<-summary(sens.vol2)$coefficients[2,4]

sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$raceth=="Black"&sensdata$sex=="Female",])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    racesexmod[18,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    racesexmod[18,6]<-summary(sens.vol3)$coefficients[2,4]


#white & female
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black"&sensdata$sex=="Female", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    racesexmod[19,3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    racesexmod[19,6] <- summary(sens.vol1)$coefficients[2, 4]

sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black"&sensdata$sex=="Female", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    racesexmod[20,3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    racesexmod[20,6] <- summary(sens.vol2)$coefficients[2, 4]

sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black"&sensdata$sex=="Female", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    racesexmod[21,3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    racesexmod[21,6] <- summary(sens.vol3)$coefficients[2, 4]

#Hispanic & female
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$raceth == "Hispanic"&sensdata$sex=="Female", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    racesexmod[22, 3:5] <-round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    racesexmod[22, 6] <- summary(sens.vol1)$coefficients[2, 4]

sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$raceth == "Hispanic"&sensdata$sex=="Female", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    racesexmod[23, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    racesexmod[23, 6] <- summary(sens.vol2)$coefficients[2, 4]

sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$raceth == "Hispanic"&sensdata$sex=="Female", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    racesexmod[24, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    racesexmod[24, 6] <- summary(sens.vol3)$coefficients[2, 4]        

#Black & male
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$raceth=="Black"&sensdata$sex=="Male",])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    racesexmod[25,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    racesexmod[25,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$raceth=="Black"&sensdata$sex=="Male",])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    racesexmod[26,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    racesexmod[26,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$raceth=="Black"&sensdata$sex=="Male",])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    racesexmod[27,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    racesexmod[27,6]<-summary(sens.vol3)$coefficients[2,4]
    
#white & male
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black"&sensdata$sex=="Male", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    racesexmod[28,3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    racesexmod[28,6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black"&sensdata$sex=="Male", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    racesexmod[29,3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    racesexmod[29,6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$raceth == "Non-Hispanic, non-Black"&sensdata$sex=="Male", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    racesexmod[30,3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    racesexmod[30,6] <- summary(sens.vol3)$coefficients[2, 4]
  
#Hispanic & male
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$raceth == "Hispanic"&sensdata$sex=="Male", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    racesexmod[31, 3:5] <-round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    racesexmod[31, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$raceth == "Hispanic"&sensdata$sex=="Male", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    racesexmod[32, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    racesexmod[32, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$raceth == "Hispanic"&sensdata$sex=="Male", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    racesexmod[33, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    racesexmod[33, 6] <- summary(sens.vol3)$coefficients[2, 4]    

#output results table
  write.csv(racesexmod,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/racesexmod_rawscores.csv")
    
    
##--------------------------------------------------

#how do financial resources (or lack of) modify impact of income volatility on cognition?
    
#poverty & home ownership over exposure period
povdata<-read.csv("pov_homeown.csv") #merge in years that age 40,50 and 60 health modules completed 
povdata<-povdata[,-2]
names(povdata)<-c("case.ID", "Ownhome.1990", "Poverty.1990","Poverty.1991","Ownhome.1992","Poverty.1992","Ownhome.1993","Poverty.1993","Ownhome.1994","Poverty.1994",
                  "Poverty.1996","Ownhome.1996","Ownhome.1998","Poverty.1998","Ownhome.2000","Poverty.2000","Poverty.2002","Ownhome.2004","Poverty.2004","Poverty.2006",
                  "Ownhome.2008","Poverty.2008","Poverty.2010")
poverty<-grep("Pov", names(povdata), value = TRUE)
  table(povdata$Poverty.1990)
povdata[, poverty] <- lapply(povdata[, poverty], function(x) ifelse(x<0, NA, x))
  table(povdata$Poverty.1990)

povvars01<-c("povmiss.1990", "povmiss.1991", "povmiss.1992", "povmiss.1993", 
                "povmiss.1994", "povmiss.1996", "povmiss.1998", "povmiss.2000", 
                "povmiss.2002","povmiss.2004", "povmiss.2006","povmiss.2008","povmiss.2010")

povdata[,povvars01] <- lapply(povdata[,poverty], function(x) ifelse(!is.na(x), 1, 0))

povdata$povtimes<-rowSums(povdata[,povvars01], na.rm=TRUE)
  table(povdata$povtimes)

povdata$avg.poverty<-rowMeans(povdata[,poverty], na.rm=TRUE)
  table(povdata$avg.poverty) #most people never in poverty

povdata$everpov<-ifelse(povdata$avg.poverty>0,1,0) #in poverty in at least one time point over exposure period
  table(povdata$everpov,exclude=NULL)  

#home ownership over exposure period
homeown<-grep("Own", names(povdata), value = TRUE)
povdata[, homeown] <- lapply(povdata[, homeown], function(x) ifelse(x<0, NA, x))

homevars01<-c("homemiss.1990", "homemiss.1992", "homemiss.1993", 
               "homemiss.1994", "homemiss.1996", "homemiss.1998",
               "homemiss.2000", "homemiss.2004", "homemiss.2008")
povdata[,homevars01] <- lapply(povdata[,homeown], function(x) ifelse(!is.na(x), 1, 0))
  
povdata$homeowntimes<-rowSums(povdata[,homevars01], na.rm=TRUE)
  table(povdata$homeowntimes, exclude=NULL)

povdata$avg.homeown<-rowMeans(povdata[,homeown], na.rm=TRUE)
  table(povdata$avg.homeown, exclude=NULL) #never, sometimes, always?

povdata$neverhome<-ifelse(povdata$avg.homeown==0,1,0) #never owned home over exposure period
  table(povdata$neverhome, exclude=NULL)    

#rejoin datasets
sensdata<-left_join(sensdata,povdata,by="case.ID")
  table(sensdata$everpov,exclude=NULL) #0 missing in sensdata  
  table(sensdata$neverhome,exclude=NULL) #0 missing in sensdata  

table(sensdata$avg.homeown) #524 people always own home so could try categorical as well
table(sensdata$avg.poverty) #only 29 people always in poverty so make binary
sensdata$ownhome.c<-ifelse(sensdata$avg.homeown==0,"never",
                           ifelse(sensdata$avg.homeown==1,"always","sometimes"))    

table(sensdata$ownhome.c)

#finally, look at income class (do baseline income?)

#also look at income levels
#https://www.pewresearch.org/social-trends/2016/05/11/methodology-4/
#https://www.pewresearch.org/social-trends/2016/05/11/americas-shrinking-middle-class-a-close-look-at-changes-within-metropolitan-areas/
#https://www.brookings.edu/articles/there-are-many-definitions-of-middle-class-heres-ours/

sensdata$eqincome90.1990.hh2<-sensdata$eqincome90.1990*sqrt(2)  
  summary(sensdata$eqincome90.1990.hh2)  

fedcut1<-31358*(2/3)
fedcut2<-31358*2

sensdata$income90.class<-ifelse(sensdata$eqincome90.1990.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.1990.hh2>fedcut2,2,1))

table(sensdata$income90.class)
prop.table(table(sensdata$income90.class))
prop.table(table(sensdata$income90.class, sensdata$num.drops4),2)


#look at income class over the entire time frame?
sensdata$eqincome90.1990.hh2<-sensdata$eqincome90.1990*sqrt(2)  
sensdata$eqincome90.1991.hh2<-sensdata$eqincome90.1991*sqrt(2)  
sensdata$eqincome90.1992.hh2<-sensdata$eqincome90.1992*sqrt(2)  
sensdata$eqincome90.1993.hh2<-sensdata$eqincome90.1993*sqrt(2)  
sensdata$eqincome90.1994.hh2<-sensdata$eqincome90.1994*sqrt(2)  
sensdata$eqincome90.1996.hh2<-sensdata$eqincome90.1996*sqrt(2)  
sensdata$eqincome90.1998.hh2<-sensdata$eqincome90.1998*sqrt(2)  
sensdata$eqincome90.2000.hh2<-sensdata$eqincome90.2000*sqrt(2)  
sensdata$eqincome90.2002.hh2<-sensdata$eqincome90.2002*sqrt(2)  
sensdata$eqincome90.2004.hh2<-sensdata$eqincome90.2004*sqrt(2)  
sensdata$eqincome90.2006.hh2<-sensdata$eqincome90.2006*sqrt(2)  
sensdata$eqincome90.2008.hh2<-sensdata$eqincome90.2008*sqrt(2)  
sensdata$eqincome90.2010.hh2<-sensdata$eqincome90.2010*sqrt(2)  

#since all are in 1990 dollars can just use 1990 fed median for cut points
sensdata$income91.class<-ifelse(sensdata$eqincome90.1991.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.1991.hh2>fedcut2,2,1))
  table(sensdata$income91.class)
sensdata$income92.class<-ifelse(sensdata$eqincome90.1992.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.1992.hh2>fedcut2,2,1))
  table(sensdata$income92.class)
sensdata$income93.class<-ifelse(sensdata$eqincome90.1993.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.1993.hh2>fedcut2,2,1))
  table(sensdata$income93.class)
sensdata$income94.class<-ifelse(sensdata$eqincome90.1994.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.1994.hh2>fedcut2,2,1))
  table(sensdata$income94.class)
sensdata$income96.class<-ifelse(sensdata$eqincome90.1996.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.1996.hh2>fedcut2,2,1))
  table(sensdata$income96.class)
sensdata$income98.class<-ifelse(sensdata$eqincome90.1998.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.1998.hh2>fedcut2,2,1))
  table(sensdata$income98.class)
sensdata$income00.class<-ifelse(sensdata$eqincome90.2000.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.2000.hh2>fedcut2,2,1))
  table(sensdata$income00.class)
sensdata$income02.class<-ifelse(sensdata$eqincome90.2002.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.2002.hh2>fedcut2,2,1))
  table(sensdata$income02.class)
sensdata$income04.class<-ifelse(sensdata$eqincome90.2004.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.2004.hh2>fedcut2,2,1))
  table(sensdata$income04.class)
sensdata$income06.class<-ifelse(sensdata$eqincome90.2006.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.2006.hh2>fedcut2,2,1))
  table(sensdata$income06.class)
sensdata$income08.class<-ifelse(sensdata$eqincome90.2008.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.2008.hh2>fedcut2,2,1))
  table(sensdata$income08.class)
sensdata$income10.class<-ifelse(sensdata$eqincome90.2010.hh2<fedcut1,0,
                                ifelse(sensdata$eqincome90.2010.hh2>fedcut2,2,1))
table(sensdata$income10.class)

inclass<-grep("class", names(sensdata), value = TRUE)

sensdata$avg.inclass<-rowMeans(sensdata[,inclass], na.rm=TRUE)
  table(sensdata$avg.inclass)
  hist(sensdata$avg.inclass)

sensdata$always.lowinc<-ifelse(sensdata$avg.inclass==0,1,0)  
  table(sensdata$always.lowinc)

#alternative way is to just use average 1990 income for HH of 2 and then dichotomize

  allincomes<-c("eqincome90.1990.hh2","eqincome90.1991.hh2","eqincome90.1992.hh2","eqincome90.1993.hh2","eqincome90.1994.hh2","eqincome90.1996.hh2","eqincome90.1998.hh2","eqincome90.2000.hh2","eqincome90.2002.hh2","eqincome90.2004.hh2","eqincome90.2006.hh2","eqincome90.2008.hh2","eqincome90.2010.hh2") 
  
sensdata$avg1990income<-rowMeans(sensdata[,allincomes], na.rm=TRUE)
  summary(sensdata$avg1990income) #most people never in poverty
  
sensdata$avg90income.class<-ifelse(sensdata$avg1990income<fedcut1,0,
                                  ifelse(sensdata$avg1990income>fedcut2,2,1))

table(sensdata$avg90income.class) #using avg 1990 income across all time points
table(sensdata$income90.class) #just baseline

#financial resources modifier variables
table(sensdata$everpov) #1=ever in poverty over exposure period, 0=never
table(sensdata$low.wealth) #average wealth over exposure period <2000 (ave assets in 1990 dollars)
table(sensdata$assets2000) #average assets over exposure period <2000 (ave assets in 1990 dollars)
table(sensdata$avgdebts01) #assets always greater than debts
table(sensdata$neverhome) #never==1 vs. sometimes/always 
table(sensdata$ownhome.c) #never, sometimes, always
table(sensdata$HSeduc) #>12 vs <=12
table(sensdata$income90.class) #low (0), middle, high (2)
table(sensdata$avg90income.class) #low (0), middle, high (2)

#create blank data frame to store modifier results
financialmod<-as.data.frame(matrix(nrow=66, ncol=6))
names(financialmod)<-c("model","N","beta","lci","uci","pval")

financialmod[,1]<-c("neverpov.m1","neverpov.m2","neverpov.m3","everpov.m1","everpov.m2","everpov.m3",
                    "avgwealth<2000.m1","avgwealth<2000.m2","avgwealth<2000.m3","avgwealth>2000.m1","avgwealth>2000.m2","avgwealth>2000.m3",
                    "avgassets<2000.m1","avgassets<2000.m2","avgassets<2000.m3","avgassets>2000.m1","avgassets>2000.m2","avgassets>2000.m3",
                    "prop2000insavingsalways.m1","prop2000insavingsalways.m2","prop2000insavingsalways.m3","prop2000insavingssome.m1","prop2000insavingssome.m2","prop2000insavingssome.m3","prop2000insavingsnever.m1","prop2000insavingsnever.m2","prop2000insavingsnever.m3",
                    "neverhome.m1","neverhome.m2","neverhome.m3","somehome.m1","somehome.m2","somehome.m3",
                    "neverhome.m1","neverhome.m2","neverhome.m3","somehome.m1","somehome.m2","somehome.m3","alwayshome.m1","alwayshome.m2","alwayshome.m3",
                    "educ=<12years.m1","educ=<12years.m2","educ=<12years.m3","educ>12years.m1","educ>12years.m2","educ>12years.m3",
                    "inclasslow.m1","inclasslow.m2","inclasslow.m3","inclassmid.m1","inclassmid.m2","inclassmid.m3","inclasshigh.m1","inclasshigh.m2","inclasshigh.m3",
                    "avginclasslow.m1","avginclasslow.m2","avginclasslow.m3","avginclassmid.m1","avginclassmid.m2","avginclassmid.m3","avginclasshigh.m1","avginclasshigh.m2","avginclasshigh.m3")
                    
financialmod[1:6,2]<-table(sensdata$everpov)[c(1,1,1,2,2,2)]
financialmod[7:12,2]<-table(sensdata$low.wealth)[c(2,2,2,1,1,1)]
financialmod[13:18,2]<-table(sensdata$assets2000)[c(2,2,2,1,1,1)]
financialmod[19:27,2]<-table(sensdata$p2000assets.c)[c(1,1,1,3,3,3,2,2,2)]
financialmod[28:33,2]<-table(sensdata$neverhome)[c(2,2,2,1,1,1)]
financialmod[34:42,2]<-table(sensdata$ownhome.c)[c(2,2,2,3,3,3,1,1,1)]
financialmod[43:48,2]<-table(sensdata$HSeduc)[c(2,2,2,1,1,1)]
financialmod[49:57,2]<-table(sensdata$income90.class)[c(1,1,1,2,2,2,3,3,3)] #baseline
financialmod[58:66,2]<-table(sensdata$avg90income.class)[c(1,1,1,2,2,2,3,3,3)] #using average 90 income across time frame

#stratify by wealth and assets<2000
#income volatility (SD of pct change over time)
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth +  HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth +  HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

#poverty status

#never in poverty (everpov==0)
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$everpov==0,])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[1,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[1,6]<-summary(sens.vol1)$coefficients[2,4]

sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$everpov==0,])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[2,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[2,6]<-summary(sens.vol2)$coefficients[2,4]

sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$everpov==0,])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[3,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[3,6]<-summary(sens.vol3)$coefficients[2,4]

#in poverty in at least one survey year (everpov==1)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$everpov==1, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[4, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[4, 6] <- summary(sens.vol1)$coefficients[2, 4]

sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$everpov==1, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[5, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[5, 6] <- summary(sens.vol2)$coefficients[2, 4]

sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$everpov==1, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[6, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[6, 6] <- summary(sens.vol3)$coefficients[2, 4]

#limited wealth on average over exposure period
    
#low avg wealth (low.wealth==1)
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$low.wealth==1,])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[7,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[7,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$low.wealth==1,])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[8,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[8,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$low.wealth==1,])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[9,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[9,6]<-summary(sens.vol3)$coefficients[2,4]
    
#in poverty in at least one survey year (everpov==1)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$low.wealth==0, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[10, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[10, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$low.wealth==0, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[11, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[11, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$low.wealth==0, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[12, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[12, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#limited assets (savings) on average over exposure period

#assets <2000 (assets2000==1)
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$assets2000==1,])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[13,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[13,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$assets2000==1,])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[14,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[14,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$assets2000==1,])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[15,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[15,6]<-summary(sens.vol3)$coefficients[2,4]
    
#assets >=2000 (assets2000==0)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$assets2000==0, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[16, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[16, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$assets2000==0, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[17, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[17, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$assets2000==0, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[18, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[18, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#proportion of exposure period had 2000 available in savings etc. 

#(p2000assets.c=="always")
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$p2000assets.c=="always",])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[19,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[19,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$p2000assets.c=="always",])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[20,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[20,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$p2000assets.c=="always",])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[21,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[21,6]<-summary(sens.vol3)$coefficients[2,4]
    
#(p2000assets.c=="sometimes")
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$p2000assets.c=="sometimes", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[22, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[22, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$p2000assets.c=="sometimes", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[23, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[23, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$p2000assets.c=="sometimes", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[24, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[24, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#(p2000assets.c=="never")
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$p2000assets.c=="never", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[25, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[25, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$p2000assets.c=="never", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[26, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[26, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$p2000assets.c=="never", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[27, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[27, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
    
#never or sometimes owned home over exposure period

#never (neverhome==1)
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$neverhome==1,])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[28,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[28,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$neverhome==1,])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[29,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[29,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$neverhome==1,])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[30,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[30,6]<-summary(sens.vol3)$coefficients[2,4]
    
#owned home at least one survey period (neverhome==0)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$neverhome==0, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[31, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[31, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$neverhome==0, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[32, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[32, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$neverhome==0, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[33, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[33, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#never, sometimes, or always owned home over exposure period

#never owned home
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$ownhome.c=="never",])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[34,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[34,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$ownhome.c=="never",])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[35,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[35,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$ownhome.c=="never",])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[36,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[36,6]<-summary(sens.vol3)$coefficients[2,4]
    
#owned home at least one survey period but not always 
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$ownhome.c=="sometimes", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[37, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[37, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$ownhome.c=="sometimes", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[38, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[38, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$ownhome.c=="sometimes", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[39, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[39, 6] <- summary(sens.vol3)$coefficients[2, 4]
  
#always owned home  
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$ownhome.c=="always", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[40, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[40, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$ownhome.c=="always", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[41, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[41, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$ownhome.c=="always", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[42, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[42, 6] <- summary(sens.vol3)$coefficients[2, 4]

#education level at baseline
    
edTICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + married
edTICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
edTICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2
    
#=<12 years education
sens.vol1 <- lm(edTICs.m1, data = sensdata[sensdata$HSeduc=="<=12 years", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[43, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[43, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(edTICs.m2, data = sensdata[sensdata$HSeduc=="<=12 years", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[44, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[44, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(edTICs.m3, data = sensdata[sensdata$HSeduc=="<=12 years", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[45, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[45, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#>12 years education
sens.vol1 <- lm(edTICs.m1, data = sensdata[sensdata$HSeduc==">12 years", ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[46, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[46, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(edTICs.m2, data = sensdata[sensdata$HSeduc==">12 years", ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[47, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[47, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(edTICs.m3, data = sensdata[sensdata$HSeduc==">12 years", ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[48, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[48, 6] <- summary(sens.vol3)$coefficients[2, 4]

#income class at baseline (1990)
table(sensdata$income90.class)    

#low income class (income90.class==0)
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$income90.class==0,])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[49,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[49,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$income90.class==0,])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[50,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[50,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$income90.class==0,])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[51,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[51,6]<-summary(sens.vol3)$coefficients[2,4]
    
#middle income class (income90.class==1)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$income90.class==1, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[52, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[52, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$income90.class==1, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[53, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[53, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$income90.class==1, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[54, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[54, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#high income class (income90.class==2)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$income90.class==2, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[55, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[55, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$income90.class==2, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[56, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[56, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$income90.class==2, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[57, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[57, 6] <- summary(sens.vol3)$coefficients[2, 4]

#income class using average 1990 income across exposure period then categorizing w.r.t. 1990 federal median
table(sensdata$avg90income.class)    
    
#low income class (income90.class==0)
sens.vol1<-lm(TICs.m1,data=sensdata[sensdata$avg90income.class==0,])
  round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
    financialmod[58,3:5]<-round(c(coefficients(sens.vol1)["sd_incvol"],confint(sens.vol1)["sd_incvol",]),5)
  summary(sens.vol1)$coefficients[2,4]
    financialmod[58,6]<-summary(sens.vol1)$coefficients[2,4]
    
sens.vol2<-lm(TICs.m2,data=sensdata[sensdata$avg90income.class==0,])
  round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
    financialmod[59,3:5]<-round(c(coefficients(sens.vol2)["sd_incvol"],confint(sens.vol2)["sd_incvol",]),5)
  summary(sens.vol2)$coefficients[2,4]
    financialmod[59,6]<-summary(sens.vol2)$coefficients[2,4]
    
sens.vol3<-lm(TICs.m3,data=sensdata[sensdata$avg90income.class==0,])
  round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
    financialmod[60,3:5]<-round(c(coefficients(sens.vol3)["sd_incvol"],confint(sens.vol3)["sd_incvol",]),5)
  summary(sens.vol3)$coefficients[2,4]
    financialmod[60,6]<-summary(sens.vol3)$coefficients[2,4]
    
#middle income class (avg90income.class==1)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$avg90income.class==1, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[61, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[61, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$avg90income.class==1, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[62, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[62, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$avg90income.class==1, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[63, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[63, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
#high income class (avg90income.class==2)
sens.vol1 <- lm(TICs.m1, data = sensdata[sensdata$avg90income.class==2, ])
  round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
    financialmod[64, 3:5] <- round(c(coefficients(sens.vol1)["sd_incvol"], confint(sens.vol1)["sd_incvol", ]), 5)
  summary(sens.vol1)$coefficients[2, 4]
    financialmod[64, 6] <- summary(sens.vol1)$coefficients[2, 4]
    
sens.vol2 <- lm(TICs.m2, data = sensdata[sensdata$avg90income.class==2, ])
  round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
    financialmod[65, 3:5] <-round(c(coefficients(sens.vol2)["sd_incvol"], confint(sens.vol2)["sd_incvol", ]), 5)
  summary(sens.vol2)$coefficients[2, 4]
    financialmod[65, 6] <- summary(sens.vol2)$coefficients[2, 4]
    
sens.vol3 <- lm(TICs.m3, data = sensdata[sensdata$avg90income.class==2, ])
  round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
    financialmod[66, 3:5] <-round(c(coefficients(sens.vol3)["sd_incvol"], confint(sens.vol3)["sd_incvol", ]), 5)
  summary(sens.vol3)$coefficients[2, 4]
    financialmod[66, 6] <- summary(sens.vol3)$coefficients[2, 4]
    
    
#output results table
  write.csv(financialmod,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/financialmod_rawscores.csv")
 
