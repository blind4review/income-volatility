#Sensitivity analysis using sd_incvol.i.i as exposure variable (which does carry forward/backward imputation (same result so calling it either is fine))

load(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.rda")

sensdata<-maindata
head(sensdata)
summary(sensdata$sd_incvol.i)
summary(sensdata$sd_pct.i)/29.96989
z<-round(sd(sensdata$sd_pct.i),6)

#----------------------------------------------------------------------------------------------------------------------------------

#create empty dataframes to store results
i.incvoltable<-as.data.frame(matrix(nrow=30,ncol=5))
i.incvoltable[,1]<-c("rawtotscore_M1","rawtotscore_M2","rawtotscore_M3",
                   "ztotscore_M1","ztotscore_M2","ztotscore_M3",
                   "rawmemscore_M1","rawmemscore_M2","rawmemscore_M3",
                   "zmemscore_M1","zmemscore_M2","zmemscore_M3",
                   "rawattnscore_M1","rawattnscore_M2","rawattnscore_M3",
                   "zattnscore_M1","zattnscore_M2","zattnscore_M3",
                   "zimmscore_M1","zimmscore_M2","zimmscore_M3",
                   "zdelscore_M1","zdelscore_M2","zdelscore_M3",
                   "zbcscore_M1","zbcscore_M2","zbcscore_M3",
                   "zs7score_M1","zs7score_M2","zs7score_M3")
names(i.incvoltable)<-c("model","beta","lci","uci","pval")                   

i.incdroptable<-as.data.frame(matrix(nrow=90,ncol=5))
names(i.incdroptable)<-c("model","beta","lci","uci","pval")                   
i.incdroptable[,1]<-c("rawtotscore_M1_1drop","rawtotscore_M1_2drop","rawtotscore_M1_3drop",
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
TICs.m1<-TICSm.score ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(TICs.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[1,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[1,5]<-summary(model1.vol)$coefficients[2,4]

#set.seed(12345)
#lboot<-lm.boot(lm(TICs.m1, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
#perc.lm(lboot,c(0.025, 0.975))[c(3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)

model2.vol<-lm(TICs.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[2,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[2,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(TICs.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[3,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[3,5]<-summary(model3.vol)$coefficients[2,4]

#for global scores as outcomes, model-based confidence intervals and boostrapped CI are super similar, so just use model-based (faster and bootstrapping probably unnecessarily overly conservative)  
#model 3,  model-based 95% ci: -0.0131 -0.0029 
#model 3, bootstrapped 95% ci: -0.0134 -0.0027 

#####
#income drops
TICs.id.m1<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.id.m2<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.id.m3<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(TICs.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[1:3,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[1:3,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[1:3,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(TICs.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[4:6,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[4:6,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[4:6,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(TICs.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[7:9,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[7:9,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[7:9,5]<-summary(model3.drops)$coefficients[c(2:4),4]

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
zTICs.m1<-TICSm.zscore ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.m2<-TICSm.zscore ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.m3<-TICSm.zscore ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(zTICs.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[4,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[4,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(zTICs.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[5,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[5,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(zTICs.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[6,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[6,5]<-summary(model3.vol)$coefficients[2,4]


#####
#income drops
zTICs.id.m1<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.id.m2<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.id.m3<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(zTICs.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[10:12,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[10:12,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[10:12,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zTICs.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[13:15,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[13:15,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[13:15,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zTICs.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[16:18,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[16:18,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[16:18,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------  
#(c).memory domain score (raw)

#####
#income volatility (SD of pct change over time)
hist(maindata$memscore.raw) #fairly normal - use model-based conf intervals (e.g., model 1: -0.01046 -0.00233)
hist(maindata$memscore.z) #fairly normal - use model-based conf intervals (e.g., model 1: -0.0106 -0.00209)

mem.m1<-memscore.raw ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
mem.m2<-memscore.raw ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
mem.m3<-memscore.raw ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(mem.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[7,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[7,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(mem.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[8,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[8,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(mem.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[9,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[9,5]<-summary(model3.vol)$coefficients[2,4]

#####
#income drops
mem.id.m1<-memscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
mem.id.m2<-memscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
mem.id.m3<-memscore.raw ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(mem.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[19:21,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[19:21,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[19:21,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(mem.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[22:24,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[22:24,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[22:24,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(mem.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[25:27,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[25:27,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[25:27,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------
#-----------------
#(d). Memory domain score (standardized)

#####
#income volatility (SD of pct change over time)
zmem.m1<-memscore.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zmem.m2<-memscore.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zmem.m3<-memscore.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(zmem.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[10,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[10,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(zmem.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[11,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[11,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(zmem.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[12,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[12,5]<-summary(model3.vol)$coefficients[2,4]  

#####
#income drops
zmem.id.m1<-memscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zmem.id.m2<-memscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zmem.id.m3<-memscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(zmem.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[28:30,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[28:30,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[28:30,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zmem.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[31:33,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[31:33,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[31:33,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zmem.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[34:36,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[34:36,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[34:36,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------  
#-----------------  
#(e). attention domain score (raw)

#####
#income volatility (SD of pct change over time)
attn.m1<-attnscore.raw ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
attn.m2<-attnscore.raw ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
attn.m3<-attnscore.raw ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

hist(maindata$attnscore.raw) #model-based ci and boostrapped are very similar (just continue with model-based)
hist(maindata$attnscore.z)

model1.vol<-lm(attn.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[13,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[13,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(attn.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[14,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[14,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(attn.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[15,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[15,5]<-summary(model3.vol)$coefficients[2,4]  

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
i.incdroptable[37:39,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[37:39,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[37:39,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(attn.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[40:42,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[40:42,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[40:42,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(attn.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[43:45,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[43:45,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[43:45,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------
#-----------------
#(f). attention domain score (standardized)

#####
#income volatility (SD of pct change over time)
zattn.m1<-attnscore.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zattn.m2<-attnscore.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zattn.m3<-attnscore.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

hist(maindata$attnscore.z)

model1.vol<-lm(zattn.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[16,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[16,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(zattn.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[17,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[17,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(zattn.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[18,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[18,5]<-summary(model3.vol)$coefficients[2,4]  

#model 3,  model-based ci: -0.0027 -0.0002 ** use model-based then, since consistent
#model 3, bootstrapped ci: -0.00274 -0.00015

#####
#income drops
zattn.id.m1<-attnscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zattn.id.m2<-attnscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zattn.id.m3<-attnscore.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(zattn.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[46:48,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[46:48,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[46:48,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zattn.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[49:51,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[49:51,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[49:51,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zattn.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[52:54,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[52:54,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[52:54,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#-----------------  
#-----------------  
#(g). cognitive impairment categories (logistic regression models)

#create new results table for cog impairment
i.cogimpair<-as.data.frame(matrix(nrow=12,ncol=5))
names(i.cogimpair)<-c("model","or","lci","uci","pval")
i.cogimpair[,1]<-c("vol_m1","vol_m2","vol_m3","1drop_m1","2drop_m1","3drop_m1","1drop_m2","2drop_m2","3drop_m2","1drop_m3","2drop_m3","3drop_m3")
####
#SD of % change income over time
sdCIND.m1<-cogimpair2 ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
sdCIND.m2<-cogimpair2 ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
sdCIND.m3<-cogimpair2 ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-glm(sdCIND.m1, family=binomial, data=maindata)
round(exp(coefficients(model1.drops))[2],3)
i.cogimpair[1,2]<-round(exp(coefficients(model1.drops))[2],3)
round(exp(confint(model1.drops))[2,],3)
i.cogimpair[1,3:4]<-round(exp(confint(model1.drops))[2,],3)
i.cogimpair[1,5]<-summary(model1.drops)$coefficients[2,4]

model2.drops<-glm(sdCIND.m2, family=binomial, data=maindata)
round(exp(coefficients(model2.drops))[2],3)
i.cogimpair[2,2]<-round(exp(coefficients(model2.drops))[2],3)
round(exp(confint(model2.drops))[2,],3)
i.cogimpair[2,3:4]<-round(exp(confint(model2.drops))[2,],3)
i.cogimpair[2,5]<-summary(model2.drops)$coefficients[2,4]

model3.drops<-glm(sdCIND.m3, family=binomial, data=maindata)
round(exp(coefficients(model3.drops))[2],3)
i.cogimpair[3,2]<-round(exp(coefficients(model3.drops))[2],3)
round(exp(confint(model3.drops))[2,],3)
i.cogimpair[3,3:4]<-round(exp(confint(model3.drops))[2,],3)
i.cogimpair[3,5]<-summary(model3.drops)$coefficients[2,4]

####
#number of drops  
CIND.m1<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
CIND.m2<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
CIND.m3<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-glm(CIND.m1, family=binomial, data=maindata)
round(exp(coefficients(model1.drops))[2:4],3)
i.cogimpair[4:6,2]<-round(exp(coefficients(model1.drops))[2:4],3)
round(exp(confint(model1.drops))[2:4,],3)
i.cogimpair[4:6,3:4]<-round(exp(confint(model1.drops))[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.cogimpair[4:6,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-glm(CIND.m2, family=binomial, data=maindata)
round(exp(coefficients(model2.drops))[2:4],3)
i.cogimpair[7:9,2]<-round(exp(coefficients(model2.drops))[2:4],3)
round(exp(confint(model2.drops))[2:4,],3)
i.cogimpair[7:9,3:4]<-round(exp(confint(model2.drops))[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.cogimpair[7:9,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-glm(CIND.m3, family=binomial, data=maindata)
round(exp(coefficients(model3.drops))[2:4],3)
i.cogimpair[10:12,2]<-round(exp(coefficients(model3.drops))[2:4],3)
round(exp(confint(model3.drops))[2:4,],3)
i.cogimpair[10:12,3:4]<-round(exp(confint(model3.drops))[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.cogimpair[10:12,5]<-summary(model3.drops)$coefficients[c(2:4),4]


################################################################################################ 
################################################################################################  
################################################################################################
#individual assessments as outcomes (sensitivity analysis)  
#z-scores so they can be compared with each other 

#-----------------  
#(sens i).immediate recall - raw score
hist(maindata$immedrecall) #~normally distributed - linear regression
hist(maindata$sd_incvol.i) #do we want to transform at all? 

#IR.m1<-immedrecall ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
#IR.m2<-immedrecall ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
#IR.m3<-immedrecall ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

IR.m1<-IR.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
IR.m2<-IR.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
IR.m3<-IR.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(IR.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[19,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[19,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(IR.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[20,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[20,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(IR.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[21,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[21,5]<-summary(model3.vol)$coefficients[2,4]  

#####
#income drops
IR.id.m1<-IR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
IR.id.m2<-IR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
IR.id.m3<-IR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(IR.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[55:57,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[55:57,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[55:57,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(IR.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[58:60,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[58:60,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[58:60,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(IR.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[61:63,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[61:63,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[61:63,5]<-summary(model3.drops)$coefficients[c(2:4),4]


#-----------------
#-----------------
#(sens ii).delayed recall
hist(maindata$DR.z) #~normally distributed - linear regression

# DR.m1<-delrecall ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
# DR.m2<-delrecall ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
# DR.m3<-delrecall ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

DR.m1<-DR.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
DR.m2<-DR.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
DR.m3<-DR.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(DR.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[22,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[22,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(DR.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[23,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[23,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(DR.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[24,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[24,5]<-summary(model3.vol)$coefficients[2,4]  


#####
#income drops
DR.id.m1<-DR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
DR.id.m2<-DR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
DR.id.m3<-DR.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(DR.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[64:66,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[64:66,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[64:66,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(DR.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[67:69,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[67:69,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[67:69,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(DR.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[70:72,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[70:72,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[70:72,5]<-summary(model3.drops)$coefficients[c(2:4),4]


#-------------
#-------------
#(sens iii). backward counting from 20
hist(maindata$BC.z) #~binary and getting test 'correct' is common 

#bc.m1<-avgBC.r ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
#bc.m2<-avgBC.r ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
#bc.m3<-avgBC.r ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

bc.m1<-BC.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
bc.m2<-BC.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
bc.m3<-BC.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(bc.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[25,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[25,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(bc.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[26,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[26,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(bc.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[27,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[27,5]<-summary(model3.vol)$coefficients[2,4]  


#####
#income drops
bc.id.m1<-BC.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
bc.id.m2<-BC.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
bc.id.m3<-BC.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(bc.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[73:75,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[73:75,3:4]<-round(confint(model1.drops)[2:4,],3)
set.seed(12345)
lboot<-lm.boot(lm(bc.id.m1, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
i.incdroptable[73:75,3:4]<-perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)][c(1,3,5,2,4,6)] 

model2.drops<-lm(bc.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[76:78,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
set.seed(12345)
lboot<-lm.boot(lm(bc.id.m2, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
i.incdroptable[76:78,3:4]<-perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)][c(1,3,5,2,4,6)] 

model3.drops<-lm(bc.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[79:81,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
set.seed(12345)
lboot<-lm.boot(lm(bc.id.m3, data=maindata), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)
i.incdroptable[79:81,3:4]<-perc.lm(lboot,c(0.025, 0.975))[,c(2,3,4)][c(1,3,5,2,4,6)] 

#-----------------
#-----------------
#(sens iv).serial 7s
hist(maindata$s7.z) #not very normally distributed...compare model-based ci and bootstrapping
#model-based are wider and both are non-significant (in model 3) so go with model-based

#s7.m1<-serial7s ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
#s7.m2<-serial7s ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
#s7.m3<-serial7s ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

s7.m1<-s7.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
s7.m2<-s7.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
s7.m3<-s7.z ~ sd_incvol.i + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(s7.m1,data=maindata)
round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
i.incvoltable[28,2:4]<-round(c(coefficients(model1.vol)["sd_incvol.i"],confint(model1.vol)["sd_incvol.i",]),5)
summary(model1.vol)$coefficients[2,4]
i.incvoltable[28,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(s7.m2, data=maindata)
round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),4)
i.incvoltable[29,2:4]<-round(c(coefficients(model2.vol)["sd_incvol.i"],confint(model2.vol)["sd_incvol.i",]),5)
summary(model2.vol)$coefficients[2,4]
i.incvoltable[29,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(s7.m3, data=maindata)
round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),4)
i.incvoltable[30,2:4]<-round(c(coefficients(model3.vol)["sd_incvol.i"],confint(model3.vol)["sd_incvol.i",]),5)
summary(model3.vol)$coefficients[2,4]
i.incvoltable[30,5]<-summary(model3.vol)$coefficients[2,4]  

#####
#income drops
s7.id.m1<-s7.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
s7.id.m2<-s7.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
s7.id.m3<-s7.z ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(s7.id.m1, data=maindata)
round(coefficients(model1.drops)[2:4],3)
i.incdroptable[82:84,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
i.incdroptable[82:84,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
i.incdroptable[82:84,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(s7.id.m2, data=maindata)
round(coefficients(model2.drops)[2:4],3)
i.incdroptable[85:87,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
i.incdroptable[85:87,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
i.incdroptable[85:87,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(s7.id.m3, data=maindata)
round(coefficients(model3.drops)[2:4],3)
i.incdroptable[88:90,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
i.incdroptable[88:90,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
i.incdroptable[88:90,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#output results table
  #write.csv(i.incvoltable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/sd_pct_impute_rawscores.csv")
  write.csv(i.incvoltable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/sd_pct_impute_zscores.csv")
  #write.csv(i.incdroptable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/numdrops_impute.csv")
  write.csv(i.cogimpair,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/i.cogimpair_impute.csv")

