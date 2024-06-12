#load libraries from main file 

wt.table<-read.table("/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/customweight_nlsy79.dat")
names(wt.table)[1]<-"case.ID"
names(wt.table)[2]<-"sample.wt"

load(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.rda")

sensdata<-left_join(maindata, wt.table, by="case.ID")

#sample weight table

summary(sensdata$sample.wt)
sensdata$norm.samp.wt<-sensdata$sample.wt/mean(sensdata$sample.wt)
summary(sensdata$norm.samp.wt)

samplewts<-as.data.frame(matrix(nrow=36,ncol=5))
samplewts[,1]<-c("rawtotscore_M1","rawtotscore_M2","rawtotscore_M3",
                "ztotscore_M1","ztotscore_M2","ztotscore_M3",
                "rawtotscore_M1_1drop","rawtotscore_M1_2drop","rawtotscore_M1_3drop",
                "rawtotscore_M2_1drop","rawtotscore_M2_2drop","rawtotscore_M2_3drop",
                "rawtotscore_M3_1drop,","rawtotscore_M3_2drop","rawtotscore_M3_3drop",
                "ztotscore_M1_1drop","ztotscore_M1_2drop","ztotscore_M1_3drop",
                "ztotscore_M2_1drop","ztotscore_M2_2drop","ztotscore_M2_3drop",
                "ztotscore_M3_1drop,","ztotscore_M3_2drop","ztotscore_M3_3drop",
                "vol_m1","vol_m2","vol_m3",
                "1drop_m1","2drop_m1","3drop_m1",
                "1drop_m2","2drop_m2","3drop_m2",
                "1drop_m3","2drop_m3","3drop_m3")
names(samplewts)<-c("model","beta","lci","uci","pval")   

#income volatility (SD of pct change over time)
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(TICs.m1,data=sensdata, weights=norm.samp.wt)
round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
samplewts[1,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
summary(model1.vol)$coefficients[2,4]
samplewts[1,5]<-summary(model1.vol)$coefficients[2,4]

#set.seed(12345)
#lboot<-lm.boot(lm(TICs.m1, data=sensdata, weights=norm.samp.wt), R=5000, rows = TRUE) # number of replications must be larger than # of rows of data
#perc.lm(lboot,c(0.025, 0.975))[c(3,4)] #bootstrapped CIs (should be similar to those from the model since outcome is ~ normal)

model2.vol<-lm(TICs.m2, data=sensdata, weights=norm.samp.wt)
round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
samplewts[2,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
summary(model2.vol)$coefficients[2,4]
samplewts[2,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(TICs.m3, data=sensdata, weights=norm.samp.wt)
round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
samplewts[3,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
summary(model3.vol)$coefficients[2,4]
samplewts[3,5]<-summary(model3.vol)$coefficients[2,4]

#for global scores as outcomes, model-based confidence intervals and boostrapped CI are super similar, so just use model-based (faster and bootstrapping probably unnecessarily overly conservative)  
#model 3,  model-based 95% ci: -0.0131 -0.0029 
#model 3, bootstrapped 95% ci: -0.0134 -0.0027 

#income volatility (SD of pct change over time)
zTICs.m1<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.m2<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.m3<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-lm(zTICs.m1,data=sensdata, weights=norm.samp.wt)
round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
samplewts[4,2:4]<-round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),5)
summary(model1.vol)$coefficients[2,4]
samplewts[4,5]<-summary(model1.vol)$coefficients[2,4]

model2.vol<-lm(zTICs.m2, data=sensdata, weights=norm.samp.wt)
round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),4)
samplewts[5,2:4]<-round(c(coefficients(model2.vol)["sd_incvol"],confint(model2.vol)["sd_incvol",]),5)
summary(model2.vol)$coefficients[2,4]
samplewts[5,5]<-summary(model2.vol)$coefficients[2,4]

model3.vol<-lm(zTICs.m3, data=sensdata, weights=norm.samp.wt)
round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),4)
samplewts[6,2:4]<-round(c(coefficients(model3.vol)["sd_incvol"],confint(model3.vol)["sd_incvol",]),5)
summary(model3.vol)$coefficients[2,4]
samplewts[6,5]<-summary(model3.vol)$coefficients[2,4]


#income drops
TICs.id.m1<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.id.m2<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.id.m3<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(TICs.id.m1, data=sensdata, weights=norm.samp.wt)
round(coefficients(model1.drops)[2:4],3)
samplewts[7:9,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
samplewts[7:9,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
samplewts[7:9,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(TICs.id.m2, data=sensdata, weights=norm.samp.wt)
round(coefficients(model2.drops)[2:4],3)
samplewts[10:12,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
samplewts[10:12,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
samplewts[10:12,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(TICs.id.m3, data=sensdata, weights=norm.samp.wt)
round(coefficients(model3.drops)[2:4],3)
samplewts[13:15,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
samplewts[13:15,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
samplewts[13:15,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#income drops
zTICs.id.m1<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.id.m2<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.id.m3<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-lm(zTICs.id.m1, data=sensdata, weights=norm.samp.wt)
round(coefficients(model1.drops)[2:4],3)
samplewts[16:18,2]<-round(coefficients(model1.drops)[2:4],3)
round(confint(model1.drops)[2:4,],3)
samplewts[16:18,3:4]<-round(confint(model1.drops)[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
samplewts[16:18,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-lm(zTICs.id.m2, data=sensdata, weights=norm.samp.wt)
round(coefficients(model2.drops)[2:4],3)
samplewts[19:21,2]<-round(coefficients(model2.drops)[2:4],3)
round(confint(model2.drops)[2:4,],3)
samplewts[19:21,3:4]<-round(confint(model2.drops)[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
samplewts[19:21,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-lm(zTICs.id.m3, data=sensdata, weights=norm.samp.wt)
round(coefficients(model3.drops)[2:4],3)
samplewts[22:24,2]<-round(coefficients(model3.drops)[2:4],3)
round(confint(model3.drops)[2:4,],3)
samplewts[22:24,3:4]<-round(confint(model3.drops)[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
samplewts[22:24,5]<-summary(model3.drops)$coefficients[c(2:4),4]


#SD of % change income over time
sdCIND.m1<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
sdCIND.m2<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
sdCIND.m3<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-glm(sdCIND.m1, family=binomial(link='logit'), data=sensdata, weights=norm.samp.wt)
round(exp(coefficients(model1.drops))[2],3)
samplewts[25,2]<-round(exp(coefficients(model1.drops))[2],3)
round(exp(confint(model1.drops))[2,],3)
samplewts[25,3:4]<-round(exp(confint(model1.drops))[2,],3)
samplewts[25,5]<-summary(model1.drops)$coefficients[2,4]

model2.drops<-glm(sdCIND.m2, family=binomial, data=sensdata, weights=norm.samp.wt)
round(exp(coefficients(model2.drops))[2],3)
samplewts[26,2]<-round(exp(coefficients(model2.drops))[2],3)
round(exp(confint(model2.drops))[2,],3)
samplewts[26,3:4]<-round(exp(confint(model2.drops))[2,],3)
samplewts[26,5]<-summary(model2.drops)$coefficients[2,4]

model3.drops<-glm(sdCIND.m3, family=binomial, data=sensdata, weights=norm.samp.wt)
round(exp(coefficients(model3.drops))[2],3)
samplewts[27,2]<-round(exp(coefficients(model3.drops))[2],3)
round(exp(confint(model3.drops))[2,],3)
samplewts[27,3:4]<-round(exp(confint(model3.drops))[2,],3)
samplewts[27,5]<-summary(model3.drops)$coefficients[2,4]

####
#number of drops  
CIND.m1<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
CIND.m2<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
CIND.m3<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-glm(CIND.m1, family=binomial, data=sensdata, weights=norm.samp.wt)
round(exp(coefficients(model1.drops))[2:4],3)
samplewts[28:30,2]<-round(exp(coefficients(model1.drops))[2:4],3)
round(exp(confint(model1.drops))[2:4,],3)
samplewts[28:30,3:4]<-round(exp(confint(model1.drops))[2:4,],3)
summary(model1.drops)$coefficients[c(2:4),4]
samplewts[28:30,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-glm(CIND.m2, family=binomial, data=sensdata, weights=norm.samp.wt)
round(exp(coefficients(model2.drops))[2:4],3)
samplewts[31:33,2]<-round(exp(coefficients(model2.drops))[2:4],3)
round(exp(confint(model2.drops))[2:4,],3)
samplewts[31:33,3:4]<-round(exp(confint(model2.drops))[2:4,],3)
summary(model2.drops)$coefficients[c(2:4),4]
samplewts[31:33,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-glm(CIND.m3, family=binomial, data=sensdata, weights=norm.samp.wt)
round(exp(coefficients(model3.drops))[2:4],3)
samplewts[34:36,2]<-round(exp(coefficients(model3.drops))[2:4],3)
round(exp(confint(model3.drops))[2:4,],3)
samplewts[34:36,3:4]<-round(exp(confint(model3.drops))[2:4,],3)
summary(model3.drops)$coefficients[c(2:4),4]
samplewts[34:36,5]<-summary(model3.drops)$coefficients[c(2:4),4]

write.csv(samplewts,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/results_custom_samplewts")

