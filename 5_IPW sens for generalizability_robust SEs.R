#IPW 

#run libraries from main file

load(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/maindata.rda")
load(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/nlsy.1990_sample.rda")
load(file="/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/NLSYINCOMEVOL/nlsy.1990.3inc_sample.rda")

analytic<-maindata
analytic$inanalytic<-1
analytic<-analytic[,c("case.ID","inanalytic","sd_incvol","TICSm.score","TICSm.zscore","cogimpair2")]

#target<-nlsy.1990
target<-nlsyincvol
names(target)[names(target) == "sd_incvol"] <- "sd_incvol_target"
target<-left_join(target, analytic, by="case.ID")
target$inanalytic[is.na(target$inanalytic)]<-0

covariates <- target %>%
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
    empstat2 = case_when(lbrf.90 == "Work FT" ~ "Works full-time",
                         lbrf.90 %in% c("Work PT","Unemployed","Not in LbrF","Disabled") ~ "Does not work full-time")
  )

#Create a Descriptive Table 1
label(covariates$age.1990)  <- "Age (years)"
label(covariates$sex) <- "Sex"
label(covariates$raceth) <- "Race/ethnicity"
label(covariates$HSeduc) <- "Years of education"
label(covariates$married) <- "Marital status"
label(covariates$hhsize.1990) <- "Household size"
label(covariates$BMI1990.ht1985) <- "Body mass index"
label(covariates$everhbp) <- "Doctor ever diagnosed with high blood pressure"
label(covariates$bpmeds2010) <- "Currently taking blood pressure medication (2010)"
label(covariates$everhichol) <- "Doctor ever diagnosed with high cholesterol"
label(covariates$everdiab) <- "Doctor ever diagnosed with diabetes"
label(covariates$smokstat92) <- "Smoking status (1992)"
label(covariates$depressed92) <- "CESD-20 score (1992)"
label(covariates$vigPA.2010) <- "Participates in vigorous exercise"
label(covariates$lightPA.2010) <- "Participates in light/moderate exercise"
label(covariates$income90.1990) <- "Net family income"
label(covariates$empstat) <- "Employment status"
label(covariates$ephealthins) <- "Health insurance type"
label(covariates$wealth90.1990) <- "Family net wealth (dollars)"
label(covariates$heavydrink) <- "Drinking status"
label(covariates$AFQTpctlscore06rev.81) <- "Baseline cognition (1980)"
label(covariates$empstat2) <- "Employment status (collapsed)"

target<-covariates

table(target$inanalytic)

#to double check identical to H&R way, also model prob of being censored (then 1-prob in weight calculation)
target$censored<-ifelse(target$inanalytic==1,0,1)
  table(target$censored) #okay, reverse coded

#what about missingness on predictors? try to minimize so selection model is close to target sample as possible or even do mean imputation within strata
summary(target$AFQTpctlscore06rev.81.r) #289 missing
summary(target$sex) #0 missing
summary(target$raceth) #0 missing
summary(target$sd_incvol_target) #0 missing
summary(target$age.1990) #0 missing
summary(target$HSeduc) #122 missing
summary(target$married) #1 missing
summary(target$BMI1990.ht1985) #226 missing - REMOVE FROM WEIGHT MODEL (NOT A SIG PREDICTOR OF BEING IN ANALYTIC SAMPLE ANYWAY)
summary(target$smokstat92) #723 missing - REMOVE FROM WEIGHT MODEL (NOT A SIG PREDICTOR OF BEING IN ANALYTIC SAMPLE ANYWAY)
summary(target$depressed92) #160 missing - REMOVE FROM WEIGHT MODEL (NOT A SIG PREDICTOR OF BEING IN ANALYTIC SAMPLE ANYWAY)
summary(target$ephealthins) #3 missing
summary(target$heavydrink) #751 missing - REMOVE FROM WEIGHT MODEL (NOT A SIG PREDICTOR OF BEING IN ANALYTIC SAMPLE ANYWAY)
summary(target$eqincome90.1990) #0 missing
table(target$empstat2,useNA="ifany") #0 missing

#create model for selection weights (ipcw) - use all covariates from final model - ~ 1500 people not used because of missingness on a covariate 
  #denom.cens<-glm(censored~sd_incvol_target+AFQTpctlscore06rev.81.r + sd_incvol_target*AFQTpctlscore06rev.81.r + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + ephealthins + eqincome90.1990 + smokstat92 + depressed92 + heavydrink + empstat2, family = binomial(link = "logit"), data=target)

#Compare results when using a reduced predictor model where only ~400 people are not used because of missing covariates
  denom.cens<-glm(censored~sd_incvol_target + AFQTpctlscore06rev.81.r + sd_incvol_target*AFQTpctlscore06rev.81.r + age.1990 + sex + raceth + HSeduc + ephealthins + empstat2, family = binomial(link = "logit"), data=target)
    summary(denom.cens)
  target$pd.cens <- 1-predict(denom.cens, newdata=target, type = "response")
    summary(target$pd.cens)  
  
#for numerator of stabilized weights 
  numer.cens<-glm(censored~sd_incvol_target, family = binomial(link = "logit"), data=target)
    summary(numer.cens)
  target$pn.cens <- 1-predict(numer.cens, newdata=target, type = "response")
    summary(target$pn.cens)  

#create stabilized weight (no truncation)
  target$stab.wt <- target$pn.cens/target$pd.cens
    summary(target$stab.wt)

#compare to modeling prob of being a complete case/in analytic sample
denom.cc<-glm(inanalytic~sd_incvol_target + AFQTpctlscore06rev.81.r + sd_incvol_target*AFQTpctlscore06rev.81.r + age.1990 + sex + raceth + HSeduc + ephealthins + empstat2, family = binomial(link = "logit"), data=target)
target$pd.cc <- predict(denom.cc, newdata=target, type = "response")
  summary(target$pd.cens)
  summary(target$pd.cc) #should be identical - awesome, it is.
  
numer.cc<-glm(inanalytic~sd_incvol_target, family = binomial(link = "logit"), data=target)
target$pn.cc <- predict(numer.cc, newdata=target, type = "response")
  summary(target$pn.cens)  
  summary(target$pn.cc) #should be identical - great, it is.  
  
target$stab.wt <- target$pn.cens/target$pd.cens
  summary(target$stab.wt)
  
#what is the distribution of weights among complete cases?
  #do we truncate in the CC sample or before restricting to CC sample?
  summary(target$stab.wt[target$inanalytic==1])

#TRUNCATE IN THE COMPLETE CASE SAMPLE
  
target2<-target[,c("case.ID","stab.wt","stab.wt","sd_incvol_target")]


sensdata<-left_join(maindata, target2, by="case.ID")

sensdata$stab.wt.trunc<-ifelse(sensdata$stab.wt>=(quantile(sensdata$stab.wt, 0.99, na.rm=TRUE)), NA, 
                         ifelse(sensdata$stab.wt<=(quantile(sensdata$stab.wt, 0.01, na.rm=TRUE)), NA, sensdata$stab.wt))

summary(sensdata$stab.wt.trunc)

#run analyses in complete case sample with weights and using robust SE

hist(sensdata$stab.wt) #need to trim at 99th percentile?
hist(sensdata$stab.wt.trunc) #looks little better

#use truncated weights

ipwtable<-as.data.frame(matrix(nrow=36,ncol=5))
ipwtable[,1]<-c("rawtotscore_M1","rawtotscore_M2","rawtotscore_M3",
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
names(ipwtable)<-c("model","beta","lci","uci","pval")   

library(geepack)
library(broom)

#write function for gee confint
confint.geeglm <- function(object, parm, level = 0.95, ...) {
  cc <- coef(summary(object))
  mult <- qnorm((1+level)/2)
  citab <- with(as.data.frame(cc),
                cbind(lwr=Estimate-mult*Std.err,
                      upr=Estimate+mult*Std.err))
  rownames(citab) <- rownames(cc)
  citab[parm,]
}

#income volatility (SD of pct change over time)
TICs.m1<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.m2<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.m3<-TICSm.score ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

#no robust se
model1.vol<-lm(TICs.m1, data=sensdata, weights=stab.wt.trunc)
summary(model1.vol)
round(c(coefficients(model1.vol)["sd_incvol"],confint(model1.vol)["sd_incvol",]),4)
summary(model1.vol)$coefficients[2,2] #0.0774 = SE without robust SE

#Here down is code with robust

#with robust SE - should do this way for IPW
model1.vol<-geeglm(TICs.m1, id=case.ID, corstr="independence", data=sensdata, weights=stab.wt.trunc)
coef(summary(model1.vol))[2,2] #0.113 = SE with robust SE -  yes, bigger! do this way
  ipwtable[1,2]<-coef(summary(model1.vol))[2,1] 
  ipwtable[1,3:4]<-confint(model1.vol)[2,1:2]
  ipwtable[1,5]<-coef(summary(model1.vol))[2,4] 

model2.vol<-geeglm(TICs.m2, id=case.ID, corstr="independence", data=sensdata, weights=stab.wt.trunc)
  ipwtable[2,2]<-coef(summary(model2.vol))[2,1] 
  ipwtable[2,3:4]<-confint(model2.vol)[2,1:2]
  ipwtable[2,5]<-coef(summary(model2.vol))[2,4] 

model3.vol<-geeglm(TICs.m3, id=case.ID, corstr="independence", data=sensdata, weights=stab.wt.trunc)
  ipwtable[3,2]<-coef(summary(model3.vol))[2,1] 
  ipwtable[3,3:4]<-confint(model3.vol)[2,1:2]
  ipwtable[3,5]<-coef(summary(model3.vol))[2,4] 

#income volatility (SD of pct change over time)
zTICs.m1<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.m2<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.m3<-TICSm.zscore ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.vol<-geeglm(zTICs.m1, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  ipwtable[4,2]<-coef(summary(model1.vol))[2,1] 
  ipwtable[4,3:4]<-confint(model1.vol)[2,1:2]
  ipwtable[4,5]<-coef(summary(model1.vol))[2,4] 

model2.vol<-geeglm(zTICs.m2, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  ipwtable[5,2]<-coef(summary(model2.vol))[2,1] 
  ipwtable[5,3:4]<-confint(model2.vol)[2,1:2]
  ipwtable[5,5]<-coef(summary(model2.vol))[2,4] 

model3.vol<-geeglm(zTICs.m3, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  ipwtable[6,2]<-coef(summary(model3.vol))[2,1] 
  ipwtable[6,3:4]<-confint(model3.vol)[2,1:2]
  ipwtable[6,5]<-coef(summary(model3.vol))[2,4] 

#income drops
TICs.id.m1<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
TICs.id.m2<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
TICs.id.m3<-TICSm.score ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-geeglm(TICs.id.m1, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  round(coefficients(model1.drops)[2:4],3)
  ipwtable[7:9,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
  ipwtable[7:9,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
  ipwtable[7:9,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-geeglm(TICs.id.m2, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  round(coefficients(model2.drops)[2:4],3)
  ipwtable[10:12,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
  ipwtable[10:12,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
  ipwtable[10:12,5]<-summary(model2.drops)$coefficients[c(2:4),4]
  
model3.drops<-geeglm(TICs.id.m3, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  round(coefficients(model3.drops)[2:4],3)
  ipwtable[13:15,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
  ipwtable[13:15,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
  ipwtable[13:15,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#income drops
zTICs.id.m1<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
zTICs.id.m2<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
zTICs.id.m3<-TICSm.zscore ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-geeglm(zTICs.id.m1, id=case.ID, data=sensdata, weights=stab.wt.trunc)
  round(coefficients(model1.drops)[2:4],3)
  ipwtable[16:18,2]<-round(coefficients(model1.drops)[2:4],3)
  round(confint(model1.drops)[2:4,],3)
  ipwtable[16:18,3:4]<-round(confint(model1.drops)[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
  ipwtable[16:18,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-geeglm(zTICs.id.m2, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  round(coefficients(model2.drops)[2:4],3)
  ipwtable[19:21,2]<-round(coefficients(model2.drops)[2:4],3)
  round(confint(model2.drops)[2:4,],3)
  ipwtable[19:21,3:4]<-round(confint(model2.drops)[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
  ipwtable[19:21,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-geeglm(zTICs.id.m3, id=case.ID,data=sensdata, weights=stab.wt.trunc)
  round(coefficients(model3.drops)[2:4],3)
  ipwtable[22:24,2]<-round(coefficients(model3.drops)[2:4],3)
  round(confint(model3.drops)[2:4,],3)
  ipwtable[22:24,3:4]<-round(confint(model3.drops)[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
  ipwtable[22:24,5]<-summary(model3.drops)$coefficients[c(2:4),4]


#SD of % change income over time
sdCIND.m1<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
sdCIND.m2<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
sdCIND.m3<-cogimpair2 ~ sd_incvol + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-glm(sdCIND.m1, family=binomial(link='logit'), data=sensdata, weights=stab.wt.trunc)
summary(model1.drops) #SE=0.0827

model1.drops<-geeglm(sdCIND.m1, id=case.ID, family=binomial(link='logit'), data=sensdata, weights=stab.wt.trunc)
  summary(model1.drops) #SE=0.0860 - bigger SE (slightly - CI will be similar)
  round(exp(coefficients(model1.drops))[2],3)
  ipwtable[25,2]<-round(exp(coefficients(model1.drops))[2],3)
  round(exp(confint(model1.drops))[2,],3)
  ipwtable[25,3:4]<-round(exp(confint(model1.drops))[2,],3)
  ipwtable[25,5]<-summary(model1.drops)$coefficients[2,4]

model2.drops<-geeglm(sdCIND.m2, id=case.ID, family=binomial, data=sensdata, weights=stab.wt.trunc)
  round(exp(coefficients(model2.drops))[2],3)
  ipwtable[26,2]<-round(exp(coefficients(model2.drops))[2],3)
  round(exp(confint(model2.drops))[2,],3)
  ipwtable[26,3:4]<-round(exp(confint(model2.drops))[2,],3)
  ipwtable[26,5]<-summary(model2.drops)$coefficients[2,4]

model3.drops<-geeglm(sdCIND.m3, id=case.ID, family=binomial, data=sensdata, weights=stab.wt.trunc)
  round(exp(coefficients(model3.drops))[2],3)
  ipwtable[27,2]<-round(exp(coefficients(model3.drops))[2],3)
  round(exp(confint(model3.drops))[2,],3)
  ipwtable[27,3:4]<-round(exp(confint(model3.drops))[2,],3)
  ipwtable[27,5]<-summary(model3.drops)$coefficients[2,4]

####
#number of drops  
CIND.m1<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married
CIND.m2<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink #these can be baseline proxies for health status?
CIND.m3<-cogimpair2 ~ num.drops4 + CogTestYear + AFQTpctlscore06rev.81.r + sumincomes + age.1990 + sex + raceth + HSeduc + married + BMI1990.ht1985 + smokstat92 + depressed92 + ephealthins + heavydrink + eqincome90.1990 + empstat2

model1.drops<-geeglm(CIND.m1, id=case.ID, family=binomial, data=sensdata, weights=stab.wt.trunc)
  round(exp(coefficients(model1.drops))[2:4],3)
  ipwtable[28:30,2]<-round(exp(coefficients(model1.drops))[2:4],3)
  round(exp(confint(model1.drops))[2:4,],3)
  ipwtable[28:30,3:4]<-round(exp(confint(model1.drops))[2:4,],3)
  summary(model1.drops)$coefficients[c(2:4),4]
  ipwtable[28:30,5]<-summary(model1.drops)$coefficients[c(2:4),4]

model2.drops<-geeglm(CIND.m2, id=case.ID, family=binomial, data=sensdata, weights=stab.wt.trunc)
  round(exp(coefficients(model2.drops))[2:4],3)
  ipwtable[31:33,2]<-round(exp(coefficients(model2.drops))[2:4],3)
  round(exp(confint(model2.drops))[2:4,],3)
  ipwtable[31:33,3:4]<-round(exp(confint(model2.drops))[2:4,],3)
  summary(model2.drops)$coefficients[c(2:4),4]
  ipwtable[31:33,5]<-summary(model2.drops)$coefficients[c(2:4),4]

model3.drops<-geeglm(CIND.m3, id=case.ID, family=binomial, data=sensdata, weights=stab.wt.trunc)
  round(exp(coefficients(model3.drops))[2:4],3)
  ipwtable[34:36,2]<-round(exp(coefficients(model3.drops))[2:4],3)
  round(exp(confint(model3.drops))[2:4,],3)
  ipwtable[34:36,3:4]<-round(exp(confint(model3.drops))[2:4,],3)
  summary(model3.drops)$coefficients[c(2:4),4]
  ipwtable[34:36,5]<-summary(model3.drops)$coefficients[c(2:4),4]

#write.csv(ipwtable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/IPCW_sens_results.csv")
#write.csv(ipwtable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/IPCW_sens_results_notrunc.csv")
write.csv(ipwtable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/stab.wt_sens_results_trunc_robustSE.csv")
#write.csv(ipwtable,"/Users/xxxx/Pooling Projects/Income Pooling/NLSY replicate Grasset et al/results/stab.wt_sens_results_notrunc_robustSE.csv")

