rm(list=ls())
library(rio)
library(lubridate)

#Load data
setwd("/Users/tavishtran/Library/CloudStorage/OneDrive-Personal/1 - Master Degree/S2-ISM6137/Project/Data")
police <- import("Police Count Data.csv")
crime <- import("estimated_crimes_1979_2022.xlsx")
gunlaw <- import("Gunlaw.xlsx")
state <- import("State.xlsx")
demographics <- import("demographics.xlsx")

head(police)
head(crime)
head(gunlaw)
head(state)
head(demographics)

#process data
colnames(police)[which(names(police)=="data_yeArkansas")] <- "year"
colnames(police)[which(names(police)=="state_abbr")] <- "state"
colnames(state)[which(names(state)=="State_Abbr")] <- "state_abbr"
colnames(state)[which(names(state)=="State")] <- "state"

colnames(police)[which(names(police)=="male_officer_Connecticut")] <- "male_officer"
colnames(police)[which(names(police)=="male_civilian_Connecticut")] <- "male_civilian"
colnames(police)[which(names(police)=="male_total_Connecticut")] <- "male_total"
colnames(police)[which(names(police)=="female_officer_Connecticut")] <- "female_officer"
colnames(police)[which(names(police)=="female_civilian_Connecticut")] <- "female_civilian"
colnames(police)[which(names(police)=="female_total_Connecticut")] <- "female_total"
colnames(police)[which(names(police)=="officer_Connecticut")] <- "officer"
colnames(police)[which(names(police)=="civilian_Connecticut")] <- "civilian"
colnames(police)[which(names(police)=="total_pe_Connecticut")] <- "total_pe"

colnames(crime)[which(names(crime)=="state_name")] <- "state"

crime$state_abbr <- tolower(crime$state_abbr)
crime$state <- tolower(crime$state)
gunlaw$state <- tolower(gunlaw$state)
police$state <- tolower(police$state)
state$state <- tolower(state$state)
state$state_abbr <- tolower(state$state_abbr)
demographics$state <- tolower(demographics$state)

table(police[police$year >= 2012 & police$year <= 2022,]$year, police[police$year >= 2012 & police$year <= 2022,]$state) # 
# -> west virginia missed out police number in 2014
table(crime[crime$year >= 2012 & crime$year <= 2022,]$year, crime[crime$year >= 2012 & crime$year <= 2022,]$state_abbr)
# -> only Peurto Rico missed out crime date from 2017 - 2022, pr will be omitted from analysis anyway

table(gunlaw[gunlaw$year >= 2012 & gunlaw$year <= 2022,]$year, gunlaw[gunlaw$year >= 2012 & gunlaw$year <= 2022,]$state)
# -> only gunlaw data from 2012 to 2020, no DC law. 

table(demographics[demographics$year >= 2012 & demographics$year <= 2022,]$year, demographics[demographics$year >= 2012 & demographics$year <= 2022,]$state)
# -> no missing data
unique(state$state)

#clean data
#Recover wv police number in 2014 
wv_total_pe_2014 = (police[police$state == "west virginia" & police$year == 2013,"total_pe"] + police[police$state == "west virginia" & police$year == 2015,"total_pe"]) / 2
police[nrow(police)+1,c("state","year","total_pe")] = c("west virginia",2014, wv_total_pe_2014)

police <- police[,c("state","year","total_pe")]
police$year <- as.integer(police$year)
police$total_pe <- as.integer(police$total_pe)
str(police)

#remove territories, remain only 50 states + 1 Federal District
state = state[which(state$state != "virgin islands, u.s." & state$state != "guam" & state$state != "american samoa" & state$state != "district of columbia"
                    & state$state != "puerto rico" & state$state != "northern mariana islands" & state$state != "united states minor outlying islands"),] 

#create year dataframe 
year_df = data.frame(year = c(2012:2022))

#construct permutation of state and year to be analyze
state_year = merge(state,year_df,all.x = TRUE, all.y =TRUE)
rm(year_df, state,wv_total_pe_2014)

police <- merge(state_year, police, by = c("state","year"), all.x = TRUE)
gunlaw <- merge(state_year, gunlaw, by = c("state","year"), all.x = TRUE)
demographics <- merge(state_year,demographics, by = c("state","year"), all.x = TRUE)
crime <- merge(state_year,crime, by = c("state","year"), all.x = TRUE)
crime$state_abbr.x <- NULL
crime$state_abbr.y <- NULL
police$state_abbr <- NULL
demographics$state_abbr <- NULL
gunlaw$state_abbr <- NULL
gunlaw$lawtotal <- NULL

crime <- merge(crime, demographics, by = c("state","year"))
crime <- merge(crime, police, by = c("state","year"))
crime <- merge(crime, gunlaw, by = c("state","year"))

colSums(is.na(police)) #50 Missing 2022 data (50 states)
colSums(is.na(demographics)) #No NA
colSums(is.na(gunlaw)) #100 NA, Missing 2021 & 2022 data (50 states)
colSums(is.na(crime)) 

crime = na.omit(crime) #omit NA (drop 2021 and 2022 in analysis)
gunlaw = na.omit(gunlaw)



#Feature engineering
str(crime)
str(gunlaw)


# Select felony crimes only
crime.eng <- crime[,c("violent_crime","homicide")] 
crime.eng$crimetotal <- crime$violent_crime + crime$homicide + crime$rape_legacy + crime$rape_revised + crime$aggravated_assault 

# Assess seriousness of 14 legals scheme by numbers of law issued
#deal regulation
crime.eng$dealerreg <- crime$dealer + crime$dealerh + crime$recordsdealer 
#buyer regulation
crime.eng$buyerreg <- crime$waiting + crime$waitingh + crime$permit + crime$permith + 
                      crime$permitlaw + crime$fingerprint + crime$training + crime$registration + crime$registrationh +
                      crime$defactoreg + crime$defactoregh + crime$age21handgunsale + crime$age18longgunsale + 
                      crime$age21longgunsaled + crime$age21longgunsale + crime$loststolen + crime$onepermonth
#Prohibitions for high-risk gun possession
crime.eng$prohibition <- crime$felony + crime$violent + crime$violenth + crime$violentpartial + crime$invcommitment +
                      crime$invoutpatient + crime$danger + crime$drugmisdemeanor + crime$alctreatment + crime$alcoholism +
                      crime$relinquishment
#background check
crime.eng$backgroundcheck <- crime$universal + crime$universalh + crime$gunshow + crime$gunshowh + crime$universalpermit + crime$universalpermith +
                            crime$backgroundpurge + crime$threedaylimit + crime$mentalhealth + crime$statechecks + crime$statechecksh
#Ammunition regulations
crime.eng$ammunition <- crime$ammlicense + crime$ammrecords + crime$ammpermit + crime$ammrestrict + crime$amm18 + crime$amm21h + crime$ammbackground
#possession regulation
crime.eng$possessreg <- crime$age18longgunpossess + crime$age21handgunpossess + crime$age21longgunpossess + crime$gvro +
                        crime$gvrolawenforcement + crime$college + crime$collegeconcealed + crime$elementary + crime$opencarryh + crime$opencarryl +
                        crime$opencarrypermith + crime$opencarrypermitl 
#Concealed carry permitting
crime.eng$concealcarry <- crime$permitconcealed + crime$mayissue + crime$showing + crime$ccrevoke + 
                          crime$ccbackground + crime$ccbackgroundnics + crime$ccrenewbackground
#Assault weapons and large-capacity magazines
crime.eng$assaultweapon <- crime$assault + crime$onefeature + crime$assaultlist + crime$assaultregister + crime$assaulttransfer + 
                            crime$magazine + crime$tenroundlimit + crime$magazinepreowned
#Child access prevention
crime.eng$chileaccess <- crime$lockd + crime$lockp + crime$locked + crime$lockstandards + crime$capliability + crime$capaccess +
                          crime$capuses + crime$capunloaded + crime$cap14 + crime$cap16 + crime$cap18
#Gun trafficking
crime.eng$guntrafficking <- crime$traffickingbackground + crime$traffickingprohibited + crime$traffickingprohibitedh +
                            crime$strawpurchase + crime$strawpurchaseh + crime$microstamp + crime$personalized
#Preemption
crime.eng$preemption <- crime$preemption + crime$preemptionnarrow + crime$preemptionbroad
#Immunity
crime.eng$immunity <- crime$immunity 
#Domestic violence
crime.eng$domesticviolence <- crime$mcdv + crime$mcdvdating + crime$mcdvsurrender + crime$mcdvsurrendernoconditions + crime$mcdvsurrenderdating + 
                              crime$mcdvremovalallowed + crime$mcdvremovalrequired + crime$incidentremoval + crime$incidentall + 
                              crime$dvro + crime$dvrodating + crime$exparte + crime$expartedating + crime$dvrosurrender + crime$dvrosurrendernoconditions + crime$dvrosurrenderdating + 
                              crime$expartesurrender + crime$expartesurrendernoconditions + crime$expartesurrenderdating +
                              crime$dvroremoval + crime$stalking


#crime.eng$violent_crime <- NULL
#crime.eng$homicide <- NULL
crime.eng$year <- crime$year
crime.eng$state <- crime$state
crime.eng$population <- crime$Population / 1000000 # x millions of population
crime.eng$police <- crime$total_pe * 1000 / crime$Population #Police force rate per 1000 population

crime.eng$poverty <- crime$BelowPovertyLevel * 1000 / crime$Population          #Poverty rate per 1000 ppl
crime.eng$education <- crime$Lessthanhighschoolgraduate * 1000 / crime$Population #Education rate per 1000 ppl
crime.eng$crimetotal <- crime.eng$crimetotal * 1000 / crime$Population            #Total felony crime rate per 1000 ppl
crime.eng$violent_crime <- crime$violent_crime * 1000 / crime$Population          #Violent crime rate per 1000 ppl
crime.eng$homicide <- crime$homicide * 1000 / crime$Population                    #Homicide rate per 1000 ppl

str(crime.eng)

#selected laws to be analyzed
crime.law <- crime[,c("violent_crime","homicide")]
crime.law$year <- crime$year
crime.law$state <- crime$state
crime.law$population <- crime$Population / 1000000 # x millions of population
crime.law$police <- crime$total_pe * 1000 / crime$Population #Police force rate per 1000 population

crime.law$poverty <- crime$BelowPovertyLevel * 1000 / crime$Population          #Poverty rate per 1000 ppl
crime.law$education <- crime$Lessthanhighschoolgraduate * 1000 / crime$Population #Education rate per 1000 ppl
crime.law$violent_crime <- crime$violent_crime * 1000 / crime$Population          #Violent crime rate per 1000 ppl
crime.law$homicide <- crime$homicide * 1000 / crime$Population                    #Homicide rate per 1000 ppl
crime.law$dealerreg.dealerh <- crime$dealerh
crime.law$buyerreg.permitlaw <- crime$permitlaw
crime.law$buyerreg.fingerprint <- crime$fingerprint
crime.law$buyerreg.registrationh <- crime$registrationh
crime.law$buyerreg.age <- ifelse(crime$age21handgunsale + crime$age21handgunsale + crime$age18longgunsale + crime$age21longgunsaled > 0,1,0)
crime.law$prohibition.violentpartial <- ifelse(crime$felony + crime$violentpartial >0, 1, 0)
crime.law$prohibition.danger <- crime$danger
crime.law$prohibition.drugmisdemeanor <- crime$drugmisdemeanor
crime.law$prohibition.alctreatment <- crime$alctreatment
crime.law$background.universal <- ifelse(crime$universal + crime$universalh > 0, 1, 0)
crime.law$background.universalpermit <- ifelse(crime$universalpermit + crime$universalpermith >0, 1, 0)
crime.law$background.mentalhealth <- crime$mentalhealth
crime.law$ammunition.ammlicense <- crime$ammlicense
crime.law$ammunition.ammpermit <- crime$ammpermit
crime.law$ammunition.ammrestrict <- crime$ammrestrict
crime.law$ammunition.ammage <- ifelse(crime$amm18 + crime$amm21h > 0, 1, 0)
crime.law$possessreg.agerestrict <- ifelse(crime$age21handgunpossess + crime$age18longgunpossess + crime$age21longgunpossess > 0, 1, 0)
crime.law$possessreg.carry <- ifelse(crime$opencarryh + crime$opencarryl + crime$opencarrypermith + crime$opencarrypermitl> 0, 1, 0)
crime.law$conceal.permit <- crime$permitconcealed
crime.law$conceal.background <- crime$ccbackground
crime.law$assaultweapon.ban <- ifelse(crime$assault + crime$onefeature + crime$assaultlist> 0, 1, 0)
crime.law$assaultweapon.magazine <- ifelse(crime$magazine + crime$tenroundlimit + crime$magazinepreowned> 0, 1, 0)
crime.law$childaccess.lock <- ifelse(crime$lockd + crime$lockp + crime$lockstandards> 0, 1, 0)
crime.law$childaccess.storage <- ifelse(crime$locked + crime$capliability + crime$capaccess> 0, 1, 0)
crime.law$childaccess.storage <- ifelse(crime$locked + crime$capliability + crime$capaccess> 0, 1, 0)
crime.law$trafficking <- ifelse(crime$traffickingbackground + crime$traffickingprohibited + crime$traffickingprohibitedh> 0, 1, 0)
crime.law$strawpurchase <- ifelse(crime$strawpurchase + crime$strawpurchaseh > 0, 1, 0)
crime.law$microstamp <- crime$microstamp
crime.law$personalized <- crime$personalized
crime.law$preemption <- ifelse(crime$preemption + crime$preemptionnarrow + crime$preemptionbroad> 0, 1, 0)
crime.law$immunity <- crime$immunity
crime.law$domestic.misdemeanor <- ifelse(crime$mcdv + crime$mcdvsurrendernoconditions + crime$mcdvremovalallowed + crime$stalking > 0, 1, 0)
crime.law$dvro <- crime$dvro

names <- c(9:40)
crime.law[,names] <- lapply(crime.law[,names] , factor)
str(crime.law)

#Data visualization 
hist(crime.eng$crimetotal) #polinomial 
hist(crime.eng$homicide) #polinomial 
hist(crime.eng$violent_crime) #polinomial 

hist(log(crime.eng$crimetotal)) #look normalized
hist(log(crime.eng$homicide)) #look normalized
hist(log(crime.eng$violent_crime)) #look normalized

d= subset(crime.eng,crime.eng$state =='florida')
plot(d$crimetotal ~ d$year)

d= subset(crime.eng,crime.eng$state =='california')
plot(d$crimetotal ~ d$year)

subset(crime.eng, crime.eng$state =='florida' & (year == 2013 | year == 2014 | year == 2015 | year == 2016 | year == 2017))

boxplot(crime.eng$violent_crime ~ crime.eng$year)
boxplot(crime.eng$homicide ~ crime.eng$year)

plot(crime.eng$crimetotal ~ crime.eng$dealerreg)
plot(crime.eng$crimetotal ~ crime.eng$buyerreg)
plot(crime.eng$prohibition, crime.eng$crimetotal)
plot(crime.eng$backgroundcheck, crime.eng$crimetotal)
plot(crime.eng$ammunition, crime.eng$crimetotal)
plot(crime.eng$possessreg, crime.eng$crimetotal)
plot(crime.eng$concealcarry, crime.eng$crimetotal)
plot(crime.eng$assaultweapon, crime.eng$crimetotal)
plot(crime.eng$chileaccess, crime.eng$crimetotal)
plot(crime.eng$guntrafficking, crime.eng$crimetotal)
plot(crime.eng$preemption, crime.eng$crimetotal)
plot(crime.eng$immunity, crime.eng$crimetotal)
plot(crime.eng$domesticviolence, crime.eng$crimetotal)

plot(violent_crime ~ year,  data = subset(crime.law, crime.law$dealerreg.dealerh == 1), col="red")
points(violent_crime ~ year, data = subset(crime.law, crime.law$dealerreg.dealerh == 0), col="blue")
boxplot(crime.law$violent_crime ~ crime.law$ammunition.ammpermit + crime.law$year)
boxplot(crime.law$violent_crime ~ crime.law$background.universalpermit + crime.law$year)
boxplot(crime.law$homicide ~ crime.law$background.mentalhealth + crime.law$year)

# plot(x = crime.law$year, y = crime.law$violent_crime, pch = 16, col = factor(crime.law$state))

#legend("topleft", legend = levels(factor(crime.law$state)),pch = 16, col = factor(levels(factor(crime.law$state))))
# plot(x = factor(crime.law$state), y = crime.law$violent_crime, pch = 16, col = factor(crime.law$year))
# legend("topleft", legend = levels(factor(crime.law$year)),pch = 16, col = factor(levels(factor(crime.law$year))))
library(ggplot2)

crime.law %>% 
  ggplot(aes(year, violent_crime)) +
  geom_point(aes(colour = factor(state)), size = 1)+
  geom_line(aes(group = factor(state), colour = factor(state)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1), axis.title.x=element_blank())

table(crime.eng$year, crime.eng$state)
# 50 states in 9 years from 2012 to 2020

#correlation
library(PerformanceAnalytics)
df <- crime.eng
df$state <- NULL
df$year <- NULL
str(df)
chart.Correlation(df) 

#Modelling for violent crime
library(lme4)
library(plm)
model_vc1 <- glm(violent_crime ~ population + police + poverty + education + dealerreg + buyerreg + prohibition + backgroundcheck + ammunition + possessreg + 
               concealcarry + assaultweapon + chileaccess + guntrafficking + preemption + immunity + domesticviolence + as.factor(year),
              data = crime.eng,family=quasipoisson (link=log))

model_vc2 <- glmer(violent_crime ~ population + police + poverty + education + dealerreg + buyerreg + prohibition + backgroundcheck + ammunition + possessreg + 
                  concealcarry + assaultweapon + chileaccess + guntrafficking + preemption + immunity + domesticviolence + (1|state / year) , data = crime.eng, family=poisson (link=log))

d <- pdata.frame(crime.eng, index=c("state","year"))
model_vc3 <- plm(log(violent_crime) ~ population + police + poverty + education + dealerreg + buyerreg + prohibition + backgroundcheck + ammunition + possessreg + 
                concealcarry + assaultweapon + chileaccess + guntrafficking + preemption + immunity + domesticviolence,
                data = d,model="random")

#Modelling for homicide 
model_hc1 <- glm(homicide ~ population + police + poverty + education + dealerreg + buyerreg + prohibition + backgroundcheck + ammunition + possessreg + 
                   concealcarry + assaultweapon + chileaccess + guntrafficking + preemption + immunity + domesticviolence + as.factor(year),
                 data = crime.eng,family=quasipoisson (link=log))

model_hc2 <- glmer(homicide ~ population + police + poverty + education + dealerreg + buyerreg + prohibition + backgroundcheck + ammunition + possessreg + 
                     concealcarry + assaultweapon + chileaccess + guntrafficking + preemption + immunity + domesticviolence + (1|state / year) , data = crime.eng, family=poisson (link=log))

d <- pdata.frame(crime.eng, index=c("state","year"))
model_hc3 <- plm(log(homicide) ~ population + police + poverty + education + dealerreg + buyerreg + prohibition + backgroundcheck + ammunition + possessreg + 
                   concealcarry + assaultweapon + chileaccess + guntrafficking + preemption + immunity + domesticviolence,
                 data = d,model="random")
model_hc3.1 <- plm(log(homicide) ~ population + police + dealerreg + buyerreg + prohibition + backgroundcheck + ammunition + possessreg + 
                   concealcarry + assaultweapon + chileaccess + guntrafficking + preemption + immunity + domesticviolence,
                 data = d,model="within", effect = "twoways")


#Outcome
lme4::ranef(model_vc2)
ranef(model_vc3)

lme4::ranef(model_hc2)
ranef(model_hc3)
fixef(model_hc3.1)


#Test
library(car)
vif(model_vc1)
vif(model_vc3)
vif(model_hc1)
vif(model_hc3)

#Interpretation
library(stargazer)
stargazer(model_vc1,model_vc2,model_vc3, title="Compare", type="text", single.row=TRUE)
stargazer(model_hc1,model_hc2,model_hc3, title="Compare", type="text", single.row=TRUE)

# Modelling with all relevant laws
str(crime.law)
d <- crime.law
d <- pdata.frame(d, index=c("state","year"))
model_vc4 <- plm(log(violent_crime) ~ police + poverty + education + dealerreg.dealerh + buyerreg.permitlaw + buyerreg.fingerprint + buyerreg.age
                 + prohibition.violentpartial + prohibition.danger + prohibition.drugmisdemeanor + prohibition.alctreatment
                 + background.universal + background.universalpermit + background.mentalhealth 
                 + ammunition.ammlicense  + ammunition.ammpermit + possessreg.agerestrict + possessreg.carry # possess may not impact
                 + conceal.permit + conceal.background + assaultweapon.ban + assaultweapon.magazine
                 + childaccess.lock + childaccess.storage 
                 + trafficking + strawpurchase + microstamp + personalized
                 + preemption + immunity + domestic.misdemeanor + dvro
                 , data = d, model="within")
summary(model_vc4)
summary(fixef(model_vc4))
within_intercept(model_vc4)
stargazer(model_vc4, title="Compare", type="text", single.row=TRUE)

model_hc4 <- plm(log(homicide) ~ police + poverty + education + dealerreg.dealerh + buyerreg.permitlaw + buyerreg.fingerprint + buyerreg.age
                 + prohibition.violentpartial + prohibition.danger + prohibition.drugmisdemeanor + prohibition.alctreatment
                 + background.universal + background.universalpermit + background.mentalhealth 
                 + ammunition.ammlicense  + ammunition.ammpermit + possessreg.agerestrict + possessreg.carry # possess may not impact
                 + conceal.permit + conceal.background + assaultweapon.ban + assaultweapon.magazine
                 + childaccess.lock + childaccess.storage 
                 + trafficking + strawpurchase + microstamp + personalized
                 + preemption + immunity + domestic.misdemeanor + dvro
                 , data = d, model="within")
summary(model_hc4)
summary(fixef(model_hc4))
within_intercept(model_hc4)
stargazer(model_vc4, model_hc4, title="Compare", type="text", single.row=TRUE)
