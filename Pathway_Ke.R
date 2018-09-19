## Setting the working space
#setwd("/Users/coolsteed/Desktop/R learning/final project")
#options()
#options(digits=3)

## Git command:
# cd Dropbox/Papers/Work in Progress/2018 Homeownership
# git add Pathway_Ke.R
# git commit -m "<versionname>"
# (git remote add origin https://github.com/norzvic/Homeownership.git)
# git push -u origin master

##loading the library 
library("Matching")
library("MatchIt")
library("haven")
library("knitr")
library("cobalt")
library("ggplot2")
library("ebal")
library("stargazer")
library("lmtest")
library("sandwich")
library("foreign")

##Reading the data
pathway <- read_por("02420-0002-Data.por")

##data cleaning 
a <- c("CPS_28","CPS_178","CPS_32","CPS_161","CPS_163",
       "CPS_180","RACE","SEX","G2_A4_C","G2_A5","G2_A5_C",
       "G2_C5","G2_P2","G2_P3","G2_B1","X3MARRNM","UNION",
       "CPS_169","CPS_180","CPS_158","G2_A4_F","CAL_32","CAL_33","CAL_34",
       "G1_A5", "G1_A5_D", "G1_A5_K1", "G1_A5_K2", "G1_A5_K3",  # Add G1 Interviews
       "G1_A6", "G1_A6_A","G1_A6_D", "G1_A7")
data <- pathway[,(colnames(pathway) %in% a)]
data$CPS_28[data$CPS_28 == 99] <- NA  # CHILDREN <8 SUPPORTED IN G1HH AT G2BIR
data$CPS_32[data$CPS_32 > 18] <- NA  # G1 YEARS OF COMPLETED SCHOOL AT G2 BIRTH
data$CPS_161[data$CPS_161 == 99999] <- NA  # G2 AGE 7/8 - G1 HH INCOME (Intervals)
data$CPS_163[data$CPS_163 == 99999] <- NA  # G2 AGE 7/8 - G1 OR NON-PARENT YEARLY HH (Intervals)
data$CPS_178[data$CPS_178 == 9] <- NA  # G2 AGE 7/8-G1 # OF ADD CHILDREN SINCE G2
data$CPS_180[data$CPS_180 == 9] <- NA  # G2 AGE 7/8- G1 MARITAL STATUS: 1 = NEver, 2 = Currently married, 3 = Formerly married
data$CPS_180[data$CPS_180 > 3] <- NA  # G2 AGE 7/8- G1 MARITAL STATUS

data$X3MARRNM[data$X3MARRNM >3] <- NA  # G2 # OF MARRIAGES
data$UNION[data$UNION > 3] <- NA  # UNION STATUS AT THE G2 INTERVIEW: 1 = Married, 2 = Non-union, 3 = Cohabitating
data$RACE[data$RACE != 1] <- 0  # G2 RACE: 1 = White, 0 = Other. NOTE: 490 IN WHITE, 2200 IN BLACK, 4 IN OTHER
data$SEX[data$SEX !=1] <- 0  # G2 SEX: 0 = Female, 1 = Male

data$G2_A4_C[data$G2_A4_C > 3] <- NA  # G2 AGE 16 - OWN OR RENT DWELLING
data$G2_A4_C[data$G2_A4_C != 1] <- 0  # 0 = Rent/Other, 1 = Own
data$G2_A4_F[data$G2_A4_F > 5] <- NA  # G2 AGE 16 - RACIAL MIX IN NEIGHBORHOOD
data$G2_A5[data$G2_A5 > 2] <- NA  # G2 SAME OR DIFF. RESIDENCE AGE 12 & 16
data$G2_A5 <- data$G2_A5 - 1  # 0 = No, 1 = Yes
data$G2_A5_C[data$G2_A5_C >6] <- NA  # G2 AGE 12 - OWN OR RENT DWELLING
data$G2_A5_C[data$G2_A5_C != 1] <- 0  # 0 = Rent/Other, 1 = Own
data$G2_B1[data$G2_B1 > 17] <- NA  # G2 EDUCATION
data$G2_C5[data$G2_C5 > 2] <- NA  # G2 EVER SERVE IN THE MILITARY
##data$G2_P2[data$G2_P2 > 250000] <- NA  # G2 TOTAL HH INCOME IN DOLLARS
data$G2_P3[data$G2_P3 >250000] <- NA  # G2 PERSONAL INCOME IN DOLLARS

data$move <- data$CAL_33  # NUMBER OF MOVES FROM YEARS 13-16 (Cal)

data$G1_A5[data$G1_A5 > 2] <- NA  # G2 living with G1 at 16-17
data$G1_A5 <- data$G1_A5 - 1  # No = 0, Yes = 1
data$G1_A5_D[data$G1_A5_D > 3] <- NA  # Rent or Own at 16-17?
data$G1_A5_D[data$G1_A5_D > 1] <- 0  # Rent/Other = 0, Own = 1
data$G1_A5_K1[data$G1_A5_K1 > 2] <- NA  # Lack of space or privacy
data$G1_A5_K1 <- data$G1_A5_K1 - 1  # No = 0, Yes = 1
data$G1_A5_K2[data$G1_A5_K2 > 2] <- NA  # Lack of security from break-ins
data$G1_A5_K2 <- data$G1_A5_K2 - 1  # No = 0, Yes = 1
data$G1_A5_K3[data$G1_A5_K3 > 2] <- NA  # Crime in the neighborhood
data$G1_A5_K3 <- data$G1_A5_K3 - 1  # No = 0, Yes = 1
data$G1_A6[data$G1_A6 > 2] <- NA  # G2 living with G1 at 11-12
data$G1_A6 <- data$G1_A6 - 1  # No = 0, Yes = 1
data$G1_A6_A[data$G1_A6_A > 2] <- NA  # The same residence as 16-17?
data$G1_A6_A <- data$G1_A6_A - 1  # No = 0, Yes = 1
data$G1_A6_D[data$G1_A6_D > 3] <- NA  # Rent or Own at 11-12?
data$G1_A6_D[data$G1_A6_D > 1] <- 0  # Rent/Other = 0, Own = 1
data$G1_A7[data$G1_A7 > 1] <- 0  # A5 + A6: living together both 11-12 and 16-17? Yes = 1, Other = 0
## Select those who form new household

data <- na.omit(data)

##recoding the treatment

#data$treatment <- data$G2_A4_C*data$G2_A5-data$G2_A5_C*data$G2_A4_C*(data$G2_A5-1)
data$treatment <- NA
for (i in 1:length(data$treatment)){
  if (data$G2_A4_C[i] * data$G2_A5[i] * data$G2_A5_C[i] * data$G1_A6[i] * data$G1_A5[i] * data$G1_A5_D[i] * data$G1_A6_A[i] == 1){
    data$treatment[i] <- 1
  } else{
    data$treatment[i] <- 0
  }
}  # 1 = G2 live in owned house 12-16 & G1 confirm that they live in owned house 12-16, 0 = Others


data <- na.omit(data)
#write.csv(data, "Ke_ckeck180918.csv")


summary(data$treatment)
hist(data$treatment)

##Recoding for the covarites 
data$nkids <- data$CPS_28+data$CPS_178+1
data$income <- data$G2_P3
data$educ <- data$G2_B1
data$vet <- data$G2_C5-1
data$pincome <- data$CPS_161
data$pincome2 <- data$CPS_163
data$peduc <- data$CPS_32
data$poverty <- data$CPS_169
data$CPS_180[data$CPS_180 != 2] <- 0
data$CPS_180[data$CPS_180 == 2] <- 1
data$pmarr <- data$CPS_180
data$live <- data$CPS_158
data$live[data$live != 10] <- 0
data$live[data$live == 10] <- 1
data$neighbor <- data$G2_A4_F

##Recoding for the responding variables
data$responding <- data$income/data$educ 
data$responding2 <- log(data$income+1)/data$educ

##Recoding the treatment for residential stability
data$treatment2 <- 1
data$treatment2[data$move >= 3] <- 0

##unmatched model
lm.model0.1 <- lm(responding ~ treatment + RACE + pincome2 + nkids + peduc + SEX + poverty + live + as.factor(neighbor) + move, data = data)
lm.model0.2 <- lm(responding2 ~ treatment + RACE + pincome2 + nkids + peduc + SEX + poverty + live + as.factor(neighbor) + move, data = data)
lm.model0.3 <- lm(income ~ treatment + RACE + pincome2 + nkids + peduc + SEX + poverty + live + as.factor(neighbor) + move, data = data)
lm.model0.4 <- lm(educ ~ treatment + RACE + pincome2 + nkids + peduc + SEX + poverty + live + as.factor(neighbor) + move, data = data)
summary(lm.model0.1)
summary(lm.model0.2)
summary(lm.model0.3)
summary(lm.model0.4)

##matching with control on the neighborhood ethnic composition. 
Y <- data$responding2
Tr <- data$treatment
x <- data[,(colnames(data) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##covariates for matching
z <- data[,(colnames(data) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##bias adjustment
match1<-Match(Y, Tr, X = x, Z = z, V = rep(1, length(Y)),
              estimand = "ATT", M = 1, BiasAdjust = TRUE, exact = c(1,0,0,0,1,0,0,1),
              caliper = NULL,  replace = FALSE, ties = TRUE,
              CommonSupport = FALSE, Weight = 1, Weight.matrix = NULL,
              weights = NULL, Var.calc = 0,  sample = FALSE, restrict = NULL,
              match.out = NULL, distance.tolerance = 1e-05,
              tolerance = sqrt(.Machine$double.eps), version = "standard")
summary(match1)

##balance check
balancechk1 <- MatchBalance(treatment ~ RACE + pincome2 + nkids + peduc + SEX + poverty + live + neighbor, data = data, match.out = match1, nboots=500)
covarnames <- c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor")
baltest.collect(balancechk1, var.names= covarnames, after = TRUE)
baltable<- baltest.collect(balancechk1, var.names= covarnames, after = TRUE)
stargazer(baltable, title = "Matching Result for PTA data", omit = "qqm")
matched1 <- data[unlist(match1[c("index.treated","index.control")]),]

##Balance check graph
t <-bal.tab(match1, treatment ~ RACE + pincome2 + nkids + peduc + SEX + poverty + live + neighbor, disp.means = TRUE,
            continuous = "std",
            binary = c("raw", "std"),
            data = data)
love.plot(t, stat = c("mean.diffs", "variance.ratios",
                      "ks.statistics"),
          threshold = NULL, 
          var.order = "unadjusted")

##model testing
lm.model1.1 <- lm(responding2 ~ treatment + RACE + SEX + nkids + peduc + live + as.factor(neighbor), data = matched1)
lm.model1.2 <- lm(responding2 ~ treatment + RACE + SEX + pincome2 + poverty + nkids + peduc + live + as.factor(neighbor), data = matched1)
lm.model1.3 <- lm(responding2 ~ treatment + RACE + SEX + pincome2 + poverty + nkids + peduc + live + move + as.factor(neighbor), data = matched1)
lm.model1.4 <- lm(responding2 ~ treatment + RACE + SEX + pincome2 + poverty + treatment*poverty + nkids + peduc + live + as.factor(neighbor), data = matched1)
lm.model1.5 <- lm(responding2 ~ treatment + RACE + SEX + pincome2 + poverty + treatment*poverty + nkids + peduc + live + move + as.factor(neighbor), data = matched1)
summary(lm.model1.1)
coeftest(lm.model1.1, vcoc=vcovHC(model, type="HC2"))
summary(lm.model1.2)
coeftest(lm.model1.2, vcoc=vcovHC(model, type="HC2"))
summary(lm.model1.3)
coeftest(lm.model1.3, vcoc=vcovHC(model, type="HC2"))
summary(lm.model1.4)
coeftest(lm.model1.4, vcoc=vcovHC(model, type="HC2"))
summary(lm.model1.5)
coeftest(lm.model1.5, vcoc=vcovHC(model, type="HC2"))

##Step 2, Residential Stability 
##Matching for treatment2 
Y <- data$responding2
Tr <- data$treatment2
x <- data[,(colnames(data) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##covariates for matching
z <- data[,(colnames(data) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##bias adjustment
match2<-Match(Y, Tr, X = x, Z = z, V = rep(1, length(Y)),
              estimand = "ATT", M = 1, BiasAdjust = TRUE, exact = c(1,0,0,0,1,0,0,1),
              caliper = NULL,  replace = FALSE, ties = TRUE,
              CommonSupport = FALSE, Weight = 1, Weight.matrix = NULL,
              weights = NULL, Var.calc = 0,  sample = FALSE, restrict = NULL,
              match.out = NULL, distance.tolerance = 1e-05,
              tolerance = sqrt(.Machine$double.eps), version = "standard")
summary(match2)

##balance check
balancechk2 <- MatchBalance(treatment2 ~ RACE + pincome2 + nkids + peduc + SEX + poverty + live + neighbor, 
                            data = data, match.out = match2, nboots=500)
covarnames <- c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor")
baltest.collect(balancechk2, var.names= covarnames, after = TRUE)
baltable<- baltest.collect(balancechk2, var.names= covarnames, after = TRUE)
stargazer(baltable, title = "Matching Result for PTA data", omit = "qqm")
matched2 <- data[unlist(match2[c("index.treated","index.control")]),]

##Balance check graph
t <-bal.tab(match2, treatment2 ~ RACE + pincome2 + nkids + peduc + SEX + poverty + live + neighbor, disp.means = TRUE,
            continuous = "std",
            binary = c("raw", "std"),
            data = data)
love.plot(t, stat = c("mean.diffs", "variance.ratios",
                      "ks.statistics"),
          threshold = NULL, 
          var.order = "unadjusted")

##Model test
lm.model2.1 <- lm(responding2 ~ treatment2 + RACE + SEX + nkids + peduc + live + as.factor(neighbor), data = matched2)
lm.model2.2 <- lm(responding2 ~ treatment2 + RACE + SEX + pincome2 + poverty + nkids + peduc + live + as.factor(neighbor), data = matched2)
lm.model2.3 <- lm(responding2 ~ treatment2 + RACE + SEX + pincome2 + poverty + nkids + peduc + live + treatment + as.factor(neighbor), data = matched2)
lm.model2.4 <- lm(responding2 ~ treatment2 + RACE + SEX + pincome2 + poverty + treatment2*poverty + nkids + peduc + live + as.factor(neighbor), data = matched2)
lm.model2.5 <- lm(responding2 ~ treatment2 + RACE + SEX + pincome2 + poverty + treatment2*poverty + nkids + peduc + live + treatment + as.factor(neighbor), data = matched2)
summary(lm.model2.1)
coeftest(lm.model2.1, vcoc=vcovHC(model, type="HC2"))
summary(lm.model2.2)
coeftest(lm.model2.2, vcoc=vcovHC(model, type="HC2"))
summary(lm.model2.3)
coeftest(lm.model2.3, vcoc=vcovHC(model, type="HC2"))
summary(lm.model2.4)
coeftest(lm.model2.4, vcoc=vcovHC(model, type="HC2"))
summary(lm.model2.5)
coeftest(lm.model2.5, vcoc=vcovHC(model, type="HC2"))

##Step3 Compared different treatments 
##Step3.1 Compared Homeownership V.S. Stable Renting
##Select the subset
data3.1 <- subset(data, (treatment == 0 & treatment2 == 1) | treatment == 1)
##Matching for the subset data 
Y <- data3.1$responding2
Tr <- data3.1$treatment
x <- data3.1[,(colnames(data3.1) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##covariates for matching
z <- data3.1[,(colnames(data3.1) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##bias adjustment
match3.1<-Match(Y, Tr, X = x, Z = z, V = rep(1, length(Y)),
              estimand = "ATT", M = 1, BiasAdjust = TRUE, exact = c(1,0,0,0,1,0,0,1),
              caliper = NULL,  replace = FALSE, ties = TRUE,
              CommonSupport = FALSE, Weight = 1, Weight.matrix = NULL,
              weights = NULL, Var.calc = 0,  sample = FALSE, restrict = NULL,
              match.out = NULL, distance.tolerance = 1e-05,
              tolerance = sqrt(.Machine$double.eps), version = "standard")
summary(match3.1)

##balance check
balancechk3.1 <- MatchBalance(treatment ~ RACE + pincome2 + nkids + peduc + SEX + poverty + live + neighbor, 
                            data = data3.1, match.out = match3.1, nboots=500)
covarnames <- c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor")
baltest.collect(balancechk3.1, var.names= covarnames, after = TRUE)
baltable<- baltest.collect(balancechk3.1, var.names= covarnames, after = TRUE)
stargazer(baltable, title = "Matching Result for PTA data", omit = "qqm")
matched3.1 <- data3.1[unlist(match3.1[c("index.treated","index.control")]),]

## Model Building 
lm.model3.1 <- lm(responding2 ~ treatment + RACE + SEX + pincome2 + poverty + nkids + peduc + live + as.factor(neighbor), data = matched3.1)
summary(lm.model3.1)

##Step3.2 Compared Homeownership V.S. untable Renting
##Select the subset
data3.2 <- subset(data, (treatment == 0 & treatment2 == 0) | treatment == 1)
##Matching for the subset data 
Y <- data3.2$responding2
Tr <- data3.2$treatment
x <- data3.2[,(colnames(data3.2) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##covariates for matching
z <- data3.2[,(colnames(data3.2) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##bias adjustment
match3.2<-Match(Y, Tr, X = x, Z = z, V = rep(1, length(Y)),
                estimand = "ATT", M = 1, BiasAdjust = TRUE, exact = c(1,0,0,0,1,0,0,1),
                caliper = NULL,  replace = FALSE, ties = TRUE,
                CommonSupport = FALSE, Weight = 1, Weight.matrix = NULL,
                weights = NULL, Var.calc = 0,  sample = FALSE, restrict = NULL,
                match.out = NULL, distance.tolerance = 1e-05,
                tolerance = sqrt(.Machine$double.eps), version = "standard")
summary(match3.2)

##balance check
balancechk3.2 <- MatchBalance(treatment ~ RACE + pincome2 + nkids + peduc + SEX + poverty + live + neighbor, 
                              data = data3.2, match.out = match3.2, nboots=500)
covarnames <- c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor")
baltest.collect(balancechk3.2, var.names= covarnames, after = TRUE)
baltable<- baltest.collect(balancechk3.2, var.names= covarnames, after = TRUE)
stargazer(baltable, title = "Matching Result for PTA data", omit = "qqm")
matched3.2 <- data3.2[unlist(match3.2[c("index.treated","index.control")]),]

## Model Building 
lm.model3.2 <- lm(responding2 ~ treatment + RACE + SEX + pincome2 + poverty + nkids + peduc + live + as.factor(neighbor), data = matched3.2)
summary(lm.model3.2)

##Step 3.3
##Select the subset
data3.3 <- subset(data, treatment == 0)
##Matching for the subset data 
Y <- data3.3$responding2
Tr <- data3.3$treatment2
x <- data3.3[,(colnames(data3.3) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##covariates for matching
z <- data3.3[,(colnames(data3.3) %in% c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor"))]##bias adjustment
match3.3<-Match(Y, Tr, X = x, Z = z, V = rep(1, length(Y)),
                estimand = "ATT", M = 1, BiasAdjust = TRUE, exact = c(1,0,0,0,1,0,0,1),
                caliper = NULL,  replace = FALSE, ties = TRUE,
                CommonSupport = FALSE, Weight = 1, Weight.matrix = NULL,
                weights = NULL, Var.calc = 0,  sample = FALSE, restrict = NULL,
                match.out = NULL, distance.tolerance = 1e-05,
                tolerance = sqrt(.Machine$double.eps), version = "standard")
summary(match3.3)

##balance check
balancechk3.3 <- MatchBalance(treatment2 ~ RACE + pincome2 + nkids + peduc + SEX + poverty + live + neighbor, 
                              data = data3.3, match.out = match3.3, nboots=500)
covarnames <- c("RACE","pincome2","nkids","peduc","SEX","poverty","live","neighbor")
baltest.collect(balancechk3.3, var.names= covarnames, after = TRUE)
baltable<- baltest.collect(balancechk3.3, var.names= covarnames, after = TRUE)
stargazer(baltable, title = "Matching Result for PTA data", omit = "qqm")
matched3.3 <- data3.3[unlist(match3.3[c("index.treated","index.control")]),]

## Model Building 
lm.model3.3 <- lm(responding2 ~ treatment2 + RACE + SEX + pincome2 + poverty + nkids + peduc + live + as.factor(neighbor), data = matched3.3)
summary(lm.model3.3)

##Making tables
stargazer(lm.model0.1, lm.model0.2, lm.model0.3, lm.model0.4,
          style = "ajs",
          omit = "as.factor",
          font.size = "footnotesize",
          column.sep.width = "3pt", 
          title = "Regression Result without Matching"
          )
stargazer(lm.model1.1, lm.model1.2, lm.model1.3, lm.model1.4, lm.model1.5,
          style = "ajs",
          omit = "as.factor",
          font.size = "tiny",
          column.sep.width = "3pt", 
          title = "Regression Result with Matching Treatment = Homeownership")
stargazer(lm.model2.1, lm.model2.2, lm.model2.3, lm.model2.4, lm.model2.5,
          style = "ajs",
          omit = "as.factor",
          font.size = "tiny",
          column.sep.width = "3pt", 
          title = "Regression Result with Matching Treatment = Residential Stability")
stargazer(lm.model3.1, lm.model3.2, lm.model3.3,
          style = "ajs",
          omit = "as.factor",
          font.size = "small",
          title = "Regression Result with Matching Comparing different treatment")

##making graph
ggplot(data, aes(x=poverty)) + 
  geom_histogram(aes(y=..density..),      
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

ggplot(data, aes(x=poverty, colour=as.factor(RACE))) + geom_density()

library(plyr)
cdat <- ddply(data, "RACE", summarise, poverty.mean=mean(poverty))
cdat
ggplot(data, aes(x=poverty, color=as.factor(RACE))) +
  geom_density() +
  geom_vline(data=cdat, aes(xintercept=poverty.mean,  colour=as.factor(RACE)),
             linetype="dashed", size=1)

ggplot(data, aes(x=RACE)) +
  geom_histogram(binwidth=1, colour="black", fill="white")