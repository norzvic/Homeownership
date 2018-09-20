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
       "CPS_180","RACE","SEX","G2_A3_A","G2_A4","G2_A4_C","G2_A5",
       "G2_C5","G2_P3","G2_B1","X3MARRNM","UNION",
       "CPS_169","CPS_180","CPS_158","G2_A4_F","CAL_32","CAL_33","CAL_34",
       "G1_A5", "G1_A5_D",   # Add G1 Interviews
       "G1_A6", "G1_A6_A")
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

data$G2_A3_A[data$G2_A3_A > 3] <- NA  # G2 CURRENT - OWN OR RENT DWELLING
data$G2_A3_A[data$G2_A3_A != 1] <- 0  # 1 = Owned, 0 = Rent/Other
data$G2_A4[data$G2_A4 > 2] <- NA  # G2 SAME OR DIFF. RESIDENCE AGE 16 & NOW
data$G2_A4 <- data$G2_A4 - 1  # 0 = Diff, 1 = Same
data$G2_A4_C[data$G2_A4_C > 6] <- NA  # G2 AGE 16 - OWN OR RENT DWELLING
data$G2_A4_C[data$G2_A4_C == 2] <- 0
data$G2_A4_C[data$G2_A4_C == 3] <- 0  # 0 = Rent/Other, 1 = Own, 6 = Skip/NA
                                      # NOTE: HERE SKIP(6) SHOULD BE INCLUDED SINCE THOSE WHO LIVE IN SAME PLACE 16-CURRENT (G2_A4 = 2) SKIP THE Q.
data$G2_A4_F[data$G2_A4_F > 5] <- NA  # G2 AGE 16 - RACIAL MIX IN NEIGHBORHOOD
data$G2_A5[data$G2_A5 > 2] <- NA  # G2 SAME OR DIFF. RESIDENCE AGE 12 & 16
data$G2_A5 <- data$G2_A5 - 1  # 0 = No, 1 = Yes
#data$G2_A5_C[data$G2_A5_C > 6] <- NA  # G2 AGE 12 - OWN OR RENT DWELLING
#data$G2_A5_C[data$G2_A5_C == 2] <- 0
#data$G2_A5_C[data$G2_A5_C == 3] <- 0  # 0 = Rent/Other, 1 = Own, 6 = Skip/NA
                                      # NOTE: HERE SKIP(6) SHOULD BE INCLUDED SINCE THOSE WHO LIVE IN SAME PLACE 16-CURRENT (G2_A5 = 2) SKIP THE Q.
data$G2_B1[data$G2_B1 > 17] <- NA  # G2 EDUCATION
data$G2_C5[data$G2_C5 > 2] <- NA  # G2 EVER SERVE IN THE MILITARY
##data$G2_P2[data$G2_P2 > 250000] <- NA  # G2 TOTAL HH INCOME IN DOLLARS
data$G2_P3[data$G2_P3 >250000] <- NA  # G2 PERSONAL INCOME IN DOLLARS

data$move <- data$CAL_33  # NUMBER OF MOVES FROM YEARS 13-16 (Cal)
data$move[data$move > 9] <- NA  # Get rid of answer = 99
data$nomove <- NA
data$nomove[data$move == 0] <- 1
data$nomove[data$move != 0] <- 0  # For the convenience of constructing treatment

data$G1_A5[data$G1_A5 > 2] <- NA  # G2 living with G1 at 16-17
data$G1_A5 <- data$G1_A5 - 1  # No = 0, Yes = 1
data$G1_A5_D[data$G1_A5_D > 6] <- NA  # Rent or Own at 16-17?
data$G1_A5_D[data$G1_A5_D == 2] <- 0
data$G1_A5_D[data$G1_A5_D == 3] <- 0  # Rent/Other = 0, G2 LIVE WITH G2 AND Own = 1, Skip/NA = 6
                                      # NOTE: HERE SKIP(6) SHOULD BE INCLUDED SINCE THOSE G2 NOT LIVING WITH G1 AT 16 (G1_A5 = 1) SKIP THE Q.
#data$G1_A5_K1[data$G1_A5_K1 > 2] <- NA  # Lack of space or privacy
#data$G1_A5_K1 <- data$G1_A5_K1 - 1  # No = 0, Yes = 1
#data$G1_A5_K2[data$G1_A5_K2 > 2] <- NA  # Lack of security from break-ins
#data$G1_A5_K2 <- data$G1_A5_K2 - 1  # No = 0, Yes = 1
#data$G1_A5_K3[data$G1_A5_K3 > 2] <- NA  # Crime in the neighborhood
#data$G1_A5_K3 <- data$G1_A5_K3 - 1  # No = 0, Yes = 1
data$G1_A6[data$G1_A6 > 2] <- NA  # G2 living with G1 at 11-12
data$G1_A6 <- data$G1_A6 - 1  # No = 0, Yes = 1
data$G1_A6_A[data$G1_A6_A > 6] <- NA  # The same residence as 16-17?
data$G1_A6_A <- data$G1_A6_A - 1
data$G1_A6_A[data$G1_A6_A == 5] <- 6  # No = 0, Yes = 1, Skip/NA = 6
                                      # NOTE: HERE SKIP(6) SHOULD BE INCLUDED SINCE THOSE G2 NOT LIVING WITH G1 AT 11 (G1_A6 = 1) SKIP THE Q.
#data$G1_A6_D[data$G1_A6_D > 3] <- NA  # Rent or Own at 11-12?
#data$G1_A6_D[data$G1_A6_D > 1] <- 0  # Rent/Other = 0, Own = 1
#data$G1_A7[data$G1_A7 > 1] <- 0  # A5 + A6: living together both 11-12 and 16-17? Yes = 1, Other = 0

data <- na.omit(data)

##recoding the treatment
#data$treatment <- data$G2_A4_C*data$G2_A5-data$G2_A5_C*data$G2_A4_C*(data$G2_A5-1)
data$treatment <- NA
for (i in 1:length(data$treatment)){
  if (data$G2_A3_A[i] == 1  &
      data$G2_A4[i] == 1    &
      data$G2_A5[i] == 1    &
      data$G1_A5[i] == 1    &
      data$G1_A5_D[i] == 1  &
      data$G1_A6[i] == 1    &
      data$G1_A6_A[i] == 1){
    data$treatment[i] <- 1
  } else if(
      data$G2_A4[i] == 0    &
      data$G2_A4_C[i] == 1  &
      data$G2_A5[i] == 1    &
      data$G1_A5[i] == 1    &
      data$G1_A5_D[i] == 1  &
      data$G1_A6[i] == 1    &
      data$G1_A6_A[i] == 1){
    data$treatment[i] <- 1
  } else{
    data$treatment[i] <- 0}
}  # 1 = G2 live in owned house 12-16 & G1 confirm that they live in owned house 12-16, 0 = Others


#summary(data$treatment)
#hist(data$treatment)

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

# Abandon observations that G2 and G1 conflict.
data$G2_dummy <- 0
for (i in 1:length(data$treatment)){
  if (data$G2_A3_A[i] == 1  &
    data$G2_A4[i] == 1    &
    data$G2_A5[i] == 1){
    data$G2_dummy[i] <- 1
  } else
  if (data$G2_A4[i] == 0    &
      data$G2_A4_C[i] == 1  &
      data$G2_A5[i] == 1){
    data$G2_dummy[i] <- 1
  }
}

data$G1_dummy <- 0
for (i in 1:length(data$treatment)){
  if (data$G1_A5[i] == 1    &
      data$G1_A5_D[i] == 1  &
      data$G1_A6[i] == 1    &
      data$G1_A6_A[i] == 1){
    data$G1_dummy[i] <- 1
  }
}

data$G1_VS_G2 <- NA
for (i in 1:length(data$treatment)){
  if (data$G1_dummy[i] == data$G2_dummy[i]){
    data$G1_VS_G2[i] <- 1
  }
}

# Abadon treatment group obs that have move > 0.
data$G2_liar <- 1
for (i in 1:length(data$treatment)){
  if (data$treatment[i] == 1 & data$move[i] > 0){
    data$G2_liar[i] <- NA
  }
}

data <- na.omit(data)
# write.csv(data, "Ke_ckeck180918.csv")

# Abandon "treatment group"
data$treatment[data$treatment == 1] <- NA
data <- na.omit(data)


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
stargazer(baltable, title = "Matching Result for Pathway (Blocked) data", omit = "qqm")
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

