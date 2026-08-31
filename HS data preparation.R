## Load dataset
library(haven)

## This is the openly available dataset on which I ran the STATA script 
data <- data.frame(haven::read_dta("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/PhD Project/Projects 2 and 3/Developmental dataset/Deming data/data_Deming_2008_0217_MF.dta"))
names(data)
dim(data)

##################################
##
## Prepare Predictors
##
##

## Code program (no preschool, preschool, HeadStart)
data$program <- factor(ifelse(data$None2_90 == 1, "None", 
                              ifelse(data$Pre2_90 == 1, "Pre", "HS")))

# Check tables
table(data[data$HS2_90 == 1,]$None2_90, data[data$HS2_90 == 1,]$Pre2_90) 
table(data[data$HS2_90 == 0,]$None2_90, data[data$HS2_90 == 0,]$Pre2_90) 

## Keep only families in which children differ in HS participation
data$Ever_Daycare <- ifelse(
  is.na(data$Ever_Daycare88) & is.na(data$Ever_Daycare90), NA, 
  ifelse(data$Ever_Daycare88 == 1 | data$Ever_Daycare90 == 1, 1, 0))

## Write variable labels to external file
write.csv2(unlist(sapply(data, \(x) attr(x, "label"))), 
           file = "variable_names.xls")

## Does it matter which AFQT variable we use? (no)
plot(data$AFQT_Pct81, data$AFQT_Pct81_REV)
cor(data$AFQT_Pct81, data$AFQT_Pct81_REV, use="pairwise.complete")
table(is.na(data$AFQT_Pct81), is.na(data$AFQT_Pct81_REV))

## 0-nonzero coded child characteristics; 96,98,100 have most data:
## Get NA if always missing, get 0 if never observed, get 1 if ever observed.

## Hyper86	## HLTH SEC 2 HYPERKINESIS,HYPERACTIVITY
## Hyperactivity is 0-3, 0-1, 0-1 in 96, 98, 100
Hyper_NA <- rowSums(is.na(data[ , paste0("Hyper", c(96, 98, 100))]))
Hyper_sum <- rowSums(sapply(data[ , paste0("Hyper", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Hyper_sum, Hyper_NA, useNA = "ifany")
Hyper1 <- ifelse(Hyper_NA == 3, NA, ifelse(Hyper_sum == 0, 0, 1))
table(Hyper_sum, Hyper_NA, Hyper1, useNA = "ifany")

## LD86 ##	CHILD HAS LEARNING DISABILITY
## LD is always 0-1 in 96, 98, 100
table(data$LD96, data$LD98, data$LD100, useNA = "ifany")
LD_NA <- rowSums(is.na(data[ , paste0("LD", c(96, 98, 100))]))
LD_sum <- rowSums(sapply(data[ , paste0("LD", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(LD_sum, LD_NA, useNA = "ifany")
LD <- ifelse(LD_NA == 3, NA, ifelse(LD_sum == 0, 0, 1))
table(LD_sum, LD_NA, LD, useNA = "ifany")

## Nerve is coded 0-14 in 96, 0-1 in 98, 0-1 in 100	##	CHILD HAS CHRONIC NERVOUS DISORDER
table(data$Nerve96, data$Nerve98, data$Nerve100, useNA = "ifany")
Nerve_NA <- rowSums(is.na(data[ , paste0("Nerve", c(96, 98, 100))]))
Nerve_sum <- rowSums(sapply(data[ , paste0("Nerve", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Nerve_sum, Nerve_NA, useNA = "ifany")
Nerve1 <- ifelse(Nerve_NA == 3, NA, ifelse(Nerve_sum == 0, 0, 1))
table(Nerve_sum, Nerve_NA, Nerve1, useNA = "ifany")

## coded 0-5 in 96, 0-1 in 98, 0-1 in 100:
## Resp86 ##	CHLD HAS RESPIRATORY DISRDER/SINUS INF
table(data$Resp96, data$Resp98, data$Resp100, useNA = "ifany")
Resp_NA <- rowSums(is.na(data[ , paste0("Resp", c(96, 98, 100))]))
Resp_sum <- rowSums(sapply(data[ , paste0("Resp", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Resp_sum, Resp_NA, useNA = "ifany")
Resp1 <- ifelse(Resp_NA == 3, NA, ifelse(Resp_sum == 0, 0, 1))
table(Resp_sum, Resp_NA, Resp1, useNA = "ifany")

## coded 0-12 in 96, 0-1 in 98, 0-1 in 100
## Retard86 ##	CHILD HAS MENTAL RETARDATION
table(data$Retard96, data$Retard98, data$Retard100, useNA = "ifany")
Retard_NA <- rowSums(is.na(data[ , paste0("Retard", c(96, 98, 100))]))
Retard_sum <- rowSums(sapply(data[ , paste0("Retard", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Retard_sum, Retard_NA, useNA = "ifany")
Retard1 <- ifelse(Retard_NA == 3, NA, ifelse(Retard_sum == 0, 0, 1))
table(Retard_sum, Retard_NA, Retard1, useNA = "ifany")

## coded 0-6 in 96, 0-1 in 98, 0-1 in 100
## Speech86 ## CHILD HAS SPEECH IMPAIRMENT
table(data$Speech96, data$Speech98, data$Speech100, useNA = "ifany")
Speech_NA <- rowSums(is.na(data[ , paste0("Speech", c(96, 98, 100))]))
Speech_sum <- rowSums(sapply(data[ , paste0("Speech", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Speech_sum, Speech_NA, useNA = "ifany")
Speech1 <- ifelse(Speech_NA == 3, NA, ifelse(Speech_sum == 0, 0, 1))
table(Speech_sum, Speech_NA, Speech1, useNA = "ifany")

## coded 0-8 in 96, 0-1 in 98, 0-1 in 100
## Blind86 ## CHILD HAS SERIOUS DIFFICULTY SEEING
table(data$Blind96, data$Blind98, data$Blind100, useNA = "ifany")
Blind_NA <- rowSums(is.na(data[ , paste0("Blind", c(96, 98, 100))]))
Blind_sum <- rowSums(sapply(data[ , paste0("Blind", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Blind_sum, Blind_NA, useNA = "ifany")
Blind1 <- ifelse(Blind_NA == 3, NA, ifelse(Blind_sum == 0, 0, 1))
table(Blind_sum, Blind_NA, Blind1, useNA = "ifany")

## Blood96 ## Blood disorder (sickle cell) 0-17 in 96, 0-1 in 98, 0-1 in 100
## 0-17 in 96, 0-1 in 98, 0-1 in 100
table(data$Blood96, data$Blood98, data$Blood100, useNA = "ifany")
Blood_NA <- rowSums(is.na(data[ , paste0("Blood", c(96, 98, 100))]))
Blood_sum <- rowSums(sapply(data[ , paste0("Blood", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Blood_sum, Blood_NA, useNA = "ifany")
Blood1 <- ifelse(Blood_NA == 3, NA, ifelse(Blood_sum == 0, 0, 1))
table(Blood_sum, Blood_NA, Blood1, useNA = "ifany")

## Crippled96 ## CHILD HAS CRIPPLED ORTHOPEDIC HANDICAP
## 0-11 in 96, 0-1 in 98, 0-1 in 100
table(data$Crippled96, data$Crippled98, data$Crippled100, useNA = "ifany")
Crippled_NA <- rowSums(is.na(data[ , paste0("Crippled", c(96, 98, 100))]))
Crippled_sum <- rowSums(sapply(data[ , paste0("Crippled", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Crippled_sum, Crippled_NA, useNA = "ifany")
Crippled1 <- ifelse(Crippled_NA == 3, NA, ifelse(Crippled_sum == 0, 0, 1))
table(Crippled_sum, Crippled_NA, Crippled1, useNA = "ifany")

## Deaf86 ## CHILD HAS SERIOUS HEARING DIFFICULTY
## 0-7 in 96, 0-1 in 98, 0-1 in 100
table(data$Deaf96, data$Deaf98, data$Deaf100, useNA = "ifany")
Deaf_NA <- rowSums(is.na(data[ , paste0("Deaf", c(96, 98, 100))]))
Deaf_sum <- rowSums(sapply(data[ , paste0("Deaf", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Deaf_sum, Deaf_NA, useNA = "ifany")
Deaf1 <- ifelse(Deaf_NA == 3, NA, ifelse(Deaf_sum == 0, 0, 1))
table(Deaf_sum, Deaf_NA, Deaf1, useNA = "ifany")

## Disturb86 ## CHLD HAS SERIOUS EMOTIONAL DISTURBANCE
## 0-9 in 96, 0-1 in 98, 0-1 in 100
table(data$Disturb96, data$Disturb98, data$Disturb100, useNA = "ifany")
Disturb_NA <- rowSums(is.na(data[ , paste0("Disturb", c(96, 98, 100))]))
Disturb_sum <- rowSums(sapply(data[ , paste0("Disturb", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Disturb_sum, Disturb_NA, useNA = "ifany")
Disturb1 <- ifelse(Disturb_NA == 3, NA, ifelse(Disturb_sum == 0, 0, 1))
table(Disturb_sum, Disturb_NA, Disturb1, useNA = "ifany")


## Ear88 # CHILD HAS CHRONIC EAR PROBLMS,INFECTIONS
## 0-11 in 96, 0-1 in 98, 0-1 in 100
table(data$Ear96, data$Ear98, data$Ear100, useNA = "ifany")
Ear_NA <- rowSums(is.na(data[ , paste0("Ear", c(96, 98, 100))]))
Ear_sum <- rowSums(sapply(data[ , paste0("Ear", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Ear_sum, Ear_NA, useNA = "ifany")
Ear1 <- ifelse(Ear_NA == 3, NA, ifelse(Ear_sum == 0, 0, 1))
table(Ear_sum, Ear_NA, Ear1, useNA = "ifany")

## Epilepsy88 ## CHILD HAS EPILESY/SEIZURES
## 0-18 in 96, 0-1 in 98, 0-1 in 100
table(data$Epilepsy96, data$Epilepsy98, data$Epilepsy100, useNA = "ifany")
Epilepsy_NA <- rowSums(is.na(data[ , paste0("Epilepsy", c(96, 98, 100))]))
Epilepsy_sum <- rowSums(sapply(data[ , paste0("Epilepsy", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Epilepsy_sum, Epilepsy_NA, useNA = "ifany")
Epilepsy1 <- ifelse(Epilepsy_NA == 3, NA, ifelse(Epilepsy_sum == 0, 0, 1))
table(Epilepsy_sum, Epilepsy_NA, Epilepsy1, useNA = "ifany")

## Allergy88 ## CHILD HAS ALLEGIC CONDITION(S)
## 0-10 in 96, 0-1 in 98, 0-1 in 100
table(data$Allergy96, data$Allergy98, data$Allergy100, useNA = "ifany")
Allergy_NA <- rowSums(is.na(data[ , paste0("Allergy", c(96, 98, 100))]))
Allergy_sum <- rowSums(sapply(data[ , paste0("Allergy", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Allergy_sum, Allergy_NA, useNA = "ifany")
Allergy1 <- ifelse(Allergy_NA == 3, NA, ifelse(Allergy_sum == 0, 0, 1))
table(Allergy_sum, Allergy_NA, Allergy1, useNA = "ifany")

## Heart88 ## CHILD HAS HEART TROUBLE
## 0-13 in 96, 0-1 in 98, 0-1 in 100
table(data$Heart96, data$Heart98, data$Heart100, useNA = "ifany")
Heart_NA <- rowSums(is.na(data[ , paste0("Heart", c(96, 98, 100))]))
Heart_sum <- rowSums(sapply(data[ , paste0("Heart", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Heart_sum, Heart_NA, useNA = "ifany")
Heart1 <- ifelse(Heart_NA == 3, NA, ifelse(Heart_sum == 0, 0, 1))
table(Heart_sum, Heart_NA, Heart1, useNA = "ifany")

## Athma88 ## CHILD HAS ASTHMA
## 0-4 in 96, 0-1 in 98, 0-1 in 100
table(data$Asthma96, data$Asthma98, data$Asthma100, useNA = "ifany")
Asthma_NA <- rowSums(is.na(data[ , paste0("Asthma", c(96, 98, 100))]))
Asthma_sum <- rowSums(sapply(data[ , paste0("Asthma", c(96, 98, 100))], \(x) ifelse(is.na(x), 0, x)))
table(Asthma_sum, Asthma_NA, useNA = "ifany")
Asthma1 <- ifelse(Asthma_NA == 3, NA, ifelse(Asthma_sum == 0, 0, 1))
table(Asthma_sum, Asthma_NA, Asthma1, useNA = "ifany")

data <- cbind(Hyper1, LD, Nerve1, Resp1, Retard1, Speech1, Blind1, Blood1, 
              Epilepsy1, Disturb1, Deaf1, Crippled1, Ear1, Allergy1, Heart1, 
              Asthma1, data)

keep_vars <- c("MotherID", "ChildID", 
               
               ## Mother characteristics
               "AgeAFQT", "Ageat1stBirth", "MomDropout", 
               "Height_Mom81", "MothED", "Age_Moth_Birth",
               
               ## Father characteristics
               "HighGrade_GDad79", "Father_HH86",
               
               ## Home / family characteristics
               "Insurance88", "Medicaid88", "PermInc",
               "HomeEmo_Pct88", "HOMECog_Pct88", "HOME_Pct88", "FamilySize79",
               "Grandmother86",
               
               ## Pre- and postnatal context characteristics
               "Freq_Alc_BefBirth", "Freq_Smoke_BefBirth", "Breastfed",
               
               ## Child characteristics at birth
               "Race_Child", "Sex_Child", "BirthWeight", "BirthOrder", 
               "BornEarlyorLate", "BornOnTime",
               
               ## Physical illnesses child
               "Illness_1stYr", "NumIll88", "HealthCond_before",
               "Resp1", "Blood1", "Epilepsy1", "Crippled1", "Ear1",
               "Allergy1", "Heart1",  "Asthma1", 
               
               ## Developmental / sensory / learning / cognitive problems
               "Retard1", "Speech1", "Blind1", "Deaf1", "LD",
               "Hyper1", "Nerve1", "Disturb1",
               
               ## Cognitive test performance
               "PIATMT_Raw88", "PIATRR_Raw88", "PPVT_Raw86", "Test_Pct104",
               
               ## Personality inventory scores
               "BPI_Raw88", "BPIAS_Raw88", "BPIAS_Raw104",
               
               ## schooling characteristics
               "program", "RepeatNone98", "Ever_Daycare")

length(keep_vars)
table(complete.cases(data[ , keep_vars]))
sapply(data[ , keep_vars], \(x) table(x, useNA = "ifany"))
sapply(data[ , keep_vars], \(x) table(is.na(x)))
predictors <- data[ , keep_vars]

table(complete.cases(predictors)) ## no complete observations

## code factors as such, make missing values a separate category
facs <- c("MotherID", "ChildID", "MomDropout", "Grandmother86", "Insurance88", 
          "Father_HH86", 
          "Medicaid88", "BornOnTime", "program", "Race_Child", "Sex_Child", 
          "RepeatNone98", "BornEarlyorLate", "Breastfed", "Ever_Daycare", 
          "Illness_1stYr", "HealthCond_before", "Hyper1", "Asthma1", "LD", 
          "Nerve1", "Resp1", "Retard1", "Speech1", "Blind1", "Blood1", 
          "Epilepsy1", "Disturb1", "Deaf1", "Crippled1", "Ear1", "Allergy1", 
          "Heart1")
sapply(predictors[ , facs[-(1:2)]], table, useNA = "ifany") ## check number of missings
predictors[ , facs] <- sapply(predictors[ , facs], \(x) ifelse(is.na(x), "Missing", x))
predictors[ , facs] <- data.frame(lapply(predictors[ , facs], factor))

table(rowSums(is.na(predictors))) 


## @Zino: Select a subset of predictors that will result in 
## a large enough sample with complete cases
sapply(predictors, \(x) table(is.na(x)))



######################################################
##
## Select response (all outcomes assessed in 2004)
##
##
table(is.na(data$PPVT_Raw104)) ## Peabody Picture Vocabulary Test
table(is.na(data$PIATMT_Raw104)) ## math score
table(is.na(data$PIATRR_Raw104)) ## reading score
table(is.na(data$BPIAS_Raw104)) ## antisocial behavior scale
table(is.na(data$Test_Pct104)) ## some combined test score
table(is.na(data$BPI_Raw104)) ## antisocial behavior scale

## @Zino: Would first try Pct 104 because it has most observations,
## and cognitive test performance might be best explainable by
## the predictors, but not entirely sure

# New: factors with "missing" as category

excl_pred_v1 <- c(
  "PIATMT_Raw88",
  "PIATRR_Raw88",
  "PPVT_Raw86",
  "BPI_Raw88",
  "BPIAS_Raw88",
  "BPIAS_Raw104",
  # "Test_Pct104",
  "NumIll88" ,
  #"HighGrade_GDad79" ,
  "Freq_Smoke_BefBirth",
  #"Freq_Alc_BefBirth",
  "HomeEmo_Pct88",
  "HOMECog_Pct88",      
  "HOME_Pct88",
  "Ageat1stBirth",
  #"RepeatNone98",
  "Ever_Daycare",
  "program",
  "Disturb1",
  "Nerve1",
  "Hyper1",
  "LD",
  "Deaf1",
  "Blind1",
  "Speech1",
  "Retard1",
  "Asthma1",
  "Heart1",
  "Allergy1",
  "Ear1",
  "Crippled1",
  "Epilepsy1",
  "Blood1",
  "Resp1",
  "HealthCond_before",
  # "Illness_1stYr",
  # "BornOnTime",
  # "BornEarlyorLate",
  "Medicaid88",
  "Insurance88"
  ,"Father_HH86"
)


# Number of complete cases
sum(complete.cases(predictors[!names(predictors) %in% excl_pred_v1]))

data_cc <- predictors[complete.cases(predictors[!names(predictors) %in% excl_pred_v1]), !names(predictors) %in% excl_pred_v1]

apply(data_cc[,c(
  # "program",
  # "Ever_Daycare",
  "RepeatNone98",
  # "Disturb1",
  # "Nerve1",
  # "Hyper1",
  # "LD",
  # "Deaf1",
  # "Blind1",
  # "Speech1",
  # "Retard1",
  # "Asthma1",
  # "Heart1",
  # "Allergy1",
  # "Ear1",
  # "Crippled1",
  # "Epilepsy1",
  # "Blood1",
  # "Resp1",
  # "HealthCond_before",
  "Illness_1stYr",
  "BornOnTime",
  "BornEarlyorLate",
  # "Medicaid88",
  "HighGrade_GDad79"
  # "Insurance88"
)],2, table)

# Sort data according to groups
data_cc <-
  data_cc[,c(2,14,15,16,17,18,19,20,22, # child characteristics
             1,3,4,5,6,7,  # mother characteristics
             9, 10, 11, 8, # family characteristics
             12, 13,       # pre- and postnatal characteristics
             21)]          # outcome
