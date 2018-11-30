#######################################
### Fake Healthcare Data Generation ###
#######################################
# Script Written by Jon Wayland


library(MASS)

set.seed(4)
# Generic rows with high correlations
mu <- rep(0,4)
Sigma <- matrix(.95, nrow=4, ncol=4) + diag(4)*.3
rawvars <- mvrnorm(n=15000, mu=mu, Sigma=Sigma)
# cor(rawvars)

### Dataframe Start ###
df<-data.frame(rawvars)
colnames(df)<-c("Age", "CC_Count", "Risk_Count", "HP_Paid")

### Creating a Unique Patient ID ###
library(stringr)
num<-data.frame(str_pad(sample(0:9999999, 15000, replace = FALSE),7,pad="0")); colnames(num)<-"numID"
let<-data.frame(toupper(sample(letters,15000, replace=TRUE))); colnames(let)<-"letID"
ID<-cbind(num,let); ID$PatientID<-paste(ID$letID,ID$numID,sep="")
ID$numID<-NULL; ID$letID<-NULL
df<-cbind(ID,df)

### Health Plan Paid Conversion ### ** Think about shifting over like 2k and flooring at 0 **
df$HP_Paid<-df$HP_Paid+abs(min(df$HP_Paid)) # Converting to non-negative
df$HP_Paid<-dgamma(df$HP_Paid, shape = 1) # Converting to gamma distribution
df$HP_Paid<-round(df$HP_Paid*500000/max(df$HP_Paid),2)

# Applying a weight by ranking
#order.scores<-order(df$HP_Paid)
#df$paidrank <- NA
#df$paidrank[order.scores] <- 1:nrow(df)

### Risk Count Conversion ###
df$Risk_Count<-df$Risk_Count+abs(min(df$Risk_Count)) # Converting to non-negative
df$Risk_Count<-dgamma(df$Risk_Count, shape = 2)
df$Risk_Count<-round(df$Risk_Count*20,0)
# cor.test(df$HP_Paid, df$Risk_Count) # Check looks good

# summary(df)

### Chronic Condition Count Conversion ###
df$CC_Count<-df$CC_Count+abs(min(df$CC_Count)) # Converting to non-negative
df$CC_Count<-dgamma(df$CC_Count, shape = 1.1)
df$CC_Count<-round(df$CC_Count*25,0)
# hist(df$CC_Count)

# summary(df$CC_Count)


### Age Conversion ###
df$Age<-df$Age+abs(min(df$Age)) # Converting to non-negative
df$Age<-dgamma(df$Age, shape = 2.5)
df$Age<-df$Age*200+16


###########################
### Adding Gender to DF ###
###########################
df$Gender<-NA
for(i in 1:nrow(df)){
  df$Gender[i]<-sample(c("female","male"), 1, replace=TRUE, prob = c(0.492, 0.508))
}


##########################
### Adding in ER Count ###
##########################
# Probability is a function of paid rank asc: p = paid_rank/nrow(df), 0-6 intervals
# seven equal buckets of costs translates to probabilities of 0-6 intervals

order.scores<-order(df$HP_Paid,df$PatientID)
df$paid_rank[order.scores] <- 1:nrow(df)
bot1<-1
bot2<-round(nrow(df)/7)
bot3<-round(nrow(df)/7)*2
bot4<-round(nrow(df)/7)*3
bot5<-round(nrow(df)/7)*4
bot6<-round(nrow(df)/7)*5
bot7<-round(nrow(df)/7)*6
df$ER_Count<-NA
# Try two
for(i in 1:nrow(df)){
  pos<-df$paid_rank[i]
  if(pos <= bot2){
    df$ER_Count[i] = {sample(c(0,1,2,3,4,5,6), 1, replace=TRUE, prob = c(0.99,0.01,0,0,0,0,0))}
  }
  if(pos > bot2 & pos <=bot3){
    df$ER_Count[i] = {sample(c(0,1,2,3,4,5,6), 1, replace=TRUE, prob = c(0.99,0.05,0,0,0,0,0))}
  }
  if(pos > bot3 & pos <=bot4){
    df$ER_Count[i] = {sample(c(0,1,2,3,4,5,6), 1, replace=TRUE, prob = c(0.99,0.09,0.01,0,0,0,0))}
  }
  if(pos > bot4 & pos <=bot5){
    df$ER_Count[i] = {sample(c(0,1,2,3,4,5,6), 1, replace=TRUE, prob = c(0.99,0.05,0.05,0,0,0,0))}
  }
  if(pos > bot5 & pos <=bot6){
    df$ER_Count[i] = {sample(c(0,1,2,3,4,5,6), 1, replace=TRUE, prob = c(0.98,0.07,0.05,0,0,0,0))}
  }
  if(pos > bot6 & pos <=bot7){
    df$ER_Count[i] = {sample(c(0,1,2,3,4,5,6), 1, replace=TRUE, prob = c(0.94,0.3,0.2,0.1,0.1,0.05,0.05))}
  }
  if(pos > bot7){
    df$ER_Count[i] = {sample(c(0,1,2,3,4,5,6), 1, replace=TRUE, prob = c(0.05,0.2,0.25,0.2,0.15,0.1,0.05))}
  }
}



#########################################
### Adding in Chronic Condition Flags ###
#########################################
# Sum of Flags must equal CC_Count
# Creating pre-determined associations for comorbidities

df$CC_Arthritis<-0
df$CC_Asthma<-0
df$CC_Atrial_Fibrillation<-0
df$CC_Austism<-0 
df$CC_Cancer<-0
df$CC_COPD<-0
df$CC_Dementia<-0
df$CC_Depression<-0
df$CC_Diabetes<-0
df$CC_Heart_Failure<-0
df$CC_Hepatitis<-0
df$CC_HIV_AIDS<-0
df$CC_Hyperlipidemia<-0
df$CC_Hypertension<-0
df$CC_Ischemic_Heart_Disease<-0
df$CC_Kidney_Disease<-0
df$CC_Osteoporosis<-0
df$CC_Schizophrenia<-0
df$CC_Stroke<-0


# Condition Mapping:
map<-data.frame(
  c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"),
  c("Arthritis","Asthma","Atrial_Fibrillation","Austism","Cancer","COPD","Dementia",
    "Depression","Diabetes","Heart_Failure","Hepatitis","HIV_AIDS","Hyperlipidemia",
    "Hypertension","Ischemic_Heart_Disease","Kidney_Disease","Osteoporosis","Schizophrenia","Stroke")
); colnames(map)<-c("Letter", "Condition")


### Condition Assigning ###

##########################################
### Assigning those with One Condition ###
##########################################
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 1){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, 
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	
                         0.020408163))
      if(ref == "A"){df$CC_Arthritis[i] <- 1}
      if(ref == "B"){df$CC_Asthma[i] <- 1}
      if(ref == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref == "D"){df$CC_Austism[i] <- 1}
      if(ref == "E"){df$CC_Cancer[i] <- 1}
      if(ref == "F"){df$CC_COPD[i] <- 1}
      if(ref == "G"){df$CC_Dementia[i] <- 1}
      if(ref == "H"){df$CC_Depression[i] <- 1}
      if(ref == "I"){df$CC_Diabetes[i] <- 1}
      if(ref == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref == "N"){df$CC_Hypertension[i] <- 1}
      if(ref == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, 
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
      if(ref == "A"){df$CC_Arthritis[i] <- 1}
      if(ref == "B"){df$CC_Asthma[i] <- 1}
      if(ref == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref == "D"){df$CC_Austism[i] <- 1}
      if(ref == "E"){df$CC_Cancer[i] <- 1}
      if(ref == "F"){df$CC_COPD[i] <- 1}
      if(ref == "G"){df$CC_Dementia[i] <- 1}
      if(ref == "H"){df$CC_Depression[i] <- 1}
      if(ref == "I"){df$CC_Diabetes[i] <- 1}
      if(ref == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref == "N"){df$CC_Hypertension[i] <- 1}
      if(ref == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, 
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
      if(ref == "A"){df$CC_Arthritis[i] <- 1}
      if(ref == "B"){df$CC_Asthma[i] <- 1}
      if(ref == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref == "D"){df$CC_Austism[i] <- 1}
      if(ref == "E"){df$CC_Cancer[i] <- 1}
      if(ref == "F"){df$CC_COPD[i] <- 1}
      if(ref == "G"){df$CC_Dementia[i] <- 1}
      if(ref == "H"){df$CC_Depression[i] <- 1}
      if(ref == "I"){df$CC_Diabetes[i] <- 1}
      if(ref == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref == "N"){df$CC_Hypertension[i] <- 1}
      if(ref == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref == "S"){df$CC_Stroke[i] <- 1}
    }
  }
}


###########################################
### Assigning those with Two Conditions ###
###########################################
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 2){
    if(df$Age[i] < 50){
      
      pref<-sample(c("A","B","C","D","E","F","G","H","I"),1,
                   prob=c(0.35,0.225,0.05,0.05,0.05,0.025,0.05,0.05,0.15))
      
      ################################
      if(pref == "A"){ref<-c("B","F")} # Asthma	COPD
      if(pref == "B"){ref<-c("M","N")} # Hyperlipidemia	Hypertension
      if(pref == "C"){ref<-c("J","O")} # Ischemic_Heart_Disease	Heart_Failure
      if(pref == "D"){ref<-c("S","J")} # Stroke	Heart_Failure
      if(pref == "E"){ref<-c("S","O")} # Stroke	Ischemic_Heart_Disease
      if(pref == "F"){ref<-c("H","R")} # Depression	Schizophrenia
      if(pref == "G"){ref<-c("C","J")} # Atrial_Fibrillation	Heart_Failure
      if(pref == "H"){ref<-c("N","O")} # Hypertension	Ischemic_Heart_Disease
      if(pref == "I"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      
      pref<-sample(c("A","B","C","D","E","F","G","H","I"),1,
                   prob=c(0.25,0.25,0.075,0.075,0.075,0.025,0.075,0.075,0.1))
      
      ################################
      if(pref == "A"){ref<-c("B","F")} # Asthma	COPD
      if(pref == "B"){ref<-c("M","N")} # Hyperlipidemia	Hypertension
      if(pref == "C"){ref<-c("J","O")} # Ischemic_Heart_Disease	Heart_Failure
      if(pref == "D"){ref<-c("S","J")} # Stroke	Heart_Failure
      if(pref == "E"){ref<-c("S","O")} # Stroke	Ischemic_Heart_Disease
      if(pref == "F"){ref<-c("H","R")} # Depression	Schizophrenia
      if(pref == "G"){ref<-c("C","J")} # Atrial_Fibrillation	Heart_Failure
      if(pref == "H"){ref<-c("N","O")} # Hypertension	Ischemic_Heart_Disease
      if(pref == "I"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 65){
      
      pref<-sample(c("A","B","C","D","E","F","G","H","I"),1,
                   prob=c(0.2,0.2,0.1,0.1,0.105,0.025,0.11,0.11,0.05))
      
      ################################
      if(pref == "A"){ref<-c("B","F")} # Asthma	COPD
      if(pref == "B"){ref<-c("M","N")} # Hyperlipidemia	Hypertension
      if(pref == "C"){ref<-c("J","O")} # Ischemic_Heart_Disease	Heart_Failure
      if(pref == "D"){ref<-c("S","J")} # Stroke	Heart_Failure
      if(pref == "E"){ref<-c("S","O")} # Stroke	Ischemic_Heart_Disease
      if(pref == "F"){ref<-c("H","R")} # Depression	Schizophrenia
      if(pref == "G"){ref<-c("C","J")} # Atrial_Fibrillation	Heart_Failure
      if(pref == "H"){ref<-c("N","O")} # Hypertension	Ischemic_Heart_Disease
      if(pref == "I"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                           0.053571429))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S"){df$CC_Stroke[i] <- 1}
    }
  }
}


#############################################
### Assigning those with Three Conditions ###
#############################################
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 3){
    if(df$Age[i] < 50){
      
      pref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N"),1,
                   prob=c(0.1,0.15,0.025,0.0025,0.0025,0.015,0.0025,0.025,0.015,0.015,0.015,0.15,0.45,0.0325))
      
      ################################
      if(pref == "A"){ref<-c("B","F", 
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0,	0.040816327,	0.081632653,	0.081632653,	0,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Asthma	COPD	Random
      
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0,	0,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Hyperlipidemia	Hypertension	Random
      
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Ischemic_Heart_Disease	Heart_Failure	Random
      
      if(pref == "D"){ref<-c("S","J", 
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0))
      )} # Stroke	Heart_Failure	Random
      
      if(pref == "E"){ref<-c("S","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0,	0.040816327,	0.06122449,	0.020408163,	0))
      )} # Stroke	Ischemic_Heart_Disease	Random
      
      if(pref == "F"){ref<-c("H","R",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0,	0.020408163))
      )} # Depression	Schizophrenia	Random
      
      if(pref == "G"){ref<-c("C","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Atrial_Fibrillation	Heart_Failure	Random
      
      if(pref == "H"){ref<-c("N","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0,	0,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Hypertension	Ischemic_Heart_Disease	Random
      
      if(pref == "I"){ref<-c("S","J","O")} # Stroke	Heart_Failure	Ischemic_Heart_Disease
      
      if(pref == "J"){ref<-c("C","J","O")} # Atrial_Fibrillation	Heart_Failure	Ischemic_Heart_Disease
      
      if(pref == "K"){ref<-c("C","J","S")} # Stroke	Heart_Failure	Atrial_Fibrillation
      
      if(pref == "L"){ref<-c("I","P",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0,	0.06122449,	0.020408163,	0.020408163))
      )} # Diabetes	Kidney_Disease	Random
      
      if(pref == "M"){ref<-c("B","H","S")} # Stroke	Asthma	Depression
      
      if(pref == "N"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A"| ref[3] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B"| ref[3] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C"| ref[3] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D"| ref[3] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E"| ref[3] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F"| ref[3] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G"| ref[3] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H"| ref[3] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I"| ref[3] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J"| ref[3] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K"| ref[3] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L"| ref[3] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M"| ref[3] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N"| ref[3] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O"| ref[3] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P"| ref[3] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q"| ref[3] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R"| ref[3] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S"| ref[3] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      
      pref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N"),1,
                   prob=c(0.08,0.15,0.05,0.005,0.005,0.015,0.005,0.05,0.05,0.05,0.05,0.2,0.15,0.14))
      
      
      ################################
      if(pref == "A"){ref<-c("B","F", 
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0,	0.063829787,	0.021276596,	0.085106383,	
                                           0,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Asthma	COPD	Random
      
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0,	0,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Hyperlipidemia	Hypertension	Random
      
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Ischemic_Heart_Disease	Heart_Failure	Random
      
      if(pref == "D"){ref<-c("S","J", 
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0))
      )} # Stroke	Heart_Failure	Random
      
      if(pref == "E"){ref<-c("S","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0,
                                           0.063829787,	0.063829787,	0.021276596,	0))
      )} # Stroke	Ischemic_Heart_Disease	Random
      
      if(pref == "F"){ref<-c("H","R",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0,	0.042553191))
      )} # Depression	Schizophrenia	Random
      
      if(pref == "G"){ref<-c("C","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Atrial_Fibrillation	Heart_Failure	Random
      
      if(pref == "H"){ref<-c("N","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0,	0,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Hypertension	Ischemic_Heart_Disease	Random
      
      if(pref == "I"){ref<-c("S","J","O")} # Stroke	Heart_Failure	Ischemic_Heart_Disease
      
      if(pref == "J"){ref<-c("C","J","O")} # Atrial_Fibrillation	Heart_Failure	Ischemic_Heart_Disease
      
      if(pref == "K"){ref<-c("C","J","S")} # Stroke	Heart_Failure	Atrial_Fibrillation
      
      if(pref == "L"){ref<-c("I","P",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0,	0.063829787,	0.021276596,	0.042553191))
      )} # Diabetes	Kidney_Disease	Random
      
      if(pref == "M"){ref<-c("B","H","S")} # Stroke	Asthma	Depression
      
      if(pref == "N"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A"| ref[3] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B"| ref[3] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C"| ref[3] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D"| ref[3] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E"| ref[3] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F"| ref[3] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G"| ref[3] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H"| ref[3] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I"| ref[3] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J"| ref[3] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K"| ref[3] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L"| ref[3] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M"| ref[3] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N"| ref[3] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O"| ref[3] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P"| ref[3] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q"| ref[3] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R"| ref[3] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S"| ref[3] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 65){
      
      pref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N"),1,
                   prob=c(0.06,0.12,0.1,0.01,0.01,0.015,0.01,0.075,0.075,0.075,0.075,0.25,0.1,0.025))
      
      
      ################################
      if(pref == "A"){ref<-c("B","F", 
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0,	0.071428571,	0.017857143,	0.071428571,	0,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Asthma	COPD	Random
      
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0,	0,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Hyperlipidemia	Hypertension	Random
      
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Ischemic_Heart_Disease	Heart_Failure	Random
      
      if(pref == "D"){ref<-c("S","J", 
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0))
      )} # Stroke	Heart_Failure	Random
      
      if(pref == "E"){ref<-c("S","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0,	0.053571429,	0.053571429,	0.017857143,
                                           0))
      )} # Stroke	Ischemic_Heart_Disease	Random
      
      if(pref == "F"){ref<-c("H","R",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0,
                                           0.053571429))
      )} # Depression	Schizophrenia	Random
      
      if(pref == "G"){ref<-c("C","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Atrial_Fibrillation	Heart_Failure	Random
      
      if(pref == "H"){ref<-c("N","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0,	0,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Hypertension	Ischemic_Heart_Disease	Random
      
      if(pref == "I"){ref<-c("S","J","O")} # Stroke	Heart_Failure	Ischemic_Heart_Disease
      
      if(pref == "J"){ref<-c("C","J","O")} # Atrial_Fibrillation	Heart_Failure	Ischemic_Heart_Disease
      
      if(pref == "K"){ref<-c("C","J","S")} # Stroke	Heart_Failure	Atrial_Fibrillation
      
      if(pref == "L"){ref<-c("I","P",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 1, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Diabetes	Kidney_Disease	Random
      
      if(pref == "M"){ref<-c("B","H","S")} # Stroke	Asthma	Depression
      
      if(pref == "N"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                           0.053571429))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A"| ref[3] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B"| ref[3] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C"| ref[3] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D"| ref[3] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E"| ref[3] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F"| ref[3] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G"| ref[3] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H"| ref[3] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I"| ref[3] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J"| ref[3] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K"| ref[3] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L"| ref[3] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M"| ref[3] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N"| ref[3] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O"| ref[3] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P"| ref[3] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q"| ref[3] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R"| ref[3] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S"| ref[3] == "S"){df$CC_Stroke[i] <- 1}
    }
  }
}

############################################
### Assigning those with four conditions ###
############################################
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 4){
    if(df$Age[i] < 50){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.2,0.1,0.1,0.5,0.1))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.020408163,	0,	0.040816327,	0.081632653,	0.081632653,	0,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0,	0,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.2,0.15,0.15,0.45,0.05))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.063829787, 0,	0.063829787,	0.021276596,	0.085106383,	
                                           0,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0,	0,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 65){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.1,0.2,0.25,0.4,0.05))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.071428571,	0,	0.071428571,	0.017857143,	0.071428571,	0,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0,	0,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 2, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                           0.053571429))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S"){df$CC_Stroke[i] <- 1}
    }
  }
}


############################################
### Assigning those with FIVE conditions ###
############################################
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 5){
    if(df$Age[i] < 50){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.2,0.1,0.1,0.5,0.1))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.020408163,	0,	0.040816327,	0.081632653,	0.081632653,	0,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0,	0,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 5, replace = FALSE,
                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.2,0.15,0.15,0.45,0.05))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.063829787, 0,	0.063829787,	0.021276596,	0.085106383,	
                                           0,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0,	0,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 5, replace = FALSE,
                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 65){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.1,0.2,0.25,0.4,0.05))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.071428571,	0,	0.071428571,	0.017857143,	0.071428571,	0,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0,	0,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 3, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 5, replace = FALSE,
                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                           0.053571429))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S"){df$CC_Stroke[i] <- 1}
    }
  }
}

###########################################
### Assigning those with SIX conditions ###
###########################################

set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 6){
    if(df$Age[i] < 50){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.2,0.1,0.1,0.5,0.1))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.020408163,	0,	0.040816327,	0.081632653,	0.081632653,	0,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                                           0,	0,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                                           0.020408163,	0.081632653,  0.06122449,	0,	0.040816327,	0.020408163,	
                                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 6, replace = FALSE,
                    prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                           0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                           0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.2,0.15,0.15,0.45,0.05))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.063829787, 0,	0.063829787,	0.021276596,	0.085106383,	
                                           0,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                                           0.021276596,	0.021276596,	0,	0,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0,
                                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0,
                                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                                           0.063829787,	0.063829787,	0.021276596,	0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 6, replace = FALSE,
                    prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                           0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                           0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                           0.063829787,	0.063829787,	0.021276596,	0.042553191))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S"){df$CC_Stroke[i] <- 1}
    }
    if(df$Age[i] >= 65){
      
      pref<-sample(c("A","B","C","D","E"),1,
                   prob=c(0.1,0.2,0.25,0.4,0.05))
      
      ################################
      if(pref == "A"){ref<-c("B","F",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.071428571,	0,	0.071428571,	0.017857143,	0.071428571,	0,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Asthma	COPD	Random	Random
      if(pref == "B"){ref<-c("M","N",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                                           0,	0,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Hyperlipidemia	Hypertension	Random	Random
      if(pref == "C"){ref<-c("J","O",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0,	0.053571429,	0.053571429,	0.017857143,
                                           0.053571429))
      )} # Diabetes	Kidney_Disease	Random	Random
      if(pref == "D"){ref<-c("S","J",
                             sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 4, replace = FALSE,
                                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                                           0.071428571,	0.071428571,	0.053571429,	0,	0.035714286,	0.017857143,
                                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                                           0))
      )} # Stroke	Asthma	Depression	Random
      if(pref == "E"){
        ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 6, replace = FALSE,
                    prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                           0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                           0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                           0.053571429))
      }
      ################################
      
      if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A"){df$CC_Arthritis[i] <- 1}
      if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B"){df$CC_Asthma[i] <- 1}
      if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
      if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D"){df$CC_Austism[i] <- 1}
      if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E"){df$CC_Cancer[i] <- 1}
      if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F"){df$CC_COPD[i] <- 1}
      if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G"){df$CC_Dementia[i] <- 1}
      if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H"){df$CC_Depression[i] <- 1}
      if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I"){df$CC_Diabetes[i] <- 1}
      if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J"){df$CC_Heart_Failure[i] <- 1}
      if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K"){df$CC_Hepatitis[i] <- 1}
      if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L"){df$CC_HIV_AIDS[i] <- 1}
      if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M"){df$CC_Hyperlipidemia[i] <- 1}
      if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N"){df$CC_Hypertension[i] <- 1}
      if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
      if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P"){df$CC_Kidney_Disease[i] <- 1}
      if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q"){df$CC_Osteoporosis[i] <- 1}
      if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R"){df$CC_Schizophrenia[i] <- 1}
      if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S"){df$CC_Stroke[i] <- 1}
    }
  }
}

#############################################
### Assigning those with SEVEN conditions ###
#############################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 7){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 7, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 7, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 7, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S"){df$CC_Stroke[i] <- 1}
  }
}

#############################################
### Assigning those with EIGHT conditions ###
#############################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 8){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 8, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 8, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 8, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S"){df$CC_Stroke[i] <- 1}
  }
}


############################################
### Assigning those with NINE conditions ###
############################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 9){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 9, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 9, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 9, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S"){df$CC_Stroke[i] <- 1}
  }
}

###########################################
### Assigning those with TEN conditions ###
###########################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 10){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 10, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 10, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 10, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S"){df$CC_Stroke[i] <- 1}
  }
}

##############################################
### Assigning those with ELEVEN conditions ###
##############################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 11){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 11, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 11, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 11, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S"){df$CC_Stroke[i] <- 1}
  }
}


##############################################
### Assigning those with TWELVE conditions ###
##############################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 12){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 12, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 12, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 12, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S"){df$CC_Stroke[i] <- 1}
  }
}


################################################
### Assigning those with THIRTEEN conditions ###
################################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 13){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 13, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 13, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 13, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A" | ref[13] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B" | ref[13] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C" | ref[13] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D" | ref[13] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E" | ref[13] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F" | ref[13] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G" | ref[13] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H" | ref[13] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I" | ref[13] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J" | ref[13] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K" | ref[13] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L" | ref[13] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M" | ref[13] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N" | ref[13] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O" | ref[13] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P" | ref[13] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q" | ref[13] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R" | ref[13] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S" | ref[13] == "S"){df$CC_Stroke[i] <- 1}
  }
}


################################################
### Assigning those with FOURTEEN conditions ###
################################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 14){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 14, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 14, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 14, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A" | ref[13] == "A" | ref[14] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B" | ref[13] == "B" | ref[14] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C" | ref[13] == "C" | ref[14] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D" | ref[13] == "D" | ref[14] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E" | ref[13] == "E" | ref[14] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F" | ref[13] == "F" | ref[14] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G" | ref[13] == "G" | ref[14] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H" | ref[13] == "H" | ref[14] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I" | ref[13] == "I" | ref[14] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J" | ref[13] == "J" | ref[14] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K" | ref[13] == "K" | ref[14] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L" | ref[13] == "L" | ref[14] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M" | ref[13] == "M" | ref[14] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N" | ref[13] == "N" | ref[14] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O" | ref[13] == "O" | ref[14] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P" | ref[13] == "P" | ref[14] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q" | ref[13] == "Q" | ref[14] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R" | ref[13] == "R" | ref[14] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S" | ref[13] == "S" | ref[14] == "S"){df$CC_Stroke[i] <- 1}
  }
}



###############################################
### Assigning those with FIFTEEN conditions ###
###############################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 15){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 15, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 15, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 15, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A" | ref[13] == "A" | ref[14] == "A" | ref[15] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B" | ref[13] == "B" | ref[14] == "B" | ref[15] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C" | ref[13] == "C" | ref[14] == "C" | ref[15] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D" | ref[13] == "D" | ref[14] == "D" | ref[15] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E" | ref[13] == "E" | ref[14] == "E" | ref[15] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F" | ref[13] == "F" | ref[14] == "F" | ref[15] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G" | ref[13] == "G" | ref[14] == "G" | ref[15] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H" | ref[13] == "H" | ref[14] == "H" | ref[15] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I" | ref[13] == "I" | ref[14] == "I" | ref[15] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J" | ref[13] == "J" | ref[14] == "J" | ref[15] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K" | ref[13] == "K" | ref[14] == "K" | ref[15] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L" | ref[13] == "L" | ref[14] == "L" | ref[15] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M" | ref[13] == "M" | ref[14] == "M" | ref[15] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N" | ref[13] == "N" | ref[14] == "N" | ref[15] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O" | ref[13] == "O" | ref[14] == "O" | ref[15] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P" | ref[13] == "P" | ref[14] == "P" | ref[15] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q" | ref[13] == "Q" | ref[14] == "Q" | ref[15] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R" | ref[13] == "R" | ref[14] == "R" | ref[15] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S" | ref[13] == "S" | ref[14] == "S" | ref[15] == "S"){df$CC_Stroke[i] <- 1}
  }
}


###############################################
### Assigning those with SIXTEEN conditions ###
###############################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 16){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 16, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 16, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 16, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A" | ref[13] == "A" | ref[14] == "A" | ref[15] == "A" | ref[16] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B" | ref[13] == "B" | ref[14] == "B" | ref[15] == "B" | ref[16] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C" | ref[13] == "C" | ref[14] == "C" | ref[15] == "C" | ref[16] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D" | ref[13] == "D" | ref[14] == "D" | ref[15] == "D" | ref[16] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E" | ref[13] == "E" | ref[14] == "E" | ref[15] == "E" | ref[16] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F" | ref[13] == "F" | ref[14] == "F" | ref[15] == "F" | ref[16] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G" | ref[13] == "G" | ref[14] == "G" | ref[15] == "G" | ref[16] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H" | ref[13] == "H" | ref[14] == "H" | ref[15] == "H" | ref[16] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I" | ref[13] == "I" | ref[14] == "I" | ref[15] == "I" | ref[16] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J" | ref[13] == "J" | ref[14] == "J" | ref[15] == "J" | ref[16] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K" | ref[13] == "K" | ref[14] == "K" | ref[15] == "K" | ref[16] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L" | ref[13] == "L" | ref[14] == "L" | ref[15] == "L" | ref[16] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M" | ref[13] == "M" | ref[14] == "M" | ref[15] == "M" | ref[16] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N" | ref[13] == "N" | ref[14] == "N" | ref[15] == "N" | ref[16] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O" | ref[13] == "O" | ref[14] == "O" | ref[15] == "O" | ref[16] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P" | ref[13] == "P" | ref[14] == "P" | ref[15] == "P" | ref[16] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q" | ref[13] == "Q" | ref[14] == "Q" | ref[15] == "Q" | ref[16] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R" | ref[13] == "R" | ref[14] == "R" | ref[15] == "R" | ref[16] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S" | ref[13] == "S" | ref[14] == "S" | ref[15] == "S" | ref[16] == "S"){df$CC_Stroke[i] <- 1}
  }
}


#################################################
### Assigning those with SEVENTEEN conditions ###
#################################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 17){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 17, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 17, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 17, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A" | ref[13] == "A" | ref[14] == "A" | ref[15] == "A" | ref[16] == "A" | ref[17] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B" | ref[13] == "B" | ref[14] == "B" | ref[15] == "B" | ref[16] == "B" | ref[17] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C" | ref[13] == "C" | ref[14] == "C" | ref[15] == "C" | ref[16] == "C" | ref[17] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D" | ref[13] == "D" | ref[14] == "D" | ref[15] == "D" | ref[16] == "D" | ref[17] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E" | ref[13] == "E" | ref[14] == "E" | ref[15] == "E" | ref[16] == "E" | ref[17] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F" | ref[13] == "F" | ref[14] == "F" | ref[15] == "F" | ref[16] == "F" | ref[17] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G" | ref[13] == "G" | ref[14] == "G" | ref[15] == "G" | ref[16] == "G" | ref[17] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H" | ref[13] == "H" | ref[14] == "H" | ref[15] == "H" | ref[16] == "H" | ref[17] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I" | ref[13] == "I" | ref[14] == "I" | ref[15] == "I" | ref[16] == "I" | ref[17] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J" | ref[13] == "J" | ref[14] == "J" | ref[15] == "J" | ref[16] == "J" | ref[17] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K" | ref[13] == "K" | ref[14] == "K" | ref[15] == "K" | ref[16] == "K" | ref[17] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L" | ref[13] == "L" | ref[14] == "L" | ref[15] == "L" | ref[16] == "L" | ref[17] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M" | ref[13] == "M" | ref[14] == "M" | ref[15] == "M" | ref[16] == "M" | ref[17] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N" | ref[13] == "N" | ref[14] == "N" | ref[15] == "N" | ref[16] == "N" | ref[17] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O" | ref[13] == "O" | ref[14] == "O" | ref[15] == "O" | ref[16] == "O" | ref[17] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P" | ref[13] == "P" | ref[14] == "P" | ref[15] == "P" | ref[16] == "P" | ref[17] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q" | ref[13] == "Q" | ref[14] == "Q" | ref[15] == "Q" | ref[16] == "Q" | ref[17] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R" | ref[13] == "R" | ref[14] == "R" | ref[15] == "R" | ref[16] == "R" | ref[17] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S" | ref[13] == "S" | ref[14] == "S" | ref[15] == "S" | ref[16] == "S" | ref[17] == "S"){df$CC_Stroke[i] <- 1}
  }
}


################################################
### Assigning those with EIGHTEEN conditions ###
################################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 18){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 18, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 18, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 18, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A" | ref[13] == "A" | ref[14] == "A" | ref[15] == "A" | ref[16] == "A" | ref[17] == "A" | ref[18] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B" | ref[13] == "B" | ref[14] == "B" | ref[15] == "B" | ref[16] == "B" | ref[17] == "B" | ref[18] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C" | ref[13] == "C" | ref[14] == "C" | ref[15] == "C" | ref[16] == "C" | ref[17] == "C" | ref[18] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D" | ref[13] == "D" | ref[14] == "D" | ref[15] == "D" | ref[16] == "D" | ref[17] == "D" | ref[18] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E" | ref[13] == "E" | ref[14] == "E" | ref[15] == "E" | ref[16] == "E" | ref[17] == "E" | ref[18] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F" | ref[13] == "F" | ref[14] == "F" | ref[15] == "F" | ref[16] == "F" | ref[17] == "F" | ref[18] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G" | ref[13] == "G" | ref[14] == "G" | ref[15] == "G" | ref[16] == "G" | ref[17] == "G" | ref[18] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H" | ref[13] == "H" | ref[14] == "H" | ref[15] == "H" | ref[16] == "H" | ref[17] == "H" | ref[18] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I" | ref[13] == "I" | ref[14] == "I" | ref[15] == "I" | ref[16] == "I" | ref[17] == "I" | ref[18] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J" | ref[13] == "J" | ref[14] == "J" | ref[15] == "J" | ref[16] == "J" | ref[17] == "J" | ref[18] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K" | ref[13] == "K" | ref[14] == "K" | ref[15] == "K" | ref[16] == "K" | ref[17] == "K" | ref[18] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L" | ref[13] == "L" | ref[14] == "L" | ref[15] == "L" | ref[16] == "L" | ref[17] == "L" | ref[18] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M" | ref[13] == "M" | ref[14] == "M" | ref[15] == "M" | ref[16] == "M" | ref[17] == "M" | ref[18] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N" | ref[13] == "N" | ref[14] == "N" | ref[15] == "N" | ref[16] == "N" | ref[17] == "N" | ref[18] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O" | ref[13] == "O" | ref[14] == "O" | ref[15] == "O" | ref[16] == "O" | ref[17] == "O" | ref[18] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P" | ref[13] == "P" | ref[14] == "P" | ref[15] == "P" | ref[16] == "P" | ref[17] == "P" | ref[18] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q" | ref[13] == "Q" | ref[14] == "Q" | ref[15] == "Q" | ref[16] == "Q" | ref[17] == "Q" | ref[18] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R" | ref[13] == "R" | ref[14] == "R" | ref[15] == "R" | ref[16] == "R" | ref[17] == "R" | ref[18] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S" | ref[13] == "S" | ref[14] == "S" | ref[15] == "S" | ref[16] == "S" | ref[17] == "S" | ref[18] == "S"){df$CC_Stroke[i] <- 1}
  }
}


################################################
### Assigning those with NINETEEN conditions ###
################################################
# Note this is all random now
set.seed(4)
for(i in 1:nrow(df)){
  if(df$CC_Count[i] == 19){
    if(df$Age[i] < 50){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 19, replace = FALSE,
                  prob=c(0.020408163,	0.081632653,	0.040816327,	0.081632653,	0.081632653,	0.06122449,	
                         0.020408163,	0.081632653,  0.06122449,	0.040816327,	0.040816327,	0.020408163,	
                         0.081632653,	0.081632653,	0.06122449,	0.040816327,	0.06122449,	0.020408163,	0.020408163))
    }
    if(df$Age[i] >= 50 & df$Age[i] < 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 19, replace = FALSE,
                  prob=c(0.063829787, 0.063829787,	0.063829787,	0.021276596,	0.085106383,	
                         0.063829787,	0.042553191,	0.085106383,	0.042553191,	0.042553191,
                         0.021276596,	0.021276596,	0.063829787,	0.063829787,	0.063829787,
                         0.063829787,	0.063829787,	0.021276596,	0.042553191))
    }
    if(df$Age[i] >= 65){
      ref<-sample(c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S"), 19, replace = FALSE,
                  prob=c(0.071428571,	0.053571429,	0.071428571,	0.017857143,	0.071428571,	0.053571429,
                         0.071428571,	0.071428571,	0.053571429,	0.071428571,	0.035714286,	0.017857143,
                         0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.053571429,	0.017857143,
                         0.053571429))
    }
    if(ref[1] == "A" | ref[2] == "A" | ref[3] == "A" | ref[4] == "A" | ref[5] == "A" | ref[6] == "A" | ref[7] == "A" | ref[8] == "A" | ref[9] == "A" | ref[10] == "A" | ref[11] == "A" | ref[12] == "A" | ref[13] == "A" | ref[14] == "A" | ref[15] == "A" | ref[16] == "A" | ref[17] == "A" | ref[18] == "A" | ref[19] == "A"){df$CC_Arthritis[i] <- 1}
    if(ref[1] == "B" | ref[2] == "B" | ref[3] == "B" | ref[4] == "B" | ref[5] == "B" | ref[6] == "B" | ref[7] == "B" | ref[8] == "B" | ref[9] == "B" | ref[10] == "B" | ref[11] == "B" | ref[12] == "B" | ref[13] == "B" | ref[14] == "B" | ref[15] == "B" | ref[16] == "B" | ref[17] == "B" | ref[18] == "B" | ref[19] == "B"){df$CC_Asthma[i] <- 1}
    if(ref[1] == "C" | ref[2] == "C" | ref[3] == "C" | ref[4] == "C" | ref[5] == "C" | ref[6] == "C" | ref[7] == "C" | ref[8] == "C" | ref[9] == "C" | ref[10] == "C" | ref[11] == "C" | ref[12] == "C" | ref[13] == "C" | ref[14] == "C" | ref[15] == "C" | ref[16] == "C" | ref[17] == "C" | ref[18] == "C" | ref[19] == "C"){df$CC_Atrial_Fibrillation[i] <- 1}
    if(ref[1] == "D" | ref[2] == "D" | ref[3] == "D" | ref[4] == "D" | ref[5] == "D" | ref[6] == "D" | ref[7] == "D" | ref[8] == "D" | ref[9] == "D" | ref[10] == "D" | ref[11] == "D" | ref[12] == "D" | ref[13] == "D" | ref[14] == "D" | ref[15] == "D" | ref[16] == "D" | ref[17] == "D" | ref[18] == "D" | ref[19] == "D"){df$CC_Austism[i] <- 1}
    if(ref[1] == "E" | ref[2] == "E" | ref[3] == "E" | ref[4] == "E" | ref[5] == "E" | ref[6] == "E" | ref[7] == "E" | ref[8] == "E" | ref[9] == "E" | ref[10] == "E" | ref[11] == "E" | ref[12] == "E" | ref[13] == "E" | ref[14] == "E" | ref[15] == "E" | ref[16] == "E" | ref[17] == "E" | ref[18] == "E" | ref[19] == "E"){df$CC_Cancer[i] <- 1}
    if(ref[1] == "F" | ref[2] == "F" | ref[3] == "F" | ref[4] == "F" | ref[5] == "F" | ref[6] == "F" | ref[7] == "F" | ref[8] == "F" | ref[9] == "F" | ref[10] == "F" | ref[11] == "F" | ref[12] == "F" | ref[13] == "F" | ref[14] == "F" | ref[15] == "F" | ref[16] == "F" | ref[17] == "F" | ref[18] == "F" | ref[19] == "F"){df$CC_COPD[i] <- 1}
    if(ref[1] == "G" | ref[2] == "G" | ref[3] == "G" | ref[4] == "G" | ref[5] == "G" | ref[6] == "G" | ref[7] == "G" | ref[8] == "G" | ref[9] == "G" | ref[10] == "G" | ref[11] == "G" | ref[12] == "G" | ref[13] == "G" | ref[14] == "G" | ref[15] == "G" | ref[16] == "G" | ref[17] == "G" | ref[18] == "G" | ref[19] == "G"){df$CC_Dementia[i] <- 1}
    if(ref[1] == "H" | ref[2] == "H" | ref[3] == "H" | ref[4] == "H" | ref[5] == "H" | ref[6] == "H" | ref[7] == "H" | ref[8] == "H" | ref[9] == "H" | ref[10] == "H" | ref[11] == "H" | ref[12] == "H" | ref[13] == "H" | ref[14] == "H" | ref[15] == "H" | ref[16] == "H" | ref[17] == "H" | ref[18] == "H" | ref[19] == "H"){df$CC_Depression[i] <- 1}
    if(ref[1] == "I" | ref[2] == "I" | ref[3] == "I" | ref[4] == "I" | ref[5] == "I" | ref[6] == "I" | ref[7] == "I" | ref[8] == "I" | ref[9] == "I" | ref[10] == "I" | ref[11] == "I" | ref[12] == "I" | ref[13] == "I" | ref[14] == "I" | ref[15] == "I" | ref[16] == "I" | ref[17] == "I" | ref[18] == "I" | ref[19] == "I"){df$CC_Diabetes[i] <- 1}
    if(ref[1] == "J" | ref[2] == "J" | ref[3] == "J" | ref[4] == "J" | ref[5] == "J" | ref[6] == "J" | ref[7] == "J" | ref[8] == "J" | ref[9] == "J" | ref[10] == "J" | ref[11] == "J" | ref[12] == "J" | ref[13] == "J" | ref[14] == "J" | ref[15] == "J" | ref[16] == "J" | ref[17] == "J" | ref[18] == "J" | ref[19] == "J"){df$CC_Heart_Failure[i] <- 1}
    if(ref[1] == "K" | ref[2] == "K" | ref[3] == "K" | ref[4] == "K" | ref[5] == "K" | ref[6] == "K" | ref[7] == "K" | ref[8] == "K" | ref[9] == "K" | ref[10] == "K" | ref[11] == "K" | ref[12] == "K" | ref[13] == "K" | ref[14] == "K" | ref[15] == "K" | ref[16] == "K" | ref[17] == "K" | ref[18] == "K" | ref[19] == "K"){df$CC_Hepatitis[i] <- 1}
    if(ref[1] == "L" | ref[2] == "L" | ref[3] == "L" | ref[4] == "L" | ref[5] == "L" | ref[6] == "L" | ref[7] == "L" | ref[8] == "L" | ref[9] == "L" | ref[10] == "L" | ref[11] == "L" | ref[12] == "L" | ref[13] == "L" | ref[14] == "L" | ref[15] == "L" | ref[16] == "L" | ref[17] == "L" | ref[18] == "L" | ref[19] == "L"){df$CC_HIV_AIDS[i] <- 1}
    if(ref[1] == "M" | ref[2] == "M" | ref[3] == "M" | ref[4] == "M" | ref[5] == "M" | ref[6] == "M" | ref[7] == "M" | ref[8] == "M" | ref[9] == "M" | ref[10] == "M" | ref[11] == "M" | ref[12] == "M" | ref[13] == "M" | ref[14] == "M" | ref[15] == "M" | ref[16] == "M" | ref[17] == "M" | ref[18] == "M" | ref[19] == "M"){df$CC_Hyperlipidemia[i] <- 1}
    if(ref[1] == "N" | ref[2] == "N" | ref[3] == "N" | ref[4] == "N" | ref[5] == "N" | ref[6] == "N" | ref[7] == "N" | ref[8] == "N" | ref[9] == "N" | ref[10] == "N" | ref[11] == "N" | ref[12] == "N" | ref[13] == "N" | ref[14] == "N" | ref[15] == "N" | ref[16] == "N" | ref[17] == "N" | ref[18] == "N" | ref[19] == "N"){df$CC_Hypertension[i] <- 1}
    if(ref[1] == "O" | ref[2] == "O" | ref[3] == "O" | ref[4] == "O" | ref[5] == "O" | ref[6] == "O" | ref[7] == "O" | ref[8] == "O" | ref[9] == "O" | ref[10] == "O" | ref[11] == "O" | ref[12] == "O" | ref[13] == "O" | ref[14] == "O" | ref[15] == "O" | ref[16] == "O" | ref[17] == "O" | ref[18] == "O" | ref[19] == "O"){df$CC_Ischemic_Heart_Disease[i] <- 1}
    if(ref[1] == "P" | ref[2] == "P" | ref[3] == "P" | ref[4] == "P" | ref[5] == "P" | ref[6] == "P" | ref[7] == "P" | ref[8] == "P" | ref[9] == "P" | ref[10] == "P" | ref[11] == "P" | ref[12] == "P" | ref[13] == "P" | ref[14] == "P" | ref[15] == "P" | ref[16] == "P" | ref[17] == "P" | ref[18] == "P" | ref[19] == "P"){df$CC_Kidney_Disease[i] <- 1}
    if(ref[1] == "Q" | ref[2] == "Q" | ref[3] == "Q" | ref[4] == "Q" | ref[5] == "Q" | ref[6] == "Q" | ref[7] == "Q" | ref[8] == "Q" | ref[9] == "Q" | ref[10] == "Q" | ref[11] == "Q" | ref[12] == "Q" | ref[13] == "Q" | ref[14] == "Q" | ref[15] == "Q" | ref[16] == "Q" | ref[17] == "Q" | ref[18] == "Q" | ref[19] == "Q"){df$CC_Osteoporosis[i] <- 1}
    if(ref[1] == "R" | ref[2] == "R" | ref[3] == "R" | ref[4] == "R" | ref[5] == "R" | ref[6] == "R" | ref[7] == "R" | ref[8] == "R" | ref[9] == "R" | ref[10] == "R" | ref[11] == "R" | ref[12] == "R" | ref[13] == "R" | ref[14] == "R" | ref[15] == "R" | ref[16] == "R" | ref[17] == "R" | ref[18] == "R" | ref[19] == "R"){df$CC_Schizophrenia[i] <- 1}
    if(ref[1] == "S" | ref[2] == "S" | ref[3] == "S" | ref[4] == "S" | ref[5] == "S" | ref[6] == "S" | ref[7] == "S" | ref[8] == "S" | ref[9] == "S" | ref[10] == "S" | ref[11] == "S" | ref[12] == "S" | ref[13] == "S" | ref[14] == "S" | ref[15] == "S" | ref[16] == "S" | ref[17] == "S" | ref[18] == "S" | ref[19] == "S"){df$CC_Stroke[i] <- 1}
  }
}


###################################################
### Adding in Copay Amounts from Benefit Design ### 
###################################################

# These will vary between $75 & $150

# No relationship between copay and count for expensive people
# Otherwise, there is a relationship between copay and count

#df$ER_Copay<-NA
#for(i in 1:nrow(df)){
#  pos<-df$paid_rank[i]
#  if(pos < bot2){
#    if(df$ER_Count[i] > 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(.43,.37,.2))
#    }
#    if(df$ER_Count[i] == 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.2,.37,.43))
#    }
#  }
#  
#  if(pos >= bot2 & pos < bot3){
#    if(df$ER_Count[i] > 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(.43,.37,.2))
#    }
#    if(df$ER_Count[i] == 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.2,.37,.43))
#    }
#  }
#  
#  if(pos >= bot3 & pos < bot4){
#    if(df$ER_Count[i] > 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(.4,.38,.22))
#    }
#    if(df$ER_Count[i] == 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.22,.38,.4))
#    }
#  }
#  
#  if(pos >= bot4 & pos < bot5){
#    if(df$ER_Count[i] > 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.38,.38,.24))
#    }
#    if(df$ER_Count[i] == 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.24,.38,.38))
#    }
#  }
#  
#  if(pos >= bot5 & pos < bot6){
#    if(df$ER_Count[i] > 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.33,.33,.33))
#    }
#    if(df$ER_Count[i] == 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.33,.33,.33))
#    }
#  }
#  
#  if(pos >= bot6 & pos < bot7){
#    if(df$ER_Count[i] > 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.33,.33,.33))
#    }
#    if(df$ER_Count[i] == 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.33,.33,.33))
#    }
#  }
#  
#  if(pos >= bot7){
#    if(df$ER_Count[i] > 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.33,.33,.33))
#    }
#    if(df$ER_Count[i] == 0){
#      df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.33,.33,.33))
#    }
#  }
#}

# No structure or relationships for this
df$ER_Copay<-NA
for(i in 1:nrow(df)){
  df$ER_Copay[i]<-sample(c("$75", "$100", "$150"), 1, prob=c(0.3,.33,.36))
}

# No structure or relationships for this
df$PCP_Copay<-NA
for(i in 1:nrow(df)){
  if(df$ER_Copay[i] == "$75"){
    df$PCP_Copay[i] <- "$20"
  }
  if(df$ER_Copay[i] == "$100"){
    df$PCP_Copay[i] <- "$35"
  }
  if(df$ER_Copay[i] == "$150"){
    df$PCP_Copay[i] <- "$50"
  }
}



#########################################
### Adding Number of Inpatient Visits ###
#########################################
deciles<-quantile(df$HP_Paid, c(.1, .2, .3, .4, .5, .6, .7, .8, .9, 1))

df$IP_Visits<-NA
for(i in 1:nrow(df)){
  val<-df$HP_Paid[i]
  if(val <= deciles[1]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.95, 0.04, 0.005, 0.005, 0, 0, 0, 0, 0, 0, 0))
  }
  if(val > deciles[1] & val <= deciles[2]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.92, 0.07, 0.005, 0.005, 0, 0, 0, 0, 0, 0, 0))
  }
  if(val > deciles[2] & val <= deciles[3]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.90, 0.09, 0.005, 0.005, 0, 0, 0, 0, 0, 0, 0))
  }
  if(val > deciles[3] & val <= deciles[4]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.87, 0.1, 0.015, 0.015, 0, 0, 0, 0, 0, 0, 0))
  }
  if(val > deciles[4] & val <= deciles[5]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.84, 0.12, 0.02, 0.02, 0, 0, 0, 0, 0, 0, 0))
  }
  if(val > deciles[5] & val <= deciles[6]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.82, 0.12, 0.02, 0.02, 0.02, 0, 0, 0, 0, 0, 0))
  }
  if(val > deciles[6] & val <= deciles[7]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.53, 0.22, 0.12, 0.05, 0.05, 0.01, 0.01, 0, 0, 0, 0))
  }
  if(val > deciles[7] & val <= deciles[8]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.45, 0.29, 0.17, 0.15, 0.05, 0.01, 0.01, 0.01, 0, 0, 0))
  }
  if(val > deciles[8] & val <= deciles[9]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.15, 0.29, 0.27, 0.25, 0.05, 0.01, 0.01, 0.01, 0.01, 0.01, 0))
  }
  if(val > deciles[9] & val < deciles[10]){
    df$IP_Visits[i]<-sample(c(0,1,2,3,4,5,6,7,8,9,10), 1, prob = c(0.01, 0.10, 0.37, 0.35, 0.05, 0.05, 0.05, 0.05, 0.05, 0.02, 0.01))
  }
  if(val == deciles[10]){df$IP_Visits[i]<-11}
}



###################################################################
### ADDING IN PEOPLE WITH RICH BENEFITS AND HIGH ER UTILIZATION ###
###################################################################
# Randomly grabbing 500 patients and increasing their average ER_Count and average cost
library(dplyr)
Rich<-df[sample(nrow(df), 5000), ]

Rich$ER_Copay<-"$25"
Rich$PCP_Copay<-"$25"

for(i in 1:nrow(Rich)){
  ERFactor<-sample(c(0,1,2,3),1, prob = c(0.05, 0.55, 0.3, 0.1))
  #CostFactor<-sample(c(645.25, 797.22, 850.44, 1184.19),1,prob=c(0.25,0.25,0.25,0.25))
  CostFactor<-sample(c(745.25, 897.22, 950.44, 1484.19),1,prob=c(0.25,0.25,0.25,0.25))
  Rich$ER_Count[i]<-Rich$ER_Count[i]+ERFactor
  Rich$HP_Paid[i]<-Rich$HP_Paid[i]+ERFactor*CostFactor
}

# Delete all rows with patient IDs from df and then append Rich
richList<-Rich$PatientID
indexList<-c()
for(i in 1:nrow(df)){
  if(df$PatientID[i] %in% richList){
    indexList<-c(indexList,i)
  }
}
df<-df[-indexList,]
df<-rbind(df,Rich)

# Re-ordering the Copays for Visuals
df$ER_Copay<-factor(df$ER_Copay,c("$25", "$75", "$100", "$150"))
df$PCP_Copay<-factor(df$PCP_Copay,c("$20", "$25", "$35", "$50"))



##############################
### END DATAFRAME CREATION ###
##############################
