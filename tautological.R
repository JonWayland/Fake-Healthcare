#################################################################
### CODE FOR EXPLAINING TAUTOLOGICAL BIAS IN A QUORA QUESTION ###
#################################################################
# Tautological Bias Example
# Written by Jon Wayland
 
library(tidyverse)
 
# Fake Healthcare
dat <- read.csv("https://raw.githubusercontent.com/JonWayland/Fake-Healthcare/master/HP-Universal_DF.csv")
 
# Creating the Decile field
dat <- dat %>%
  arrange(HP_Paid) %>%
  mutate(cum_per = cumsum(HP_Paid)/sum(dat$HP_Paid),
         act_per = HP_Paid/sum(dat$HP_Paid)) %>%
  mutate(Decile = case_when(
    cum_per <= 0.1 ~ 1,
    cum_per <= 0.2 & cum_per > 0.1 ~ 2,
    cum_per <= 0.3 & cum_per > 0.2 ~ 3,
    cum_per <= 0.4 & cum_per > 0.3 ~ 4,
    cum_per <= 0.5 & cum_per > 0.4 ~ 5,
    cum_per <= 0.6 & cum_per > 0.5 ~ 6,
    cum_per <= 0.7 & cum_per > 0.6 ~ 7,
    cum_per <= 0.8 & cum_per > 0.7 ~ 8,
    cum_per <= 0.9 & cum_per > 0.8 ~ 9,
    TRUE ~ 10
  )) %>%
  mutate(Decile = as.factor(Decile))
 
# Splitting the data
library(caret)
trainIndex <- createDataPartition(dat$Decile, p = .8,
                                  list = FALSE,
                                  times = 1)
 
train <- dat[ trainIndex,]
test  <- dat[-trainIndex,]
 
summary(train$Decile)
summary(test$Decile)
 
# Looking at the outcome
train %>%
  group_by(Decile) %>%
  summarize(N = n()) %>%
  ggplot(aes(x = Decile, y = N))+
  geom_bar(stat = "Identity", color = "black", fill = "red4") +
  scale_y_continuous(name = "Total Number of Patients", labels = scales::comma)+
  ggtitle("Distribution of the Decile Field")+
  geom_text(aes(label = scales::percent(N/nrow(train))), nudge_y = 150)+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
 
 
# Fitting basic random forest model
library(randomForest)
fit <- randomForest(Decile ~ Age + Gender + CC_Count + Risk_Count + ER_Count + IP_Visits + HP_Paid,
                    mtry = 3,
                    ntree = 150,
                    data = train)
 
# Making predictions on the test set
test$pred <- predict(fit, newdata = test)
 
# Assessing the performance
perf <- confusionMatrix(test$pred, test$Decile)
 
# Formatting the confusion matrix data
df_conf <- rbind(
  data.frame(Reference = 1, Value = perf$table[,1], Prediction = seq(1,10)),
  data.frame(Reference = 2, Value = perf$table[,2], Prediction = seq(1,10)),
  data.frame(Reference = 3, Value = perf$table[,3], Prediction = seq(1,10)),
  data.frame(Reference = 4, Value = perf$table[,4], Prediction = seq(1,10)),
  data.frame(Reference = 5, Value = perf$table[,5], Prediction = seq(1,10)),
  data.frame(Reference = 6, Value = perf$table[,6], Prediction = seq(1,10)),
  data.frame(Reference = 7, Value = perf$table[,7], Prediction = seq(1,10)),
  data.frame(Reference = 8, Value = perf$table[,8], Prediction = seq(1,10)),
  data.frame(Reference = 9, Value = perf$table[,9], Prediction = seq(1,10)),
  data.frame(Reference = 10, Value = perf$table[,10], Prediction = seq(1,10))
)
 
# Plotting the confusion matrix
df_conf %>%
  ggplot(aes(x = as.factor(Reference), y = as.factor(Prediction), fill = Value))+
  geom_tile()+
  scale_x_discrete(name = "Actual Decile")+
  scale_y_discrete(name = "Predicted Decile")+
  scale_fill_gradient2(name = "Total Patients", low="white", high="red", labels = scales::comma)+
  theme_bw()
 
# Seeing the overall accuracy
perf$overall
 
# Plotting the decile by the paid amounts
train %>%
  ggplot(aes(Decile, HP_Paid))+
  geom_point(size = 2, alpha = 0.7, fill = "lightgreen", pch = 21)+
  scale_y_continuous(name = "Total Paid", labels = scales::dollar)+
  theme_bw()
 
# Seeing the correlation between decile and paid amounts
cor(as.numeric(dat$Decile), dat$HP_Paid)
 
# Re-training without paid
fit <- randomForest(Decile ~ Age + Gender + CC_Count + Risk_Count + ER_Count + IP_Visits,
                    mtry = 3,
                    ntree = 150,
                    data = train)
 
# Making the new predictions
test$pred <- predict(fit, newdata = test)
 
# Getting the performance of the predictions
perf <- confusionMatrix(test$pred, test$Decile)
 
# Formatting the confusion matrix data
df_conf <- rbind(
  data.frame(Reference = 1, Value = perf$table[,1], Prediction = seq(1,10)),
 data.frame(Reference = 2, Value = perf$table[,2], Prediction = seq(1,10)),
  data.frame(Reference = 3, Value = perf$table[,3], Prediction = seq(1,10)),
  data.frame(Reference = 4, Value = perf$table[,4], Prediction = seq(1,10)),
  data.frame(Reference = 5, Value = perf$table[,5], Prediction = seq(1,10)),
  data.frame(Reference = 6, Value = perf$table[,6], Prediction = seq(1,10)),
  data.frame(Reference = 7, Value = perf$table[,7], Prediction = seq(1,10)),
  data.frame(Reference = 8, Value = perf$table[,8], Prediction = seq(1,10)),
  data.frame(Reference = 9, Value = perf$table[,9], Prediction = seq(1,10)),
  data.frame(Reference = 10, Value = perf$table[,10], Prediction = seq(1,10))
)
 
# Plotting the confusion matrix
df_conf %>%
  ggplot(aes(x = as.factor(Reference), y = as.factor(Prediction), fill = Value))+
  geom_tile()+
  scale_x_discrete(name = "Actual Decile")+
  scale_y_discrete(name = "Predicted Decile")+
  scale_fill_gradient2(name = "Total Patients", low="white", high="red", labels = scales::comma)+
  theme_bw()
 
# Seeing the overall accuracy
perf$overall
 
