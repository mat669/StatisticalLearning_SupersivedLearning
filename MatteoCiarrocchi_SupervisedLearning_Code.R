library(plyr)  
library(caret)
library(gridExtra) 
library(tidyverse) 
library(rsample)
library(e1071) 
library(GGally)
library(data.table)
library(DT)
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
library(rms)
library(MASS)
library(e1071)
library(ROCR)
library(gplots)
library(pROC)
library(ggpubr)

airlines <- read.csv("C:\\Users\\Utente\\OneDrive\\Desktop\\SL exam\\airline_passenger_satisfaction.csv")
airlines['X'] <- NULL

# visualize dataset
str(airlines)
head(airlines)

# Data preparation
airlines_clean <- airlines[complete.cases(airlines), ]
airlines_clean$satisfaction<-as.factor(airlines_clean$satisfaction)

##### primo tentativo (no factor)
# Data description
t=ggplot(airlines_clean, aes(x = satisfaction)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
t


#Customer categorical features and travel features
# Gender
t1 <- ggplot(airlines_clean, aes(x = Gender)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Customer type
t2 <- ggplot(airlines_clean, aes(x = customer_type)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Type of travel
t3 <- ggplot(airlines_clean, aes(x = type_of_travel)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

#Customer class
t4 <- ggplot(airlines_clean, aes(x = customer_class)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)



#Plot data within a grid
grid.arrange(t1, t2, t3, t4, ncol=1)



# categorical variables evaluated by customers between 0 and 5 about fly experience

p1 <- ggplot(airlines_clean, aes(x = inflight_wifi_service)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p2 <- ggplot(airlines_clean, aes(x = departure_arrival_time_convenient)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p3 <- ggplot(airlines_clean, aes(x = ease_of_online_booking)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p4 <- ggplot(airlines_clean, aes(x = gate_location)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p5 <- ggplot(airlines_clean, aes(x = food_and_drink)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p6 <- ggplot(airlines_clean, aes(x = online_boarding)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p7 <- ggplot(airlines_clean, aes(x = seat_comfort)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)
p8 <- ggplot(airlines_clean, aes(x = inflight_entertainment)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p9 <- ggplot(airlines_clean, aes(x = onboard_service)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p10 <- ggplot(airlines_clean, aes(x = leg_room_service)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p11 <- ggplot(airlines_clean, aes(x = baggage_handling)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p12 <- ggplot(airlines_clean, aes(x = checkin_service)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p13 <- ggplot(airlines_clean, aes(x = inflight_service)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

p14 <- ggplot(airlines_clean, aes(x = cleanliness)) +
  geom_bar(aes(fill = satisfaction)) +
  geom_text(aes(y = ..count.. -200, 
                label = paste0(round(prop.table(..count..),4) * 100, '%')), 
            stat = 'count', 
            position = position_dodge(.1), 
            size = 3)

grid.arrange(p1,p2,p3,p4,p5,p6,ncol=2)
grid.arrange(p7,p8,p9,p10,p11,p12,p13,p14, ncol=2)


# numerical variables
n1 <- ggplot(data = airlines_clean, aes(age, color = satisfaction))+
  geom_freqpoly(binwidth = 5, size = 1)

#Monthly charges histogram
n2 <- ggplot(data = airlines_clean, aes(flight_distance, color = satisfaction))+
  geom_freqpoly(binwidth = 5, size = 1)

#Total charges histogram
n3 <- ggplot(data = airlines_clean, aes(departure_delay_in_minutes, color = satisfaction))+
  geom_freqpoly(binwidth = 200, size = 1)

n4 <- ggplot(data = airlines_clean, aes(arrival_delay_in_minutes, color = satisfaction))+
  geom_freqpoly(binwidth = 200, size = 1)

#Plot quantitative data within a grid
grid.arrange(n1, n2, n3, n4,ncol=1)


######################## Logistic regression
# Making factors
airlines_clean$inflight_wifi_service <- as.character(airlines_clean$inflight_wifi_service)
airlines_clean$departure_arrival_time_convenient <- as.character(airlines_clean$departure_arrival_time_convenient)
airlines_clean$ease_of_online_booking <- as.character(airlines_clean$ease_of_online_booking)
airlines_clean$gate_location <- as.character(airlines_clean$gate_location)
airlines_clean$food_and_drink <- as.character(airlines_clean$food_and_drink)
airlines_clean$online_boarding <- as.character(airlines_clean$online_boarding)
airlines_clean$seat_comfort <- as.character(airlines_clean$seat_comfort)
airlines_clean$inflight_entertainment <- as.character(airlines_clean$inflight_entertainment)
airlines_clean$onboard_service <- as.character(airlines_clean$onboard_service)
airlines_clean$leg_room_service <- as.character(airlines_clean$leg_room_service)
airlines_clean$baggage_handling <- as.character(airlines_clean$baggage_handling)
airlines_clean$checkin_service <- as.character(airlines_clean$checkin_service)
airlines_clean$inflight_service <- as.character(airlines_clean$inflight_service)
airlines_clean$cleanliness <- as.character(airlines_clean$cleanliness)

airlines_clean <- subset(airlines_clean, airlines_clean$seat_comfort != 0)
airlines_clean <- subset(airlines_clean, airlines_clean$gate_location != 0)


# Run regression
lr_fit <- glm(satisfaction~. , data = airlines_clean,
              family=binomial(link='logit'))
summary(lr_fit)

########## Detecting multicollinearity
library(performance)
check_collinearity(lr_fit)

# + ease_of_online_booking
lr_fit <- glm(satisfaction~.-ease_of_online_booking , airlines_clean,
              family=binomial(link='logit')) 
lr_fit
check_collinearity(lr_fit)




# + inflight_entertainment
lr_fit <- glm(satisfaction~.-ease_of_online_booking-inflight_entertainment , data = airlines_clean,
              family=binomial(link='logit'))

lr_fit
check_collinearity(lr_fit)

# + arrival delay
lr_fit <- glm(satisfaction~.-ease_of_online_booking-inflight_entertainment-arrival_delay_in_minutes , data = airlines_clean,
              family=binomial(link='logit'))
lr_fit
check_collinearity(lr_fit)

# + departure_arrival_time_convenient
lr_fit <- glm(satisfaction~.-ease_of_online_booking-inflight_entertainment-arrival_delay_in_minutes-departure_arrival_time_convenient , data = airlines_clean,
              family=binomial(link='logit')) 
summary(lr_fit)
check_collinearity(lr_fit)


#########
library(leaps)
# forward selection approach
regfit.fwd=regsubsets(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient,data=airlines_clean,method="forward", nvmax=70)
reg.summary = summary(regfit.fwd)
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp")
which.min(reg.summary$cp)

############ Predictions
set.seed(123)
split_train_test <- createDataPartition(airlines_clean$satisfaction,p=0.7,list=FALSE)
dtrain<- airlines_clean[split_train_test,]
dtest<-  airlines_clean[-split_train_test,]
lr_fit <- glm( satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = dtrain,
               family=binomial(link='logit'))
summary(lr_fit)

# Confusion matrix


lr_prob1 <- predict(lr_fit, dtest, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1, Actual = dtest$satisfaction)


# Accuracy
lr_prob2 <- predict(lr_fit, dtrain, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
lr_tab1 <- table(Predicted = lr_pred2, Actual = dtrain$satisfaction)
lr_tab2 <- table(Predicted = lr_pred1, Actual = dtest$satisfaction)

dtrain$satisfaction <- as.factor(mapvalues(dtrain$satisfaction,
                                           from = c('satisfied',
                                                    'neutral or dissatisfied'),
                                           to = c('Yes',
                                                  'No')))

dtest$satisfaction <- as.factor(mapvalues(dtest$satisfaction,
                                          from = c('satisfied',
                                                   'neutral or dissatisfied'),
                                          to = c('Yes',
                                                 'No')))
# Train
confusionMatrix(
  as.factor(lr_pred2),
  dtrain$satisfaction,
  positive = "Yes" 
)
# Test
confusionMatrix(
  as.factor(lr_pred1),
  dtest$satisfaction,
  positive = "Yes" 
)




######## Robustness changing data partition
set.seed(456)
split_train_test_rob <- createDataPartition(airlines_clean$satisfaction,p=0.7,list=FALSE)
dtrain_rob<- airlines_clean[split_train_test_rob,]
dtest_rob<-  airlines_clean[-split_train_test_rob,]
lr_fit_rob <- glm( satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient , data = dtrain_rob,
                   family=binomial(link='logit'))
summary(lr_fit_rob)

# Confusion matrix


lr_prob1_rob <- predict(lr_fit_rob, dtest_rob, type="response")
lr_pred1_rob <- ifelse(lr_prob1_rob > 0.5,"Yes","No")
table(Predicted = lr_pred1_rob, Actual = dtest_rob$satisfaction)


# Accuracy
lr_prob2_rob <- predict(lr_fit_rob, dtrain_rob, type="response")
lr_pred2_rob <- ifelse(lr_prob2_rob > 0.5,"Yes","No")
lr_tab1_rob <- table(Predicted = lr_pred2_rob, Actual = dtrain_rob$satisfaction)
lr_tab2_rob <- table(Predicted = lr_pred1_rob, Actual = dtest_rob$satisfaction)

dtrain_rob$satisfaction <- as.factor(mapvalues(dtrain_rob$satisfaction,
                                               from = c('satisfied',
                                                        'neutral or dissatisfied'),
                                               to = c('Yes',
                                                      'No')))

dtest_rob$satisfaction <- as.factor(mapvalues(dtest_rob$satisfaction,
                                              from = c('satisfied',
                                                       'neutral or dissatisfied'),
                                              to = c('Yes',
                                                     'No')))
# Train
confusionMatrix(
  as.factor(lr_pred2_rob),
  dtrain_rob$satisfaction,
  positive = "Yes" 
)
# Test
confusionMatrix(
  as.factor(lr_pred1_rob),
  dtest_rob$satisfaction,
  positive = "Yes" 
)

########## Third different data partition
set.seed(789)
split_train_test_rob1 <- createDataPartition(airlines_clean$satisfaction,p=0.7,list=FALSE)
dtrain_rob1<- airlines_clean[split_train_test_rob1,]
dtest_rob1<-  airlines_clean[-split_train_test_rob1,]
lr_fit_rob1 <- glm(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = dtrain_rob1,
                   family=binomial(link='logit'))
summary(lr_fit_rob)

# Confusion matrix


lr_prob1_rob1 <- predict(lr_fit_rob1, dtest_rob1, type="response")
lr_pred1_rob1 <- ifelse(lr_prob1_rob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1_rob1, Actual = dtest_rob1$satisfaction)


# Accuracy
lr_prob2_rob1 <- predict(lr_fit_rob1, dtrain_rob1, type="response")
lr_pred2_rob1 <- ifelse(lr_prob2_rob1 > 0.5,"Yes","No")
lr_tab1_rob1 <- table(Predicted = lr_pred2_rob1, Actual = dtrain_rob1$satisfaction)
lr_tab2_rob1 <- table(Predicted = lr_pred1_rob1, Actual = dtest_rob1$satisfaction)

dtrain_rob1$satisfaction <- as.factor(mapvalues(dtrain_rob1$satisfaction,
                                                from = c('satisfied',
                                                         'neutral or dissatisfied'),
                                                to = c('Yes',
                                                       'No')))

dtest_rob1$satisfaction <- as.factor(mapvalues(dtest_rob1$satisfaction,
                                               from = c('satisfied',
                                                        'neutral or dissatisfied'),
                                               to = c('Yes',
                                                      'No')))
# Train
confusionMatrix(
  as.factor(lr_pred2_rob1),
  dtrain_rob1$satisfaction,
  positive = "Yes" 
)
# Test
confusionMatrix(
  as.factor(lr_pred1_rob1),
  dtest_rob1$satisfaction,
  positive = "Yes" 
)

### Training size equal to 0.9
set.seed(555)
split_train_test <- createDataPartition(airlines_clean$satisfaction,p=0.9,list=FALSE)
dtrain<- airlines_clean[split_train_test,]
dtest<-  airlines_clean[-split_train_test,]
lr_fit <- glm(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = dtrain,
              family=binomial(link='logit'))
summary(lr_fit)

# Confusion matrix


lr_prob1 <- predict(lr_fit, dtest, type="response")
lr_pred1 <- ifelse(lr_prob1 > 0.5,"Yes","No")
table(Predicted = lr_pred1, Actual = dtest$satisfaction)


# Accuracy
lr_prob2 <- predict(lr_fit, dtrain, type="response")
lr_pred2 <- ifelse(lr_prob2 > 0.5,"Yes","No")
lr_tab1 <- table(Predicted = lr_pred2, Actual = dtrain$satisfaction)
lr_tab2 <- table(Predicted = lr_pred1, Actual = dtest$satisfaction)

dtrain$satisfaction <- as.factor(mapvalues(dtrain$satisfaction,
                                           from = c('satisfied',
                                                    'neutral or dissatisfied'),
                                           to = c('Yes',
                                                  'No')))

dtest$satisfaction <- as.factor(mapvalues(dtest$satisfaction,
                                          from = c('satisfied',
                                                   'neutral or dissatisfied'),
                                          to = c('Yes',
                                                 'No')))
# Train
confusionMatrix(
  as.factor(lr_pred2),
  dtrain$satisfaction,
  positive = "Yes" 
)
# Test
confusionMatrix(
  as.factor(lr_pred1),
  dtest$satisfaction,
  positive = "Yes" 
)

######## k-cross-validation
# Define training control
set.seed(124) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train( satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = airlines_clean, method = "glm",
                trControl = train.control)
# Summarize the results
print(model)


####### With normalized variables

# First split

set.seed(999)
split_train_test_norm <- createDataPartition(airlines_clean$satisfaction,p=0.7,list=FALSE)
dtrain_nor<- airlines_clean[split_train_test_norm,]
dtest_nor<-  airlines_clean[-split_train_test_norm,]

# normalize training set
dist_scaled <- (dtrain_nor$flight_distance-mean(dtrain_nor$flight_distance))/(max(dtrain_nor$flight_distance) - min(dtrain_nor$flight_distance))
age_scaled <- (dtrain_nor$age-mean(dtrain_nor$age))/(max(dtrain_nor$age) - min(dtrain_nor$age))
dep_scaled <- (dtrain_nor$departure_delay_in_minutes-mean(dtrain_nor$departure_delay_in_minutes))/(max(dtrain_nor$departure_delay_in_minutes) - min(dtrain_nor$departure_delay_in_minutes))
arr_scaled <- (dtrain_nor$arrival_delay_in_minutes-mean(dtrain_nor$arrival_delay_in_minutes))/(max(dtrain_nor$arrival_delay_in_minutes) - min(dtrain_nor$arrival_delay_in_minutes))
dtrain_nor$flight_distance <- dist_scaled
dtrain_nor$age <- age_scaled
dtrain_nor$arrival_delay_in_minutes <- arr_scaled

dtrain_norm<- dtrain_nor
# normalize test set
dist_scaled <- (dtest_nor$flight_distance-mean(dtest_nor$flight_distance))/(max(dtest_nor$flight_distance) - min(dtest_nor$flight_distance))
age_scaled <- (dtest_nor$age-mean(dtest_nor$age))/(max(dtest_nor$age) - min(dtest_nor$age))
dep_scaled <- (dtest_nor$departure_delay_in_minutes-mean(dtest_nor$departure_delay_in_minutes))/(max(dtest_nor$departure_delay_in_minutes) - min(dtest_nor$departure_delay_in_minutes))
arr_scaled <- (dtest_nor$arrival_delay_in_minutes-mean(dtest_nor$arrival_delay_in_minutes))/(max(dtest_nor$arrival_delay_in_minutes) - min(dtest_nor$arrival_delay_in_minutes))
dtest_nor$flight_distance <- dist_scaled
dtest_nor$age <- age_scaled
dtest_nor$arrival_delay_in_minutes <- arr_scaled

dtest_norm<-  dtest_nor

lr_fit_norm <- glm(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = dtrain_norm,
                   family=binomial(link='logit'))


# Confusion matrix


lr_prob1_norm <- predict(lr_fit_norm, dtest_norm, type="response")
lr_pred1_norm <- ifelse(lr_prob1_norm > 0.5,"Yes","No")
table(Predicted = lr_pred1_norm, Actual = dtest_norm$satisfaction)


# Accuracy
lr_prob2_norm <- predict(lr_fit_norm, dtrain_norm, type="response")
lr_pred2_norm <- ifelse(lr_prob2_norm > 0.5,"Yes","No")
lr_tab1_norm <- table(Predicted = lr_pred2_norm, Actual = dtrain_norm$satisfaction)
lr_tab2_norm <- table(Predicted = lr_pred1_norm, Actual = dtest_norm$satisfaction)

dtrain_norm$satisfaction <- as.factor(mapvalues(dtrain_norm$satisfaction,
                                                from = c('satisfied',
                                                         'neutral or dissatisfied'),
                                                to = c('Yes',
                                                       'No')))

dtest_norm$satisfaction <- as.factor(mapvalues(dtest_norm$satisfaction,
                                               from = c('satisfied',
                                                        'neutral or dissatisfied'),
                                               to = c('Yes',
                                                      'No')))
# Train
confusionMatrix(
  as.factor(lr_pred2_norm),
  dtrain_norm$satisfaction,
  positive = "Yes" 
)
# Test
confusionMatrix(
  as.factor(lr_pred1_norm),
  dtest_norm$satisfaction,
  positive = "Yes" 
)

############# Second split
set.seed(888)
split_train_test_norm <- createDataPartition(airlines_clean$satisfaction,p=0.7,list=FALSE)
dtrain_nor<- airlines_clean[split_train_test_norm,]
dtest_nor<-  airlines_clean[-split_train_test_norm,]
# normalize training set
dist_scaled <- (dtrain_nor$flight_distance-mean(dtrain_nor$flight_distance))/(max(dtrain_nor$flight_distance) - min(dtrain_nor$flight_distance))
age_scaled <- (dtrain_nor$age-mean(dtrain_nor$age))/(max(dtrain_nor$age) - min(dtrain_nor$age))
dep_scaled <- (dtrain_nor$departure_delay_in_minutes-mean(dtrain_nor$departure_delay_in_minutes))/(max(dtrain_nor$departure_delay_in_minutes) - min(dtrain_nor$departure_delay_in_minutes))
arr_scaled <- (dtrain_nor$arrival_delay_in_minutes-mean(dtrain_nor$arrival_delay_in_minutes))/(max(dtrain_nor$arrival_delay_in_minutes) - min(dtrain_nor$arrival_delay_in_minutes))
dtrain_nor$flight_distance <- dist_scaled
dtrain_nor$age <- age_scaled
dtrain_nor$departure_delay_in_minutes <- dep_scaled
dtrain_nor$arrival_delay_in_minutes <- arr_scaled

dtrain_norm<- dtrain_nor
# normalize test set
dist_scaled <- (dtest_nor$flight_distance-mean(dtest_nor$flight_distance))/(max(dtest_nor$flight_distance) - min(dtest_nor$flight_distance))
age_scaled <- (dtest_nor$age-mean(dtest_nor$age))/(max(dtest_nor$age) - min(dtest_nor$age))
dep_scaled <- (dtest_nor$departure_delay_in_minutes-mean(dtest_nor$departure_delay_in_minutes))/(max(dtest_nor$departure_delay_in_minutes) - min(dtest_nor$departure_delay_in_minutes))
arr_scaled <- (dtest_nor$arrival_delay_in_minutes-mean(dtest_nor$arrival_delay_in_minutes))/(max(dtest_nor$arrival_delay_in_minutes) - min(dtest_nor$arrival_delay_in_minutes))
dtest_nor$flight_distance <- dist_scaled
dtest_nor$age <- age_scaled
dtest_nor$departure_delay_in_minutes <- dep_scaled
dtest_nor$arrival_delay_in_minutes <- arr_scaled

dtest_norm<-  dtest_nor

lr_fit_norm <- glm(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = dtrain_norm,
                   family=binomial(link='logit'))

summary(lr_fit_norm)

# Confusion matrix


lr_prob1_norm <- predict(lr_fit_norm, dtest_norm, type="response")
lr_pred1_norm <- ifelse(lr_prob1_norm > 0.5,"Yes","No")
table(Predicted = lr_pred1_norm, Actual = dtest_norm$satisfaction)


# Accuracy
lr_prob2_norm <- predict(lr_fit_norm, dtrain_norm, type="response")
lr_pred2_norm <- ifelse(lr_prob2_norm > 0.5,"Yes","No")
lr_tab1_norm <- table(Predicted = lr_pred2_norm, Actual = dtrain_norm$satisfaction)
lr_tab2_norm <- table(Predicted = lr_pred1_norm, Actual = dtest_norm$satisfaction)

dtrain_norm$satisfaction <- as.factor(mapvalues(dtrain_norm$satisfaction,
                                                from = c('satisfied',
                                                         'neutral or dissatisfied'),
                                                to = c('Yes',
                                                       'No')))

dtest_norm$satisfaction <- as.factor(mapvalues(dtest_norm$satisfaction,
                                               from = c('satisfied',
                                                        'neutral or dissatisfied'),
                                               to = c('Yes',
                                                      'No')))
# Train
confusionMatrix(
  as.factor(lr_pred2_norm),
  dtrain_norm$satisfaction,
  positive = "Yes" 
)
# Test
confusionMatrix(
  as.factor(lr_pred1_norm),
  dtest_norm$satisfaction,
  positive = "Yes" 
)



###################################
# With cross-validation
dist_scaled <- (airlines_clean$flight_distance-mean(airlines_clean$flight_distance))/(max(airlines_clean$flight_distance) - min(airlines_clean$flight_distance))
age_scaled <- (airlines_clean$age-mean(airlines_clean$age))/(max(airlines_clean$age) - min(airlines_clean$age))
dep_scaled <- (airlines_clean$departure_delay_in_minutes-mean(airlines_clean$departure_delay_in_minutes))/(max(airlines_clean$departure_delay_in_minutes) - min(airlines_clean$departure_delay_in_minutes))
arr_scaled <- (airlines_clean$arrival_delay_in_minutes-mean(airlines_clean$arrival_delay_in_minutes))/(max(airlines_clean$arrival_delay_in_minutes) - min(airlines_clean$arrival_delay_in_minutes))
airlines_clean$flight_distance <- dist_scaled
airlines_clean$age <- age_scaled
airlines_clean$departure_delay_in_minutes <- dep_scaled
airlines_clean$arrival_delay_in_minutes <- arr_scaled

set.seed(777) 
train.control_norm <- trainControl(method = "cv", number = 10)
# Train the model
model_norm <- train(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = airlines_clean, method = "glm",
                    trControl = train.control_norm)
# Summarize the results
print(model_norm)


############################################################
########## Recover the starting dataset

airlines <- read.csv("C:\\Users\\Utente\\OneDrive\\Desktop\\SL exam\\airline_passenger_satisfaction.csv")
airlines['X'] <- NULL

# Data preparation
airlines_clean <- airlines[complete.cases(airlines), ]
airlines_clean$satisfaction<-as.factor(airlines_clean$satisfaction)
airlines_clean <- subset(airlines_clean, airlines_clean$seat_comfort != 0)
########## Be careful of error

##############Tree

library(tree)
library(ISLR)

tree.airlines <- tree(satisfaction~., data = airlines_clean)
summary(tree.airlines)
plot(tree.airlines)
text(tree.airlines, pretty = 0)
tree.airlines

# Accuracy 0.85
####### Trying to split train-test
set.seed(101)
train = sample(1:nrow(airlines_clean), 0.7*nrow(airlines_clean))
airlines_train = airlines_clean[train,-23]
airlines_test = airlines_clean[-train,-23]
airlines_train_labels <- airlines_clean[train, 23]
airlines_test_labels <- airlines_clean[-train, 23]
summary(airlines_train_labels)
summary(airlines_test_labels)

tree_airlines_train <- tree(satisfaction~., airlines_clean[train,])
tree_airlines_pred <- predict(tree_airlines_train, airlines_clean[-train,], type = 'class')
table(tree_airlines_pred, airlines_test_labels)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table(tree_airlines_pred, airlines_test_labels))

# 0.85
######### Pruning it

set.seed(101)
cv.airlines=cv.tree(tree_airlines_train,FUN=prune.misclass)
cv.airlines
plot(cv.airlines) #8

prune.airlines=prune.misclass(tree_airlines_train,best=8)
plot(prune.airlines);text(prune.airlines,pretty=0)
tree.air.pred=predict(prune.airlines,airlines_clean[-train,],type="class")
with(airlines_clean[-train,],table(tree.air.pred,satisfaction))

### Accuracy 0.85

########### Repeat without discarded variables


tree.airlines <- tree(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, data = airlines_clean)
summary(tree.airlines)
plot(tree.airlines)
text(tree.airlines, pretty = 0)
tree.airlines


####### Trying to split train-test
set.seed(202)
train = sample(1:nrow(airlines_clean), 0.7*nrow(airlines_clean))
airlines_train = airlines_clean[train,-23]
airlines_test = airlines_clean[-train,-23]
airlines_train_labels <- airlines_clean[train, 23]
airlines_test_labels <- airlines_clean[-train, 23]
summary(airlines_train_labels)
summary(airlines_test_labels)

tree_airlines_train <- tree(satisfaction~.-ease_of_online_booking-inflight_entertainment-departure_delay_in_minutes-departure_arrival_time_convenient, airlines_clean[train,])
tree_airlines_pred <- predict(tree_airlines_train, airlines_clean[-train,], type = 'class')
table(tree_airlines_pred, airlines_test_labels)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(table(tree_airlines_pred, airlines_test_labels))


######### Pruning it

set.seed(202)
cv.airlines=cv.tree(tree_airlines_train,FUN=prune.misclass)
cv.airlines
plot(cv.airlines) 

prune.airlines=prune.misclass(tree_airlines_train,best=8)
plot(prune.airlines);text(prune.airlines,pretty=0)
tree.air.pred=predict(prune.airlines,airlines_clean[-train,],type="class")
with(airlines_clean[-train,],table(tree.air.pred,satisfaction))



################################ K-NN
library(class)

airlines_clean$inflight_wifi_service <- as.integer(airlines_clean$inflight_wifi_service)
airlines_clean$departure_arrival_time_convenient <- as.integer(airlines_clean$departure_arrival_time_convenient)
airlines_clean$ease_of_online_booking <- as.integer(airlines_clean$ease_of_online_booking)
airlines_clean$gate_location <- as.integer(airlines_clean$gate_location)
airlines_clean$food_and_drink <- as.integer(airlines_clean$food_and_drink)
airlines_clean$online_boarding <- as.integer(airlines_clean$online_boarding)
airlines_clean$seat_comfort <- as.integer(airlines_clean$seat_comfort)
airlines_clean$inflight_entertainment <- as.integer(airlines_clean$inflight_entertainment)
airlines_clean$onboard_service <- as.integer(airlines_clean$onboard_service)
airlines_clean$leg_room_service <- as.integer(airlines_clean$leg_room_service)
airlines_clean$baggage_handling <- as.integer(airlines_clean$baggage_handling)
airlines_clean$checkin_service <- as.integer(airlines_clean$checkin_service)
airlines_clean$inflight_service <- as.integer(airlines_clean$inflight_service)
airlines_clean$cleanliness <- as.integer(airlines_clean$cleanliness)
airlines_clean$Gender <- as.integer(mapvalues(airlines_clean$Gender,
                                              from = c('Female',
                                                       'Male'),
                                              to = c('0' , '1')))
airlines_clean$customer_type <- as.integer(mapvalues(airlines_clean$customer_type,
                                                     from = c('disloyal Customer',
                                                              'Loyal Customer'),
                                                     to = c('0' , '1')))
airlines_clean$type_of_travel <- as.integer(mapvalues(airlines_clean$type_of_travel,
                                                      from = c('Personal Travel',
                                                               'Business travel'),
                                                      to = c('0' , '1')))
airlines_clean$customer_class <- as.integer(mapvalues(airlines_clean$customer_class,
                                                      from = c('Eco',
                                                               'Eco Plus',
                                                               'Business'),
                                                      to = c('0' , '1', '2')))
airlines_clean$satisfaction <- as.integer(mapvalues(airlines_clean$satisfaction,
                                                    from = c('neutral or dissatisfied',
                                                             'satisfied'),
                                                    to = c('0' , '1')))




normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
}

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

dt <- as.data.frame(lapply(airlines_clean, normalize))

######## size 0.7
set.seed(12)
train.index7 <- createDataPartition(dt$satisfaction, p = .7, list = FALSE)
dt_train7 <- dt[ train.index7,-23]
dt_test7  <- dt[-train.index7,-23]

dt_train_labels7 <- dt[train.index7, 23]
dt_test_labels7 <- dt[-train.index7, 23]

table17 <- table(dt_train_labels7)
round(prop.table(table17), 2)

table27 <- table(dt_test_labels7)
round(prop.table(table27), 2)


##run knn function 
pr7 <- knn(dt_train7,dt_test7,cl=dt_train_labels7,k=20)

##create confusion matrix
tab7 <- table(pr7,dt_test_labels7)

#### size 0.6
set.seed(34)
train.index6 <- createDataPartition(dt$satisfaction, p = .6, list = FALSE)
dt_train6 <- dt[ train.index6,-23]
dt_test6  <- dt[-train.index6,-23]

dt_train_labels6 <- dt[train.index6, 23]
dt_test_labels6 <- dt[-train.index6, 23]

table16 <- table(dt_train_labels6)
round(prop.table(table16), 2)

table26 <- table(dt_test_labels6)
round(prop.table(table26), 2)


##run knn function 
pr6 <- knn(dt_train6,dt_test6,cl=dt_train_labels6,k=20)

##create confusion matrix
tab6 <- table(pr6,dt_test_labels6)



#### size 0.8
set.seed(56)
train.index8 <- createDataPartition(dt$satisfaction, p = .8, list = FALSE)
dt_train8 <- dt[ train.index8,-23]
dt_test8  <- dt[-train.index8,-23]

dt_train_labels8 <- dt[train.index8, 23]
dt_test_labels8 <- dt[-train.index8, 23]

table18 <- table(dt_train_labels8)
round(prop.table(table18), 2)

table28 <- table(dt_test_labels8)
round(prop.table(table28), 2)


##run knn function 
pr8 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=20)

##create confusion matrix
tab8 <- table(pr8,dt_test_labels8)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.

accuracy(tab8)

# size 0.9
set.seed(78)
train.index9 <- createDataPartition(dt$satisfaction, p = .9, list = FALSE)
dt_train9 <- dt[ train.index9,-23]
dt_test9  <- dt[-train.index9,-23]

dt_train_labels9 <- dt[train.index9, 23]
dt_test_labels9 <- dt[-train.index9, 23]

table19 <- table(dt_train_labels9)
round(prop.table(table19), 2)

table29 <- table(dt_test_labels9)
round(prop.table(table29), 2)


##run knn function 
pr9 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=20)

##create confusion matrix
tab9 <- table(pr9,dt_test_labels9)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.


# size 0.5
set.seed(90)
train.index5 <- createDataPartition(dt$satisfaction, p = .5, list = FALSE)
dt_train5 <- dt[ train.index5,-23]
dt_test5  <- dt[-train.index5,-23]

dt_train_labels5 <- dt[train.index5, 23]
dt_test_labels5 <- dt[-train.index5, 23]

table15 <- table(dt_train_labels5)
round(prop.table(table15), 2)

table25 <- table(dt_test_labels5)
round(prop.table(table25), 2)


##run knn function 
pr5 <- knn(dt_train5,dt_test5,cl=dt_train_labels5,k=20)

##create confusion matrix
tab5 <- table(pr5,dt_test_labels5)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.




################ plot based on training size
accuracies <- c(accuracy(tab5), accuracy(tab6), accuracy(tab7),accuracy(tab8), accuracy(tab9) )
thicks <- c(0.5,0.6,0.7,0.8,0.9)

plot(thicks, accuracies, type="o", col="blue", xlab = "training size", ylab = "accuracy")

################## Accuracies are pretty stable around 92.60. The best one is that with training size 0.9, therefore I move on with this size and analyze different k


#K = 1
prK1 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=1)
##create confusion matrix
tabK1 <- table(prK1,dt_test_labels9)

#K = 3
prK3 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=3)
##create confusion matrix
tabK3 <- table(prK3,dt_test_labels9)

#K = 5
prK5 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=5)
##create confusion matrix
tabK5 <- table(prK5,dt_test_labels9)

# K = 7
prK7 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=7)
##create confusion matrix
tabK7 <- table(prK7,dt_test_labels9)

## k = 10
pr10 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=10)
##create confusion matrix
tabK10 <- table(pr10,dt_test_labels9)



## K = 30
pr30 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=30)
##create confusion matrix
tab30 <- table(pr30,dt_test_labels9)


## K = 40
pr40 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=40)
##create confusion matrix
tab40 <- table(pr40,dt_test_labels9)


## K = 50
pr50 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=50)
##create confusion matrix
tab50 <- table(pr50,dt_test_labels9)


accuraciesK <- c(accuracy(tabK1), accuracy(tabK3), accuracy(tabK5),accuracy(tabK7), accuracy(tab10), accuracy(tab9), accuracy(tab30),accuracy(tab40), accuracy(tab50) )
thicksK <- c(1,3,5,7,10,20,30,40,50)

plot(thicksK, accuraciesK, type="o", col="blue", xlab = "K-neighbours", ylab = "accuracy")
##################### K = 5 best neighbours with accuracy93.334 and tr. size = 0.9


############ repeat the analysis without the discarded variables because of multicollinearity

dt <- dt[,-c(8,9,14,21)]

######## size 0.7
set.seed(1212)
train.index7 <- createDataPartition(dt$satisfaction, p = .7, list = FALSE)
dt_train7 <- dt[ train.index7,-19]
dt_test7  <- dt[-train.index7,-19]

dt_train_labels7 <- dt[train.index7, 19]
dt_test_labels7 <- dt[-train.index7, 19]

table17 <- table(dt_train_labels7)
round(prop.table(table17), 2)

table27 <- table(dt_test_labels7)
round(prop.table(table27), 2)


##run knn function 
pr7 <- knn(dt_train7,dt_test7,cl=dt_train_labels7,k=20)

##create confusion matrix
tab7 <- table(pr7,dt_test_labels7)

#### size 0.6
set.seed(3434)
train.index6 <- createDataPartition(dt$satisfaction, p = .6, list = FALSE)
dt_train6 <- dt[ train.index6,-19]
dt_test6  <- dt[-train.index6,-19]

dt_train_labels6 <- dt[train.index6, 19]
dt_test_labels6 <- dt[-train.index6, 19]

table16 <- table(dt_train_labels6)
round(prop.table(table16), 2)

table26 <- table(dt_test_labels6)
round(prop.table(table26), 2)


##run knn function 
pr6 <- knn(dt_train6,dt_test6,cl=dt_train_labels6,k=20)

##create confusion matrix
tab6 <- table(pr6,dt_test_labels6)



#### size 0.8
set.seed(5656)
train.index8 <- createDataPartition(dt$satisfaction, p = .8, list = FALSE)
dt_train8 <- dt[ train.index8,-19]
dt_test8  <- dt[-train.index8,-19]

dt_train_labels8 <- dt[train.index8, 19]
dt_test_labels8 <- dt[-train.index8, 19]

table18 <- table(dt_train_labels8)
round(prop.table(table18), 2)

table28 <- table(dt_test_labels8)
round(prop.table(table28), 2)


##run knn function 
pr8 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=20)

##create confusion matrix
tab8 <- table(pr8,dt_test_labels8)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.

accuracy(tab8)

# size 0.9
set.seed(7878)
train.index9 <- createDataPartition(dt$satisfaction, p = .9, list = FALSE)
dt_train9 <- dt[ train.index9,-19]
dt_test9  <- dt[-train.index9,-19]

dt_train_labels9 <- dt[train.index9, 19]
dt_test_labels9 <- dt[-train.index9, 19]

table19 <- table(dt_train_labels9)
round(prop.table(table19), 2)

table29 <- table(dt_test_labels9)
round(prop.table(table29), 2)


##run knn function 
pr9 <- knn(dt_train9,dt_test9,cl=dt_train_labels9,k=20)

##create confusion matrix
tab9 <- table(pr9,dt_test_labels9)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.


# size 0.5
set.seed(9090)
train.index5 <- createDataPartition(dt$satisfaction, p = .5, list = FALSE)
dt_train5 <- dt[ train.index5,-19]
dt_test5  <- dt[-train.index5,-19]

dt_train_labels5 <- dt[train.index5, 19]
dt_test_labels5 <- dt[-train.index5, 19]

table15 <- table(dt_train_labels5)
round(prop.table(table15), 2)

table25 <- table(dt_test_labels5)
round(prop.table(table25), 2)


##run knn function 
pr5 <- knn(dt_train5,dt_test5,cl=dt_train_labels5,k=20)

##create confusion matrix
tab5 <- table(pr5,dt_test_labels5)

##this function divides the correct predictions by total number of predictions that tell us how accurate the model is.




################ plot based on training size
accuracies <- c(accuracy(tab5), accuracy(tab6), accuracy(tab7),accuracy(tab8), accuracy(tab9) )
thicks <- c(0.5,0.6,0.7,0.8,0.9)

plot(thicks, accuracies, type="o", col="blue", xlab = "training size", ylab = "accuracy")

################## Accuracies are pretty stable around 92.60. The best one is that with training size 0.9, therefore I move on with this size and analyze different k


#K = 1
prK1 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=1)
##create confusion matrix
tabK1 <- table(prK1,dt_test_labels8)

#K = 3
prK3 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=3)
##create confusion matrix
tabK3 <- table(prK3,dt_test_labels8)

#K = 5
prK5 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=5)
##create confusion matrix
tabK5 <- table(prK5,dt_test_labels8)

# K = 7
prK7 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=7)
##create confusion matrix
tabK7 <- table(prK7,dt_test_labels8)

## k = 10
pr10 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=10)
##create confusion matrix
tabK10 <- table(pr10,dt_test_labels8)



## K = 30
pr30 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=30)
##create confusion matrix
tab30 <- table(pr30,dt_test_labels8)


## K = 40
pr40 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=40)
##create confusion matrix
tab40 <- table(pr40,dt_test_labels8)


## K = 50
pr50 <- knn(dt_train8,dt_test8,cl=dt_train_labels8,k=50)
##create confusion matrix
tab50 <- table(pr50,dt_test_labels8)


accuraciesK <- c(accuracy(tabK1), accuracy(tabK3), accuracy(tabK5),accuracy(tabK7), accuracy(tabK10), accuracy(tab8), accuracy(tab30),accuracy(tab40), accuracy(tab50) )
thicksK <- c(1,3,5,7,10,20,30,40,50)

plot(thicksK, accuraciesK, type="o", col="blue", xlab = "K-neighbours", ylab = "accuracy")
