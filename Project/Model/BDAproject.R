memory.limit(size=500000)

setwd("C:/Users/charl/Documents")
flights<-read.csv("flights.csv")
weather<-read.csv("stormevents.csv")[,c(4,7)]
fips<-read.csv("fips.csv")[,2:3] #mapping state name to state abbreviation
airports<-read.csv("airports.csv") #mapping state to airport

library(dplyr)
library(randomForest)
library(ggplot2) 
library(caret) #for cross validation
library(lubridate)
library(pROC)
library(caret)
library(miscTools)
#CANCELLATIONS DATASET FOR ANALYSIS
df0 <- flights[!is.na(flights$MONTH)&!is.na(flights$DAY_OF_WEEK)&!is.na(flights$AIRLINE)&!is.na(flights$SCHEDULED_DEPARTURE)
               &!is.na(flights$SCHEDULED_TIME)&!is.na(flights$SCHEDULED_ARRIVAL),]
colsc<-c("CANCELLED", "MONTH" ,"DAY_OF_WEEK" ,"AIRLINE" ,"SCHEDULED_DEPARTURE" ,"SCHEDULED_TIME" ,   "SCHEDULED_ARRIVAL" )
df0$CANCELLED <- as.factor(df0$CANCELLED)
dff1<- df0[ df0$CANCELLED == "1" ,  colsc ]
select1 <- sample( 1:sum( df0$CANCELLED == "0" ), size=nrow(dff1) )
dff1 <- rbind( dff1, df0[ df0$CANCELLED == "0" , colsc ][ select1 , ] )

dff1$MONTH <- as.factor(dff1$MONTH)
dff1$DAY_OF_WEEK <- as.factor(dff1$DAY_OF_WEEK)
print( sum(dff1$CANCELLED=="1") )
print( sum(dff1$CANCELLED=="0") )


#DELAYS DATASET FOR ANALYSIS
colsd<-c("ARRIVAL_DELAY", "MONTH" ,"DAY_OF_WEEK" ,"AIRLINE" ,"SCHEDULED_DEPARTURE" ,"SCHEDULED_TIME" ,   "SCHEDULED_ARRIVAL" )
df2 <- df0[!is.na(df0$ARRIVAL_DELAY)&!is.na(df0$MONTH)&!is.na(df0$DAY_OF_WEEK)&!is.na(df0$AIRLINE)&!is.na(df0$SCHEDULED_DEPARTURE)&!is.na(df0$SCHEDULED_ARRIVAL)&!is.na(df0$ORIGIN_AIRPORT)&!is.na(df0$DESTINATION_AIRPORT),colsd]

arr0 <- df2[df2$ARRIVAL_DELAY == 0,colsd]
arr1 <- df2[df2$ARRIVAL_DELAY < 15 & df2$ARRIVAL_DELAY > 0,colsd]
arr15 <- df2[df2$ARRIVAL_DELAY >= 15,colsd]

dff2<- rbind.data.frame( arr0[sample(nrow(arr0),60000), ], arr1[ sample(nrow(arr1),60000), ] , arr15[ sample(nrow(arr15),60000), ]) 


dim(dff2[dff2$ARRIVAL_DELAY==0,])
dim(dff2[dff2$ARRIVAL_DELAY>0 & dff2$ARRIVAL_DELAY<15,])       
dim(dff2[dff2$ARRIVAL_DELAY>=15,])

dff2$ANYDELAY <- as.factor(as.numeric(dff2$ARRIVAL_DELAY > 0))
dff2$DELAY15 <- as.factor(as.numeric(dff2$ARRIVAL_DELAY >= 15))

dff2$MONTH <- as.factor(dff2$MONTH)
dff2$DAY_OF_WEEK <- as.factor(dff2$DAY_OF_WEEK)

###################PREDICTING CANCELLATIONS#######################################
set.seed(411)
selectc <- sample( 1:nrow(dff1), size=nrow(dff1)*0.8 )
trainingc <- dff1[ selectc, ]
testc <- dff1[ -selectc, ]

# model with more variables
rfcancellation <- randomForest( CANCELLED ~ MONTH + DAY_OF_WEEK + AIRLINE + SCHEDULED_DEPARTURE + SCHEDULED_TIME + SCHEDULED_ARRIVAL, data = trainingc )
impc <- importance(rfcancellation)

predictedcp <- predict( rfcancellation, testc, type = "prob" )
plot( roc( testc$CANCELLED, predictedcp[,2] ) )

predictedc <- predict( rfcancellation, testc )
confusionMatrix( predictedc, testc$CANCELLED)

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 13048  4348
# 1  5070 13486
# 
# Accuracy : 0.738           
# 95% CI : (0.7335, 0.7426)
# No Information Rate : 0.5039          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.4762          
# Mcnemar's Test P-Value : 1.091e-13       
# 
# Sensitivity : 0.7202          
# Specificity : 0.7562          
# Pos Pred Value : 0.7501          
# Neg Pred Value : 0.7268          
# Prevalence : 0.5039          
# Detection Rate : 0.3629          
# Detection Prevalence : 0.4839          
# Balanced Accuracy : 0.7382          
# 
# 'Positive' Class : 0          

###################PREDICTING POSITIVE DELAY#######################################
set.seed(611)
select0 <- sample( 1:nrow(dff2), size=nrow(dff2)*0.8 )
training0 <- dff2[ select0, ]
test0 <- dff2[ -select0, ]

# model with more variables
rfdelaypos<- randomForest( ANYDELAY ~ MONTH + DAY_OF_WEEK + AIRLINE + SCHEDULED_DEPARTURE + SCHEDULED_TIME + SCHEDULED_ARRIVAL, data = training0 )
imp <- importance(rfdelaypos)

predicted0 <- predict( rfdelaypos, test0, type = "prob" )
plot( roc( test0$ANYDELAY, predicted0[,2] ) )

predicted0 <- predict( rfdelaypos, test0 )
confusionMatrix( predicted0, test0$ANYDELAY )
test2<-test0[35010,]
#output
#round(predict(rfdelaypos, test2, type = "prob" )[[2]] ,2)*100
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0  1851  2658
# 1 10216 21275
# 
# Accuracy : 0.6424          
# 95% CI : (0.6374, 0.6473)
# No Information Rate : 0.6648          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0501          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.15339         
# Specificity : 0.88894         
# Pos Pred Value : 0.41051         
# Neg Pred Value : 0.67559         
# Prevalence : 0.33519         
# Detection Rate : 0.05142         
# Detection Prevalence : 0.12525         
# Balanced Accuracy : 0.52117         
# 
# 'Positive' Class : 0               
###################PREDICTING IF DELAY > 15min IF THERE IS A DELAY#######################################
set.seed(911)
dff3 <- dff2[dff2$ANYDELAY == "1",]
select15 <- sample( 1:nrow(dff3), size=nrow(dff3)*0.8 )
training15 <- dff3[ select15, ]
test15 <- dff3[ -select15, ]

# model with more variables
rfdelayed15 <- randomForest( DELAY15 ~ MONTH + DAY_OF_WEEK + AIRLINE + SCHEDULED_DEPARTURE + SCHEDULED_TIME + SCHEDULED_ARRIVAL, data = training15 )
imp <- importance(rfdelayed15)

predictedp15 <- predict( rfdelayed15, test15, type = "prob" )
plot( roc( test15$DELAY15, predictedp15[,2] ) )

predicted15 <- predict( rfdelayed15, test15 )
confusionMatrix( predicted15, test15$DELAY15 )
test2<-test15[35010,]
#output
#round(predict(rfcancellation, test2, type = "prob" )[[2]] ,2)*100

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
# 0 6637 5196
# 1 5421 6746
# 
# Accuracy : 0.5576          
# 95% CI : (0.5513, 0.5639)
# No Information Rate : 0.5024          
# P-Value [Acc > NIR] : < 2e-16         
# 
# Kappa : 0.1153          
# Mcnemar's Test P-Value : 0.02971         
# 
# Sensitivity : 0.5504          
# Specificity : 0.5649          
# Pos Pred Value : 0.5609          
# Neg Pred Value : 0.5545          
# Prevalence : 0.5024          
# Detection Rate : 0.2765          
# Detection Prevalence : 0.4930          
# Balanced Accuracy : 0.5577          
# 
# 'Positive' Class : 0 

###################EXPECTED DELAY IF A DELAY IS LIKELY#######################################

set.seed(1111)
dff3 <- dff2[dff2$ANYDELAY == "1",]
select <- sample( 1:nrow(dff3), size=nrow(dff3)*0.8 )
training <- dff3[ select, ]
test <- dff3[ -select, ]

rf <- randomForest(ARRIVAL_DELAY ~ MONTH + DAY_OF_WEEK + AIRLINE + SCHEDULED_DEPARTURE + SCHEDULED_TIME + SCHEDULED_ARRIVAL, data=training, ntree=500,mtry=4)

(r2 <- rSquared(test$ARRIVAL_DELAY, test$ARRIVAL_DELAY - predict(rf, test)))
#0.88
(mse <- mean((test$ARRIVAL_DELAY - predict(rf, test))^2))

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=test$ARRIVAL_DELAY, pred=predict(rf, test)))


p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("Predicted vs actual ARRIVAL_DELAY", round(r2,2), sep=", R-squared= "))
ri<-round(importance(rf), 2)
write.csv(ri,"Relative importance ARRIVAL_DELAY.csv")



##################try log transforming outcome###############

set.seed(1111)
dff3 <- dff2[dff2$ANYDELAY == "1",]
dff3$lARRIVAL_DELAY<-log(dff3$ARRIVAL_DELAY)
dff3<-dff3[,c("lARRIVAL_DELAY","MONTH","DAY_OF_WEEK","AIRLINE","SCHEDULED_DEPARTURE","SCHEDULED_TIME","SCHEDULED_ARRIVAL")]
select <- sample( 1:nrow(dff3), size=nrow(dff3)*0.8 )
training <- dff3[ select, ]
test <- dff3[ -select, ]

rfLOG <- randomForest(lARRIVAL_DELAY ~ MONTH + DAY_OF_WEEK + AIRLINE + SCHEDULED_DEPARTURE + SCHEDULED_TIME + SCHEDULED_ARRIVAL, data=training, ntree=500,mtry=4)

(r2 <- rSquared(test$lARRIVAL_DELAY, test$lARRIVAL_DELAY - predict(rfLOG, test)))
#0
(mse <- mean((test$lARRIVAL_DELAY - predict(rfLOG, test))^2))

p <- ggplot(aes(x=actual, y=pred),
            data=data.frame(actual=test$lARRIVAL_DELAY, pred=predict(rfLOG, test)))


p + geom_point() +
  geom_abline(color="red") +
  ggtitle(paste("Predicted vs actual ARRIVAL_DELAY", round(r2,2), sep=", R-squared= "))
ri<-round(importance(rfLOG), 2)
write.csv(ri,"Relative importance log ARRIVAL_DELAY.csv")

###TERRIBLE

# 
# 
# fips$STATE_NAME <-  toupper(fips$STATE_NAME) #state name and state (abb)
# weather<-weather[!is.na(weather$date) & weather$date != "",] #state name and date
# names(weather) = c('STATE_NAME','date')
# 
# origweather<- left_join(weather,fips);names(origweather) <- c('STATE_NAME','DATE','ORIG_STATE')
# destweather<- left_join(weather,fips);names(destweather) <- c('STATE_NAME','DATE','DEST_STATE')
# 
# origweather$DATE <- as.Date(factor(origweather$DATE), format = "%m/%d/%y")-365*5-1
# destweather$DATE <- as.Date(factor(origweather$DATE), format = "%m/%d/%y")-365*5-1
# origairport<-airports[,c(1,4)];names(origairport)<-c('ORIGIN_AIRPORT', 'ORIG_STATE')
# destairport <- origairport;names(destairport)<-c('DESTINATION_AIRPORT', 'DEST_STATE')
# flights$DATE<- as.Date(with(flights, paste(flights$YEAR, flights$MONTH, flights$DAY,sep="-")), "%Y-%m-%d") 
# 
# flightsorigap <- left_join(flights, origairport)
# flightsap <- left_join(flightsorigap, destairport)
# 
# flights0a <- left_join(flightsap, origweather)
# #Error: cannot allocate vector of size 44.4 Mb
# flights0b <- left_join( flights0a, destweather)
# 
# flights1 <- left_join(flights0b, weather1)
# flights_d0 <- flights[!is.na(flights$ARRIVAL_DELAY) & flights$ARRIVAL_DELAY >0,]
# flights_n0 <- flights[!is.na(flights$ARRIVAL_DELAY) & flights$ARRIVAL_DELAY ==0,]
# 
# seed=411
# 
# flights1a <-left_join(rbind.data.frame(flights_d,flights_n), airlines)
# flights1b <-left_join(flights1a, airports)
# flights1c <-left_join(flights1b, weather)
# 
# #################FROM PREVIOUS ONLY########################
# 
# flights1$AIRLINE=as.factor(flights1$AIRLINE)
# flights1$MONTH=as.factor(flights1$MONTH)
# flights1$DAY_OF_WEEK=as.factor(flights1$DAY_OF_WEEK)
# flights1$ORIGIN_AIRPORT=as.factor(flights1$ORIGIN_AIRPORT)
# flights1$DESTINATION_AIRPORT=as.factor(flights1$DESTINATION_AIRPORT)
# 
# 
# flights2<-flights1[!is.na(flights1$ARRIVAL_DELAY)&!is.na(flights1$MONTH)&!is.na(flights1$DAY_OF_WEEK)&!is.na(flights1$AIRLINE)&!is.na(flights1$SCHEDULED_DEPARTURE)&!is.na(flights1$SCHEDULED_ARRIVAL)&!is.na(flights1$ORIGIN_AIRPORT)&!is.na(flights1$DESTINATION_AIRPORT),]
# flights2$ARRIVAL_DELAY2 <- flights2$ARRIVAL_DELAY + 1 #add a minute to all flight delays to have a strictly positive outcome for a log-gamma regression
# flights2$lARRIVAL_DELAY2 <- log(flights2$ARRIVAL_DELAY + 1)  #log-transform to compare to prediction from log-gamma regression
# flights2$ARRIVALD01 <- as.factor(as.numeric(flights2$ARRIVAL_DELAY > 15)) #create a dichotomous variable 0 if no delay, 1 if delay greater than 15 minutes
# 
# seed=411
# k<-sample(nrow(flights2), 0.8*nrow(flights2))
# cols <- c("ARRIVAL_DELAY", "MONTH"    ,   "DAY_OF_WEEK"        ,"AIRLINE"  ,     "SCHEDULED_DEPARTURE"   ,     "SCHEDULED_ARRIVAL" ) #,   "ORIGIN_AIRPORT",'DESTINATION_AIRPORT'  )
# cols_z<-c("ARRIVALD01", "MONTH"    ,   "DAY_OF_WEEK"        ,"AIRLINE"  ,     "SCHEDULED_DEPARTURE"   ,     "SCHEDULED_ARRIVAL" ) #,   "ORIGIN_AIRPORT",'DESTINATION_AIRPORT'  )
# cols_nozero<-c(  "ARRIVAL_DELAY", "MONTH"    ,   "DAY_OF_WEEK"        ,"AIRLINE"  ,     "SCHEDULED_DEPARTURE"   ,     "SCHEDULED_ARRIVAL" ) #,   "ORIGIN_AIRPORT",'DESTINATION_AIRPORT'  )
# 
# predictors<-c( "MONTH"    ,   "DAY_OF_WEEK"        ,"AIRLINE"  ,     "SCHEDULED_DEPARTURE"   ,     "SCHEDULED_ARRIVAL" ) #,   "ORIGIN_AIRPORT",'DESTINATION_AIRPORT'  )
# 
# train<-flights2[k,cols_z]
# test<-flights2[-k,cols_z]
# test_nozero<-flights2[flights2$ARRIVAL_DELAY > 0, cols_nozero][k,] #only those with delays
# train_nozero<-flights2[flights2$ARRIVAL_DELAY > 0, cols_nozero][k,] #only those with delays
# 
# #plot the distribution of the arrival delays and the log-transformed positive arrival delays
# 
# par(mfrow=c(2,1))
# hist(flights2[k,cols]$ARRIVAL_DELAY,main="Arrival Delay Training Set",xlab= "Delay")
# hist(log(train_nozero$ARRIVAL_DELAY),main="Log-transformed Possitive Arrival Delay",xlab= "Delay") #looks more bell-shaped...probably ok for linear regression
# par(mfrow=c(1,1))
# 
# ######WILL THE FLIGHT BE DELAYED?###########
# ######WILL THE FLIGHT BE DELAYED?###########
# ######WILL THE FLIGHT BE DELAYED?###########
# # use a log-binomial model since it is a common outcome...if it doesn't converge will use logistic
# #Outcome=ARRIVALD01; Predictors= MONTH, DAY_OF_WEEK, AIRLINE, SCHEDULED_DEPARTURE, SCHEDULED_ARRIVAL
# train$ARRIVALD01 <- relevel(factor(train$ARRIVALD01),"1")
# LB_mod1 <- glm(ARRIVALD01 ~ MONTH + DAY_OF_WEEK + AIRLINE + SCHEDULED_DEPARTURE + SCHEDULED_ARRIVAL, data=train, family=binomial(link="log")) #didn't converge
# 
# LB_mod2 <- glm(ARRIVALD01 ~ MONTH + DAY_OF_WEEK + AIRLINE + SCHEDULED_DEPARTURE + SCHEDULED_ARRIVAL, data=train, family=binomial(link="logit"))
# pred(LB_mod2, test)
# 
# 
# 
# 
# # define training control
# train_control<- trainControl(method="cv", number=10)
# 
# # train the model 
# model<- train(traget~., data=dataset, trControl=train_control, method="glm", family=binomial())
# 
# # print cv scores
# summary(model)
# 
# 
# 
# 
# ######IF DELAYED WILL IT BE MORE THAN 15 MINUTES?#####################
# #expect this is not adequate for the delay data which is very skewed
# 
# linear_mod1<-lm(ARRIVAL_DELAY ~ as.factor(MONTH) + as.factor(DAY_OF_WEEK) + as.factor(AIRLINE) + SCHEDULED_DEPARTURE + SCHEDULED_ARRIVAL , data=train)
# ggplot(aes(x=actual, y=pred),
#        data=data.frame(actual=test$ARRIVAL_DELAY, pred=predict(linear_mod1, test[,predictors]))) + geom_point() +
#   geom_abline(intercept=0, slope=1, color="red") +
#   ggtitle(paste("Predicted vs actual arrival delay"))
# 
# hist(test$lARRIVAL_DELAY2)
# 
# ######GAMMA REGRESSION#######################
# 
# glmGamma_mod1 <- glm(ARRIVAL_DELAY2 ~ as.factor(MONTH) + as.factor(DAY_OF_WEEK) + as.factor(AIRLINE) + SCHEDULED_DEPARTURE + SCHEDULED_ARRIVAL , family = Gamma(link = "log"), data=train)
# 
# summary(glmGamma_mod1)   
# ggplot(aes(x=actual, y=pred),
#             data=data.frame(actual=test$lARRIVAL_DELAY2, pred=predict(glmGamma_mod1, test[,predictors]))) + geom_point() +
#   geom_abline(intercept=0, slope=1, color="red") +
#   ggtitle(paste("Predicted vs log transformed actual arrival delay"))
# 
# #terrible fit...too skewed even for a gamma
# #this really needs a two-part gamma but that is too much trouble to carry out for the expected rewards of doing it
# 
# 
# 
# ###################
# ######RANDOM FOREST REGRESSION################                
# 
# #bestmtry <- tuneRF(train[,-1],train[,1], ntreeTry=200, stepFactor=1.5,improve=0.01, trace=TRUE, plot=TRUE, dobest=FALSE) 
# 
# rf <- randomForest(ARRIVAL_DELAY ~ ., data=train[,cols], ntree=500,mtry=4)
# 
# (r2 <- rSquared(test$ARRIVAL_DELAY, test$ARRIVAL_DELAY - predict(rf, test[,cols])))
#
# (mse <- mean((test$ARRIVAL_DELAY - predict(rf, test[,cols]))^2))
# 
# p <- ggplot(aes(x=actual, y=pred),
#             data=data.frame(actual=test$ARRIVAL_DELAY, pred=predict(rf, test[,cols])))
# 
# 
# p + geom_point() +
#   geom_abline(color="red") +
#   ggtitle(paste("Predicted vs actual ARRIVAL_DELAY", round(r2,2), sep=", R-squared= "))
# ri<-round(importance(rf), 2)
# write.csv(ri,"Relative importance ARRIVAL_DELAY.csv")
# 
# 
