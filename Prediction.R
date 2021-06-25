##### Setup #####
library(BAS); library(readxl); library(dplyr); library(hydroGOF); library(GGally); library(plotly); library(ggplot2); library(Metrics)

### Training Data ###
original = read_excel("C:/Users/brenn/Box Sync/App/AP Water Quality Master Sheet.xlsx")
edited = original[,c(1,6:9)]
colnames(edited) = c("Date","feed","pH","Temp","DO")
edited$pH = as.numeric(edited$pH)

daily = edited %>% filter(Date>=as.POSIXct("2018-01-01"),between(feed,0,7)&between(pH,0,14)&between(Temp,0,100)&between(DO,0,13)) %>% na.omit()
train = daily %>% mutate(feed1=lag(feed,1),pH1=lag(pH,1),Temp1=lag(Temp,1),DO1=lag(DO,1),
																		 feed2=lag(feed,2),pH2=lag(pH,2),Temp2=lag(Temp,2),DO2=lag(DO,2),
																		 feed3=lag(feed,3),pH3=lag(pH,3),Temp3=lag(Temp,3),DO3=lag(DO,3),
																		 feed4=lag(feed,4),pH4=lag(pH,4),Temp4=lag(Temp,4),DO4=lag(DO,4),
																		 feed5=lag(feed,5),pH5=lag(pH,5),Temp5=lag(Temp,5),DO5=lag(DO,5),
																		 feed6=lag(feed,6),pH6=lag(pH,6),Temp6=lag(Temp,6),DO6=lag(DO,6),
																		 feed7=lag(feed,7),pH7=lag(pH,7),Temp7=lag(Temp,7),DO7=lag(DO,7)) %>% na.omit()

### Testing Data ###
testdata = read_excel("C:/Users/brenn/Box Sync/App/AP Water Quality Master Sheet.xlsx",sheet=2)
editedX = testdata[,c(1,3:6)]
colnames(editedX) = c("Date","feed","pH","Temp","DO")
editedX$pH = as.numeric(editedX$pH)
editedX$DO = as.numeric(editedX$DO)

daily2 = editedX %>% filter(Date>=as.POSIXct("2018-01-01"),between(feed,0,7)&between(pH,0,14)&between(Temp,0,100)&between(DO,0,13)) %>% na.omit()
test = daily2 %>% mutate(feed1=lag(feed,1),pH1=lag(pH,1),Temp1=lag(Temp,1),DO1=lag(DO,1),
																		 feed2=lag(feed,2),pH2=lag(pH,2),Temp2=lag(Temp,2),DO2=lag(DO,2),
																		 feed3=lag(feed,3),pH3=lag(pH,3),Temp3=lag(Temp,3),DO3=lag(DO,3),
																		 feed4=lag(feed,4),pH4=lag(pH,4),Temp4=lag(Temp,4),DO4=lag(DO,4),
																		 feed5=lag(feed,5),pH5=lag(pH,5),Temp5=lag(Temp,5),DO5=lag(DO,5),
																		 feed6=lag(feed,6),pH6=lag(pH,6),Temp6=lag(Temp,6),DO6=lag(DO,6),
																		 feed7=lag(feed,7),pH7=lag(pH,7),Temp7=lag(Temp,7),DO7=lag(DO,7)) %>% na.omit()


##### Prediction --- tilapia system (training) and koi system (testing) #####
# Anything with "lm1" is 1st linear model, "lm2" is 2nd linear model, "b1" is 1st Bayesian model, and "b2" is 2nd Bayesian model
lm1 = lm(pH~pH1+pH2,train); summary(lm1)  # R2 = 0.7725
lm2 = lm(pH~feed1+feed3+pH1+pH2+pH3+pH5+pH6+DO1+DO2+DO4+DO6,train); summary(lm2)  # R2 = 0.7807
b1 = lm(pH~feed1+Temp1+Temp6+pH1+pH2+DO2+DO5,train); summary(b1)  # R2 = 0.7857
b2 = lm(pH~feed1+Temp1+pH1+pH2+DO2+DO5,train); summary(b2)  # R2 = 0.7849

plm1 = predict.lm(lm1,test)  # prediction
plm2 = predict.lm(lm2,test)
pb1 = predict.lm(b1,test)
pb2 = predict.lm(b2,test)

output = cbind(test$pH,plm1,plm2,pb1,pb2) %>% as.data.frame()
colnames(output) = c("actual","plm1","plm2","pb1","pb2")

eval_lm1 = rmse(output$actual,output$plm1); eval_lm1  # RMSE = 0.148
gof_lm1 = NSE(output$plm1,output$actual); gof_lm1  # GOF = 0.801

eval_lm2 = rmse(output$actual,output$plm2); eval_lm2  # RMSE = 0.185
gof_lm2 = NSE(output$plm2,output$actual); gof_lm2  # GOF = 0.69

eval_b1 = rmse(output$actual,output$pb1); eval_b1  # RMSE = 0.218
gof_b1 = NSE(output$pb1,output$actual); gof_b1  # GOF = 0.57

eval_b2 = rmse(output$actual,output$pb2); eval_b2  # RMSE = 0.223
gof_b2 = NSE(output$pb2,output$actual); gof_b2  # GOF = 0.55

##### Sensitivity and Specificity #####
out = output[,c("actual","plm1")]
out2 = out %>% mutate(actual_less = actual < 6.4, actual_more = actual > 7.4,pred_less=plm1<6.4,pred_more=plm1>7.4)  # establish bounds
pred = cbind(out2$actual_less,out2$pred_less)
colnames(pred) = c("actual","predict")
pred = data.frame(pred)
pred$actual = as.character(pred$actual)  # make variables characters to make sorting easier
pred$predict = as.character(pred$predict)

# Calcuate true/false negative/positive for best model
pred2 = pred %>% as.data.frame() %>% mutate(test=actual==predict)  # test whether actual values were matched by predicted
pred3 = pred2 %>% filter(test==FALSE) # misclassifications (false negatives/positives)
pred4 = pred2 %>% filter(test==TRUE)  # correct assignments
TP = pred4 %>% filter(actual==TRUE)  # true positive
TN = pred4 %>% filter(actual==FALSE)  # true negative
FN = 3  # false negative (manually counted rows with predicted = FALSE and actual = TRUE)
FP = 5  # false positive (manually counted rows with predicted = TRUE and actual = FALSE)

sensitivity = nrow(TP)/(nrow(TP)+FN)  # 0.78 --- ability to predict boundary violations --- true positive/(true positive + false negative)
specificity = nrow(TN)/(nrow(TN)+FP)  # 0.99 --- ability to predict non-violations --- true negative/(true negative + false positive)

### Calculate the deviation of predictions from the actual measurements ###

# Isolate true/false positives/negatives
TP_out = out2 %>% filter(actual_less == TRUE & pred_less == TRUE | actual_more == TRUE & pred_more == TRUE)  # true positive
TN_out = out2 %>% filter(actual_less == FALSE & actual_more == FALSE & pred_less == FALSE & pred_more == FALSE)  # true negative
FP_out = out2 %>% filter(actual_less == FALSE & pred_less == TRUE | actual_more == FALSE & pred_more == TRUE)  # false positive
FN_out = out2 %>% filter(actual_less == TRUE & pred_less == FALSE | actual_more == TRUE & pred_more == FALSE)  # false negative

# Summary statistics of deviations for true positive and false negative
TP_out2 = mutate(TP_out,diff = abs(actual-plm1))
mean(TP_out2$diff); sd(TP_out2$diff)

FN_out2 = mutate(FN_out,diff = abs(actual-plm1))
mean(FN_out2$diff); sd(FN_out2$diff)

##### Prediction --- koi system (training) and tilapia system (testing) #####
lm1 = lm(pH~pH1+pH2,test); summary(lm1) 
lm2 = lm(pH~feed1+feed3+pH1+pH2+pH3+pH5+pH6+DO1+DO2+DO4+DO6,test); summary(lm2)  
b1 = lm(pH~feed1+Temp1+Temp6+pH1+pH2+DO2+DO5,test); summary(b1)  
b2 = lm(pH~feed1+Temp1+pH1+pH2+DO2+DO5,test); summary(b2)  

plm1 = predict.lm(lm1,train)
plm2 = predict.lm(lm2,train)
pb1 = predict.lm(b1,train)
pb2 = predict.lm(b2,train)

output = cbind(train$pH,plm1,plm2,pb1,pb2) %>% as.data.frame()
colnames(output) = c("actual","plm1","plm2","pb1","pb2")

eval_lm1 = rmse(output$actual,output$plm1); eval_lm1  # RMSE = 0.159
gof_lm1 = NSE(output$plm1,output$actual); gof_lm1  # GOF = 0.766

eval_lm2 = rmse(output$actual,output$plm2); eval_lm2  # RMSE = 0.249
gof_lm2 = NSE(output$plm2,output$actual); gof_lm2  # GOF = 0.428

eval_b1 = rmse(output$actual,output$pb1); eval_b1  # RMSE = 0.203
gof_b1 = NSE(output$pb1,output$actual); gof_b1  # GOF = 0.619

eval_b2 = rmse(output$actual,output$pb2); eval_b2  # RMSE = 0.25
gof_b2 = NSE(output$pb2,output$actual); gof_b2  # GOF = 0.423
