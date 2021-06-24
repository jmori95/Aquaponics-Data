##### Bayesian pH model #####
library(BAS); library(readxl); library(dplyr); library(bootStepAIC); library(GGally); library(plotly); library(ggplot2)
original = read_excel("C:/Users/brenn/Box Sync/App/AP Water Quality Master Sheet.xlsx")
edited = original[,c(1,6:13)]
colnames(edited) = c("Date","feed","pH","Temp","DO","NH4","NO2","NO3","kH")
edited$pH = as.numeric(edited$pH)
edited$NO2 = as.numeric(edited$NO2)

daily = edited %>% select(Date,pH,Temp,feed,DO) %>% filter(Date>=as.POSIXct("2018-01-01"),between(feed,0,7) & 
				between(pH,0,14) & between(Temp,0,100) & between(DO,0,13)) %>% na.omit()  # variables isolated and outliers removed
lag_daily = daily %>% mutate(feed1=lag(feed,1),pH1=lag(pH,1),Temp1=lag(Temp,1),DO1=lag(DO,1),
																		 feed2=lag(feed,2),pH2=lag(pH,2),Temp2=lag(Temp,2),DO2=lag(DO,2),  # lag variables up to 7 days
																		 feed3=lag(feed,3),pH3=lag(pH,3),Temp3=lag(Temp,3),DO3=lag(DO,3),
																		 feed4=lag(feed,4),pH4=lag(pH,4),Temp4=lag(Temp,4),DO4=lag(DO,4),
																		 feed5=lag(feed,5),pH5=lag(pH,5),Temp5=lag(Temp,5),DO5=lag(DO,5),
																		 feed6=lag(feed,6),pH6=lag(pH,6),Temp6=lag(Temp,6),DO6=lag(DO,6),
																		 feed7=lag(feed,7),pH7=lag(pH,7),Temp7=lag(Temp,7),DO7=lag(DO,7)) %>% na.omit()

##### Modeling #####
model = bas.lm(formula=pH~feed1+feed2+feed3+feed4+feed5+feed6+feed7+Temp1+Temp2+Temp3+Temp4+Temp5+Temp6+Temp7+pH1+pH2+pH3+pH4+pH5+pH6+pH7+
								 	DO1+DO2+DO3+DO4+DO5+DO6+DO7,data=lag_daily,method="MCMC+BAS",prior = "AIC",modelprior = uniform(),MCMC.iterations = 20000)
model; summary(model)
