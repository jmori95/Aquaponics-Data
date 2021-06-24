##### PREDICTIVE MODEL FOR PH IN AQUAPONICS #####

##### DATA UPLOAD AND CLEANING #####
library(readxl); library(dplyr); library(bootStepAIC); library(ggplot2); library(reshape2); library(corrplot)
library(tidyr); library(naniar); library(psych); library(Metrics); library(hydroGOF)

# Training Data 
original = read_excel("C:/Users/brenn/Box Sync/App/AP Water Quality Master Sheet.xlsx")
edited = original[,c(1,6:13)]
colnames(edited) = c("Date","feed","pH","Temp","DO","NH4","NO2","NO3","kH")
edited$pH = as.numeric(edited$pH)
edited$NO2 = as.numeric(edited$NO2)
edited2 = edited[,2:9]
edited3 = melt(edited2) %>% na.omit()
edited3$value = as.numeric(edited3$value)

daily = edited %>% select(Date,pH,Temp,feed,DO) %>% filter(Date>=as.POSIXct("2018-01-01"),between(feed,0,7) & 
				between(pH,0,14) & between(Temp,0,100) & between(DO,0,13)) %>% na.omit()  

lag_daily = daily %>% mutate(feed1=lag(feed,1),pH1=lag(pH,1),Temp1=lag(Temp,1),DO1=lag(DO,1),
																		 feed2=lag(feed,2),pH2=lag(pH,2),Temp2=lag(Temp,2),DO2=lag(DO,2),  # lag variables up to 7 days
																		 feed3=lag(feed,3),pH3=lag(pH,3),Temp3=lag(Temp,3),DO3=lag(DO,3),
																		 feed4=lag(feed,4),pH4=lag(pH,4),Temp4=lag(Temp,4),DO4=lag(DO,4),
																		 feed5=lag(feed,5),pH5=lag(pH,5),Temp5=lag(Temp,5),DO5=lag(DO,5),
																		 feed6=lag(feed,6),pH6=lag(pH,6),Temp6=lag(Temp,6),DO6=lag(DO,6),
																		 feed7=lag(feed,7),pH7=lag(pH,7),Temp7=lag(Temp,7),DO7=lag(DO,7)) %>% na.omit()

# Testing Data
testdata = read_excel("C:/Users/brenn/Box Sync/App/AP Water Quality Master Sheet.xlsx",sheet=2)
testdata$D.O. = as.numeric(testdata$D.O.)
testing = testdata %>% filter(Date >= as.POSIXct("2018-01-01")) %>% select(Date,Feed,Temp,pH,D.O.) %>% 
	na.omit() %>% filter(between(pH,0,14) & D.O.<=13)
colnames(testing) = c("Date","feed","Temp","pH","DO")
test = testing %>% mutate(feed1=lag(feed,1),pH1=lag(pH,1),Temp1=lag(Temp,1),DO1=lag(DO,1),
																		 feed2=lag(feed,2),pH2=lag(pH,2),Temp2=lag(Temp,2),DO2=lag(DO,2),
																		 feed3=lag(feed,3),pH3=lag(pH,3),Temp3=lag(Temp,3),DO3=lag(DO,3),
																		 feed4=lag(feed,4),pH4=lag(pH,4),Temp4=lag(Temp,4),DO4=lag(DO,4),
																		 feed5=lag(feed,5),pH5=lag(pH,5),Temp5=lag(Temp,5),DO5=lag(DO,5),
																		 feed6=lag(feed,6),pH6=lag(pH,6),Temp6=lag(Temp,6),DO6=lag(DO,6),
																		 feed7=lag(feed,7),pH7=lag(pH,7),Temp7=lag(Temp,7),DO7=lag(DO,7)) %>% na.omit() 

### Establish reasonable ranges for each variable 

# pH
vp = edited2$pH %>% as.data.frame() %>% filter(between(.,0,14))  # natural limits of pH
summary(vp)

# Temperature (F)
vt = edited2$Temp %>% as.data.frame() %>% filter(.>32)  # above freezing
summary(vt)

# Dissolved oxygen (ppm)
vd = edited2$DO %>% as.data.frame() %>% filter(between(.,0,20))  # rough upper limit based on chemistry and water temperature
summary(vd)

# Ammonia species
vn4 = as.data.frame(edited2$NH4); summary(vn4)  # NH4
vn2 = as.data.frame(edited2$NO2); summary(vn2)  # NO2
vn3 = as.data.frame(edited2$NO3); summary(vn3)  # NO3

# kH (dKH)
vk = edited2$kH %>% as.data.frame() %>% filter(.<=20)  # rough upper limit based on water chemistry
summary(vk)

### Variable density plots ###

# Isolate variables and restrict to values within the range of min to max, as calculated above
feed = edited3 %>% filter(variable=="feed") 
pH = edited3 %>% filter(variable=="pH" & between(value,5,7.7))
Temp = edited3 %>% filter(variable=="Temp" & between(value,50,100))
DO = edited3 %>% filter(variable=="DO" & between(value,0.7,20))
NH4 = edited3 %>% filter(variable=="NH4" & between(value,0,2.8))
NO2 = edited3 %>% filter(variable=="NO2" & between(value,0,2))
NO3 = edited3 %>% filter(variable=="NO3" & between(value,3,180))
kH = edited3 %>% filter(variable=="kH" & between(value,1,6))

# Density plot generation
feed_plot = ggplot(feed,aes(x=value))+geom_density(fill="grey",size=1.25)+labs(x="Fish Feed (cups)",y="Density")+theme_bw()+
	theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(breaks = c(0,0.1,0.2,0.3,0.4,0.5,0.6),expand = c(0,0))+
	scale_x_continuous(expand=c(0,0),limits=c(0,6))+annotate("text",x=5.7,y=0.59,label="A",fontface="bold",size=5); feed_plot

pH_plot = ggplot(pH,aes(x=value))+geom_density(fill="grey",size=1.25)+geom_vline(xintercept = c(6.4,7.4),color="blue",size=1,linetype=2)+
	labs(x="pH",y="Density")+theme_bw()+theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(expand = c(0,0),breaks=seq(0,1,0.2),limits=c(0,1))+scale_x_continuous(expand=c(0,0),breaks=seq(0,14,1),limits=c(0,14))+
	annotate("text",x=13.5,y=0.97,label="B",fontface="bold",size=5)+
	annotate("text",x=3,y=0.6,label="Within Range: \n 74.6%",fontface="bold",size=4); pH_plot

Temp_plot = ggplot(Temp,aes(x=value))+geom_density(fill="grey",size=1.25)+labs(x="Temperature (F)",y="Density")+theme_bw()+
	geom_vline(xintercept = c(65,85),color="blue",size=1,linetype=2)+theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(expand = c(0,0),limits=c(0,0.09),breaks=seq(0,0.09,0.01))+
	scale_x_continuous(expand=c(0,0),breaks=seq(0,100,10),limits=c(0,100))+annotate("text",x=95,y=0.085,label="C",fontface="bold",size=5)+
	annotate("text",x=35,y=0.05,label="Within Range: \n 91.5%",fontface="bold",size=4); Temp_plot

DO_plot = ggplot(DO,aes(x=value))+geom_density(fill="grey",size=1.25)+labs(x="DO (ppm)",y="Density")+theme_bw()+
	geom_vline(xintercept = c(5,10),color="blue",size=1,linetype=2)+theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(expand = c(0,0),breaks=seq(0,0.35,0.05),limits=c(0,0.35))+
	scale_x_continuous(expand=c(0,0),breaks=seq(0,20,2),limits=c(0,14))+annotate("text",x=13.4,y=0.335,label="D",fontface="bold",size=5)+
	annotate("text",x=2.4,y=0.2,label="Within Range: \n 80.4%",fontface="bold",size=4); DO_plot

NH4_plot = ggplot(NH4,aes(x=value))+geom_density(fill="grey",size=1.25)+labs(x="NH4 (ppm)",y="Density")+theme_bw()+
	geom_vline(xintercept = c(0.02,1),color="blue",size=1,linetype=2)+theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(expand = c(0,0),limits=c(0,3),breaks=seq(0,3,0.5))+
	scale_x_continuous(expand=c(0,0),breaks=seq(0,3,0.5),limits=c(0,3))+annotate("text",x=2.85,y=2.85,label="E",fontface="bold",size=5)+
	annotate("text",x=2,y=1.8,label="Within Range: \n 95.5%",fontface="bold",size=4); NH4_plot

NO2_plot = ggplot(NO2,aes(x=value))+geom_density(fill="grey",size=1.25)+labs(x="NO2 (ppm)",y="Density")+theme_bw()+
	geom_vline(xintercept = c(0.02,1),color="blue",size=1,linetype=2)+theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(expand = c(0,0),limits=c(0,5),breaks=seq(0,5,1))+
	scale_x_continuous(expand=c(0,0),breaks=seq(0,2.5,0.5),limits=c(0,2.5))+annotate("text",x=2.4,y=4.8,label="F",fontface="bold",size=5)+
	annotate("text",x=1.75,y=3,label="Within Range: \n 99.5%",fontface="bold",size=4); NO2_plot

NO3_plot = ggplot(NO3,aes(x=value))+geom_density(fill="grey",size=1.25)+labs(x="NO3 (ppm)",y="Density")+theme_bw()+
	geom_vline(xintercept = c(5,150),color="blue",size=1,linetype=2)+theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(expand = c(0,0),limits=c(0,0.01),breaks=seq(0,0.01,0.0025))+
	scale_x_continuous(expand=c(0,0),breaks=seq(0,200,25),limits = c(0,210))+annotate("text",x=200,y=0.0096,label="G",fontface="bold",size=5)+
	annotate("text",x=80,y=0.0075,label="Within Range: \n 76.2%",fontface="bold",size=4); NO3_plot

kH_plot = ggplot(kH,aes(x=value))+geom_density(fill="grey",size=1.25)+labs(x="kH",y="Density")+theme_bw()+
	geom_vline(xintercept = c(2.8,5.6),color="blue",size=1,linetype=2)+theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+
	scale_y_continuous(expand = c(0,0),breaks=seq(0,0.6,0.1),limits=c(0,0.6))+
	scale_x_continuous(expand=c(0,0),limits=c(0,8),breaks=seq(0,8,1))+annotate("text",x=7.6,y=0.57,label="H",fontface="bold",size=5)+
	annotate("text",x=4.2,y=0.4,label="Within Range: \n 30.2%",fontface="bold",size=4); kH_plot


### Correlation matrix and plot ###

# Check for outliers
boxplot(edited2$feed)  # yes
boxplot(edited2$pH)  # yes
boxplot(edited2$Temp)  # yes
boxplot(edited2$DO)  # yes
boxplot(edited2$NH4)  # yes 
boxplot(edited2$NO2)  # yes
boxplot(edited2$NO3)  # no
boxplot(edited2$kH)  # yes

# Correlation analysis (choose Spearman because of outliers)
x = corr.test(edited2,method = "spearman")  # correlation matrix
corrplot(x$r,p.mat = x$p,insig = "p-value")
corPlot(x$r,colors = FALSE,stars = TRUE,pval = x$p,diag = FALSE,upper = FALSE,cex = 0.9)

xp = as.data.frame(x$p); write.csv(xp,"pvalues.csv")  # write p-values to csv file
xn = as.data.frame(x$n); write.csv(xn,"sample.csv")  # write sample size of each comparison to csv file

##### BOUNDARY VIOLATIONS #####
## STEPS:
	# Isolate variable from "edited" matrix and convert to dataframe
	# Get count of variable values within ideal range (equal to number of rows in vector)
	# Calculate % of values within this range by dividing the length of the ideal range vector by the length of the original vector

pH = edited$pH %>% as.data.frame()
pH_good = edited$pH %>% as.data.frame() %>% filter(between(.,6.4,7.4)) %>% na.omit()
length(pH_good$.)/length(pH$.)  # 74.6%

Temp = edited$Temp %>% as.data.frame() %>% na.omit()
Temp_good = Temp %>% filter(between(.,65,85))
length(Temp_good$.)/length(Temp$.)  # 91.5%

DO = edited$DO %>% as.data.frame() %>% na.omit()
DO_good = DO %>% filter(between(.,5,10))
length(DO_good$.)/length(DO$.)  # 80.4%

NH4 = edited$NH4 %>% as.data.frame() %>% na.omit()
NH4_good = NH4 %>% filter(between(.,0,1))
length(NH4_good$.)/length(NH4$.)  # 95.5%

NO2 = edited$NO2 %>% as.data.frame() %>% na.omit()
NO2_good = NO2 %>% filter(between(.,0,1))
length(NO2_good$.)/length(NO2$.)  # 99.5%

NO3 = edited$NO3 %>% as.data.frame() %>% na.omit()
NO3_good = NO3 %>% filter(between(.,5,150))
length(NO3_good$.)/length(NO3$.)  # 76.2%

kH = edited$kH %>% as.data.frame() %>% na.omit()
kH_good = kH %>% filter(between(.,2.8,5.6))
length(kH_good$.)/length(kH$.)  # 30.2%

##### MULTIPLE LINEAR REGRESSION MODEL --- PH, TEMPERATURE, DO, AND FEED ##### 
### Original model
daily.model = lm(pH~feed1+feed2+feed3+feed4+feed5+feed6+feed7+Temp1+Temp2+Temp3+Temp4+Temp5+Temp6+Temp7+pH1+pH2+pH3+pH4+pH5+pH6+pH7+
								 	DO1+DO2+DO3+DO4+DO5+DO6+DO7,lag_daily)
boot.stepAIC(daily.model,lag_daily)
chosen.daily.model = lm(pH~feed1+Temp1+Temp6+pH1+pH2+DO2+DO5,lag_daily); summary(chosen.daily.model)  # R2 = 0.7857

##### PREDICTIONS WITH LINEAR MODEL #####
## 2 rounds of outlier removal with Cook's D plot (remove 3 numbered values in each plot)

# Model based on trial and error
model = lm(pH~pH1+pH2,lag_daily)
plot(model,4) 
train2 = lag_daily[c(1:38,40:115,117,118,120:341),]
model = lm(pH~pH1 + pH2,train2)
plot(model,4)
train3 = train2[c(1:52,54:113,115:298,300:338),]
model = lm(pH~pH1+pH2,train3)
p = predict.lm(model,test)

output = cbind(test$pH,p) %>% as.data.frame()
colnames(output) = c("actual","predicted")
eval = rmse(output$actual,output$predicted); eval  # RMSE = 0.1807818
gof = NSE(output$predicted,output$actual); gof  # NSE = 0.7955051
summary(model)

# Model based on bootstrapped linear regression 
model2 = lm(pH~feed1+Temp1+Temp6+pH1+pH2+DO2+DO5,lag_daily)
plot(model2,4)
train2 = lag_daily[c(1:94,96:115,117,118,120:341),]
model2 = lm(pH~feed1+Temp1+Temp6+pH1+pH2+DO2+DO5,train2)
plot(model2,4)
train3 = train2[c(1:3,5:21,23:241,243:338),]
model2 = lm(pH~feed1+Temp1+Temp6+pH1+pH2+DO2+DO5,train3)
p2 = predict.lm(model2,test)

output = cbind(test$pH,p2) %>% as.data.frame()
colnames(output) = c("actual","predicted")
eval = rmse(output$actual,output$predicted); eval  # RMSE = 0.2129543
gof = NSE(output$predicted,output$actual); gof  # NSE = 0.7162435

# Graph of actual vs. predicted values for the better model (pH~pH1+pH2)
# Make sure to rerun that section since the second model section has the same variable names
ggplot(output,aes(x=actual,y=predicted))+geom_point()+theme_bw()+labs(x="Actual Values",y="Predicted Values")+
	theme(panel.grid = element_blank(),axis.title = element_text(face="bold"))+stat_smooth(method="lm",se=FALSE,color="black")+
	scale_y_continuous(breaks = seq(5.5,7.5,0.5),limits=c(5.5,7.5))+scale_x_continuous(breaks=seq(5.5,7.5,0.5),limits=c(5.5,7.5))

x = lm(actual~predicted,output)  # regress actual vs. predicted values to assess model fit
summary(x)  # statistics of fit

# Graph of regression residuals over time to detect seasonal effects
ggplot(train3,aes(x=Date,y=model$residuals))+geom_point()+geom_smooth(method="loess",color="black")+theme_bw()+labs(x="Date",y="Residuals")+
	theme(axis.title = element_text(face="bold"),panel.grid=element_blank())
