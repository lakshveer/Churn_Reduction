getwd()

#Libraries used
library(ggplot2)
library(gridExtra)
library(DMwR)
library(corrgram)
library(smotefamily)
library(pROC)
library(ROCR)
library(ParamHelpers)
library(mlr)
library(caret)
library(e1071)


#Loading train and test data

train_data = read.csv("Train_data.csv", header = T, as.is = T)
test_data = read.csv("Test_data.csv", header = T, as.is = T)

#Percentage of train and test data
Total_TrainObs = nrow(train_data)
Total_TestObs = nrow(test_data)
Total_obs = Total_TrainObs + Total_TestObs
Train_percent = (Total_TrainObs*100)/Total_obs
Test_percent = Total_TestObs*100/Total_obs
#So Our train data percentage is 66.6 and test data percentage is 33.34


#Analysing raw data
dim(train_data)
dim(test_data)
colnames(train_data)
Top10 = head(train_data)
str(train_data)
summary(train_data)
typeof(summary(train_data))


#Merging test and train data as this will be required for data cleaning and feature engineering
Complete_data = rbind(train_data, test_data)


#Completeness of data
Missing_Value = data.frame(sapply(Complete_data, function(x){sum(is.na(x))}))
#There is no missing value in the train and test data
colnames(Missing_Value)[1] = 'Missing_Value_Count'
write.csv(Missing_Value, 'Missing_value.csv')
#There seems to be no missing value in the entire dataset which is pretty good.



#**********Exploring some of the important variables

#Converting the variables in factors
#State, area.code , internation.plan, voice.mail.plan, number.service.calls, Churn
factor_variables = c("state", "area.code", "international.plan", "voice.mail.plan", "number.customer.service.calls", "Churn")
for(i in factor_variables)
{
  Complete_data[,i] = as.factor(Complete_data[,i])
}

#interger to numeric
int_variables = c('total.intl.calls','total.night.calls','total.eve.calls','total.day.calls','number.vmail.messages', 'account.length')
for (i in int_variables)
{
  Complete_data[,i] = as.numeric(Complete_data[,i])
}


#Exploring data
#The Response Variable : Churn
library(ggplot2)

#How many customers churned out
p1 = ggplot(Complete_data, aes(x = Churn, fill = Churn)) + geom_bar(stat = 'count', col = rainbow(2))  +
geom_label(stat='count',aes(label=..count..), size=5) + labs(ggtitle("Churning Out Count")) +
  theme_grey(base_size = 18)

#Checking churn  wrt state
#increasing the width between the bar using width parameter
p2 = ggplot(Complete_data, aes(x = state, fill = Churn, width= 5)) + geom_bar(stat = 'count') + 
  labs(ggtitle("Customer churn based on state")) + geom_label(stat='count',aes(label=..count..), size=5) +
  theme_grey()

#Checking the customer service call vs Churn out data
p3 = ggplot(Complete_data, aes(x = number.customer.service.calls, fill = Churn, width = 5))+
  geom_bar(stat = 'count', position = 'dodge') + geom_label(stat = 'count', aes(label= ..count..))+
   labs(ggtitle('Customer churn based on number of service calls made'))+theme_grey()

#checking churn out cutomer wrt to area code
p4 = ggplot(Complete_data, aes(x = area.code, fill = Churn, width = 1)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label =..count..)) +
  labs(ggtitle('Customer Churn based on their area code')) +
theme_grey()

#Checking the area wise customer service calls churn rate
p5 = ggplot(Complete_data, aes(x = number.customer.service.calls, fill = Churn)) +geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label =..count..)) + facet_grid(.~area.code) +
  labs(ggtitle('Customer churn out based on area wise service calls made'))+
  theme_grey()

#Checking churn with voice call plan
p6 = ggplot(Complete_data, aes(x = voice.mail.plan, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge')+
  labs(ggtitle("Number of Churned out customer wrt to voice plan")) + geom_label(stat = 'count', aes(label = ..count..)) +
  theme_grey()

#Checking international calls with Churn
p7 = ggplot(Complete_data, aes(x = international.plan, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) +
labs(ggtitle("Number of Churned out customer wrt to international paln")) +
theme_gray()

grid.arrange(p6,p7,ncol = 1)

p8 = ggplot(Complete_data, aes(x = number.customer.service.calls, fill = Churn)) +geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label =..count..)) + facet_grid(.~international.plan) +
  labs(ggtitle('Customer churn having international plan by no of service calls'))+
  theme_grey()

p9 = ggplot(Complete_data, aes(x = number.customer.service.calls, fill = Churn)) +geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label =..count..)) + facet_grid(.~voice.mail.plan) +
  labs(ggtitle('Customer churn having voice plan by no of service calls'))+
  theme_grey()

grid.arrange(p8, p9, ncol = 1)



#Making the density plot
p10 = ggplot(Complete_data, aes(x = total.day.charge)) + geom_density()
#Data is normally distributed
#Checking for even calls charge, night, international
p11 = ggplot(Complete_data, aes(x = total.eve.charge)) + geom_density()
p12 = ggplot(Complete_data, aes(x = total.night.charge)) + geom_density()
p13 = ggplot(Complete_data, aes(x = total.intl.charge)) + geom_density()
grid.arrange(p10, p11, p12, p13, ncol=2)
#Charges are normally distributed

#Distribution of charges wrt to churn
#using alpha to make it transparent
p14 = ggplot(Complete_data, aes(x = total.day.charge)) + geom_density(aes(group = Churn, colour= Churn, fill = Churn), alpha = 0.3)
p15 = ggplot(Complete_data, aes(x = total.eve.charge)) + geom_density(aes(group = Churn, colour= Churn, fill = Churn), alpha = 0.3)
p16 = ggplot(Complete_data, aes(x = total.night.charge)) + geom_density(aes(group = Churn, colour= Churn , fill = Churn) , alpha = 0.3)
p17 = ggplot(Complete_data, aes(x = total.intl.charge)) + geom_density(aes(group = Churn, colour= Churn , fill = Churn), alpha = 0.3)                                                                  
grid.arrange(p14, p15, p16, p17, ncol = 2)                                                          


#Lets check the distribution of other variables as well
p18 = ggplot(Complete_data, aes(x = total.day.minutes)) + geom_density()
p19 = ggplot(Complete_data, aes(x = total.day.calls)) + geom_density()
p20 = ggplot(Complete_data, aes(x = total.eve.minutes)) + geom_density()
p21 = ggplot(Complete_data, aes(x = total.eve.calls)) + geom_density()
p22 = ggplot(Complete_data, aes(x = total.night.minutes)) + geom_density()
p23 = ggplot(Complete_data, aes(x = total.night.calls)) + geom_density()
p24 = ggplot(Complete_data, aes(x = total.intl.minutes)) + geom_density()
p25 = ggplot(Complete_data, aes(x = total.intl.calls)) + geom_density()
p26 = ggplot(Complete_data, aes(x = number.customer.service.calls)) + geom_density()
p27 = ggplot(Complete_data, aes(x = number.vmail.messages)) + geom_density()
p28 = ggplot(Complete_data, aes(x = account.length)) + geom_density()
grid.arrange(p18, p19, p20, p21, p22, p23, p24, p25, p26, p27, p28, ncol = 3)


#Feature Engineering
#Total Minutes
Complete_data$total.minutes = with(Complete_data, Complete_data$total.day.minutes + Complete_data$total.eve.minutes + Complete_data$total.night.minutes + Complete_data$total.intl.minutes)

#Total Calls
Complete_data$total.calls = with(Complete_data, Complete_data$total.day.calls + Complete_data$total.eve.calls + Complete_data$total.night.calls + Complete_data$total.intl.calls)

#Total Charge
Complete_data$total.charge = with(Complete_data, Complete_data$total.day.charge + Complete_data$total.eve.charge + Complete_data$total.night.charge + Complete_data$total.intl.charge)

#Getting to the specific location using prefix of the phone number
Complete_data$prefix = gsub("-.*", "", Complete_data$phone.number)
Complete_data$line.number = gsub(".*-", "", Complete_data$phone.number)
Complete_data$phone.number =  NULL

#Exploring prefix
#converting to numeric
Complete_data$prefix = as.factor(Complete_data$prefix)
p29 = ggplot(Complete_data, aes(x = prefix, fill = Churn)) +geom_bar(stat = 'count')+ theme_grey()
p29

#Combing prefix with area code
p30 = ggplot(Complete_data, aes(x = prefix, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge') + facet_grid(.~area.code) +theme_grey()
p30

#Prefix does not give any information


#Density plot for engineering features
p31 = ggplot(Complete_data, aes(x = total.minutes)) + geom_density(aes(group = Churn, colour= Churn, fill = Churn), alpha = 0.3)
p32 = ggplot(Complete_data, aes(x = total.charge)) + geom_density(aes(group = Churn, colour= Churn, fill = Churn), alpha = 0.3)
p33 = ggplot(Complete_data, aes(x = total.calls)) + geom_density(aes(group = Churn, colour= Churn, fill = Churn), alpha = 0.3)
grid.arrange(p31, p32, p33, ncol = 2)

#Lets create a categorical variable for charges and minutes
#Converting numeric to categorical
Complete_data$charge_category[Complete_data$total.charge>20 & Complete_data$total.charge <=40] = 'Low'
Complete_data$charge_category[Complete_data$total.charge>40 & Complete_data$total.charge <=60] = 'Average'
Complete_data$charge_category[Complete_data$total.charge >60 & Complete_data$total.charge <=80] = 'High'
Complete_data$charge_category[Complete_data$total.charge>80 & Complete_data$total.charge<=100 ] = 'VHigh'

#Converting minutes to categorical as well
Complete_data$minutes_category[Complete_data$total.minutes>200 & Complete_data$total.minutes <=400] = 'Low'
Complete_data$minutes_category[Complete_data$total.minutes>400 & Complete_data$total.minutes <=600] = 'Average'
Complete_data$minutes_category[Complete_data$total.minutes >600 & Complete_data$total.minutes <=800] = 'High'
Complete_data$minutes_category[Complete_data$total.minutes>800 & Complete_data$total.minutes<=1000 ] = 'VHigh'

#Converting calls to categorical as well
Complete_data$call_category[Complete_data$total.calls>150 & Complete_data$total.calls <=225] = 'Low'
Complete_data$call_category[Complete_data$total.calls>225 & Complete_data$total.calls <=300] = 'Average'
Complete_data$call_category[Complete_data$total.calls >300 & Complete_data$total.calls <=375] = 'High'
Complete_data$call_category[Complete_data$total.calls>375 & Complete_data$total.calls<=450 ] = 'VHigh'


#Lets analyze these a bit

#Churn out based on call, minutes and charge category
p34 = ggplot(Complete_data, aes(x = charge_category, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle("Churn out based on charge category"))+
  theme_grey()


p35 = ggplot(Complete_data, aes(x = minutes_category, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) +labs(ggtitle("Churn out based on minutes category"))+
  theme_grey()
 
p36 = ggplot(Complete_data, aes(x = call_category, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) +labs(ggtitle("Churn out based on call category"))+
  theme_grey()

grid.arrange(p34,p35,p36, ncol = 1)

#Lets examine minutes, charge and call together with churn
p37 = ggplot(Complete_data, aes(x = charge_category, fill = Churn)) +geom_bar(stat = 'count')+
               geom_label(stat = 'count', aes(label = ..count..))+ labs(ggtitle("Churn out based on charge and minutes")) +
  facet_grid(.~minutes_category)+
  theme_grey()

p38 = ggplot(Complete_data, aes(x = call_category, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) +labs(ggtitle("Churn out based on call and minutes"))+
  theme_grey() +facet_grid(.~minutes_category)

p39 = ggplot(Complete_data, aes(x = charge_category, fill = Churn)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) +labs(ggtitle("Churn out based on charge and call"))+
  theme_grey() +facet_grid(.~call_category)

grid.arrange(p37,p38, p39, ncol = 1)


#Lets calculate service call based on each category
p40 = ggplot(Complete_data, aes(x = number.customer.service.calls, fill = Churn)) + 
  geom_bar(stat = 'count', position = 'dodge') + geom_label(stat = 'count', aes(label = ..count..)) + 
  labs(ggtitle('Churn out based on number of service calls and charge category')) +
  facet_grid(.~charge_category) +theme_grey()

p41 = ggplot(Complete_data, aes(x = number.customer.service.calls, fill = Churn))+
  geom_bar(stat = 'count', position = 'dodge') + geom_label(stat = 'count', aes(label = ..count..)) +
  labs(ggtitle('Churn out based on number of service calls and minutes category')) +
  facet_grid(.~minutes_category) +theme_grey()

p42 = ggplot(Complete_data, aes(x = number.customer.service.calls, fill = Churn))+
  geom_bar(stat = 'count', position = 'dodge') + geom_label(stat = 'count', aes(label = ..count..)) +
  labs(ggtitle('Churn out based on number of service calls and call category')) +
  facet_grid(.~call_category) +theme_grey()


grid.arrange(p40, p41, p42, ncol = 1)


#Lets create one more variable, average_minutesperCall
Complete_data$average.minutes.percall = Complete_data$total.minutes /Complete_data$total.calls
#Categorizing the average call variable
Complete_data$Avg_Min_call[Complete_data$average.minutes.percall >0 & Complete_data$average.minutes.percall <=1] = 'Low'
Complete_data$Avg_Min_call[Complete_data$average.minutes.percall >1 & Complete_data$average.minutes.percall <=2] = 'Average'
Complete_data$Avg_Min_call[Complete_data$average.minutes.percall >2 & Complete_data$average.minutes.percall <=3] = 'High'
Complete_data$Avg_Min_call[Complete_data$average.minutes.percall >3 & Complete_data$average.minutes.percall <=4] = 'VHigh'
#Lets visualize average minute per call with charge and churn
p43 = ggplot(Complete_data, aes(x = Avg_Min_call, fill= Churn)) +geom_bar(stat = 'count')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle('Churn out based on avg call and charge'))+
  theme_grey() +facet_grid(.~charge_category)
p43


#Lets visualize international plan based on charge category
p44 = ggplot(Complete_data, aes(x = charge_category, fill = Churn )) +geom_bar(stat='count') +
  geom_label(stat = 'count', aes(label = ..count..)) +labs(ggtitle('Churn out based on charge category and international plan'))+
  theme_grey() +facet_grid(.~international.plan)

p45 = ggplot(Complete_data, aes(x = charge_category, fill = Churn )) +geom_bar(stat='count') +
  geom_label(stat = 'count', aes(label = ..count..)) +labs(ggtitle('Churn out based on charge category and voice mail plan'))+
  theme_grey() +facet_grid(.~voice.mail.plan)

grid.arrange(p44, p45, ncol = 1)


#***********Data Pe-processing**********************
#Lets first convert the engineering feature into factor variable
fe_variables = c('charge_category','minutes_category','call_category', 'Avg_Min_call')
for(i in fe_variables)
{
  Complete_data[,i] = as.factor(Complete_data[,i])
  
}

#COnverting the engineering feature line number into numeric
Complete_data$line.number = as.numeric(Complete_data$line.number)


#As I want the levels of category to follow some order not using direct method to assign a label
#i.e for Low 1, Medium 2, High 3 and VHigh 4

Engineer_Cat_Fac = c('charge_category', 'minutes_category', 'call_category','Avg_Min_call')
for(i in Engineer_Cat_Fac)
{
  Complete_data[,i] = factor(Complete_data[,i], levels = c("Average", "High", "Low", "VHigh" ), labels = c( 2, 3, 1,4))
}

#Replacing all the other string factor variable into integer levels
Other_fac = c('state', 'international.plan', 'voice.mail.plan','Churn')

for(i in Other_fac){
  
    Complete_data[,i] = factor(Complete_data[,i], labels=(1:length(levels(factor(Complete_data[,i])))))
    
  }

#********************Outlier Analysis**********************

numeric_index = sapply(Complete_data,is.numeric) #selecting only numeric

numeric_data = Complete_data[,numeric_index]

cnames = colnames(numeric_data)
 
 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Churn"), data = subset(Complete_data))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="Churn")+
            ggtitle(paste("Box plot of Churn for",cnames[i])))
 }
 
# ## Plotting together
gridExtra::grid.arrange(gn1,gn2,gn3,gn4,ncol=2)
gridExtra::grid.arrange(gn5,gn6,gn7,gn8,ncol=2)
gridExtra::grid.arrange(gn9,gn10,gn11,gn12,ncol=2)
gridExtra::grid.arrange(gn13,gn14,gn15,gn16, ncol=2)
gridExtra::grid.arrange(gn17,gn18,gn19, ncol=2)


#As our data is less we will impute the outliers treating them as missing value with the help of KnnImputation
for(i in cnames)
  {
  val = Complete_data[,i][Complete_data[,i] %in% boxplot.stats(Complete_data[,i])$out]
  print(length(val))
  Complete_data[,i][Complete_data[,i] %in% val] = NA
  }

#Imputation
#As knnimputation takes only numeric variables into consideration, converting all the 
Complete_data = knnImputation(Complete_data, k = 3)

#Confirming that there is no missing value left
sum(is.na(Complete_data))


#*****************Correlation Plot********************
## Correlation Plot 
corrgram(Complete_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#Here we can see that day charge and day minute, eve charge and eve min 
#night charge and night min, and inter charge and intern min are highly correlted
#We can remove any one of each from the dataset

factor_index = sapply(Complete_data,is.factor)
factor_data = Complete_data[,factor_index]
#Removing the Churn variable
factor_data$Churn = NULL
fcname = colnames(factor_data)


for (i in 1:length(fcname))
{
  print(names(factor_data)[i])
  print(chisq.test(table(Complete_data$Churn, factor_data[,i])))
}

#Comparing correleated variables with Churn
#total.day.minutes, total.day.charge, total.eve.minutes, total.eve.charge, total.night.minutes
#total.night.charge,total.intl.minutes, total.intl.charge

#Checking the correlation with churn using anova to find  which correlated variables to remove
#From the correlated variable
#The one which contributes the less in defining churn should be removed.

avo1 = aov(Complete_data$total.day.minutes ~ Complete_data$Churn)
avo2 = aov(Complete_data$total.day.charge ~ Complete_data$Churn)
avo3 = aov(Complete_data$total.eve.minutes ~ Complete_data$Churn)
avo4 = aov(Complete_data$total.eve.charge ~ Complete_data$Churn)
avo5 = aov(Complete_data$total.night.minutes ~ Complete_data$Churn)
avo6 = aov(Complete_data$total.night.charge ~ Complete_data$Churn)
avo7 = aov(Complete_data$total.intl.minutes ~ Complete_data$Churn)
avo8 = aov(Complete_data$total.intl.charge ~ Complete_data$Churn)
avo9 = aov(Complete_data$total.minutes ~ Complete_data$Churn)
avo10 = aov(Complete_data$total.charge ~ Complete_data$Churn)
avo11 = aov(Complete_data$average.minutes.percall ~ Complete_data$Churn)
#as line number is not correlated with any independent variable and line.number seems
#to be independent with the target variable as well. Removing the line number as well
avo12 = aov(Complete_data$line.number ~ Complete_data$Churn)
#Voice mail which is not correlated with numeric but is depended on Churn
avo13 = aov(Complete_data$number.vmail.messages ~ Complete_data$Churn)


#Not removing account length but can consider to check the accuracy as it doesn't give
#any information to the model.
#Account Length
avo14 = aov(Complete_data$account.length ~ Complete_data$Churn)

#Every correlated element comes out with same p value. So eleminating based on variables.

Complete_data = subset(Complete_data, select = -c(area.code, prefix, call_category, total.day.minutes, total.eve.minutes,
                                                  total.night.minutes, total.intl.minutes, total.minutes, total.day.calls, total.eve.calls,
                                                  total.night.calls, total.intl.calls, total.day.charge, total.eve.charge,
                                                  total.night.charge, total.intl.charge, average.minutes.percall, line.number))

#After feature selection we have 11 independent variables and 1 dependent variable.


#Keeping the Churn variable at last
Complete_data = Complete_data[,c(1,2,3,4,5,6,8,9,10,11,12,7)]


#*************Feature Scaling***************

qqnorm(Complete_data$total.charge)
histogram(Complete_data$total.charge)
histogram(Complete_data$account.length)
qqnorm(Complete_data$account.length)
histogram(Complete_data$number.vmail.messages)
qqnorm(Complete_data$number.vmail.messages)

#Applying Standardization Techinque
str(Complete_data)
fs_stand_numeric_index = sapply(Complete_data, is.numeric)
fs_stand_numeric_data = Complete_data[,fs_stand_numeric_index]
fs_stand_names = colnames(fs_stand_numeric_data)

for (i in fs_stand_names)
{
  print(i)
  Complete_data[,i] = (Complete_data[,i] - mean(Complete_data[,i]))/
                                    sd(Complete_data[,i])
}


Complete_data$state = NULL
Complete_data$account.length = NULL
Complete_data$total.calls = NULL

Complete_data$Avg_Min_call = NULL
Complete_data$voice.mail.plan = NULL
Complete_data$number.vmail.messages = NULL





#***************Information Gain Plot***************************
train.task <- makeClassifTask(data = train_ig,target = "Churn")

#remove zero variance features
train.task <- removeConstantFeatures(train.task)

#get variable importance chart
var_imp = generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)



#Removing 4 variables after infomation gain plot which are not contributing anything to response variable
Complete_data$state = NULL
Complete_data$account.length = NULL
Complete_data$total.calls = NULL

Complete_data$Avg_Min_call = NULL
Complete_data$voice.mail.plan = NULL
Complete_data$number.vmail.messages = NULL




#***********Sampling****************

#Segregating the train and test data
train_preproc = Complete_data[1:nrow(train_data),]
test_preproc= Complete_data[3334:nrow(Complete_data),]
write.csv(train_preproc, "train_preproccedwithoutoversampling.csv")
write.csv(test_preproc, "test.csv")

#SMOTE: Sythetic Minority Oversampling Technique
#AS we have a 84:14% class imbalance, we will go for smote techique

train_preproc_smote = train_preproc
#Converting variables to numeric for SMOTE
for (i in 1: ncol(train_preproc_smote))
      {
        if(class(train_preproc_smote[,i]) == 'factor')
        {
          train_preproc_smote[,i] = as.numeric(train_preproc_smote[,i])
        }
}

smote_data = SMOTE(train_preproc_smote[,1:11], train_preproc_smote[,12], K = 3, dup_size = 0)


oversampled_data = smote_data[["syn_data"]]
#Renaming the class variable with Churn
colnames(oversampled_data)[12] = 'Churn'
oversampled_data$Churn = as.numeric(oversampled_data$Churn)

n_index = sapply(test_preproc, is.factor)
n_data = test_preproc[,n_index]
#Index of factor variables from train data
for (i in colnames(n_data))
{
  oversampled_data[,i] = round(oversampled_data[,i])
}


#Coverting train and test into numeric variable
for (i in colnames(test_preproc))
{
  if(class(test_preproc) != 'num')
  {
    test_preproc[,i] = as.numeric(test_preproc[,i])
  }
}

for (i in colnames(train_preproc))
{
  if(class(train_preproc) != 'num')
  {
    train_preproc[,i] = as.numeric(train_preproc[,i])
  }
}

train_after_smote = rbind(train_preproc_smote, oversampled_data)


train_after_smote$Churn = gsub(2,0, train_after_smote$Churn)
train_after_smote$Churn = as.numeric(train_after_smote$Churn)
test_preproc$Churn = gsub(2,0, test_preproc$Churn)
test_preproc$Churn = as.numeric(test_preproc$Churn)


#Trying a hack
#Converting train and test into numeric

#**********Model Generation******************
#Logistic Regression
logit_model = glm(Churn ~ ., data = train_after_smote, family = "binomial")
#Binomial as we have Flase and True

#summary of the model
summary(logit_model)

test = test_preproc
test$Churn = NULL


#predict using logistic regression
logit_Predictions = predict(logit_model, newdata = test, type = "response")
#type = response gives probablity.

write(capture.output(summary(logit_Predictions)), "logit_with_OOT_Oversampled_allnumeric.txt")

#convert prob
logit_Predictions = ifelse(logit_Predictions > 0.5, 1, 0)
  

##Evaluate the performance of classification model
ConfMatrix_RF = table(test_preproc$Churn, logit_Predictions)


#ROCR curve)

test_preproc$logit_Predictions = logit_Predictions
gg11 = roc(Churn ~logit_Predictions, data = test_preproc)
plot(gg11) 
auc(gg11)


ROCRpred = prediction(logit_Predictions, test_preproc$Churn)
ROCRperf = performance(ROCRpred, 'tpr','fpr')
gg12 = plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))



