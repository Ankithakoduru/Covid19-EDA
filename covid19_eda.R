#Cleaning the workspace
rm(list=ls())
#importing libraries
library(rio)
library(moments)
library(Hmisc)
#importing the csv file
covid.data=read.csv("COVID19_line_list_data.csv")
describe(covid.data)
#cleaning the death data
covid.data$death.new=as.integer(covid.data$death!=0)
#calculating death rate with death.new
sum(covid.data$death.new)/nrow(covid.data)

#claim1 to prove: people who die are older than people who survive
#subsetting the dead and survived data
dead=subset(covid.data,death.new == 1)
survived=subset(covid.data,death.new == 0)
#calculating the mean age of the dead and survived
#ignoring the entries where age is unknown
mean(dead$age, na.rm=TRUE)
mean(survived$age, na.rm=TRUE)

#Is the difference in age of dead and survived statistically significant to prove the claim?
#Null hypothesis= true difference in means is equal to 0
#Alternate hypothesis= true difference in means is not equal to 0
t.test(survived$age, dead$age, alternative = "two.sided", conf.level = 0.95)
#Since, p<0.05, we reject the null hypothesis
#Conclusion: people who die of covid are much older than the people who survived

#claim2 to prove: gender has not effect in covid deaths
#subsetting men and women data
men=subset(covid.data,gender == "male")
women=subset(covid.data,gender == "female")
#calculating the mean deaths in men and women
#ignoring the entries where age is unknown
mean(men$death.new, na.rm=TRUE)
mean(women$death.new, na.rm=TRUE)

#Is the difference in death rate of men and women statistically significant to prove the claim?
#Null hypothesis= true difference in means is equal to 0
#Alternate hypothesis= true difference in means is not equal to 0
t.test(men$death.new, women$death.new, alternative = "two.sided", conf.level = 0.95)
#Since, p<0.05, we reject the null hypothesis; It is statistically significant
#Conclusion: men have higher death rate than women