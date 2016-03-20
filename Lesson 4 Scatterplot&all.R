getwd()
dim(pf)
setwd('C:/Users/CMD JMRC/Downloads/R files')


install.packages('ggplot2')
library(ggplot2)

qplot(age, friend_count, data = pf)

ggplot(aes(x = age, y = friend_count), data = pf)+geom_jitter(alpha = 1/20)+
  scale_x_continuous(limits = c(13, 90))+coord_trans(y = "sqrt")

ggplot(aes(x = age, y = friend_count), data = pf)+
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0))+
  xlim(13,90)+
  coord_trans(y = 'sqrt')

?coord_trans

data(women)

view(pf)

qplot(x = height, data = women) +
  coord_trans(x = "log10")

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
dim(pf)

?geom_jitter
?position_jitter
names(pf)

qplot(age, friendships_initiated, data = pf)

ggplot(aes(x = age, y = friendships_initiated), data = pf)+
  geom_jitter(alpha = 1/20, position =  position_jitter(h = 0))+
  scale_x_continuous(limits = c(13,90), breaks = seq(13,90,10))+
  coord_trans(y = "sqrt")

#Conditional Means

install.packages('dplyr')
library(dplyr)

?dplyr
?summarise()

data(mtcars)

mtcars$disp

summarise(mtcars, mean(disp))
summarise(group_by(mtcars,cyl), mean(disp))

age_groups <- group_by(pf, age)

pf.fc_by_age <-summarise(age_groups, mean_friend_count = mean(friend_count),
          median_friend_count = median(friend_count),
          number_users = n())
head(pf.fc_by_age)

pf.fc_by_age <- pf %>%
  group_by(age)%>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            number_users = n())

head(pf.fc_by_age)

k2 <- ggplot(aes(y= mean_friend_count, x = age),data = subset(pf.fc_by_age, age < 71)+
               geom_line()
  
?geom_line()
?quantile()
?coord_cartesian

#overlayng summaries over raw data

ggplot(aes(x = age, y = friend_count), data = pf)+
  geom_jitter(alpha = 1/20, position = position_jitter(h = 0), color = 'yellow')+
  coord_cartesian(xlim = c(13,70), ylim = c(0,1000))+
  geom_line(stat = 'summary', fun.y = mean)+
  geom_line(stat = 'summary', fun.y = quantile, fun.args = list(probs = .1), 
            linetype =2, color = 'blue')+
  geom_line(stat = 'summary',  fun.y = quantile, fun.args = list(probs = .9), 
            linetype =2, color = 'blue')+
  geom_line(stat = 'summary', fun.y = median, color = 'red')

?cor.test
##correlation

#correlation reflects the noisiness and diraction of a linear relationship
#corelation coefficient = covariance(X,Y)/(Sd(x)*Sd(Y)) (sd = standard deviation)

cor.test(pf$age, pf$friend_count, method = c("pearson"))

with(pf, cor.test(age, friend_count, method = "pearson" ))

with(subset(pf, age <= 70 ), cor.test(age, friend_count, method = "pearson" ))

with(subset(pf, age <= 70 ), cor.test(age, friend_count, method = "spearman" ))
names(pf)
summary(pf$likes_received)
summary(pf$www_likes_received)
##create scatterplots of likes_received vs www_likes_received

ggplot(aes(x = www_likes_received, y = likes_received), data = pf)+
  geom_point()+
  xlim(0,quantile(pf$www_likes_received, 0.95))+
  ylim(0,quantile(pf$likes_received, 0.95))+
  geom_smooth(method = 'lm', color = 'red')
  

quantile(pf$likes_received, probs = 0.95)
quantile(pf$www_likes_received, probs = 0.90)
?xlim
?coord_trans
cor.test(pf$www_likes_received, pf$likes_received, method = 'pearson')
cor.test(pf$www_likes_received, pf$likes_received, method = 'spearman')

#More Caution with Correlation

install.packages('alr3')
library(alr3)

data(Mitchell)
summary(Mitchell)
str(Mitchell)
names(Mitchell)

ggplot(aes(x = Month, y = Temp), data = Mitchell)+geom_point()+
  scale_x_discrete(breaks = seq(0,203,12))+
  coord_cartesian(xlim = c(0, 96))
  
ggplot(aes(x=(Month%%12),y=Temp),data=Mitchell)+ 
  geom_point() 


  
cor.test(Mitchell$Month, Mitchell$Temp)

#Understanding Noise

a1 <- (1 - ((pf$dob_month)/12))
a2 <- pf$age
pf$age_with_months <- a1+a2

pf.fc_byage_months <- pf %>%
  group_by(age_with_months)%>%
  summarise( meanfriend_count = mean(friend_count),
             medianfriend_count = median(friend_count),
             number_people = n())%>%
  arrange(age_with_months)

#noise in conditional means

k1 <-ggplot(aes(x = age_with_months, y = meanfriend_count), 
data = subset(pf.fc_byage_months, age_with_months < 71.000))+
  geom_line()+geom_smooth()

k2 <- ggplot(aes(y= mean_friend_count, x = age),data = subset(pf.fc_by_age, age < 71))+
               geom_line()+geom_smooth()
library(gridExtra)
grid.arrange(k1,k2)
