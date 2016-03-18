getwd()
setwd('C:/Users/CMD JMRC/Downloads/R files')

pf <- read.csv('pseudo_facebook.tsv', sep = '\t')
dim(pf)
names(pf)
install.packages('ggplot2')
library(ggplot2)

ggplot(data = pf, aes(x = dob_day))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(breaks = 1:31)+
  facet_wrap(~dob_month, ncol = 3)

names(pf)

qplot(data= subset(pf,!is.na(gender)), x= friend_count, binwidth = 25)+
  scale_x_continuous(limits = c(0,1000), breaks = seq(0,1000,50))+
  facet_grid(. ~gender)
table(pf$gender)
by(pf$friend_count, pf$gender, summary)
names(pf)

qplot(data = pf, x = (tenure/365), binwidth = 0.25, 
      xlab = 'Number of years using fb',
      ylab = 'Number of users in sample',
      color = I('black'), fill = I('#F79420'))+
  scale_x_continuous(breaks = seq(0,7,1), limits = c(0,7))
  
names(pf)

qplot(data = pf, x = age, binwidth = 1,
      xlab = 'age of fb user',
      ylab = 'number of users in sample',
      color = I('black'), fill = I('#5760AB'))+
  scale_x_continuous(limits = c(0,113), breaks = seq(0,113,10))

summary(pf$age)

install.packages('gridExtra') 
library(gridExtra) 
 
names(pf)

p1 <- qplot(data = pf, x= friend_count)
  

p2 <- qplot(data = pf, x= (friend_count+1))+
  scale_x_log10()
summary(log10(pf$friend_count+1))

p3 <- qplot(data = pf, x= (friend_count))+
  scale_x_sqrt()

grid.arrange(p1, p2, p3)

summary(pf$www_likes)

qplot(data = subset(pf,!is.na(gender)), x = www_likes,y = ..count../sum(..count..),
      xlab = 'fb likes on world wide web',
      ylab = 'proportion of users',
      geom = 'freqpoly',color = gender)+
  scale_x_continuous()+
  scale_x_log10()

summary(pf$www_likes)

summary(pf$gender)

km <- data.frame(sumpf$www_likes & pf$ gender == 'male')
                 
by(pf$www_likes, pf$gender, sum)

qplot(x = gender, y = (friend_count) , data = subset(pf,!is.na(gender)), 
    geom = 'boxplot')+
  scale_y_continuous(limits = c(0,1000))

qplot(x = gender, y = (friend_count) , data = subset(pf,!is.na(gender)), 
      geom = 'boxplot')+
  coord_cartesian(ylim = c(0,1000))

by(pf$friend_count, pf$gender, summary)
  
by(pf$friendships_initiated, pf$gender, summary)

by(pf$friendships_initiated, pf$gender, sum)

names(pf)

summary(pf$mobile_likes)
summary(pf$mobile_likes > 0)

mobile_check_in <- NA
pf$mobile_check_in <- ifelse(pf$mobile_likes > 0, 1, 0)
pf$mobile_check_in <- factor(pf$mobile_check_in)

summary(pf$mobile_check_in)
length(pf$mobile_check_in)

sum(pf$mobile_check_in == 1)/length(pf$mobile_check_in)