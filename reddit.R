reddit <- read.csv('reddit.csv')
str(reddit)

table(reddit$education)
summary(reddit)

install.packages('ggplot2', dependencies = T) 
library(ggplot2)

levels(reddit$income.range)

qplot(data = reddit, x = income.range)

income.range_order <- ordered(reddit$income.range, levels = c("Under $20,000", "$20,000 - $29,999","$30,000 - $39,999","$40,000 - $49,999","$50,000 - $69,999","$70,000 - $99,999","$100,000 - $149,999","$150,000 or more"))

levels(income.range_order)

qplot(data = reddit, x = income.range_order)
