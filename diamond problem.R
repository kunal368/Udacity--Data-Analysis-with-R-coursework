library(ggplot2)
data(diamonds)
summary(diamonds)
str(diamonds)
?diamonds
dim(diamonds)
levels(diamonds$color)
summary(diamonds$price)
price_histogram <-qplot(data = diamonds, x = price, color = I('black'), fill = ('blue'),
                        ylab ='number of diamonds', xlab = 'price of diamonds',
                        binwidth = 100)+
  scale_x_continuous(limits = c(0, 10000), breaks = seq(0,10000, 1000))
  

ggsave(price_histogram.png)

pricebycut <- qplot(data = diamonds, x = price, color = I('black'), fill = ('blue'),
                        ylab ='number of diamonds', xlab = 'price of diamonds',
                        binwidth = 50)+
  scale_x_continuous(limits = c(300, 2000), breaks = seq(300,2000, 100))+
  facet_wrap(~cut, ncol = 2)

by(diamonds$price, diamonds$cut, min)
summary(diamonds$price)

  
cheap_diamond <- diamonds$price < 500
summary(cheap_diamond)

verycheap_diamond <- diamonds$price < 250
summary(verycheap_diamond)

costly_diamond <- diamonds$price >= 15000

summary(diamonds$cut)

qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")

qplot(x = price, data = diamonds, binwidth = 0.03) + facet_wrap(~cut, scales = "free_y")+
  scale_x_log10()
str(diamonds)

qplot(x = color, y =price , data = diamonds, geom = 'boxplot', fill = color)

qplot(x= clarity, y = price, data = diamonds, geom ='boxplot', fill = clarity)

by(diamonds$price, diamonds$color, summary)
 

