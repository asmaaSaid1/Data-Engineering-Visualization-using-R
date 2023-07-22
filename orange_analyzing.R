#load the data
data=data.frame(Orange)
data("Orange")
#display the head of data
head(Orange)
#check for null values
is.null(data)
#attach the data
attach(data)
#display the summary of data
summary(data)
#import the libraries
library(ggplot2)
library(hrbrthemes)
library(tidyverse)
library(viridis)

#calclate the correlation between circumference and age of tree
cor(circumference,age)
#scatter plot
plot(circumference, age)
plot(circumference,age,xlab='Circumference',ylab='Age')
abline(model,col="red",lty=2,lwd=3)
ggplot(data, aes(x=circumference, y=age, color=Tree)) + 
  geom_point(size=2) +
    theme_ipsum()

#bar plot of circumference relative 
ggplot(data, aes(x=factor(Tree), y=circumference,fill=Tree, color=Tree)) + 
  geom_bar(stat='identity',position = 'dodge')
#box plot of circumference among different type of tree
data %>%
  ggplot( aes(x=Tree, y=circumference, fill=Tree)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("Basic boxplot") +
  xlab("")
data %>%
  ggplot( aes(x=Tree, y=circumference, fill=Tree)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")

model <- lm(age ~ circumference , data = Orange)
model
summary(model)
x=as.integer(readline(prompt = "Enter orange circumference : "))
#predicting the type of tree and age using the linear regression 'model' created above:
round(predict(model,data.frame("circumference"=x))) 


