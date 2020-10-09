library(tidyverse)
library(ggplot2)
getwd()
setwd("/Users/joshhoward/DCS 520")
head(iris)
iris%>%ggplot(aes(x = Sepal.Length))+
  geom_histogram(bins=15, fill='red', alpha=0.3)
iris%>%ggplot(aes(x = Sepal.Length))+
  geom_histogram(bins=15, fill='red', alpha=0.3)+
  facet_wrap(~Species)
(m_S.L <- mean(iris$Sepal.Length))
iris%>%select(Sepal.Length)%>%
  mutate(Deviation = Sepal.Length-m_S.L)%>%summarise(sum(Deviation))
#Calculate Variance of the sample
var(iris$Sepal.Length)
install.packages('sjstats')
library(sjstats)
#To calculate Population Variance 
var_pop(iris$Sepal.Length)
#Sample Standard Dev
sd(iris$Sepal.Length)
#Population Standard Dev
sd_pop(iris$Sepal.Length)
#Median Absolute Deviation from the Median (MAD)
mad(iris$Sepal.Length)
#IQR shows the length of the middle 50% of data
IQR(iris$Sepal.Length)
#Five number summary used for Box and Whisker plot
fivenum(iris$Sepal.Length)
boxplot(iris$Sepal.Length, horizontal = TRUE)
iris%>%ggplot()+
  geom_boxplot(aes(x = Sepal.Length))
iris%>%ggplot()+
  geom_boxplot(aes(x = Sepal.Length, y = Species))
state <- read_csv('state.csv', col_names=TRUE)
boxplot(state$Population, horizontal = TRUE)
state%>%ggplot()+
  geom_boxplot(aes(x=Population))
#Creating a frequency table for histogram
breaks <- seq(from=min(state$Population), to=max(state$Population), length=11)
pop_freq <- cut(state$Population, breaks=breaks, right=TRUE, include.lowest=TRUE)
table(pop_freq)
hist(state$Population, breaks=breaks)
state%>%ggplot(aes(x=Population))+
  geom_histogram(fill='red', alpha=0.3, breaks=breaks)
state%>%ggplot(aes(x=Population))+geom_histogram(aes(y=..density..), fill='red', alpha=0.3, bins=15)+
  geom_density(color='blue', size=2)
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut))
tmp <- diamonds%>%group_by(cut)%>%summarise(n=n())
#DONT USE PIE CHARTS IF AT ALL POSSIBLE
pie(tmp$n)
ggplot(data=diamonds)+
  geom_bar(mapping=aes(x=cut, fill=cut))+coord_polar()
