setwd("/Users/joshhoward/DCS 520")
getwd()
library(tidyverse)
cov(iris$Sepal.Length[iris$Species=='setosa'], iris$Sepal.Width[iris$Species=='setosa'])
cor(iris$Sepal.Length[iris$Species=='setosa'], iris$Sepal.Width[iris$Species=='setosa'])
library(GGally)    
iris%>%ggplot(aes(x = Sepal.Length, y=Sepal.Width, color=Species))+
  geom_point()+
  geom_smooth(method='lm', se=FALSE)
cor(iris$Sepal.Length, iris$Sepal.Width)
mtcars%>%ggpairs()
library(ggcorrplot)
install.packages('ggcorrplot')
library(ggcorrplot)
corr_tmp <- cor(mtcars)
#hc.order = TRUE makes it much easier to read
corr_tmp%>%ggcorrplot(method='circle', hc.order=TRUE
                      type='lower', outline.col='white')
corr_tmp
kc_tax <- read_csv("kc_tax (1).csv")
kable(kc_tax[1:5,])
nrow(kc_tax)
ncol(kc_tax)
kc_tax0 <- subset(kc_tax, TaxAssessedValue<750000
                  &SqFtTotLiving>100 & SqFtTotLiving<3500)
nrow(kc_tax0)
library(hexbin)
ggplot(kc_tax0, aes(SqFtTotLiving, TaxAssessedValue))+
  theme_bw()+
  geom_point(alpha=0.1)+
  geom_density2d(color='white')+
  labs(x="Finished Square Feet", y="Tax Assessed Value")
ggplot(kc_tax0, (aes(x = SqFtTotLiving, y=TaxAssessedValue)))+
  stat_binhex(color='white')+theme_bw()+
  scale_fill_gradient(low='white', high='black')+
  labs(x='Finished Square Feet, y=Tax Assessed Value')
lc_loans <- read_csv("lc_loans.csv")       
kable(lc_loans[1:5,])
as_tibble(lc_loans)
library(descr)
install.packages('descr')
library(descr)
head(lc_loans)
unique(lc_loans$status)
unique(lc_loans$grade)
#Contingency Table
CrossTable(lc_loans$grade, lc_loans$status,
           prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE)

diamonds%>%ggplot(aes(x=cut, y=price))+geom_boxplot()
diamonds%>%ggplot(aes(x=cut, y=price))+
  geom_violin()

ggplot(subset(kc_tax0, ZipCode %in% c(98188, 98105, 98108, 98126)),
       aes(x=SqFtTotLiving, y=TaxAssessedValue))+
  stat_binhex(color='white')+
  theme_bw()+
  scale_fill_gradient(low='white', high='blue')+
  labs(x='Finished Square Feet', y='Tax Assessed Value')+
  facet_wrap('ZipCode')

library(nycflights13)
(dist_data <- flights%>%select(distance)%>%filter(!is.na(distance)))

# We are taking 1000 samples of size 5 out of the total 336,776
samp_mean_05 <- rep(0,1000)
for(i in 1:1000){samp_mean_05[i] <- mean(sample(dist_data$distance,5))}
q <- tibble(samp_mean_05)
ggplot(data=q)+geom_histogram(mapping=aes(x=samp_mean_05), bins=15)

# We are taking 1000 samples of size 25 out of the total 336,776
samp_mean_05 <- rep(0,1000)
for(i in 1:1000){samp_mean_05[i] <- mean(sample(dist_data$distance,25))}
q <- tibble(samp_mean_05)
ggplot(data=q)+geom_histogram(mapping=aes(x=samp_mean_05), bins=15)

# We are taking 1000 samples of size 64 out of the total 336,776
samp_mean_05 <- rep(0,1000)
for(i in 1:1000){samp_mean_05[i] <- mean(sample(dist_data$distance,64))}
q <- tibble(samp_mean_05)
ggplot(data=q)+geom_histogram(mapping=aes(x=samp_mean_05), bins=15)

single_sample_64 <- sample(dist_data$distance, 64)
(SE <- sd(single_sample_64)/sqrt(length(single_sample_64)))

binom_sample <- rbinom(100, 2500, 1/6)
head(binom_sample)
min(binom_sample)
?rbinom
qqnorm(binom_sample)
  
)?geom_bar
(g <- lc_loans%>%group_by(grade, status)%>%count())
g+ geom_bar()
ggplot(data=lc_loans)+geom_bar(mapping=aes(x = status, y=grade), stat = 'identity', count=n())
