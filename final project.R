### Libraries
library(dplyr)


### Feeding in CSV Data
world <- read.csv("/Users/danbaker/Desktop/MIS500/Final/1st_2nd_3rd_only.csv")
compare_years <- read.csv("/Users/danbaker/Desktop/MIS500/Final/2005_vs_2015_compare.csv")
by_gender <- read.csv("/Users/danbaker/Desktop/MIS500/Final/by_gender.csv")

by_gender

### Summary Statistics
summary(world)
summary(compare_years)
summary(years)

### Summary Statistics by 1st, 2nd and 3rd World Countries
group_by(world,status)%>%
  summarise(
    count = n(),
    mean = mean(rate_per_100000_population,na.rm=TRUE),
    sd = sd(rate_per_100000_population,na.rm=TRUE)
  )

### Boxplots for 1st, 2nd and 3rd World Countries
boxplot(rate_per_100000_population~status,data=world,main="1st, 2nd, and 3rd World Country Homicide Rate",ylab="Homicide Rate Per 100,000 Population",xlab="Country Status")

### ANOVA Test (H0: all 3 are the same, HA: )
fit = lm(formula = rate_per_100000_population~status,data=world)
fit
anova(fit)

### Multiple 1-sided, 2 sample t-test
t.test(rate_per_100000_population~status,data=world, car.equal = TRUE, alternative="two.sided")



### Comparison of  2005 & 2015 Homicide Rates

### Boxplot
boxplot(compare_years$X2005,compare_years$X2015, main="Comparing Homicide Rate Between 2005 and 2015",names=c("2005","2015"),ylab="Homicide Rate Per 100,000 Population")

### Summary Statistics
sd_2005 <- sd(compare_years$X2005)
sd_2015 <- sd(compare_years$X2015)
summary(compare_years)
sd_2005
sd_2015

### T-Test
t.test(compare_years$X2005,compare_years$X2015, var.equal= TRUE, alternative="two.sided", paired=TRUE, conf.level=0.95)

t.test(compare_years$X2005,compare_years$X2015, var.equal= TRUE, alternative="greater", paired=TRUE, conf.level=0.95)

### Comparison Between Men & Women

### Summary Statistics
sd_male <- sd(by_gender$male_rate_2016)
sd_female <- sd(by_gender$female_rate_2016)
summary(by_gender)
sd_male
sd_female

### Boxplot 
boxplot(by_gender$male_rate_2016,by_gender$female_rate_2016, main="Comparing Homicide Rate Between Men and Women",names=c("Male","Female"),ylab="Homicide Rate Per 100,000 Population (in 2016)")

### T-Test
t.test(by_gender$male_rate_2016,by_gender$female_rate_2016, var.equal= FALSE, alternative="greater", conf.level=0.95)


boxplot(worlds)
