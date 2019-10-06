tScore_before <- c(40, 62, 74, 22, 64, 65, 49, 49, 49)
tScore_after <- c(68, 61, 64, 76, 90, 75, 66, 60, 63)
my_data <- data.frame(
  group = rep(c("Score Before", "Score After"), each = 9),
  scores = c(tScore_before, tScore_after)
)
print(my_data)
#Computing Summary Statistics By Group
library(dplyr)
group_by(my_data,group)%>%
  summarise(
    count = n(),
    mean = mean(scores,na.rm=TRUE),
    sd = sd(scores,na.rm = TRUE)
  )
# Computing Unpaired Two Sample t-test
res <- t.test(tScore_before,tScore_after,var.equal = TRUE)
res

# Computing Independent t-test
res <- t.test(scores~group,data = my_data,var.equal=TRUE)
res

# Testing whether the average score before score is different than the average after score
t.test(scores~group, data = my_data,
       var.equal= TRUE,alternative="two.sided", paired=T)


# Testing whether the average score before score is less than the average after score
t.test(scores~group, data = my_data,
       var.equal= TRUE,alternative="less", paired=T)


# Testing whether the average score before score is greater than the average after score
t.test(scores~group, data = my_data,
       var.equal= TRUE,alternative="greater", paired=T)

# Creating a Boxplot to visualize the data
boxplot(tScore_before,tScore_after)

# Computing a 2-sided paired t-test between the two populations w/ 95% confidence
t.test(tScore_before,tScore_after,mu=0,alt="two.sided",paired=T,conf.level=0.95)

# Computing a 1-sided paired t-test between the two populations w/ 95% confidence
t.test(tScore_before,tScore_after,mu=0,alt="greater",paired=T,conf.level=0.95)



