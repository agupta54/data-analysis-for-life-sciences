dat <- read.csv("femaleMiceWeights.csv")

### Exercises ### 
"1."
dat$Bodyweight
"2."
dat[12,2]
"3."
dat$Bodyweight[11]
"4."
length(dat$Bodyweight)
"5."
View(dat)
"6."
?sample
set.seed(1)
dat$Bodyweight[sample(13:24,1)]
###         ###       ###

dat <- read.csv("femaleMiceWeights.csv")
head(dat)
library(dplyr)
chow <- filter(dat, Diet=="chow")
head(chow)
chowVals <- select(chow, Bodyweight)
head(chowVals)
chowVals <- filter(dat, Diet=="chow") %>% select(Bodyweight)
class(dat)
class(chowVals)
chowVals <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
class(chowVals)

### Exercises ### 
"1."
dat <- read.csv("msleep.csv")
"2."
nrow(filter(dat, order=="Primates"))
"3."
class(filter(dat, order=="Primates"))
"4."
class(filter(dat, order=="Primates") %>% select(sleep_total))
"5."
mean(filter(dat, order=="Primates") %>% select(sleep_total) %>% unlist)
"6."
?summarise
dat %>% group_by(order) %>% summarise(mean = mean(sleep_total), n=n())
### ### ### ### 

x <- 1:5 
n <- 1000
x <- 1:n
S <- sum(x)
onethird <- function(n) sum( 3/10^c(1:n))
1/3 - onethird(4)
1/3 - onethird(10)
1/3 - onethird(16)
