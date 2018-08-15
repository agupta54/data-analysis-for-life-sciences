dat <- read.csv("femaleMiceWeights.csv")
head(dat)
View(dat)
library(dplyr)
control <- filter(dat,Diet=="chow") %>%
  select(Bodyweight) %>%
  unlist
treatment <- filter(dat, Diet=="hf") %>%
  select(Bodyweight) %>%
  unlist 
print(mean(treatment))
print(mean(control))
obsdiff <- mean(treatment) - mean(control)
print(obsdiff)
population <- read.csv("femaleControlsPopulation.csv")
class(population)
population <- unlist(population)
control <- sample(population,12)
mean(control)
control <- sample(population,12)
mean(control)
control <- sample(population, 12)
treatment <- sample(population, 12)
print(mean(treatment) - mean(control))
n <- 1000
null <- vector("numeric", n)
for (i in 1:n){
  control <- sample(population, 12)
  treatment <- sample(population, 12)
  null[i] <- mean(treatment) - mean(control)
}
mean(null >= obsdiff)
library(UsingR)
x <- father.son$fheight
round(sample(x, 10), 1)
smallest <- floor( min(x) )
largest <- ceiling( max(x) )
values <- seq(smallest, largest, len=300)
heightecdf <- ecdf(x)
plot(values, heightecdf(values), type="l",
     xlab="a (Height in inches)", ylab="Pr(x <=a)")
hist(x)
bins <- seq(smallest, largest)
hist(x,breaks = bins,xlab="Height (in inches)", main="Adult men heights")
n <- 100
library(rafalib)
nullplot(-5,5,1,30, xlab="Observed differences (grams)", ylab = "Frequency")
totals <- vector("numeric", 11)
for (i in 1:n){
  control <- sample(population,12)
  treatment <- sample(population,12)
  nulldiff <- mean(treatment) - mean(control)
  j <- pmax(pmin(round(nulldiff)+6,11),1)
  totals[j] <- totals[j]+1
  text(j-6,totals[j],pch=15,round(nulldiff,1))
}
hist(null, freq=TRUE)
abline(v=obsdiff, col="red", lwd=2)
1 - pnorm(obsdiff, mean(null), sd(null))
set.seed(1)

### Exercises ### 
x <- unlist(read.csv("femaleControlsPopulation.csv"))
"1."
mean(x)
"2."
set.seed(1)
abs(mean(sample(x,5)) - mean(x))
"3."
set.seed(5)
abs(mean(sample(x,5)) - mean(x))
"4. C"
"5."
set.seed(1)
n <- 1000
null <- vector("numeric", n)
for (i in 1:n) {
  null[i] <- mean(sample(x,5))
}
mean(abs(null - mean(x)) > 1)
"6."
set.seed(1)
n <- 10000
null <- vector("numeric", n)
for (i in 1:n) {
  null[i] <- mean(sample(x,5))
}
mean(abs(null - mean(x)) > 1)
"7."
set.seed(1)
n <- 1000
null <- vector("numeric", n)
for (i in 1:n) {
  null[i] <- mean(sample(x,50))
}
mean(abs(null - mean(x)) > 1)
"8. B"
"9."
mean(null > 23 & null <25)
"10."
pnorm(25, mean=23.9, sd=.43) - pnorm(23, mean=23.9, sd=0.43)
### ### ### ### 

dat <- read.csv("mice_pheno.csv")
library(dplyr)
controlPopulation <- filter(dat, Sex=="F" & Diet=="chow") %>%
  select(Bodyweight) %>% unlist 
length(controlPopulation)
hfPopulation <- filter(dat, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>% unlist 
length(hfPopulation)

### Exercises ### 
dat <- read.csv("mice_pheno.csv")
dat <- na.omit(dat)
"1."
x <- filter(dat, Sex=="M" & Diet=="chow") %>%
  select(Bodyweight) %>% unlist
mean(x)
"2."
library(rafalib)
popsd(x)
"3."
set.seed(1)
X <- sample(x, 25)
mean(X)
"4."
y <- filter(dat, Sex=="M" & Diet=="hf") %>%
  select(Bodyweight) %>% unlist
mean(y)
"5."
popsd(y)
"6."
set.seed(1)
Y <- sample(y, 25)
mean(Y)
"7."
a <- mean(y) - mean(x)
b <- mean(Y) - mean(X) 
abs(a - b)        
"8."
x <- filter(dat, Sex=="F" & Diet=="chow") %>%
  select(Bodyweight) %>% unlist
set.seed(1)
X <- sample(x, 25)
y <- filter(dat, Sex=="F" & Diet=="hf") %>%
  select(Bodyweight) %>% unlist
set.seed(1)
Y <- sample(y, 25)
a <- mean(y) - mean(x)
b <- mean(Y) - mean(X) 
abs(a - b)  
"9. A"
### ### ### ### 

pnorm(-2) + (1 - pnorm(2))
library(dplyr)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist
hfPopulation <- filter(dat, Sex =="F" & Diet == "hf") %>% 
  select(Bodyweight) %>% unlist 
library(rafalib)
mypar(1,2)
hist(hfPopulation)
hist(controlPopulation)
mypar(1,2)
qqnorm(hfPopulation)
qqline(hfPopulation)
qqnorm(controlPopulation)
qqline(controlPopulation)

### Exercises ### 
dat <- na.omit(read.csv("mice_pheno.csv"))
"1."
pnorm(1) - pnorm(-1)
"2."
pnorm(2) - pnorm(-2)
"3."
pnorm(3) - pnorm(-3)
"4."
y <- filter(dat, Sex == "M" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist 
z <- (y - mean(y)) / popsd(y)
mean(abs(z) <= 1 )
"5." 
mean(abs(z) <= 2)
"6."
mean(abs(z) <= 3)
"7."
library(rafalib)
mypar(1,1)
qqnorm(z)
qqline(z)
"8."
mypar(2,2)
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="chow") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="M" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% select(Bodyweight) %>% unlist
z <- ( y - mean(y) ) / popsd(y)
qqnorm(z);abline(0,1)
"9."
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
avgs <- replicate(10000, mean( sample(y, 25)))
mypar(1,2)
hist(avgs)
qqnorm(avgs)
qqline(avgs)
"10."
popsd(avgs)
"11. D"
"12."
y <- filter(dat, Sex=="M" & Diet=="chow") %>% select(Bodyweight) %>% unlist
sds <- replicate(10000, sd( sample(y, 25)))
mypar(1,1)
hist(sds)
mean(sds < 3.5)
"13."
x <- seq(0.0001, 0.9999, len=300)
qt(x, df=3)
qnorm(x)
### ### ### ###

dat <- read.csv("mice_pheno.csv")
head(dat)
controlPopulation <- filter(dat, Sex=="F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist
hfPopulation <- filter(dat, Sex == "F" & Diet == "hf") %>% 
  select(Bodyweight) %>% unlist 
mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
library(rafalib)
sd_hf <- popsd(hfPopulation)
sd_control <- popsd(controlPopulation)
N <- 12
hf <- sample(hfPopulation, 12)
control <- sample(controlPopulation, 12)
Ns <- c(3,12,25,50)
B <- 10000 #number of simulations
res <-  sapply(Ns,function(n) {
  replicate(B,mean(sample(hfPopulation,n))-mean(sample(controlPopulation,n)))
})
mypar(2,2)
for (i in seq(along=Ns)) {
  titleavg <- signif(mean(res[,i]),3)
  titlesd <- signif(popsd(res[,i]),3)
  title <- paste0("N=",Ns[i]," Avg=",titleavg," SD=",titlesd)
  qqnorm(res[,i],main=title)
  qqline(res[,i],col=2)
}
Ns <- c(3,12,25,50)
B <- 10000 
computestat <- function(n){
  y <- sample(hfPopulation,n)
  x <- sample(controlPopulation,n)
  (mean(y)-mean(x))/sqrt(var(y)/n+var(x)/n)
}
res <- sapply(Ns, function(n){
  replicate(B,computestat(n))
})
mypar(2,2)
for (i in seq(along=Ns)){
  qqnorm(res[,i],main=Ns[i])
  qqline(res[,i],col=2)
}
