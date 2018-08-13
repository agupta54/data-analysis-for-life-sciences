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
