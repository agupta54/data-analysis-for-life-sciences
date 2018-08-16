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

### Exercises ### 
dat <- read.csv("femaleMiceWeights.csv")
"1."
n <- 100
x <- sample(1:6, n, replace = TRUE)
mean(x==6)
set.seed(1)
avgd <- replicate(10000,mean(sample(1:6, n, replace = TRUE) == 6))
p = 1/6
z <- (avgd - p) / (sqrt(p*(1-p)/n))
mean(abs(z) >= 2)
"2."
qqnorm(z)
qqline(z)
mypar(2,2)
n <- 5
x <- sample(1:6, n, replace = TRUE)
set.seed(1)
avgd <- replicate(10000,mean(sample(1:6, n, replace = TRUE) == 6))
p = 0.5
z <- (avgd - p) / (sqrt(p*(1-p)/n))
qqnorm(z)
qqline(z)
n <- 30
x <- sample(1:6, n, replace = TRUE)
set.seed(1)
avgd <- replicate(10000,mean(sample(1:6, n, replace = TRUE) == 6))
p = 0.5
z <- (avgd - p) / (sqrt(p*(1-p)/n))
qqnorm(z)
qqline(z)
n <- 30
x <- sample(1:6, n, replace = TRUE)
set.seed(1)
avgd <- replicate(10000,mean(sample(1:6, n, replace = TRUE) == 6))
p = 0.01
z <- (avgd - p) / (sqrt(p*(1-p)/n))
qqnorm(z)
qqline(z)
n <- 100
x <- sample(1:6, n, replace = TRUE)
set.seed(1)
avgd <- replicate(10000,mean(sample(1:6, n, replace = TRUE) == 6))
p = 0.01
z <- (avgd - p) / (sqrt(p*(1-p)/n))
qqnorm(z)
qqline(z)
"3."
X <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
Y <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
mean(X)
"4. D"
"5. Zero"
"6."
seX <- sd(X)/sqrt(length(X))
"7."
Z <- 5.21 / se
(1-pnorm(Z)) + pnorm(-Z)
"8."
se <- sqrt((sd(Y)*sd(Y)/12) + (sd(X)*sd(X)/12))
"9. 10. A"
"11."
z <- 5.21/se
(1-pnorm(z)) 
"12."
qqnorm(X)
qqnorm(Y)
t.test(X,Y)
"13. C"
### ### ### ### 

dat <- read.csv("femaleMiceWeights.csv")
control <- filter(dat, Diet=="chow") %>% select(Bodyweight) %>% unlist
treatment <- filter(dat, Diet=="hf") %>% select(Bodyweight) %>% unlist
diff <- mean(treatment) - mean(control)
print(diff)
sd(control)/sqrt(length(control))
se <- sqrt(
  var(treatment)/length(treatment) + 
    var(control)/length(control)
)
diff
tstat <- diff/se
righttail <- 1 - pnorm(abs(tstat))
lefttail <- pnorm(-abs(tstat))
pval <- lefttail + righttail
print(pval)
mypar(1,2)
qqnorm(treatment)
qqline(treatment,col=2)
qqnorm(control)
qqline(control,col=2)
t.test(treatment, control)
result <- t.test(treatment,control)
result$p.value
dat <- read.csv("mice_pheno.csv")
control <- filter(dat, Diet=="chow") %>% select(Bodyweight)
treatment <- filter(dat, Diet=="hf") %>% select(Bodyweight)
t.test(treatment,control)
dat <- read.csv("mice_pheno.csv")
chowPopulation <- dat[dat$Sex=="F" & dat$Diet=="chow",3]
mu_chow <- mean(chowPopulation)
print(mu_chow)
N <- 30
chow <- sample(chowPopulation,N)
print(mean(chow))
se <- sd(chow)/sqrt(N)
print(se)
pnorm(2)-pnorm(-2)
Q <- qnorm(1 - 0.05/2)
interval <- c(mean(chow)-Q*se,mean(chow)+Q*se)
interval
interval[1] < mu_chow & interval[2] > mu_chow
B <- 250
mypar()
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
for (i in 1:B){
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se,mean(chow)+Q*se)
  covered <- 
    mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval,c(i,i),col=color)
}
mypar()
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
Q <- qnorm(1- 0.05/2)
N <- 5
for (i in 1:B) {
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}
mypar()
plot(mean(chowPopulation)+c(-7,7),c(1,1),type="n",
     xlab="weight",ylab="interval",ylim=c(1,B))
abline(v=mean(chowPopulation))
Q <- qt(1- 0.05/2, df=4)
N <- 5
for (i in 1:B) {
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}
qnorm(1-0.05/2)
qt(1-0.05/2,df=4)
dat <- read.csv("mice_pheno.csv")
controlPopulation <- filter(dat, Sex == "F" & Diet == "chow") %>%
  select(Bodyweight) %>% unlist
hfPopulation <- filter(dat, Sex == "F" & Diet == "hf") %>%
  select(Bodyweight) %>% unlist 
mu_hf <- mean(hfPopulation)
mu_control <- mean(controlPopulation)
print(mu_hf - mu_control)
print((mu_hf-mu_control)/mu_control * 100)
set.seed(1)
N <- 5
hf <- sample(hfPopulation,N)
control <- sample(controlPopulation,N)
t.test(hf,control)$p.value
N <- 12
alpha <- 0.05
B <- 2000
reject <- function(N, alpha=0.05){
  hf <- sample(hfPopulation,N)
  control <- sample(controlPopulation,N)
  pval <- t.test(hf,control)$p.value
  pval < alpha
}
reject(12)
rejections <- replicate(B, reject(N))
mean(rejections)
Ns <- seq(5, 50, 5)
power <- sapply(Ns,function(N){
  rejections <- replicate(B,reject(N))
  mean(rejections)
})
plot(Ns, power, type="b")
N <- 30
alphas <- c(0.1,0.05,0.01,0.001,0.0001)
power <- sapply(alphas, function(alpha){
  rejections <- replicate(B,reject(N,alpha=alpha))
  mean(rejections)
})
plot(alphas,power,log="x")

### Exercises ### 
babies <- read.table("babies.txt", header = TRUE)
bwt.nonsmoke <- filter(babies,smoke==0) %>% select(bwt) %>% unlist
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist
library(rafalib)
mean(bwt.nonsmoke) - mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)
"1."
set.seed(1)
N <- 25
dat.ns <- sample(bwt.nonsmoke,N) 
dat.s <- sample(bwt.smoke,N)
tval <- t.test(dat.ns,dat.s)
tval
"2."
pval <- 1-(pnorm(abs(tval$statistic))-pnorm(-abs(tval$statistic)))
pval
tval$p.value
"3. D"
"4."
qnorm(0.995)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
"5."
set.seed(1)
qt(0.995,2*N-2)*sqrt( sd( dat.ns)^2/N + sd( dat.s)^2/N )
"6. C"
"7. A"
"8."
N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value
"9. B"
"10."
N=5
set.seed(1)
dat.ns <- sample(bwt.nonsmoke , N) 
dat.s <- sample(bwt.smoke , N) 
t.test(dat.s, dat.ns)$p.value>0.05
N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)
"11."
Ns=c(10,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.05
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ] 
"12."
Ns=c(10,60,90,120)
res <- sapply(Ns, function(N){
  set.seed(1)
  rejects <- replicate(10000,{
    dat.ns <- sample(bwt.nonsmoke , N)
    dat.s <- sample(bwt.smoke , N)
    t.test(dat.s, dat.ns)$p.value < 0.01
  })
  mean(rejects)
})
Ns[ which.min( abs( res - .8) ) ]
