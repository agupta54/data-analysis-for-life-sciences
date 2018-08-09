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

