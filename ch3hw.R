#ch3hw
#Ahmar Gordon

## Part 1

### Problem 3
#### (a)

#Read in the data set
FlightDelays <- read.csv(file = url("http://www1.appstate.edu/~thomleyje/data-files/FlightDelays.csv"))

ua = subset(FlightDelays,Carrier=="UA")
aa = subset(FlightDelays,Carrier=="AA")
obs.diff = mean(ua$Delay) - mean(aa$Delay)

data = subset(FlightDelays, select=Delay,drop=T)

N <- 10^5-1         
result <- numeric(N)
for(i in 1:N)       
{
  index <- sample(4029, size=1123, replace = FALSE)     
  result[i] <- mean(data[index]) - mean(data[-index])
}

# Compute both the one-sided p-values based on the test statistic.
pvalue.upper<-(sum(result>=obs.diff)+1)/(N+1)
pvalue.lower<-(sum(result<=obs.diff)+1)/(N+1)

# Compute the two-sided p-value using the smaller one-sided value.
pvalue.two<-format(2*min(pvalue.upper,pvalue.lower),scientific=F)
print(paste0("The two-sided p-value for UA mean =/= AA mean is ", pvalue.two),quote=F)

#### (b)
may = subset(FlightDelays,Month=="May")
june = subset(FlightDelays,Month=="May")
obs.diff = mean(may$Delay) - mean(june$Delay)

data = subset(FlightDelays, select=Delay,drop=T)

N <- 10^5-1         
result <- numeric(N)
for(i in 1:N)       
{
  index <- sample(2100, size=1999, replace = FALSE)      
  result[i] <- mean(data[index]) - mean(data[-index])
}

# Compute both the one-sided p-values based on the test statistic.
pvalue.upper<-(sum(result>=obs.diff)+1)/(N+1)
pvalue.lower<-(sum(result<=obs.diff)+1)/(N+1)

# Compute the two-sided p-value using the smaller one-sided value.
pvalue.two<-format(2*min(pvalue.upper,pvalue.lower),scientific=F)
print(paste0("The two-sided p-value for May Delay mean =/= June Delay mean is ", pvalue.two),quote=F)


### Problem 4
#### (a)

ua = subset(FlightDelays,Carrier=="UA")
aa = subset(FlightDelays,Carrier=="AA")
obs.diff = mean(ua$Delay > 20) - mean(aa$Delay > 20)

data = subset(FlightDelays, select=Delay,drop=T)

N <- 10^5-1         
result <- numeric(N)
for(i in 1:N)       
{
  index <- sample(4029, size=1123, replace = FALSE)      
  result[i] <- mean(data[index] > 20) - mean(data[-index] > 20)
}
# Compute both the one-sided p-values based on the test statistic.
pvalue.upper<-(sum(result>=obs.diff)+1)/(N+1)
pvalue.lower<-(sum(result<=obs.diff)+1)/(N+1)

# Compute the two-sided p-value using the smaller one-sided value.
pvalue.two<-format(2*min(pvalue.upper,pvalue.lower),scientific=F)
print(paste0("The two-sided p-value for UA Delay mean =/= AA Delay mean is ", pvalue.two),quote=F)

####################################################
### Promblem 11

#Read in the data set
Marijuana <- read.csv(file = url("http://www1.appstate.edu/~thomleyje/data-files/Marijuana.csv"))
# Create a function to compute the chi-squared test statistic
# Observed is the observed data; the variables are defined later
chisq<-function(Observed)
{ 
  Expected <- outer(rowSums(Observed),colSums(Observed))/sum(Observed)
  sum((Observed-Expected)^2/Expected)
}
age<-Marijuana$Age
res<-Marijuana$Response
Observed<-table(age,res)
test.stat<-round(chisq(Observed),4)
Expected <- outer(rowSums(Observed),colSums(Observed))/sum(Observed)
pvalue = pchisq(test.stat,df=2,lower.tail=F)
print(paste0("The chi-square test statistic is ", test.stat),quote=F)
print(paste0("The p-value for the chi-square test of independence is ",pvalue),quote=F)
round(prop.table(table(Marijuana),1)*100,2)

####################################################
### Problem 12
#### (a)

Cereals<- read.csv(file = url("http://www1.appstate.edu/~thomleyje/data-files/Cereals.csv"))
age = Cereals$Age
shelf = Cereals$Shelf
table(age,shelf)

#Gives warning message
chisq.test(table(Cereals))

#### (c)
chisq<-function(Observed)
{ 
  Expected <- outer(rowSums(Observed),colSums(Observed))/sum(Observed)
  sum((Observed-Expected)^2/Expected)
}
Expected<-outer(rowSums(Observed),colSums(Observed))/sum(Observed)
Expected

#### (d)
#Resampling permutation test loop and graph of results
N <- 10^4-1
result<-numeric(N)
for (i in 1:N)
{
  ST.perm <-sample(shelf)
  RND.table <- table(age, ST.perm)
  result[i]<-chisq(RND.table)
}
hist(result,xlab="chi squared statistic",
     main="Distribution of Chi Squared Statistic")

FishRays<- read.csv(file = url("http://www1.appstate.edu/~thomleyje/data-files/FishRays.csv"))
# Create a function to compute the chi-squared test statistic
# Observed is the observed data; the variables are defined later
chisq<-function(Observed)
{ 
  Expected <- outer(rowSums(Observed),colSums(Observed))/sum(Observed)
  sum((Observed-Expected)^2/Expected)
}
hab<-FishRays$Habitat
rays<-FishRays$Count
Observed<-table(hab,rays)
test.stat<-round(chisq(Observed),4)
Expected<-outer(rowSums(Observed),colSums(Observed))/sum(Observed)
pvalue = pchisq(test.stat,df=10,lower.tail=F)
print(paste0("The chi-square test statistic is ", test.stat),quote=F)
print(paste0("The p-value for the chi-square test of independence is ",pvalue),quote=F)
round(prop.table(table(FishRays),1)*100,2)
