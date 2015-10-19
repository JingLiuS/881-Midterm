setwd("E:\\MSSP\\881\\midterm\\chsi_dataset")
library(useful)


death0 <- read.csv("MEASURESOFBIRTHANDDEATH.csv", header = TRUE, stringsAsFactors = FALSE)
# remove the columns we don't need
death1 <- death0[, c(3, 5, 85, 91, 97, 109, 121)]

# change the column names to help understand
names(death1) <- c("county name", "state abbreviation", "breast cancer", "colon cancer", 
                   "coronary heart disease", "lung cancer", "stroke")

# let's count the missing values in each columns
sum(death1$`breast cancer` < 0)
sum(death1$`colon cancer` < 0)
sum(death1$`coronary heart disease` <0)
sum(death1$`lung cancer` <0)
sum(death1$stroke <0)

# we don't want to simply remove the missing data 
# because it will cause to lose a lot of information
# we can't change them to zeros, either
# because it will have impact on the clustering result
# thus we decide to change the missing data into mean of meaningful values in their columns

# mean value of breast cancer
bc.m <- mean(death1$`breast cancer`[death1$`breast cancer` >= 0])

# change the missing values in breast cancer
for(i in 1:length(death1$`breast cancer`))
{
  if(death1$`breast cancer`[i] < 0)
  {
    death1$`breast cancer`[i] <- bc.m
  }
}

# mean value of colon cancer
cc.m <- mean(death1$`colon cancer`[death1$`colon cancer` >= 0])

# change the missing values in colon cancer
for(i in 1:length(death1$`colon cancer`))
{
  if(death1$`colon cancer`[i] < 0)
  {
    death1$`colon cancer`[i] <- cc.m
  }
}

# mean value of coronary heart disease
chd.m <- mean(death1$`coronary heart disease`[death1$`coronary heart disease` >= 0])

# change the missing value in coronary heart disease
for(i in 1:length(death1$`coronary heart disease`))
{
  if(death1$`coronary heart disease`[i] < 0)
  {
    death1$`coronary heart disease`[i] <- chd.m
  }
}

# mean value of lung cancer
lc.m <- mean(death1$`lung cancer`[death1$`lung cancer` >= 0])

# change the missing value in lung cancer
for(i in 1:length(death1$`lung cancer`))
{
  if(death1$`lung cancer`[i] < 0)
  {
    death1$`lung cancer`[i] <- lc.m
  }
}

# mean value of stroke
str.m <- mean(death1$stroke[death1$stroke >= 0])

# change the missing value in stroke
for(i in 1:length(death1$stroke))
{
  if(death1$stroke[i] < 0)
  {
    death1$stroke[i] <- str.m
  }
}



# now read another table focusing on risk factors and access to care
risk0 <- read.csv("RISKFACTORSANDACCESSTOCARE.csv", header = TRUE, stringsAsFactors = FALSE)

# remove the columns we don't need
risk1 <- risk0[, c(3, 5, 7, 13, 16, 19, 22)]

# change the column names to help understand
names(risk1) <- c("county name", "state abbreviation", "no exercise", 
                  "obesity", "high blood pressure", "smoker", "diabetes")

# examine the number of missing values in each column
sum(risk1$`no exercise` <0)
sum(risk1$obesity <0)
sum(risk1$`high blood pressure` <0)
sum(risk1$smoker <0)
sum(risk1$diabetes <0)

# mean value of no exercise
exe.m <- mean(risk1$`no exercise`[risk1$`no exercise` >= 0])

# change the missing value in exercise
for(i in 1:length(risk1$`no exercise`))
{
  if(risk1$`no exercise`[i] < 0)
  {
    risk1$`no exercise`[i] <- exe.m
  }
}

# mean value of obesity
obe.m <- mean(risk1$obesity[risk1$obesity >= 0])

# change the missing value in obesity
for(i in 1:length(risk1$obesity))
{
  if(risk1$obesity[i] < 0)
  {
    risk1$obesity[i] <- obe.m
  }
}

# mean value of high blood pressure
hbp.m <- mean(risk1$`high blood pressure`[risk1$`high blood pressure` >= 0])

# change the missing value in high blood pressure
for(i in 1:length(risk1$`high blood pressure`))
{
  if(risk1$`high blood pressure`[i] < 0)
  {
    risk1$`high blood pressure`[i] <- hbp.m
  }
}

# mean value of smoker
smo.m <- mean(risk1$smoker[risk1$smoker >= 0])

# change the missing value in smoker
for(i in 1:length(risk1$smoker))
{
  if(risk1$smoker[i] < 0)
  {
    risk1$smoker[i] <- smo.m
  }
}

# mean value of diabetes
dia.m <- mean(risk1$diabetes[risk1$diabetes >= 0])

# change the missing value in diabetes
for(i in 1:length(risk1$diabetes))
{
  if(risk1$diabetes[i] < 0)
  {
    risk1$diabetes[i] <- dia.m
  }
}



# have a look at how we've done
View(death1)
View(risk1)



# things look good
# now we can do the kmeans clustering
# kmeans clustering can only deal with numeric
# so remember to exclude the fist two columns

set.seed(50)

# first we want to find out the best number of clusters
# we should keep adding clusters until the Hartigan value is less than 10
death.kn <- FitKMeans(death1[, 3:7], max.clusters = 10, nstart = 25)
death.kn

# the result shows that we need more than 10 clusters but that's difficult for interpretation
# thus we choose 4 to be the number of clusters
death.km <- kmeans(x = death1[, 3:7], centers = 4, nstart = 25)
death.km

plot(death.km, data = death1)

# best number of clusters for risk factors
risk.kn <- FitKMeans(risk1[, 3:7], max.clusters = 10, nstart = 25)
risk.kn

# we choose 3 clusters
risk.km <- kmeans(x = risk1[, 3:7], centers = 3)
risk.km

plot(risk.km, data = risk1)



# since outliers will impact the clustering results
# we use boxplot to have a look at the outliers
boxplot(death1[, 3:7])
boxplot(risk1[, 3:7])
