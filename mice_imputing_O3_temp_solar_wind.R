##### Imputing Missing Data with MICE #####

# load libraries
library(tidyverse)
library(mice)
library(VIM)

# get data from the airquality dataset 
# for the purposes of an example, remove some datapoints from the dataset
data <- airquality
tb <- data
tb[4:10, 3] <- rep(NA, 7)
tb[1:5, 4] <- NA

##### replacing categorical variables is usually not advisable - some common practice include replacing missing 
##### categorical variables with the mode of the observed ones - however, it is questionable whether it is a good choice - 
##### even though in this case no datapoints are missing from the categorical variables, we remove them from our 
##### dataset (we can add them back later if needed) and take a look at the data using summary()
tb <- tb[-c(5,6)]
summary(tb)

# asses amount and kind of missing data - MCAR or MNAR
# if missing data for a certain feature or sample is more than 5% then you probably should leave that feature or sample out
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(tb,2,pMiss)
apply(tb,1,pMiss)

# mice  provides a nice function md.pattern() to get a better understanding of the pattern of missing data
md.pattern(tb)
##### 104 samples are complete, 34 samples miss only the Ozone measurement, 
##### 4 samples miss only the Solar.R value and so on...

# another way to look using the VIM package
aggr_plot <- aggr(tb, col = c('navyblue', 'red'), numbers = TRUE, sortVars = TRUE, labels = names(data), 
                  cex.axis = 0.7, gap = 3, ylab = c("Histogram of missing data", "Pattern"))

# aother helpful visual approach is a special box plot
marginplot(tb[c(1, 2)])
##### If our assumption of MCAR data is correct, then we expect the red and blue box plots to be very similar

# impute the missing data
tempData <- mice(tb, m = 5, maxit = 50, meth = 'pmm', seed = 500)
summary(tempData)
#####  m = 5 refers to the number of imputed datasets, five is the default value
##### meth = 'pmm' refers to the imputation method - in this case we are using predictive mean matching as imputation method

# to check the imputed data, for instance for the variable Ozone
tempData$imp$Ozone
# to check the imputation method used for each variable
tempData$meth

# get back the completed dataset using the complete() function
completedData <- mice::complete(tempData, 1)
##### the missing values have been replaced with the imputed values in the first of the five datasets - if you wish 
##### to use another one, just change the second parameter in the complete() function

# inspecting the distribution of original and imputed data

# use a scatterplot and plot Ozone against all the other variables
xyplot(tempData,Ozone ~ Wind + Temp + Solar.R, pch = 18, cex = 1)
##### we would like to see is that the shape of the magenta points (imputed) matches the shape of the blue ones (observed) 
##### the matching shape tells us that the imputed values are indeed “plausible values”

# density plot
densityplot(tempData)
##### density of the imputed data for each imputed dataset is showed in magenta while the density of the observed data 
##### is showed in blue - under our previous assumptions we expect the distributions to be similar

# stripplot() function that shows the distributions of the variables as individual points
stripplot(tempData, pch = 20, cex = 1.2)

# Pooling - suppose that the next step in our analysis is to fit a linear model to the data - you may ask what 
# imputed dataset to choose - the mice package can fit a model to each of the imputed datasets and then pool the 
# results together
modelFit1 <- with(tempData, lm(Temp ~ Ozone + Solar.R + Wind))
summary(pool(modelFit1))
##### modelFit1 containts the results of the fitting performed over the imputed datasets, while the pool() function 
##### pools them all together - apparently, only the Ozone variable is statistically significant - 
##### there are other columns aside from those typical of the lm() model: fmi contains the fraction of missing 
##### information while lambda is the proportion of total variance that is attributable to the missing data

# we initialized the mice function with a specific seed, therefore the results are somewhat dependent on our initial 
# choice - to reduce this effect, we can impute a higher number of dataset, by changing the default m=5 parameter in 
# the mice() function as follows
tempData2 <- mice(data, m = 50, seed = 245435)
modelFit2 <- with(tempData2, lm(Temp ~ Ozone + Solar.R + Wind))
summary(pool(modelFit2))
##### having taken into account the random seed initialization, we obtain (in this case) more or less the same results 
##### as before with only Ozone showing statistical significance


