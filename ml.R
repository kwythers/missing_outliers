##### MICE example #####
require(tidyverse)
require(mice)

dataset <- tibble(var1=rnorm(20,0,1), var2=rnorm(20,5,1))
dataset[c(2,5,7,10),1] <- NA
dataset[c(4,8,19),2] <- NA
summary(dataset)

dataset2 <- mice(dataset)
dataset2<-complete(dataset2)
summary(dataset2)


##### RPART example #####
require(rpart)
require(party)

rpart_tree <- rpart(formula = Species ~ ., data=iris, method = 'class')
summary(rpart_tree)
plot(rpart_tree)

party_tree <- ctree(formula=Species~. , data = iris)
plot(party_tree)
