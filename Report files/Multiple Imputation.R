#///////////////////////////////////////////////////////////////////////////////
#---- Missing data and Multiple Imputation
#///////////////////////////////////////////////////////////////////////////////
library(mice)
library(dplyr)


# Perform training test split
setwd("~/Desktop/PHP 2550 Pratical Data Analysis/Project 2")
smoke <- read.csv("~/Desktop/PHP 2550 Pratical Data Analysis/Project 2/project2.csv")

# Remove id
smoke <- subset(smoke, select = c(-id))

# Factor categorical data
col_names <- names(smoke)[-c(4,11,13:18,22,24)]
smoke[,col_names] <- lapply(smoke[,col_names] , factor)

# Income
smoke$inc <- ordered(smoke$inc, levels = 1:5, 
                     labels = c("Less than $20,000", "$20,000-$35,000", "$35,001–50,000", 
                                "$50,001–75,000", "More than $75,000"))
# Education
smoke$edu <- ordered(smoke$edu, levels = 1:5,
                     labels = c("Grade school", "Some high school", "High school graduate or GED",
                                "Some college/technical school", "College graduate"))


set.seed(2550)
test_ind <- sample(c(TRUE, FALSE), nrow(smoke), replace=TRUE, prob=c(0.70, 0.30))
df_train <- smoke[test_ind,]
df_test <- smoke[!test_ind,]

# MI on train data
train_mice_out <- mice(df_train, 5, print=F, seed=2550)

# MI on test data
test_mice_out <- mice(df_test, 5, print=F, seed=2550)

# Store each imputed data set
imp_train <- vector("list",5)    
for (i in 1:5){
  imp_train[[i]] <- mice::complete(train_mice_out,i) 
}

imp_test <- vector("list",5)    
for (i in 1:5){
  imp_test[[i]] <- mice::complete(test_mice_out,i) 
}

save(imp_train, file = "smoke_train.RData")
save(imp_test, file = "smoke_test.RData")

# Use long data for predicted probabilities
imp_train_long <- complete(train_mice_out,action="long")
imp_test_long <- complete(test_mice_out,action="long")

save(imp_train_long, file = "imp_train_long.RData")
save(imp_test_long, file = "imp_test_long.RData")
