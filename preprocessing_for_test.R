## ---- include=FALSE-------------------------------------------------------------------------------
library(tidyverse)
benchmark_read_df <- read_csv('performance/benchmark_df.csv')
options("digits" = 6)


## ----firstbenchmarkdf, echo=FALSE-----------------------------------------------------------------
knitr::kable(
  head(benchmark_read_df), booktabs = TRUE,
  caption = 'benchmark on the validation set comparing all the models that we tried'
)


## ----echo=FALSE-----------------------------------------------------------------------------------
echo_flag <- FALSE
results_option <- 'hide'


## ---- results=results_option----------------------------------------------------------------------
library(tidyverse)


## ---- results=results_option----------------------------------------------------------------------
library(nycflights13)
library(Hmisc)
set.seed(42)
original_data <- read_csv("fltrain.csv.gz")
DF <- original_data


## ----table1, echo=echo_flag-----------------------------------------------------------------------
knitr::kable(
  head(original_data), booktabs = TRUE,
  caption = 'A table of the first few rows of the nycflights13 data.'
)


## ---- results=results_option----------------------------------------------------------------------
DF[sapply(DF, is.character)] <- lapply(DF[sapply(DF, is.character)], 
                                       as.factor)
DF$flight <- as.factor(DF$flight)


## ---- results=results_option----------------------------------------------------------------------
library(lubridate)
DF$sched_arr_time_posix <- as.POSIXct(str_pad(as.character(DF$sched_arr_time), 4, pad="0"),format="%H%M")
DF$sched_arr_time_hour <- hour(DF$sched_arr_time_posix)
DF$sched_arr_time_minute <- minute(DF$sched_arr_time_posix)

#num minute is number of minutes since start of day for scheduled arrival time
DF$sched_arr_time_num_minute <- 60*DF$sched_arr_time_hour + DF$sched_arr_time_minute

DF$sched_dep_time_posix <- as.POSIXct(str_pad(as.character(DF$sched_dep_time),4 , pad="0"),format="%H%M")
DF$sched_dep_time_hour <- hour(DF$sched_dep_time_posix)
DF$sched_dep_time_minute <- minute(DF$sched_dep_time_posix)
#num minute is number of minutes since start of day for scheduled depival time
DF$sched_dep_time_num_minute <- 60*DF$sched_dep_time_hour + DF$sched_dep_time_minute

## ---- results=results_option----------------------------------------------------------------------
select(original_data, time_hour, sched_dep_time, sched_arr_time, tz, tzone)
select(DF, sched_arr_time, sched_arr_time_hour)


## ---- results=results_option----------------------------------------------------------------------
DF$sched_air_time <- DF$sched_arr_time_posix - DF$sched_dep_time_posix
drops <- c('sched_arr_time_posix', 'sched_arr_time_hour', 'sched_dep_time_posix', 'sched_dep_time_hour', 'sched_dep_time', 'sched_arr_time', 'hour', 'time', 'minute', 'time_hour' )
DF <- DF[ , !(names(DF) %in% drops)]


## ---- results=results_option----------------------------------------------------------------------
drops <- c("dep_time", "arr_time", "air_time", "arr_delay", "year.x", 'tailnum')
DF <- DF[ , !(names(DF) %in% drops)]


## ---- results=results_option----------------------------------------------------------------------
## Remove columns with more than 50% NA
DF <- DF[, -which(colMeans(is.na(DF)) > 0.5)]


## ---- results=results_option----------------------------------------------------------------------
DF$sched_air_time <- as.numeric(DF$sched_air_time)
library(imputeMissings)
impute_model <- imputeMissings::compute(DF, method="median/mode")
DF <- impute(DF, object=impute_model, flag=TRUE)
DF <- DF[!duplicated(as.list(DF))]  #remove all redundant flag columns that are identical to each other. 


## ---- results=results_option----------------------------------------------------------------------
numeric_only_df <- dplyr::select_if(DF, is.numeric)
library(corrplot)


## ----corrplot, fig.cap = "grid depicting correlation amongst all numerical variables", echo=echo_flag----
corrplot(cor(numeric_only_df), type = 'lower')


## ---- results=results_option----------------------------------------------------------------------
dep_delay_vec <- DF$dep_delay
DF$dep_delay <- NULL
head(DF)

library(dplyr)
DF <- DF %>% mutate_if(is.numeric, scale)
head(DF)
DF$dep_delay <- dep_delay_vec


## ---- results=results_option----------------------------------------------------------------------
numeric_DF <- dplyr::select_if(DF, is.numeric) %>%  scale()


## ---- results=results_option----------------------------------------------------------------------
prcomp_res <- prcomp(numeric_DF)
sdev <- prcomp_res$sdev
sdev


## ---- results=results_option----------------------------------------------------------------------
pve <- colSums(prcomp_res$x^2)/sum(numeric_DF^2)


## ----pve, echo=echo_flag--------------------------------------------------------------------------
knitr::kable(
  head(pve), booktabs = TRUE,
  caption = 'proportion of variance explained by each principal component'
)


## ---- results=results_option----------------------------------------------------------------------
rotation <- as.data.frame(prcomp_res$rotation)
rotation[order(-abs(rotation$PC1)),]

## ---- results=results_option----------------------------------------------------------------------
pca_rotation <- head(rotation[order(-abs(rotation$PC1)),])


## ----pcarotation, echo=echo_flag------------------------------------------------------------------
knitr::kable(
  head(pca_rotation), booktabs = TRUE,
  caption = 'coefficients for each variable on each principal component'
)


## ---- results=results_option----------------------------------------------------------------------
DF<-DF[DF$dep_delay < 30,]


## ---- results=results_option----------------------------------------------------------------------
set.seed(42)
DF$flight <- NULL
train_index <- sample(1:nrow(DF),size=2*nrow(DF)/3,replace=FALSE)
train_df <- DF[train_index,]
test_df <- DF[-train_index,]


## -------------------------------------------------------------------------------------------------
train_and_validation_df <- DF


## ---- results=results_option----------------------------------------------------------------------
benchmark_df <- data.frame(model_description = character(), rmse = numeric(), stringsAsFactors = FALSE)
rmse = mean((test_df$dep_delay-0)^2) %>% sqrt()
model_description = "predicting 0"
benchmark_df <- rbind(benchmark_df, data.frame(model_description = model_description, rmse=rmse))


## ---- results=results_option----------------------------------------------------------------------
rmse = mean((test_df$dep_delay-mean(train_df$dep_delay))^2)%>% sqrt()
rmse
model_description <- 'predicting the mean'
benchmark_df <- rbind(benchmark_df, data.frame(model_description = model_description, rmse=rmse))
benchmark_df


## ---- results=results_option----------------------------------------------------------------------
rmse = mean((test_df$dep_delay-median(train_df$dep_delay))^2)%>% sqrt()
rmse
model_description <- 'predicting the median'
benchmark_df <- rbind(benchmark_df, data.frame(model_description = model_description, rmse=rmse))


## ---- results=results_option----------------------------------------------------------------------
model <- lm(dep_delay ~ ., data=train_df)
model_without_dest <-  lm(dep_delay ~ .-dest, data=train_df)
anova(model, model_without_dest)
summary <- round(summary(model)$coefficients,6)
sorteddf <- summary[order(summary[,ncol(summary)]),]
head(sorteddf)


## ---- results=results_option----------------------------------------------------------------------
lm_test_df <- test_df

in_test_but_not_train <- setdiff(unique(lm_test_df$model), unique(train_df$model))
lm_test_df <- lm_test_df[ !lm_test_df$model %in% in_test_but_not_train, ]

in_test_but_not_train <- setdiff(unique(lm_test_df$dest), unique(train_df$dest))
lm_test_df <- lm_test_df[ !lm_test_df$dest %in% in_test_but_not_train, ]

preds = predict(model, newdata=lm_test_df)
rmse = sqrt(mean((lm_test_df$dep_delay - preds)^2))
rmse
model_description <- 'linear regression'
benchmark_df <- rbind(benchmark_df, data.frame(model_description = model_description, rmse=rmse))


## ---- results=results_option----------------------------------------------------------------------
library(gbm)

train_gbm <- function(filename){
num_trees <- 2^13
set.seed(42)
model <- gbm(dep_delay ~ ., data=train_df,
              n.trees=num_trees, shrinkage=0.01) # default shrinkage = 0.1
preds = predict(model, newdata=test_df, n.trees=num_trees)
rmse = sqrt(mean((test_df$dep_delay - preds)^2))
summary(model)
saveRDS(model, filename)
return(model)
}

destfile <- "models2/gbm_shrinkage_0point01_ntrees_8192_v2.rds"
if (!file.exists(destfile)) {
   train_gbm(destfile)
 }
model <- readRDS(destfile)



## ----GBMsummary, echo=echo_flag-------------------------------------------------------------------
gbmsummary <- summary(model)
knitr::kable(
  head(gbmsummary), booktabs = TRUE,
  caption = 'gbm relative influence'
)


## ---- eval=TRUE-----------------------------------------------------------------------------------
library(gbm)
train_gbm_rmse_vs_num_trees <- function(shrinkage, rerun, num_trees_2_exp=16) {
  #rerun <- TRUE
  set.seed(42)
  x <- 2 ^ seq(5, num_trees_2_exp, by = 1)
  rmse_vec <- numeric(length(x))
  count <- 1
  filename_vec1 <- c("models2/gbm_shrinkage_")
  filename_vec1 <-
    append(filename_vec1, gsub('\\.', 'point', toString(shrinkage)))
  filename_vec1 <- append(filename_vec1, "_ntrees_")
  filename_prefix1 <- paste(filename_vec1, collapse = '')
  
  for (val in x) {
    filename_vec2 <- append(filename_prefix1, val)
    filename_vec2 <- append(filename_vec2, "_v2.rds")
    filename <- paste(filename_vec2, collapse = '')
    if (!file.exists(filename) | rerun) {
      hboost <- gbm(
        dep_delay ~ .,
        data = train_df,
        n.trees = val,
        shrinkage = shrinkage
      ) # default shrinkage = 0.1
      saveRDS(hboost, filename)
      hboost <- readRDS(filename)
    } else {
      print("reading saved model")
      hboost <- readRDS(filename)
    }
    
    preds = predict(hboost, n.trees = val, newdata = test_df)
    mse = mean((test_df$dep_delay - preds) ^ 2)
    rmse <- sqrt(mse)
    rmse_vec[count] <- rmse
    print(val)
    print(rmse)
    count = count + 1
  }
  
  filename_vec1 <- c("performance2/gbm_shrinkage_")
  filename_vec1 <-
    append(filename_vec1, gsub('\\.', 'point', toString(shrinkage)))
  #filename_vec1 <- append(filename_vec1, "_ntrees_")
  #filename_prefix1 <- paste(filename_vec1, collapse = '')
  #filename_vec2 <- append(filename_prefix1, x[length(x)])
  
  #summary filename
  filename_vec_summary <- append(filename_vec1, "_v2_summary.csv")
  filename_summary <- paste(filename_vec_summary, collapse = '')
  
  #rmse_vs_num_trees filename
  filename_vec_rmse_vs_num_trees <- append(filename_vec1, "_v2_rmse_vs_num_trees.csv")
  filename_rmse_vs_num_trees <- paste(filename_vec_rmse_vs_num_trees, collapse = '')
  
  summary <- summary(hboost)
  write.csv(summary, filename_summary)
  
  num_trees_vs_rmse <-
    data.frame("num_trees" = x, "rmse" = rmse_vec)
  write.csv(
    num_trees_vs_rmse, filename_rmse_vs_num_trees
  )
}

train_gbm_rmse_vs_num_trees(shrinkage = 0.01, rerun = FALSE, num_trees_2_exp=17)
train_gbm_rmse_vs_num_trees(shrinkage = 0.001, rerun = FALSE, num_trees_2_exp=17)


## ---- results=results_option----------------------------------------------------------------------
shrinkage_0point01_bench <- read.csv('performance2/gbm_shrinkage_0point01_v2_rmse_vs_num_trees.csv')
shrinkage_0point01_bench$X <- NULL
shrinkage_0point01_bench <- shrinkage_0point01_bench %>% 
  dplyr::rename(
    "shrinkage_0point01_rmse" = rmse
    )
shrinkage_0point01_bench


## ---- results=results_option----------------------------------------------------------------------
shrinkage_0point001_bench <- read.csv('performance2/gbm_shrinkage_0point001_v2_rmse_vs_num_trees.csv')
shrinkage_0point001_bench$X <- NULL
shrinkage_0point001_bench <- shrinkage_0point001_bench %>% 
  dplyr::rename(
    "shrinkage_0point001_rmse" = rmse
    )
shrinkage_0point001_bench


## ---- results=results_option----------------------------------------------------------------------
gbm_merge_df <- merge(shrinkage_0point01_bench, shrinkage_0point001_bench, all.x = TRUE)
gbm_merge_df

## ----RMSEvsNumTrees, fig.cap = "plot of RMSE vs number of trees for shrinkage = 0.01 and shrinkage = 0.001", echo=echo_flag----
ggplot(gbm_merge_df, aes(num_trees)) + 
  geom_line(aes(y = shrinkage_0point01_rmse, colour = "shrinkage_0point01_rmse")) +  geom_point(aes(y = shrinkage_0point01_rmse, colour = "shrinkage_0point01_rmse")) +
  geom_line(aes(y = shrinkage_0point001_rmse, colour = "shrinkage_0point001_rmse")) + geom_point(aes(y = shrinkage_0point001_rmse, colour = "shrinkage_0point001_rmse"))+ ggtitle("GBM: RMSE vs. Number of Trees for Different Shrinkage") +
  xlab("Root Mean Squared Error (RMSE)") + ylab("number of trees")


## ---- results=results_option----------------------------------------------------------------------
benchmark_df <- rbind(benchmark_df, data.frame(model_description = 'GBM (tuned)', rmse=7.89071))
write_csv(benchmark_df, 'performance/benchmark_df.csv')


## ----benchmarkdf, echo=echo_flag------------------------------------------------------------------
knitr::kable(
  head(benchmark_df), booktabs = TRUE,
  caption = 'RMSE on the validation set, benchmark comparing all the models that we tried'
)


## ---- results=results_option----------------------------------------------------------------------
library(gbm)
num_trees <- 2^13
train_final_gbm <- function(filename, num_trees){
set.seed(42)
model <- gbm(dep_delay ~ ., data=train_and_validation_df,
              n.trees=num_trees, shrinkage=0.01) # default shrinkage = 0.1
saveRDS(model, filename)
return(model)
}

destfile <- "models_ultimate/final_train_on_train_and_validation_gbm_v1.rds"
if (!file.exists(destfile)) {
   train_final_gbm(destfile)
}

final_model <- readRDS(destfile)
final_preds = predict(final_model, newdata=train_and_validation_df, n.trees=num_trees)
final_rmse = sqrt(mean((train_and_validation_df$dep_delay - final_preds)^2))
summary(final_model)


## -------------------------------------------------------------------------------------------------
rmse


## -------------------------------------------------------------------------------------------------
print("done")

