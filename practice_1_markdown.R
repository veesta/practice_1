"1_raw_to_analytic_data_practice_one_script.R"
library(tidyverse)

#load raw data
raw_data <- read_csv(file="practice1_analytic_data.csv")

#View (raw_data)

#remove missing data 
raw_data <- read_csv(file="practice1_analytic_data.csv", na=c("","NA","-999"))

#View (raw_data)

#tell R about categorical variables sex and major 

categorical_variables <- select(raw_data, sex, major)

categorical_variables$sex <- as.factor(categorical_variables$sex)
levels(categorical_variables$sex) <- list("Male"=1, "Female"=2)
       
categorical_variables$major <- as.factor(categorical_variables$major)
levels(categorical_variables$major) <- list("Psychology"=1, "Sociology"=2, "Math"=3, "Engineering"=4, "Science"=5)
       
#View(raw_data)

#create your scaled item sets

self_esteem_items <- select (raw_data, SE1, SE2, SE3, SE4, SE5)
dep_items <- select (raw_data, D1, D2, D3, D4, D5)
job_sat_items <- select (raw_data, JS1, JS2, JS3, JS4, JS5)

#View (dep_items)

#check for impossible values 
psych::describe(self_esteem_items)

is_bad_value <- self_esteem_items<1 | self_esteem_items>7
self_esteem_items[is_bad_value] <- NA

psych::describe(self_esteem_items)

psych::describe(dep_items)
is_bad_value <- dep_items<1 | dep_items>4
dep_items[is_bad_value] <- NA
psych::describe(dep_items)

psych::describe(job_sat_items)
is_bad_value <- job_sat_items<1 | job_sat_items>6
job_sat_items[is_bad_value] <- NA

psych::describe(job_sat_items)

#deal with reverse keyed items

#View(self_esteem_items)
self_esteem_items <- mutate(self_esteem_items,SE1=8-SE1)
#View(self_esteem_items)

dep_items <- mutate(dep_items, D4=5-D4)
dep_items <- mutate(dep_items, D5=5-D5)
#View(dep_items)

job_sat_items <- mutate(job_sat_items, JS1=7-JS1)
job_sat_items <- mutate(job_sat_items, JS2=7-JS2)
#View(job_sat_items)

#obtain scaled mean scores 

self_esteem <- psych::alpha(as.data.frame(self_esteem_items), check.keys=FALSE)$scores
dep <- psych::alpha(as.data.frame(dep_items), check.keys=FALSE)$scores
job_sat <- psych::alpha(as.data.frame(job_sat_items), check.keys=FALSE)$scores

#combine into analytic_data
analytic_data <- cbind(categorical_variables,self_esteem,dep,job_sat)

analytic_data
