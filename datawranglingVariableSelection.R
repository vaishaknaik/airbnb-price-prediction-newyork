
# Title: "Data Wrangling and Variable Selection"
# Author: "Vaishak Naik"



## Imported Library


library(forcats)
library(caret)
library(dplyr)
library(glmnet)


## Importing Data


data <- read.csv('analysisData.csv')
scoringData <- read.csv('scoringData.csv')


## Price Distribution 

# hist(data$price)


## Combining Analysis and Scoring Data for Cleaning the Data


combinedData <- bind_rows(data, scoringData)




## Data Wrangling

### Using Regular Expression to Clean and Standardize Data

combinedData$zipcode <- gsub("[^\\d]", "", combinedData$zipcode, perl=TRUE)
combinedData$zipcode <- substr(combinedData$zipcode, 1, 5)
combinedData$zipcode[nchar(combinedData$zipcode) < 5] <- NA_character_
combinedData$zipcode <- as.factor(combinedData$zipcode)


combinedData$city <- gsub(",|-|\\n|\\)|\\(|/|\\.", " ", tolower(combinedData$city))
combinedData$city <- stringr::str_trim(gsub("\\s+", " ", combinedData$city))







### Checking and Converting to Standard NA Format


sum(combinedData=="", na.rm=TRUE)
sum(combinedData=="N/A", na.rm=TRUE)
sum(combinedData=="NA", na.rm=TRUE)

char2na <- function(x) {
  x <- as.character(x)
  return(case_when(
    x == "" ~ NA_character_,
    x == "N/A" ~ NA_character_,
    TRUE ~ x
  ))
}

combinedData <- combinedData %>%
  mutate_if(is.character, char2na) %>%
  mutate_if(is.factor, char2na)
sum(combinedData=="", na.rm=TRUE)
char_ind <-  which(sapply(combinedData, class) == "character")

sum(table(combinedData[,char_ind[1]])==1)
sum(table(combinedData[,char_ind[2]])==1)







### Median Imputation


combinedData$cleaning_fee[is.na(combinedData$cleaning_fee)] <- median(combinedData$cleaning_fee, na.rm=TRUE)
combinedData$square_feet[is.na(combinedData$square_feet)] <- median(combinedData$square_feet, na.rm=TRUE)

numeric_predictors <- which(colnames(combinedData) != "price" &  sapply(combinedData, is.numeric))

imp_model_med <- preProcess(combinedData[,numeric_predictors], method = 'medianImpute')
set.seed(617)
combinedData[,numeric_predictors] <- predict(imp_model_med, newdata=combinedData[,numeric_predictors])

combinedData$host_acceptance_rate <- as.numeric(gsub("%", "", combinedData$host_acceptance_rate))
combinedData$host_response_rate <- as.numeric(gsub("%", "", combinedData$host_response_rate))

combinedData$cleaning_fee = as.character(combinedData$cleaning_fee)
combinedData$cleaning_fee = str_replace(combinedData$cleaning_fee, ",", "")
combinedData$cleaning_fee = as.numeric(str_sub(combinedData$cleaning_fee, 2))
indices = which(is.na(combinedData$cleaning_fee))
combinedData[indices, "cleaning_fee"] = median(combinedData$cleaning_fee,na.rm=TRUE)



### Data Imputation using quantiles

fun <- function(x, q1, q3){
  quantiles <- quantile( x, c(q1, q3), na.rm = TRUE )
  x[ x < quantiles[1] ] <- median(x,na.rm=TRUE)
  x[ x > quantiles[2] ] <- median(x,na.rm=TRUE)
  x
}



combinedData$host_acceptance_rate <- fun( combinedData$host_acceptance_rate , 0.1, 0.9)
combinedData$host_acceptance_rate <- fun( combinedData$host_acceptance_rate, 0.1, 0.9)
combinedData$guests_included <- fun( combinedData$guests_included, 0.1, 0.9)
combinedData$maximum_nights <- fun( combinedData$maximum_nights, 0.1, 0.9)
combinedData$minimum_nights <- fun( combinedData$minimum_nights, 0.1, 0.9 )
combinedData$cleaning_fee <- fun( combinedData$cleaning_fee, 0.1, 0.9)
combinedData$bathrooms <- fun( combinedData$bathrooms, 0.1, 0.9)
combinedData$bedrooms <- fun( combinedData$bedrooms, 0.1, 0.9)
combinedData$security_deposit <- fun( combinedData$security_deposit, 0.35, 0.999)
combinedData$calculated_host_listings_count_private_rooms <- fun( combinedData$calculated_host_listings_count_private_rooms, 0.35, 0.98)




### Date Manupulation, Using Only Month and Year



indices1 = which(is.na(combinedData$host_since))
combinedData[indices1, "host_since"] = c('2012-01-01')
combinedData$host_since_test <- as.POSIXct(combinedData$host_since, format = "%Y-%m-%d")
combinedData$host_since_year <- format(combinedData$host_since_test, format="%m-%Y")
sort(table(combinedData$host_since_year))




### Low Frequency Data Grouping Using Forcats



indices2 = which(is.na(combinedData$host_identity_verified))
combinedData[indices2, "host_identity_verified"] = c('f')

combinedData$host_verifications <- fct_lump_min(combinedData$host_verifications, min=6, other_level = "Other")

combinedData$bed_type <- fct_lump_min(combinedData$bed_type, min=280, other_level = "Futon")

combinedData$property_type <- fct_lump_min(combinedData$property_type, min=10, other_level = "Other")

combinedData$neighbourhood_cleansed <- fct_lump_min(combinedData$neighbourhood_cleansed, min=10, other_level = "Other")

combinedData$neighbourhood_group_cleansed <- fct_lump_min(combinedData$neighbourhood_group_cleansed, min=10, other_level = "Staten Island")

combinedData$cancellation_policy <- fct_lump_min(combinedData$cancellation_policy, min=10, other_level = "Other")

combinedData$host_is_superhost[is.na(combinedData$host_is_superhost)] <- c("t")

combinedData$host_response_rate[is.na(combinedData$host_response_rate)] <- 76 #median(combinedData$host_response_rate,na.rm=TRUE)

combinedData$host_acceptance_rate[is.na(combinedData$host_acceptance_rate)] <- 48 #median(combinedData$host_acceptance_rate,na.rm=TRUE)

combinedData$host_response_time[is.na(combinedData$host_response_time)] <- c("not sure")

combinedData$zipcode[is.na(combinedData$zipcode)] <- c("44444")
combinedData$zipcode <- fct_lump_min(combinedData$zipcode, min=10, other_level = "11363")

combinedData$city <- fct_lump_min(combinedData$city, min=10, other_level = "Other")

combinedData$city[is.na(combinedData$city)] <- c("Other")



### Data type Conversion

combinedData$require_guest_phone_verification <- as.logical(unclass(as.factor(combinedData$require_guest_phone_verification)))
combinedData$host_identity_verified <- unclass(as.factor(combinedData$host_identity_verified)) 
combinedData$host_response_time <- unclass(as.factor(combinedData$host_response_time)) 
combinedData$host_is_superhost <- as.logical(unclass(as.factor(combinedData$host_is_superhost)))
combinedData$host_verifications <- as.factor(unclass(as.factor(combinedData$host_verifications)))
combinedData$room_type <- as.numeric(unclass(as.factor(combinedData$room_type)))


## Variable Selection

### Variables Shortlist Based on Domain Knowledge

combinedData = combinedData[, c('id','neighbourhood_cleansed',
                                'property_type',
                                'room_type',
                                'accommodates',
                                'bathrooms',
                                'bedrooms',
                                'price',
                                'cleaning_fee',
                                'availability_30',
                                'availability_60',
                                'number_of_reviews',
                                'review_scores_rating',
                                'reviews_per_month',
                                'zipcode',
                                'city',
                                'cancellation_policy',
                                'host_acceptance_rate',
                                'host_is_superhost',
                                'host_response_rate',
                                'host_response_time',
                                'neighbourhood_group_cleansed',
                                'host_listings_count',
                                'availability_90',
                                'extra_people',
                                'security_deposit',
                                'minimum_nights',
                                'maximum_nights',
                                'review_scores_value',
                                'review_scores_location',
                                'review_scores_communication',
                                'review_scores_checkin',
                                'review_scores_cleanliness',
                                'review_scores_accuracy',
                                'reviews_per_month',
                                'instant_bookable',
                                'host_listings_count',
                                'host_since',
                                'host_verifications',
                                'host_identity_verified',
                                'host_since_year',
                                'guests_included',
                                'require_guest_phone_verification',
                                'calculated_host_listings_count_private_rooms'
)]





### Variable Selection using lasso regression

dataForVariableSelection <- combinedData[!is.na(combinedData$price),]
x = model.matrix(price~.-1, data = dataForVariableSelection)
y = dataForVariableSelection$price
cv.lasso = cv.glmnet(x,y,alpha=1)
#coef(cv.lasso)

variableNew <- c("id","price","host_listings_count" 
                 ,"accommodates"
                 ,"availability_90" 
                 ,"extra_people" 
                 ,"number_of_reviews"
                 ,"bathrooms"
                 ,"security_deposit"
                 ,"room_type"
                 ,"property_type"
                 ,"host_acceptance_rate"
                 ,"zipcode"
                 ,"cleaning_fee"
                 ,"bedrooms"
                 ,"host_response_rate"
                 ,"host_is_superhost"
                 ,"availability_30"
                 ,"minimum_nights"
                 ,"maximum_nights"
                 ,"cancellation_policy"
                 ,"review_scores_value"
                 ,"review_scores_location"
                 ,"review_scores_communication"
                 ,"review_scores_checkin"
                 ,"review_scores_cleanliness"
                 ,"review_scores_accuracy"
                 ,"instant_bookable"
                 ,"city"
                 ,"host_since_year"
                 ,"guests_included"
                 ,"neighbourhood_cleansed"
                 ,"require_guest_phone_verification")

combinedDataModified = combinedData[, variableNew]



### Removing Varibles with Near Zero Variance

zero_var_table <- nearZeroVar(combinedDataModified, saveMetrics= TRUE)
combinedDataModified <- combinedDataModified[, !zero_var_table$nzv]




## Dummy encoding

dmy <- dummyVars(" ~ .", data = combinedDataModified)
combinedDataModified <- data.frame(predict(dmy, newdata = combinedDataModified))


## Splitting Analysis and Scoring Data

combinedDataModified$id <- combinedData$id
combinedDataModified$price <- combinedData$price


data <- combinedDataModified[!is.na(combinedDataModified$price),]
scoringData <- combinedDataModified[is.na(combinedDataModified$price),]

