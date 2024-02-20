coffee <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/DATA NOTES/Datacamp Data Scientist Cert/coffee.csv")

coffee$Rating <- coffee$Rating %>% replace_na(0)
coffee$Reviews <- coffee$Reviews %>% replace_na(median(coffee$Reviews, na.rm = TRUE))
coffee$Dine.in.option <- coffee$`Dine in option` %>% replace_na(FALSE)
coffee$Takeout.option <- coffee$`Takeout option` %>% replace_na(FALSE)
coffee$Delivery.option <- coffee$`Delivery option` %>% replace_na(FALSE)

coffee["Takeout.option"][coffee["Takeout.option"] == ''] <- "False"
coffee["Dine.in.option"][coffee["Dine.in.option"] == ''] <- "False"
coffee["Delivery.option"][coffee["Delivery.option"] == ''] <- "False"

coffee$Region <- as.factor(coffee$Region)
coffee$Rating <- factor(coffee$Rating, levels = seq(0, 5, by = 0.1), ordered = TRUE)
coffee$Price <- factor(coffee$Price, ordered = TRUE)
coffee$Reviews <- as.numeric(coffee$Reviews)
coffee$Place.name <- as.character(coffee$`Place name`)
coffee$Place.type <- as.factor(coffee$`Place type`)

clean_data <-  coffee[c(1,13,14,4,5,6,12,10,11)]

clean_data

###############################

task2 <- coffee %>% group_by(Rating) %>% 
  summarize(med_review = median(Reviews), min_review = min(Reviews), 
            max_review = max(Reviews)) %>% mutate(rating = Rating)

reviews_by_rating <- task2[,c(5,2,3,4)]
reviews_by_rating

################################

#Fit a baseline model to predict the number of reviews a store will get.
#Fit your model using the data contained in “train.csv”

#Use “validation.csv” to predict new values based on your model. You must return a 
#dataframe named base_result, that includes Place name and rating. The rating 
#column must be your predicted values.

train <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/DATA NOTES/Datacamp Data Scientist Cert/train.csv")

colSums(is.na(train))
train[train == ''] <- NA
colSums(is.na(train))

train$Rating <- train$Rating %>% replace_na(0)
train$Reviews <- train$Reviews %>% replace_na(round(median(coffee$Reviews, na.rm = TRUE)))
train$Dine.in.option <- train$Dine.in.option %>% replace_na("False")
train$Takeout.option <- train$Takeout.option %>% replace_na("False")
colSums(is.na(train))

train$Region <- as.factor(train$Region)
train$Rating <- factor(train$Rating, levels = seq(0, 5, by = 0.1), ordered = TRUE)
train$Price <- factor(train$Price, ordered = TRUE)
train$Reviews <- as.numeric(train$Reviews)
train$Place.name <- as.character(train$Place.name)
train$Place.type <- as.factor(train$Place.type)
train$Delivery.option <- as.logical(train$Delivery.option)
train$Dine.in.option <- as.logical(train$Dine.in.option)
train$Takeout.option <- as.logical(train$Dine.in.option)

train_clean <- train

validation <- read.csv("~/Library/Mobile Documents/com~apple~CloudDocs/DATA NOTES/Datacamp Data Scientist Cert/validation.csv")

colSums(is.na(validation))
validation[validation == ''] <- NA
colSums(is.na(validation))

validation$Rating <- validation$Rating %>% replace_na(0)
validation$Dine.in.option <- validation$Dine.in.option %>% replace_na("False")
validation$Takeout.option <- validation$Takeout.option %>% replace_na("False")
colSums(is.na(validation))

validation$Region <- as.factor(validation$Region)
validation$Rating <- factor(validation$Rating, levels = seq(0, 5, by = 0.1), ordered = TRUE)
validation$Price <- factor(validation$Price, ordered = TRUE)
validation$Place.name <- as.character(validation$Place.name)
validation$Place.type <- as.factor(validation$Place.type)
validation$Delivery.option <- as.logical(validation$Delivery.option)
validation$Dine.in.option <- as.logical(validation$Dine.in.option)
validation$Takeout.option <- as.logical(validation$Dine.in.option)

validation_clean <- validation

# model 1: 

# Fit lm model: model
model1 <- lm(Reviews ~ Place.type + Region + Price, train_clean)

# Predict on full data: p
p <- predict(model1, train_clean)
validation_clean$rating <- predict(model1, newdata = validation_clean)

# Compute errors: error
error <- p - train_clean[["Reviews"]]

# Calculate RMSE
sqrt(mean(error ^ 2)) 

base_result <- validation_clean[c(2,9)]

## Model 2

model2 <- train(
  Reviews ~ Place.type + Rating + Price + Delivery.option + Dine.in.option + Takeout.option,
  tuneLength = 10,
  data = train_clean, 
  method = "glmnet",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE))

# Predict on full data
p2 <- predict(model2, train_clean)
validation_clean2 <- validation_clean
validation_clean2$rating <- predict(model2, newdata = validation_clean)

# Compute errors: error
error2 <- p2 - train_clean[["Reviews"]]

# Calculate RMSE
sqrt(mean(error2 ^ 2)) 

compare_result <- validation_clean2[c(2,9)]

## Model 3: 

model3 <- train(
  Reviews ~ Place.type + Rating + Price + Delivery.option + Dine.in.option + Takeout.option,
  tuneLength = 10,
  data = train_clean, 
  method = "ranger",
  trControl = trainControl(
    method = "cv", 
    number = 5, 
    verboseIter = TRUE))

# Predict on full data
p3 <- predict(model3, train_clean)
validation_clean3 <- validation_clean
validation_clean3$rating <- predict(model3, newdata = validation_clean)

# Compute errors: error
error3 <- p3 - train_clean[["Reviews"]]

# Calculate RMSE
sqrt(mean(error3 ^ 2)) 

compare_result <- validation_clean2[c(2,9)]

  