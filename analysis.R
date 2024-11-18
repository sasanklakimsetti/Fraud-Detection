data<-read.csv("C://lpu//5th sem//INT234//Project//dataset.csv")
# View(data)
colnames(data)
# removing the Is.Fraud column from its actual position to the last so that it will be easy during analysis
fraud_column <- data[["Is.Fraud."]]   # Extracting 'Is Fraud?' column
data <- data[, !names(data) %in% "Is.Fraud."]  # Removing the 'Is Fraud?' column
data$Is.Fraud. <- fraud_column        # Adding it back to the end
# checking for null values in the dataset
colSums(is.na(data))
head(data)

str(data)
library(zoo)

# Function to clean the data for safety
cleaned_data <- function(data) {
  for (col in colnames(data)) {
    # If the column is numeric
    if (is.numeric(data[[col]])) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }
    # If the column is character
    else if (is.character(data[[col]])) {
      # Replace NA values with previous valid value (or next valid if needed)
      data[[col]] <- na.locf(data[[col]], na.rm = FALSE)  # Carry forward the last valid value
    }
  }
  return(data)
}

# Apply the function to your data
data <- cleaned_data(data)

# Check the cleaned data
head(data)
# Time has all dates same and there is another column DateTime with correct data. So, deleting the Time column as it doesn't contribute for the analysis
data$Time<-NULL

# deleting DayOfMonth column as Day and DayOfMonth are same, and Hour and HourDay are same, deleting Hour
data$DayOfMonth<-NULL
data$Hour<-NULL
# deleting DayOfWeek as it has 0 only
data$DayOfWeek<-NULL

# deleting the Merchant.Name and User as they doesn't contribute for the analysis
data$Merchant.Name<-NULL
data$User<-NULL
# deleting mean and median columns 
data$mean<-NULL
data$median<-NULL

# Seperating Date and Time from DateTime column
str(data$Datetime)
data$Datetime <- as.POSIXct(data$Datetime, format = "%Y-%m-%d %H:%M:%S")  # temporarily changing the datatype of Datetime so that we can extract Date and Time
data$Date <- as.Date(data$Datetime)
data$Time <- format(data$Datetime, "%H:%M:%S")
data$Datetime<-as.character(data$Datetime)

# saving the updated dataframe
write.csv(data, "updated_dataset.csv", row.names = FALSE)

# algo 1: KNN

a<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
colnames(a)
ncol(a)
str(a)

# to-be factor columns are Card, Year, Month, Day, Use.Chip, Merchant.City, Merchant.State, Zip, MCC, Errors, HourOfDay, Hour, Is.Fraud, IsWeekend, TotalAmountPerUser, TotalAmountPerCard
factor_columns<-c("Card","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","IsWeekend","HasErrors")
a[factor_columns]<-lapply(a[factor_columns],factor)
a$Year <- as.numeric(format(as.Date(a$Datetime), "%Y"))
a$Month <- as.numeric(format(as.Date(a$Datetime), "%m"))
a <- a[, !(colnames(a) %in% c("Datetime"))]  # Dropping Datetime as we already have Date and Time in the dataset
# Converting all character columns to factors
char_cols <- sapply(a, is.character)
a[char_cols] <- lapply(a[char_cols], as.factor)
# Ensure target column is a factor
a$Is.Fraud.<-factor(a$Is.Fraud.,levels = c("0","1"),labels=c("No","Yes"))
str(a)
# checking the distribution of Fraud in the dataset
table(a$Is.Fraud.)
round(prop.table(table(a$Is.Fraud.))*100,digits = 1)
summary(a)
library(ROSE)
a<-ROSE(Is.Fraud. ~ .,data=a,seed = 42)$data
normalize <- function(x) {
  if (min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
    return(rep(0, length(x)))  # Handle constant columns
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}
# considering only numeric columns for normalization
numeric_col <- sapply(a, is.numeric)
# Applying normalization only to numeric columns
a_norm <- as.data.frame(lapply(a[, numeric_col], normalize))
indexes<-sample(1:nrow(a),0.7*nrow(a))
a_train<-a_norm[indexes,]
a_test<-a_norm[-indexes,]
a_train<-as.data.frame(a_train)
a_test<-as.data.frame(a_test)
a_train_labels<-a[indexes,]$Is.Fraud.
a_test_labels<-a[-indexes,]$Is.Fraud.
# sum(is.na(a_train_labels))
# sum(is.na(a_test_labels))
library(class)
a_test_prepd<-knn(train=a_train,test=a_test,cl=a_train_labels,k=100)
library(gmodels)
CrossTable(x=a_test_labels,y=a_test_prepd,prop.chisq = FALSE)
aa<-table(a_test_labels,a_test_prepd)
library(caret)
cnf1<-confusionMatrix(aa)
cnf1
knn_aacuracy<-(sum(diag(cnf1$table))/sum(cnf1$table))*100
print(paste(knn_aacuracy,"%"))



# algo 2: Naive Bayes

b<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
# sum(is.na(b))
# str(b)
# head(b)
# Check unique values in the Is.Fraud column before conversion
unique(b$Is.Fraud.)
# to-be factor columns are Card, Year, Month, Day, Use.Chip, Merchant.City, Merchant.State, Zip, MCC, Errors, HourOfDay, Hour, Is.Fraud, IsWeekend, TotalAmountPerUser, TotalAmountPerCard
factor_columns<-c("Card","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","IsWeekend","HasErrors")
b[factor_columns]<-lapply(b[factor_columns],factor)
b$Year <- as.numeric(format(as.Date(b$Datetime), "%Y"))
b$Month <- as.numeric(format(as.Date(b$Datetime), "%m"))
b <- b[, !(colnames(b) %in% c("Datetime"))]  # Dropping Datetime as we already have Date and Time in the dataset
# Converting all character columns to factors
char_cols <- sapply(b, is.character)
b[char_cols] <- lapply(b[char_cols], as.factor)
b$Is.Fraud. <- factor(b$Is.Fraud., levels = c(0, 1), labels = c("No", "Yes"))
# sum(is.na(b$Is.Fraud.))
# converting all character columns into factors
b[sapply(b, is.character)] <- lapply(b[sapply(b, is.character)], factor)
# Remove DateTime, Date, and Time columns as naive bayes can't compute them due to the format
b <- b[, !names(b) %in% c("DateTime", "Date", "Time")]
# sum(is.na(b))
b<-ROSE(Is.Fraud. ~ .,data=b,seed = 42)$data
normalize <- function(x) {
  if (min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
    return(rep(0, length(x)))  # Or return rep(1, length(x)) if you prefer to normalize constant columns to 1
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}
# considering only numeric columns for normalization
numeric_col <- sapply(b, is.numeric)
# Applying normalization only to numeric columns
norm_num <- as.data.frame(lapply(b[, numeric_col], normalize))
b_norm <- cbind(norm_num, b[, !numeric_col])
# sum(is.na(b_norm))
#set.seed(42)
indexes<-sample(1:nrow(b),0.7*nrow(b))
b_train<-b_norm[indexes,]
b_test<-b_norm[-indexes,]
b_train_labels<-b[indexes,]$Is.Fraud.
b_test_labels<-b[-indexes,]$Is.Fraud.
# sum(is.na(b_train))
# sum(is.na(b_test))
library(e1071)
b_classifier<-naiveBayes(b_train,b_train_labels)
# b_classifier
b_pred<-predict(b_classifier,b_test)
# length(b_pred)
# length(b_test_labels)
bb<-table(b_pred,b_test_labels)
# bb
CrossTable(b_pred,b_test_labels,prop.chisq = FALSE,prop.t = FALSE,dnn = c('predicted','actual'))  # making a table which tells about how many values have been predicted correctly classified by predicted and actual
library(caret)
cnf2<-confusionMatrix(bb)
naiveBayes_aacuracy<-(sum(diag(cnf2$table))/sum(cnf2$table))*100
print(paste(naiveBayes_aacuracy,"%"))


# algo 3: Decision Tree
library(rpart)
library(rpart.plot)
library(caret)
c<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
colnames(c)
# to-be factor columns are Card, Year, Month, Day, Use.Chip, Merchant.City, Merchant.State, Zip, MCC, Errors, HourOfDay, Hour, Is.Fraud, IsWeekend, TotalAmountPerUser, TotalAmountPerCard
factor_columns<-c("Card","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","IsWeekend","HasErrors")
c[factor_columns]<-lapply(c[factor_columns],factor)
c$Year <- as.numeric(format(as.Date(c$Datetime), "%Y"))
c$Month <- as.numeric(format(as.Date(c$Datetime), "%m"))
c <- c[, !(colnames(c) %in% c("Datetime"))]  # Dropping Datetime as we already have Date and Time in the dataset
# Converting all character columns to factors
char_cols <- sapply(c, is.character)
c[char_cols] <- lapply(c[char_cols], as.factor)
c$Is.Fraud. <- factor(c$Is.Fraud., levels = c(0, 1), labels = c("No", "Yes"))
c[sapply(c, is.character)] <- lapply(c[sapply(c, is.character)], factor)
c_balanced <- upSample(x = c[, -which(names(c) == "Is.Fraud.")], 
                       y = c$Is.Fraud.)
c_balanced$Is.Fraud. <- c_balanced$Class
c_balanced$Class <- NULL
set.seed(42)
indexes<-sample(1:nrow(c_balanced),0.7*nrow(c_balanced))
c_train<-c_balanced[indexes,]
c_test<-c_balanced[-indexes,]
target<-Is.Fraud. ~ Month+Day+Amount+Use.Chip+MCC+Errors.+HourOfDay+TotalMinutes+TransactionFrequency
# ,control = rpart.control(maxdepth = 5, cp = 0.02, minsplit = 20)
tree<-rpart(target,data=c_train,method="class")
# png("decision_tree.png", width = 5000, height = 4000, res = 300)
rpart.plot(tree)
# dev.off()
c_pred<-predict(tree,c_test,type="class")
cc<-table(c_test$Is.Fraud.,c_pred)
cc
cnf3<-confusionMatrix(cc)
decision_tree_accuracy<-sum(diag(cnf3$table))/sum(cnf3$table)
print(paste(round(decision_tree_accuracy * 100, 2), "%"))



# algo 4: Artificial Neural Networks
d<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
# d_train$Use.Chip <- as.factor(d_train$Use.Chip)
# d_train$Errors. <- as.factor(d_train$Errors.)
# d_train$HasErrors <- as.factor(d_train$HasErrors)
d$Year <- as.numeric(format(as.Date(d$Datetime), "%Y"))
d$Month <- as.numeric(format(as.Date(d$Datetime), "%m"))
d <- d[, !(colnames(d) %in% c("Datetime"))]  # Dropping Datetime as we already have Date and Time in the dataset
# d$Is.Fraud. <- factor(d$Is.Fraud., levels = c(0, 1), labels = c("No", "Yes"))
# sum(is.na(b$Is.Fraud.))
# converting all character columns into factors
# d[sapply(d, is.character)] <- lapply(d[sapply(d, is.character)], factor)
normalize <- function(x) {
  if (min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
    return(rep(0, length(x)))  # Handle constant columns
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}
numeric_col <- sapply(d, is.numeric)
# Applying normalization only to numeric columns
norm_num<-as.data.frame(lapply(d[numeric_col],normalize))
d_norm<-cbind(norm_num, d[, !numeric_col])
d_norm$Is.Fraud. <- factor(d_norm$Is.Fraud., levels = c(0, 1), labels = c("No", "Yes"))
d_balanced <- upSample(x = d_norm[, -which(names(d_norm) == "Is.Fraud.")], y = d_norm$Is.Fraud.)
d_balanced$Is.Fraud. <- d_balanced$Class 
d_balanced$Class <- NULL # Split dataset into training and testing 
set.seed(42)
indexes <- sample(1:nrow(d_balanced), 0.7 * nrow(d_balanced)) 
d_train <- d_balanced[indexes,]
d_test <- d_balanced[-indexes,]
d_train$Amount <- scale(d_train$Amount)
d_train$TotalMinutes <- scale(d_train$TotalMinutes)
d_train$TransactionFrequency <- scale(d_train$TransactionFrequency)
table(d_train$Is.Fraud.)
table(d_test$Is.Fraud.)
# sum(is.na(d_test))
library("neuralnet")
colnames(d_norm)
d_model<-neuralnet(Is.Fraud. ~ Amount+TotalMinutes+TransactionFrequency, data = d_train, hidden = c(5, 3), linear.output = FALSE)
plot(d_model)
d_results<-compute(d_model,d_test)
d_pred_fraud<-d_results$net.result
d_pred_class <- ifelse(d_pred_fraud > 0.5, 1, 0)
# Find the union of the levels of predicted and actual factors
levels <- union(levels(factor(d_pred_class)), levels(factor(d_test$Is.Fraud.)))

# Ensure both factors have the same levels
d_pred_class <- factor(d_pred_class, levels = levels)
d_test$Is.Fraud. <- factor(d_test$Is.Fraud., levels = levels)
dd<-table(d_pred_class,d_test$Is.Fraud.)
dd
# Create confusion matrix
cnf4 <- confusionMatrix(d_pred_class, d_test$Is.Fraud.)

# Print confusion matrix
print(cnf4)