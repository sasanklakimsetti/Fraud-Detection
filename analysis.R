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
    # if a column is numeric
    if (is.numeric(data[[col]])) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }
    # if a column is character
    else if (is.character(data[[col]])) {
      # replacing NA value with either previous or next valid value
      data[[col]] <- na.locf(data[[col]], na.rm = FALSE)
    }
  }
  return(data)
}

# Applying the cleaning function to the actual data
data <- cleaned_data(data)
# Cross checking the cleaned data
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

# Separating Date and Time from DateTime column
str(data$Datetime)
data$Datetime <- as.POSIXct(data$Datetime, format = "%Y-%m-%d %H:%M:%S")  # temporarily changing the datatype of Datetime so that we can extract Date and Time
data$Date <- as.Date(data$Datetime)
data$Time <- format(data$Datetime, "%H:%M:%S")
data$Datetime<-as.character(data$Datetime)

# saving the updated dataframe
write.csv(data, "updated_dataset.csv", row.names = FALSE)

normalize <- function(x) {
  if (min(x, na.rm = TRUE) == max(x, na.rm = TRUE)) {
    return(rep(0, length(x)))  # Handle constant columns
  } else {
    return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }
}

# algo 1: KNN
a<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
colnames(a)
ncol(a)
str(a)

# to-be factor columns are Card, Year, Month, Day, Use.Chip, Merchant.City, Merchant.State, Zip, MCC, Errors, HourOfDay, Hour, Is.Fraud, IsWeekend, TotalAmountPerUser, TotalAmountPerCard
factor_columns<-c("Card","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","IsWeekend","HasErrors")
a[factor_columns]<-lapply(a[factor_columns],factor)
# changing the format of year and month
a$Year <- as.numeric(format(as.Date(a$Datetime), "%Y"))
a$Month <- as.numeric(format(as.Date(a$Datetime), "%m"))
# Dropping Datetime from the dataframe as we already have Date and Time in the dataset
a <- a[, !(colnames(a) %in% c("Datetime"))]
# Converting all character columns to factors
char_cols <- sapply(a, is.character)
a[char_cols] <- lapply(a[char_cols], as.factor)
# Cross checking whether target column is a factor or not
a$Is.Fraud.<-factor(a$Is.Fraud.,levels = c(0,1),labels=c("No","Yes"))
str(a)
# checking the distribution of Fraud values in the dataset
table(a$Is.Fraud.)
round(prop.table(table(a$Is.Fraud.))*100,digits = 1)
summary(a)
library(ROSE)
a<-ROSE(Is.Fraud. ~ .,data=a,seed = 42)$data
# considering only numeric columns for normalization
numeric_col <- sapply(a, is.numeric)
# Applying normalization only on numeric columns
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
# training the model on the training data
a_test_prepd<-knn(train=a_train,test=a_test,cl=a_train_labels,k=100)
library(gmodels)
CrossTable(x=a_test_labels,y=a_test_prepd,prop.chisq = FALSE)
aa<-table(a_test_labels,a_test_prepd)
library(caret)
cnf1<-confusionMatrix(aa)
cnf1
knn_aacuracy<-(sum(diag(cnf1$table))/sum(cnf1$table))*100
print(paste("Accuracy: ",round(knn_aacuracy,2),"%"))


# algo 2: Naive Bayes
b<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
# sum(is.na(b))
# str(b)
# head(b)
# Checking unique values in the Is.Fraud column before conversion to factor
unique(b$Is.Fraud.)
# to-be factor columns are Card, Year, Month, Day, Use.Chip, Merchant.City, Merchant.State, Zip, MCC, Errors, HourOfDay, Hour, Is.Fraud, IsWeekend, TotalAmountPerUser, TotalAmountPerCard
factor_columns<-c("Card","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","IsWeekend","HasErrors")
b[factor_columns]<-lapply(b[factor_columns],factor)
# changing the format of year and month
b$Year <- as.numeric(format(as.Date(b$Datetime), "%Y"))
b$Month <- as.numeric(format(as.Date(b$Datetime), "%m"))
# Dropping Datetime as we already have Date and Time in the dataset
b <- b[, !(colnames(b) %in% c("Datetime"))]
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
# training the model on the training data
b_classifier<-naiveBayes(b_train,b_train_labels)
# b_classifier
b_pred<-predict(b_classifier,b_test)
# length(b_pred)
# length(b_test_labels)
bb<-table(b_pred,b_test_labels)
# bb
# making a table which tells about how many values have been predicted correctly classified by predicted and actual
CrossTable(b_pred,b_test_labels,prop.chisq = FALSE,prop.t = FALSE,dnn = c('predicted','actual'))  
library(caret)
cnf2<-confusionMatrix(bb)
cnf2
naiveBayes_aacuracy<-(sum(diag(cnf2$table))/sum(cnf2$table))*100
print(paste("Accuracy: ",round(naiveBayes_aacuracy,2),"%"))


# algo 3: Decision Tree
library(rpart)
library(rpart.plot)
library(caret)
c<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
colnames(c)
# to-be factor columns are Card, Year, Month, Day, Use.Chip, Merchant.City, Merchant.State, Zip, MCC, Errors, HourOfDay, Hour, Is.Fraud, IsWeekend, TotalAmountPerUser, TotalAmountPerCard
factor_columns<-c("Card","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","IsWeekend","HasErrors")
c[factor_columns]<-lapply(c[factor_columns],factor)
# changing the format of year and month
c$Year <- as.numeric(format(as.Date(c$Datetime), "%Y"))
c$Month <- as.numeric(format(as.Date(c$Datetime), "%m"))
c <- c[, !(colnames(c) %in% c("Datetime"))]  # Dropping Datetime as we already have Date and Time in the dataset
# Converting all character columns to factors
char_cols <- sapply(c, is.character)
c[char_cols] <- lapply(c[char_cols], as.factor)
c$Is.Fraud. <- factor(c$Is.Fraud., levels = c(0, 1), labels = c("No", "Yes"))
c[sapply(c, is.character)] <- lapply(c[sapply(c, is.character)], factor)
# upsampling the dataframe as there is an imbalance between the values in the Is.Fraud. column. It will be difficult for the model to get trained on every category present in the column.
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
# training the model on the training data
tree<-rpart(target,data=c_train,method="class")
png("decision_tree.png", width = 5000, height = 4000, res = 300)
rpart.plot(tree)
dev.off()
c_pred<-predict(tree,c_test,type="class")
cc<-table(c_test$Is.Fraud.,c_pred)
cc
cnf3<-confusionMatrix(cc)
cnf3
decision_tree_accuracy<-(sum(diag(cnf3$table))/sum(cnf3$table))*100
print(paste("Accuracy: ",round(decision_tree_accuracy, 2), "%"))


# algo 4: Support Vector Machine (SVM)
library(e1071)
library(caret)
d<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv",stringsAsFactors = FALSE)
str(d)
d$Time<-NULL
d$Date<-NULL
d$IsWeekend<-NULL
d$Datetime<-NULL
# Converting the Is.Fraud. column to a factor
d$Is.Fraud. <- factor(d$Is.Fraud., levels = c(0, 1), labels = c("No", "Yes"))
numeric_col<-sapply(d, is.numeric)
d[numeric_col]<-lapply(d[numeric_col],normalize)
char_cols <- sapply(d, is.character)
d[char_cols] <- lapply(d[char_cols], as.factor)
str(d)
set.seed(42)
indexes<-sample(1:nrow(d),0.7*nrow(d))
d_train<-d[indexes,]
# downsampling the dataframe as there is an imbalance between the values in the Is.Fraud. column. It will be difficult for the model to get trained on every category present in the column.
d_train_balanced <- downSample(
  x = d_train[, -which(names(d_train) == "Is.Fraud.")],   # All columns except the target column
  y = d_train$Is.Fraud.,                                  # target column
  yname = "Is.Fraud."                                      # target column name
)
d_test<-d[-indexes,]
# training the model on the training data
d_model <- svm(Is.Fraud. ~ ., data = d_train_balanced, kernel = "linear", cost = 1, scale = FALSE)
summary(d_model)
d_pred<-predict(d_model,newdata=d_test)
cnf4<-confusionMatrix(d_pred,d_test$Is.Fraud.)
cnf4
svm_model_accuracy<-(sum(diag(cnf4$table))/sum(cnf4$table))*100
print(paste("Accuracy: ",round(svm_model_accuracy,2),"%"))


# plotting the accuracies
library(ggplot2)
acc<-data.frame(model=c("KNN","NaiveBayes","DecisionTree","SVM"),
                accuracy=c(knn_aacuracy,naiveBayes_aacuracy,decision_tree_accuracy,svm_model_accuracy))

knn_aacuracy
naiveBayes_aacuracy
decision_tree_accuracy
svm_model_accuracy

ggplot(acc, aes(x = model, y = accuracy, fill = model)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = round(accuracy, 2)), vjust = -0.5) +
  labs(title = "Fraud Detection Models Accuracy Comparison", x = "Model", y = "Accuracy (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
