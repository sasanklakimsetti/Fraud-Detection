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
# factor_columns<-c("Card","Year","Month","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","IsWeekend","TotalAmountPerUser","TotalAmountPerCard")
# a[factor_columns]<-lapply(data[factor_columns],factor)
a$Is.Fraud.<-factor(a$Is.Fraud.,levels = c("0","1"),labels=c("No","Yes"))
str(a)
# checking the distribution of Fraud in the dataset
table(a$Is.Fraud.)
round(prop.table(table(a$Is.Fraud.))*100,digits = 1)
summary(a)
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
norm_num <- as.data.frame(lapply(a[, numeric_col], normalize))
# Combinining normalized numeric columns with the non-numeric columns
a_norm <- cbind(norm_num, a[, !numeric_col])
set.seed(42)
indexes<-sample(1:nrow(a),0.7*nrow(a))
a_train<-a_norm[indexes,]
a_test<-a_norm[-indexes,]
sum(is.na(a_train))
sum(is.na(a_test))
a_train_labels<-a[indexes,]$Is.Fraud.
a_test_labels<-a[-indexes,]$Is.Fraud.
sum(is.na(a_train_labels))
sum(is.na(a_test_labels))
library(class)
a_test_prepd<-knn(train=a_train,test=a_test,cl=a_train_labels,k=69)
