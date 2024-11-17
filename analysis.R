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

a<-read.csv("C://lpu//5th sem//INT234//Project//updated_dataset.csv")
colnames(a)
str(a)

# to-be factor columns are Card, Year, Month, Day, Use.Chip, Merchant.City, Merchant.State, Zip, MCC, Errors, HourOfDay, Hour, Is.Fraud, IsWeekend, TotalAmountPerUser, TotalAmountPerCard
factor_columns<-c("Card","Year","Month","Day","Use.Chip","Merchant.City","Merchant.State","Zip","MCC","Errors.","HourOfDay","Is.Fraud.","IsWeekend","TotalAmountPerUser","TotalAmountPerCard")
a[factor_columns]<-lapply(data[factor_columns],factor)
str(a)
