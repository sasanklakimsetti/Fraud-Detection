data<-read.csv("C://lpu//5th sem//INT234//Project//dataset.csv")
View(data)
colnames(data)
fraud_column <- data[["Is.Fraud."]]   # Extract the 'Is Fraud?' column
data <- data[, !names(data) %in% "Is.Fraud."]  # Remove the 'Is Fraud?' column
data$Is.Fraud. <- fraud_column        # Add it back to the end

# View the updated dataframe
head(data)

# Optionally, save the updated dataframe
write.csv(data, "updated_dataset.csv", row.names = FALSE)
