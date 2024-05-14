# Load text file into local variable called 'data'
data = read.delim(file = 'C:/Users/Harish/Desktop/sem5_projects/MKTANA/Assign2/purchases.txt', header = FALSE, sep = '\t', dec = '.')

# Add headers and interpret the last column as a date, extract year of purchase
colnames(data) = c('customer_id', 'purchase_amount', 'date_of_purchase')
data$date_of_purchase = as.Date(data$date_of_purchase, "%Y-%m-%d")
data$days_since       = as.integer(difftime(time1 = "2016-01-01",
                                            time2 = data$date_of_purchase,
                                            units = "days"))

# Display the data after transformation
head(data)
summary(data)

# Compute key marketing indicators using SQL language
library(sqldf)

# Compute recency, frequency, and average purchase amount
customers = sqldf("SELECT customer_id,
                  MIN(days_since) AS 'recency',
                  COUNT(*) AS 'frequency',
                  AVG(purchase_amount) AS 'amount'
                  FROM data GROUP BY 1")

# Explore the data
head(customers)
summary(customers)
# Assuming 'customers' data frame is already available from the previous code

# Define scoring function for recency
score <- function(value, breakpoints, scores) {
  intervals <- findInterval(value, breakpoints, all.inside = TRUE)
  return(ifelse(is.na(intervals), NA, scores[intervals]))
}

# Define function to print 33.33% quantiles
print_quantiles <- function(data, variable) {
  quantiles <- quantile(data[[variable]], probs = c(1/3, 2/3), na.rm = TRUE)
  cat(paste("33.33% Quantiles for", variable, ":", "\n"))
  cat(paste("   ", variable, "<=", quantiles[1]), "\n")
  cat(paste("   ", quantiles[1], "<", variable, "<=", quantiles[2]), "\n")
  cat(paste("   ", variable, ">", quantiles[2]), "\n\n")
}

# Print quantiles for recency, frequency, and monetary value
print_quantiles(customers, "recency")
print_quantiles(customers, "frequency")
print_quantiles(customers, "amount")

# Define scoring breakpoints and scores
recency_breakpoints <- c(-Inf, 365, 1095, Inf)
frequency_breakpoints <- c(-Inf, 1.9, 2.1, Inf)
monetary_value_breakpoints <- c(-Inf, 28, 44, Inf)

recency_scores <- c(3, 2, 1)
frequency_scores <- c(1, 2, 3)
monetary_scores <- c(1, 2, 3)

# Assign scores based on custom scoring function
customers$recency_score <- score(customers$recency, recency_breakpoints, recency_scores)
customers$frequency_score <- score(customers$frequency, frequency_breakpoints, frequency_scores)
customers$amount_score <- score(customers$amount, monetary_value_breakpoints, monetary_scores)

# Create RFM column
customers$RFM <- customers$recency_score * 100 + customers$frequency_score * 10 + customers$amount_score

# Display the updated data
head(customers)

# Create a table with counts for each combination of RFM values
rfm_counts <- as.data.frame(table(customers$RFM))
colnames(rfm_counts) <- c("RFM", "Count")

# Display the table
print(rfm_counts)