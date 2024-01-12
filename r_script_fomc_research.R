rm(list = ls())
library(pdftools)
library(stringr)
library(tm)
library(sentimentr)
library(ggplot2)


################################
#IMPORTING AND PROCESSING
################################

# Directories paths
transcripts_directory <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/transcripts"
minutes_directory <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/minutes/txt"
years <- c("2020", "2021", "2022", "2023")
press_dates <- c("2020-01-29", "2020-03-03", "2020-03-15", "2020-04-29", "2020-06-10", "2020-07-29", "2020-09-16", "2020-11-05", "2020-12-16", "2021-01-27", "2021-03-17", "2021-04-28", "2021-06-16", "2021-07-28", "2021-09-22", "2021-11-03", "2021-12-15", "2022-01-26", "2022-03-16", "2022-05-04", "2022-06-15", "2022-07-27", "2022-09-21", "2022-11-02", "2022-12-14", "2023-02-01", "2023-03-22", "2023-05-03", "2023-06-14", "2023-07-26", "2023-09-20", "2023-11-01", "2023-12-13")
minutes_dates <- c("2020-02-19", "2020-04-08", "2020-05-20", "2020-07-01", "2020-08-19", "2020-10-07", "2020-11-25", "2021-01-06", "2021-02-17", "2021-04-07", "2021-05-19", "2021-07-07", "2021-08-18", "2021-10-13", "2021-11-24", "2022-01-05", "2022-02-16", "2022-04-06", "2022-05-25", "2022-07-06", "2022-08-17", "2022-10-12", "2022-11-23", "2023-01-04", "2023-02-22", "2023-04-12", "2023-05-24", "2023-07-05", "2023-08-16", "2023-10-11", "2023-11-22", "2024-01-03")
print(press_dates)

# List to store text from each file
transcripts_list <- list()
minutes_list <- list()

# Loop through each year's folder for transcripts
for (year in years) {
  # Path to the year's folder
  year_directory <- file.path(transcripts_directory, year)
  # List PDF files in the year's folder
  pdf_files <- list.files(year_directory, pattern = "\\.pdf$", full.names = TRUE)
  
  # Loop through each PDF file
  for (i in seq_along(pdf_files)) {
    # Extract text from each PDF file
    pdf_text <- pdf_text(pdf_files[i])
    
    # Remove line breaks and extra spaces
    pdf_text <- gsub("[\r\n\t]", " ", pdf_text)  # Remove line breaks, tabs, etc.
    pdf_text <- gsub("\\s+", " ", pdf_text)  # Remove multiple spaces
    pdf_text <- paste(pdf_text, collapse = " ")
    pdf_text <- substr(pdf_text, 100, nchar(pdf_text))
    
    # Conditionally remove the footer-like content
    pdf_text <- gsub("Page \\d+ of \\d+\\s+\\w+ \\d+, \\d+ Chair Powell’s Press Conference FINAL", "", pdf_text)
    pdf_text <- gsub("Page \\d+ of \\d+", "", pdf_text)
    
    pdf_text <- tolower(pdf_text) # Convert text to lowercase
    
    # Store text in the list, using the file name as the variable name
    transcripts_list[[paste0(year, "_pdf_", i)]] <- pdf_text
  }
}

print(transcripts_list[["2020_pdf_1"]])

# Initialize empty lists
statements_list <- list()
q_and_a_list <- list()

# Loop through each transcript
for (transcript_name in names(transcripts_list)) {
  transcript_text <- transcripts_list[[transcript_name]]
  
  # Find the position of "our questions" or "few questions"
  question_index <- regexpr("our questions|few questions", transcript_text, ignore.case = TRUE)
  
  # Check if "our questions" or "few questions" is found
  if (question_index == -1) {
    warning(paste("Markers 'our questions' or 'few questions' not found in:", transcript_name))
  } else {
    # Extract statement and Q&A based on the marker position
    statement_text <- substr(transcript_text, 1, question_index)
    q_and_a_text <- substr(transcript_text, question_index + attr(question_index, "match.length"), nchar(transcript_text))
    
    # Store in respective lists
    statements_list[[transcript_name]] <- statement_text
    q_and_a_list[[transcript_name]] <- q_and_a_text
  }
}

print(statements_list[["2020_pdf_1"]])
print(q_and_a_list[["2020_pdf_1"]])


# Loop through each year's folder for minutes
for (year in years) {
  # Path to the year's folder
  year_directory <- file.path(minutes_directory, year)
  # List text files in the year's folder
  text_files <- list.files(year_directory, pattern = "\\.txt$", full.names = TRUE)
  
  # Loop through each text file
  for (i in seq_along(text_files)) {
    # Read text from each text file
    text_content <- readLines(text_files[i], warn = FALSE)
    # Combine text lines into a single string
    txt_text <- paste(text_content, collapse = " ")
    # 
    # Remove line breaks and extra spaces
    txt_text <- gsub("[\r\n\t]", "", txt_text)  # Remove line breaks, tabs, etc.
    txt_text <- gsub("\\s+", " ", txt_text)  # Remove multiple spaces
    txt_text <- gsub("_", "", txt_text)  # Remove underscores
    txt_text <- gsub("https?://\\S+\\b", "", txt_text)  # Remove hyperlinks
    txt_text <- tolower(txt_text) # Convert text to lowercase
    
    # Find the index where the desired text starts ("Annual Organizational Matters" or "Developments in Financial Markets and Open Market Operations")
    start_index <- regexpr("annual organizational matters", txt_text, ignore.case = TRUE)
    
    # If the first mention is not found, check for the second mention
    if (start_index <= 0) {
      start_index <- regexpr("developments? in financial markets and open market operations", txt_text, ignore.case = TRUE)
    }
    
    # If the desired text is found, extract the text starting from that point
    if (start_index > 0) {
      txt_text <- substr(txt_text, start_index, nchar(txt_text))
    } else {
      # If the desired text is not found, keep the original text
      warning(paste("Specified text not found in the document:", text_files[i]))
    }
    
    # Store text in the list, using the file name as the variable name
    minutes_list[[paste0(year, "_txt_", i)]] <- txt_text
  }
}

print(minutes_list[["2020_txt_1"]])

# Define stopwords
stopwords_en <- stopwords("en")
print(stopwords_en)

# Create new lists to store text without stopwords
transcripts_list_nostopwords <- list()
minutes_list_nostopwords <- list()

# Loop through transcripts_list
for (key in names(transcripts_list)) {
  # Remove stopwords from transcripts
  processed_text <- paste(
    setdiff(
      unlist(strsplit(transcripts_list[[key]], "\\s+")),
      stopwords_en
    ),
    collapse = " "
  )
  
  # Store processed text in new list
  transcripts_list_nostopwords[[paste0(key, "_nostopwords")]] <- processed_text
}

# Loop through minutes_list
for (key in names(minutes_list)) {
  # Remove stopwords from minutes
  processed_text <- paste(
    setdiff(
      unlist(strsplit(minutes_list[[key]], "\\s+")),
      stopwords_en
    ),
    collapse = " "
  )
  
  # Store processed text in new list
  minutes_list_nostopwords[[paste0(key, "_nostopwords")]] <- processed_text
}
print(minutes_list_nostopwords[["2020_txt_1_nostopwords"]])

################################
#SENTIMENT ANALYSIS
################################


# Function to calculate sentiment
calculate_sentiment <- function(text_list) {
  scores <- list()
  for (name in names(text_list)) {
    text <- text_list[[name]]
    sentiment_score <- sentiment_by(text, by = "sentence")
    #sentiment_score <- sentiment(text)
    scores[[name]] <- sentiment_score
  }
  return(scores)
}

# Calculate sentiment by sentence for each data structure
transcripts_sentiment <- calculate_sentiment(transcripts_list)
statements_sentiment <- calculate_sentiment(statements_list)
q_and_a_sentiment <- calculate_sentiment(q_and_a_list)
minutes_sentiment <- calculate_sentiment(minutes_list)

transcripts_sentiment_scores <- sapply(transcripts_sentiment, function(x) x$ave_sentiment)
statements_sentiment_scores <- sapply(statements_sentiment, function(x) x$ave_sentiment)
q_and_a_sentiment_scores <- sapply(q_and_a_sentiment, function(x) x$ave_sentiment)
minutes_sentiment_scores <- sapply(minutes_sentiment, function(x) x$ave_sentiment)

# Create new data frames with the desired format 
# Transcripts
transcripts_sentiment <- data.frame(
  DATE = as.Date(paste0(press_dates, "-01")),
  Sentiment_Score = transcripts_sentiment_scores,
  stringsAsFactors = FALSE,
  row.names = NULL
)
print(transcripts_sentiment)
# Statements
statements_sentiment <- data.frame(
  DATE = as.Date(paste0(press_dates, "-01")),
  Sentiment_Score = statements_sentiment_scores,
  stringsAsFactors = FALSE,
  row.names = NULL
)
print(statements_sentiment)
# Q&As
q_and_a_sentiment <- data.frame(
  DATE = as.Date(paste0(press_dates, "-01")),
  Sentiment_Score = q_and_a_sentiment_scores,
  stringsAsFactors = FALSE,
  row.names = NULL
)
print(q_and_a_sentiment)
# Minutes
minutes_sentiment <- data.frame(
  DATE = as.Date(paste0(minutes_dates, "-01")),
  Sentiment_Score = minutes_sentiment_scores,
  stringsAsFactors = FALSE,
  row.names = NULL
)
# The third meeting of 2020 was an emergency one, hence why the second minutes should apply meetings 2 and 3 of 2020
new_row <- minutes_sentiment[2, ]
minutes_sentiment <- rbind(minutes_sentiment[1:2, ], new_row, minutes_sentiment[3:nrow(minutes_sentiment), ])
rownames(minutes_sentiment) <- NULL
print(minutes_sentiment)

# Plotting sentiment evolution meeting by meeting 
colors <- colorRampPalette(c("red", "yellow", "green"))(length(transcripts_sentiment$Sentiment_Score))
color_index <- findInterval(transcripts_sentiment$Sentiment_Score, sort(unique(transcripts_sentiment$Sentiment_Score)))

plot(transcripts_sentiment$DATE, transcripts_sentiment$Sentiment_Score, type = "n", xlab = "Date", ylab = "Sentiment Score", main = "Sentiment Scores Across Meetings")
lines(transcripts_sentiment$DATE, transcripts_sentiment$Sentiment_Score, type = "o", col = "black", pch = 16, cex = 0.1)
points(transcripts_sentiment$DATE, transcripts_sentiment$Sentiment_Score, col = colors[color_index], pch = 16, cex = 1.5)

# Plotting statement vs Q&A

# Difference
merged_data <- merge(statements_sentiment, q_and_a_sentiment, by = "DATE", suffixes = c("_statements", "_q_and_a"))
merged_data$DATE <- as.Date(merged_data$DATE)
merged_data$Sentiment_Difference <- merged_data$Sentiment_Score_q_and_a - merged_data$Sentiment_Score_statements
merged_data$Sentiment_Difference <- as.numeric(merged_data$Sentiment_Difference)

ggplot(merged_data, aes(x = DATE, y = Sentiment_Difference, color = Sentiment_Difference)) +
  geom_point(size = 4, shape = 18) +
  geom_line(linetype = "twodash", color = "wheat2") +
  scale_color_gradient(low = "red1", high = "springgreen", name = "Difference") +
  labs(title = "Scope of Difference in Sentiment between Q&A and Statements",
       x = "Date",
       y = "Sentiment Difference") +
  theme_minimal()

# Scope
statements_sentiment$ID <- seq_along(statements_sentiment$DATE)
q_and_a_sentiment$ID <- seq_along(q_and_a_sentiment$DATE)
q_and_a_sentiment$DifferentPoints <- FALSE 
selected_indices <- c(2, 3, 4, 5, 6, 7, 19, 20, 29, 30, 32)
q_and_a_sentiment$DifferentPoints[selected_indices] <- TRUE

combined_plot <- ggplot() +
  geom_point(data = statements_sentiment, aes(x = DATE, y = Sentiment_Score, color = "midnightblue"), shape = 18, size = 2) +
  geom_point(data = q_and_a_sentiment, aes(x = DATE, y = Sentiment_Score, color = DifferentPoints, fill = DifferentPoints), 
             shape = ifelse(q_and_a_sentiment$DifferentPoints, 24, 25),
             size = ifelse(q_and_a_sentiment$DifferentPoints, 3, 3)) +
  geom_segment(data = merge(statements_sentiment, q_and_a_sentiment, by = "ID"),
               aes(x = DATE.x, xend = DATE.y, y = Sentiment_Score.x, yend = Sentiment_Score.y),
               linetype = "dashed", color = "gray") +
  labs(title = "Sentiment Difference between Q&A compared to the Statement",
       x = "Date",
       y = "Sentiment Score") +
  scale_color_manual(values = c("TRUE" = "olivedrab2", "FALSE" = "tomato1"), guide = FALSE) + 
  theme_minimal()
print(combined_plot)

# Plotting statement vs minutes
statements_sentiment$ID <- seq_along(statements_sentiment$DATE)
minutes_sentiment$ID <- seq_along(minutes_sentiment$DATE)
minutes_sentiment$DifferentPoints <- FALSE 
selected_indices <- c(6, 17, 19, 20, 21, 22, 26, 30, 32)
minutes_sentiment$DifferentPoints[selected_indices] <- TRUE

combined_plot <- ggplot() +
  geom_point(data = statements_sentiment, aes(x = DATE, y = Sentiment_Score, color = "midnightblue"), shape = 18, size = 2) +
  geom_point(data = minutes_sentiment, aes(x = DATE, y = Sentiment_Score, color = DifferentPoints, fill = DifferentPoints), 
             shape = ifelse(minutes_sentiment$DifferentPoints, 24, 25),
             size = ifelse(minutes_sentiment$DifferentPoints, 3, 3)) +
  geom_segment(data = merge(statements_sentiment, minutes_sentiment, by = "ID"),
               aes(x = DATE.x, xend = DATE.y, y = Sentiment_Score.x, yend = Sentiment_Score.y),
               linetype = "twodash", color = "snow4") +
  labs(title = "Sentiment Difference between Statement and Minutes",
       x = "Date",
       y = "Sentiment Score") +
  scale_color_manual(values = c("TRUE" = "springgreen1", "FALSE" = "tomato3"), guide = FALSE) + 
  theme_minimal()
print(combined_plot)

################################
# PLOTTING INFLATION AND INFLATION EXPECTATIONS
################################

# CPI
file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/pce & cpi/CPIAUCSL.csv"
cpi <- read.csv(file_path)
print(cpi)
cpi$DATE <- as.Date(cpi$DATE)
plot(cpi$DATE, cpi$CPIAUCSL_PC1, type = "l", col = "orchid1", lwd = 2, xlab = "Date", ylab = "CPI Percent Change from a Year ago", main = "CPI from 2020 through 2023")

#PCE
file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/pce & cpi/PCEPILFE.csv"
pce <- read.csv(file_path)
pce$DATE <- as.Date(pce$DATE)
plot(pce$DATE, pce$PCEPILFE_PC1, type = "l", col = "mediumpurple4", lwd = 2, xlab = "Date", ylab = "Percent Change from a Year ago", main = "Core PCE from 2020 through 2023")

#Together (CPI & Core PCE)
plot(cpi$DATE, cpi$CPIAUCSL_PC1, type = "l", col = "orchid1", lwd = 2, xlab = "Date", ylab = "Percent Change from a Year ago", main = "CPI and Core PCE from 2020 through 2023")
lines(pce$DATE, pce$PCEPILFE_PC1, col = "mediumpurple4", lwd = 2)
legend("topright", legend = c("CPI", "Core PCE"), col = c("orchid1", "mediumpurple4"), lwd = 2)


#Inflation Expectations (2 Year and 10 Year)
file_path_2y <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/inflation expectations/EXPINF2YR.csv"
file_path_10y <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/inflation expectations/EXPINF10YR.csv"

ie2y <- read.csv(file_path_2y)
ie10y <- read.csv(file_path_10y)

ie2y$DATE <- as.Date(ie2y$DATE)
ie10y$DATE <- as.Date(ie10y$DATE)

plot(ie2y$DATE, ie2y$EXPINF2YR, type = "l", col = "lightskyblue", lwd = 2, xlab = "Date", ylab = "Percent", main = "2 Year and 10 Year Inflation Expectations")
lines(ie10y$DATE, ie10y$EXPINF10YR, col = "mediumblue", lwd = 2)


#All Together (Inflation and Expectations)
plot(cpi$DATE, cpi$CPIAUCSL_PC1, type = "l", col = "orchid1", lwd = 2, xlab = "Date", ylab = "Percent", main = "Inflation & Inflation Expectations")
lines(pce$DATE, pce$PCEPILFE_PC1, col = "mediumpurple4", lwd = 2)
lines(ie10y$DATE, ie10y$EXPINF10YR, col = "mediumblue", lwd = 2)
lines(ie2y$DATE, ie2y$EXPINF2YR, col = "lightskyblue", lwd = 2)

legend("topright", legend = c("CPI", "Core PCE", "2Y Expectations", "10Y Expectations"), col = c("orchid1", "mediumpurple4", "lightskyblue", "mediumblue"), lwd = 4)





