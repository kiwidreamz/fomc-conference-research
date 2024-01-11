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
press_dates <- c("2020-01-29", "2020-03-03", "2020-03-15", "2020-04-29", "2020-06-10", "2020-07-29", "2020-09-16", "2020-11-05", "2020-12-16", "2021-01-27", "2021-03-17", "2021-04-28", "2021-06-16", "2021-07-28", "2021-09-22", "2021-11-03", "2021-12-15", "2022-01-26", "2022-03-16", "2022-04-27", "2022-06-15", "2022-07-27", "2022-09-21", "2022-11-02", "2022-12-14", "2023-01-25", "2023-03-15", "2023-04-26", "2023-06-14", "2023-07-26", "2023-09-20", "2023-11-01", "2023-12-13")
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


# Function to calculate sentiment scores sentence by sentence
calculate_sentiment_by_sentence <- function(text_list) {
  scores <- list()
  for (name in names(text_list)) {
    text <- text_list[[name]]
    sentiment_score <- sentiment_by(text, by = "sentence")
    scores[[name]] <- sentiment_score
  }
  return(scores)
}

# Calculate sentiment by sentence for each data structure
transcripts_sentiment_by_sentence <- calculate_sentiment_by_sentence(transcripts_list)
statements_sentiment_by_sentence <- calculate_sentiment_by_sentence(statements_list)
q_and_a_sentiment_by_sentence <- calculate_sentiment_by_sentence(q_and_a_list)
minutes_sentiment_by_sentence <- calculate_sentiment_by_sentence(minutes_list)


# Function to calculate overall sentiment scores for entire text
calculate_sentiment_overall <- function(text_list) {
  scores <- list()
  for (name in names(text_list)) {
    text <- text_list[[name]]
    sentiment_score <- sentiment(text)
    scores[[name]] <- sentiment_score
  }
  return(scores)
}

# Calculate overall sentiment overall for each data structure
transcripts_sentiment_overall <- calculate_sentiment_overall(transcripts_list)
statements_sentiment_overall <- calculate_sentiment_overall(statements_list)
q_and_a_sentiment_overall <- calculate_sentiment_overall(q_and_a_list)
minutes_sentiment_overall <- calculate_sentiment_overall(minutes_list)

head(transcripts_sentiment_overall["2023_pdf_8"])
head(transcripts_sentiment_by_sentence)

# Create a new data frame with the desired format for each meeting
years <- substr(names(sentiment_scores), 1, 4)
months <- substr(names(sentiment_scores), 6, 7)
transcripts_sentiment <- data.frame(
  DATE = as.Date(paste0(press_dates, "-01")),
  Sentiment_Score = as.numeric(sentiment_scores),
  stringsAsFactors = FALSE
)
print(transcripts_sentiment)

# Plotting sentiment evolution meeting by meeting 
colors <- colorRampPalette(c("red", "yellow", "green"))(length(transcripts_sentiment$Sentiment_Score))
color_index <- findInterval(transcripts_sentiment$Sentiment_Score, sort(unique(transcripts_sentiment$Sentiment_Score)))

plot(transcripts_sentiment$DATE, transcripts_sentiment$Sentiment_Score, type = "n", xlab = "Date", ylab = "Sentiment Score", main = "Sentiment Scores Across Meetings")
lines(transcripts_sentiment$DATE, transcripts_sentiment$Sentiment_Score, type = "o", col = "black", pch = 16, cex = 0.1)
points(transcripts_sentiment$DATE, transcripts_sentiment$Sentiment_Score, col = colors[color_index], pch = 16, cex = 1.5)

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






