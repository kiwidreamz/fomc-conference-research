rm(list = ls())
library(pdftools)
library(stringr)
library(tm)
library(sentimentr)
library(ggplot2)
library(fuzzyjoin)
library(dplyr)

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
selected_indices <- c(2, 3, 4, 5, 6, 7, 19, 20, 29, 30, 32) # manually input positive difference for graphical purposes
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

# Plotting meetings vs minutes
transcripts_sentiment$ID <- seq_along(transcripts_sentiment$DATE)
minutes_sentiment$ID <- seq_along(minutes_sentiment$DATE)
minutes_sentiment$DifferentPoints <- FALSE 
selected_indices <- c(10, 11, 13, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 30) # manually input positive difference for graphical purposes
minutes_sentiment$DifferentPoints[selected_indices] <- TRUE

combined_plot <- ggplot() +
  geom_point(data = transcripts_sentiment, aes(x = DATE, y = Sentiment_Score, color = "midnightblue"), shape = 18, size = 2) +
  geom_point(data = minutes_sentiment, aes(x = DATE, y = Sentiment_Score, color = DifferentPoints, fill = DifferentPoints), 
             shape = ifelse(minutes_sentiment$DifferentPoints, 24, 25),
             size = ifelse(minutes_sentiment$DifferentPoints, 3, 3)) +
  geom_segment(data = merge(transcripts_sentiment, minutes_sentiment, by = "ID"),
               aes(x = DATE.x, xend = DATE.y, y = Sentiment_Score.x, yend = Sentiment_Score.y),
               linetype = "twodash", color = "snow4") +
  labs(title = "Sentiment Difference between Transcripts and Minutes",
       x = "Date",
       y = "Sentiment Score") +
  scale_color_manual(values = c("TRUE" = "springgreen1", "FALSE" = "tomato3"), guide = FALSE) + 
  theme_minimal()
print(combined_plot)

# Difference
transcripts_sentiment$ID <- seq_along(transcripts_sentiment$DATE)
minutes_sentiment$ID <- seq_along(minutes_sentiment$DATE)
merged_data2 <- merge(transcripts_sentiment, minutes_sentiment, by = "ID", suffixes = c("_transcripts", "_minutes"))
#merged_data2$DATE <- as.Date(merged_data2$DATE)
merged_data2$Sentiment_Difference <- merged_data2$Sentiment_Score_transcripts - merged_data2$Sentiment_Score_minutes
merged_data2$Sentiment_Difference <- as.numeric(merged_data2$Sentiment_Difference)

ggplot(merged_data2, aes(x = transcripts_sentiment$DATE, y = Sentiment_Difference, color = Sentiment_Difference)) +
  geom_point(size = 4, shape = 18) +
  geom_line(linetype = "twodash", color = "wheat2") +
  scale_color_gradient(low = "red1", high = "springgreen", name = "Difference") +
  labs(title = "Scope of Difference in Sentiment between Transcripts and Minutes",
       x = "Date",
       y = "Sentiment Difference") +
  theme_minimal()

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


################################
# PLOTTING FED FUNDS RATES
################################

file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/interest rates/FEDFUNDS.csv"
fed_funds_rate <- read.csv(file_path)
fed_funds_rate$DATE <- as.Date(fed_funds_rate$DATE)
plot(fed_funds_rate$DATE, fed_funds_rate$FEDFUNDS, type = "l", col = "royalblue", lwd = 3, xlab = "Date", ylab = "Percent", main = "Fed Funds Effective Rate")


################################
# CALCULATING CORRELATION
################################


new_entry <- data.frame(DATE = "2023-12-01", PCEPILFE_PC1 = 3.08504) # adding latest figure which wasn't available when i downloaded the pce figures
pce_corr <- rbind(pce, new_entry)


transcripts_sentiment_corr

cpi
pce
ie2y
ie10y
fed_funds_rate

# Convert DATE columns to Date class
transcripts_sentiment_corr <- transcripts_sentiment
transcripts_sentiment_corr$DATE <- format(transcripts_sentiment_corr$DATE, "%Y-%m")
cpi$DATE <- format(cpi$DATE, "%Y-%m")
pce_corr$DATE <- format(pce_corr$DATE, "%Y-%m")
ie2y$DATE <- format(ie2y$DATE, "%Y-%m")
ie10y$DATE <- format(ie10y$DATE, "%Y-%m")
fed_funds_rate$DATE <- format(fed_funds_rate$DATE, "%Y-%m")

# Merge data structures based on the DATE column
merged_data <- merge(transcripts_sentiment_corr, cpi, by = "DATE", all.x = TRUE)
merged_data <- merge(merged_data, pce_corr, by = "DATE", all.x = TRUE)
merged_data <- merge(merged_data, ie2y, by = "DATE", all.x = TRUE)
merged_data <- merge(merged_data, ie10y, by = "DATE", all.x = TRUE)
merged_data <- merge(merged_data, fed_funds_rate, by = "DATE", all.x = TRUE)
dim(merged_data)

# Calculate correlation
cor_cpi <- cor(merged_data$Sentiment_Score, merged_data$CPIAUCSL_PC1)
cor_pce <- cor(merged_data$Sentiment_Score, merged_data$PCEPILFE_PC1)
cor_ie2y <- cor(merged_data$Sentiment_Score, merged_data$EXPINF2YR)
cor_ie10y <- cor(merged_data$Sentiment_Score, merged_data$EXPINF10YR)
cor_fed_funds <- cor(merged_data$Sentiment_Score, merged_data$FEDFUNDS)

cat( "\n",
  "Transcript Sentiment Correlation with CPI:", cor_cpi, "\n",
  "Transcript Sentiment Correlation with Core PCE:", cor_pce, "\n",
  "Transcript Sentiment Correlation with 2 Year Inflation Expectations:", cor_ie2y, "\n",
  "Transcript Sentiment Correlation with 10 Year Inflation Expectations:", cor_ie10y, "\n",
  "Transcript Sentiment Correlation with Fed Funds Rate:", cor_fed_funds, "\n"
)


################################
# WORD FREQUENCY
################################

names(transcripts_list) <- press_dates
names(minutes_list) <- minutes_dates
transcripts_data <- data.frame(DATE = as.Date(names(transcripts_list), format = "%Y-%m-%d"), TEXT = unlist(transcripts_list), stringsAsFactors = FALSE)
minutes_data <- data.frame(DATE = as.Date(names(minutes_list), format = "%Y-%m-%d"), TEXT = unlist(minutes_list), stringsAsFactors = FALSE)


# Function to count word occurrence through transcripts and minutes
count_word_appearances <- function(transcripts_data, minutes_data, target_word) {
  transcripts_data$DATE <- as.Date(transcripts_data$DATE)
  minutes_data$DATE <- as.Date(minutes_data$DATE)
  
  combined_data <- rbind(transcripts_data, minutes_data)
  unique_dates <- unique(combined_data$DATE)
  
  word_counts <- data.frame(DATE = unique(combined_data$DATE), Transcripts_Appearances = 0, Minutes_Appearances = 0)
  
  # Looping through each date and counting word appearances
  for (i in seq_along(unique_dates)) {
    current_date <- unique_dates[i]
    
    transcripts_text <- transcripts_data[transcripts_data$DATE == current_date, "TEXT"]
    transcripts_text <- paste(transcripts_text, collapse = " ")
    
    minutes_text <- minutes_data[minutes_data$DATE == current_date, "TEXT"]
    minutes_text <- paste(minutes_text, collapse = " ")
    
    word_counts$Transcripts_Appearances[i] <- sum(str_count(tolower(transcripts_text), target_word))
    word_counts$Minutes_Appearances[i] <- sum(str_count(tolower(minutes_text), target_word))
  }
  
  return(word_counts)
}

# Get counts for inflation
inflation_counts <- count_word_appearances(transcripts_data, minutes_data, "inflation")
transcripts_counts <- inflation_counts[1:33, ]
minutes_counts <- inflation_counts[34:nrow(inflation_counts), ]

# Get counts for inflation expectations
inflation_expectations_counts <- count_word_appearances(transcripts_data, minutes_data, "inflation expectations")
transcripts_counts <- inflation_expectations_counts[1:33, ]
minutes_counts <- inflation_expectations_counts[34:nrow(inflation_expectations_counts), ]

# Get counts for disinflation
disinflation_counts <- count_word_appearances(transcripts_data, minutes_data, "disinflation")
transcripts_counts <- disinflation_counts[1:33, ]
minutes_counts <- disinflation_counts[34:nrow(disinflation_counts), ]

# Get counts for deflation
deflation_counts <- count_word_appearances(transcripts_data, minutes_data, "deflation")
transcripts_counts <- deflation_counts[1:33, ]
minutes_counts <- deflation_counts[34:nrow(deflation_counts), ]

# Get counts for transitory
transitory_counts <- count_word_appearances(transcripts_data, minutes_data, "transitory")
transcripts_counts <- transitory_counts[1:33, ]
minutes_counts <- transitory_counts[34:nrow(transitory_counts), ]

# Get counts for 2 percent
twopercent_counts <- count_word_appearances(transcripts_data, minutes_data, "2 percent")
transcripts_counts <- twopercent_counts[1:33, ]
minutes_counts <- twopercent_counts[34:nrow(twopercent_counts), ]

# Get counts for flexible average
flexible_counts <- count_word_appearances(transcripts_data, minutes_data, "flexible average")
transcripts_counts <- flexible_counts[1:33, ]
minutes_counts <- flexible_counts[34:nrow(flexible_counts), ]


# Get counts for unemployment
unemployment_counts <- count_word_appearances(transcripts_data, minutes_data, "unemployment")
transcripts_counts <- unemployment_counts[1:33, ]
minutes_counts <- unemployment_counts[34:nrow(unemployment_counts), ]

# Get counts for tight labor market
tlm_counts <- count_word_appearances(transcripts_data, minutes_data, "tight labor market|labor market remains tight")
transcripts_counts <- tlm_counts[1:33, ]
minutes_counts <- tlm_counts[34:nrow(tlm_counts), ]

# Get counts for wage–price spiral
spiral_counts <- count_word_appearances(transcripts_data, minutes_data, "wage–price spiral")
transcripts_counts <- spiral_counts[1:33, ]
minutes_counts <- spiral_counts[34:nrow(spiral_counts), ]


# Get counts for stimulus
stimulus_counts <- count_word_appearances(transcripts_data, minutes_data, "stimulus")
transcripts_counts <- stimulus_counts[1:33, ]
minutes_counts <- stimulus_counts[34:nrow(stimulus_counts), ]

# Get counts for taper 
taper_counts <- count_word_appearances(transcripts_data, minutes_data, "taper")
transcripts_counts <- taper_counts[1:33, ]
minutes_counts <- taper_counts[34:nrow(taper_counts), ]

# Get counts for hikes
hike_counts <- count_word_appearances(transcripts_data, minutes_data, "hik")
transcripts_counts <- hike_counts[1:33, ]
minutes_counts <- hike_counts[34:nrow(hike_counts), ]

# Get counts for rate increases 
increase_counts <- count_word_appearances(transcripts_data, minutes_data, "rate increase")
transcripts_counts <- increase_counts[1:33, ]
minutes_counts <- increase_counts[34:nrow(increase_counts), ]

# Get counts for raise rates
raise_counts <- count_word_appearances(transcripts_data, minutes_data, "raise interest rates|raise rates")
transcripts_counts <- raise_counts[1:33, ]
minutes_counts <- raise_counts[34:nrow(raise_counts), ]

# Get counts for peak
peak_counts <- count_word_appearances(transcripts_data, minutes_data, "peak")
transcripts_counts <- peak_counts[1:33, ]
minutes_counts <- peak_counts[34:nrow(peak_counts), ]

# Get counts for rate cuts
cut_counts <- count_word_appearances(transcripts_data, minutes_data, "cuts|rate cut|cutting")
transcripts_counts <- cut_counts[1:33, ]
minutes_counts <- cut_counts[34:nrow(cut_counts), ]

# Get counts for neutral
neutral_counts <- count_word_appearances(transcripts_data, minutes_data, "neutral")
transcripts_counts <- neutral_counts[1:33, ]
minutes_counts <- neutral_counts[34:nrow(neutral_counts), ]

# Get counts for real rate
real_rate_counts <- count_word_appearances(transcripts_data, minutes_data, "real rate")
transcripts_counts <- real_rate_counts[1:33, ]
minutes_counts <- real_rate_counts[34:nrow(real_rate_counts), ]

# Get counts for restrictive
restrictive_counts <- count_word_appearances(transcripts_data, minutes_data, "sufficiently restrictive")
transcripts_counts <- restrictive_counts[1:33, ]
minutes_counts <- restrictive_counts[34:nrow(restrictive_counts), ]

# Get counts for lag
lag_counts <- count_word_appearances(transcripts_data, minutes_data, "lag")
transcripts_counts <- lag_counts[1:33, ]
minutes_counts <- lag_counts[34:nrow(lag_counts), ]

# Time series plot (manually update title every time)
ggplot(transcripts_counts, aes(x = DATE)) +
  geom_line(aes(y = Transcripts_Appearances, color = "Transcripts"), size = 1) +
  geom_line(data = minutes_counts, aes(x = DATE, y = Minutes_Appearances, color = "Minutes"), size = 1) +
  labs(title = 'Word Frequency Over Time - "cuts|rate cut|cutting"',
       x = "Date",
       y = "Word Count") +
  scale_color_manual(values = c("Transcripts" = "mediumblue", "Minutes" = "seagreen1")) +
  theme_minimal()

################################
# UNEMPLOYMENT
################################

# UR
file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/UNRATE.csv"
ur <- read.csv(file_path)
print(ur)
ur$DATE <- as.Date(ur$DATE)
plot(ur$DATE, ur$UNRATE, type = "l", col = "salmon1", lwd = 2, xlab = "Date", ylab = "UNRATE", main = "Unemployment Rate")

# Correlation (UR vs U mentions)
unemployment_counts
ur

ur$DATE <- format(ur$DATE, "%Y-%m")
unemployment_counts$DATE <- format(unemployment_counts$DATE, "%Y-%m")
unemployment_counts$COUNTS <- unemployment_counts$Transcripts_Appearances + unemployment_counts$Minutes_Appearances
unemployment_counts <- unemployment_counts[, !(names(unemployment_counts) %in% c("Transcripts_Appearances", "Minutes_Appearances"))]
unemployment_counts <- unemployment_counts[order(unemployment_counts$DATE), ]

unique_dates <- unique(unemployment_counts$DATE)
combined_counts <- sapply(unique_dates, function(date) {
  sum(unemployment_counts$COUNTS[unemployment_counts$DATE == date])
})
combined_df <- data.frame(DATE = unique_dates, COUNTS = combined_counts)
rownames(combined_df) <- NULL
combined_df <- combined_df[combined_df$DATE != "2024-01", ]
combined_df

merged_unemployment <- merge(combined_df, ur, by = "DATE", all.x = TRUE)
cor_ur <- cor(merged_unemployment$COUNTS, merged_unemployment$UNRATE)

cat( "\n",
     "Unemployment Rate Correlation with Unemployment Counts:", cor_ur, "\n")

################################
# REAL RATE
################################

# RR
file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/interest rates/REAINTRATREARAT1YE.csv"
rr1 <- read.csv(file_path)
file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/interest rates/REAINTRATREARAT10Y.csv"
rr10 <- read.csv(file_path)
file_path <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/interest rates/REAINTRATREARAT1MO.csv"
rrm <- read.csv(file_path)

rr1$DATE <- as.Date(rr1$DATE)
rr10$DATE <- as.Date(rr10$DATE)
rrm$DATE <- as.Date(rrm$DATE)

plot(rr1$DATE, rr1$REAINTRATREARAT1YE, type = "l", col = "deepskyblue3", lwd = 2, xlab = "Date", ylab = "REAINTRATREARAT", main = "Real Rates", ylim = c(-8, 8))
lines(rr10$DATE, rr10$REAINTRATREARAT10Y, col = "forestgreen", lwd = 2)
lines(rr10$DATE, rrm$REAINTRATREARAT1MO, col = "firebrick1", lwd = 2)
abline(h = 0, col = "gray57", lty = 2)

legend("topleft", legend = c("1-Month", "1-Year",  "10-Year"), col = c("firebrick1", "deepskyblue3", "forestgreen"), lwd = 2)

################################
# COMPLETE CORPUS TEXT
################################

transcripts_data$DATE <- as.Date(transcripts_data$DATE)
minutes_data$DATE <- as.Date(minutes_data$DATE)

merged_data <- merge(transcripts_data, minutes_data, by = "DATE", all = TRUE)
merged_data <- transform(merged_data, complete_corpus = paste(TEXT.x, TEXT.y, sep = " "))
sorted_data <- merged_data[order(merged_data$DATE), ]
complete_corpus <- sorted_data[, c("DATE", "complete_corpus")]

writeLines(complete_corpus$complete_corpus, "complete_corpus.txt")


################################
# HYPOTHESIS TESTING
################################

# Transcripts vs Minutes
transcripts_subset <- transcripts_sentiment[c("ID", "Sentiment_Score")]
minutes_subset <- minutes_sentiment[c("ID", "Sentiment_Score")]
transcripts_vs_minutes <- merge(transcripts_subset, minutes_subset, by = "ID", suffixes = c("_transcripts", "_minutes"))
transcripts_vs_minutes$Absolute_Sentiment_Difference <- abs(transcripts_vs_minutes$Sentiment_Score_transcripts - transcripts_vs_minutes$Sentiment_Score_minutes)
t_test_result <- t.test(transcripts_vs_minutes$Absolute_Sentiment_Difference)

# Statements vs Minutes
statement_subset <- statements_sentiment[c("ID", "Sentiment_Score")]
minutes_subset <- minutes_sentiment[c("ID", "Sentiment_Score")]
statements_vs_minutes <- merge(statement_subset, minutes_subset, by = "ID", suffixes = c("_statements", "_minutes"))
statements_vs_minutes$Absolute_Sentiment_Difference <- abs(statements_vs_minutes$Sentiment_Score_statements - statements_vs_minutes$Sentiment_Score_minutes)
t_test_result <- t.test(statements_vs_minutes$Absolute_Sentiment_Difference)

# Statements vs Q&A
statement_subset <- statements_sentiment[c("ID", "Sentiment_Score")]
q_and_a_subset <- q_and_a_sentiment[c("ID", "Sentiment_Score")]
statements_vs_q_and_a <- merge(statement_subset, q_and_a_subset, by = "ID", suffixes = c("_statements", "_QA"))
statements_vs_q_and_a$Absolute_Sentiment_Difference <- abs(statements_vs_q_and_a$Sentiment_Score_statements - statements_vs_q_and_a$Sentiment_Score_QA)
t_test_result <- t.test(statements_vs_q_and_a$Absolute_Sentiment_Difference)

# Hike vs Increase & Raise
merged_data <- merge(merge(hike_counts, increase_counts, by = "DATE", suffixes = c("_hike", "_increase")), raise_counts, by = "DATE")

colnames(merged_data) <- c("DATE", "hike_t", "hike_m", "increase_t", "increase_m", "raise_t", "raise_m")
contingency_table <- table(merged_data$hike_t, merged_data$hike_m, merged_data$increase_t, merged_data$increase_m, merged_data$raise_t, merged_data$raise_m)
contingency_matrix <- as.matrix(contingency_table)
chi_squared_result <- chisq.test(contingency_matrix)

# Merging rows 4 and 5 which was an emergency meeting
merged_data[4, 2:7] <- merged_data[4, 2:7] + merged_data[5, 2:7]
merged_data <- merged_data[-5, ]
rownames(merged_data) <- NULL

# Combining minutes and transcripts dates
numeric_cols <- sapply(merged_data, is.numeric)
merged_data[is.na(merged_data)] <- 0
merged_data <- merged_data[, numeric_cols]
merged_data <- merged_data[, !grepl("DATE", names(merged_data))]
combined_data <- merged_data[1, ]

for (i in seq(2, nrow(merged_data), by = 2)) {
  combined_data <- rbind(combined_data, colSums(merged_data[i:(i + 1), ], na.rm = TRUE))
}

rownames(combined_data) <- seq_len(nrow(combined_data))
combined_data <- combined_data[-nrow(combined_data), ]

# Combining minutes and transcripts frequency counts
combined_data <- data.frame(
  hike = combined_data$hike_t + combined_data$hike_m,
  increase = combined_data$increase_t + combined_data$increase_m,
  raise = combined_data$raise_t + combined_data$raise_m
)

# Combine with data frame showing the meetings that saw an interest rate increase AT THE NEXT MEETING
next_meeting_rate_increase = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0)
combined_data$nextt <- next_meeting_rate_increase

# Counts
hike_count <- sum(combined_data$hike)
increase_count <- sum(combined_data$increase)
raise_count <- sum(combined_data$raise)

cat("Total count of 'hike':", sum(combined_data$hike), "\n",
    "Total count of 'increase':", sum(combined_data$increase), "\n",
    "Total count of 'raise':", sum(combined_data$raise), "\n")

# Calculate probabilities
prob_hike_positive_nextt <- sum(combined_data$hike[combined_data$nextt == 1]) / hike_count
prob_increase_positive_nextt <- sum(combined_data$increase[combined_data$nextt == 1]) / increase_count
prob_raise_positive_nextt <- sum(combined_data$raise[combined_data$nextt == 1]) / raise_count

cat("\n",
  "Probability of hike leading to higher rates at the next meeting:", prob_hike_positive_nextt, "\n",
  "Probability of increase leading to higher rates at the next meeting:", prob_increase_positive_nextt, "\n",
  "Probability of raise leading to higher rates at the next meeting:", prob_raise_positive_nextt, "\n"
)

sum(combined_data$hike[combined_data$nextt == 1])

# Average Percentage Increase
next_meet_rate_increase = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.25, 0.5, 0.75, 0.75, 0.75, 0.75, 0.5, 0.25, 0.25, 0.25, 0, 0.25, 0, 0, 0, 0)
combined_data$next_amount <- next_meet_rate_increase

prob_hike_amount <- sum(combined_data$hike * combined_data$next_amount) / hike_count
prob_increase_amount <- sum(combined_data$increase * combined_data$next_amount) / increase_count
prob_raise_amount <- sum(combined_data$raise * combined_data$next_amount) / raise_count

cat("\n",
    "Average rate increase following usage of the term hike:", prob_hike_amount, "\n",
    "Average rate increase following usage of the term increase:", prob_increase_amount, "\n",
    "Average rate increase following usage of the term raise:", prob_raise_amount, "\n"
)
