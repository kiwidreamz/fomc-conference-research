rm(list = ls())
library(pdftools)
library(stringr)
library(tm)


# Directories paths
transcripts_directory <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/transcripts"
minutes_directory <- "C:/Users/STEPHANE/Documents/fomc-conference-research/data/minutes/txt"
years <- c("2020", "2021", "2022", "2023")

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



