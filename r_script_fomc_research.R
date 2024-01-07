rm(list = ls())
library(pdftools)
library(stringr)


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
    
    # Remove line breaks and extra spaces
    txt_text <- gsub("[\r\n\t]", "", txt_text)  # Remove line breaks, tabs, etc.
    txt_text <- gsub("\\s+", " ", txt_text)  # Remove multiple spaces
    txt_text <- gsub("_", "", txt_text)  # Remove underscores
    txt_text <- gsub("https?://\\S+\\b", "", txt_text)  # Remove hyperlinks
    txt_text <- tolower(txt_text) # Convert text to lowercase
    
    # Store text in the list, using the file name as the variable name
    minutes_list[[paste0(year, "_txt_", i)]] <- txt_text
  }
}

print(minutes_list[["2020_txt_1"]])
