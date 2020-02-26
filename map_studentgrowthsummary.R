# Parse data from MAP Student Growth Summary Report pdfs

# Clear console
#cat("\014") 

# Clear memory
#rm(list=ls())
#gc()

# Install/load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, tidyr, dplyr, plyr, pdftools)

# File name
v_file_names <- choose.files(caption = "Select All Student Growth Summary Files:")
               
# Define Column headers
v_headers <- c("Grade", "Growth Count",
               "Initial Mean RIT", "Initial SD", "Initial Percentile", # Initial test data
               "Post Mean RIT", "Post SD", "Post Percentile", # Post test data 
               "Observed Growth", "Observed Growth SE", # Growth data
               "Project Growth", "School Conditional Growth Index", "School Conditional Growth Percentile", # School norms
               "Count with Projection", "Count Met Projection", "Percent Met Projection", "Student Median Conditional Growth Percentile") # Student norms

# Initialize lists for text and data
l_pdf_text <- list()
l_pdf_data <- list()

# Read text from pdf
for (i in 1:length(v_file_names)) {
  l_pdf_text[[i]] <- pdf_text(v_file_names[i]) %>% read_lines()
}

# Store grade level data into data frame
for (i in 1:length(l_pdf_text)) {
  
  l_pdf_data[[i]] <-  l_pdf_text[[i]] %>%
    
    # Remove extra whitepace
    str_squish() %>%
    
    # Split string into vector
    strsplit(split = " ") %>%
    
    # Keep rows with grade level data
    keep(~ .x[1] %in% c("PK", "K", "1", "2","3", "4", "5", "6", "7", "8")) %>%
    
    # Each vector needs to be the same length in order to convert to data frame
    lapply(FUN = function(x) {
      
      # Length needed for each vector
      z = length(v_headers)
      
      if(length(x) < z) {
        
        # If vectors don't have enough columns then append values
        v <- append(x, rep("**", z - length(x)))
        
      } else {
        
        # If vector has correct number of columns no change needed
        v <- x
        
      }
      
      return(v)
      
    }) %>%
    
    # Save as data frame
    ldply() %>%
    
    # Add variable to indicate growth period
    mutate("Growth Period" = substring(v_file_names[i] , 71, 86)) %>%
    
    # Replace * with NA values
    mutate_all(~ na_if(., "**"))
}

# Convert list into data frame
df_pdf_data <- l_pdf_data %>%
  
  ldply() %>%
  
  # Rename columns
  rename_at(vars(-"Growth Period"), function(x) v_headers)

# Reshape data into tidy data set



   