# Function to check and install packages
check_and_install <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# List of required packages
required_packages <- c( "dplyr", "bibliometrix", "writexl", "stringr")
check_and_install(required_packages)

# Set Working Directory
setwd("C:/Users/PC/Downloads/LCNC_dataSource")

# Define File Paths
scopus_base_path <- "scopus_base.csv"
scopus_review_path <- "scopus_review.csv"
wos_path <- "wos.bib"

# Load and Clean Data Function
load_and_clean_data <- function(file_path, dbsource, format) {
  if (file.exists(file_path)) {
    tryCatch({
      data <- convert2df(file_path, dbsource = dbsource, format = format)
      # Filter records published in or before 2024
      if ("PY" %in% colnames(data)) {  # Check if publication year column exists
        data <- data[data$PY <= 2024, ]  # Retain records with PY <= 2024
      } else {
        print("Warning: 'PY' column (Publication Year) not found in the dataset.")
      }
      data[is.na(data) | is.null(data) | data == "NA" | data == "" | data == " "] <- NA  # Replace blanks with NA
      print(paste("Loaded data from:", file_path, "with Rows =", dim(data)[1], "and Columns =", dim(data)[2]))
      return(data)
    }, error = function(e) {
      print(paste("Error loading file:", file_path))
      print(e)
      return(NULL)
    })
  } else {
    print(paste("File not found:", file_path))
    return(NULL)
  }
}

scopus_base_data <- load_and_clean_data(scopus_base_path, "scopus", "csv")
scopus_review_data <- load_and_clean_data(scopus_review_path, "scopus", "csv")
wos_data <- load_and_clean_data(wos_path, "wos", "bibtex")

# Merge Data
merged_data <- tryCatch({
  data <- mergeDbSources(scopus_base_data, scopus_review_data, wos_data)
  print(paste("Merged Data Dimensions: Rows =", dim(data)[1], ", Columns =", dim(data)[2]))
  data
}, error = function(e) {
  print("Error merging data")
  print(e)
  NULL
})


# Normalization Replacements List
replacements <- list(
  "A" = c("Á", "À", "Â", "Ã", "Å", "Ä", "Ā", "Ă", "Ą"),
  "AE" = c("Æ"),
  "B" = c("Б", "Β"),
  "C" = c("Ć", "Ç", "Č", "Ĉ", "Ċ"),
  "D" = c("Ð", "Ď", "Đ"),
  "E" = c("É", "È", "Ê", "Ë", "Ē", "Ĕ", "Ė", "Ę", "Ě"),
  "G" = c("Ģ", "Ğ", "Ġ", "Ĝ"),
  "H" = c("Ĥ", "Ħ"),
  "I" = c("Í", "Ì", "Î", "Ï", "Ĩ", "Ī", "Ĭ", "Į", "İ"),
  "L" = c("Ł", "Ľ", "Ĺ", "Ļ", "Ŀ"),
  "M" = c("M"),
  "N" = c("Ń", "Ň", "Ņ", "Ŋ", "Ñ"),
  "OE" = c("œ"),
  "O" = c("Ó", "Ò", "Ô", "Õ", "Ö", "Ø", "Ō", "Ŏ", "Ő"),
  "PLAT" = c("PLATFORM"),
  "R" = c("Ŕ", "Ř", "Ŗ"),
  "S" = c("Ś", "Š", "Ș", "Ŝ", "Ş"),
  "SS" = c("ß"),
  "T" = c("Ť", "Ț", "Ţ", "Ŧ"),
  "TH" = c("Þ"),
  "U" = c("Ú", "Ù", "Û", "Ü", "Ũ", "Ū", "Ŭ", "Ů", "Ű", "Ų"),
  "Y" = c("Ý", "Ÿ", "Ŷ"),
  "Z" = c("Ž", "Ž", "Ź", "Ż", "Ž"),
  "-" = c("–", "—")
)

# Function to Normalize Author Names
normalize_authors <- function(author_name, replacements) {
  for (key in names(replacements)) {
    for (char in replacements[[key]]) {
      author_name <- gsub(char, key, author_name, fixed = TRUE)
    }
  }

  return(trimws(author_name))
}

# Function to Normalize publication venues
normalize_publication_venues <- function(journal_name) {
  # Replace all & variations with "AND"
  journal_name <- gsub("&|\\\\&|\\&", "AND", journal_name) 
  # Replace hyphens and underscores with space
  journal_name <- gsub("-|–|—|_", " ", journal_name) 
  # Remove leading/trailing spaces
  return(trimws(journal_name))                            
}

# Abbreviation Replacement List
abbreviation_dict <- list(
  "AI" = "ARTIFICIAL INTELLIGENCE",
  "AI/ML" = "ARTIFICIAL INTELLIGENCE AND MACHINE LEARNING",
  "API" = "APPLICATION PROGRAMMING INTERFACES",
  "APP" = "APPLICATION",
  "BDD" = "BASE DE DONNEES",
  "BPT" = "BUSINESS PROCESS TECHNOLOGY",
  "CI/CD" = "CONTINUOUS INTEGRATION AND CONTINUOUS DEPLOYMENT",
  "CUI" = "CONVERSATIONAL USER INTERFACE",
  "DEMO" = "DEMONSTRATION",
  "DEI" = "DATA ENGINEERING AND INTEGRATION",
  "DEVMODEL" = "DEVELOPMENT MODEL",
  "DSL" = "DOMAIN SPECIFIC LANGUAGE",
  "DSLS" = "DOMAIN SPECIFIC LANGUAGE SYSTEMS",
  "ERP" = "ENTERPRISE RESOURCE PLANNING",
  "GUI" = "GRAPHICAL USER INTERFACE",
  "HMI" = "HUMAN MACHINE INTERFACE",
  "IDE" = "INTEGRATED DEVELOPMENT ENVIRONMENT",
  "IOT" = "INTERNET OF THINGS",
  "LCAP" = "LOW CODE APPLICATION PLATFORM",
  "LCNC" = "LOW CODE AND NO CODE",
  "LLM" = "LARGE LANGUAGE MODEL",
  "LCDP" = "LOW CODE DEVELOPMENT PLATFORM",
  "MDE" = "MODEL DRIVEN ENGINEERING",
  "MDA" = "MODEL DRIVEN ARCHITECTURE",
  "MDSD" = "MODEL DRIVEN SOFTWARE DEVELOPMENT",
  "MDD" = "MODEL DRIVEN DEVELOPMENT",
  "ML" = "MACHINE LEARNING",
  "PLAT" = "PLATFORM",
  "RDF" = "RESOURCE DESCRIPTION FRAMEWORK",
  "SE" = "SOFTWARE ENGINEERING",
  "SPI" = "SOFTWARE PROCESS IMPROVEMENT",
  "SUS" = "SYSTEM USABILITY SCALE",
  "TOE" = "TECHNOLOGY AND ORGANIZATION AND ENVIRONMENT",
  "USE" = "USEFULNESS",
  "UML" = "UNIFIED MODELING LANGUAGE" ,
  "SYSML" = "SYSTEM MODELING LANGUAGE",
  "XML" = "EXTENSIBLE MARKUP LANGUAGE",
  "SDGs" = "SUSTAINABLE DEVELOPMENT GOALS"
)
# Function to Normalize Keywords
normalize_keywords <- function(keywords, abbreviation_dict) {
  # Check if keywords  are missing
  if ((is.na(keywords) || keywords == "")) {
    return(NA)  
  }
  
  # Split the keywords into a vector (assuming they are separated by ";")
  keyword_list <- unlist(strsplit(keywords, ";"))
  
  # Normalize each keyword
  normalized_keywords <- sapply(keyword_list, function(keyword) {
    keyword <- trimws(keyword)  # Remove leading/trailing spaces
    # Remove parentheses and their content
    keyword <- gsub("\\s*\\([^)]*\\)", "", keyword)
    # Check if the keyword matches an abbreviation
    if (keyword %in% names(abbreviation_dict)) {
      return(abbreviation_dict[[keyword]])  # Replace with full definition
    }
    return(keyword)  # Keep as is if no match
  })
  
  # Rejoin the normalized keywords into a single string
  normalized_keywords <- paste(normalized_keywords, collapse = "; ")
  
  # Replace all & variations with "AND"
  normalized_keywords <- gsub("&|\\\\&|\\&", "AND", normalized_keywords)
  # Replace hyphens with spaces
  normalized_keywords <- gsub("-|–|—|_", " ", normalized_keywords)
  
  return(trimws(normalized_keywords))  # Return cleaned and normalized keywords
}

normalize_affiliations <- function(affiliations) {
  if (is.null(affiliations) || all(is.na(affiliations))) {
    return(affiliations)
  }
  
  # Replace common abbreviations and handle special characters
  replacements <- list(
    "\\bFED\\b" = "FEDERAL",
    "\\bFAC\\b" = "FACULTY",
    "\\bUNIV\\b" = "UNIVERSITY",
    "\\bINST\\b" = "INSTITUTE",
    "\\bDEPT\\b" = "DEPARTMENT",
    "\\bFAC\\b" = "FACULTY",
    "\\bTECH\\b" = "TECHNOLOGY",
    "\\bTECHNOL\\b" = "TECHNOLOGY",
    "\\bSCI\\b" = "SCIENCE",
    "\\bENG\\b" = "ENGINEERING",
    "\\bRES\\b" = "RESEARCH",
    "\\bCTR\\b" = "CENTER",
    "\\bLAB\\b" = "LABORATORY",
    "\\bSCH\\b" = "SCHOOL",
    "\\bNAT\\b" = "NATIONAL",
    "\\bINTL\\b" = "INTERNATIONAL",
    "\\bCOLL\\b" = "COLLEGE",
    "\\bAPPL\\b" = "APPLIED",
    "&" = "AND",
    "\\\\" = "",   # Remove backslashes
    "-" = " ",     # Replace dashes with space
    "_" = " "      # Replace underscores with space
  )
  
  # Function to normalize a single affiliation
  normalize_single_affiliation <- function(aff) {
    normalized_aff <- aff
    
    # Replace abbreviations
    for (pattern in names(replacements)) {
      normalized_aff <- gsub(pattern, replacements[[pattern]], normalized_aff, ignore.case = TRUE)
    }
    
    # Add "OF" after specific abbreviations when they are followed by another word
    normalized_aff <- gsub(
      "\\b(UNIV|INST|DEPT|FAC)\\b\\s+(\\w+)",  # Match abbreviations followed by a word
      "\\1 OF \\2",                           # Add "OF" between them
      normalized_aff,
      ignore.case = TRUE
    )
    
    # Clean up extra spaces
    normalized_aff <- gsub("\\s+", " ", normalized_aff)  # Remove extra spaces
    normalized_aff <- trimws(normalized_aff)  # Trim leading and trailing spaces
    return(normalized_aff)
  }
  
  # Split affiliations by `;`, normalize each, and rejoin
  normalize_affiliation_cell <- function(cell) {
    if (is.na(cell) || cell == "") return(NA)
    affiliations_split <- strsplit(cell, ";")[[1]]
    normalized_affiliations <- sapply(affiliations_split, normalize_single_affiliation)
    return(paste(normalized_affiliations, collapse = "; "))
  }
  
  # Apply the normalization to each row
  normalized_affiliations <- sapply(affiliations, normalize_affiliation_cell)
  return(normalized_affiliations)
}

# Function to check and exclude incomplete records
handle_incomplete_records <- function(data, critical_fields) {
  if (is.null(data) || dim(data)[1] == 0) {
    print("Dataset is NULL or empty. No processing done.")
    return(NULL)
  }
  
  # Check if all critical fields exist in the dataset
  missing_fields <- setdiff(critical_fields, colnames(data))
  if (length(missing_fields) > 0) {
    print(paste("The following critical fields are missing from the dataset:", paste(missing_fields, collapse = ", ")))
    stop("Cannot proceed without all critical fields.")
  }
  
  
  
  # Identify rows with missing or invalid critical fields
  incomplete_rows <- which(apply(data[, critical_fields, drop = FALSE], 1, function(row) {
    any(is.na(row) | row == "" | row == "NA" | is.null(row))
  }))
  
  if (length(incomplete_rows) > 0) {
    print(paste("Number of incomplete records identified:", length(incomplete_rows)))
    
    # Display missing counts for each critical field
    print("Missing values for each critical field:")
    for (field in critical_fields) {
      missing_count <- sum(is.na(data[[field]]) | data[[field]] == "" | data[[field]] == "NA" | is.null(data[[field]]))
      print(paste(field, ":", missing_count, "missing values"))
    }
    
    # Save the incomplete records for review
    incomplete_records <- data[incomplete_rows, ]
    #write.csv(incomplete_records, "incomplete_records.csv", row.names = FALSE)
    write_xlsx(incomplete_records, path = "incomplete_records.xlsx")
    print("Incomplete records saved to 'incomplete_records.xlsx'.")
    
    # Remove incomplete records from the dataset
    data <- data[-incomplete_rows, , drop = FALSE]
    print(paste("Remaining records after removal:", dim(data)[1]))
  } else {
    print("No incomplete records found.")
  }
  
  return(data)
}


# Fonction pour normaliser les noms des auteurs dans le champ CR uniquement

process_references <- function(reference_string) {
  # Diviser les références en utilisant ";" comme séparateur
  references <- unlist(strsplit(reference_string, ";"))
  
  # Fonction pour normaliser une référence
  normalize_reference <- function(ref) {
    # Supprimer les espaces en début et fin
    ref <- trimws(ref)
    # Supprimer les points après les initiales des noms
    ref <- gsub("([A-Z])\\.", "\\1", ref)
    return(ref)
  }
  
  # Appliquer la normalisation à chaque référence
  normalized_references <- sapply(references, normalize_reference)
  
  # Reconstituer les références normalisées en une chaîne unique
  processed_string <- paste(normalized_references, collapse = "; ")
  return(processed_string)
}

# Modification de la fonction handle_column_renaming_and_normalization
handle_column_renaming_and_normalization <- function(data, replacements) {
  if ("CR_raw" %in% colnames(data)) {
    if ("CR" %in% colnames(data)) {
      # Move data from CR_raw to CR if CR exists
      data$CR <- ifelse(is.na(data$CR) | data$CR == "" | data$CR == "NA",
                        data$CR_raw,
                        data$CR)
      # Remove CR_raw after merging
      data$CR_raw <- NULL
      print("Data from CR_raw has been moved to CR, and CR_raw has been removed.")
    } else {
      # Rename CR_raw to CR if CR does not exist
      colnames(data)[colnames(data) == "CR_raw"] <- "CR"
      print("Column CR_raw has been renamed to CR.")
    }
    # Normaliser les noms des auteurs uniquement dans CR
    data$CR <- sapply(data$CR, process_references)
    print("Normalized author names in CR.")
  } else {
    print("Column CR_raw not found in the dataset.")
  }
  
  if ("SC" %in% colnames(data)) {
    # Rename SC to WC
    colnames(data)[colnames(data) == "SC"] <- "WC"
    print("Column SC has been renamed to WC.")
  } else {
    print("Column SC not found in the dataset.")
  }
  
  return(data)
}
# Function to identify and remove empty columns
remove_empty_columns <- function(data) {
  if (is.null(data) || dim(data)[1] == 0) {
    print("Dataset is NULL or empty. No processing done.")
    return(data)
  }
  
  # Identify empty columns
  empty_columns <- colnames(data)[colSums(!is.na(data) & data != "") == 0]
  
  if (length(empty_columns) > 0) {
    # Remove empty columns
    data <- data[, colSums(!is.na(data) & data != "") > 0]
    
    # Print the names of removed columns
    print("The following empty columns were removed:")
    print(empty_columns)
  } else {
    print("No empty columns were found.")
  }
  
  return(data)
}






if (!is.null(merged_data)) {
  
  # Apply Author Names Normalization
  if ("AU" %in% colnames(merged_data)) {
    merged_data$AU <- sapply(merged_data$AU, normalize_authors, replacements)
    print("Author names have been normalized.")
  } else {
    print("Author column (AU) not found in the dataset.")
  }
  
  # Apply Keyword Normalization
  if ("DE" %in% colnames(merged_data) ) {
    merged_data$DE <- mapply(
      normalize_keywords, 
      keywords = merged_data$DE, 
      MoreArgs = list(abbreviation_dict = abbreviation_dict)
    )
    print("Keywords have been normalized.")
  } else {
    print("Keywords columns DE  not found in the dataset.")
  }
  if ( "ID" %in% colnames(merged_data)) {
    merged_data$ID <- mapply(
      normalize_keywords, 
      keywords = merged_data$ID, 
      MoreArgs = list(abbreviation_dict = abbreviation_dict)
    )
    print("Keywords have been normalized.")
  } else {
    print("Keywords columns ID not found in the dataset.")
  }
  
  
  # Apply Source Normalization 
  if ("SO" %in% colnames(merged_data)) {
    merged_data$SO <- sapply(merged_data$SO, normalize_publication_venues)
    print("Source names have been normalized.")
  } else {
    print("Source column (SO) not found in the dataset.")
  }
  if ("C1" %in% colnames(merged_data)) {
    merged_data$C1 <- sapply(merged_data$C1, normalize_authors, replacements)
    merged_data$C1 <- normalize_affiliations(merged_data$C1)
    print("Affiliations have been normalized, including multiple entries per cell.")
  } else {
    print("Affiliation column 'C1' not found in the dataset.")
  }
  merged_data <- handle_column_renaming_and_normalization(merged_data, replacements)
  merged_data <- remove_empty_columns(merged_data)
 
  # Define critical fields
  critical_fields <- c("TI", "PY", "AU", "SO", "CR","C1","DT")  
  
  # Handle incomplete records
  merged_data <- handle_incomplete_records(merged_data, critical_fields)
}



# Export Data
if (!is.null(merged_data) && nrow(merged_data) > 0) {
  cat("Select the output format (CSV, Excel, RDS): ")
  output_format <- tryCatch({
    toupper(readline())
  }, error = function(e) {
    print("Error reading input. Defaulting to CSV.")
    "CSV"
  })
  
  tryCatch({
    if (output_format == "CSV") {
      write.csv(merged_data, file = "dataSet_output.csv", row.names = FALSE)
      print("Dataset saved as CSV.")
    } else if (output_format == "EXCEL") {
      write_xlsx(merged_data, path = "dataSet_output2.xlsx")
      print("Dataset saved as Excel.")
    } else if (output_format == "RDS") {
      saveRDS(merged_data, file = "dataSet_output.rds")
      print("Dataset saved as RDS.")
    } else {
      print("Invalid format. No file was saved.")
    }
  }, error = function(e) {
    print("Error saving dataset.")
    print(e)
  })
} else {
  print("Merged dataset is empty. No data to export.")
}
