#NLP
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(widyr)
library(readxl)

# keyword recognition
#base$text comprises all relevant qualitative information

base=df  %>% 
  mutate(text=paste(df$ShortDescription,df$Objectives1,df$Objectives2,df$Objectives3,df$Objectives4,df$Objectives5,df$Objectives6, df$Background))%>% 
  select(InitiativeID, NameEnglish, CountryLabel,text, international, wicked)

# --- 1. Import keyword list and select relevant columns ---
keywords <- read_excel("FILENAME") %>%
  select(1:6)

# --- 2. Function to convert search terms into strict regex patterns ---
convert_to_regex <- function(term, exclude = NA) {
  if (is.na(term) || term == "") return(NA)
  
  # Split by OR
  parts <- unlist(strsplit(term, "\\s+OR\\s+", perl = TRUE))
  
  # Convert each part
  regex_parts <- lapply(parts, function(p) {
    p <- gsub("\\*", "\\\\w*", p)
    p <- gsub("\\s+", " ", p)
    words <- strsplit(p, " ", fixed = TRUE)[[1]]
    if (length(words) > 1) {
      p <- paste0("\\b", paste(words, collapse = "\\s{1}"), "\\b")
    } else {
      p <- paste0("\\b", words, "\\b")
    }
    return(p)
  })
  
  # Join parts with |
  pattern <- paste(regex_parts, collapse = "|")
  
  # Add exclusion if present
  if (!is.na(exclude) && exclude != "") {
    exclude_parts <- unlist(strsplit(exclude, "\\s+OR\\s+", perl = TRUE))
    exclude_regex <- lapply(exclude_parts, function(e) {
      e <- gsub("\\*", "\\\\w*", e)
      e <- gsub("\\s+", " ", e)
      words <- strsplit(e, " ", fixed = TRUE)[[1]]
      if (length(words) > 1) {
        e <- paste0(paste(words, collapse = "\\s{1}"))
      }
      return(e)
    })
    # Negative lookahead added before the match
    neg_lookahead <- paste0("(?!.*(", paste(exclude_regex, collapse = "|"), "))")
    pattern <- paste0(neg_lookahead, "(", pattern, ")")
  }
  
  return(paste0("(?i)", pattern))
}
# --- 3. Generate regex columns in the keyword table ---
keywords <- keywords %>%
  mutate(
    term_regex = mapply(convert_to_regex, term, exclude, SIMPLIFY = TRUE),
    and_regex = sapply(AND, convert_to_regex)
  )

# --- 4. Get unique policy orientation categories ---
policy_categories <- unique(keywords$policy_orientation)

# --- 5. Loop through each policy_orientation and create binary match columns in base ---
for (policy in policy_categories) {
  policy_terms <- keywords %>%
    filter(policy_orientation == policy) %>%
    select(term_regex, and_regex)
  
  check_policy_match <- function(text) {
    term_matches <- sapply(1:nrow(policy_terms), function(i) {
      term_match <- str_detect(text, policy_terms$term_regex[i])
      if (!is.na(policy_terms$and_regex[i])) {
        and_match <- str_detect(text, policy_terms$and_regex[i])
        return(term_match & and_match)
      }
      return(term_match)
    })
    return(any(term_matches))
  }
  
  base[[paste0("policy_", policy)]] <- as.integer(sapply(base$text, check_policy_match))
}

# --- 6. Create a global binary flag for any policy match ---
base <- base %>%
  mutate(policy_or = as.integer(if_any(starts_with("policy_"), ~ . == 1)))

# --- 7. Create matched_df to track which sub_concepts match for each text ---
matched_df <- base %>% select(InitiativeID:policy_or)

for (i in 1:nrow(keywords)) {
  sub_concept_name <- keywords$sub_concept[i]
  term_regex <- keywords$term_regex[i]
  and_regex  <- keywords$and_regex[i]
  
  matched_df[[sub_concept_name]] <- sapply(base$text, function(text) {
    term_match <- str_detect(text, term_regex)
    if (!is.na(and_regex)) {
      and_match <- str_detect(text, and_regex)
      return(as.integer(term_match & and_match))
    }
    return(as.integer(term_match))
  })
}


# --- 8. Extract which specific keyword(s) triggered the match per sub_concept ---
matched_df2 <- base %>% select(InitiativeID:policy_or)

# Helper: Convert a single keyword with wildcard and spacing logic to strict regex
convert_single_term_to_regex <- function(term) {
  term <- gsub("\\*", "\\\\w*", term)                # wildcard becomes \w*
  term <- gsub("\\s+", " ", term)                    # normalize spaces
  words <- strsplit(term, " ", fixed = TRUE)[[1]]    # split into words
  if (length(words) > 1) {
    return(paste0("(?i)\\b", paste(words, collapse = "\\s{1}"), "\\b"))
  } else {
    return(paste0("(?i)\\b", term, "\\b"))
  }
}

# Loop through keywords and evaluate specific matches
for (i in 1:nrow(keywords)) {
  sub_concept_name <- keywords$sub_concept[i]
  term_text <- keywords$term[i]
  and_text  <- keywords$AND[i]
  
  # Split terms and ANDs by OR
  terms <- str_split(term_text, "\\s+OR\\s+", simplify = TRUE) %>% str_trim()
  and_terms <- if (!is.na(and_text)) str_split(and_text, "\\s+OR\\s+", simplify = TRUE) %>% str_trim() else character(0)
  
  # Convert to strict regex patterns
  term_patterns <- sapply(terms, convert_single_term_to_regex)
  and_patterns <- sapply(and_terms, convert_single_term_to_regex)
  
  # For each text, return the matched terms (only if AND condition is satisfied)
  matched_df2[[sub_concept_name]] <- sapply(base$text, function(text) {
    # Find matching term(s)
    matched_terms <- terms[
      sapply(term_patterns, function(pat) str_detect(text, regex(pat)))
    ]
    
    # Apply AND condition if present
    if (length(and_patterns) > 0) {
      and_matched <- any(sapply(and_patterns, function(pat) str_detect(text, regex(pat))))
      if (!and_matched || length(matched_terms) == 0) {
        return(NA)
      }
    }
    
    if (length(matched_terms) > 0) {
      return(paste(matched_terms, collapse = "; "))
    } else {
      return(NA)
    }
  })
}

# --- 9. Loop through each policy_orientation and count total matches ---
for (policy in policy_categories) {
  policy_terms <- keywords %>%
    filter(policy_orientation == policy) %>%
    select(term_regex, and_regex)
  
  count_policy_matches <- function(text) {
    total <- 0
    for (i in 1:nrow(policy_terms)) {
      if (!is.na(policy_terms$and_regex[i])) {
        and_match <- str_detect(text, policy_terms$and_regex[i])
        if (!and_match) next
      }
      term_count <- str_count(text, policy_terms$term_regex[i])
      total <- total + term_count
    }
    return(total)
  }
  
  base[[paste0("policy_", policy)]] <- sapply(base$text, count_policy_matches)
}

# --- 10. Create a global binary flag for any policy match ---
base <- base %>%
  mutate(policy_or = as.integer(if_any(starts_with("policy_"), ~ . > 0)))

# --- 11. Create matched_df3 to track which sub_concepts match for each text ---
matched_df3 <- base %>% select(InitiativeID:policy_or)

for (i in 1:nrow(keywords)) {
  sub_concept_name <- keywords$sub_concept[i]
  term_regex <- keywords$term_regex[i]
  and_regex  <- keywords$and_regex[i]
  
  matched_df3[[sub_concept_name]] <- sapply(base$text, function(text) {
    if (!is.na(and_regex)) {
      and_match <- str_detect(text, and_regex)
      if (!and_match) return(0)
    }
    term_count <- str_count(text, term_regex)
    return(term_count)
  })
