---
title: "Problem Statement 1"
author: "Damario Abdalla, Drew Levitt, Si Thu Aung"
date: "2024-11-06"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Function to create a simulated dataset 

```{r, message=F}
library(tidyverse)
library(dplyr)
library(broom)
data <- read_csv("claims.csv")
head(data)
```
```{r}
# Count missing values for each column
missing_counts <- sapply(data, function(x) sum(is.na(x)))

# Convert the result to a data frame for easier viewing
missing_counts_df <- data.frame(Column = names(missing_counts), Missing_Values = missing_counts)

# Print the result
print(missing_counts_df)
```

```{r}
# Identify pre-randomization outcome variables (e.g., those measured prior to August 2016)
# Let's assume variables starting with "spend" from "0715_0716" are pre-randomization
# Define outcome variables with names that include "_0716"
outcome_vars <- grep("_0716", names(data), value = TRUE)


# Initialize a list to store results
results <- list()

# Loop over each outcome variable to calculate means and p-values
for (outcome in outcome_vars) {
  # Subset data, removing rows with missing values in the current outcome variable
  df <- data %>% select(treat, all_of(outcome)) %>% na.omit()
  
  # Calculate means for control and treatment groups
  control_mean <- mean(df %>% filter(treat == 0) %>% pull(outcome), na.rm = TRUE)
  treatment_mean <- mean(df %>% filter(treat == 1) %>% pull(outcome), na.rm = TRUE)
  
  # Perform linear regression
  model <- lm(as.formula(paste(outcome, "~ treat")), data = df)
  summary_stats <- tidy(model)
  
  # Extract p-value of the treatment effect
  p_value <- summary_stats %>% filter(term == "treat") %>% pull(p.value)
  
  # Append results to list
  results[[outcome]] <- data.frame(
    Variable_Description = outcome,
    Control_Group_Mean = control_mean,
    Treatment_Group_Mean = treatment_mean,
    P_value = p_value
  )
}

# Combine all results into a single data frame
results_df <- do.call(rbind, results)

# Print the results
print(results_df)

```

```{r}
# Define outcome variables for the first year post-randomization (assuming "_0816_0717")
outcome_vars <- grep("_0816_0717", names(data), value = TRUE)

# Define demographic controls
demographic_controls <- c("male", "white", "age37_49", "age50")

# Initialize a list to store results
results <- list()

# Loop over each outcome variable to calculate the treatment effect estimates
for (outcome in outcome_vars) {
  # Subset data, removing rows with missing values for the current outcome and demographic controls
  df <- data %>% select(treat, all_of(outcome), all_of(demographic_controls)) %>% na.omit()
  
  # Regression without demographic controls
  model_no_controls <- lm(as.formula(paste(outcome, "~ treat")), data = df)
  summary_no_controls <- tidy(model_no_controls)
  
  # Extract estimate and standard error for the treatment effect (treat)
  estimate_no_controls <- summary_no_controls %>% filter(term == "treat") %>% pull(estimate)
  se_no_controls <- summary_no_controls %>% filter(term == "treat") %>% pull(std.error)
  
  # Regression with demographic controls
  model_with_controls <- lm(as.formula(paste(outcome, "~ treat +", paste(demographic_controls, collapse = " + "))), data = df)
  summary_with_controls <- tidy(model_with_controls)
  
  # Extract estimate and standard error for the treatment effect with controls
  estimate_with_controls <- summary_with_controls %>% filter(term == "treat") %>% pull(estimate)
  se_with_controls <- summary_with_controls %>% filter(term == "treat") %>% pull(std.error)
  
  # Append results to the list
  results[[outcome]] <- data.frame(
    Variable_Description = outcome,
    Difference_Treatment_Control = paste0(round(estimate_no_controls, 3), " (", round(se_no_controls, 3), ")"),
    Difference_With_Demographics = paste0(round(estimate_with_controls, 3), " (", round(se_with_controls, 3), ")")
  )
}

# Combine all results into a single data frame
results_df <- do.call(rbind, results)

# Print the results
print(results_df)
```
```{r}
# Define outcome variables for the first year post-randomization (assuming "_0816_0717")
outcome_vars <- grep("_0816_0717", names(data), value = TRUE)

# Define demographic controls
demographic_controls <- c("male", "white", "age37_49", "age50")

# Initialize a list to store results
results <- list()

# Loop over each outcome variable to calculate the participation effect estimates
for (outcome in outcome_vars) {
  # Subset data, removing rows with missing values for the current outcome and demographic controls
  df <- data %>% select(hra_c_yr1, all_of(outcome), all_of(demographic_controls)) %>% na.omit()
  
  # Regression without demographic controls
  model_no_controls <- lm(as.formula(paste(outcome, "~ hra_c_yr1")), data = df)
  summary_no_controls <- tidy(model_no_controls)
  
  # Extract estimate and standard error for the participation effect (hra_c_yr1)
  estimate_no_controls <- summary_no_controls %>% filter(term == "hra_c_yr1") %>% pull(estimate)
  se_no_controls <- summary_no_controls %>% filter(term == "hra_c_yr1") %>% pull(std.error)
  
  # Regression with demographic controls
  model_with_controls <- lm(as.formula(paste(outcome, "~ hra_c_yr1 +", paste(demographic_controls, collapse = " + "))), data = df)
  summary_with_controls <- tidy(model_with_controls)
  
  # Extract estimate and standard error for the participation effect with controls
  estimate_with_controls <- summary_with_controls %>% filter(term == "hra_c_yr1") %>% pull(estimate)
  se_with_controls <- summary_with_controls %>% filter(term == "hra_c_yr1") %>% pull(std.error)
  
  # Append results to the list
  results[[outcome]] <- data.frame(
    Variable_Description = outcome,
    Difference_Participants_NonParticipants = paste0(round(estimate_no_controls, 3), " (", round(se_no_controls, 3), ")"),
    Difference_With_Demographics = paste0(round(estimate_with_controls, 3), " (", round(se_with_controls, 3), ")")
  )
}

# Combine all results into a single data frame
results_df <- do.call(rbind, results)

# Print the results
print(results_df)


```

