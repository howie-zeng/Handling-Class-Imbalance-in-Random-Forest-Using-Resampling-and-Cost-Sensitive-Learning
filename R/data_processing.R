#' Process All DataFrames
#'
#' This function processes a list of dataframes by applying specific transformations,
#' including renaming columns, feature engineering, and any other preprocessing steps.
#'
#' @param df_list A named list of dataframes to process.
#' @return A named list of processed dataframes.

process_all_dataframes <- function(df_list) {
  processed_list <- list()

  for (name in names(df_list)) {
    df <- df_list[[name]]

    tryCatch({
      if (name == "HR Dataset") {
        df <- df %>%
          rename(relevant_experience = relevent_experience) %>%
          mutate(
            relevant_experience = str_replace(
              relevant_experience,
              pattern = "relevent",
              replacement = "relevant"
            )
          ) %>%
          mutate_if(is.character, as.factor) %>%
          mutate(target = as.factor(target)) %>%
          mutate(city_development_index = as.numeric(city_development_index)) %>%
          select(-enrollee_id, -city, -gender)
      } else if (name == "Bank Fraud Dataset") {
        df <- df %>%
          rename(target = fraud_bool)
      } else if (name == "Credit Card Fraud Dataset") {
        df <- df %>%
          rename(target = Class)
      } else if (name == "Diabetes Dataset") {
        df <- df %>%
          rename(target = Diabetes_binary) %>%
          select(-Fruits, -Veggies, -Sex, -CholCheck, -AnyHealthcare)
      } else if (name == "Insurance Dataset") {
        df <- df %>%
          rename(target = Response)
      } else if (name == "Credit Card Default Dataset") {
        if (!all(c("EDUCATION", "MARRIAGE") %in% colnames(df))) {
          stop("Missing required columns in Credit Card Default Dataset")
        }
        df <- df %>%
          rename(target = default.payment.next.month) %>%
          mutate(
            EDUCATION = ifelse(EDUCATION %in% c(0, 5, 6), 4, EDUCATION),
            MARRIAGE = ifelse(MARRIAGE == 0, 3, MARRIAGE)
          ) %>%
          mutate(
            Payment_Sum = PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6,
            Dues = BILL_AMT1 + BILL_AMT2 + BILL_AMT3 - (PAY_AMT1 + PAY_AMT2 + PAY_AMT3)
          ) %>%
          mutate(
            SEX = factor(SEX, levels = c(1, 2), labels = c("MALE", "FEMALE")),
            EDUCATION = factor(EDUCATION, labels = c("Graduate School", "University", "High School", "Others")),
            MARRIAGE = factor(MARRIAGE, labels = c("Married", "Single", "Others"))
          )
      } else if (name == "Credit Card Approval Dataset") {
        df_credit_data <- read.csv("data/credit_record.csv", stringsAsFactors = FALSE)
        df <- process_credit_card_data(df_credit_data, df)
      }

      # Apply Label Encoding to All Categorical Columns
      categorical_columns <- sapply(df, is.factor) | sapply(df, is.character)
      df[categorical_columns] <- lapply(df[categorical_columns], as.factor)

      # Store processed dataframe
      processed_list[[name]] <- df
    }, error = function(e) {
      warning(paste("Error processing dataset:", name, "\n", e$message))
      processed_list[[name]] <- NULL  # Add NULL if the dataset cannot be processed
    })
  }

  return(processed_list)
}
