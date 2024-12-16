#' Process All DataFrames
#'
#' This function processes a list of dataframes by applying specific transformations,
#' including renaming columns, feature engineering, and any other preprocessing steps.
#'
#' @param df_list A named list of dataframes to process.
#' @return A named list of processed dataframes.
#' @importFrom dplyr %>% bind_rows mutate select
#' @export
process_all_dataframes <- function(df_list) {
  processed_list <- list()

  for (name in names(df_list)) {
    df <- df_list[[name]]

    if (name == "HR Dataset") {
      # HR Dataset Processing
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
      # Bank Fraud Dataset Processing

    } else if (name == "Credit Card Fraud Dataset") {
      # Credit Card Fraud Dataset Processing

    } else if (name == "Diabetes Dataset") {
      # Diabetes Dataset Processing
      df <- df %>%
        select(-Fruits, -Veggies, -Sex, -CholCheck, -AnyHealthcare)
    } else if (name == "Insurance Dataset") {
      # Insurance Dataset Processing

    } else if (name == "Credit Card Default Dataset") {
      # Credit Card Default Dataset Processing
      df <- df %>%
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
      # Credit Card Approval Dataset Processing
      df <- df %>% select(-risk)

    }

    categorical_columns <- sapply(df, is.factor) | sapply(df, is.character)
    df[categorical_columns] <- lapply(df[categorical_columns], as.factor)

    # Add the processed dataframe to the list
    processed_list[[name]] <- df
  }

  return(processed_list)
}

