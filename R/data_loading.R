#' Load and Preprocess Datasets
#'
#' This function loads all required datasets from the specified directory, processes them as needed,
#' and returns a list of processed dataframes.
#'
#' @param data_dir A character string specifying the directory where the data files are located. Default is "data/".
#' @return A named list of processed dataframes.
#' @export

load_data <- function(data_dir = "data/") {
  # Load datasets with error handling
  tryCatch({
    df_hr <- read.csv(file.path(data_dir, "hr.csv"), stringsAsFactors = FALSE)
    df_credit_card_default <- read.csv(file.path(data_dir, "UCI_Credit_Card.csv"), stringsAsFactors = FALSE)
    df_credit_card_approval <- read.csv(file.path(data_dir, "Application_record.csv"), stringsAsFactors = FALSE)
    df_credit_data <- read.csv(file.path(data_dir, "credit_record.csv"))
    df_diabetes <- read.csv(file.path(data_dir, "diabetes_binary_health_indicators_BRFSS2015.csv"), stringsAsFactors = FALSE)
    df_insurance <- read.csv(file.path(data_dir, "insurance_train.csv"), stringsAsFactors = FALSE)
    df_credit_card_fraud <- read.csv(file.path(data_dir, "creditcard.csv"), stringsAsFactors = FALSE)
    # df_bank_fraud <- read.csv(file.path(data_dir, "Base.csv"), stringsAsFactors = FALSE)
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })

  # Process credit card approval data
  df_credit_card_approval <- process_credit_card_data(df_credit_data, df_credit_card_approval)

  # Create list of datasets
  df_list <- list(
    "HR Dataset" = df_hr,
    # "Bank Fraud Dataset" = df_bank_fraud,
    "Credit Card Fraud Dataset" = df_credit_card_fraud,
    "Diabetes Dataset" = df_diabetes,
    "Insurance Dataset" = df_insurance,
    "Credit Card Default Dataset" = df_credit_card_default,
    "Credit Card Approval Dataset" = df_credit_card_approval
  )

  # Rename target columns
  df_list <- lapply(names(df_list), function(name) {
    df <- df_list[[name]]
    target_col <- switch(name,
                         "Credit Card Fraud Dataset" = "Class",
                         "Diabetes Dataset" = "Diabetes_binary",
                         # "Bank Fraud Dataset" = "fraud_bool",
                         "Insurance Dataset" = "Response",
                         "Credit Card Default Dataset" = "default.payment.next.month",
                         "Credit Card Approval Dataset" = "target",
                         NULL
    )
    if (!is.null(target_col)) {
      df <- rename_target_column(df, target_col)
    }
    return(df)
  })
  names(df_list) <- c("HR Dataset", "Credit Card Fraud Dataset",
                      "Diabetes Dataset", "Insurance Dataset", "Credit Card Default Dataset",
                      "Credit Card Approval Dataset")

  return(df_list)
}


rename_target_column <- function(data, original_name, new_name = "target") {
  if (original_name %in% colnames(data)) {
    colnames(data)[colnames(data) == original_name] <- new_name
    data[[new_name]] <- as.factor(data[[new_name]])
  }
  return(data)
}



process_credit_card_data <- function(df_credit_data, df_credit_card_approval) {
  # Check required columns
  required_cols_data <- c("STATUS", "ID")
  required_cols_approval <- c("CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY",
                              "FLAG_WORK_PHONE", "FLAG_PHONE", "FLAG_EMAIL", "DAYS_BIRTH",
                              "AMT_INCOME_TOTAL", "NAME_INCOME_TYPE", "NAME_HOUSING_TYPE",
                              "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS")

  if (!all(required_cols_data %in% colnames(df_credit_data))) {
    stop("Missing required columns in credit data")
  }
  if (!all(required_cols_approval %in% colnames(df_credit_card_approval))) {
    stop("Missing required columns in credit card approval data")
  }

  # Update dep_value based on STATUS
  df_credit_data <- df_credit_data %>%
    mutate(dep_value = ifelse(STATUS %in% c("2", "3", "4", "5"), "Yes", "No"))

  # Group by ID and determine risk status
  overdue_status <- df_credit_data %>%
    group_by(ID) %>%
    summarize(risk = ifelse(any(dep_value == "Yes"), 1, 0), .groups = "drop")

  # Merge with application data and create target
  df_credit_card_approval <- df_credit_card_approval %>%
    inner_join(overdue_status, by = "ID") %>%
    mutate(target = factor(risk)) %>% select-(risk)

  # Binary features
  df_credit_card_approval <- df_credit_card_approval %>%
    mutate(
      Gender = as.factor(ifelse(CODE_GENDER == "M", 1, 0)),
      Car = as.factor(ifelse(FLAG_OWN_CAR == "Y", 1, 0)),
      Reality = as.factor(ifelse(FLAG_OWN_REALTY == "Y", 1, 0)),
      wkphone = as.factor(FLAG_WORK_PHONE),
      phone = as.factor(FLAG_PHONE),
      email = as.factor(FLAG_EMAIL)
    )

  # Age
  df_credit_card_approval <- df_credit_card_approval %>%
    mutate(Age = -DAYS_BIRTH %/% 365) %>%
    mutate(gp_Age = cut(
      Age,
      breaks = quantile(Age, probs = seq(0, 1, 0.2), na.rm = TRUE),
      labels = c("lowest", "low", "medium", "high", "highest"),
      include.lowest = TRUE
    ))

  # Income groups
  df_credit_card_approval <- df_credit_card_approval %>%
    mutate(
      inc = AMT_INCOME_TOTAL / 10000,
      gp_inc = cut(
        inc,
        breaks = quantile(inc, probs = seq(0, 1, 0.3), na.rm = TRUE),
        labels = c("low", "medium", "high"),
        include.lowest = TRUE
      )
    )

  # Categorical features
  df_credit_card_approval <- df_credit_card_approval %>%
    mutate(
      inctp = recode(NAME_INCOME_TYPE, "Pensioner" = "State servant", "Student" = "State servant"),
      houtp = as.factor(NAME_HOUSING_TYPE),
      edutp = as.factor(ifelse(NAME_EDUCATION_TYPE == "Academic degree", "Higher education", NAME_EDUCATION_TYPE)),
      famtp = as.factor(NAME_FAMILY_STATUS)
    )

  return(df_credit_card_approval)
}
