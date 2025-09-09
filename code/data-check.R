check_data <- function(df, id_var = NULL, numeric_rules = list(), logic_rules = list()) {
  require(dplyr)
  require(skimr)
  require(janitor)

  cat("==== Structure ====\n")
  print(str(df))

  cat("\n==== Variable Types ====\n")
  print(sapply(df, class))

  cat("\n==== Missing Values ====\n")
  print(colSums(is.na(df)))

  cat("\n==== Summary Statistics ====\n")
  print(skim(df))

  if (!is.null(id_var) && id_var %in% names(df)) {
    cat("\n==== Uniqueness Check ====\n")
    dup_count <- anyDuplicated(df[[id_var]])
    if (dup_count == 0) {
      cat("All IDs are unique.\n")
    } else {
      cat("WARNING: Duplicate IDs found!\n")
    }
  }

  cat("\n==== Frequency Tables (Categoricals) ====\n")
  cat_vars <- names(df)[sapply(df, is.character) | sapply(df, is.factor)]
  for (v in cat_vars) {
    cat(paste0("\nVariable: ", v, "\n"))
    print(janitor::tabyl(df[[v]]))
  }

  cat("\n==== Outlier Detection (Numeric Variables) ====\n")
  num_vars <- names(df)[sapply(df, is.numeric)]
  for (v in num_vars) {
    outliers <- boxplot.stats(df[[v]])$out
    if (length(outliers) > 0) {
      cat(paste0("Variable ", v, " has ", length(outliers), " potential outliers.\n"))
    }
  }

  if (length(numeric_rules) > 0) {
    cat("\n==== Numeric Rules Check ====\n")
    for (rule in numeric_rules) {
      cat(paste0("Checking rule: ", rule$var, " in [", rule$min, ", ", rule$max, "]\n"))
      bad <- df %>% filter(.data[[rule$var]] < rule$min | .data[[rule$var]] > rule$max)
      if (nrow(bad) > 0) {
        cat(paste0("  -> Found ", nrow(bad), " violations.\n"))
      } else {
        cat("  -> Passed.\n")
      }
    }
  }

  if (length(logic_rules) > 0) {
    cat("\n==== Logical Rules Check ====\n")
    for (rule in logic_rules) {
      bad <- df %>% filter(!(!!rlang::parse_expr(rule)))
      if (nrow(bad) > 0) {
        cat(paste0("Rule failed: ", rule, " (", nrow(bad), " cases)\n"))
      } else {
        cat(paste0("Rule passed: ", rule, "\n"))
      }
    }
  }

  cat("\n==== Done. ====\n")
}


# ## Example use
#
# df <- data.frame(
#   id = c(1, 2, 2, 4),
#   age = c(25, -3, 45, 200),
#   gender = c("M", "F", "F", "X"),
#   income = c(50000, 60000, 70000, 999999),
#   start_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-01-15")),
#   end_date   = as.Date(c("2020-12-31", "2020-03-01", "2020-04-01", "2019-12-31"))
# )
#
# # Define rules
# numeric_rules <- list(
#   list(var = "age", min = 0, max = 120),
#   list(var = "income", min = 0, max = 500000)
# )
#
# logic_rules <- list(
#   "start_date <= end_date"
# )
#
# # Run checker
# check_data(df, id_var = "id", numeric_rules = numeric_rules, logic_rules = logic_rules)
#
# ## Tidy report
#
# # Example dataset
# df <- data.frame(
#   id = c(1, 2, 2, 4),
#   age = c(25, -3, 45, 200),
#   gender = c("M", "F", "F", "X"),
#   income = c(50000, 60000, 70000, 999999),
#   start_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-01-15")),
#   end_date   = as.Date(c("2020-12-31", "2020-03-01", "2020-04-01", "2019-12-31"))
# )
#
# # Define rules
# numeric_rules <- list(
#   list(var = "age", min = 0, max = 120),
#   list(var = "income", min = 0, max = 500000)
# )
#
# logic_rules <- list(
#   "start_date <= end_date"
# )
#
# # Run checker
# results <- check_data(df, id_var = "id", numeric_rules = numeric_rules, logic_rules = logic_rules)
#
# # Example: view missing values
# results$missing
#
# # Example: check duplicates
# results$id_duplicates
#
# # Example: see violations of numeric rules
# results$numeric_rule_violations$age
#
