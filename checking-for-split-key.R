check_split_key <- function(data, key_vars) {
  all_vars <- colnames(data)
  non_key_vars <- setdiff(all_vars, key_vars)

  results <- list()

  # For each non-key variable, check if it depends only on a *part* of the key
  for (nk in non_key_vars) {
    for (subset_size in 1:(length(key_vars)-1)) {
      subsets <- combn(key_vars, subset_size, simplify = FALSE)

      for (sub in subsets) {
        # Count how many unique values of nk per subset combination
        dup_counts <- data %>%
          group_by(across(all_of(sub))) %>%
          summarise(n_unique = n_distinct(.data[[nk]]), .groups = "drop")

        # If every group has exactly 1 unique value of nk, then nk depends only on sub
        if (all(dup_counts$n_unique == 1)) {
          results[[length(results)+1]] <- list(
            non_key = nk,
            depends_on = sub
          )
        }
      }
    }
  }

  if (length(results) == 0) {
    message("✅ No split key detected: all non-key attributes depend on the full key.")
  } else {
    message("⚠️ Split key detected: some non-key attributes depend only on part of the key.")
    return(results)
  }
}

# Example
df <- data.frame(
  student_id = c(1,1,2,2,3,3),
  course_id  = c("MATH101","STAT101","MATH101","STAT101","MATH101","STAT101"),
  semester = c("Fall2025", "Fall2025", "Fall2025", "Fall2025", "Fall2024", "Fall2024"),
  major      = c("Math","Math","Math","Math","CS","CS"),
  instructor  = c("A","B","A","B","C","D"),
  grade      = c("A","B","A","A","C","B")
)

# composite key is (student_id, course_id)
check_split_key(df, c("student_id", "course_id"))
