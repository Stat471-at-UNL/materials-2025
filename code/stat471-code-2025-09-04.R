#
# What is the average grade in each subject?
#
#   How many courses does each student take?
#
#   How many students are enrolled in total?

students <- data.frame(
  student_name = c("Alice", "Alice", "Bob", "Bob", "Alice", "Alice"),
  course = c("Math", "Science", "Math", "Science", "Math", "Science"),
  grade = c(85, 92, 78, 88, 82, 90),
  student_id = rep(1:3, each = 2)
)

library(tidyverse)

duplicated(students |> select(student_id, course))

students |> group_by(course) |>
  summarise(
    grade = mean(grade, na.rm=TRUE),
    n = n(),
    non_missing_grade = sum(is.na(grade))
  )

students |> group_by(student_id) |>
  summarise(
    num_courses = length(unique(course)),
    n = n(),
  ) |> ungroup()

students |> summarize(
  num_enrolled = length(unique(student_id))
)

students |> count(student_id) |> nrow()
