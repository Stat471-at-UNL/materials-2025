student_courses <- data.frame(
  student_id = c(101, 101, 102, 102, 103, 103),
  course_id = c("CS101", "MATH201", "CS101", "PHYS101", "MATH201", "CS101"),
  semester = c("Fall2023", "Fall2023", "Fall2023", "Fall2023", "Fall2023", "Fall2023"),
  instructor_name = c("Dr. Smith", "Dr. Johnson", "Dr. Smith",   "Dr. Johnson", "Dr. Brown", "Dr. Smith"),
  grade = c("A", "B+", "B", "A-", "A", "B+")
)

# We don't have duplicates:
!any(duplicated(student_courses))

student_courses |> count(student_id, course_id) |> filter(n > 1)

student_courses |> count(student_id) |> filter(n > 1)
student_courses |> count(course_id) |> filter(n > 1)

# yes, data set is in 1st normal form

courses <- student_courses |> group_by(course_id) |>
  summarize(
    semester = unique(semester),
    instructor_name = paste(unique(instructor_name), collapse=", ")
  )

!any(duplicated(courses$course_id))
