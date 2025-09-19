library(tidyverse)
library(stat471)
data(brfss)
summary(brfss$HEIGHT3)

brfss |> ggplot(aes( x= HEIGHT3)) + geom_histogram()


employees <- data.frame(
  id = c(1, 2, 3, 4),
  age = c(25, -3, 45, 200),
  gender = c("M", "F", "F", "X"),
  salary = c(50000, 60000, 70000, 999999),
  start_date = ymd(c("2020-01-01", "2021-02-01", "2020-03-01", "2020-01-15")),
  end_date   = ymd(c("2020-12-31", "2020-03-01", NA, NA))
)

library(testdat)
set_testdata(employees)

expect_unique(id)

expect_range(age, 0, 120)
expect_range(salary, 0, 500000)

expect_cond(is.na(end_date), start_date <= end_date)

expect_cond(TRUE, start_date <= end_date)



with_testdata(data, code, quosure = TRUE)

