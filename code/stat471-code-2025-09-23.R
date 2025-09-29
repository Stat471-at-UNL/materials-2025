set.seed(20250923)

outliers <- t(replicate(1000, {
  x <- rnorm(100)

  quartiles <- quantile(x, probs = c(.25, .75))
  iqr <- diff(quartiles)

  # determine the number of outliers

  lower_out <- sum(x < quartiles[1]- 1.5*iqr)
  upper_out <- sum(x > quartiles[2]+ 1.5*iqr)

  # determine the number of extreme outliers
  lower_extr_out <- sum(x < quartiles[1]- 3*iqr)
  upper_extr_out <- sum(x > quartiles[2]+ 3*iqr)

  # report both numbers back
  c("outliers"=lower_out + upper_out,
    "extreme" = lower_extr_out +  upper_extr_out)
})) |> data.frame()

# find the proportion of outliers across the samples

summary(outliers)
