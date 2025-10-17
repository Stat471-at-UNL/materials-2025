set.seed(20251002)
n <- 100
dframe <- data.frame(y1 = rnorm(n),
                     y2 = rnorm(n))
dframe <- dframe |> mutate(y2 = ifelse(y1>2, NA, y2))

library(naniar)

mcar_test(dframe)


library(NHANES)
head(NHANES)
library(naniar)
library(visdat)

vis_miss(NHANES) +
  scale_x_discrete(expand=FALSE, position="top") +
  theme(axis.text.x.top = element_text(vjust=0, angle =90))
#Visual Exploration of Missingness Patterns
#Upset plot: variables are connected when they are jointly missing

df |> gg_miss_upset()


#Real Data Example: NHANES
library(NHANES)

NHANES |> vis_miss() +
  scale_x_discrete(expand=FALSE, position="top") +
  theme(axis.text.x.top = element_text(vjust=0, angle =90))
mcar_test(NHANES |> select(SleepHrsNight, SurveyYr))

NHANES |> group_by(SurveyYr) |> select(CompHrsDay, SleepHrsNight) |>
  summarize(computer = mean(CompHrsDay, na.rm=TRUE), sleep = mean(SleepHrsNight, na.rm=TRUE))

