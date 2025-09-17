property <- readxl::read_xlsx("../data/state/NIBRS_Table_28_Crimes_Against_Property_Offenses_Offense_Category_by_State_2024.xlsx",
                              skip=4)

property_names <- readxl::read_xlsx("../data/state/NIBRS_Table_28_Crimes_Against_Property_Offenses_Offense_Category_by_State_2024.xlsx",
                              skip=5)

#values_only <- readxl::read_xlsx("../data/state/NIBRS_Table_28_Crimes_Against_Property_Offenses_Offense_Category_by_State_2024.xlsx",
#                                    skip=6)

names(property)[-(1:4)] <- names(property_names)[-(1:4)]
names(property)



property <- janitor::clean_names(property)


library(rvest)
url <- "https://www.the-numbers.com/box-office-chart/weekend/2025/09/05"
doc <- read_html(url)
tables <- doc |> html_table()
tables[[2]]


property <- property |> dplyr::slice(-1)


summary(property)
