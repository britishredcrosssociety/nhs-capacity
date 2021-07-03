library(readr)
library(writexl)

england  <- read_csv("data/nhs-performance-england.csv")
wales    <- read_csv("data/nhs-performance-wales.csv")
scotland <- read_csv("data/nhs-performance-scotland.csv")
ni       <- read_csv("data/nhs-performance-ni.csv")

write_xlsx(
  list("England" = england, "Wales" = wales, "Scotland" = scotland, "Northern Ireland" = ni),
  path = "NHS performance.xlsx"
)
