runtimes <- read.csv("data-raw/runtimes.csv")

runtimes$combination_no <- rep(1:80, each = 30)
runtimes <- runtimes[, c(13, 1:12)]

usethis::use_data(runtimes, overwrite = TRUE)
