if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(httr)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(readxl)
library(httr)

file_path <- "FedCourtDatabase.xlsx"
file_url <- "https://www.umassd.edu/media/umassdartmouth/political-science/facultydocs/fdcdata_thru_2012_n=110977.xlsx"

if (file.exists(file_path)) {
    xl <- read_excel(file_path)
} else {
    GET(file_url, write_disk(dl <- tempfile(fileext = ".xlsx")))
    xl <- read_excel(dl)
}

head(xl)

