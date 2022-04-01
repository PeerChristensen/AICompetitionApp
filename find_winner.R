# find winner

library(tidyverse)
library(AzureStor)
library(lubridate)

readRenviron(".Renviron")
sas_token <- Sys.getenv("SAS_TOKEN")

endpoint <- storage_endpoint("https://demoeventstorage.blob.core.windows.net", sas=sas_token)
container <- storage_container(endpoint, "aicompetition")

# get data from blob storage
storage_download(container, "leaderboard/leaderboard.csv","leaderboard.csv",overwrite=T)

df <- read_csv("leaderboard.csv") %>%
	drop_na() %>%
	mutate(time = as.POSIXct(time)) %>%
	group_by(mail) %>%
	slice_min(time, n = 3) %>%
	filter(score == max(score)) %>%
	ungroup() %>%
	distinct(mail,.keep_all = T) %>%
	filter(!str_detect(mail,"kapacity")) %>%
	arrange(desc(score)) %>%
	mutate(rank = row_number()) %>%
	mutate(initials = str_trunc(initials, 4, ellipsis="")) %>%
	mutate(Navn = paste0(rank,".  ",toupper(initials))) %>%
	slice(1:10)
