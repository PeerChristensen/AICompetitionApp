library(AzureStor)
library(tidyverse)

readRenviron(".Renviron")
sas_token <- Sys.getenv("SAS_TOKEN")

endpoint <- storage_endpoint("https://demoeventstorage.blob.core.windows.net", sas=sas_token)
container <- storage_container(endpoint, "aicompetition")

# create new leaderboard
leaderboard <- tibble(
					name = character(),
					mail = character(), 
					initials = character(),
					score = numeric(),
					permission_mail = logical(),
					permission= logical(),
					time = character()
					)

write_csv(leaderboard, "leaderboard.csv")

storage_upload(container, src="leaderboard.csv", dest="leaderboard/leaderboard.csv", type="AppendBlob")

# appending to an existing blob
#storage_upload(container, src="leaderboard.csv", dest="mtcars.csv", type="AppendBlob", append=TRUE)


