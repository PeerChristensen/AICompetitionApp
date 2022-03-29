library(AzureStor)


sas_token <- "sp=racwl&st=2022-03-18T10:12:30Z&se=2022-04-10T17:12:30Z&spr=https&sv=2020-08-04&sr=c&sig=bYR1OCdyEgAwIWK5bENii6ZZbgmcnBmIIkE6wL5MiBo%3D"
sas_url <- "https://demoeventstorage.blob.core.windows.net/aicompetition?sp=racwl&st=2022-03-18T10:12:30Z&se=2022-04-10T17:12:30Z&spr=https&sv=2020-08-04&sr=c&sig=bYR1OCdyEgAwIWK5bENii6ZZbgmcnBmIIkE6wL5MiBo%3D"

readRenviron(".Renviron")
sas_token <- Sys.getenv("SAS_TOKEN")

endpoint <- storage_endpoint("https://demoeventstorage.blob.core.windows.net", sas=sas_token)
container <- storage_container(endpoint, "aicompetition")

storage_write_csv(mtcars, container, "archive/mtcars.csv")

write_csv(mtcars,"mtcars.csv")
 
storage_upload(container, src="mtcars.csv", dest="mtcars.csv", type="AppendBlob")

# appending to an existing blob
storage_upload(container, src="mtcars.csv", dest="mtcars.csv", type="AppendBlob", append=TRUE)

storage_download(container, "leaderboard/leaderboard.rds","leaderboard.rds",overwrite=T)
k=read_rds("leaderboard.rds")
