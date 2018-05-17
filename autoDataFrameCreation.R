source("webscrape.R")
df <- data.frame()

for (page in 1:10){
  url <- paste0("http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature&page=", page)
  df.temp <- scrape_imdb(url)
  df<- rbind(df, df.temp)
}