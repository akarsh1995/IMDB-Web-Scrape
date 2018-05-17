library(rvest)
library(data.table)
library(dplyr)

scrape_imdb<- function(url = "http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature&page=1"){
  
  webpage <-read_html(url)
  
  ### rank
  rank_data_html<- html_nodes(webpage, '.text-primary')
  
  rank_data<- html_text(rank_data_html)
  
  df <- tibble(rank = rank_data)
  
  ### title
  title_data_html <- html_nodes(webpage,
                                '.lister-item-header a')
  title_data<- html_text(title_data_html, trim = T)
  
  df <- mutate(df, title = title_data)
  
  ### description
  short = c("actor_data", "certificate_data", "description_data", "director_data", 
            "genre_data", "gross_data", "meta_data", "rank_data", "release_year_data", 
            "running_time_data", "star_data", "title_data", "vote_data")
  
  description_data_html <- html_nodes(webpage,
                                      '.ratings-bar+ .text-muted')
  
  description_data<- html_text(description_data_html,trim = T)
  if(length(description_data)==100){
    df <- mutate(df, description = description_data)
  } else {
    description_data_html <- html_nodes(webpage,
                                        '.text-primary , .ratings-bar+ .text-muted')
    description_data<- html_text(description_data_html,trim = T)
    description<- data.frame(rank = NA, description = NA)
    for( i in (!(description_data %in% rank_data))%>%which()){
      description[i,1] = description_data[i-1]
      description[i,2] = description_data[i]
    }
    df <- df%>%left_join(y = description, by= "rank")
  }
  
  ### stars
  star_data_html <- html_nodes(webpage,
                               '.ratings-imdb-rating strong')
  star_data<- html_text(star_data_html, trim = T)
  
  
  if(length(star_data)==100){
    df <- mutate(df, star = star_data)
  } else {
    star_data_html <- html_nodes(webpage,
                                 '.text-primary , .ratings-imdb-rating strong')
    star_data<- html_text(star_data_html, trim = T)
    star<- data.frame(rank = NA, star = NA)
    for( i in (!(star_data %in% rank_data))%>%which()){
      star[i,1] = star_data[i-1]
      star[i,2] = star_data[i]
    }
    star$star<-as.numeric(star$star)
    df <- df%>%left_join(y = star, by= "rank")
  }
  
  
  ### genre
  genre_data_html <- html_nodes(webpage,
                                '.genre')
  genre_data<- html_text(genre_data_html, trim = T)
  
  
  if(length(genre_data)==100){
    df <- mutate(df, genre = genre_data)
  } else {
    genre_data_html <- html_nodes(webpage,
                                  '.text-primary , .genre')
    genre_data<- html_text(genre_data_html, trim = T)
    genre<- data.frame(rank = NA, genre = NA)
    for( i in (!(genre_data %in% rank_data))%>%which()){
      genre[i,1] = genre_data[i-1]
      genre[i,2] = genre_data[i]
    }
    df <- df%>%left_join(y = genre, by= "rank")
  }
  
  ### running time
  running_time_data_html <- html_nodes(webpage,
                                       '.text-muted .runtime')
  running_time_data <- html_text(running_time_data_html)
  
  if(length(running_time_data)==100){
    running_time_data<-gsub(pattern = "\\smin$",replacement = "",
                            running_time_data)%>%as.numeric()
    df <- mutate(df, running_time = running_time_data%>%as.numeric())
  } else {
    running_time_data_html <- html_nodes(webpage,
                                         '.text-primary , .text-muted .runtime')
    running_time_data<- html_text(running_time_data_html, trim = T)
    running_time_data<-gsub(pattern = "\\smin$",replacement = "",
                            running_time_data)
    running_time<- data.frame(rank = NA, running_time = NA)
    for( i in (!(running_time_data %in% rank_data))%>%which()){
      running_time[i,1] = running_time_data[i-1]
      running_time[i,2] = running_time_data[i]%>%as.numeric()
    }
    df <- df%>%left_join(y = running_time, by= "rank")
  }
  
  
  ### release year
  release_year_data_html <- html_nodes(webpage,
                                       '.text-muted.unbold')
  release_year_data<- html_text(release_year_data_html)
  
  if(length(release_year_data)==100){
    release_year_data <- release_year_data%>%stringr::str_extract(pattern = "\\d+")%>%as.numeric()
    df <- mutate(df, release_year = release_year_data)
    
  } else {
    release_year_data_html <- html_nodes(webpage,
                                         '.text-primary , .text-muted.unbold')
    release_year_data<- html_text(release_year_data_html, trim = T)
    release_year<- data.frame(rank = NA, release_year = NA)
    for( i in (!(release_year_data %in% rank_data))%>%which()){
      release_year[i,1] = release_year_data[i-1]
      release_year[i,2] = release_year_data[i]
    }
    release_year$release_year<- release_year$release_year%>%
      stringr::str_extract(pattern = "\\d+")%>%as.numeric()
    
    df <- df%>%left_join(y = release_year, by= "rank")
  }
  
  ### gross edition required
  gross_data_html <- html_nodes(webpage,
                                '.ghost~ .text-muted+ span')
  gross_data<- html_text(gross_data_html)
  
  if(length(gross_data)==100){
    df <- mutate(df, gross = gross_data)
  } else {
    gross_data_html <- html_nodes(webpage,
                                  '.text-primary , .ghost~ .text-muted+ span')
    gross_data<- html_text(gross_data_html)
    gross <- data.frame(rank = NA, gross = NA)
    for(i in (!(gross_data%in%rank_data))%>%which){
      gross[i,2] = stringr::str_extract(gross_data[i], "\\d+\\.\\d+")%>%as.numeric()
      gross[i,1] = gross_data[i-1]
    }
    df<- df%>%left_join(y = gross, by= "rank")
  }
  
  ### votes
  vote_data_html <- html_nodes(webpage,
                               '.sort-num_votes-visible span:nth-child(2)')
  vote_data<- html_text(vote_data_html)
  
  if(length(vote_data) == 100){
    vote_data<-gsub(pattern = ",",replacement = "",x = vote_data)%>%as.numeric()
    df <- mutate(df, vote = vote_data)
    
  } else {
    vote_data_html <- html_nodes(webpage,
                                 '.text-primary , .sort-num_votes-visible span:nth-child(2)')
    vote_data<- html_text(vote_data_html)
    vote <- data.frame(rank  = NA, vote = NA)
    for(i in (!(vote_data %in% rank_data))%>%which){
      vote[i,2] = vote_data[i]
      vote[i,1] = vote_data[i-1]
    }
    vote$vote <- gsub(pattern = ",",replacement = "",x = vote$vote)%>%as.numeric()
    df<- df%>%left_join(y = vote, by= "rank")
    
  }
  ### certificate
  certificate_data_html <- html_nodes(webpage,
                                      '.certificate')
  certificate_data<- html_text(certificate_data_html)
  
  if(length(certificate_data)==100){
    df <- mutate(df, certificate = certificate_data)
    
  } else {
    certificate_data_html <- html_nodes(webpage,
                                        '.text-primary , .certificate')
    certificate_data<- html_text(certificate_data_html)
    
    certificate<-data.frame(rank = NA, certificate = NA)
    for(i in (!(certificate_data %in% rank_data))%>%which()){
      certificate[i,1] = paste(certificate_data[i-1])
      certificate[i,2] = paste(certificate_data[i])
    }
    df<- df%>%left_join(y = certificate, by= "rank")
  }
  ### meta score
  meta_score_data_html <- html_nodes(webpage,
                                     '.favorable')
  meta_score_data<- html_text(meta_score_data_html,trim = T)
  if( length(meta_score_data)==100){
    df <- mutate(df, meta_score = meta_score_data)
  } else {
    meta_score_data_html <- html_nodes(webpage,
                                       '.text-primary , .favorable')
    meta_score_data<- html_text(meta_score_data_html,trim = T)
    meta_score = data.frame(rank = NA, `meta_score`= NA)
    if(!all(meta_score_data%in%rank_data)){
      for(i in (!(meta_score_data%in%rank_data))%>%which()){
        meta_score[i,1] = meta_score_data[i-1]
        meta_score[i,2] = meta_score_data[i]%>%as.numeric()
      }
      df<- df%>%left_join(y = meta_score, by= "rank")
    } else {
      df<- df%>%mutate(meta_score = NA)
    }
  }
  
  ###director
  
  director_data<-html_nodes(webpage, '.text-muted+ p')%>%html_text(trim = T)%>%
    tstrsplit(split = "|", fixed = T, names = c("director"), keep = 1)%>%data.frame(director=.)
  
  director_data<- stringr::str_replace(string = director_data[,1],pattern = "(Director:\\s)|(Directors:\\s)",replacement = "")%>%trimws()
  
  if(length(director_data)==100){
    df <- mutate(df, director = director_data)
    
  } else {
    director_data<-html_nodes(webpage, '.text-primary , .text-muted+ p')%>%html_text(trim = T)%>%
      tstrsplit(split = "|", fixed = T, names = c("director"), keep = 1)%>%data.frame(director=.)
    
    director_data<- stringr::str_replace(string = director_data[,1],pattern = "(Director:\\s)|(Directors:\\s)",replacement = "")%>%trimws()
    director <- data.frame(rank = NA, director = NA)
    for(i in (!(director_data%in%rank_data))%>%which){
      director[i,1] = director_data[i-1]
      director[i,2] = director_data[i]
    }
    df<- df%>%left_join(y = director, by= "rank")
    
  }
  
  ###actor
  actor_data<- html_nodes(webpage, '.text-muted+ p')%>%html_text()%>%
    tstrsplit(split = "|", fixed = T,names = c("actor"),keep = 2)%>%data.frame(actor=.)
  
  actor_data<-
    stringr::str_replace(string = actor_data[,1],pattern = "Stars:\\s",replacement = "")%>%trimws()
  
  
  if(length(actor_data)==100){
    df <- mutate(df, actor_data)
    
  } else {
    actor_data<-html_nodes(webpage, '.text-primary , .text-muted+ p')%>%html_text(trim = T)%>%
      tstrsplit(split = "|", fixed = T, names = c("director", "actor"))%>%as.data.frame(stringsAsFactors = F)
    
    actor_data<-within(actor_data,{
      actor[is.na(actor)] <- director[is.na(actor)]
    })[2]
    
    actor_data<- stringr::str_replace(string = actor_data[,1],pattern = "Stars:\\s",replacement = "")%>%trimws()
    actor <- data.frame(rank = NA, actor = NA)
    for(i in (!(actor_data%in%rank_data))%>%which){
      actor[i,1] = actor_data[i-1]
      actor[i,2] = actor_data[i]
    }
    df<- df%>%left_join(y = actor, by= "rank")
  }
  df[[1]]<-gsub(pattern = ",",replacement = "",x = df[[1]])%>%as.numeric()
  df
}
