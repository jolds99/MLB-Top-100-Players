## Load Packages
library(rvest)
library(XML)
library(tidyverse)
library(plyr)

## Vector of Years for Data Scraping
years = 2008:2019

## Standard Batting Tables From 2008-2019
standard_batting_tables_scrape_function = function(years){
  df = list()
  ## Obtaining Standard Batting Data By Year
  for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-standard-batting.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
          html_text() %>%    # extract comment text
          paste(collapse = '') %>%    # collapse to a single string
          read_html() %>%
          html_node('table') %>%    # select the desired table
          html_table() 
      ## Removing Header Rows
      index = seq(0, nrow(data), by=26)
      data = data[-index,]
      df[[i]] = data
  }
 return(df)
}

sb_stats = standard_batting_tables_scrape_function(years)

## Advanced Batting Tables 2008-2019
advanced_batting_tables_scrape_function = function(years){
  df = list()
  ## Obtaining Advanced Batting Data By Year
  for(i in 1:length(years)){
    url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-advanced-batting.shtml"))
    data = url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
      html_text() %>%    # extract comment text
      paste(collapse = '') %>%    # collapse to a single string
      read_html() %>%
      html_node('table') %>%    # select the desired table
      html_table() 
    ## Removing Header Rows
    index = seq(0, nrow(data), by=26)
    data = data[-index,]
    df[[i]] = data
  }
  return(df)
}

ab_stats = advanced_batting_tables_scrape_function(years)

## Value Batting Tables 2008-2019
value_batting_tables_scrape_function = function(years){
  df = list()
  ## Obtaining Value Batting Data By Year
  for(i in 1:length(years)){
    url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-value-batting.shtml"))
    data = url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
      html_text() %>%    # extract comment text
      paste(collapse = '') %>%    # collapse to a single string
      read_html() %>%
      html_node('table') %>%    # select the desired table
      html_table() 
    ## Removing Header Rows
    index = seq(0, nrow(data), by=26)
    data = data[-index,]
    df[[i]] = data
  }
  return(df)
}
vb_stats = value_batting_tables_scrape_function(years)

## Standard Pitching Tables 2008-2019
standard_pitching_tables_scrape_function = function(years){
  df = list()
  ## Obtaining Standard Pitching Data By Year
  for(i in 1:length(years)){
    url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-standard-pitching.shtml"))
    data = url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
      html_text() %>%    # extract comment text
      paste(collapse = '') %>%    # collapse to a single string
      read_html() %>%
      html_node('table') %>%    # select the desired table
      html_table() 
    ## Removing Header Rows
    index = seq(0, nrow(data), by=26)
    data = data[-index,]
    df[[i]] = data
  }
  return(df)
}
sp_stats = standard_pitching_tables_scrape_function(years)

## Value Pitching Tables 2008-2019
value_pitching_tables_scrape_function = function(years){
  df = list()
  ## Obtaining Advanced Pitching Data By Year
  for(i in 1:length(years)){
    url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-value-pitching.shtml"))
    data = url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
      html_text() %>%    # extract comment text
      paste(collapse = '') %>%    # collapse to a single string
      read_html() %>%
      html_node('table') %>%    # select the desired table
      html_table() 
    ## Removing Header Rows
    index = seq(0, nrow(data), by=26)
    data = data[-index,]
    df[[i]] = data
  }
  return(df)
}
vp_stats = value_pitching_tables_scrape_function(years)

## Standard Fielding Tables 2008-2019
standard_fielding_tables_scrape_function = function(years){
  df = list()
  ## Obtaining Advanced Pitching Data By Year
  for(i in 1:length(years)){
    url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-standard-fielding.shtml"))
    data = url %>% html_nodes(xpath = '//comment()') %>%    # select comment nodes
      html_text() %>%    # extract comment text
      paste(collapse = '') %>%    # collapse to a single string
      read_html() %>%
      html_node('table') %>%    # select the desired table
      html_table() 
    ## Removing Header Rows 
    index = seq(0, nrow(data), by=26)
    data = data[-index,]
    df[[i]] = data
  }
  return(df)
}
sf_stats = standard_fielding_tables_scrape_function(years)

## Removing "*,#,+" from Player Names in All Datasets
remove_junk_function = function(data){
  for(i in 1:12){
  x = data[[i]]$Name
  for(j in 1:length(x)){
    x[j] = gsub("[*]", "", x[j])
  }
  for(j in 1:length(x)){
    x[j] = gsub("[#]", "", x[j])
  }
  for(j in 1:length(x)){
    x[j] = gsub("[+]", "", x[j])
  }
  for(j in 1:length(x)){
    x[j] = stringi::stri_trans_general(x[j], "Latin-ASCII")
  }
  data[[i]]$Name = x
  }
  data
}

sb_stats = remove_junk_function(sb_stats)
ab_stats = remove_junk_function(ab_stats)
vb_stats = remove_junk_function(vb_stats)
sp_stats = remove_junk_function(sp_stats)
vp_stats = remove_junk_function(vp_stats)
sf_stats = remove_junk_function(sf_stats)

## Creating 2019 Tables 
sb_2019 = sb_stats[[12]]
ab_2019 = ab_stats[[12]]
vb_2019 = vb_stats[[12]]
sp_2019 = sp_stats[[12]]
vp_2019 = vp_stats[[12]]
sf_2019 = sf_stats[[12]]

## Collecting Top 100 Players List 2011-2020
top100_2011 = read_csv("Top 100 Player Datasets/2011 Top 100 Players List.csv")
top100_2012 = read_csv("Top 100 Player Datasets/2012 Top 100 Players List.csv")
top100_2013 = read_csv("Top 100 Player Datasets/2013 Top 100 Players List.csv")
top100_2014 = read_csv("Top 100 Player Datasets/2014 Top 100 Players List.csv")
top100_2015 = read_csv("Top 100 Player Datasets/2015 Top 100 Players List.csv")
top100_2016 = read_csv("Top 100 Player Datasets/2016 Top 100 Players List.csv")
top100_2017 = read_csv("Top 100 Player Datasets/2017 Top 100 Players List.csv")
top100_2018 = read_csv("Top 100 Player Datasets/2018 Top 100 Players List.csv")
top100_2019 = read_csv("Top 100 Player Datasets/2019 Top 100 Players List.csv")
top100_2020 = read_csv("Top 100 Player Datasets/2020 Top 100 Players List.csv")

## Merging Batting Datasets
full_batting_data = list()
merge_batting_datasets_function = function(dataset){
for(i in 1:12){
dataset[[i]] = join(sb_stats[[i]], ab_stats[[i]], by = "Name", match = "first")
dataset[[i]] = join(dataset[[i]], vb_stats[[i]], by = "Name", type = "full", match = "first")
}
  dataset
}

full_batting_data = merge_batting_datasets_function(full_batting_data)

## Cleanup Dataset
for(i in 1:12){
  # Remove LgAvg Row
  full_batting_data[[i]] = full_batting_data[[i]][!(full_batting_data[[i]]$Name=="LgAvg per 600 PA"),]
  # Remove Unneccessary Columns
  full_batting_data[[i]] = full_batting_data[[i]][,c(1,3:26,29:30,32:33,35,40:42,52:53,59:60)]
  # Make Most Variables Numeric
  full_batting_data[[i]][,c(2,5:26,28:37)] <- lapply(full_batting_data[[i]][,c(2,5:26,28:37)], as.numeric)
}

n_occur <- data.frame(table(full_batting_data[[8]]$Name))
n_occur[n_occur$Freq > 1,]
test = full_batting_data[[8]][full_batting_data[[8]]$Name %in% n_occur$Var1[n_occur$Freq > 1],]

test = test %>% group_by(Name,G) %>% mutate(Tm=replace(Tm, Tm == "TOT", Tm[which(test$G == nth(test$G,2))]))

test$Tm = ifelse(test$Tm == "TOT", test$Tm[which(test$G == nth(test$G,2))], test$Tm)
  

for(i in unique(test$Name)){
 
 test$Tm[i] = ifelse(test$Tm == "TOT", test$Tm[which(test$G == nth(test$G,2))], test$Tm)
}

view(full_batting_data[[8]])



## Merging Pitching Datasets
full_pitching_data = list()
merge_pitching_datasets_function = function(dataset){
  for(i in 1:12){
    dataset[[i]] = join(sp_stats[[i]], vp_stats[[i]], by = "Name", type = "full", match = "first")
  }
  dataset
}

full_pitching_data = merge_pitching_datasets_function(full_pitching_data)


for(i in 1:12){
  full_pitching_data[[i]] = full_pitching_data[[i]][!(full_pitching_data[[i]]$Name=="LgAvg per 180 IP"),]
}
 
## Renaming Fielding Data
full_fielding_data = sf_stats

for(i in 1:12){
  full_fielding_data[[i]] = full_fielding_data[[i]][!(full_fielding_data[[i]]$Name=="LgAvg"),]
}

view(full_fielding_data[[8]])

## Fixing Typos & Errors in Top 100/Batting Datasets
setdiff(top100_2011$Name, full_batting_data[[3]]$Name)
    which(top100_2011$Name == "Miguel Cabera")
    top100_2011$Name[4] = "Miguel Cabrera"
    which(top100_2011$Name == "Mark Trixeira")
    top100_2011$Name[24] = "Mark Teixeira"
    which(top100_2011$Name == "Brain Wilson")
    top100_2011$Name[44] = "Brian Wilson"
    which(top100_2011$Name == "Mike Stanton")
    top100_2011$Name[76] = "Giancarlo Stanton"
setdiff(top100_2011$Name, full_batting_data[[3]]$Name) 
    
setdiff(top100_2012$Name, full_batting_data[[4]]$Name)
    which(top100_2012$Name == "Howard Kendrick")
    top100_2012$Name[85] = "Howie Kendrick"
    which(top100_2012$Name == "Michael Morse")
    top100_2012$Name[95] = "Mike Morse"
    newrow = c("Adam Wainwright", 1253, 29, "STL", "NL", rep(0,24), 1, rep(0,33))
    full_batting_data[[4]] = rbind(full_batting_data[[4]][1:1252,],newrow,full_batting_data[[4]][-(1:1252),])
setdiff(top100_2012$Name, full_batting_data[[4]]$Name)

setdiff(top100_2013$Name, full_batting_data[[5]]$Name)
    which(top100_2013$Name == "B.J. Upton")
    top100_2013$Name[69] = "Melvin Upton Jr."
    which(top100_2013$Name == "Michael Morse")
    top100_2013$Name[85] = "Mike Morse"
    newrow = c("Victor Martinez", 802, 33, "DET", "AL", rep(0,24), "D", rep(0,33))
    full_batting_data[[5]] = rbind(full_batting_data[[5]][1:801,],newrow,full_batting_data[[5]][-(1:801),])
setdiff(top100_2013$Name, full_batting_data[[5]]$Name)    

setdiff(top100_2014$Name, full_batting_data[[6]]$Name)
    which(top100_2014$Name == "Clay Buccholz")
    top100_2014$Name[89] = "Clay Buchholz"
setdiff(top100_2014$Name, full_batting_data[[6]]$Name)

setdiff(top100_2015$Name, full_batting_data[[7]]$Name)
    newrow = c("Matt Harvey", 557, 25, "NYM", "NL", rep(0,24), "1", rep(0,33))
    full_batting_data[[7]] = rbind(full_batting_data[[7]][1:556,],newrow,full_batting_data[[7]][-(1:556),])
setdiff(top100_2015$Name, full_batting_data[[7]]$Name)
    
setdiff(top100_2016$Name, full_batting_data[[8]]$Name)
    which(top100_2016$Name == "A.J. Pollock")
    top100_2016$Name[20] = "AJ Pollock"
    which(full_batting_data[[8]]$Name == "Dee Strange-Gordon")
    full_batting_data[[8]]$Name[1317] = "Dee Gordon"
setdiff(top100_2016$Name, full_batting_data[[8]]$Name)

setdiff(top100_2017$Name, full_batting_data[[9]]$Name)
    which(top100_2017$Name == "Zach Britton")
    top100_2017$Name[39] = "Zack Britton"
    which(top100_2017$Name == "A.J. Pollock")
    top100_2017$Name[46] = "AJ Pollock"
    which(top100_2017$Name == "D.J. LeMahieu")
    top100_2017$Name[63] = "DJ LeMahieu"
setdiff(top100_2017$Name, full_batting_data[[9]]$Name)

setdiff(top100_2018$Name, full_batting_data[[10]]$Name)
    which(top100_2018$Name == "D.J. LeMahieu")
    top100_2018$Name[81] = "DJ LeMahieu"
    newrow = c("Shohei Ohtani", 957, 23, "LAA", "AL", rep(0,24), "1", rep(0,33))
    full_batting_data[[10]] = rbind(full_batting_data[[10]][1:956,],newrow,full_batting_data[[10]][-(1:956),])
setdiff(top100_2018$Name, full_batting_data[[10]]$Name)  

setdiff(top100_2019$Name, full_batting_data[[11]]$Name)

setdiff(top100_2020$Name, full_batting_data[[12]]$Name)
    which(top100_2020$Name == "D.J. LeMahieu")
    top100_2020$Name[37] = "DJ LeMahieu"
    which(top100_2020$Name == "Hyun-Jin Ryu")
    top100_2020$Name[53] = "Hyun Jin Ryu"
    which(top100_2020$Name == "Nick Castellanos")
    top100_2020$Name[71] = "Nicholas Castellanos"
setdiff(top100_2020$Name, full_batting_data[[12]]$Name)

## Merge Top 100 List with Batting Dataset
for(i in 1:12){
  full_batting_data[[i]]$Top100Indicator = rep(NA, nrow(full_batting_data[[i]]))
}

  # Joining By Name
  full_batting_data[[3]] = join(full_batting_data[[3]], top100_2011, by = "Name")
  full_batting_data[[4]] = join(full_batting_data[[4]], top100_2012, by = "Name")
  full_batting_data[[5]] = join(full_batting_data[[5]], top100_2013, by = "Name")
  full_batting_data[[6]] = join(full_batting_data[[6]], top100_2014, by = "Name")
  full_batting_data[[7]] = join(full_batting_data[[7]], top100_2015, by = "Name")
  full_batting_data[[8]] = join(full_batting_data[[8]], top100_2016, by = "Name")
  full_batting_data[[9]] = join(full_batting_data[[9]], top100_2017, by = "Name")
  full_batting_data[[10]] = join(full_batting_data[[10]], top100_2018, by = "Name")
  full_batting_data[[11]] = join(full_batting_data[[11]], top100_2019, by = "Name")
  full_batting_data[[12]] = join(full_batting_data[[12]], top100_2020, by = "Name")

  # Modifying Top 100 Indicator Variable
    for(i in 3:12){
      full_batting_data[[i]]$Top100Indicator = ifelse(is.na(full_batting_data[[i]]$`Top 100 Rank`) == FALSE, 1, 0)
    }

  # Removing Non-Top 100 Players OR Players w/ less than 502 AB
  for(i in 3:12){
    full_batting_data[[i]] = full_batting_data[[i]][!(full_batting_data[[i]]$Top100Indicator == 0 & 
                                                        (full_batting_data[[i]]$AB < 502 | 
                                                           is.na(full_batting_data[[i]]$AB == TRUE))),] 
  }

 

  
  
  