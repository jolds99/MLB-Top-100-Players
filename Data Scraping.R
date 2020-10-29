## Loading Packages
  library(rvest)
  library(XML)
  library(tidyverse)
  library(plyr)
  library(dplyr)
  # Use if dplyr doesn't work
  detach(package:plyr)
  detach(package:dplyr)
    

## Creating Vector of Years for Data Scraping
  years = 2008:2019

## Scraping Standard Batting Tables From 2008-2019
  standard_batting_tables_scrape_function = function(years){
    df = list()
    ## Obtaining Standard Batting Data By Year
    for(i in 1:length(years)){
        url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-standard-batting.shtml"))
        data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
            html_text() %>%    # extracting comment text
            paste(collapse = '') %>%    # collapsing to a single string
            read_html() %>%
            html_node('table') %>%    # selecting the desired table
            html_table() 
        ## Removing Header Rows
        index = seq(0, nrow(data), by=26)
        data = data[-index,]
        df[[i]] = data
    }
    ## Returning List of Dataframes
   return(df)
  }
  
  sb_stats = standard_batting_tables_scrape_function(years)

## Scraping Advanced Batting Tables 2008-2019
  advanced_batting_tables_scrape_function = function(years){
    df = list()
    ## Obtaining Advanced Batting Data By Year
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-advanced-batting.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_node('table') %>%    # selecting the desired table
        html_table() 
      ## Removing Header Rows
      index = seq(0, nrow(data), by=26)
      data = data[-index,]
      df[[i]] = data
    }
    ## Returning List of Dataframes
    return(df)
  }
  
  ab_stats = advanced_batting_tables_scrape_function(years)

## Scraping Value Batting Tables 2008-2019
  value_batting_tables_scrape_function = function(years){
    df = list()
    ## Obtaining Value Batting Data By Year
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-value-batting.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_node('table') %>%    # selecting the desired table
        html_table() 
      ## Removing Header Rows
      index = seq(0, nrow(data), by=26)
      data = data[-index,]
      df[[i]] = data
    }
    ## Returning List of Dataframes
    return(df)
  }
  vb_stats = value_batting_tables_scrape_function(years)

## Scraping Standard Pitching Tables 2008-2019
  standard_pitching_tables_scrape_function = function(years){
    df = list()
    ## Obtaining Standard Pitching Data By Year
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-standard-pitching.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_node('table') %>%    # selecting the desired table
        html_table() 
      ## Removing Header Rows
      index = seq(0, nrow(data), by=26)
      data = data[-index,]
      df[[i]] = data
    }
    ## Returning List of Dataframes
    return(df)
  }
  sp_stats = standard_pitching_tables_scrape_function(years)

## Scraping Value Pitching Tables 2008-2019
  value_pitching_tables_scrape_function = function(years){
    df = list()
    ## Obtaining Advanced Pitching Data By Year
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-value-pitching.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_node('table') %>%    # selecting the desired table
        html_table() 
      ## Removing Header Rows
      index = seq(0, nrow(data), by=26)
      data = data[-index,]
      df[[i]] = data
    }
    ## Returning List of Dataframes
    return(df)
  }
  vp_stats = value_pitching_tables_scrape_function(years)

## Scraping Standard Fielding Tables 2008-2019
  standard_fielding_tables_scrape_function = function(years){
    df = list()
    ## Obtaining Advanced Pitching Data By Year
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/leagues/MLB/",years[i],"-standard-fielding.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_node('table') %>%    # selecting the desired table
        html_table() 
      ## Removing Header Rows 
      index = seq(0, nrow(data), by=26)
      data = data[-index,]
      df[[i]] = data
    }
    ## Returning List of Dataframes
    return(df)
  }
  sf_stats = standard_fielding_tables_scrape_function(years)

## Removing "*,#,+" from Player Names in All Datasets
  remove_junk_function = function(data){
    for(i in 1:12){
    x = data[[i]]$Name
    for(j in 1:length(x)){
      x[j] = gsub("[*]", "", x[j]) # removing '*' character
    }
    for(j in 1:length(x)){
      x[j] = gsub("[#]", "", x[j]) # removing '#' character
    }
    for(j in 1:length(x)){
      x[j] = gsub("[+]", "", x[j]) # removing '+' character
    }
    for(j in 1:length(x)){
      x[j] = stringi::stri_trans_general(x[j], "Latin-ASCII") # removing Latin characters
    }
    data[[i]]$Name = x
    }
    data
  }

  # Running each list of dataframes through remove junk function
  sb_stats = remove_junk_function(sb_stats)
  ab_stats = remove_junk_function(ab_stats)
  vb_stats = remove_junk_function(vb_stats)
  sp_stats = remove_junk_function(sp_stats)
  vp_stats = remove_junk_function(vp_stats)
  sf_stats = remove_junk_function(sf_stats)

## Pulling In Top 100 Players List 2011-2020
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
  # Creating function to merge datasets
  merge_batting_datasets_function = function(dataset){
  for(i in 1:12){
  dataset[[i]] = join(sb_stats[[i]], ab_stats[[i]], by = "Name", match = "first") # joining sb stats and ab stats by year
  dataset[[i]] = join(dataset[[i]], vb_stats[[i]], by = "Name", type = "full", match = "first") # joining sb,ab stats with vb stats by year
  dataset[[i]] = dataset[[i]][order(dataset[[i]]$Name),]
  }
  dataset
  }
  
  # Creating full batting data set with function
  full_batting_data = merge_batting_datasets_function(full_batting_data)
  
## Cleaning up Dataset
  for(i in 1:12){
     # Remove LgAvg Row
     full_batting_data[[i]] = full_batting_data[[i]][!(full_batting_data[[i]]$Name=="LgAvg per 600 PA"),]
     # Remove Unneccessary Columns
     full_batting_data[[i]] = full_batting_data[[i]][,c(1,3:26,29:30,32:33,35,40:42,52:53,55:56,59:60)]
     # Make Most Variables Numeric
     full_batting_data[[i]][,c(2,5:26,28:37)] <- lapply(full_batting_data[[i]][,c(2,5:26,28:37)], as.numeric)
   }
 
## Removing Duplicate Names
   remove_duplicate_name_function = function(dataset){
     for(i in 1:12){
     occurences = data.frame(table(dataset[[i]]$Name))
     duplicates = dataset[[i]][dataset[[i]]$Name %in% 
                                     occurences$Var1[occurences$Freq > 1],] # finding names that appear more than once
     duplicates = duplicates %>% 
       group_by(Name,Age) %>% 
       mutate(Lg = case_when(Tm == "TOT" ~ Lg[order(G)[2]], TRUE ~ Lg),
              Tm=case_when(Tm == "TOT" ~ Tm[order(G)[2]],TRUE ~ Tm)) %>% # changing team name of duplicate with "TOT" as team name, then selecting that row
       slice(1)
        non_duplicates = dataset[[i]][!(duplicated(dataset[[i]]$Name) | 
                                               duplicated(dataset[[i]]$Name, fromLast = TRUE)), ] # creating dataset of only non-duplicate names
     non_duplicates = rbind(non_duplicates, duplicates) # merging duplicate with non-duplicate sets
     non_duplicates = non_duplicates[order(non_duplicates$Name),]
     dataset[[i]] = non_duplicates
     }
     dataset
   }
  # Removing duplicates in full batting data with function
  full_batting_data = remove_duplicate_name_function(full_batting_data)
 
## Merging Pitching Datasets
  full_pitching_data = list()
  # Creating function to merge datasets
  merge_pitching_datasets_function = function(dataset){
   for(i in 1:12){
     dataset[[i]] = join(sp_stats[[i]], vp_stats[[i]], by = "Name", type = "full", match = "first") # joining sb stats with vp stats by season
     dataset[[i]] = dataset[[i]][order(dataset[[i]]$Name),]
    }
   dataset
  }
 # Creating full pitching data with function
 full_pitching_data = merge_pitching_datasets_function(full_pitching_data)
 
## Cleaning up Dataset
 for(i in 1:12){
   # Removing Lg Avg Variable
   full_pitching_data[[i]] = full_pitching_data[[i]][!(full_pitching_data[[i]]$Name=="LgAvg per 180 IP"),]
   # Removing Unnecessary Variables
   full_pitching_data[[i]] = full_pitching_data[[i]][,c(1,3:23,27:36,38,42:49)]
   # Make Most Variables Numeric
   full_pitching_data[[i]][,c(2,5:41)] <- lapply(full_pitching_data[[i]][,c(2,5:41)], as.numeric)
 }
 
## Removing Duplicate Names
 # Removing duplicate names in full pitching data with function
 full_pitching_data = remove_duplicate_name_function(full_pitching_data)
 
## Renaming Fielding Data
 full_fielding_data = sf_stats
 
## Cleaning Up Dataset
 for(i in 1:12){
   ## Sorting Alphabetically By Name
   full_fielding_data[[i]] = full_fielding_data[[i]][order(full_fielding_data[[i]]$Name),]
   ## Removing Lg Avg Row
   full_fielding_data[[i]] = full_fielding_data[[i]][!(full_fielding_data[[i]]$Name=="LgAvg"),]
   # Removing Unnecessary Variables
   full_fielding_data[[i]] = full_fielding_data[[i]][,c(2:23)]
   # Make Most Variables Numeric
   full_fielding_data[[i]][,c(2,5:21)] <- lapply(full_fielding_data[[i]][,c(2,5:21)], as.numeric)
 }
 
## Merging full_batting_data, full_pitching_data, full_fielding_data to create one dataset
 # Adding season variable to each data set
 for(i in 1:12){
   full_batting_data[[i]]$Season = i + 2007
   full_pitching_data[[i]]$Season = i + 2007
   full_fielding_data[[i]]$Season = i + 2007
 }
 
 # Merging functions into one dataset each, with season as column
 full_batting_data = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), full_batting_data)
 full_pitching_data = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), full_pitching_data)
 full_fielding_data = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), full_fielding_data)
 
 # Creating team name vectors
 nl_team_names = c("ARI", "ATL", "CHC", "CIN", "COL", "LAD", "MIA", "MIL",
                   "NYM", "PHI", "PIT", "SDP", "SFG", "STL", "WSN")
 al_team_names = c("BAL", "BOS", "CHW", "CLE", "DET", "HOU", "KCR", "LAA",
                   "MIN", "NYY", "OAK", "SEA", "TBR", "TEX", "TOR")
 
 # Defining 'not in' function
 '%!in%' <- function(x,y)!('%in%'(x,y))
 
 # Creating function to fix incorrect league values
 correct_leagues_fill_na_function = function(dataset){
 for(i in 1:nrow(dataset)){
     if(dataset$Tm[i] %in% nl_team_names){
       dataset$Lg[i] = "NL"
     }
     if(dataset$Tm[i] %in% al_team_names){
       dataset$Lg[i] = "AL"
     }
     if(dataset$Tm[i] %!in% al_team_names && dataset$Tm[i] %!in% nl_team_names){
       dataset$Lg[i] = "MLB"
     }
     dataset[is.na(dataset)] = 0
   }
   dataset
 }
 # Applying function to each dataset
 full_batting_data = correct_leagues_fill_na_function(full_batting_data)
 full_pitching_data = correct_leagues_fill_na_function(full_pitching_data)
 full_fielding_data = correct_leagues_fill_na_function(full_fielding_data)
 
## Reading in Fangraphs Data
fangraphs_batting_data = read_csv("Fangraphs Data/FanGraphs Batting Stats Leaderboard.csv")
fangraphs_pitching_data = read_csv("Fangraphs Data/FanGraphs Pitching Stats Leaderboard.csv")

which(colnames(full_batting_data) == "WAR")
colnames(full_batting_data)[37] = "Br_WAR"

which(colnames(full_pitching_data) == "WAR")
colnames(full_pitching_data)[38] = "Br_WAR"
 
## Merging Full Batting with Fangraphs Batting
  # Creating function to find distance between strings for differences in names
  distance_function <- function(a,b,d){
  x <- numeric(length(a)) # assign variable: 0 for unassigned but assignable, 
  # 1 for already assigned, -1 for unassigned and unassignable
  while(any(x==0)){
    min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
    a_sel <- a[d==min_d & x==0][1] 
    b_sel <- b[d==min_d & a == a_sel & x==0][1] 
    x[a==a_sel & b == b_sel] <- 1
    x[x==0 & (a==a_sel|b==b_sel)] <- -1
  }
  cbind(a=a[x==1],b=b[x==1],d=d[x==1])
  }
  
  # Finding differences in names across datasets
  x = setdiff(full_batting_data$Name, fangraphs_batting_data$Name)
  y = setdiff(fangraphs_batting_data$Name, full_batting_data$Name)
 
  # Creating name dictionary between full & fangraphs data to change misspellings
  create_name_dictionary_function = function(x,y){ 
  library(stringdist)
  distance_chart <- expand.grid(x,y) 
  names(distance_chart) <- c("BR_Name","FG_Name")
  distance_chart$distance <- stringdist(distance_chart$BR_Name, distance_chart$FG_Name, method="jw")
  dictionary = data.frame(distance_function(as.character(distance_chart$BR_Name),as.character(distance_chart$FG_Name),distance_chart$distance)) # Creating dataframe of names plus distance
  dictionary$a = as.character(dictionary$a)
  dictionary$b = as.character(dictionary$b)
  dictionary
  }

  # Applying function to create dictionary
  test = create_name_dictionary_function(x,y)
  # Removing Jose Miguel Fernandez/Javier Guerra b/c error didn't come from alternate spelling
  test = test[-83,]

  # Changing names that were misspelled in Fangraphs 
    # Creating function
    order_names_function = function(dataset,test){
      for(i in 1:length(dataset$Name)){
        for(j in 1:length(test$b)){
          if(dataset$Name[i] == test$b[j]){
            dataset$Name[i] = test$a[j]
          }
        }
      }
      dataset
    }
    # Applying functions to fangraphs batting data
   fangraphs_batting_data = order_names_function(fangraphs_batting_data,test)

  # Reordering datasets by name and season
  fangraphs_batting_data = fangraphs_batting_data[order(fangraphs_batting_data$Name,
                                                          fangraphs_batting_data$Season),]
  full_batting_data = full_batting_data[order(full_batting_data$Name,
                                              full_batting_data$Season),]

  # Finding any remaining differences in names
  setdiff(full_batting_data$Name, fangraphs_batting_data$Name)
  setdiff(fangraphs_batting_data$Name, full_batting_data$Name)

  # Changing names of three rows due to misnaming in dataset
    # Fangraphs change
    which(full_batting_data$Name == "Jose Fernandez")
    which(fangraphs_batting_data$Name == "Jose Fernandez")
    fangraphs_batting_data$Name[8660] = "Jose Miguel Fernandez"
  
    # FullBatting change
    which(fangraphs_batting_data$Name == "Javy Guerra")
    which(full_batting_data$Name == "Javy Guerra")
    full_batting_data$Name[c(7278,7280)] = "Javier Guerra"

  # Finding source of 8 extra rows - where fangraphs data has extra row for certain players
    full_name_count = full_batting_data %>% group_by(Name) %>% summarise(n = n())
    fangraphs_name_count = fangraphs_batting_data %>% group_by(Name) %>% summarise(n = n())
    
    # Combining frequency of names by dataset - full and fangraphs - to find differences
    name_count = as.data.frame(cbind(full_name_count,fangraphs_name_count))
  
    # Adding Extra 'Chris Carpenter' Row
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_batting_data[which(full_batting_data$Name == "Chris Carpenter"),] # locating full data with name
    fangraphs_misname = fangraphs_batting_data[which(fangraphs_batting_data$Name == "Chris Carpenter"),] # locating fangraphs data with name
    which(fangraphs_batting_data$Name == "Chris Carpenter") # finding extra row in fangraphs
    full_batting_data[15850,c(1,6,40)] = fangraphs_batting_data[3010,c(2,3,1)] # adding data from extra row to full data
  
    # Adding Extra 'Chris Smith' Row
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_batting_data[which(full_batting_data$Name == "Chris Smith"),] # locating full data with name
    fangraphs_misname = fangraphs_batting_data[which(fangraphs_batting_data$Name == "Chris Smith"),] # locating fangraphs data with names
    which(fangraphs_batting_data$Name == "Chris Smith") # finding extra row in fangraphs
    full_batting_data[15851,c(1,6,40)] = fangraphs_batting_data[3251,c(2,3,1)] # adding data from extra row to full data
    
    # Adding 2 Extra 'David Carpenter' Rows
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_batting_data[which(full_batting_data$Name == "David Carpenter"),] # locating full data with name
    fangraphs_misname = fangraphs_batting_data[which(fangraphs_batting_data$Name == "David Carpenter"),] # locating fangraphs data with name
    which(fangraphs_batting_data$Name == "David Carpenter") # finding two extra rows
    full_batting_data[15852,c(1,6,40)] = fangraphs_batting_data[4250,c(2,3,1)] # adding data from extra rows to full data
    full_batting_data[15853,c(1,6,40)] = fangraphs_batting_data[4253,c(2,3,1)]
  
    # Adding Extra 'Francisco Rodriguez' Row
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_batting_data[which(full_batting_data$Name == "Francisco Rodriguez"),] # locating full data with name
    fangraphs_misname = fangraphs_batting_data[which(fangraphs_batting_data$Name == "Francisco Rodriguez"),] # locating fangraphs data with name
    which(fangraphs_batting_data$Name == "Francisco Rodriguez") # finding extra row
    full_batting_data[15854,c(1,6,40)] = fangraphs_batting_data[5666,c(2,3,1)] # adding data from extra rows to full data
  
    # Adding Extra 'Jose Ramirez' Row
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_batting_data[which(full_batting_data$Name == "Jose Ramirez"),] # locating full data with name
    fangraphs_misname = fangraphs_batting_data[which(fangraphs_batting_data$Name == "Jose Ramirez"),] # locating fangraphs data with name
    which(fangraphs_batting_data$Name == "Jose Ramirez") # finding extra row
    full_batting_data[15855,c(1,6,40)] = fangraphs_batting_data[8740,c(2,3,1)] # adding data from extra rows to full data
    
    # Adding Extra 'Miguel Gonzalez' Row
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_batting_data[which(full_batting_data$Name == "Miguel Gonzalez"),] # locating full data with name
    fangraphs_misname = fangraphs_batting_data[which(fangraphs_batting_data$Name == "Miguel Gonzalez"),] # locating fangraphs data with name
    which(fangraphs_batting_data$Name == "Miguel Gonzalez") # finding extra row
    full_batting_data[15856,c(1,6,40)] = fangraphs_batting_data[11309,c(2,3,1)] # adding data from extra row to full data
    
    # Adding Extra 'Rich Thompson' Row
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_batting_data[which(full_batting_data$Name == "Rich Thompson"),] # locating full data with name
    fangraphs_misname = fangraphs_batting_data[which(fangraphs_batting_data$Name == "Rich Thompson"),] # locating fangraphs data with name
    which(fangraphs_batting_data$Name == "Rich Thompson") # finding extra row
    full_batting_data[15857,c(1,6,40)] = fangraphs_batting_data[12809,c(2,3,1)] # adding data from extra row to full data

  # Merging Datasets
    
    # calling merged data set 'new' to test for merging accuracy, merging by name and plate appearances
  new = join(full_batting_data, fangraphs_batting_data, by = c("Name", "PA"), type = "full", match = "first")
  
  # Transferring a 'Beau Taylor' row to correct location
  which(new$Name == "Beau Taylor" & new$Season == 2019)
  new[1293,41:77] = new[15858,41:77]
  
  # Transferring a 'Cody Bellinger' row to correct location
  which(new$Name == "Cody Bellinger" & new$Season == 2019)
  new[3509,41:77] = new[15859,41:77]
  
  # Transferring a 'Cody Ransom' row to correct location 
  which(new$Name == "Cody Ransom" & new$Season == 2012)
  new[3526,41:77] = new[15860,41:77]
  
  # Transferring a 'Dee Strange-Gordon' row to correct location
  which(new$Name == "Dee Strange-Gordon" & new$Season == 2016)
  new[4447,41:77] = new[15861,41:77]
  
  # Transferring a 'Devon Travis' row to correct location
  which(new$Name == "Devon Travis" & new$Season == 2015)
  new[4583,41:77] = new[15862,41:77]
  
  # Transferring a 'Erick Aybar' row to correct location
  which(new$Name == "Erick Aybar" & new$Season == 2014)
  new[5339,41:77] = new[15863,41:77]
  
  # Transferring a 'Jon Jay' row to correct location
  which(new$Name == "Jon Jay" & new$Season == 2016)
  new[8311,41:77] = new[15864,41:77]
  
  # Transferring a 'Lyle Overbay' row to correct location
  which(new$Name == "Lyle Overbay" & new$Season == 2013)
  new[10279,41:77] = new[15865,41:77]

  # Transferring a 'Michael Crotta' row to correct location
  which(new$Name == "Michael Crotta" & new$Season == 2011)
  new[11144,41:77] = new[15866,41:77]
  
  # Transferring a 'Mike Ryan' row to correct location
  which(new$Name == "Mike Ryan" & new$Season == 2010)
  new[11576,41:77] = new[15867,41:77]
  
  # Transferring a 'Ronald Torreyes' row to correct location
  which(new$Name == "Ronald Torreyes" & new$Season == 2016)
  new[13091,41:77] = new[15868,41:77]
  
  # Removing old, unecessary rows
  new = new[-(15858:15868),]
  # Renaming full batting data to become merged dataset
  full_batting_data = new
  
## Merging Full Pitching with Fangraphs Pitching
  # Finding differences in names across datasets
  x = setdiff(full_pitching_data$Name, fangraphs_pitching_data$Name)
  y = setdiff(fangraphs_pitching_data$Name, full_pitching_data$Name)
  
  # Creating name dictionary between full & fangraphs data to change misspellings
  test = create_name_dictionary_function(x,y)

  # Changing names that were mispelled in fangraphs 
  fangraphs_pitching_data = order_names_function(fangraphs_pitching_data,test)
  
  # Reordering datasets by name and season
  fangraphs_pitching_data = fangraphs_pitching_data[order(fangraphs_pitching_data$Name,
                                                        fangraphs_pitching_data$Season),]
  full_pitching_data = full_pitching_data[order(full_pitching_data$Name,
                                              full_pitching_data$Season),]
  
  # Checking for any other name errors
  setdiff(full_pitching_data$Name, fangraphs_pitching_data$Name)
  
  setdiff(fangraphs_pitching_data$Name, full_pitching_data$Name)

  # Changing one Javy Guerra row to Javier Guerra due to error in fangraphs
  which(fangraphs_pitching_data$Name == "Javy Guerra")
  which(full_pitching_data$Name == "Javy Guerra")
  full_pitching_data$Name[3852] = "Javier Guerra"

  # Finding source of extra row - where fangraphs data has extra row for certain players
    full_name_count = full_pitching_data %>% group_by(Name) %>% summarise(n = n())
    fangraphs_name_count = fangraphs_pitching_data %>% group_by(Name) %>% summarise(n = n())
    
    # Combining frequency of names by dataset - full and fangraphs - to find differences 
    name_count = as.data.frame(cbind(full_name_count,fangraphs_name_count))
    
    # Removing Extra 'Miguel Gonzalez' Row
    name_count[which((full_name_count$n == fangraphs_name_count$n) == FALSE),] # locating differences
    full_misname = full_pitching_data[which(full_pitching_data$Name == "Miguel Gonzalez"),] # locating full data with name
    fangraphs_misname = fangraphs_pitching_data[which(fangraphs_pitching_data$Name == "Miguel Gonzalez"),] # locating fangraphs data with name
    which(fangraphs_pitching_data$Name == "Miguel Gonzalez") # finding extra row
    full_pitching_data[8507,c(1,8,40)] = fangraphs_pitching_data[6030,c(2,3,1)] # adding data from extra row to full
  
  # Merging Datasets
    # calling merged dataset 'new' to test merging accuracy
    new = join(full_pitching_data, fangraphs_pitching_data, by = c("Name", "ERA"), type = "full", match = "first")

    # Transferring a 'Andrew Vasquez' row to correct location
    which(new$Name == "Andrew Vasquez" & new$Season == 2019)
    new[422,c(2,43:70)] = new[8508,c(2,43:70)]
    
    # Transferring a 'Daniel McCutchen' row to correct location
    which(new$Name == "Daniel McCutchen" & new$Season == 2012)
    new[2137,c(2,43:70)] = new[8509,c(2,43:70)]
    
    # Transferring a 'Dustin Moseley' row to correct location
    which(new$Name == "Dustin Moseley" & new$Season == 2010)
    new[2594,43:70] = new[8510, 43:70]

    # Transferring a 'Gerardo Parra' row to correct location
    which(new$Name == "Gerardo Parra" & new$Season == 2019)
    new[3111,c(2,43:70)] = new[8511,c(2,43:70)]
    
    # Transferring a 'J.P. Howell' row to correct location
    which(new$Name == "J.P. Howell" & new$Season == 2013)
    new[3465,43:70] = new[8512,43:70]
    
    # Transferring a 'Jeremy Bonderman' row to correct location
    which(new$Name == "Jeremy Bonderman" & new$Season == 2010)
    new[4019,43:70] = new[8513,43:70]
    
    # Transferring a 'Jim Henderson' row to correct location
    which(new$Name == "Jim Henderson" & new$Season == 2016)
    new[4155,43:70] = new[8514,43:70]
    
    # Transferring a 'Joel Zumaya' row to correct location
    which(new$Name == "Joel Zumaya" & new$Season == 2010)
    new[4353,43:70] = new[8515,43:70]
    
    # Transferring a 'John Lackey' row to correct location
    which(new$Name == "John Lackey" & new$Season == 2010)
    new[4411,43:70] = new[8516,43:70]
    
    # Transferring a 'Matt Koch' row to correct location
    which(new$Name == "Matt Koch" & new$Season == 2017)
    new[5816,c(2,43:70)] = new[8517,c(2,43:70)]
    
    # Transferring a 'Nate Jones' row to correct location
    which(new$Name == "Nate Jones" & new$Season == 2014)
    new[6223,c(2,43:70)] = new[8518,c(2,43:70)]
    
    # Transferring a 'Nick Greenwood' row to correct location
    which(new$Name == "Nick Greenwood" & new$Season == 2015)
    new[6304,c(2,43:70)] = new[8519,c(2,43:70)]
    
    # Transferring a 'Rex Brothers' row to correct location
    which(new$Name == "Rex Brothers" & new$Season == 2018)
    new[6727,c(2,43:70)] = new[8520,c(2,43:70)]
    
    # Transferring a 'Stephen Fife' row to correct location
    which(new$Name == "Stephen Fife" & new$Season == 2013)
    new[7519,43:70] = new[8521,43:70]
    
    # Transferring a 'Zack Weiss' row to correct location
    which(new$Name == "Zack Weiss" & new$Season == 2018)
    new[8499,c(2,43:70)] = new[8522,c(2,43:70)]
    
    # Getting rid of old, unecessary rows
    new = new[-(8508:15822),]
    # Renaming full pitching data to become merged dataset
    full_pitching_data = new
    
  ## Ordering Datasets by Name and Season
  full_batting_data = full_batting_data[order(full_batting_data$Name,
                                                full_batting_data$Season),]
  full_pitching_data = full_pitching_data[order(full_pitching_data$Name,
                                                full_pitching_data$Season),]
  
## Merging Batting and Pitching Data Together
  # Finding common columns betwen batting and pitching, renaming to specify whether they apply for batting or pitching
    # Finding common columns
    target_batpitch = Reduce(intersect, list(colnames(full_batting_data), colnames(full_pitching_data)))
    target_batpitch
    target_batpitch = target_batpitch[c(5:15,17:32)]
    # Renaming column names
    for(i in 1:length(target_batpitch)){
      colnames(full_batting_data)[which(names(full_batting_data) == target_batpitch[i])] = paste(target_batpitch[i],"_Bat", "")
      colnames(full_pitching_data)[which(names(full_pitching_data) == target_batpitch[i])] = paste(target_batpitch[i], "_Pitch", "")
    }
  
  # Moving PA and ERA Columns 
  full_batting_data = full_batting_data[, c(1,3:6,2,7:77)]
  full_pitching_data = full_pitching_data[, c(1,3:8,2,9:70)]
  
  # Joining full batting and full pitching datasets
  full_data = join(full_batting_data, full_pitching_data, by = c("Name", "Age"), type = "full", match = "first")
  
  # Fixing two name mistakes
  which(full_data$Name == "Chris Carpenter" & is.na(full_data$Age)) # locating row
  which(full_data$Name == "Chris Carpenter") # locating all possible rows
  cc = rbind(full_data[3011,], full_data[15858,]) # finding which data to swap
  full_data[3011,c(2:4,77:141)] = full_data[15858,c(2:4,77:141)] # swapping rows
  
  which(full_data$Name == "Rich Thompson" & is.na(full_data$Age)) # locating row
  which(full_data$Name == "Rich Thompson") # locating all possible rows
  rt = rbind(full_data[12810,], full_data[15859,]) # finding which data to swap
  full_data[12810,c(2:4,77:141)] = full_data[15859,c(2:4,77:141)] # swapping rows
  
  full_data = full_data[-(15858:15859),] # removing unnecessary rows
  full_data = full_data[order(full_data$Name),] # reordering dataset
  
  ## Adding Fielding Data to Full Data
    # Finding common columns between batting, pitching and fielding to specify which is which
    target_fullfield = Reduce(intersect, list(colnames(full_batting_data), colnames(full_fielding_data)))
    target_fullfield = c(target_fullfield, Reduce(intersect, list(colnames(full_pitching_data), colnames(full_fielding_data))))
    target_fullfield
    target_fullfield = target_fullfield[c(5,11,12)]
    
    # Renaming columns
    for(i in 1:length(target_fullfield)){
      colnames(full_fielding_data)[which(names(full_fielding_data) == target_fullfield[i])] = paste(target_fullfield[i],"_Field", "")
    }
    
    colnames(full_fielding_data)[5] = paste(colnames(full_fielding_data)[5],"_Field", "")
    
    ## Joining full fielding with full data
    full_data1 = join(full_data, full_fielding_data, by = c("Name", "Age"), type = "full", match = "first")
      # Transferring unnecessary extra data from two rows
      full_data1[7262,142:159] = full_data1[15858,142:159]
      full_data1[7263,142:159] = full_data1[15859,142:159]
      full_data1 = full_data1[-c(15858,15859),]
      
      # Renaming to full dataset
      full_data = full_data1
    
## Saving full data without all star or awards
  write_csv(full_data, "FullData.csv")

  
## Add in All Star Game Results
  # Function to scrape all star results from baseball reference
  all_star_voting_scrape_function = function(years){
    df = list()
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/allstar/",years[i],"-allstar-game.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_nodes('table') %>%    # selecting the desired table
        html_table()
      data_1 = data[[1]]
      data_1 = data_1[!(data_1$X2=="Manager"),] # removing manager row
      data_1 = data_1[!(data_1$X2=="Reserves"),] # removing reserve row
      data_2 = data[[2]]
      data_2 = data_2[!(data_2$X2=="Manager"),]
      data_2 = data_2[!(data_2$X2=="Reserves"),]
      data = rbind(data_1, data_2)
      data = data[,2:3]
      names(data)[1] <- "Name" # changing column names
      names(data)[2] <- "Position" 
      data$Season = years[i]
      df[[i]] = data
    }
    # Returning List of Dataframes
    df
  }
  
  all_star = all_star_voting_scrape_function(years)

  # Merging all star results by year to one dataset
  all_star = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), all_star)

  # Removing accents
  for(i in 1:nrow(all_star)){
  all_star$Name[i] = stringi::stri_trans_general(all_star$Name[i], "Latin-ASCII")
  }

  # Removing duplicate names
  all_star = all_star %>% group_by(Season) %>% filter(!duplicated(Name) | n()==1)
  
  # Checking for any name differences between allstar and full dataset
  setdiff(all_star$Name, full_data$Name)
  
  # Adding Indicator Variable 
  all_star$AllStar = 1

  ## Joining with Allstar
  full_data1 = join(full_data, all_star, by = c("Name", "Season"), type = "full", match = "all")
  
  ## Fixing extra rows
    # Finding duplicate names
    allstarfull = full_data1[which(full_data1$AllStar == 1), c(1,40,158)]
    
    # Getting rid of extra 'Chris Young' row
    length(which(allstarfull$Season == 2010)) # noticing extra row
    full_data1[which(full_data1$Name == "Chris Young"),c(1,3,40,161)] # finding extra row
    full_data1[3316,161] = NA # changing all star indicator for extra row to na
  
    # Getting rid of 2 extra 'Jose Ramirez' Rows
    length(which(allstarfull$Season == 2017)) # noticing extra row
    full_data1[which(full_data1$Name == "Jose Ramirez"),c(1,3,40,161)] # finding extra row
    full_data1[8746,161] = NA # changing all star indicator for extra row to na
    full_data1[8748,161] = NA # changing all star indicator for extra row to na
    
    # Getting rid of extra 'Will Smith' Row
    length(which(allstarfull$Season == 2019)) # noticing extra row
    full_data1[which(full_data1$Name == "Will Smith"),c(1,3,40,161)] # finding extra row
    full_data1[15389,161] = NA # changing all star indicator for extra row to na
    
    allstarfull = full_data1[which(full_data1$AllStar == 1), c(1,40,161)]
    
    # Renaming to apply changes
    full_data = full_data1
  
## Adding Golden Glove & Silver Slugger
  # Function to scrape golden glove and silver slugger data
  gg_ss_voting_scrape_function = function(years){
    df_gg = list()
    df_ss = list()
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/leagues/NL/",years[i],"-other-leaders.shtml"))
      data = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_nodes('table') %>%    # selecting the desired table
        html_table()
      data_gg = data[[1]]
      data_gg = data_gg[,2]
      data_ss = data[[2]]
      data_ss = data_ss[,2]
      data = as.data.frame(cbind(data_gg, data_ss))
      names(data)[1] <- "GoldGlove" # changing column names
      names(data)[2] <- "SilverSlugger"
      data$Season = years[i]
      df_gg[[i]] = data[,c(1,3)]
      df_ss[[i]] = data[,c(2,3)]
    }
    # Returning List of Dataframes
    output = list(df_gg, df_ss)
    output
  }
  
  gg_ss_results = gg_ss_voting_scrape_function(years)
  
  gg_results = gg_ss_results[[1]]
  ss_results = gg_ss_results[[2]]
  
  # Merging gg/ss results by year to one dataset
  gg_results = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), gg_results)
  ss_results = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), ss_results)
  
  # Removing duplicate German Marquez row
  ss_results = ss_results[-51,]
  
  # Changing column names back to name
  names(gg_results)[1] = "Name"
  names(ss_results)[1] = "Name"
  
  # Removing accents
  for(i in 1:nrow(gg_results)){
    gg_results$Name[i] = stringi::stri_trans_general(gg_results$Name[i], "Latin-ASCII")
  }
  for(i in 1:nrow(ss_results)){
    ss_results$Name[i] = stringi::stri_trans_general(ss_results$Name[i], "Latin-ASCII")
  }
  
  
  # Checking for any name differences between gg_results and full dataset
  setdiff(gg_results$Name, full_data$Name)
  
  # Checking for any name differences between ss_results and full dataset
  setdiff(ss_results$Name, full_data$Name)
  
  # Adding Gold Glove Indicator Variable
  gg_results$GoldGlove = 1
  
  # Adding Silver Slugger Indicator Variable
  ss_results$SilverSlugger = 1
  
  # Joining with gg_results
  full_data1 = join(full_data, gg_results, by = c("Name", "Season"), type = "full", match = "all")
  
  # Confirming no errors
  length(which(full_data1$GoldGlove == 1))
  
  # Renaming to apply changes
  full_data = full_data1
  
  # Joining with ss_results
  full_data1 = join(full_data, ss_results, by = c("Name", "Season"), type = "full", match = "all")
  
  # Confirming no erros
  length(which(full_data1$SilverSlugger == 1))
  
  # Renaming to apply changes
  full_data = full_data1
  
## Adding MVP, CY Young, ROY 
  # Function to scrape mvp, cy young, roy data
  mvp_cy_roy_voting_scrape_function = function(years){
    df_mvp = list()
    df_cy = list()
    df_roy= list()
    for(i in 1:length(years)){
      url = read_html(paste("https://www.baseball-reference.com/awards/awards_",years[i],".shtml"))
      data_mvp = url %>% 
        html_nodes('table') %>%    # selecting the desired table
        html_table()
      data_rest = url %>% html_nodes(xpath = '//comment()') %>%    # selecting comment nodes
        html_text() %>%    # extracting comment text
        paste(collapse = '') %>%    # collapsing to a single string
        read_html() %>%
        html_nodes('table') %>%    # selecting the desired table
        html_table()
      # Locating correct tables
      data_mvp_al = data_mvp[[1]] 
      data_mvp_nl = data_mvp[[2]]
      data_cy_al = data_rest[[1]]
      data_cy_nl = data_rest[[2]]
      data_roy_al = data_rest[[3]]
      data_roy_nl = data_rest[[4]]
      
      # Combining AL and NL 
      data_mvp = as.data.frame(rbind(data_mvp_al, data_mvp_nl))
      data_cy = as.data.frame(rbind(data_cy_al, data_cy_nl))
      data_roy = as.data.frame(rbind(data_roy_al, data_roy_nl))
      
      # Renaming column to 'Name'
      names(data_mvp)[2] = "Name"
      names(data_cy)[2] <- "Name"
      names(data_roy)[2] <- "Name"
      
      # Choosing necessary columns
      data_mvp = data_mvp[,1:6]
      data_cy = data_cy[,1:6]
      data_roy = data_roy[,1:6]

      # Removing 'Name' rows
      data_mvp = data_mvp[!(data_mvp$Name=="Name"),]
      data_cy = data_cy[!(data_cy$Name=="Name"),]
      data_roy = data_roy[!(data_roy$Name=="Name"),]
      
      # Changing column names
      colnames(data_mvp)[1:6] = c("MVPRank", "Name", "Tm", "MVPVotePoints", "MVP1stPlace", "MVPShare")
      colnames(data_cy)[1:6] = c("CYRank", "Name", "Tm", "CYVotePoints", "CY1stPlace", "CYShare")
      colnames(data_roy)[1:6] = c("ROYRank", "Name", "Tm", "ROYVotePoints", "ROY1stPlace", "ROYShare")
      
      # Adding season variable
      data_mvp$Season = years[i]
      data_cy$Season = years[i]
      data_roy$Season = years[i]
      
      df_mvp[[i]] = data_mvp
      df_cy[[i]] = data_cy
      df_roy[[i]] = data_roy
    }
    # Returning List of Dataframes
    output = list(df_mvp, df_cy, df_roy)
    output
  }
  
  mvp_cy_roy_results = mvp_cy_roy_voting_scrape_function(years)
  
  mvp_results = mvp_cy_roy_results[[1]]
  cy_results = mvp_cy_roy_results[[2]]
  roy_results = mvp_cy_roy_results[[3]]
  
  # Merging mvp/cy/roy results by year to one dataset
  mvp_results = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), mvp_results)
  cy_results = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), cy_results)
  roy_results = Reduce(function(x, y, ...) merge(x, y, all = TRUE,...), roy_results)
  
  # Removing accents
  for(i in 1:nrow(mvp_results)){
    mvp_results$Name[i] = stringi::stri_trans_general(mvp_results$Name[i], "Latin-ASCII")
  }
  for(i in 1:nrow(ss_results)){
    cy_results$Name[i] = stringi::stri_trans_general(cy_results$Name[i], "Latin-ASCII")
  }
  for(i in 1:nrow(ss_results)){
    roy_results$Name[i] = stringi::stri_trans_general(roy_results$Name[i], "Latin-ASCII")
  }
  
  # Checking for any name differences between mvp_results and full dataset
  setdiff(mvp_results$Name, full_data$Name)
  
  # Checking for any name differences between cy_results and full dataset
  setdiff(cy_results$Name, full_data$Name)
  
  # Checking for any name differences between roy_results and full dataset
  setdiff(roy_results$Name, full_data$Name)
  
  # Adding MVP Indicator Variable
  mvp_results$MVP_Q = 1
  
  # Adding CY Indicator Variable
  cy_results$CY_Q = 1
  
  # Adding ROY Indicator Variable
  roy_results$ROY_Q = 1
  
  # Joining with mvp_results
  full_data1 = join(full_data, mvp_results, by = c("Name", "Tm", "Season"), type = "full", match = "all")
  
  # Checking for merging errors
  length(which(full_data1$MVP_Q == 1))
  
  # Transferring a 'Yoenis Cespedes' row to correct location
  which(full_data1$Name == "Yoenis Cespedes" & full_data1$Season == 2015)
  full_data1[15618,164:168] = full_data1[15858,164:168]

  # Transferring a 'Hunter Pence' row to correct location
  which(full_data1$Name == "Hunter Pence" & full_data1$Season == 2011)
  full_data1[6382,164:168] = full_data1[15859,164:168]
  
  # Transferring a 'Justin Upton' row to correct location
  which(full_data1$Name == "Justin Upton" & full_data1$Season == 2017)
  full_data1[9322,164:168] = full_data1[15860,164:168]
  
  # Transferring a 'Matt Holliday' row to correct location
  which(full_data1$Name == "Matt Holliday" & full_data1$Season == 2009)
  full_data1[10823,164:168] = full_data1[15861,164:168]

  # Transferring a 'Carlos Beltran' row to correct location
  which(full_data1$Name == "Carlos Beltran" & full_data1$Season == 2011)
  full_data1[2426,164:168] = full_data1[15862,164:168]
  
  # Transferring a 'Mark Teixeira' row to correct location
  which(full_data1$Name == "Mark Teixeira" & full_data1$Season == 2008)
  full_data1[10545,164:168] = full_data1[15863,164:168]
  
  # Transferring a 'Victor Martinez' row to correct location
  which(full_data1$Name == "Victor Martinez" & full_data1$Season == 2009)
  full_data1[15170,164:168] = full_data1[15864,164:168]
  
  # Transferring a 'Hunter Pence' row to correct location
  which(full_data1$Name == "Hunter Pence" & full_data1$Season == 2012)
  full_data1[6383,164:168] = full_data1[15865,164:168]
  
  # Transferring a 'Manny Ramirez' row to correct location
  which(full_data1$Name == "Manny Ramirez" & full_data1$Season == 2008)
  full_data1[10368,164:168] = full_data1[15866,164:168]
  
  # Transferring a 'David Price' row to correct location
  which(full_data1$Name == "David Price" & full_data1$Season == 2015)
  full_data1[4381,164:168] = full_data1[15867,164:168]
  
  # Removing old, unecessary rows
  full_data1 = full_data1[-(15858:15867),]
  
  # Renaming to apply changes
  full_data = full_data1
  
  # Joining with cy_results
  full_data1 = join(full_data, cy_results, by = c("Name", "Tm", "Season"), type = "full", match = "all")
  
  # Checking for merging errors
  length(which(full_data1$CY_Q == 1))
  
  # Transferring a 'David Price' row to correct location
  which(full_data1$Name == "David Price" & full_data1$Season == 2015)
  full_data1[4381,169:173] = full_data1[15858,169:173]
  
  # Transferring a 'Jon Lester' row to correct location
  which(full_data1$Name == "Jon Lester" & full_data1$Season == 2014)
  full_data1[8326,169:173] = full_data1[15859,169:173]
  
  # Transferring a 'Justin Verlander' row to correct location
  which(full_data1$Name == "Justin Verlander" & full_data1$Season == 2017)
  full_data1[9334,169:173] = full_data1[15860,169:173]
  
  # Transferring a 'David Price' row to correct location
  which(full_data1$Name == "David Price" & full_data1$Season == 2014)
  full_data1[4380,169:173] = full_data1[15861,169:173]
  
  # Transferring a 'Roy Oswalt' row to correct location
  which(full_data1$Name == "Roy Oswalt" & full_data1$Season == 2010)
  full_data1[13172,169:173] = full_data1[15862,169:173]
  
  # Transferring a 'Cliff Lee' row to correct location
  which(full_data1$Name == "Cliff Lee" & full_data1$Season == 2010)
  full_data1[3451,169:173] = full_data1[15863,169:173]
  
  # Transferring a 'Andrew Miller' row to correct location
  which(full_data1$Name == "Andrew Miller" & full_data1$Season == 2016)
  full_data1[875,169:173] = full_data1[15864,169:173]
  
  # Removing old, unecessary rows
  full_data1 = full_data1[-(15858:15864),]

  # Renaming to apply changes
  full_data = full_data1
  
  # Joining with roy_results
  full_data1 = join(full_data, roy_results, by = c("Name", "Tm", "Season"), type = "full", match = "all")
  
  # Checking for merging errors
  length(which(full_data1$ROY_Q == 1))
  
  # # Transferring a 'Jose Iglesias' row to correct location
  which(full_data1$Name == "Jose Iglesias" & full_data1$Season == 2013)
  full_data1[8667,174:178] = full_data1[15858,174:178]
  
  # Removing old, unecessary row
  full_data1 = full_data1[-15858,]
  
  # Renaming to apply changes
  full_data = full_data1
  
## Saving full data Without Three Year Averages
write_csv(full_data, "InitialDataUpdated.csv")
  
  ## load data if needed

full_data = read_csv("InitialDataUpdated.csv")
  
## Removing Unnecessary Columns
  full_data = full_data[,-c(8,24:26,63,65,68:69,84,94,106,153,155,160)]
  
  
  ## Reorganizing Columns
  full_data = full_data[,c(1,69,2:4,36,5:8,15:16,9:11,22,12,17:20,26,37:50,21,24:25,51:52,28:31,53,32:34,62:68,27,
                           58,60,13:14,54:57,70:72,74:78,86,110:112,79:82,73,89,83:85,108:109,90:95,107,104:106,
                           116:118,119,87:88,120,122,121,98,96:97,99,102:103,101,100,123:140,144:145,141:143,
                           35,59,61,23,146:164)]

  
## Adding 3 Year Average/Total Variables
  # Writing necessary functions
  # Function to compute rolling three year average
  add_3yraverage = function(.data, variable){
    .data %>% group_by(playerid) %>%
      mutate(lag1=lag({{variable}}), # create variable with value from previous year
             lag2=lag({{variable}}, 2)) %>% # create variable with value from two years prior
      ungroup() %>%
      rowwise(playerid, Season) %>%
      mutate("{{variable}}_3yravg" := round(mean(c({{variable}}, lag1, lag2), na.rm = TRUE),3)) %>% # create rolling average of past three years 
      select(-c(lag1,lag2)) # remove variables with prior values
  }
  
  # Function to compute rolling three year total
  add_3yrtotal = function(.data, variable){
    .data %>% group_by(playerid) %>%
      mutate(lag1=lag({{variable}}), # create variable with value from previous year
             lag2=lag({{variable}}, 2)) %>% # create variable with value from two years prior
      ungroup() %>%
      rowwise(playerid, Season) %>%
      mutate("{{variable}}_3yrtotal" := round(sum(c({{variable}}, lag1, lag2), na.rm = TRUE),3)) %>% # create rolling sum of past three years
      select(-c(lag1,lag2)) # remove variables with prior values
  }
  
  # Function to compute overall total
  add_total = function(.data, variable){
    .data %>% group_by(playerid) %>%
      mutate("{{variable}}_total" := round(cumsum(ifelse(is.na({{variable}}),0,{{variable}})),3)) %>% # cumulative sum of values 
      ungroup() %>%
      rowwise(playerid, Season) 
  }
  
  # Removing extra space in column names
  for(i in 1:ncol(full_data)){
    colnames(full_data)[i] = gsub("\\s", "", colnames(full_data)[i])
  }
  
  full_data <- mutate_if(full_data, 
                 is.character, 
                 str_replace_all, pattern = "%", replacement = "")
 
  full_data[,c(23:34,95:101)] = lapply(full_data[,c(23:34,95:101)], as.numeric)
  full_data[,c(147,152,157)] = lapply(full_data[,c(142,152,157)], as.numeric)
  
  # Applying functions as necessary to each column
  fulldatatest = full_data %>%
    add_3yraverage(G_Bat) %>%
    add_3yraverage(PA) %>% 
    add_3yraverage(AB) %>% 
    add_3yraverage(H_Bat) %>% 
    add_3yraverage(BB_Bat) %>% 
    add_3yraverage(SO_Bat) %>% 
    add_3yraverage(`2B`) %>% 
    add_3yraverage(`3B`) %>% 
    add_3yraverage(HR_Bat) %>% 
    add_3yraverage(TB) %>% 
    add_3yraverage(RBI) %>% 
    add_3yraverage(BA) %>% 
    add_3yraverage(OBP) %>% 
    add_3yraverage(SLG) %>% 
    add_3yraverage(OPS) %>% 
    add_3yraverage(BAbip) %>% 
    add_3yraverage(`LD%_Bat`) %>% 
    add_3yraverage(`GB%_Bat`) %>% 
    add_3yraverage(`FB%_Bat`) %>% 
    add_3yraverage(`HR/FB`) %>% 
    add_3yraverage(`Pull%_Bat`) %>% 
    add_3yraverage(`Cent%_Bat`) %>% 
    add_3yraverage(`Oppo%_Bat`) %>%
    add_3yraverage(`Soft%_Bat`) %>% 
    add_3yraverage(`Med%_Bat`) %>% 
    add_3yraverage(`Hard%_Bat`) %>%
    add_3yraverage(`BB%`) %>% 
    add_3yraverage(`K%`) %>% 
    add_3yraverage(`BB/K`) %>% 
    add_3yraverage(wOBA) %>% 
    add_3yraverage(`OPS+`) %>% 
    add_3yraverage(RC) %>% 
    add_3yraverage(`RC/G`) %>% 
    add_3yraverage(wRC) %>% 
    add_3yraverage(`wRC+`) %>% 
    add_3yraverage(BtRuns) %>% 
    add_3yraverage(BtWins) %>% 
    add_3yraverage(RAA_Bat) %>%
    add_3yraverage(WAA_Bat) %>% 
    add_3yraverage(wRAA) %>% 
    add_3yraverage(RAR_Bat) %>%
    add_3yraverage(Br_WAR_Bat) %>%
    add_3yraverage(oWAR) %>%
    add_3yraverage(WAR_Bat) %>%
    add_3yraverage(RE24_Bat) %>% 
    add_3yraverage(REW_Bat) %>% 
    add_3yraverage(WPA_Bat) %>%
    add_3yraverage(pLI_Bat) %>%
    add_3yraverage(`WPA/LI_Bat`) %>%
    add_3yraverage(Clutch_Bat) %>% 
    add_3yraverage(`OWn%`) %>%
    add_3yraverage(Batting) %>% 
    add_3yraverage(Off) %>% 
    add_3yraverage(SB) %>%
    add_3yraverage(CS) %>%
    add_3yraverage(UBR) %>%
    add_3yraverage(wGDP) %>%
    add_3yraverage(wSB) %>% 
    add_3yraverage(BsR) %>%
    add_3yraverage(W) %>%
    add_3yraverage(L) %>%
    add_3yraverage(`W-L%`) %>% 
    add_3yraverage(G_Pitch) %>%
    add_3yraverage(GS) %>%
    add_3yraverage(CG) %>%
    add_3yraverage(SHO) %>%
    add_3yraverage(SV) %>%
    add_3yraverage(BF) %>%
    add_3yraverage(Balls) %>%
    add_3yraverage(Strikes) %>%
    add_3yraverage(Pitches) %>%
    add_3yraverage(IP) %>%
    add_3yraverage(H_Pitch) %>%
    add_3yraverage(R_Pitch) %>%
    add_3yraverage(ER) %>%
    add_3yraverage(ERA) %>%
    add_3yraverage(WHIP) %>%
    add_3yraverage(HR_Pitch) %>%
    add_3yraverage(BB_Pitch) %>%
    add_3yraverage(SO_Pitch) %>%
    add_3yraverage(RS) %>%
    add_3yraverage(`RS/9`) %>%
    add_3yraverage(H9) %>%
    add_3yraverage(HR9) %>%
    add_3yraverage(BB9) %>%
    add_3yraverage(SO9) %>%
    add_3yraverage(`SO/W`) %>%
    add_3yraverage(RA9) %>%
    add_3yraverage(`LOB%`) %>%
    add_3yraverage(`LD%_Pitch`) %>%
    add_3yraverage(`GB%_Pitch`) %>%
    add_3yraverage(`FB%_Pitch`) %>%
    add_3yraverage(`Soft%_Pitch`) %>%
    add_3yraverage(`Med%_Pitch`) %>%
    add_3yraverage(`Hard%_Pitch`) %>%
    add_3yraverage(BABIP) %>%
    add_3yraverage(`ERA+`) %>%
    add_3yraverage(FIP) %>%
    add_3yraverage(xFIP) %>%
    add_3yraverage(SIERA) %>%
    add_3yraverage(WAR_Pitch) %>%
    add_3yraverage(gmLI) %>%
    add_3yraverage(`RAA_Pitch`) %>%
    add_3yraverage(`WAA_Pitch`) %>%
    add_3yraverage(WAAadj) %>%
    add_3yraverage(`waaWL%`) %>%
    add_3yraverage(`162WL%`) %>%
    add_3yraverage(RAR_Pitch) %>%
    add_3yraverage(Br_WAR_Pitch) %>%
    add_3yraverage(WPA_Pitch) %>%
    add_3yraverage(RE24_Pitch) %>%
    add_3yraverage(REW_Pitch) %>%
    add_3yraverage(pLI_Pitch) %>%
    add_3yraverage(`WPA/LI_Pitch`) %>%
    add_3yraverage(Clutch_Pitch) %>%
    add_3yraverage(SD) %>%
    add_3yraverage(MD) %>%
    add_3yraverage(G_Field) %>%
    add_3yraverage(GS_Field) %>%
    add_3yraverage(CG_Field) %>%
    add_3yraverage(Inn) %>%
    add_3yraverage(Ch) %>%
    add_3yraverage(PO) %>%
    add_3yraverage(A) %>%
    add_3yraverage(E) %>%
    add_3yraverage(DP) %>%
    add_3yraverage(`Fld%`) %>%
    add_3yraverage(`RF/9`) %>%
    add_3yraverage(`RF/G`) %>%
    add_3yraverage(Rtot) %>%
    add_3yraverage(Rdrs) %>%
    add_3yraverage(Rgood) %>%
    add_3yraverage(dWAR) %>%
    add_3yraverage(Fielding) %>%
    add_3yraverage(Def) %>%
    add_total(AllStar) %>%
    add_total(GoldGlove) %>%
    add_total(SilverSlugger) %>%
    add_3yraverage(MVPRank) %>%
    add_3yraverage(MVPVotePoints) %>%
    add_3yraverage(MVP1stPlace) %>%
    add_3yraverage(MVPShare) %>%
    add_total(MVP_Q) %>%
    add_3yraverage(CYRank) %>%
    add_3yraverage(CYVotePoints) %>%
    add_3yraverage(CY1stPlace) %>%
    add_3yraverage(CYShare) %>%
    add_total(CY_Q) %>%
    add_3yraverage(ROYRank) %>%
    add_3yraverage(ROYVotePoints) %>%
    add_3yraverage(ROY1stPlace) %>%
    add_3yraverage(ROYShare) %>%
    add_total(ROY_Q)
  
    # Renaming to apply changes
    full_data = fulldatatest
    
## Modifying Top 100 Lists to Add Join Variables
    # Adding Season Variable
    top100_2011$Season = 2010
    top100_2012$Season = 2011
    top100_2013$Season = 2012
    top100_2014$Season = 2013
    top100_2015$Season = 2014
    top100_2016$Season = 2015
    top100_2017$Season = 2016
    top100_2018$Season = 2017
    top100_2019$Season = 2018
    top100_2020$Season = 2019
  
    # Adding Indicator Variable
    top100_2011$Top100_2011 = 1
    top100_2012$Top100_2012 = 1
    top100_2013$Top100_2013 = 1
    top100_2014$Top100_2014 = 1
    top100_2015$Top100_2015 = 1 
    top100_2016$Top100_2016 = 1
    top100_2017$Top100_2017 = 1
    top100_2018$Top100_2018 = 1
    top100_2019$Top100_2019 = 1
    top100_2020$Top100_2020 = 1
    
    # Changing Column Name
    colnames(top100_2011)[1] = "Top100Rank_2011"
    colnames(top100_2012)[1] = "Top100Rank_2012"
    colnames(top100_2013)[1] = "Top100Rank_2013"
    colnames(top100_2014)[1] = "Top100Rank_2014"
    colnames(top100_2015)[1] = "Top100Rank_2015"
    colnames(top100_2016)[1] = "Top100Rank_2016"
    colnames(top100_2017)[1] = "Top100Rank_2017"
    colnames(top100_2018)[1] = "Top100Rank_2018"
    colnames(top100_2019)[1] = "Top100Rank_2019"
    colnames(top100_2020)[1] = "Top100Rank_2020"
    
    # Reordering Columns
    top100_2011 = top100_2011[,c(2:4,1)]
    top100_2012 = top100_2012[,c(2:4,1)]
    top100_2013 = top100_2013[,c(2:4,1)]
    top100_2014 = top100_2014[,c(2:4,1)]
    top100_2015 = top100_2015[,c(2:4,1)]
    top100_2016 = top100_2016[,c(2:4,1)]
    top100_2017 = top100_2017[,c(2:4,1)]
    top100_2018 = top100_2018[,c(2:4,1)]
    top100_2019 = top100_2019[,c(2:4,1)]
    top100_2020 = top100_2020[,c(2:4,1)]

## Fixing Typos & Errors in Top 100 List/ Full Datasets
  # 2011 List
  setdiff(top100_2011$Name, full_data$Name[which(full_data$Season == 2010)])
    which(top100_2011$Name == "Miguel Cabera") # changing typo
    top100_2011$Name[4] = "Miguel Cabrera"
    which(top100_2011$Name == "Mark Trixeira") # changing typo
    top100_2011$Name[24] = "Mark Teixeira"
    which(top100_2011$Name == "Brain Wilson") # changing typo
    top100_2011$Name[44] = "Brian Wilson"
    which(top100_2011$Name == "Mike Stanton") # changing typo
    top100_2011$Name[76] = "Giancarlo Stanton"
  setdiff(top100_2011$Name, full_data$Name[which(full_data$Season == 2010)]) # Confirming correct changes

  # 2012 List
  setdiff(top100_2012$Name, full_data$Name[which(full_data$Season == 2011)]) 
    which(top100_2012$Name == "Howard Kendrick") # changing typo
    top100_2012$Name[85] = "Howie Kendrick"
    which(top100_2012$Name == "Michael Morse") # changing typo
    top100_2012$Name[95] = "Mike Morse"
    newrow = c("Adam Wainwright", 2233, 29, "STL", "NL", 2011, rep(0,308)) # adding missed row
    full_data = rbind(full_data, newrow)
  setdiff(top100_2012$Name, full_data$Name[which(full_data$Season == 2011)]) # Confirming correct changes

  # 2013 List
  setdiff(top100_2013$Name, full_data$Name[which(full_data$Season == 2012)]) 
    which(top100_2013$Name == "B.J. Upton") # changing typo
    top100_2013$Name[69] = "Melvin Upton Jr."
    which(top100_2013$Name == "Michael Morse") # changing typo
    top100_2013$Name[85] = "Mike Morse"
    newrow = c("Victor Martinez", 393, 33, "DET", "AL", 2012, rep(0,308)) # adding missed row
    full_data = rbind(full_data, newrow)
  setdiff(top100_2013$Name, full_data$Name[which(full_data$Season == 2012)]) # Confirming correct changes

    # 2014 List
  setdiff(top100_2014$Name, full_data$Name[which(full_data$Season == 2013)]) 
    which(top100_2014$Name == "Clay Buccholz") # changing typo
    top100_2014$Name[89] = "Clay Buchholz"
  setdiff(top100_2014$Name, full_data$Name[which(full_data$Season == 2013)]) # Confirming correct changes

  # 2015 List
  setdiff(top100_2015$Name, full_data$Name[which(full_data$Season == 2014)])
    newrow = c("Matt Harvey", 11713, 25, "NYM", "NL",2014, rep(0,308)) # adding missed row
    full_data = rbind(full_data,newrow)
  setdiff(top100_2015$Name, full_data$Name[which(full_data$Season == 2014)]) # Confirming correct changes
    
  # 2016 List
  setdiff(top100_2016$Name,full_data$Name[which(full_data$Season == 2015)]) 
    which(top100_2016$Name == "A.J. Pollock") # changing typo
    top100_2016$Name[20] = "AJ Pollock"
    which(full_data[which(full_data$Season == 2015),]$Name == "Dee Strange-Gordon")  # changing typo
    full_data[which(full_data$Season == 2015),]$Name[396] = "Dee Gordon"
  setdiff(top100_2016$Name, full_data$Name[which(full_data$Season == 2015)]) # Confirming correct changes

  # 2017 List
  setdiff(top100_2017$Name, full_data$Name[which(full_data$Season == 2016)]) 
    which(top100_2017$Name == "Zach Britton") # changing typo
    top100_2017$Name[39] = "Zack Britton"
    which(top100_2017$Name == "A.J. Pollock") # changing typo
    top100_2017$Name[46] = "AJ Pollock"
    which(top100_2017$Name == "D.J. LeMahieu") # changing typo
    top100_2017$Name[63] = "DJ LeMahieu"
  setdiff(top100_2017$Name, full_data$Name[which(full_data$Season == 2016)]) # Confirming correct changes

  # 2018 List
  setdiff(top100_2018$Name, full_data$Name[which(full_data$Season == 2017)]) 
    which(top100_2018$Name == "D.J. LeMahieu") # changing typo
    top100_2018$Name[81] = "DJ LeMahieu"
    newrow = c("Shohei Ohtani", 19755, 23, "LAA", "AL",2017, rep(0,308)) # adding missed row
    full_data = rbind(full_data, newrow)
  setdiff(top100_2018$Name, full_data$Name[which(full_data$Season == 2017)])  # Confirming correct changes

  # 2019 List
  setdiff(top100_2019$Name, full_data$Name[which(full_data$Season == 2018)]) 

  # 2020 List
  setdiff(top100_2020$Name, full_data$Name[which(full_data$Season == 2019)]) 
    which(top100_2020$Name == "D.J. LeMahieu") # changing typo
    top100_2020$Name[37] = "DJ LeMahieu"
    which(top100_2020$Name == "Hyun-Jin Ryu") # changing typo
    top100_2020$Name[53] = "Hyun Jin Ryu"
    which(top100_2020$Name == "Nick Castellanos") # changing typo
    top100_2020$Name[71] = "Nicholas Castellanos"
  setdiff(top100_2020$Name, full_data$Name[which(full_data$Season == 2019)])

   ## Reordering fulldata
   full_data = full_data[order(full_data$Name),]
   
## Merge Top 100 Lists with Batting Dataset
  # Merging by season 
  full_data = join(full_data, top100_2011, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2012, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2013, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2014, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2015, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2016, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2017, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2018, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2019, by = c("Name", "Season"), match = "first")
  full_data = join(full_data, top100_2020, by = c("Name", "Season"), match = "first")
  
## Fixing Errors
  # 2012
  length(which(full_data$Top100_2012 == 1))
  full_data$Name[which(full_data$Top100_2012 == 1)]
  full_data[which(full_data$Name == "Chris Carpenter" & full_data$Season == 2011),]
  full_data[3009,]$Top100_2012 = NA # changing indicator to 0 from extra row
  
  # 2018
  length(which(full_data$Top100_2018 == 1))
  full_data$Name[which(full_data$Top100_2018 == 1)]
  full_data[which(full_data$Name == "Jose Ramirez" & full_data$Season == 2017),]
  full_data[8747,]$Top100_2018 = NA # changing indicator to 0 from extra row
  
  # 2019
  length(which(full_data$Top100_2019 == 1))
  full_data$Name[which(full_data$Top100_2019 == 1)]
  full_data[which(full_data$Name == "Jose Ramirez" & full_data$Season == 2018),]
  full_data[8749,]$Top100_2019 = NA # changing indicator to 0 from extra row

  full_data = full_data[,c(1,3,2,4:334)]
## Removing Non-Top 100 Players OR Players w/ less than 251 avg over three years or Players w/ less than 41 IP avg over three years
  # Make Most Variables Numeric
  full_data[,c(2:4,7:141,144:334)] <- lapply(full_data[,c(2:4,7:141,144:334)], as.numeric)
  # Reordering dataset by name and season
  full_data = full_data[order(full_data$Name, full_data$Season),]
  
  # Saving data prior to trimming
  write.csv(full_data, "FullDataBeforeTrim.csv")
  
  # Reload data if necessary 
   full_data = read_csv("FullDataBeforeTrim.csv")
   full_data = full_data[,-1]

  # Replacing NA values with 0
  full_data = full_data %>% mutate_at(c(7:141,144:334), ~replace(., is.na(.), 0))
  
  # At-Bats Check
    # Function to check for average at bats over rolling three years
    atbats_3yr251avgcheck = function(.data, variable){
      .data %>% group_by(playerid) %>%
        mutate(lag1=lag({{variable}}), # create variable with value from past year
               lag2=lag({{variable}}, 2)) %>% # create variable with value from two years ago
        ungroup() %>%
        rowwise(playerid, Season) %>%
        mutate("{{variable}}_3yr251avgcheck" := case_when(mean(c({{variable}},lag1,lag2), na.rm = TRUE) >= 251 ~ 1, 
                                                       mean(c({{variable}},lag1,lag2), na.rm = TRUE) < 251 ~ 0)) %>% # Giving value of 1 if average greater than 251, 0 if not 
      select(-c(lag1,lag2))
    }
  
    # Testing function on small subset of full data
    test = full_data[266:274,]
    test = test %>% atbats_3yr251avgcheck(AB)
    
    # Testing function on full data
    test = full_data %>% atbats_3yr251avgcheck(AB)

  # Function to check for average innings pitched over rolling three years
  ip_3yr41avgcheck = function(.data, variable){
    .data %>% group_by(playerid) %>%
      mutate(lag1=lag({{variable}}),  # creating variable with value from past year
             lag2=lag({{variable}}, 2)) %>% # creating variable with value from two years ago
      ungroup() %>%
      rowwise(playerid, Season) %>%
      mutate("{{variable}}_3yr41avgcheck" := case_when(mean(c({{variable}},lag1,lag2), na.rm = TRUE) >= 41 ~ 1, 
                                                    mean(c({{variable}},lag1,lag2), na.rm = TRUE) < 41 ~ 0)) %>% # giving value of 1 if average is greater than 41, 0 if not 
    select(-c(lag1,lag2))
    }

  # Testing function on small subset of full data
  test2 = full_data[35:45,]
  test2 = test2 %>% ip_3yr41avgcheck(IP)
  
  # Testing function on full data
  test = test %>% ip_3yr41avgcheck(IP)
  
  # Renaming to apply changes
  full_data = test

  # Making AB Check and IP Check variables numeric
  full_data[,335:336]= lapply(full_data[,335:336], as.numeric)
  
  # Adding Overall Top 100 Indicator to full data
  full_data = full_data %>% 
    mutate(Top100 = ifelse(Top100_2011 == 1 | Top100_2012 == 1 | Top100_2013 == 1 |
                             Top100_2014 == 1 | Top100_2015 == 1 | Top100_2016 == 1 | 
                             Top100_2017 == 1 | Top100_2018 == 1 | Top100_2019 == 1 | 
                             Top100_2020 == 1, 1, 0))
 
  # Removing all rows that don't match conditions, giving complete dataset
  full_data = full_data[!(full_data$Top100 == 0 & full_data$AB_3yr251avgcheck == 0 & full_data$IP_3yr41avgcheck == 0),]

  # Checking all necessary players remain
  length(which(full_data$Top100 == 1))
  
  # Saving data
  write_csv(full_data, "FullDataTrimmed.csv")
  
  
  file.rename("FullData.csv", "InitialData.csv")
  file.rename("FullDataUpdated.csv", "InitialDataUpdated.csv")
  file.rename("FullDataBeforeTrim.csv", "InitialDataBeforeTrim.csv")  
  file.rename("FullDataTrimmed.csv", "FullData.csv")
 
  ## FIXING ISSUES Found during data exploration
  # Issue with incorrect team/league names
 
  full_data = read_csv("FullData.csv")
  which(full_data$Tm == "TOT")
  full_data[3890,5] = "OAK"
  full_data[3890,6] = "AL"
  
  which(full_data$Tm == "2TM")
  full_data[606,5] = "SEA"
  full_data[606,6] = "AL"
  
  full_data[813,5] = "OAK"
  full_data[813,6] = "AL"
  
  full_data[2446,5] = "MIN"
  full_data[2446,6] = "AL"
  
  full_data[2833,5] = "OAK"
  full_data[2833,6] = "AL"
  
  full_data[3652,5] = "SEA"
  full_data[3652,6] = "AL"
  
  full_data[4231,5] = "TEX"
  full_data[4231,6] = "AL"
  
  full_data[4663,5] = "NYY"
  full_data[4663,6] = "AL"
  
  full_data[4868,5] = "KCR"
  full_data[4868,6] = "AL"
  
  full_data[5391,5] = "SEA"
  full_data[5391,6] = "AL"
  
  full_data[5572,5] = "SEA"
  full_data[5572,6] = "AL"
  
  full_data[6082,5] = "DET"
  full_data[6082,6] = "AL"
  
  full_data[6181,5] = "BAL"
  full_data[6181,6] = "AL"
  
  full_data[7216,5] = "TBR"
  full_data[7216,6] = "AL"
  
  full_data[7410,5] = "OAK"
  full_data[7410,6] = "AL"
  
  full_data[7633,5] = "BAL"
  full_data[7633,6] = "AL"
  
  # Duplicate Rows
  which(is.na(full_data$Tm))
  full_data = full_data[-c(2906,2909),]
  full_data = full_data[-c(6008),]
  
  # Incorrect playerid
  full_data %>% count(playerid, Season) %>% filter(n > 1)
  full_data[which(full_data$Name == "Ramon Ramirez"),]
  full_data[6675,3] = 7986

  # Changing league name of Florida Marlins to NL
  full_data[which(full_data$Lg == "MLB"),6] = "NL"
  
  # Adding Top 100 Rank Variable, not separate from year
  full_data$Top100Rank = ifelse(full_data$Top100Rank_2011 != 0, full_data$Top100Rank_2011,     
                           ifelse(full_data$Top100Rank_2012 != 0, full_data$Top100Rank_2012,
                              ifelse(full_data$Top100Rank_2013 != 0, full_data$Top100Rank_2013,
                                  ifelse(full_data$Top100Rank_2014 != 0, full_data$Top100Rank_2014,
                                      ifelse(full_data$Top100Rank_2015 != 0, full_data$Top100Rank_2015,
                                          ifelse(full_data$Top100Rank_2016 != 0, full_data$Top100Rank_2016,
                                              ifelse(full_data$Top100Rank_2017 != 0, full_data$Top100Rank_2017,
                                                  ifelse(full_data$Top100Rank_2018 != 0, full_data$Top100Rank_2018,
                                                      ifelse(full_data$Top100Rank_2019 != 0, full_data$Top100Rank_2019,
                                                          ifelse(full_data$Top100Rank_2020 != 0, full_data$Top100Rank_2020,0))))))))))
  
  
  # Removing secondary positions from position
  full_data$PosSummary_Field = gsub("-.*","",full_data$PosSummary_Field)
  
  # Fixing rows with no position
  which(full_data$PosSummary_Field == 0)
  full_data[179,143] = "P"
  full_data[2268,143] = "DH"
  full_data[3241,143] = "OF"
  full_data[3474,143] = "OF"
  full_data[3871,143] = "2B"
  full_data[4103,143] = "DH"
  full_data[4104,143] = "DH"
  full_data[4105,143] = "DH"
  full_data[4888,143] = "DH"
  full_data[4981,143] = "DH"
  full_data[5614,143] = "DH"
  full_data[5765,143] = "P"
  full_data[6280,143] = "DH"
  full_data[7390,143] = "P"
  full_data[7392,143] = "P"
  full_data[7702,143] = "DH"
  full_data[7703,143] = "DH"
  full_data[7704,143] = "DH"
  full_data[7705,143] = "DH"
  full_data[7706,143] = "DH"
  full_data[7921,143] = "DH"
  full_data[7926,143] = "DH"
  full_data[7950,143] = "DH"
  
  # Making NA's 0 in Top100Rank
  full_data[, 338][full_data[, 338] == 0] <- NA
  
  colnames(full_data) = gsub("%","Percent", colnames(full_data))
  colnames(full_data) = gsub("2B", "Doubles", colnames(full_data))
  colnames(full_data) = gsub("3B", "Triples", colnames(full_data))
  colnames(full_data) = gsub("\\+", "Plus", colnames(full_data))
  colnames(full_data) = gsub("\\+", "Plus", colnames(full_data))
  colnames(full_data) = gsub("/", "Per", colnames(full_data))
  colnames(full_data) = gsub("162", "FullSeason", colnames(full_data))
  colnames(full_data) = gsub("\\-", "LPercent", colnames(full_data))
  
  # Resaving full data
  write_csv(full_data,"FullData.csv")
  
  