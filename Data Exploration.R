## Loading Data
full_data = read_csv("FullData.csv")
library(ggplot2)
## Creating Team Color Vector
team_colors = c("#A71930", "#CE1141", "#DF4601", "#C62033", "#0E3386", "#231F20", "#C6011F",
                "#E31937", "#333366", "#0C2C56", "#41748D", "#F4871E", "#004687", "#8F0028", 
                "#005A9C", "#00A3E0", "#B6922E", "#D31145", "#FF5910", "#142448", "#003831",
                "#E81828", "#FDB827", "#002D62", "#005C5C", "#F4793E", "#B72126", "#8FBCE6", 
                "#003278", "#1348AE", "#AB0003")


## Creating Graphs
  # Plotting Count of Top 100 Players by Team
  ggplot(full_data, aes(x = Tm, y = Top100, fill = Tm)) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = team_colors) + 
    xlab("Team Abbreviation") + 
    ylab("Top 100 List Frequency") +
    labs(fill="Team") + 
    ggtitle("Number of Players on Top 100 List By Team") + 
    theme(plot.title = element_text(hjust = 0.5))
    
  
  # Plotting Count of Top 100 Players by League
  ggplot(full_data, aes(x = Lg, y = Top100, fill = Lg)) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = c("#D50032", "#002D72")) + 
    xlab("League") + 
    ylab("Top 100 List Frequency") + 
    ggtitle("Number of Players on Top 100 List By League") + 
    theme(plot.title = element_text(hjust = 0.5))
    
  
  # Plotting Count of Top 100 Players by Age
  ggplot(full_data, aes(x = Age, y = Top100, fill = as.numeric(-Age))) + 
    geom_bar(stat = "identity", show.legend = FALSE) + 
    scale_fill_gradient(low = "black", high = "black") + 
    xlab("Age") + 
    ylab("Top 100 List Frequency") + 
    ggtitle("Number of Players on Top 100 List by Age") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # Plotting Top 100 Rank of Batters by WAR
  ggplot(batters, aes(x = Top100Rank, y = WAR_Bat)) + 
    geom_point(color = "#002D72") + 
    geom_smooth(method = "lm", se = FALSE, color = "#D50032") + 
    xlab("Top 100 Rank") + 
    ylab("WAR") + 
    ggtitle("Top 100 Rank vs. WAR Value - Batters") + 
    theme(plot.title = element_text(hjust = 0.5))
  

  # Plotting Top 100 Rank of Pitchers by WAR
  ggplot(pitchers, aes(x = Top100Rank, y = WAR_Pitch)) + 
    geom_point(color = "#002D72") + 
    geom_smooth(method = "lm", se = FALSE, color = "#D50032") + 
    xlab("Top 100 Rank") + 
    ylab("WAR") + 
    ggtitle("Top 100 Rank vs. WAR Value - Pitchers") + 
    theme(plot.title = element_text(hjust = 0.5))
  
  # Plotting Top 100 Players by Position
  full_data$PosSummary_Field <- factor(full_data$PosSummary_Field,levels = c("1B", "2B", "SS", "3B", "OF", "C", "P", "DH"))
  uniquenames = x = full_data[which(full_data$Top100 == 1),] %>% distinct(playerid, PosSummary_Field, .keep_all = TRUE)
  ggplot(uniquenames, aes(x = PosSummary_Field, y = Top100)) + 
    geom_bar(stat = "identity", show.legend = FALSE, color = "#002D72") + 
    xlab("Position") + 
    ylab("Top 100 List Frequency") + 
    ggtitle("Top 100 Players By Position") + 
    theme(plot.title = element_text(hjust = 0.5))

  # Plotting Top 100 Players by Position, By Season
  positionbyseason = subset(full_data, Season > 2009)
  ggplot(positionbyseason, aes(x = as.factor(Season), y = Top100, fill = PosSummary_Field)) + 
    geom_bar(stat = "identity") + scale_fill_brewer(palette = "RdBu") + 
    xlab("Season") + 
    ylab("Top 100 List Frequency") + 
    ggtitle("Top 100 Players By Position Each Season") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    labs(fill = "Position")

  # Plotting Top 100 Players Who Appeared Most Frequently
  full_data = full_data %>% group_by(playerid) %>% mutate(Top100Frequency = sum(Top100))
  frequentlist = subset(full_data, Top100Frequency > 7)
  frequentlist$Name = factor(frequentlist$Name, levels = c("Clayton Kershaw","Giancarlo Stanton",
                                                           "Zack Greinke", "Andrew McCutchen", 
                                                           "Aroldis Chapman", "Buster Posey", 
                                                           "Joey Votto", "Justin Upton", 
                                                           "Justin Verlander","Robinson Cano",
                                                           "Stephen Strasburg","Adrian Beltre",
                                                           "Bryce Harper", "Chris Sale", "David Price",
                                                           "Madison Bumgarner", "Miguel Cabrera", 
                                                           "Mike Trout", "Nelson Cruz", "Ryan Braun"))
  ggplot(frequentlist, aes(x = Name, y = Top100, fill = Lg)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = c("#D50032", "#002D72")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_y_continuous(breaks=seq(0,10,2)) + 
    ylab("Top 100 List Frequency") + 
    ggtitle("All Players Who Appeared on a Top 100 List At Least Eight Times") + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # Plotting Top 100 Players Who Appeared in Top 10 Most Frequently
  full_data = full_data %>% group_by(playerid) %>% mutate(Top10Frequency = sum(Top100Rank < 11, na.rm = TRUE))
  bestlist = subset(full_data, Top10Frequency > 2)
  bestlist = bestlist[which(bestlist$Top100Rank < 11),]
  bestlist$Name = factor(bestlist$Name, levels = c("Mike Trout", "Clayton Kershaw",
                                                   "Miguel Cabrera", "Robinson Cano", 
                                                   "Felix Hernandez", "Joey Votto",
                                                   "Andrew McCutchen", "Bryce Harper",
                                                   "Jose Altuve", "Josh Donaldson",
                                                   "Mookie Betts", "Nolan Arenado",
                                                   "Ryan Braun"))
  
  ggplot(bestlist, aes(x = Name, y = Top100, fill = Lg)) + 
    geom_bar(stat="identity") + 
    scale_fill_manual(values = c("#D50032", "#002D72")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
    scale_y_continuous(breaks=seq(0,10,2)) + 
    ylab("Top 100 List Frequency") + 
    ggtitle("All Players Who Appeared on in the Top 10 of a Top 100 List At Least Three Times") + 
    theme(plot.title = element_text(hjust = 0.5)) 
  
  # Creating dataset where batters and pitchers are separate
  batters = full_data[which(full_data$PosSummary_Field != "P"),]
  pitchers = full_data[which(full_data$PosSummary_Field == "P"),]
  
  ## Density Plots of Variables
  # Function to plot all non-award variables (3 yr avg editions)
  plot_data_nonawards = function(data, column) {
    ggplot(data, aes_string(x = column, fill = as.factor(data$Top100))) +
      geom_density(alpha = 0.5) + 
      scale_fill_manual(values = c("#D50032", "#002D72")) + 
      labs(fill = "Top 100") + 
      xlab(paste("3 Year Avg. of", sub("_.*", "", column)))
  }

  # Creating list of plots by batters and pitchers separately
  battingplots_nonawards = lapply(colnames(full_data)[c(162:220)], plot_data_nonawards, data = batters)
  pitchingplots_nonawards = lapply(colnames(full_data)[221:278], plot_data_nonawards, data = pitchers)

  # Function to plot award variables 
  plot_data_awards = function(data, column) {
    ggplot(data, aes_string(x = column, fill = as.factor(data$Top100))) +
      geom_density(alpha = 0.5) + 
      scale_fill_manual(values = c("#D50032", "#002D72")) + 
      labs(fill = "Top 100") + 
      xlab(paste("Total", sub("_.*", "", column)))
  }
  
  # Creating list of award plots for batters and pitchers separately
  battingplots_awards = lapply(colnames(full_data)[c(297:299,300,310)], plot_data_awards, data = batters)
  pitchingplots_awards = lapply(colnames(full_data)[c(297:299,300,305,310)], plot_data_awards, data = pitchers)
 
  # Merging award and non-award plots together
  battingplots = append(battingplots_nonawards, battingplots_awards)
  pitchingplots = append(pitchingplots_nonawards, pitchingplots_awards)
  
  # Creating PDF files of plots
  library(gridExtra)
  pdf("battingplots.pdf")
  for (i in seq(length(list(battingplots)))) {
    print(list(battingplots)[[i]])
  }
  dev.off()
  
  pdf("pitchingplots.pdf")
  for (i in seq(length(list(pitchingplots)))) {
    print(list(pitchingplots)[[i]])
  }
  dev.off()
  
## Changing WinLossPercentLPercent Column Name due to Error in Code
colnames(full_data)[68] = "WLPercent"

## Removing Similar/Unnecessary Variables from full data
dropcolumns = c("BBPercent", "KPercent", "wRC", "SOPerW", "Rtot", "HR_Pitch",
                "BB_Pitch", "SO_Pitch", "RS", "WLPercent", "RFPerG", "RAA_Bat",
                "RAR_Bat", "RAA_Pitch", "RAR_Pitch", "BtRuns", "RE24_Bat", "RE24_Pitch",
                "wRAA","BBPercent_3yravg", "KPercent_3yravg", "wRC_3yravg", "SOPerW_3yravg",
                "Rtot_3yravg", "HR_Pitch_3yravg","BB_Pitch_3yravg", "SO_Pitch_3yravg",
                "RS_3yravg", "WinLossPercentLPercent_3yravg", "RFPerG_3yravg", "RAA_Bat_3yravg",
                "RAR_Bat_3yravg", "RAA_Pitch_3yravg", "RAR_Pitch_3yravg", "BtRuns_3yravg",
                "RE24_Bat_3yravg", "RE24_Pitch_3yravg", "wRAA_3yravg")


full_data = full_data[,!(names(full_data) %in% dropcolumns)]


## Renaming Columns & Adusting Values for Clarity
  # Batting to Batting_Wins_FG, altering runs to wins
  which(colnames(full_data) == "Batting")
  full_data$Batting = full_data$Batting/10
  which(colnames(full_data) == "Batting_3yravg")
  full_data$Batting_3yravg = full_data$Batting_3yravg/10
  colnames(full_data)[50] = "BattingWins_FG"
  colnames(full_data)[186] = "BattingWins_FG_3yravg"

  # BtWins to Batting_Wins_BR
  which(colnames(full_data) == "BtWins")
  colnames(full_data)[39] = "BattingWins_BR"
  which(colnames(full_data) == "BtWins_3yravg")
  colnames(full_data)[175] = "BattingWins_BR_3yravg"

  # Br_WAR_Bat to WAR_Pos_BR
  which(colnames(full_data) == "Br_WAR_Bat")
  colnames(full_data)[41] = "WAR_Pos_BR"
  which(colnames(full_data) == "Br_WAR_Bat_3yravg")
  colnames(full_data)[177] = "WAR_Pos_BR_3yravg"

  # WAR_Bat to WAR_Pos_FG
  which(colnames(full_data) == "WAR_Bat")
  colnames(full_data)[43] = "WAR_Pos_FG"
  which(colnames(full_data) == "WAR_Bat_3yravg")
  colnames(full_data)[179] = "WAR_Pos_FG_3yravg"

  # oWAR to oWAR_BR
  which(colnames(full_data) == "oWAR")
  colnames(full_data)[42] = "oWAR_BR"
  which(colnames(full_data) == "oWAR_3yravg")
  colnames(full_data)[178] = "oWAR_BR_3yravg"

  # Off to oWAR_FG, altering runs to wins
  which(colnames(full_data) == "Off")
  full_data$Off = full_data$Off/10
  which(colnames(full_data) == "Off_3yravg")
  full_data$Off_3yravg = full_data$Off_3yravg/10
  colnames(full_data)[51] = "oWAR_FG"
  colnames(full_data)[187] = "oWAR_FG_3yravg"
  
  #Br_WAR_Pitch to WAR_Pitch_BR
  which(colnames(full_data) == "Br_WAR_Pitch")
  colnames(full_data)[99] = "WAR_Pitch_BR"
  which(colnames(full_data) == "Br_WAR_Pitch_3yravg")
  colnames(full_data)[236] = "WAR_Pitch_BR_3yravg"
  
  # WAR_Pitch to WAR_Pitch_FG
  which(colnames(full_data) == "WAR_Pitch")
  colnames(full_data)[93] = "WAR_Pitch_FG"
  which(colnames(full_data) == "WAR_Pitch_3yravg")
  colnames(full_data)[230] = "WAR_Pitch_FG_3yravg"
  
  # Rdrs to DRS_BR
  which(colnames(full_data) == "Rdrs")
  colnames(full_data)[118] = "DRS_BR"
  which(colnames(full_data) == "Rdrs_3yravg")
  colnames(full_data)[255] = "DRS_BR_3yravg"

  # Fielding to DRS_FG
  which(colnames(full_data) == "Fielding")
  colnames(full_data)[121] = "DRS_FG"
  which(colnames(full_data) == "Fielding_3yravg")
  colnames(full_data)[258] = "DRS_FG_3yravg"

  # dWAR to dWAR_BR
  which(colnames(full_data) == "dWAR")
  colnames(full_data)[120] = "dWAR_BR"
  which(colnames(full_data) == "dWAR_3yravg")
  colnames(full_data)[257] = "dWAR_BR_3yravg"

  # Def to dWAR_FG
  which(colnames(full_data) == "Def")
  full_data$Def = full_data$Def/10
  which(colnames(full_data) == "Def_3yravg")
  full_data$Def_3yravg = full_data$Def_3yravg/10
  colnames(full_data)[122] = "dWAR_FG"
  colnames(full_data)[259] = "dWAR_FG_3yravg"

## Creating dataset where batters and pitchers are separate
batters = full_data[which(full_data$PosSummary_Field != "P"),]
pitchers = full_data[which(full_data$PosSummary_Field == "P"),]

## Saving Data
write_csv(full_data, "DataForAnalysis.csv")
## Saving Work Environment
save.image()
