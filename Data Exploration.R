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


  
  ## Density Plots
  ggplot(batters, aes(x = `2B_3yravg`, fill = as.factor(Top100))) + 
    geom_density(alpha = 0.5) + 
    scale_fill_manual(values = c("#D50032", "#002D72")) + 
    labs(fill = "Top 100") + 
    xlab("3 Year Avg. of BA")
  
  plot_data_nonawards = function(data, column) {
    ggplot(data, aes_string(x = column, fill = as.factor(data$Top100))) +
      geom_density(alpha = 0.5) + 
      scale_fill_manual(values = c("#D50032", "#002D72")) + 
      labs(fill = "Top 100") + 
      xlab(paste("3 Year Avg. of", sub("_.*", "", column)))
  }

  battingplots_nonawards = lapply(colnames(full_data)[c(160:217)], plot_data_nonawards, data = batters)
  pitchingplots_nonawards = lapply(colnames(full_data)[218:274], plot_data_nonawards, data = pitchers)

    
  plot_data_awards = function(data, column) {
    ggplot(data, aes_string(x = column, fill = as.factor(data$Top100))) +
      geom_density(alpha = 0.5) + 
      scale_fill_manual(values = c("#D50032", "#002D72")) + 
      labs(fill = "Top 100") + 
      xlab(paste("Total", sub("_.*", "", column)))
  }
  
  battingplots_awards = lapply(colnames(full_data)[c(293:295,300,310)], plot_data_awards, data = batters)
  pitchingplots_awards = lapply(colnames(full_data)[c(293:295,300,305,310)], plot_data_awards, data = pitchers)
 
  
  battingplots = append(battingplots_nonawards, battingplots_awards)
  pitchingplots = append(pitchingplots_nonawards, pitchingplots_awards)

  battingplots[20]
  
  
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
  

  prcomp() 
  

  