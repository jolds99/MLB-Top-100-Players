## Load Data
full_data = read_csv("DataForAnalysis.csv")

## Obtain Required Batting/Pitching datasets
batters = full_data[which(full_data$PosSummary_Field != "P"),]
pitchers = full_data[which(full_data$PosSummary_Field == "P"),]

## Trimming Batters to only Batting/Fielding variables
batters = batters[,c(1:57,107:122,124:132,138:193,244:267,273:303)]
## Trimming Pitchers to only Pitching variables
pitchers = pitchers[,c(1:6,58:106,125:142,194:243,260:303)]

## Correlation tests between various fangraphs/baseball reference variables
ggplot(batters,aes(x = dWAR_BR_3yravg, y = dWAR_FG_3yravg)) + geom_point()
cor.test(batters$dWAR_BR_3yravg, batters$dWAR_FG_3yravg)

ggplot(batters,aes(x = DRS_BR_3yravg, y = DRS_FG_3yravg)) + geom_point()
cor.test(batters$DRS_BR_3yravg, batters$DRS_FG_3yravg)

ggplot(batters,aes(x = WAR_Pos_BR_3yravg, y = WAR_Pos_FG_3yravg)) + geom_point()
cor.test(batters$WAR_Pos_BR_3yravg, batters$WAR_Pos_FG_3yravg)

ggplot(pitchers,aes(x = WAR_Pitch_BR_3yravg, y = WAR_Pitch_FG_3yravg)) + geom_point()
cor.test(pitchers$WAR_Pitch_BR_3yravg, pitchers$WAR_Pitch_FG_3yravg)

## PCA
  # Fixing column with NA's
  batters[,191][is.na(batters[,191])] = 0
  pitchers[,165][is.na(pitchers[,165])] = 0
  
  pitchers = pitchers[,-76]
  
  # Saving data 
  write_csv(batters, "BattersData.csv")
  write_csv(pitchers, "PitchersData.csv")
  
  batters = read_csv("BattersData.csv")
  pitchers = read_csv("PitchersData.csv")
  
  # Removing 2008 & 2009 seasons
  batters = batters %>% filter(Season>=2010)
  pitchers = pitchers %>% filter(Season >= 2010)
  
  # Creating Test & Train data for batters
  set.seed(2020)
  batters$Top100 = as.factor(batters$Top100)
  batters_pi = unique(batters$playerid)
  batters_piselections = batters_pi[createDataPartition(y = batters_pi, p = 0.7, list = FALSE)]  
  batters_train = batters[which(batters$playerid %in% batters_piselections),]
  batters_test = batters[which(batters$playerid %!in% batters_piselections),]
  which(batters$Name %in% batters_train & batters$Name %in% batters_test)
  
  # Create Test & Train data for pitchers
  set.seed(2020)
  pitchers$Top100 = factor(pitchers$Top100)
  pitchers_pi = unique(pitchers$playerid)
  pitchers_piselections = pitchers_pi[createDataPartition(y = pitchers_pi, p = 0.7, list = FALSE)]  
  pitchers_train = pitchers[which(pitchers$playerid %in% pitchers_piselections),]
  pitchers_test = pitchers[which(pitchers$playerid %!in% pitchers_piselections),]  
  which(pitchers$Name %in% pitchers_train & pitchers$Name %in% pitchers_test)
  
  # Dataframe of only pca-eligible variables - batters
  batters_active = batters[,c(88:167)]
  batters_active = batters_active[,which(apply(batters_active, 2, sd) != 0)]
  
  # Dataframe of only pca-eligible variables - pitchers
  pitchers_active = pitchers[,74:140]
  pitchers_active = pitchers_active[,which(apply(pitchers_active, 2, sd) != 0)]
  
  # Function to calculate test accuracy
  calc_accuracy_function = function(actual, predicted) {
    mean(actual == predicted)
  }
  
  ## Traditional PCA & LR
  # Batters
  # Selecting variables
  batters_traditional = batters_active[,c(4:15,46:47,31,57:62,68:72,75:76)]
  # PCA 
  pca_batters_traditional = prcomp(batters_traditional, scale. = TRUE)
  summary(pca_batters_traditional)
  cbind(batters[1:6,1],pca_batters_traditional$x[1:6,1:6])
  head(pca_batters_traditional$scale^2, n = 6)
  # Assigning two elements of variability to vectors
  PVE = summary(pca_batters_traditional)$importance[2,]
  CVE = summary(pca_batters_traditional)$importance[3,]
  
  # Graph of variability explained
  PVEplot <- qplot(c(1:20), PVE[1:20]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("PVE - Batters") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  PVEplot
  # Flattens out after the 17th PCA component
  
  #Graph of cumulative variance explained
  CVEplot <- qplot(c(1:20), CVE[1:20]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("CVE - Batters") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) + 
    geom_hline(yintercept = 0.95, color = "red", lwd = 1)
  CVEplot
  # Reaches 95% cumulative variability after 16th component
  
  # Adding pca components and descriptive values to dataset
  batters_traditional = cbind(batters_traditional, pca_batters_traditional$x[,1:16])
  batters_traditional = cbind(batters[,c(1:6,190:193)], batters_traditional)
  
  # Dividing into test and train
  batters_train = batters_traditional[which(batters_traditional$playerid %in% batters_piselections),]
  batters_test = batters_traditional[which(batters_traditional$playerid %!in% batters_piselections),]
  
  # Logistic regression model
  set.seed(2020)
  batters_glm_traditional = train(
    form = Top100 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11 + 
      PC12 + PC13 + PC14 + PC15 + PC16,
    data = batters_train,
    trControl = trainControl(method = "cv", number = 10),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  batters_glm_traditional
  # Model values
  batters_glm_traditional$finalModel
  
  # Test data accuracy
  calc_accuracy_function(batters_test$Top100,
                         predict(batters_glm_traditional, newdata = batters_test))
  
  # Dataset of predictions v actual
  batterscomp_traditional = cbind(batters_train$Name, predict(batters_glm_traditional, 
                                                  newdata = batters_train, 
                                                  type = "prob"), batters_train$Top100)
  
  # Confusion matrix
  batters_cm_trad = confusionMatrix(predict(batters_glm_traditional,newdata = batters_test), batters_test$Top100)
  batters_cm_trad
  

  ## Traditional PCA & LR
  # Pitchers
  # Selecting variables
  pitchers_traditional = pitchers_active[,c(1:2,5:7,13:18,20:23,50:54,57:58,61:62)]
  # PCA 
  pca_pitchers_traditional = prcomp(pitchers_traditional, scale. = TRUE)
  summary(pca_pitchers_traditional)
  cbind(pitchers[1:6,1],pca_pitchers_traditional$x[1:6,1:6])
  head(pca_pitchers_traditional$scale^2, n = 6)
  # Assigning elements of variability to vectors
  PVE = summary(pca_pitchers_traditional)$importance[2,]
  CVE = summary(pca_pitchers_traditional)$importance[3,]
  
  # Graph of variability explained
  PVEplot <- qplot(c(1:20), PVE[1:20]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("PVE - pitchers") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  PVEplot
  # Flattens out after the 17th PCA component
  
  # Graph of cumulative variance explained
  CVEplot <- qplot(c(1:20), CVE[1:20]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("CVE - pitchers") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) + 
    geom_hline(yintercept = 0.95, color = "red", lwd = 1)
  CVEplot
  # Reaches 95% cumulative variability after 16th component
  
  # Adding PCA components and descriptive statistics to dataset
  pitchers_traditional = cbind(pitchers_traditional, pca_pitchers_traditional$x[,1:16])
  pitchers_traditional = cbind(pitchers[,c(1:6,163:166)], pitchers_traditional)
  
  # Dividing into test and train
  pitchers_train = pitchers_traditional[which(pitchers_traditional$playerid %in% pitchers_piselections),]
  pitchers_test = pitchers_traditional[which(pitchers_traditional$playerid %!in% pitchers_piselections),]

  # Logistic Regression Model
  set.seed(2020)
  pitchers_glm_traditional = train(
    form = Top100 ~ PC1 + PC2,
    data = pitchers_train,
    trControl = trainControl(method = "cv", number = 10),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  pitchers_glm_traditional
  # Model values
  pitchers_glm_traditional$finalModel
  
  # Test accuracy
  calc_accuracy_function(pitchers_test$Top100,
                         predict(pitchers_glm_traditional, newdata = pitchers_test))
  
  # Dataset measuring predictions vs accuracy
  pitcherscomp_traditional = cbind(pitchers_train$Name, predict(pitchers_glm_traditional, 
                                                  newdata = pitchers_train, 
                                                  type = "prob"), pitchers_train$Top100)

  # Confusion Matrix
  pitchers_cm_trad = confusionMatrix( predict(pitchers_glm_traditional, newdata = pitchers_test),pitchers_test$Top100)
  pitchers_cm_trad
  
  ## Advanced PCA & LR
  # Batters
  # Selecting Variables
  batters_advanced = batters_active[,c(16:29,32:45,48:51,63:72,75:76)]
  # PCA
  pca_batters_advanced = prcomp(batters_advanced, scale. = TRUE)
  summary(pca_batters_advanced)
  cbind(batters[1:6,1],pca_batters_advanced$x[1:6,1:6])
  head(pca_batters_advanced$scale^2, n = 6)
  # Assigning elements of variability to vectors
  PVE = summary(pca_batters_advanced)$importance[2,]
  CVE = summary(pca_batters_advanced)$importance[3,]
  
  # Graph of variability explained
  PVEplot <- qplot(c(1:25), PVE[1:25]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("PVE - Batters") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  PVEplot
  # Flattens out after the 18th PCA component
  
  # Graph of cumulative variability explained
  CVEplot <- qplot(c(1:25), CVE[1:25]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("CVE - Batters") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) + 
    geom_hline(yintercept = 0.95, color = "red", lwd = 1)
  CVEplot
  # Reaches 95% cumulative variability after 23rd component
  
  # Adding PCA components and descriptive statistics back to dataset
  batters_advanced = cbind(batters_advanced, pca_batters_advanced$x[,1:23])
  batters_advanced = cbind(batters[,c(1:6,190:193)], batters_advanced)
  
  # Dividing into test and train
  batters_train = batters_advanced[which(batters_advanced$playerid %in% batters_piselections),]
  batters_test = batters_advanced[which(batters_advanced$playerid %!in% batters_piselections),]
  
  # Logistic regression model
  set.seed(2020)
  batters_glm_advanced = train(
    form = Top100 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
      PC11 + PC12 + PC13 + PC14,
    data = batters_train,
    trControl = trainControl(method = "cv", number = 10),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  batters_glm_advanced
  # Model values
  batters_glm_advanced$finalModel
  
  # Test accuracy
  calc_accuracy_function(batters_test$Top100,
                         predict(batters_glm_advanced, newdata = batters_test))
  
  # Dataset measuring predictions vs accuracy
  batterscomp_advanced = cbind(batters_train$Name, predict(batters_glm_advanced, 
                                                              newdata = batters_train, 
                                                              type = "prob"), batters_train$Top100)
  
  # Confusion Matrix
  batters_cm_adv = confusionMatrix(predict(batters_glm_advanced, newdata = batters_test),batters_test$Top100)
  batters_cm_adv
  
  # Pitchers
  # Selecting variables
  pitchers_advanced = pitchers_active[,c(24:49,50:54,57:58,61:62)]
  # PCA
  pca_pitchers_advanced = prcomp(pitchers_advanced, scale. = TRUE)
  summary(pca_pitchers_advanced)
  cbind(pitchers[1:6,1],pca_pitchers_advanced$x[1:6,1:6])
  head(pca_pitchers_advanced$scale^2, n = 6)
  # Assigning elements of variability to vectors
  PVE = summary(pca_pitchers_advanced)$importance[2,]
  CVE = summary(pca_pitchers_advanced)$importance[3,]
  
  # Graph of variability explained 
  PVEplot <- qplot(c(1:20), PVE[1:20]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("PVE - pitchers") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  PVEplot
  # Flattens out after the 16th PCA component
  
  CVEplot <- qplot(c(1:25), CVE[1:25]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("CVE - pitchers") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) + 
    geom_hline(yintercept = 0.95, color = "red", lwd = 1)
  CVEplot
  # Reaches 95% cumulative variability after 21th component
  
  # Adding PCA Components and descriptive values back to dataset
  pitchers_advanced = cbind(pitchers_advanced, pca_pitchers_advanced$x[,1:21])
  pitchers_advanced = cbind(pitchers[,c(1:6,163:166)], pitchers_advanced)
  
  # Dividing into test and train
  pitchers_train = pitchers_advanced[which(pitchers_advanced$playerid %in% pitchers_piselections),]
  pitchers_test = pitchers_advanced[which(pitchers_advanced$playerid %!in% pitchers_piselections),]  
  
  # Logistic Regression Model
  set.seed(2020)
  pitchers_glm_advanced = train(
    form = Top100 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11,
    data = pitchers_train,
    trControl = trainControl(method = "cv", number = 10),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  pitchers_glm_advanced
  # Model values
  pitchers_glm_advanced$finalModel

  # Test accuracy
  calc_accuracy_function(pitchers_test$Top100,
                         predict(pitchers_glm_advanced, newdata = pitchers_test))
  
  # Dataset to compare predictions vs actual
  pitcherscomp_advanced = cbind(pitchers_train$Name, predict(pitchers_glm_advanced, 
                                                           newdata = pitchers_train, 
                                                           type = "prob"), pitchers_train$Top100)
  
  
  # Confusion Matrix
  pitchers_cm_adv = confusionMatrix(predict(pitchers_glm_advanced, newdata = pitchers_test), pitchers_test$Top100)
  pitchers_cm_adv
  
  ## Combining PCA Elements and then LR
  # Batters
  # Merging PCA elements to one dataset
  colnames(batters_traditional)[39:54] = gsub("PC", "PC_T", colnames(batters_traditional)[39:54]) 
  colnames(batters_advanced)[55:77] = gsub("PC", "PC_A", colnames(batters_advanced)[55:77])
  batters_trad_adv = cbind(batters_traditional, batters_advanced)
  # Getting rid of non-PCA columns
  batters_trad_adv = batters_trad_adv[,-c(32:38,55:64)] 
  batters_trad_adv = batters_trad_adv[,-c(11:31,48:91)]
  
  # Dividing into test and train
  batters_train = batters_trad_adv[which(batters_trad_adv$playerid %in% batters_piselections),]
  batters_test = batters_trad_adv[which(batters_trad_adv$playerid %!in% batters_piselections),]  
  
  # Logistic Regression
  set.seed(2020)
  batters_glm_trad_adv = train(
    form = Top100 ~ PC_T1 + PC_T2 + PC_T3 + PC_T4 + PC_T5 + PC_T6 + PC_T7 + PC_T8 + PC_T9 + 
      PC_T10 + PC_T11 + PC_T12 + PC_T13 + PC_T14 + PC_T15 + PC_T16 +
      PC_A1 + PC_A2 + PC_A3 + PC_A4 + PC_A5 + PC_A6 + PC_A7+ PC_A8 + PC_A9 + PC_A10 + 
      PC_A11 + PC_A12 + PC_A13 + PC_A14 + PC_A15 + PC_A16 + PC_A17 + PC_A18 + PC_A19 + PC_A20 +
      PC_A21 + PC_A22 + PC_A23,
    data = batters_train,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  batters_glm_trad_adv
  # Model values
  batters_glm_trad_adv$finalModel
  
  # Test accuracy
  calc_accuracy_function(batters_test$Top100,
                         predict(batters_glm_trad_adv, newdata = batters_test))

  # Dataset to compare predictions v accuracy
  batterscomp_trad_adv = cbind(batters_train$Name, predict(batters_glm_trad_adv, 
                                                           newdata = batters_train, 
                                                           type = "prob"), batters_train$Top100)
  
  # Confusion Matrix
  batters_cm_trad_adv = confusionMatrix( predict(batters_glm_trad_adv, newdata = batters_test), batters_test$Top100)
  batters_cm_trad_adv
  
  
  
  # Pitchers
  # Combining PCA columns to one dataset
  colnames(pitchers_traditional)[35:50] = gsub("PC", "PC_T", colnames(pitchers_traditional)[35:50]) 
  colnames(pitchers_advanced)[46:66] = gsub("PC", "PC_A", colnames(pitchers_advanced)[46:66])
  pitchers_trad_adv = cbind(pitchers_traditional, pitchers_advanced)
  pitchers_trad_adv = pitchers_trad_adv[,-c(26:34,51:60)] 
  pitchers_trad_adv = pitchers_trad_adv[,-c(11:25,42:76)]
  
  # Dividing into test and train
  pitchers_train = pitchers_trad_adv[which(pitchers_trad_adv$playerid %in% pitchers_piselections),]
  pitchers_test = pitchers_trad_adv[which(pitchers_trad_adv$playerid %!in% pitchers_piselections),]  
  
  # Logistic Regression
  set.seed(2020)
  pitchers_glm_trad_adv = train(
    form = Top100 ~ PC_T1 + PC_T2 + PC_T3 + PC_T4 + PC_T5 + PC_T6 + PC_T7 + PC_T8 + 
      PC_T9 + PC_T10 + PC_T11 + PC_T12 + PC_T13 + PC_T14 + PC_T15 + 
      PC_A1 + PC_A2 + PC_A3 + PC_A4 + PC_A5 + PC_A6 + PC_A7 + PC_A8 + PC_A9 + PC_A10 + PC_A11 + 
      PC_A12 + PC_A13 + PC_A14 + PC_A15,
    data = pitchers_train,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  pitchers_glm_trad_adv
  # Model values
  pitchers_glm_trad_adv$finalModel
  
  
  # Test accuracy
  calc_accuracy_function(pitchers_test$Top100,
                         predict(pitchers_glm_trad_adv, newdata = pitchers_test))
  
  # Dataset to compare predictions v accuracy
  pitcherscomp_trad_adv = cbind(pitchers_train$Name, predict(pitchers_glm_trad_adv, 
                                                           newdata = pitchers_train, 
                                                           type = "prob"), pitchers_train$Top100)

  # Confusion Matrix
  pitchers_cm_trad_adv = confusionMatrix(predict(pitchers_glm_trad_adv, newdata = pitchers_test),pitchers_test$Top100)
  pitchers_cm_trad_adv
  
  
  ## Performing PCA on Traditional and Advanced then LR  
  # Batters
  # Merging datasets
  batters_both = cbind(batters_traditional, batters_advanced)
  # Removing name, descriptive values and PCA
  batters_both = batters_both[,-(55:64)] 
  batters_both = batters_both[,-c(1:10,39:54,92:121)]
    
  # PCA
  pca_batters_both= prcomp(batters_both, scale. = TRUE)
  summary(pca_batters_both)
  cbind(batters[1:6,1],pca_batters_both$x[1:6,1:6])
  head(pca_batters_both$scale^2, n = 6)
  PVE = summary(pca_batters_both)$importance[2,]
  CVE = summary(pca_batters_both)$importance[3,]
  
  # Graph of variability explained
  PVEplot <- qplot(c(1:20), PVE[1:20]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("PVE - Batters") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  PVEplot
  # Flattens out after the 15th PCA component
  
  # Graph of cumulative variability explained
  CVEplot <- qplot(c(1:30), CVE[1:30]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("CVE - Batters") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) + 
    geom_hline(yintercept = 0.95, color = "red", lwd = 1)
  CVEplot
  # Reaches 95% cumulative variability after 29th component
  
  # Readding pca and descriptive values back to dataset
  batters_both = cbind(batters_both, pca_batters_both$x[,1:29])
  batters_both = cbind(batters[,c(1:6,190:193)], batters_both)
  
  # Dividing into test and train
  batters_train = batters_both[which(batters_both$playerid %in% batters_piselections),]
  batters_test = batters_both[which(batters_both$playerid %!in% batters_piselections),]  

  # Logistic Regression
  set.seed(2020)
  batters_glm_both = train(
    form = Top100 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
      PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + 
      PC21 + PC22 + PC23 + PC24 + PC25 + PC26 + PC27 + PC28 + PC29,
    data = batters_train,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  batters_glm_both
  # Model values
  batters_glm_both$finalModel
  
  # Test accuracy
  calc_accuracy_function(batters_test$Top100,
                         predict(batters_glm_both, newdata = batters_test))

  # Dataset to compare predictions with actual
  batterscomp_both = cbind(batters_train$Name, predict(batters_glm_both, 
                                                           newdata = batters_train, 
                                                           type = "prob"), batters_train$Top100)
  
  # Confusion Matrix
  batters_cm_both = confusionMatrix(predict(batters_glm_both, newdata = batters_test),batters_test$Top100)
  batters_cm_both
  

  # Pitchers
  # Combining dataset
  pitchers_both = cbind(pitchers_traditional, pitchers_advanced)
  # Removing unnecessary variables
  pitchers_both = pitchers_both[,-(51:60)] 
  pitchers_both = pitchers_both[,-c(1:10,35:50,77:106)]
  
  # PCA
  pca_pitchers_both= prcomp(pitchers_both, scale. = TRUE)
  summary(pca_pitchers_both)
  cbind(pitchers[1:6,1],pca_pitchers_both$x[1:6,1:6])
  head(pca_pitchers_both$scale^2, n = 6)
  PVE = summary(pca_pitchers_both)$importance[2,]
  CVE = summary(pca_pitchers_both)$importance[3,]
  
  # Graph of variability explained
  PVEplot <- qplot(c(1:20), PVE[1:20]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("PVE - pitchers") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.1))
  PVEplot
  # Flattens out after the 15th PCA component
  
  # Graph of cumulative variability explained
  CVEplot <- qplot(c(1:30), CVE[1:30]) + 
    geom_bar(stat = "Identity") + 
    xlab("Principal Component") + 
    ylab("CVE - pitchers") +
    ggtitle("Scree Plot") +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,0.2)) + 
    geom_hline(yintercept = 0.95, color = "red", lwd = 1)
  CVEplot
  # Reaches 95% cumulative variability after 26th component
  
  # Adding PCA components and descriptive statistics back to dataset
  pitchers_both = cbind(pitchers_both, pca_pitchers_both$x[,1:26])
  pitchers_both = cbind(pitchers[,c(1:6,163:166)], pitchers_both)
  
  # Dividing into test and train
  pitchers_train = pitchers_both[which(pitchers_both$playerid %in% pitchers_piselections),]
  pitchers_test = pitchers_both[which(pitchers_both$playerid %!in% pitchers_piselections),]  
  
  # Logistic Regression
  set.seed(2020)
  pitchers_glm_both = train(
    form = Top100 ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + 
      PC11 + PC12 + PC13 + PC14 + PC15 + PC16 + PC17 + PC18 + PC19 + PC20 + PC21,
    data = pitchers_train,
    trControl = trainControl(method = "cv", number = 5),
    method = "glm",
    family = "binomial"
  )
  
  # Model summary
  pitchers_glm_both
  # Model values
  pitchers_glm_both$finalModel
  
  # Test accuracy
  calc_accuracy_function(pitchers_test$Top100,
                         predict(pitchers_glm_both, newdata = pitchers_test))
  
  # Dataset to compare predictions with accuracy
  pitcherscomp_both = cbind(pitchers_train$Name, predict(pitchers_glm_both, 
                                                       newdata = pitchers_train, 
                                                       type = "prob"), pitchers_train$Top100)
  # Confusion Matrix
  pitchers_cm_both = confusionMatrix(predict(pitchers_glm_both, newdata = pitchers_test), pitchers_test$Top100)
  pitchers_cm_both
  