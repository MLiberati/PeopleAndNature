
##### PURPOSE: Create dynamic consequence tables in Excel workbooks so teams can 
#####          explore different weighting scenarios

##### AUTHOR: Marjorie R. Liberati

#### OUTPUTS:
# 1. Winnowing_Workbook_[TEAM]_2020.01.03.xlsx 
#    --> This script writes Excel formulas, but Excel will not recognize them immediately.
#         You will have to go into the workbook and 'find-replace' all of the equal signs by:
#           a. Ctrl + F to open 'Find and Replace' screen
#           b. Click on 'Replace' tab
#           c. Click on 'Options >>'
#           d. Go to 'Within' dropdown option and select "Workbook"
#           e. Enter "=" into the section for 'Find what:' and 'Replace with:'
#           f. Click 'Replace All' (should make ~800 changes)

# NOTE: Realism & Richness scores that were unique to teams (e.g., Urban)
#       were entered manually after these workbooks were created.


###------------ SETUP ------------### 

rm(list = ls())

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_281') # tell the xlsx package where to find Java

library(readxl) # read excel files directly without converting to csv first
library(writexl) # write excel files
library(xlsx) # write directly to excel sheets
library(dplyr) # create summary tables and information
library(reshape2) # use to restructure (i.e., "melt" and "cast") data frames
library(ggplot2) # create figures
library(cowplot) # arrange figures in grids
library(stringr) # format character objects
library(tibble) # changing dplyr tibbles

###------------ DATA & INFORMATION ------------### 

########## WEIGHTS CHOOSEN BY TEAMS ########## 

w_urban <- as.numeric(c(1,1,1,1,1,1))
w_coastal <- as.numeric(c(1,1,1,1,1,1))
w_ag <- as.numeric(c(1,1,1,1,1,1))

weights <- list(w_urban, w_coastal, w_ag)

##############################################

teams <- c("Urban", "Coastal", "Agriculture") # team names

criteria <- c("Relevant", "Resonant", "Responsive", "Confidence", "Realism", "Data-availability")


# Relevant, Resonant, Responsive, & Confidence
filename <- "IndividualScores_URBAN_2019.11.05.xlsx"
df_urban <- as.data.frame(read_excel(filename, sheet = "URBAN"))
df_urban <- df_urban %>% arrange(Shorthand)

filename <- "IndividualScores_COASTAL_2019.11.05.xlsx"
df_coastal <- as.data.frame(read_excel(filename, sheet = "COASTAL"))
df_coastal <- df_coastal %>% arrange(Shorthand)

filename <- "IndividualScores_AGRICULTURE_2019.11.05.xlsx"
df_ag <- as.data.frame(read_excel(filename, sheet = "AGRICULTURE"))
df_ag <- df_ag %>% arrange(Shorthand)

team_scores <- list(df_urban, df_coastal, df_ag)

# Realism & Data-availability
filename <- "Indicator Scoring - Data-richness & Realism - 2020.04.16.xlsx"
lit_review_urban <- as.data.frame(read_excel(filename, sheet = "URBAN"))
lit_review_urban <- lit_review_urban %>% arrange(Shorthand)

lit_review_coastal <- as.data.frame(read_excel(filename, sheet = "COASTAL"))
lit_review_coastal <- lit_review_coastal %>% arrange(Shorthand)

lit_review_ag <- as.data.frame(read_excel(filename, sheet = "AGRICULTURE"))
lit_review_ag <- lit_review_ag %>% arrange(Shorthand)

lit_review <- list(lit_review_urban, lit_review_coastal, lit_review_ag)

##########################################
########## WORKBOOKS #####################
##########################################

df_blank <- data.frame(NA)

for(i in 1:length(teams)){
  
  filename <- paste0("Winnowing_Workbook_",str_to_upper(teams[i]),".xlsx")
  xlsx::write.xlsx(df_blank, file=filename, sheetName = "Weights", row.names=FALSE, col.names=TRUE, append=FALSE)
  
  #########################
  #### META-DATA
  #########################
  
  sh_name <- "Meta-data"

  meta <- rbind(
    c("Sheet name", "Description"),
    c("Weights", paste0("The relative prioritization of the six selection critiera. ",
                        "We used a weighting scale from 0-5, but any range can be used. ", 
                        "A weight of zero will mean that the criteria does not matter for indicator selection,", 
                        "and the criteria will be excluded from the remaining calculations.")),
    c("W_Scaled", "The weights identified in sheet 'Weights' are scaled from 0 to 1."),
    c("U_Scaled", paste0("The weights identified in sheet 'Weights' for Relevant, Resonant, ",
                         "and Responsive are scaled from 0 to 1 since the weights for these ",
                         "criteria are used to calculate the score for Confidence, which is measure ",
                         "of within-team scoring alignment.")),
    c(paste0(teams[i],"_SD"),paste0("The standard deviation of scores for Relevant, Resonant, and Responsive ",
                              "are weighted, summed, and then scaled between 0 and 3. This value is then ",
                              "subtracted from 3 to produce the final value of Confidence. We calculate the ", 
                              "'inverse' because high standard deviations indicate low team Confidence.")),
    c(str_to_upper(teams[i]), paste0("The full confidence table with indicators as rows and the six criteria as ",
                                     "columns. The column 'Overall Score' calculates the weighted sum for each ",
                                     "indicator. The column 'Rank' ranks the Overall Scores from highest to lowest.")),

    c("",""),
    
    c("Column heading", "Description"),
    c("Criteria", paste0("Selection criteria that indicators were evaluated for. Included: (1) Relevant to TNC ",
                         "projects; (2) Resonant with community members; (3) Responsive to changes in the system; ",
                         "(4) Confidence of team scoring (i.e., low standard deviation of scores for the first three ",
                         "criteria); (5) Realism of time and resources needed to access data; and (6) ",
                         "Data-availability of the indicator for time and space")),
    c("Weights", paste0("The relative prioritization of the six selection critiera. We used a weighting scale ",
                        "from 0-5, but any range can be used. A weight of zero will mean that the criteria does ",
                        "not matter for indicator selection, and the criteria will be excluded from the ",
                        "remaining calculations.")),
    c("Scaled (0-1)", paste0("Weights are scaled between zero and one and then used to calculate the weight ",
                             "sum scores (i.e., Overall Score).")),
    c("Scaled_SD", paste0("Weights are scaled between zero and one specifically for Relevant, Resonant, ",
                          "and Resposive and used to calculate the score for Confidence.")),
    c("Metric", "Full name of indicators"),
    c("Shorthand", "Shortened indicator name for easier reference and visualizations"),
    c("Relevant_SD", "The standard deviation of team member scores for Relevant"),
    c("Resonant_SD", "The standard deviation of team member scores for Resonant"),
    c("Responsive_SD", "The standard deviation of team member scores for Responsive"),
    c("SD_WSM", "Standard deviations are summed (Relevant_SD + Resonant_SD + Responsive_SD)"),
    c("SD_WSMscale", paste0("The sum of standard deviations (SD_WSM) are scaled between 0 and 3 to ",
                            "match the scales for the other criteria")),
    c("Inverse", paste0("The inverse of the scaled sum of standard deviations (SD_WSMsclale) since a ",
                        "lower standard deviation reflects higher team Confidence in that indicator")),
    c("Relevant", "Mean score for Relevant (Relevant to TNC projects)"),
    c("Resonant", "Mean score for Resonant (Resonant with community members)"),
    c("Responsive", "Mean score for Responsive (Responsive to changes in the system)"),
    c("Confidence", paste0("Values are drawn from column 'Inverse' in sheet '", teams[i],"_SD'")),
    c("Realism", "Score for Realism (Realism of time and resources needed to access data)"),
    c("Data-availability", "Score for Data-availability (Data-availability of time and dasources needed to access data)"),
    c("Overall Score", "Weighted sum of the six criteria "),
    c("Current Rank", "Overall scores ranked from highest to lowest (1st = highest Overall Score)"),
    c("Scenario 'X'", "Columns to save different rankings to compared differences between weight scenarios"))
  
  info <- meta
  
  wb <- loadWorkbook(filename)
  sheets <- names(getSheets(wb))
  if(sh_name %in% sheets == TRUE){
    removeSheet(wb, sheetName = sh_name)
    saveWorkbook(wb, filename)
    write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=FALSE)
  }else{write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=FALSE, append=FALSE)} 
  
  #########################
  #### WEIGHTS
  #########################
  
  sh_name <- "Weights"
  
  df_weights <- as.data.frame(cbind(criteria, weights[[i]])) # combine criteria & weights
  df_weights[,2] <- sapply(df_weights[,2], as.numeric) # make weights numeric
  colnames(df_weights) <- c("Criteria", "Weights") # change column names
  
  info <- df_weights
  
  wb <- loadWorkbook(filename)
  sheets <- names(getSheets(wb))
  if(sh_name %in% sheets == TRUE){
    removeSheet(wb, sheetName = sh_name)
    saveWorkbook(wb, filename)
    write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)
  }else{write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)}
  
  
  ###########################
  #### ALL WEIGHTS normalized
  ###########################
  
  sh_name <- "W_Scaled"

    df_weights_N <- df_weights
  df_weights_N$Weights <- paste0("=Weights!B",1:6+1L)
  df_weights_N$`Scaled (0-1)` <- paste0("=IF(COUNTIF($B$2:$B$7,$B$2)=6,1,B",1:6+1L,"/MAX($B$2:$B$7))")
  
  info <- df_weights_N
  
  wb <- loadWorkbook(filename)
  sheets <- names(getSheets(wb))
  if(sh_name %in% sheets == TRUE){
    removeSheet(wb, sheetName = sh_name)
    saveWorkbook(wb, filename)
    write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)
  }else{write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)}

  ####################################
  #### NORM'ed WEIGHTS for Confidence
  ####################################
  
  sh_name <- "U_Scaled"
  
  df_weights_U <- df_weights[1:3,]
  df_weights_U$Weights <- paste0("=Weights!$B$",1:3+1L)
  df_weights_U$Scaled_SD <- paste0("=IF(COUNTIF($B$2:$B$4,$B$2)=3,1,B",1:3+1L,"/MAX($B$2:$B$4))")

  info <- df_weights_U
  
  wb <- loadWorkbook(filename)
  sheets <- names(getSheets(wb))
  if(sh_name %in% sheets == TRUE){
    removeSheet(wb, sheetName = sh_name)
    saveWorkbook(wb, filename)
    write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)
  }else{write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)}
  
  #########################
  #### StDev of TEAM SCORES
  #########################
  
  sh_name <- paste0(teams[i], "_SD")
  
  df_sd <- team_scores[[i]] %>% 
    select(c("Metric", "Shorthand", "Relevant_SD", "Resonant_SD", "Responsive_SD"))
  
  df_sd$SD_WSM <- paste0("=U_Scaled!$C$2*$C$",1:78+1L,"+U_Scaled!$C$3*$D$",1:78+1L,"+U_Scaled!$C$4*$E$",1:78+1L)
  df_sd$SD_WSMscale <- paste0("=IF(AND(U_Scaled!$B$2=0,U_Scaled!$B$3=0,U_Scaled!$B$4=0),0,3/(MAX($F$2:$F$79)-MIN($F$2:$F$79))*(F",
                          1:78+1L,"-MAX($F$2:$F$79))+3)")
  df_sd$Inverse <- paste0("=3-$G",1:78+1L)
  
  info <- df_sd
  
  wb <- loadWorkbook(filename)
  sheets <- names(getSheets(wb))
  if(sh_name %in% sheets == TRUE){
    removeSheet(wb, sheetName = sh_name)
    saveWorkbook(wb, filename)
    write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)
  }else{write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)}
  
  ##############################
  #### Scores for all 6 CRITERIA
  ##############################
  
  sh_name <- str_to_upper(teams[i])
  
  # extract columns with mean scores of Relevant, Resonant, and Responsive
  df_all <- team_scores[[i]] %>% 
    select(c("Metric", "Shorthand", "Relevant_Mean", "Resonant_Mean", "Responsive_Mean")) %>% 
    rename(Relevant = Relevant_Mean, Resonant = Resonant_Mean, Responsive = Responsive_Mean)

  df_all$Confidence <- paste0("=",teams[i],"_SD!$H$",1:78+1L)
  
  df_data <- lit_review[[i]]
  
  df_all <- df_all %>% 
    mutate(Realism = df_data$Realism, `Data-availability` = df_data$`Data-richness`)
  
  df_all$OverallScore <- paste0("=C",1:78+1L,"*W_Scaled!$C$2 + ",
                        "D",1:78+1L,"*W_Scaled!$C$3 + ",
                        "E",1:78+1L,"*W_Scaled!$C$4 + ",
                        "F",1:78+1L,"*W_Scaled!$C$5 + ",
                        "G",1:78+1L,"*W_Scaled!$C$6 + ",
                        "H",1:78+1L,"*W_Scaled!$C$7")
  
  df_all[is.na(df_all)] <- 0.001
  
  df_all$CurrentRank <- paste0("=SUMPRODUCT(($I",1:78+1L,"<=$I$2:$I$79)/COUNTIF($I$2:$I$79,$I$2:$I$79))")
  
  df_all$ScenarioA <- rep(NA,78)
  df_all$ScenarioB <- rep(NA,78)
  df_all$ScenarioC <- rep(NA,78)
  df_all$ScenarioD <- rep(NA,78)

  info <- df_all
  
  wb <- loadWorkbook(filename)
  sheets <- names(getSheets(wb))
  if(sh_name %in% sheets == TRUE){
    removeSheet(wb, sheetName = sh_name)
    saveWorkbook(wb, filename)
    write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)
  }else{write.xlsx(info, file=filename, sheetName = sh_name, row.names=FALSE, col.names=TRUE, append=TRUE)}

}

##########################################
########## FORMATTING ####################
##########################################

for(i in 1:length(teams)){
  
  filename <- paste0("Winnowing_Workbook_",str_to_upper(teams[i]),".xlsx")
  
  wb <- loadWorkbook(filename) # load workbook
  sheets <- getSheets(wb) # load sheets
  
  # sheet = Meta-data
  setColumnWidth(sheets[[1]], colIndex=1, colWidth=15)
  setColumnWidth(sheets[[1]], colIndex=2, colWidth=60)
  n.rows <-  length(getRows(sheets[[1]], rowIndex = NULL)) # select all rows
  all.rows <- getRows(sheets[[1]], rowIndex=1:n.rows) # select all rows:columns
  all.cells <- getCells(all.rows, colIndex = 1:15) # select all cells within the rows:columns
  cs <- CellStyle(wb) + Alignment(horizontal = "ALIGN_LEFT", vertical = "VERTICAL_TOP", wrapText = TRUE) # set style
  invisible(lapply(all.cells, setCellStyle, cs)) # apply style to all cells
  first.row <- getRows(sheets[[1]], rowIndex=c(1,8)) # select all rows:columns
  all.cells <- getCells(first.row, colIndex = 1:2) # select all cells within the rows:columns
  cs <- CellStyle(wb) + Font(wb,  heightInPoints=11, isItalic=FALSE, isBold=TRUE) +
    Alignment(horizontal = "ALIGN_LEFT", vertical = "VERTICAL_TOP", wrapText = TRUE)
  invisible(lapply(all.cells, setCellStyle, cs)) # apply style to all cells
  
  # sheet = Team_SD
  setColumnWidth(sheets[[5]], colIndex=1, colWidth=60)
  setColumnWidth(sheets[[5]], colIndex=2, colWidth=40)
  setColumnWidth(sheets[[5]], colIndex=3:10, colWidth=14)
  
  # sheet = TEAM
  setColumnWidth(sheets[[6]], colIndex=1, colWidth=60)
  setColumnWidth(sheets[[6]], colIndex=2, colWidth=40)
  setColumnWidth(sheets[[6]], colIndex=3:13, colWidth=12)
  
  # sheets = Weights, W_Scaled, & U_Scaled
  for(k in 2:4){
    setColumnWidth(sheets[[k]], colIndex=1:3, colWidth=16)
  }
  
  # Bold and align first rows
  for(k in 1:length(sheets)){
    n.rows <-  length(getRows(sheets[[k]], rowIndex = NULL)) # select all rows
    all.rows <- getRows(sheets[[k]], rowIndex=1:n.rows) # select all rows:columns
    all.cells <- getCells(all.rows, colIndex = 1:15) # select all cells within the rows:columns
    cs <- CellStyle(wb) + Alignment(horizontal = "ALIGN_LEFT", vertical = "VERTICAL_TOP", wrapText = TRUE) # set style
    invisible(lapply(all.cells, setCellStyle, cs)) # apply style to all cells
    
    first.row <- getRows(sheets[[k]], rowIndex=1:1) # select all rows:columns
    all.cells <- getCells(first.row, colIndex = 1:15) # select all cells within the rows:columns
    cs <- CellStyle(wb) + Font(wb,  heightInPoints=11, isItalic=FALSE, isBold=TRUE) +
      Alignment(horizontal = "ALIGN_LEFT", vertical = "VERTICAL_TOP", wrapText = TRUE)
    invisible(lapply(all.cells, setCellStyle, cs)) # apply style to all cells
  }
  
  saveWorkbook(wb, filename)
  
}


