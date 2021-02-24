
##### PURPOSE: Display scores for quality-of-life indicators in Scoring List
##### AUTHOR: Marjorie R. Liberati

#### INPUT files:
# "URBAN - Individual Scores - Revised - 2019.11.05.xlsx"
# "AGRICULTURE - Individual Scores - Revised - 2019.11.05.xlsxx"
# "COASTAL - Individual Scores - Revised - 2019.11.05.xlsx"

#### OUTPUTS:
# Figure 3 in "Planning for People and Nature" 

###------------ SETUP ------------### 

rm(list = ls())

library(readxl) # read excel files directly without converting to csv first
library(xlsx) # write directly to excel sheets
library(dplyr) # create summary tables and information
library(reshape2) # use to restructure (i.e., "melt" and "cast") data frames
library(ggplot2) # create figures
library(ggthemes) # access colorblind friendly pallettes
library(stringr) # format character objects
library(lme4) # regression analyses
library(car)
library(ggpubr)
library(rcompanion)

###------------ REFERENCE LISTS ------------### 

teams <- c("Urban", "Coastal", "Agriculture") # team names

criteria <- c("Relevant", "Resonant", "Responsive")

initials <- c("A1", "A2", "A3", "C1", "C2", "C3", "C4", "U1", "U2", "U3", "U4")
initials_urban <- initials[8:11]
initials_coastal <- initials[4:7]
initials_ag <- initials[1:3]

###------------ UPDATED METRIC SCORES ------------### 

# Excel file with metric scores - each sheet is a Person-Criteria combo (i.e., 3 sheets per person)
# In the team for-loops:
# 1. Identify team's filename
# 2. Extract the an individual's sheet to a dataframe based on Initials_Criteria naming
# 3. Assing the Initial_Criteria dataframe to a unique object with same naming convention
# 4. Also add Initial_Criteria dataframe to team's dataframe that compiles all Initial-Criteria combinations

# URBAN
df_urban <- data.frame()
for(i in 1:length(initials_urban)){
  for(j in 1:length(criteria)){
    filename <- paste0(teams[1]," - Individual Scores - Revised - 2019.11.05.xlsx") 
    df <- as.data.frame(read_excel(filename, sheet = paste0(initials_urban[i],"_",criteria[j])))
    assign(paste0(initials_urban[i],"_",criteria[j]), df)
    df_urban <- rbind(df_urban, get(paste0(initials_urban[i],"_",criteria[j])))
  }}
# Remove U4's scores for Relevant because was a recent hire and was still learning about the program 
df_urban[df_urban$Individual=="U4" & df_urban$Criteria=="Relevant",]$Score <- NA 

# COASTAL
df_coastal <- data.frame()
for(i in 1:length(initials_coastal)){
  for(j in 1:length(criteria)){
    filename <- paste0(teams[2]," - Individual Scores - Revised - 2019.11.05.xlsx")
    df <- as.data.frame(read_excel(filename, sheet = paste0(initials_coastal[i],"_",criteria[j])))
    assign(paste0(initials_coastal[i],"_",criteria[j]), df)
    df_coastal <- rbind(df_coastal, get(paste0(initials_coastal[i],"_",criteria[j])))
  }}

# AGRICULTURE
df_ag <- data.frame()
for(i in 1:length(initials_ag)){
  for(j in 1:length(criteria)){
    filename <- paste0(teams[3]," - Individual Scores - Revised - 2019.11.05.xlsx") 
    df <- as.data.frame(read_excel(filename, sheet = paste0(initials_ag[i],"_",criteria[j]))) 
    assign(paste0(initials_ag[i],"_",criteria[j]), df) 
    df_ag <- rbind(df_ag, get(paste0(initials_ag[i],"_",criteria[j]))) 
  }}

# ALL THREE TEAMS
df_all <- rbind(df_urban, df_coastal, df_ag)
df_all$Score <- as.numeric(df_all$Score)

###------------ APPENDIX D FIGURES 1-3 ------------### 

titles <- c("Urban water scores", "Coastal wetland scores", "Agriculture scores")

counter <- 3

for(i in 1:length(teams)){
  
  filename <- "Indicator Scoring - Data-richness & Realism - 2020.04.16.xlsx" 
  df_data <- as.data.frame(read_excel(filename, sheet = toupper(teams[i])))
  df_data <- df_data %>% select(Shorthand, Realism, `Data-availability`)
  df_data <- melt(df_data)
  df_data[is.na(df_data)] <- 0.001
  colnames(df_data) <- c("Shorthand", "Criteria", "Score")
  
  df <- df_all %>% 
    filter(Team == teams[i])
  
  df_combo <- bind_rows(df, df_data)
  
  df_summary <- df_combo %>% 
    select(Shorthand, Criteria, Individual, Score) %>%
    group_by(Shorthand,Criteria) %>%
    summarise(MEAN = mean(Score, na.rm = TRUE), SD = sd(Score, na.rm = TRUE)) %>%
    mutate(yMin = ifelse(MEAN-SD < 0, 0, MEAN-SD), yMax = ifelse(MEAN+SD > 3, 3, MEAN+SD))
  df_summary[is.na(df_summary)] <- 0
  
  df_criteria <- df_summary %>% 
    filter(Criteria == "Relevant") %>%
    mutate(yMin = ifelse(MEAN-SD < 0, 0, MEAN-SD), yMax = ifelse(MEAN+SD > 3, 3, MEAN+SD)) %>%
    arrange(MEAN)
  
  metric_order <- df_criteria %>% pull(Shorthand)
  df_summary$Shorthand <- factor(df_summary$Shorthand, levels = metric_order)
  df_summary$Criteria <- factor(df_summary$Criteria, levels=c("Relevant", "Resonant", "Responsive", "Realism","Data-availability"))
  
  plt <- ggplot(df_summary, aes(x = Shorthand, y = MEAN, ymin=yMin, ymax=yMax)) +
    geom_pointrange(size=0.3) +
    geom_point() +
    facet_grid(cols = vars(Criteria)) +
    theme_light(base_family = "serif") + 
    ggtitle(titles[i]) +
    ylab("Score") +
    coord_flip() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          axis.title.y = element_blank())
  
  filename <- paste0("FigD", counter, "_Planning_for_people_and_nature",".tiff")
  ggsave(filename, plt, dpi=300, width = 6.5, height = 9, unit = "in")
  
  counter <- counter - 1
  
}

