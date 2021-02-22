
##### PURPOSE: Display consequence scores for quality-of-life 
#####           indicators in Scoring List
##### AUTHOR: Marjorie R. Liberati

#### INPUT files:
# "IndividualScores_URBAN_2019.11.05.xlsx"
# "IndividualScores_AGRICULTURE_2019.11.05.xlsx"
# "IndividualScores_COASTAL_2019.11.05.xlsx"
# "Indicator Scoring - Data-richness & Realism - 2020.04.16.xlsx"
# "SCORING LIST - METRIC LIST - with Shorthand.xlsx"

#### OUTPUTS:
# Figure 3 in "Planning for People and Nature" 
# Figure D4 in "Planning for People and Nature" 

###------------ SETUP ------------### 

rm(list = ls())

library(readxl) # read excel files directly without converting to csv first
library(writexl) # write directly to excel sheets
library(dplyr) # create summary tables and information
library(reshape2) # use to restructure (i.e., "melt" and "cast") data frames
library(ggplot2) # create figures
library(stringr) # format character objects
library(tibble) # changing dplyr tibbles
library(ggpubr) # arrange ggplots into a grid
library(ggpubr) # arrange plots
library(FSA) # access Dunn tests

###------------ DATA & INFORMATION ------------### 

teams <- c("Urban", "Coastal", "Agriculture") # team names

criteria <- c("Relevant", "Resonant", "Responsive", "Confidence", "Realism", "Data-availability")

Criteria <- rep(rep(criteria[1:3], each=78), 2)
Statistic <- c(rep("Mean",78*3),rep("SD", 78*3))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# Relevant, Resonant, Responsive, & Confidence

filename <- "IndividualScores_URBAN_2019.11.05.xlsx"
df_urban <- as.data.frame(read_excel(filename, sheet = "URBAN"))
df_urban <- select(df_urban, -contains("Rank"))
df_urban <- melt(df_urban)
df_urban$Criteria <- Criteria
df_urban$Statistic <- Statistic
df_urban$Team <- "Urban"
df_urban$variable <- NULL

filename <- "IndividualScores_COASTAL_2019.11.05.xlsx"
df_coastal <- as.data.frame(read_excel(filename, sheet = "COASTAL"))
df_coastal <- select(df_coastal, -contains("Rank"))
df_coastal <- melt(df_coastal)
df_coastal$Criteria <- Criteria
df_coastal$Statistic <- Statistic
df_coastal$Team <- "Coastal"
df_coastal$variable <- NULL

filename <- "IndividualScores_AGRICULTURE_2019.11.05.xlsx"
df_ag <- as.data.frame(read_excel(filename, sheet = "AGRICULTURE"))
df_ag <- select(df_ag, -contains("Rank"))
df_ag <- melt(df_ag)
df_ag$Criteria <- Criteria
df_ag$Statistic <- Statistic
df_ag$Team <- "Agriculture"
df_ag$variable <- NULL

df_all <- rbind(df_urban, df_coastal, df_ag)

# Realism & DataAvailability

filename <- "Indicator Scoring - Data-richness & Realism - 2020.04.16.xlsx"

df_data_ag <- as.data.frame(read_excel(filename, sheet = "AGRICULTURE"))
df_data_ag <- select(df_data_ag, Shorthand, Realism, 'Data-availability')
df_data_ag <- melt(df_data_ag)
colnames(df_data_ag) <- c("Shorthand", "Criteria", "value")
df_data_ag$Team <- "Agriculture"
df_data_ag$Statistic <- "Mean"

df_data_coastal <- as.data.frame(read_excel(filename, sheet = "COASTAL"))
df_data_coastal <- select(df_data_coastal, Shorthand, Realism, 'Data-availability')
df_data_coastal <- melt(df_data_coastal)
colnames(df_data_coastal) <- c("Shorthand", "Criteria", "value")
df_data_coastal$Team <- "Coastal"
df_data_coastal$Statistic <- "Mean"

df_data_urban <- as.data.frame(read_excel(filename, sheet = "URBAN"))
df_data_urban <- select(df_data_urban, Shorthand, Realism, 'Data-availability')
df_data_urban <- melt(df_data_urban)
colnames(df_data_urban) <- c("Shorthand", "Criteria", "value")
df_data_urban$Team <- "Urban"
df_data_urban$Statistic <- "Mean"

df_data <- bind_rows(df_all, df_data_urban, df_data_coastal, df_data_ag)
df_data$value <- as.numeric(df_data$value)

# Merge all criteria together

df <- bind_rows(df_data, df_data)
df$Scenario <- c(rep("Scenario A", nrow(df_data)), rep("Scenario B", nrow(df_data)))
df$Criteria <- as.factor(df$Criteria)
df$Statistic <- as.factor(df$Statistic)
df$Team <- as.factor(df$Team)

########## CASE STUDY WEIGHTS ########## 

S1 <- c(10, 10, 10, 10, 10, 10) # equal weights
S2 <- c(10, 10, 10, 10, 10, 10) # equal weights
Weight <- c(S1, S2)
Criteria <- rep(criteria, 2)
Scenario <- c(rep("Scenario A",length(S1)),rep("Scenario B", length(S2)))
weights <- data.frame(Weight, Criteria, Scenario, stringsAsFactors = TRUE)

###########################
#### ALL WEIGHTS normalized
###########################

scaled <- weights %>% 
  group_by(Scenario) %>%
  mutate(Scaled = Weight/max(Weight))

df <- left_join(df, scaled, by = c("Criteria","Scenario"))
df$Scenario <- as.factor(df$Scenario)
df$Criteria <- as.factor(df$Criteria)

#########################
#### WEIGHT * SCORES
#########################

df <- df %>%
  mutate(WScore = value * Scaled)

#########################
#### StDev of TEAM SCORES
#########################

df_sd <- df %>%
  filter(Statistic == "SD") %>%
  filter(Criteria == "Relevant" | Criteria == "Resonant" | Criteria == "Responsive")

df_sd <- df_sd %>%
  group_by(Scenario, Team, Shorthand) %>%
  summarise(USum = sum(WScore)) %>%
  mutate(value = 3 - (3 / (max(USum)-min(USum)) * (USum - max(USum)) + 3))

df_sd <- left_join(df_sd, filter(scaled, Criteria=='Confidence'), by = "Scenario")
df_sd <- df_sd %>%
  mutate(WScore = value * Scaled)

df_sd$value <- round(df_sd$value, 2)
df_sd$WScore <- round(df_sd$WScore, 2)
df_sd$USum <- NULL
df_sd$Criteria <- "Confidence"
df_sd$Statistic <- "Mean"

##############################
#### Scores for all 6 CRITERIA
##############################

df6 <- bind_rows(df, df_sd)
df6$Scenario <- as.factor(df6$Scenario)
df6$Criteria <- as.factor(df6$Criteria)
df6$Team <- as.factor(df6$Team)
df6$Statistic <- as.factor(df6$Statistic)

##############################
#### ADD DOMAINS
##############################

filename <- "SCORING LIST - METRIC LIST - with Shorthand.xlsx"
df_domains <- as.data.frame(read_excel(filename, sheet = "SCORING_LIST"))

df6 <- left_join(df6, df_domains[,3:4], by = "Shorthand")

##############################
#### OVERALL SCORES & RANKINGS
##############################

df_WSM <- df6 %>%
  filter(Statistic == "Mean") %>%
  group_by(Scenario, Team, Shorthand) %>%
  summarise(Overall = sum(WScore), Domain = first(Domain)) %>%
  arrange(Team, desc(Overall))

df_rank <- df6 %>%
  filter(Statistic == "Mean") %>%
  group_by(Scenario, Team, Shorthand) %>%
  summarise(Overall = sum(WScore), Domain = first(Domain)) %>%
  mutate(Rank = dense_rank(desc(Overall)))

########################################
############## FIGURE 3 ################
########################################

cs <- left_join(df6, df_rank, by = c("Team", "Shorthand", "Scenario", "Domain"))
target <- c("Relevant", "Resonant", "Responsive", "Confidence")

cs <- filter(cs, Statistic == "Mean", Criteria %in% target)
cs <- arrange(cs, desc(Overall))

cs$Shorthand <- as.factor(cs$Shorthand)
cs$Criteria <- factor(cs$Criteria, levels=criteria)
cs$Team <- factor(cs$Team, levels=rev(teams))

metric_order <- unique(select(cs, Shorthand, Team, Scenario, WScore, Overall)) %>%
  mutate(order = row_number()) %>%
  select(Shorthand, Team, Scenario, order)

plt <- ggplot(cs, aes(y=WScore, fill=Team)) +
  geom_boxplot() +
  facet_grid(rows=vars(as.factor(Criteria)), cols=vars(as.factor(Domain)),
             switch = "x") +
  scale_fill_manual(values = c("#73D055FF", "#2D708EFF", "#FDE725FF"),
                    labels = c("Agriculture", "Coastal wetlands", "Urban water")) +
  theme_light(base_family = "serif") +
  ylab("Mean indicator score") +
  xlab("Quality of life domain") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill="white", linetype="solid", colour="black"),
        legend.title = element_text(size=10),
        strip.text.y = element_text(size=12, face = "bold"),
        strip.text.x = element_text(size=10),
        strip.background = element_rect(fill="gray20"),
        axis.title = element_text(size=14),
        axis.text = element_text(size=11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

filename <- "Fig3_Planning_for_people_and_nature.tiff"
ggsave(filename, plt, dpi=300, width = 8, height = 6, unit = "in")

########################################
############# FIGURE D4 ################
########################################

cs <- left_join(df6, df_rank, by = c("Team", "Shorthand", "Scenario", "Domain"))
target <- c("Realism", "Data-availability")

cs <- filter(cs, Statistic == "Mean", Criteria %in% target)
cs <- arrange(cs, desc(Overall))

cs$Shorthand <- as.factor(cs$Shorthand)
cs$Criteria <- factor(cs$Criteria, levels=criteria)
cs$Team <- factor(cs$Team, levels=rev(teams))

metric_order <- unique(select(cs, Shorthand, Team, Scenario, WScore, Overall)) %>%
  mutate(order = row_number()) %>%
  select(Shorthand, Team, Scenario, order)

plt <- ggplot(cs, aes(y=WScore, fill=Team)) +
  geom_boxplot() +
  facet_grid(rows=vars(as.factor(Criteria)), cols=vars(as.factor(Domain)),
             switch = "x") +
  scale_fill_manual(values = c("#73D055FF", "#2D708EFF", "#FDE725FF"),
                    labels = c("Agriculture", "Coastal wetlands", "Urban water")) +
  theme_light(base_family = "serif") +
  ylab("Mean indicator score") +
  xlab("Quality of life domain") +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(fill="white", linetype="solid", colour="black"),
        legend.title = element_text(size=10),
        strip.text.y = element_text(size=12, face = "bold"),
        strip.text.x = element_text(size=10),
        strip.background = element_rect(fill="gray20"),
        axis.title = element_text(size=14),
        axis.text = element_text(size=11),
        axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")),
        axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

filename <- "FigD4_Planning_for_people_and_nature.tiff"
ggsave(filename, plt, dpi=300, width = 8, height = 6, unit = "in")