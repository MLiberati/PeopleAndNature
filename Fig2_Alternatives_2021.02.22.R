
##### PURPOSE: Display breakdown of Brainstorming indicators by quality-of-life
#####           domain and conservation program
##### AUTHOR: Marjorie R. Liberati

#### INPUT files:
# "GLOBAL LIST - METRIC LISTS.xlsx"
# "SCORING LIST - METRIC LIST.xlsx"
# "Overlap _ Additional Indicators - 2019.08.12.xlsx"

#### OUTPUTS:
# Figure 2 in "Planning for People and Nature" 

###------------ SETUP ------------### 

library(readxl) # read excel files directly without converting to csv first
library(xlsx) # write directly to excel sheets
library(dplyr) # create summary tables and information
library(tidyr) # organize data
library(reshape) # use to restructure (i.e., "melt") data frames
library(rcompanion) # pairwise comparisons for fisher tests
library(ggplot2) # create figures
library(ggthemes) # access colorblind-friendly palettes
library(cowplot) # arrange ggplot figures into a panel

###----------- IMPORT DATASETS -----------###

rm(list = ls())

global <- as.data.frame(read_excel("GLOBAL LIST - METRIC LISTS.xlsx", sheet = "FULL LIST", col_names=TRUE))
names(global)

score <- as.data.frame(read_excel("SCORING LIST - METRIC LIST.xlsx", sheet = "SCORING_LIST", col_names=TRUE))
names(score)

score_byteam <- as.data.frame(read_excel("Overlap _ Additional Indicators - 2019.08.12.xlsx", 
                                         sheet = "SCORING", col_names=TRUE))
score <- mutate(score, Score = "Y")
names(score_byteam)

###------------ Merge Global and Scoring Lists ------------### 

df <- full_join(global, score, by=c("ID", "Domain"))
df <- df %>% 
  mutate(Metric = if_else(is.na(Metric.x), Metric.y, Metric.x)) %>%
  select(-Metric.x, -Metric.y)

# update indicator wording
newInd <- df[which(df$ID > 274),]
newInd$Metric

# Update ID163:
global[grep("trust", global$Metric, ignore.case=TRUE),]
ID163 <- df[which(df$ID==163),]
ID163$Metric <- df$Metric[which(df$ID==277)] #Change metric to wording to ID277's
ID163$Urban <- 1 #Make Urban's dummy variable a 1
ID163$Score <- "Y" #Make the Scoring list dummy variable a 'yes'
df[which(df$ID==163),] <- ID163 # update row in data frame
df <- df[-which(df$ID==277),] # remove ID 277

# Update ID126:
global[grep("rice", global$Metric, ignore.case=TRUE),]
ID126 <- df[which(df$ID==126),]
ID126$Metric <- df$Metric[which(df$ID==282)] #Change metric to wording to ID282's
ID126$Score <- "Y" #Make the Scoring list dummy variable a 'yes'
df[which(df$ID==126),] <- ID126 # update row in data frame
df <- df[-which(df$ID==282),] # remove ID 282

# Update ID110:
global[grep("safe", global$Metric, ignore.case=TRUE),]
ID110 <- df[which(df$ID==110),]
ID110$Metric <- df$Metric[which(df$ID==281)] #Change metric to wording to ID281's
ID110$Score <- "Y" #Make the Scoring list dummy variable a 'yes'
ID110$Domain <- "Social cohesion"
df[which(df$ID==110),] <- ID110 # update row in data frame
df <- df[-which(df$ID==281),] # remove ID 281

# Update ID145:
global[grep("cultural", global$Metric, ignore.case=TRUE),]
ID145 <- df[which(df$ID==145),]
ID145$Metric <- df$Metric[which(df$ID==276)] #Change metric to wording to ID276's
ID145$Urban <- 1 #Make Urban's dummy variable a 1
ID145$Score <- "Y" #Make the Scoring list dummy variable a 'yes'
df[which(df$ID==145),] <- ID145 # update row in data frame
df <- df[-which(df$ID==276),] # remove ID276

# Add missing team information
df[which(df$ID > 274),]
df[which(df$ID > 274),which(colnames(df)=="Overlap")] <- 0 # new variables did not overlap
df[which(df$Metric == "Number of farms with succession plans"),which(colnames(df)=="Ag")] <- 1
df[which(df$Metric == "Number of events that use natural areas"),which(colnames(df)=="Coastal")] <- 1
df[which(df$Metric == "Satisfaction with distribution of public/private funding within community"),which(colnames(df)=="Urban")] <- 1
df[which(df$Metric == "Revitalization index - dollars on infrastructure improvement; number of revitalization projects; number of new businesses; number of visitors"),which(colnames(df)=="Urban")] <- 1
df$Ag <- replace_na(df$Ag, 0)
df$Coastal <- replace_na(df$Coastal, 0)
df$Urban <- replace_na(df$Urban, 0)
df$Score <- replace_na(df$Score, "N")

###------------ Global vs. Scoring Lists ------------### 

summary(as.factor(df$Score))

# Domain cnts in Global List
df_global_cnt <- df %>% 
  group_by(Domain) %>% 
  count(name = "Global List") %>%
  arrange(desc(`Global List`))
df_global_cnt

domain_order <- df_global_cnt$Domain

# Domain cnts for Scoring List
df_score_cnt <- df %>% 
  filter(Score ==  "Y") %>%
  group_by(Domain) %>% 
  count(name = "Scoring List")
df_score_cnt

df_cnt <- melt(as.data.frame(left_join(df_global_cnt, df_score_cnt, by = "Domain")))

df_percent <- df_cnt %>%
  group_by(variable) %>% 
  mutate(per = value/sum(value)*100) 
cast(df_percent, Domain ~ variable, value = "per")

df_percent$Domain <- factor(df_percent$Domain, levels=domain_order)

plt_lists <- 
  ggplot(df_percent, aes(x=Domain, y=per, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge(), color="black") +
  # geom_text(aes(label = value), position=position_dodge(width=0.9), vjust=-0.25,
  #           family="serif", size=4) +
  scale_y_continuous(limits = c(0, 30), breaks=seq(0,30,10)) +
  scale_fill_grey() +
  # scale_fill_manual(values = c("#2D708EFF", "#73D055FF")) +
  theme_bw(base_family = "serif") + 
  ylab("Percent by list") +
  xlab(element_blank()) + 
  theme(legend.position = "right",
        legend.background = element_rect(color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.title.y = element_text(margin=margin(0,10,0,0)),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size=12),
        plot.margin = margin(5.5,5.5,-5,5.5,"pt"))
plt_lists

###------------ Global and Scoring Lists by team ------------### 

# Domain cnts in Global List by team
df_melt <- melt(df,
             id.vars = c("Domain", "Score","Metric"),
             measure.vars = c("Ag", "Coastal", "Urban"))

df_global_team <- df_melt %>% 
  filter(value == 1) %>%
  group_by(Domain, variable) %>% 
  count(name = "Global List")
df_global_team

cast(df_global_team, Domain ~ variable)

# Domain cnts for Scoring List by team
df_score_team <- df_melt %>% 
  filter(value == 1, Score == "Y") %>%
  group_by(Domain, variable) %>% 
  count(name = "Scoring List")
df_score_team

df_team_cnt <- melt(as.data.frame(left_join(df_global_team, 
                                            df_score_team, 
                                            by = c("Domain","variable"))))
df_team_cnt
colnames(df_team_cnt) <- c("Domain", "Team", "List", "n")
head(df_team_cnt)

df_team_percent <- df_team_cnt %>%
  group_by(List, Team) %>% 
  mutate(per = n/sum(n)*100) 
cast(df_team_percent, Domain + List ~ Team, value = "per")

df_team_percent$Domain <- factor(df_team_percent$Domain, levels=domain_order)

plt_teams <- 
  ggplot(df_team_percent, aes(x=Domain, y=per, fill=Team)) +
  geom_bar(stat="identity", position=position_dodge(), color="black") +
  facet_wrap(facets = vars(List), nrow=2, strip.position = "right") +
  scale_y_continuous(limits = c(0, 30), breaks=seq(0,30,10)) +
  scale_x_discrete(labels=c("Work &\nLeisure", "Health", "Social\ncohesion",
                            "Living\nstandards", "Eduction", "Security", "Governance")) +
  # scale_fill_grey() +
  scale_fill_manual(values = c("#73D055FF", "#2D708EFF", "#FDE725FF"),
                    labels = c("Agriculture", "Coastal wetlands", "Urban water")) +
  theme_bw(base_family = "serif") + 
  ylab("Percent by team") +
  theme(legend.position = "right",
        legend.background = element_rect(color="black"),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        axis.title = element_text(size=14),
        axis.title.y = element_text(margin=margin(0,10,0,0)),
        axis.title.x = element_text(margin=margin(10,0,0,0)),
        axis.text = element_text(size=12),
        strip.text = element_text(color="black", face="bold", size=12),
        strip.background = element_rect(fill="white", color="black"),
        plot.margin = margin(-30,5.5,5.5,5.5,"pt"))
plt_teams

###------------ Combine figures into a single panel ------------### 

plt_panel <- plot_grid(plt_lists, 
          plt_teams,
          ncol = 1,
          align = "hv",
          axis = "br",
          rel_heights = c(1,1.5))
plt_panel

save_plot(plt_panel, 
          filename = "Fig2_Planning_for_people_and_nature.tiff",
          ncol = 1, nrow = 2, 
          base_height = 4, base_width = 10)
