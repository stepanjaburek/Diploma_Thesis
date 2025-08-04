#----------------------------------------------------------
# Graphs
#---------------------------------
# Setup
library(flexplot)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(ggbeeswarm)
library(data.table)

#---------------------------------
# Data
#---------------------------------

write.csv(data,"eu_data_with_mentions.csv")

#------------------------------------------
# filter parties by number of eu mentions
parties <- data %>% 
  group_by(party) %>% 
  reframe(EU_sum = sum(eu_mentions))


# lets keep at least 500 mentions
atleast500<- c("ANO", "ČSSD", "ODS", "KSČM", "KDU-ČSL","Piráti", "SPD", "TOP 09", "STAN") 

traditional<- c( "ČSSD", "ODS", "KSČM", "KDU-ČSL") 
new<- c("ANO", "Piráti", "SPD", "TOP 09", "STAN") 

#----------------------------------------------------------
# Calculate salience and group by party and year
party_sal <- data %>%
  group_by(party, year) %>%
  reframe(
    eu_salience = sum(eu_mentions >=1) / n(), # salience# Rauh and Parizek salience
    eu_institutions_salience = sum(eu_institutions_mentions >=1) / n(), 
    eu_total_salience = sum(eu_mentions >=1 | eu_institutions_mentions >=1) / n(),# EU + EU institutions
    parliament_salience = sum(parliament_mentions >=1) / n(),
    commission_salience = sum(comission_mentions >=1) / n(),
    council_salience = sum(council_mentions >=1) / n(),
    european_council_salience = sum(eu_council_mentions >=1) / n(),
    ecb_salience = sum(ecb_mentions >=1) / n(),
    ecj_salience = sum(ecj_mentions >=1) / n()
  ) %>% 
  filter(party %in% atleast500) %>%  # keep only parties with at least 500 mentions
  slice(-1)


total_sal <- data %>%
  group_by(year) %>%
  reframe(
    eu_salience = sum(eu_mentions >=1) / n(), # clean EU salience# Rauh and Parizek salience
    eu_institutions_salience = sum(eu_institutions_mentions >=1) / n(),
    eu_total_salience = sum(eu_mentions >=1 | eu_institutions_mentions >=1) / n(),# EU + EU institutions
    parliament_salience = sum(parliament_mentions >=1) / n(),
    commission_salience = sum(comission_mentions >=1) / n(),
    council_salience = sum(council_mentions >=1) / n(),
    european_council_salience = sum(eu_council_mentions >=1) / n(),
    ecb_salience = sum(ecb_mentions >=1) / n(),
    ecj_salience = sum(ecj_mentions >=1) / n()
  ) 

#----------------------------------------------------------
# Salience graphs
# Total time series

#-----------------------------------
# Figure 6.1
#------------
ggplot(total_sal, aes(x = year, y = eu_salience * 100)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Direct EU Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        title = element_text(size = 16))+
  theme_minimal(base_size = 18)+
   scale_x_continuous(breaks = seq(1993, 2023, 3))


#-----------------------------------
# Figure 6.2
#------------
ggplot(total_sal, aes(x = year, y = eu_institutions_salience*100)) +
  geom_line(color = "steelblue", linewidth = 1) +
  #geom_smooth(method = "loess")+
  labs(title = "EU Institutions Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  theme(axis.text = element_text(size = 16),
        axis.title = element_text(size = 35),
        title = element_text(size = 20))+
  theme_minimal(base_size = 18)+
  scale_x_continuous(breaks = seq(1993, 2023, 3))#+
  geom_point(color = "darkblue", size = 1.3)
  

#-----------------------------------
# Figure 6.3
#------------
ggplot(total_sal, aes(x = year, y = eu_total_salience*100)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Total EU Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        title = element_text(size = 16))+
  theme_minimal(base_size = 18)+
  scale_x_continuous(breaks = seq(1993, 2023, 3))


#----------------------------------------------------------
# Time series cross section visuals

plot_df1 <- party_sal
# Create a duplicate dataframe for the grey background lines
plot_df2 <- plot_df1 %>%
  rename(party_group_for_ghosts = party) 


#-----------------------------------
# Figure 6.5
#------------
ggplot() +
  geom_line(data = plot_df2, aes(x = year, y = eu_total_salience * 100,
                                 group = party_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + #
  geom_line(data = plot_df1, aes(x = year, y = eu_total_salience * 100,
                                 color = party), 
            linetype = "solid",
            linewidth = 1.2) +
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728",
                                "KDU-ČSL" = "#ffd700", "ANO" = "#add8e6", "SPD" = "#a0522d",
                                "TOP 09" = "#800080", "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  facet_wrap(~party) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  labs(y = "% of speeches mentioned", x = "Year")+
       theme(axis.text = element_text(size = 18),
         plot.title = element_text(size = 20),  
        plot.subtitle = element_text(size = 14),
        title = element_text(size = 20)) 

#----------------------------------------------------------
# plot yearly means - within and between case variation
#-----------------------------------
# Figure 6.4
#------------
party_sal %>% ggplot( aes(x = party, y = eu_total_salience*100,color = party)) +
  geom_beeswarm(alpha = 1, size = 2) +  
  geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "", y = "% of speeches mentioned", title = "Yearly EU Salience by Party")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700","ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


#---------------------------------------------------------------------------
#----------------------------------------------
# Stance graphs

stance_eu <- fread("stance_data.csv")

# again big parties only
stance_eu$support_eu <- ifelse(stance_eu$label=="supports", 1,0)
stance_eu$oppose_eu <- ifelse(stance_eu$label=="opposes", 1,0)
#--------------------
# stance total

#yearly stance scores, min N=10
stance_eu_yearly <- stance_eu %>% 
  group_by(year) %>% 
  reframe(
    support = sum(support_eu),
    oppose = sum(oppose_eu),
    total = n(),
    stance_score = (support-oppose)/total) %>% 
  dplyr::filter(total>10)
flexplot(support_eu ~ galtan, method = "logistic", data=stance_eu)

#-----------------------------------
# Figure 6.10
#------------
flexplot(stance_score ~ year, method = "loess", data=stance_eu_yearly) +
  labs(       title = "Stance Score Over Time: LOESS Smoothing",
       subtitle = "Dashed lines: EU accession (2004); Migration Crisis (2015-2017); Ukraine Invasion (2022)",
       y = "<- Oppose : Support ->", x = "Year") +
  ylim(-0.2, 0.2) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
    geom_vline(xintercept = 2015, linetype = "dashed", color = "purple",linewidth = 0.7) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "purple",linewidth = 0.7) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "green",linewidth = 0.7) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  theme_minimal() +
    theme(axis.text = element_text(size = 18),
         plot.title = element_text(size = 20),      # For the main title
        plot.subtitle = element_text(size = 14),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(1993, 2023, 3))



stance_eu<-stance_eu %>% 
  filter(party %in% atleast500)
# again big parties only
stance_eu$support_eu <- ifelse(stance_eu$label=="supports", 1,0)
stance_eu$oppose_eu <- ifelse(stance_eu$label=="opposes", 1,0)

#yearly stance scores, min N=10
stance_eu_party <- stance_eu %>% 
  group_by(party,year) %>% 
  reframe(
    support = sum(support_eu),
    oppose = sum(oppose_eu),
    total = n(),
    stance_score = (support-oppose)/total) %>% 
  slice(-1) %>% 
  filter(total>10)

plot_df1 <- stance_eu_party
plot_df2 <- stance_eu_party %>%
  rename(party_group_for_ghosts = party)

#-----------------------------------
# Figure 6.12
#------------
ggplot() +
  geom_line(data = plot_df2, aes(x = year, y = stance_score ,
                                 group = party_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + 
  geom_line(data = plot_df1, aes(x = year, y = stance_score ,
                                 color = party), 
            linetype = "solid",
            linewidth = 1.2) + 
  ylim(-0.55, 0.55) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728",
                                "KDU-ČSL" = "#ffd700", "ANO" = "#add8e6", "SPD" = "#a0522d",
                                "TOP 09" = "#800080", "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  facet_wrap(~party) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") +
  labs(y = "<- Oppose : Support ->", x = "Year") +
    theme(axis.text = element_text(size = 18),
         plot.title = element_text(size = 20),    
        plot.subtitle = element_text(size = 14),
        title = element_text(size = 20))

#-----------------------------------
# Figure 6.11
#------------
stance_eu_party %>% ggplot( aes(x = party, y = stance_score,color = party)) +
  geom_beeswarm(alpha = 1, size = 2) +  
  geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(subtitle = "Yearly EU Stance Score by Party",
       y = "<- Oppose : Support ->", x = "") +
  ylim(-0.55, 0.55) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700","ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 20),
        title = element_text(size = 24))


#----------------------------------------------
# Framing graphs
#-----------

frames_eu <- fread("frame_data.csv")

frames_eu$consequences <- ifelse(frames_eu$label=="Consequences", 1,0)
frames_eu$appropriateness <- ifelse(frames_eu$label=="Appropriateness", 1,0)
sum(frames_eu$appropriateness)

#yearly stance scores, min N=10
frames_eu_yearly <- frames_eu %>% 
  group_by(year) %>% 
  reframe(
    consequences = sum(consequences),
    appropriateness = sum(appropriateness),
    total = n(),
    frame_score = (consequences-appropriateness)/total) %>% 
  filter(total>10)

#-----------------------------------
# Figure 6.7
#------------
flexplot(frame_score ~ year, data=frames_eu_yearly) +
  labs(title = "Framing Score Over Time: LOESS Smoothing",
       subtitle = "Dashed lines: EU accession (2004); Migration Crisis (2015-2017); Ukraine Invasion (2022)",
       y = "<- Appropriateness : Consequences ->", x = "Year") +
  ylim(-0.4, 0.4) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16)) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  geom_vline(xintercept = 2015, linetype = "dashed", color = "purple",linewidth = 0.7) +
  geom_vline(xintercept = 2017, linetype = "dashed", color = "purple",linewidth = 0.7) +
  geom_vline(xintercept = 2022, linetype = "dashed", color = "green",linewidth = 0.7) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  theme_minimal() +
      theme(axis.text = element_text(size = 18),
         plot.title = element_text(size = 20),      # For the main title
        plot.subtitle = element_text(size = 14),
        title = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(1993, 2023, 3))



frames_eu<-frames_eu %>% 
  filter(party %in% atleast500)
# again big parties only

#yearly stance scores, min N=10
frames_eu_party <- frames_eu %>% 
  group_by(party,year) %>% 
  reframe(
    consequences = sum(consequences),
    appropriateness = sum(appropriateness),
    total = n(),
    frame_score = (consequences-appropriateness)/total) %>% 
  #slice(-1) %>% 
  filter(total>10)


plot_df1 <- frames_eu_party
plot_df2 <- frames_eu_party %>%
  rename(party_group_for_ghosts = party)

#-----------------------------------
# Figure 6.9
#------------
ggplot() +
  geom_line(data = plot_df2, aes(x = year, y = frame_score ,
                                 group = party_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + 
  geom_line(data = plot_df1, aes(x = year, y = frame_score ,
                                 color = party), 
            linetype = "solid", 
            linewidth = 1.2) + 
  ylim(-0.6, 0.6) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728",
                                "KDU-ČSL" = "#ffd700", "ANO" = "#add8e6", "SPD" = "#a0522d",
                                "TOP 09" = "#800080", "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  facet_wrap(~party) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), 
        legend.position = "none") +
  labs(y = "<- Appropriateness : Cosnequences ->", x = "Year") +
      theme(axis.text = element_text(size = 18),
         plot.title = element_text(size = 20),      
        plot.subtitle = element_text(size = 14),
        title = element_text(size = 20)) 



#-----------------------------------
# Figure 6.8
#------------
frames_eu_party %>% ggplot( aes(x = party, y = frame_score,color = party)) +
  geom_beeswarm(alpha = 1, size = 2) +  
  geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
  theme_minimal()+
  theme(legend.position = "none") +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  labs(subtitle = "Yearly EU Framing Score by Party",
       y = "<- Appropriateness : Consequences ->", x = "") +
  ylim(-0.6, 0.6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700","ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
      theme(axis.text = element_text(size = 14),
         plot.title = element_text(size = 35),      # For the main title
        #plot.subtitle = element_text(size = 14),
        title = element_text(size = 20)) 




#-----------------------------------------------------
# Stances + framing graphs
#-------------------
frames_eu <- fread("frame_data.csv")
stance_eu <- fread("stance_data.csv")
frames<-frames_eu %>% 
    #dplyr::select(11:13) %>% 
  rename("Frame" = "label" ) %>% 
  dplyr::select("Frame") 
eu <- cbind(stance_eu, frames)

eu <- eu%>% 
  rename("Stance" = "label" ) 
flexplot(frame~stance, eu)

df_counts <- eu %>%
  count(Frame)

#-----------------------------------
# Figure 6.6
#------------
ggplot(df_counts, aes(x = Frame, y = n,fill = Frame)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Prevalence of Frames about the EU",
    x = "Frame",
    y = "Number of Mentions",
   # fill = "Framing"
  ) +
  theme_minimal()+
  scale_y_continuous(breaks = seq(0, 24000, 3000)) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))



df_props <- eu %>%
  count(Stance, Frame) %>%
  group_by(Stance) %>%
  mutate(prop = n / sum(n))

df_props$Stance <- factor(df_props$Stance, levels = c("supports", "is neutral towards", "opposes"))


#-----------------------------------
# Figure 6.13
#------------
ggplot(df_props, aes(x = Stance, y = prop, fill = Frame)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Stances Towards the EU by Frames",
    x = "Stance Toward the EU",
    y = "Proportion of Frame",
    fill = "Frame"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()+ 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16),
        legend.text = element_text(size = 12))


