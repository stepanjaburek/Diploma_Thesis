library(flexplot)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(ggbeeswarm)


#----------------------------------------------------------
setwd("C:/Users/stepa/EU_Thesis_Paper")
data<-fread("eu_data.csv")

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

# Calculate salience and group by party and year
trad_party_sal <- data %>%
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
  filter(party %in% traditional) %>%  # keep only parties with at least 500 mentions
  dplyr::slice(-1)

# Calculate salience and group by party and year
new_party_sal <- data %>%
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
  filter(party %in% new) %>%  # keep only parties with at least 500 mentions
  dplyr::slice(-1)



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
# Time seriesvisuals

#flexplot
flexplot(eu_salience ~ year, method = "none", data=total_sal) +
    labs(subtitle = "Direct EU Salience: LOESS Smoothing",
         y = "EU Salience", x = "Year") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


ggplot(total_sal, aes(x = year, y = eu_salience)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Direct EU Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        title = element_text(size = 16))+
  theme_minimal(base_size = 14)+
   scale_x_continuous(breaks = seq(1993, 2023, 3))


#flexplot
flexplot(eu_institutions_salience ~ year, method = "loess", data=total_sal) +
  labs(subtitle = "EU Institutions: LOESS Smoothing",
       y = "EU Salience", x = "Year") + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))

ggplot(total_sal, aes(x = year, y = eu_institutions_salience)) +
  geom_line(color = "steelblue", linewidth = 1) +
  #geom_smooth(method = "loess")+
  labs(title = "EU Institutions Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        title = element_text(size = 16))+
  theme_minimal(base_size = 14)+
  scale_x_continuous(breaks = seq(1993, 2023, 3))#+
  geom_point(color = "darkblue", size = 1.3)
  
  ggplot(total_sal, aes(x = year, y = eu_institutions_salience)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 1.3) +
    geom_smooth(method = "loess", color = "darkorange", fill = "orange", se = F, linewidth = 1.2) +
    geom_vline(xintercept = 2004, linetype = "dashed", color = "red", linewidth = 0.8) +
    labs(
      title = "EU Institutions Salience Over Time",
      subtitle = "Dashed line = Czech EU accession (2004)",
      y = "% of speeches mentioned",
      x = "Year"
    ) +
    scale_x_continuous(breaks = seq(1993, 2023, 3)) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 14),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 12)
    )

#flexplot
flexplot(eu_total_salience ~ year, method = "loess", data=total_sal) +
  labs(subtitle = "Total EU Salience: LOESS Smoothing",
       y = "EU Salience", x = "Year") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))

ggplot(total_sal, aes(x = year, y = eu_total_salience)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Total EU Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        title = element_text(size = 16))+
  theme_minimal(base_size = 14)+
  scale_x_continuous(breaks = seq(1993, 2023, 3))

#----------------------------------------------------------
# Time series cross section visuals

# coplot
coplot(eu_salience ~ year|party, type="l", data=party_sal)  

#flexplot
flexplot(eu_salience ~ year+party, method = "loess", data=party_sal)
flexplot(eu_salience ~ year|party, method = "loess", data=party_sal)

ggplot(trad_party_sal, aes(x = year, y = eu_total_salience)) +
  geom_line() +
 # geom_smooth(method = "loess", se = FALSE)+
  facet_wrap(~ party) +
  labs(subtitle = "Yearly EU Salience by Traditional Parties",
       y = "EU Salience", x = "Year") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16)) +
  theme_minimal()

ggplot(party_sal, aes(x = year, y = eu_total_salience)) +
  geom_line() +
  #geom_smooth(method = "loess", se = FALSE)+
  facet_wrap(~ party) +
  labs(subtitle = "Yearly EU Salience by New Parties",
       y = "EU Salience", x = "Year") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16)) +
  theme_minimal()

ggplot(party_sal, aes(x = factor(year), y = eu_total_salience)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ party) +
  labs(
    title = "Yearly EU Salience by Party",
    subtitle = "Each facet shows annual salience for a single party",
    x = "Year",
    y = "EU Salience"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 16),
    strip.text = element_text(size = 14),
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 14)
  )


ggplot(party_sal, aes(x = year, y = eu_total_salience)) +
  geom_line() +
  #geom_smooth(method = "loess", se = FALSE)+
  facet_wrap(~ party) +
  labs(subtitle = "Yearly EU Salience by Party",
       y = "EU Salience", x = "Year")




plot_df1 <- party_sal

# Create a duplicate dataframe for the grey background lines
plot_df2 <- plot_df1 %>%
  rename(party_group_for_ghosts = party) # Rename 'party' to avoid aesthetic clashes
library(ggplot2)
library(dplyr)
library(ggpubr)

# Assuming plot_df1 and plot_df2 are already defined as in your previous code
# (e.g., plot_df1 <- party_sal, and plot_df2 is a renamed version of plot_df1)

ggplot() +
  # Background grey lines
  geom_line(data = plot_df2, aes(x = year, y = eu_total_salience * 100,
                                 group = party_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + # Adjusted linewidth for grey lines
  # Colored lines for individual parties
  geom_line(data = plot_df1, aes(x = year, y = eu_total_salience * 100,
                                 color = party), # linetype is already solid, no need to map
            linetype = "solid", # Explicitly set linetype
            linewidth = 1.2) + # Adjusted linewidth for colored lines
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728",
                                "KDU-ČSL" = "#ffd700", "ANO" = "#add8e6", "SPD" = "#a0522d",
                                "TOP 09" = "#800080", "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  facet_wrap(~party) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), # Added hjust for better alignment
        legend.position = "none") +
  labs(y = "Yearly Salience (%)", x = "Year")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))




ggplot() +
  geom_line(data = plot_df2, aes(x = year, y = eu_total_salience*100, 
                                   group= party_group_for_ghosts),
              #geom = "line",
              color = "grey",
              linetype = "solid")+
              #span = 0.5, se= F) +
  geom_line(data = plot_df1, aes(x = year, y = eu_total_salience*100,
                                   linetype = "solid",
                                   color = party,
                                  # fill = party,
                                 linewidth = 0.1))+ 
              #se = F,              span = 0.5) +
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700","ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  facet_wrap(~party)+
  theme_pubr()+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")+
  labs(y = "Monthly salience (%)", x = "Date")

# --- STEP 2: Create the ggplot object with geom_line ---



#----------------------------------------------------------
# plot yearly means - within and between case variation

trad_party_sal %>% ggplot( aes(x = party, y = eu_total_salience,color = party)) +
    geom_beeswarm(alpha = 1, size = 2) +  
    geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
      theme_minimal()+
      theme(legend.position = "none") +
      labs(x = "", y = "salience", title = "EU salience")+
      scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700")) +
      theme(axis.text = element_text(size = 12),
            axis.title = element_text(size = 16),
            title = element_text(size = 16))

new_party_sal %>% ggplot( aes(x = party, y = eu_total_salience,color = party)) +
  geom_beeswarm(alpha = 1, size = 2) +  
  geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "", y = "salience", title = "EU salience")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


party_sal %>% ggplot( aes(x = party, y = eu_total_salience,color = party)) +
  geom_beeswarm(alpha = 1, size = 2) +  
  geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(x = "", y = "EU salience", title = "Yearly EU Salience by Party - Overall")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700","ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))

#---------------------------------------------------------------------------
#----------------------------------------------
# stance

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

#flexplot
flexplot(stance_score ~ year, method = "loess", data=stance_eu_yearly) +
  labs(       title = "Total Stance Score Over Time: LOESS Smoothing",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "Oppose - Support", x = "Year") +
  ylim(-0.2, 0.2) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16)) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1993, 2023, 3))


ggplot(stance_eu_yearly, aes(x = year, y = stance_score)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Total EU Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  ylim(-0.2, 0.2) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.7) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        title = element_text(size = 16))+
  theme_minimal(base_size = 14)+
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

stance_eu_trad <-stance_eu_party %>% 
  filter(party %in% traditional)

stance_eu_new <-stance_eu_party %>% 
  filter(party %in% new)



stance_eu_trad <-stance_eu_party %>% 
  filter(party %in% traditional) %>% 
  filter(year>=2002)
stance_eu_new <-stance_eu_party %>% 
  filter(party %in% new)%>% 
  filter(year>=2002)

flexplot(stance_score ~ year + party, stance_eu_trad) +
  labs(subtitle = "EU Stance Score by Traditional Parties: LOESS Smoothing",
       y = "Oppose - Support", x = "Year")+
  ylim(-0.37, 0.37) +
  scale_x_continuous(breaks = seq(min(stance_eu_trad$year), max(stance_eu_trad$year), by = 3)) +
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#F4B400")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))



flexplot(stance_score ~ year + party, stance_eu_new) +
  labs(subtitle = "EU Stance Score by New Parties: LOESS Smoothing",
       y = "Oppose - Support", x = "Year") +
  ylim(-0.55, 0.55) +
  scale_x_continuous(breaks = seq(min(stance_eu_new$year), max(stance_eu_new$year), by = 3)) +
  scale_color_manual(values = c("ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) 

?flexplot

plot_df1 <- stance_eu_party

# Create a duplicate dataframe for the grey background lines
plot_df2 <- stance_eu_party %>%
  rename(party_group_for_ghosts = party)

ggplot() +
  # Background grey lines
  geom_line(data = plot_df2, aes(x = year, y = stance_score ,
                                 group = party_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + # Adjusted linewidth for grey lines
  # Colored lines for individual parties
  geom_line(data = plot_df1, aes(x = year, y = stance_score ,
                                 color = party), # linetype is already solid, no need to map
            linetype = "solid", # Explicitly set linetype
            linewidth = 1.2) + # Adjusted linewidth for colored lines
  ylim(-0.55, 0.55) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728",
                                "KDU-ČSL" = "#ffd700", "ANO" = "#add8e6", "SPD" = "#a0522d",
                                "TOP 09" = "#800080", "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  facet_wrap(~party) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), # Added hjust for better alignment
        legend.position = "none") +
  labs(y = "Oppose - Support", x = "Year") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))

stance_eu_party %>% ggplot( aes(x = party, y = stance_score,color = party)) +
  geom_beeswarm(alpha = 1, size = 2) +  
  geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
  theme_minimal()+
  theme(legend.position = "none") +
  labs(subtitle = "Yearly EU Stance Score by Party - Overall",
       y = "Oppose - Support", x = "") +
  ylim(-0.55, 0.55) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700","ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))


#---------------------------------------------------------------------------
#----------------------------------------------
# framing

frames_eu <- fread("debate_framing_eu (1).csv")
frames_eu <- fread("debate_discourse_eu.csv")

1+1
frames_eu <-frames_eu %>% 
  filter(score)

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

#flexplot
flexplot(frame_score ~ year, data=frames_eu_yearly) +
  labs(title = "Total Discourse Score Over Time: LOESS Smoothing",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "Appropriateness - Consequences", x = "Year") +
  ylim(-0.4, 0.4) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16)) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = seq(1993, 2023, 3))

#flexplot
flexplot(stance_score ~ year, method = "loess", data=stance_eu_yearly) +
  labs(       title = "Total Stance Score Over Time: LOESS Smoothing",
              subtitle = "Dashed line = Czech EU accession (2004)",
              y = "Oppose - Support", x = "Year") +
  ylim(-0.2, 0.2) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16)) +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  theme_minimal(base_size = 14) +
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

frames_eu_trad <-frames_eu_party %>% 
  filter(party %in% traditional)

frames_eu_new <-frames_eu_party %>% 
  filter(party %in% new)



plot_df1 <- frames_eu_party

# Create a duplicate dataframe for the grey background lines
plot_df2 <- frames_eu_party %>%
  rename(party_group_for_ghosts = party)

ggplot() +
  # Background grey lines
  geom_line(data = plot_df2, aes(x = year, y = frame_score ,
                                 group = party_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + # Adjusted linewidth for grey lines
  # Colored lines for individual parties
  geom_line(data = plot_df1, aes(x = year, y = frame_score ,
                                 color = party), # linetype is already solid, no need to map
            linetype = "solid", # Explicitly set linetype
            linewidth = 1.2) + # Adjusted linewidth for colored lines
  ylim(-0.6, 0.6) +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728",
                                "KDU-ČSL" = "#ffd700", "ANO" = "#add8e6", "SPD" = "#a0522d",
                                "TOP 09" = "#800080", "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  facet_wrap(~party) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1), # Added hjust for better alignment
        legend.position = "none") +
  labs(y = "Appropriateness - Cosnequences", x = "Year") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))




frames_eu_party %>% ggplot( aes(x = party, y = frame_score,color = party)) +
  geom_beeswarm(alpha = 1, size = 2) +  
  geom_boxplot(alpha = 0, width = 0.3, notch = F, size = 1.1) +
  theme_minimal()+
  theme(legend.position = "none") +
  geom_hline(yintercept = 0,, color = "black",linewidth = 0.4) +
  labs(subtitle = "Yearly EU Discourse Score by Party - Overall",
       y = "Appropriateness - Consequences", x = "") +
  ylim(-0.6, 0.6) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 6))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#ffd700","ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16))




frames<-frames_eu %>% 
    dplyr::select(11:13) %>% 
  rename("Frame" = "label" )
eu <- cbind(stance_eu, frames)

eu <- eu%>% 
  rename("Stance" = "label" ) #%>% 
  #filter(Stance!="is neutral towards") %>%  
  filter(Frame=="Consequences" |Frame=="Appropriateness"|Frame=="Both")
flexplot(frame~stance, eu)


df_counts <- eu %>%
  count(Frame)

# Plot
ggplot(df_counts, aes(x = Stance, y = n, fill = Frame)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Framing Types by Stance",
    x = "Stance Toward the EU",
    y = "Number of Mentions",
    fill = "Framing"
  ) +
  theme_minimal()

# Plot
ggplot(df_counts, aes(x = Frame, y = n,fill = Frame)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Total Prevalence of Discourses about the EU",
    x = "Discourse",
    y = "Number of Mentions",
    fill = "Framing"
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


# Plot
ggplot(df_props, aes(x = Stance, y = prop, fill = Frame)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Stances Towards the EU by Discourse",
    x = "Stance Toward the EU",
    y = "Proportion of Discourse",
    fill = "Discourse"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()+ 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16),
        legend.text = element_text(size = 12))



df_counts <- eu %>% count(Stance, Frame)
df_counts$Frame <- factor(df_counts$Frame, levels = c("Appropriateness", "Consequences", "Both", "Neither"))
df_counts$Stance <- factor(df_counts$Stance, levels = c("supports", "is neutral towards", "opposes"))


ggplot(df_counts, aes(x = Frame, y = Stance, fill = n)) +
  geom_tile(color = "white") +
  geom_text(aes(label = n), color = "black") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Counts of All Frame Types by Stance") +
  theme_minimal()+ 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 16),
        title = element_text(size = 16),
        legend.text = element_text(size = 12))

#----------------------------
# chappel hill


czechia_trad <-czechia %>% 
  filter(party %in% traditional)

czechia_new <-czechia %>% 
  filter(party %in% new)

flexplot(eu_position~year + party, czechia_trad,  plot.type ="line") +
  labs(subtitle = "LOESS Smoothing",
       y = "EU Position", x = "Year")+
  ylim(1,7)

library(ggplot2)

ggplot(czechia_trad, aes(x = year, y = eu_position, color = party)) +
  geom_line(size = 1) +               # Thicker lines
  geom_point(size = 2) +  
  labs(
    subtitle = "Chapel Hill Expert Survey",
    y = "EU Position (1 = anti-EU, 7 = pro-EU)",
    x = "Year"
  ) +
  scale_y_continuous(limits = c(1, 7)) +
  #scale_x_continuous(breaks = c(2002,2006,2010,2014,2019,2023)) +
  scale_x_continuous(breaks = seq(min(stance_eu_trad$year), max(2023), by = 3)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        title = element_text(size = 16))+
  scale_color_manual(values = c("ODS" = "#1f77b4", "ČSSD" = "#ff7f0e", "KSČM" = "#d62728", "KDU-ČSL" = "#F4B400"))


ggplot(czechia_new, aes(x = year, y = eu_position, color = party)) +
  geom_line(size = 1) +               # Thicker lines
  geom_point(size = 2) +  
  labs(
    subtitle = "Chapel Hill Expert Survey",
    y = "EU Position (1 = anti-EU, 7 = pro-EU)",
    x = "Year"
  ) +
  scale_y_continuous(limits = c(1, 7)) +
  scale_x_continuous(breaks = c(2010, 2014,2019)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        title = element_text(size = 16))+
  scale_color_manual(values = c("ANO" = "#add8e6", "SPD" = "#a0522d", "TOP 09" = "#800080",
                                "Piráti" = "#000000", "STAN" = "#FF69B4"))

flexplot(eu_position~year + party, czechia_new) +
  labs(subtitle = "",
       y = "EU Position", x = "Year")+
  ylim(1,7)














data$month <- as.factor(data$month) 

party_sal <- data %>%
  group_by(party, month) %>%
  reframe(
    eu_salience = sum(eu_mentions >= 1) / n(),
    year = first(year),
    year_centered = first(year_centered),
    chamber = first(chamber),
    gov_ideology = first(gov_ideology),
    election_year = first(election_year),
    election_period = first(election_period),
    inflation = first(inflation),
    wing = first(wing),
    eu_position = first(eu_position),
    eu_salience = first(eu_salience),
    lrgen = first(lrgen),
    galtan = first(galtan)
  ) %>%
  filter(party %in% atleast500) %>%  # keep only parties with at least 500 mentions
  slice(-1:-4)


party_sal <- data %>%
  group_by(party, month) %>%
  reframe(
    eu_salience = sum(eu_mentions >= 1) / n(),
    year = first(year),
    year_centered = first(year_centered),
    chamber = first(chamber),
    gov_ideology = first(gov_ideology),
    election_year = first(election_year),
    election_period = first(election_period),
    inflation = first(inflation),
    wing = first(wing)
  ) %>%
  filter(party %in% traditional) #%>%  # keep only parties with at least 500 mentions


flexplot(eu_salience~1,party_sal)  
sum(party_sal$eu_salience==0)
model_bin_self <- glmmTMB(eu_salience ~ year_centered +  inflation  + election_period + factor(party), 
                          family = ordbeta(link = "logit"),
                          data = party_sal
)

summary(model_bin_self)





library(glmmTMB)
# Fit the ordered beta regression model
model <- glmmTMB(
  eu_salience ~ year_centered + inflation + election_period + factor(party),
  family = ordbeta(link = "logit"),
  data = party_sal
)

# Summary of the model
summary(model)

library(ggeffects)
# Generate predictions for year_centered
predictions <- ggpredict(model, terms = "inflation")

# Plot the predictions
plot(predictions)


# Convert the logical response variable to a factor
party_sal$eu_salience_factor <- factor(party_sal$eu_salience > 0)
party_sal$eu_salience_num <- ifelse(party_sal$eu_salience > 0, 1,0)

# Fit the binary logistic regression model using the factor response variable
model_bin <- glmmTMB(
  eu_salience_factor ~ year_centered + inflation + election_period + factor(party),
  family = binomial(link = "logit"),
  data = party_sal
)

summary(model_bin)

# Generate predictions for year_centered
predictions <- ggpredict(model_bin, terms = "year_centered")

# Plot the predictions
plot(predictions)
flexplot(eu_salience~1, party_sal)
table(party_sal$election_period,party_sal$eu_salience_factor)
