## Replication for Venturelli et al. (2026)
## Main Models reported in the article start in line ####

library(tidyverse)
library(ggplot2)
library(zoo)
library(lubridate)
library(scales)
library(viridis)
library(stargazer)
library(car)
library(stats)
library(glmnet)
library(progressr)
library(tictoc)
library(gridExtra)
library(margins)
library(writexl)
library(readxl)
library(rio)
library(sandwich)
library(lmtest) 
library(ggeffects)
library(marginaleffects)
library(patchwork)


pop_tweets_22_23 <- import("venturelli et al 2026.rds")


pop_tweets_22_23 <- pop_tweets_22_23 %>%
  distinct(username, text, .keep_all = TRUE)

## You want that for the clean script later
# Group by year-month and compute populism proportion
pop_month_22_23 <- pop_tweets_22_23 %>%
  mutate(year = year(date),           # Extract year from date
         month = month(date)) %>%     # Extract month from date
  group_by(year, month) %>%
  summarise(populism_avg = mean(populism, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(year_month = as.yearmon(paste(year, month, sep = "-")))

pop_tweets_22_23$date <- as.Date(pop_tweets_22_23$date)
## week prop of pop all
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(week = floor_date(date, unit = "week")) %>%  # Create a new column for the week
  group_by(week) %>%                                   # Group by the new week column
  mutate(pop_prop_all_week = mean(populism, na.rm = TRUE),  # Example aggregation (mean)
         .groups = 'drop')  # Drop the grouping after summarising


## week prop of pop by user
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(week = floor_date(date, unit = "week")) %>%  # Create a new column for the week
  group_by(week, username) %>%                                   # Group by the new week column
  mutate(pop_prop_week = mean(populism, na.rm = TRUE),  # Example aggregation (mean)
         .groups = 'drop')  # Drop the grouping after summarising



## graphic 2 electoral year: level of populism by candidate
figure2 <- ggplot(pop_tweets_22_23, aes(x = week, y = pop_prop_all_week, group = 1)) +
  geom_smooth(method = "loess", span = 0.5, se = FALSE, linewidth = .5, color = "black", linetype = "dashed") +  # Using loess for moving average
  geom_point(color = "black", shape = 17, size = 1) +  # Adding points for clarity
  labs(
    # title = "Proportion of Populism per Week (2022)",
    x = "Month",
    y = "Proportion of Populist Tweets"
  ) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 0.1)) +  # Format y-axis as percentage with 20% limit
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels to 45 degrees
        plot.subtitle = element_text(face = "italic")) +     # Make subtitle italic
  # Vertical lines
  geom_vline(xintercept = as.numeric(as.Date("2022-04-02")), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2022-08-16")), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-02")), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2022-10-30")), linetype = "dotted", color = "black") +
  
  # Adding labels with annotate
  annotate("text", x = as.Date("2022-03-25"), y = 0.0825, label = "Apr 2 2022:\nPre-campaign begins",
           vjust = -0.5, hjust = 1, size = 2.5, color = "black") +
  geom_segment(aes(x = as.Date("2022-03-26") - 0.1, xend = as.Date("2022-04-01"),
                   y = 0.09, yend = 0.09),
               arrow = arrow(length = unit(0.05, "inches")), color = "black") +
  
  annotate("text", x = as.Date("2022-08-08"), y = 0.0825, label = "Aug 16 2022:\nOfficial campaign begins",
           vjust = -0.5, hjust = 1, size = 2.5, color = "black") +
  geom_segment(aes(x = as.Date("2022-08-09") - 0.1, xend = as.Date("2022-08-15"),
                   y = 0.09, yend = 0.09),
               arrow = arrow(length = unit(0.05, "inches")), color = "black") +
  
  annotate("text", x = as.Date("2022-09-23"), y = 0.0825, label = "Oct 02 2022:\nFirst round",
           vjust = -0.5, hjust = 1, size = 2.5, color = "black") +
  geom_segment(aes(x = as.Date("2022-09-24") - 0.1, xend = as.Date("2022-10-01"),
                   y = 0.09, yend = 0.09),
               arrow = arrow(length = unit(0.05, "inches")), color = "black") +
  
  annotate("text", x = as.Date("2022-10-23"), y = 0.077, label = "Oct 30 \n2022:\nRunoff",
           vjust = -0.5, hjust = 1, size = 2.5, color = "black") +
  geom_segment(aes(x = as.Date("2022-10-24") - 0.1, xend = as.Date("2022-10-29"),
                   y = 0.09, yend = 0.09),
               arrow = arrow(length = unit(0.05, "inches")), color = "black")


print(figure2)
## this is not working
ggsave("figure2.png", height = 10, width = 20, units = "cm", dpi = 700)



# Create the faceted plot
figure3 <- ggplot(pop_tweets_22_23, aes(x = date, y = populism)) +
  # geom_point(alpha = 0.6, color = "#2c3e50", size = 0.8) +
  geom_smooth(method = "loess", se = TRUE, color = "black", 
              fill = "gray", alpha = 0.3, size = 0.8,
              linetype = "dashed") +
  facet_wrap(~ username, ncol = 2) +
  
  # Styling consistent with academic publication standards
  theme_minimal() +
  theme(
    strip.text = element_text(size = 10, face = "bold", color = "black"),
    strip.background = element_rect(fill = "#ecf0f1", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5, color = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "lightgray", size = 0.3),
    panel.border = element_rect(color = "lightgray", fill = NA, size = 0.5)
  ) +
  
  # Labels and formatting
  labs(
    # title = "Temporal Patterns of Populist Discourse Deployment",
    #  subtitle = "Brazilian Presidential Candidates Twitter Communications (2022)",
    x = "Date",
    y = "Percentage of Populist Tweets"
  ) +
  
  # Scale formatting for academic presentation
  scale_x_date(
    date_labels = "%b %Y",
    date_breaks = "2 months"
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, NA)
  )+
  geom_vline(xintercept = as.numeric(pop_tweets_22_23$date[which(pop_tweets_22_23$date == as.Date("2022-04-02"))]), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(pop_tweets_22_23$date[which(pop_tweets_22_23$date == as.Date("2022-08-16"))]), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(pop_tweets_22_23$date[which(pop_tweets_22_23$date == as.Date("2022-10-02"))]), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(pop_tweets_22_23$date[which(pop_tweets_22_23$date == as.Date("2022-10-30"))]), linetype = "dotted", color = "black")+
  # Adding labels with annotate
  annotate("text", x = as.Date("2022-03-25"), y = 0.25, label = "Apr 2 2022:\nPre-campaign begins",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-03-26") - 0.1, xend = as.Date("2022-04-01"),
                   y = 0.3, yend = 0.3),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4") +
  
  annotate("text", x = as.Date("2022-08-08"), y = 0.25, label = "Aug 16 2022:\nOfficial campaign begins",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-08-09") - 0.1, xend = as.Date("2022-08-15"),
                   y = 0.3, yend = 0.3),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4") +
  
  annotate("text", x = as.Date("2022-09-23"), y = 0.25, label = "Oct 02 2022:\nFirst round",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-09-24") - 0.1, xend = as.Date("2022-10-01"),
                   y = 0.3, yend = 0.3),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4") +
  
  annotate("text", x = as.Date("2022-10-23"), y = 0.225, label = "Oct 30 \n2022:\nRunoff",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-10-24") - 0.1, xend = as.Date("2022-10-29"),
                   y = 0.3, yend = 0.3),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4")

# Display the plot
print(figure3)

ggsave("figure3.png", height = 22, width = 30, units = "cm", dpi = 700)



## Lula X Bolsonaro
frontrunners <- pop_tweets_22_23 %>%
  filter(username == "LulaOficial" |username== "jairbolsonaro")

max(frontrunners$pop_prop_week)

## Figure 4
## populist over time frontrunners
figure4 <- ggplot(frontrunners, aes(x = week, y = pop_prop_week, linetype = username)) +
  geom_smooth(method = "loess", span = 0.5, se = T, linewidth = .5, color = "black") +  # Using loess for moving average
  #geom_point(shape = 17, size = 1) +  # Adding points for clarity
  labs(
    # title = "Weekly Proportion of Populism (Lula da Silva and Jair Bolsonaro)",
    x = "Month",
    y = "Proportion of Populist Tweets",
    linetype = "Candidate"
  ) +
  #scale_y_continuous(labels = scales::percent_format(), 
  #                     limits = c(0, max(lulabozo$pop_prop_week, na.rm = TRUE) + 0.05))+
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, .10)) +  # Format y-axis as percentage with 0-1 limit
  scale_color_viridis_d() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels to 45 degrees
        plot.subtitle = element_text(face = "italic")) +     # Make subtitle italic
  geom_vline(xintercept = as.numeric(frontrunners$date[which(frontrunners$date == as.Date("2022-04-02"))]), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(frontrunners$date[which(frontrunners$date == as.Date("2022-08-16"))]), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(frontrunners$date[which(frontrunners$date == as.Date("2022-10-02"))]), linetype = "dotted", color = "black") +
  geom_vline(xintercept = as.numeric(frontrunners$date[which(frontrunners$date == as.Date("2022-10-30"))]), linetype = "dotted", color = "black")+
  # Adding labels with annotate
  annotate("text", x = as.Date("2022-03-25"), y = 0.075, label = "Apr 2 2022:\nPre-campaign begins",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-03-26") - 0.1, xend = as.Date("2022-04-01"),
                   y = 0.08, yend = 0.08),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4") +
  
  annotate("text", x = as.Date("2022-08-08"), y = 0.075, label = "Aug 16 2022:\nOfficial campaign begins",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-08-09") - 0.1, xend = as.Date("2022-08-15"),
                   y = 0.08, yend = 0.08),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4") +
  
  annotate("text", x = as.Date("2022-09-23"), y = 0.075, label = "Oct 02 2022:\nFirst round",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-09-24") - 0.1, xend = as.Date("2022-10-01"),
                   y = 0.08, yend = 0.08),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4") +
  
  annotate("text", x = as.Date("2022-10-23"), y = 0.07, label = "Oct 30 \n2022:\nRunoff",
           vjust = -0.5, hjust = 1, size = 2, color = "black") +
  geom_segment(aes(x = as.Date("2022-10-24") - 0.1, xend = as.Date("2022-10-29"),
                   y = 0.08, yend = 0.08),
               arrow = arrow(length = unit(0.1, "inches")), color = "slategray4")

print(figure4)
ggsave("figure4.png", height = 10, width = 22, units = "cm", dpi = 700)



## Average of populist tweets by candidate
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  group_by(username) %>%
  mutate(pop_avg = mean(populism)) %>%
  ungroup()



# Compute mean, se, and CI per candidate
candidate_stats <- pop_tweets_22_23 %>%
  group_by(username, ideology) %>%
  summarise(
    pop_avg = mean(populism, na.rm = TRUE),
    se = sd(populism, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) %>%
  mutate(
    ci_lower = pop_avg - 1.96 * se,
    ci_upper = pop_avg + 1.96 * se
  )

# Plot with confidence intervals
figure5 <- ggplot(candidate_stats, aes(x = ideology, y = pop_avg, shape = username)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.02, linewidth = 0.4, color = "grey40") +  # CI bars
  geom_point(size = 3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = F, color = "darkgrey", linetype = "dashed",
              inherit.aes = FALSE, aes(x = ideology, y = pop_avg)) +  # Quadratic fit across all candidates
  labs(
    x = "Ideology",
    y = "Proportion of Populist Tweets",
    shape = "Candidate"
  ) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)) +  # Up to 18 distinct shapes
  theme_bw() +
  xlim(0, 1) +
  ylim(0, 0.2) +
  geom_text(aes(label = username), vjust = -0.5, hjust = .5, size = 3.5, color = "black")



# Show the plot
print(figure5)

ggsave("figure5.png", height = 14, width = 21, units = "cm", dpi = 700)


## Ideology cluster
# Check the distribution
summary(pop_tweets_22_23$ideology)
quantile(pop_tweets_22_23$ideology, probs = c(0.33, 0.67), na.rm = TRUE)

# Visualize
ggplot(pop_tweets_22_23, aes(x = ideology)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = c(0.3, 0.7), color = "red", linetype = "dashed")

## Ideological clusters for interactions (collapsing nine candidates into 3 gorups only): follows distribution looking for similar size groups
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(ideology_cluster = case_when(
    ideology >= 0 & ideology <= 0.3 ~ "Left",
    ideology > 0.3 & ideology <= 0.7 ~ "Center",
    ideology > 0.7 & ideology <= 1 ~ "Right",
    TRUE ~ NA_character_
  ))


#factor variable: change baseline
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(ideology_cluster = relevel(factor(ideology_cluster), ref = "Center"))


## basic ideological cluster following Bolognesi et al (2023) threshold for the ideological center
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(ideology_cluster2 = case_when(
    ideology < 0.45 ~ "Left",
    ideology > 0.55 ~ "Right",
    TRUE ~ "Center"  # or NA_character_ to exclude
  ))
pop_tweets_22_23$ideology_cluster2 <- as.factor(pop_tweets_22_23$ideology_cluster2)

# fine-grained ideological cluster following the literature (check paper)
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(
    ideology_cluster3 = case_when(
      username %in% c("LulaOficial", "Ciro Gomes", "Leonardo Pericles", 
                      "Sofia Manzano", "Vera Lucia") ~ "Left",
      username %in% c("Simone Tebet") ~ "Center",
      username %in% c("jairbolsonaro", "Felipe Davila", 
                      "Soraya Thronicke") ~ "Right",
      TRUE ~ NA_character_
    ),
    # Dummy variables for interactions
    left  = as.integer(ideology_cluster3 == "Left"),
    center = as.integer(ideology_cluster3 == "Center")
  )

pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(ideology_cluster3 = factor(ideology_cluster3, 
                                    levels = c("Right", "Center", "Left")))

summary(pop_tweets_22_23$ideology_cluster3)
table(pop_tweets_22_23$username, pop_tweets_22_23$ideology_cluster3)


# Descriptive stats for appendix
descriptive_stats <- pop_tweets_22_23 %>% 
  group_by(username) %>% 
  summarise(
    n_tweets = n(),                                    # Total observations per candidate
    mean_populism = mean(populism, na.rm = TRUE),      # Average populist discourse intensity
    sd_populism = sd(populism, na.rm = TRUE),          # Within-candidate discourse variability
    ideology = mean(ideology, na.rm = TRUE),           # Ideological positioning measure
    .groups = 'drop'
  ) %>%
  select(username, ideology, n_tweets, mean_populism, sd_populism) %>%
  arrange(desc(mean_populism)) %>% 
  # Systematic marginal calculation framework with ideological aggregation
  add_row(
    username = "Total/Overall",
    ideology = weighted.mean(.$ideology, .$n_tweets, na.rm = TRUE),
    n_tweets = sum(.$n_tweets),
    mean_populism = weighted.mean(.$mean_populism, .$n_tweets, na.rm = TRUE),
    sd_populism = sqrt(sum((.$n_tweets - 1) * .$sd_populism^2, na.rm = TRUE) / 
                         (sum(.$n_tweets, na.rm = TRUE) - length(.$sd_populism)))
  ) %>% 
  mutate(
    ideology = round(ideology, 2),                     # Standardized ideological precision
    mean_populism = round(mean_populism, 2),           # Two-decimal precision for proportional measures
    sd_populism = round(sd_populism, 2)                # Consistent variability measurement precision
  )


descriptive_stats

# candidate descriptive statistics table
stargazer(as.data.frame(descriptive_stats),
          type = "latex", 
          summary = FALSE,
          title = "Populist Discourse and Ideological Positioning: Brazilian Presidential Candidates (2022-2023)",
          label = "tab:candidate_populism_ideology",
          digits = 2,
          digits.extra = 0,
          column.labels = c("Candidate", "Ideological\\\\Position", "Tweet\\\\Volume", 
                            "Mean Populist\\\\Proportion", 
                            "Populist Discourse\\\\Variability"),
          covariate.labels = NULL,
          align = TRUE,
          no.space = TRUE,
          notes = c("\\textit{Methodological Framework:} Populist discourse classification based on dual-dimension model",
                    "incorporating people-centric and anti-elite appeals through fine-tuned BERTimbau algorithms.",
                    "Ideological position measured on continuous scale; populist proportion represents frequency",
                    "of classified populist appeals; standard deviation indicates within-candidate strategic consistency.",
                    "Total row presents simple mean ideology and weighted populist discourse aggregations."),
          notes.align = "l",
          notes.append = FALSE,
          header = FALSE,
          rownames = FALSE,
          table.placement = "htbp",
          font.size = "small")



## percentage of people centric, antiestablishment and populism by candidate (appendix)
# Calculate proportions by candidate
candidate_props <- pop_tweets_22_23 %>%
  group_by(username) %>%
  summarise(
    people_centric = mean(pc, na.rm = TRUE) * 100,
    anti_establishment = mean(ae, na.rm = TRUE) * 100,
    populism = mean(populism, na.rm = TRUE) * 100,
    n_tweets = n()
  ) %>%
  ungroup()

# Order candidates by populism level
candidate_order <- candidate_props %>%
  arrange(desc(populism)) %>%
  pull(username)

candidate_props_long <- candidate_props %>%
  mutate(username = factor(username, levels = candidate_order)) %>%
  pivot_longer(
    cols = c(people_centric, anti_establishment, populism),
    names_to = "appeal_type",
    values_to = "percentage"
  ) %>%
  mutate(appeal_type = case_match(appeal_type,
                                  "people_centric" ~ "People-Centric",
                                  "anti_establishment" ~ "Anti-Establishment",
                                  "populism" ~ "Populism"
  ))


app_pcaepop <- ggplot(candidate_props_long, aes(x = username, y = percentage, fill = appeal_type)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_y_continuous(labels = percent_format(scale = 1),
                     limits = c(0, 50),
                     breaks = seq(0, 100, 20)) +
scale_fill_manual(values = c("People-Centric" = "#2E86AB",      # Blue
                             "Anti-Establishment" = "#2A9D8F",  # Teal/green
                             "Populism" = "#F4A259"))  +        # Coral
  labs(
   # title = "Populist Appeals by Candidate",
  #  subtitle = "Candidates ordered by populism prevalence (highest to lowest)",
    x = "Candidate",
    y = "Percentage of Tweets (%)",
    fill = " "
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

print(app_pcaepop)
ggsave("app_pcaepop.png", height = 14, width = 21, units = "cm", dpi = 700)


############################################# POLICY AGENDA ######################################
############################################# POLICY AGENDA ######################################
############################################# POLICY AGENDA ######################################
############################################# POLICY AGENDA ######################################

## predicted label final: in the original version, we used .6 as the threshold.
## we adjusted to have more substantial observations, and a smaller N of "none" (no policy topic)
pop_tweets_22_23 <- pop_tweets_22_23 %>% 
  mutate(predicted_label_final = ifelse(predicted_prob >= .5, predicted_label, 99))


pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(macroeconomics = ifelse(predicted_label_final == 0, 1,0),
         civil_rights = ifelse(predicted_label_final == 1, 1,0),
         health = ifelse(predicted_label_final == 2, 1,0),
         agriculture = ifelse(predicted_label_final == 3, 1,0),
         labor_employment = ifelse(predicted_label_final == 4, 1,0),
         education = ifelse(predicted_label_final == 5, 1,0),
         environment = ifelse(predicted_label_final == 6, 1,0),
         energy = ifelse(predicted_label_final == 7, 1,0),
         immigration = ifelse(predicted_label_final == 8, 1,0),
         transportation = ifelse(predicted_label_final == 9, 1,0),
         law_crime = ifelse(predicted_label_final == 10, 1,0),
         social_welfare = ifelse(predicted_label_final == 11, 1,0),
         housing_issues = ifelse(predicted_label_final == 12, 1,0),
         banking_finance= ifelse(predicted_label_final == 13, 1,0),
         defense = ifelse(predicted_label_final == 14, 1,0),
         science_tech = ifelse(predicted_label_final == 15, 1,0),
         foreign_trade = ifelse(predicted_label_final == 16, 1,0),
         international_affairs = ifelse(predicted_label_final == 17, 1,0),
         government_operations = ifelse(predicted_label_final == 18, 1,0),
         lands_water = ifelse(predicted_label_final == 19, 1,0),
         culture = ifelse(predicted_label_final == 20, 1,0),
         none = ifelse(predicted_label_final == 99,1,0)
  )


# factor variable
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(policy = case_when(
    predicted_label_final == 0 ~ "macroeconomics",
    predicted_label_final == 1 ~ "civil_rights",
    predicted_label_final == 2 ~ "health",
    predicted_label_final == 3 ~ "agriculture",
    predicted_label_final == 4 ~ "labor_employment",
    predicted_label_final == 5 ~ "education",
    predicted_label_final == 6 ~ "environment",
    predicted_label_final == 7 ~ "energy",
    predicted_label_final == 8 ~ "immigration",
    predicted_label_final == 9 ~ "transportation",
    predicted_label_final == 10 ~ "law_crime",
    predicted_label_final == 11 ~ "social_welfare",
    predicted_label_final == 12 ~ "housing_issues",
    predicted_label_final == 13 ~ "banking_finance",
    predicted_label_final == 14 ~ "defense",
    predicted_label_final == 15 ~ "science_tech",
    predicted_label_final == 16 ~ "foreign_trade", 
    predicted_label_final == 17 ~ "international_affairs",
    predicted_label_final == 18 ~ "government_operations",
    predicted_label_final == 19 ~ "lands_water",
    predicted_label_final == 20 ~ "culture",
    predicted_label_final == 99 ~ "none"
  ))


pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(policy = relevel(factor(policy), ref = "government_operations"))



## policy reduced
#rename
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(policy_reduced = factor(case_when(
    policy %in% c("government_operations",
                  "civil_rights",
                  "labor_employment",
                  "law_crime",
                  "social_welfare") ~ policy,
    TRUE ~ "other"
  ), levels = c("government_operations", "other", "civil_rights",
                "labor_employment", "law_crime", "social_welfare")))


levels(pop_tweets_22_23$policy_reduced)


## creating a new variable to assess temporal variation. 
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(period = factor(case_when(
    (date >= as.Date("2022-01-01") & date <= as.Date("2022-04-01")) |
      (date >= as.Date("2022-10-31") & date <= as.Date("2023-02-07")) ~ "Regular Politics",
    date >= as.Date("2022-04-02") & date <= as.Date("2022-08-15") ~ "Pre-Campaign",
    date >= as.Date("2022-08-16") & date <= as.Date("2022-10-30") ~ "Campaign",
    TRUE ~ NA_character_
  ), levels = c("Regular Politics", "Pre-Campaign", "Campaign")))


## campaign period variable (dichotomous)
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(campaign = factor(case_when(
    date >= as.Date("2022-08-16") & date <= as.Date("2022-10-30") ~ "Campaign",
    TRUE ~ "Regular Politics"
  ), levels = c("Regular Politics", "Campaign")))


## Sample for manual validation of the policy model
# Check distribution
policy_dist <- pop_tweets_22_23 %>%
  count(policy) %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(n))

print(policy_dist, n = 21)


#sample_size <- 2000  # adjust as needed

#set.seed(123)  # for reproducibility
#pop_sample <- pop_tweets_22_23 %>%
#  group_by(policy) %>%
#  slice_sample(prop = sample_size / nrow(pop_tweets_22_23)) %>%
#  ungroup()

# Verify the sample preserves proportions
#pop_sample %>%
#  count(policy) %>%
#  mutate(pct = n / sum(n) * 100) %>%
#  arrange(desc(n))

#write_xlsx(pop_sample, "sample_CAP_validation.xlsx")


## Plot distribution of tweets by policy topic
prop_pop_policy <- pop_tweets_22_23 %>%
  group_by(policy) %>%
  summarise(
    n_tweets = n(),                                    # Observational density per policy domain
    populism_mean = mean(populism, na.rm = TRUE),      # Average populist discourse intensity
    .groups = 'drop'
  ) %>%
  mutate(
    policy_prop = n_tweets / sum(n_tweets)             # Proportional policy domain representation
  ) %>%
  arrange(desc(n_tweets)) 

#rename
prop_pop_policy <- prop_pop_policy %>% 
  mutate(
    policy = case_when(
      policy == "government_operations" ~ "Government Operations",
      policy == "none" ~ "None",
      policy == "education" ~ "Education",
      policy == "civil_rights" ~ "Civil Rights",
      policy == "energy" ~ "Energy",
      policy == "social_welfare" ~ "Social Welfare",
      policy == "health" ~ "Health",
      policy == "labor_employment" ~ "Labor and Employment",
      policy == "law_crime" ~ "Law and Crime",
      policy == "macroeconomics" ~ "Macroeconomics",
      policy == "international_affairs" ~ "International Affairs",
      policy == "culture" ~ "Culture",
      policy == "agriculture" ~ "Agriculture",
      policy == "science_tech" ~ "Science and Technology",
      policy == "housing_issues" ~ "Housing",
      policy == "defense" ~ "Defense",
      policy == "transportation" ~ "Transportation",
      policy == "environment" ~ "Environment",
      policy == "banking_finance" ~ "Banking and Finance",
      policy == "foreign_trade" ~ "Foreign Trade",
      policy == "lands_water" ~ "Land and Water Management",
      TRUE ~ policy  # Preserves any unmatched values
    )
  )


# Systematic policy domain visualization with horizontal orientation
figure1 <- ggplot(prop_pop_policy, aes(x = policy_prop, y = reorder(policy, policy_prop))) +
  geom_col(fill = "grey", alpha = 0.7, width = 0.8) +
  labs(
    # title = "Policy Domain Representation in Presidential Campaign Discourse",
    #  subtitle = "Proportional Distribution Across Substantive Political Areas (2022-2023)",
    x = "Proportion",
    y = "Policy Domain"
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12, face = "bold"),
    plot.subtitle = element_text(size = 10, face = "italic"),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

print(figure1)
ggsave("figure1.png", height = 10, width = 22, units = "cm", dpi = 700)


# Calculate the mean of 'populism' for each 'predicted_label_final' group and arrange in descending order
prop_populism_policy <- pop_tweets_22_23 %>%
  group_by(policy) %>%
  summarise(populism_mean = mean(populism, na.rm = TRUE)) %>%
  arrange(desc(populism_mean))



# Reset the factor levels of 'policy' so that none is the baseline
pop_tweets_22_23$policy <- relevel(pop_tweets_22_23$policy, ref = "government_operations")
pop_tweets_22_23$period <- relevel(pop_tweets_22_23$period, ref = "Regular Politics")
pop_tweets_22_23$ideology_cluster <- relevel(pop_tweets_22_23$ideology_cluster, ref = "Center")
pop_tweets_22_23$username <- as.factor(pop_tweets_22_23$username)
pop_tweets_22_23$username <- relevel(pop_tweets_22_23$username, ref = "jairbolsonaro")



## Prep data for models
## check variables which prevalence is lower than 1%
drop_these <- prop_pop_policy %>% 
  filter(policy_prop < 0.01)
print(drop_these)
levels(pop_tweets_22_23$policy)

pop_tweets_22_23$policy <- as.factor(pop_tweets_22_23$policy)
levels(pop_tweets_22_23$policy)
unique(pop_tweets_22_23$policy)
# Exclude observations where the policy is one of the unwanted categories
data_model_all <- pop_tweets_22_23 %>% 
  filter(!policy %in% c("foreign_trade", "lands_water"))

data_model_all$policy <- droplevels(data_model_all$policy)


levels(data_model_all$ideology_cluster3)


## Prep data for interactions (R was being stubborn in using Left as baseline, so I did manually)
data_model_all <- data_model_all %>%
  mutate(
    cr = as.integer(policy == "civil_rights"),
    le = as.integer(policy == "labor_employment"),
    lc = as.integer(policy == "law_crime"),
    sw = as.integer(policy == "social_welfare"),
    left = as.integer(ideology_cluster3 == "Left"),
    right = as.integer(ideology_cluster3 == "Right")
  )


## UNWEIGHTED
model_1_unweighted <- glm(
  populism ~ 
    period + policy + ideology_cluster3 + word_count +
    left:cr + right:cr +
    left:le + right:le +
    left:lc + right:lc +
    left:sw + right:sw,
  data = data_model_all,
  family = binomial(link = "logit")
)



# Get clustered standard errors by username
summary_m1_clustered <- coeftest(
  model_1_unweighted, 
  vcov = vcovCL(model_1_unweighted, 
                cluster = ~ username, 
                type = "HC1")
)

# Display results with clustered SEs
print(summary_m1_clustered)

# CORRECT: This object contains coefficients, clustered SEs, z-values, and p-values
# The p-values ARE based on the clustered SEs
summary_m1_unweighted <- summary_m1_clustered

# To view it nicely:
print(summary_m1_unweighted)

# Extract clustered SEs from the coeftest object
clustered_se <- summary_m1_unweighted[, "Std. Error"]

# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_unweighted,
          se = list(clustered_se),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")



# WEIGHTED MODEL
# Weighted main model -- all candidates
# STEP 1: Class Balancing Weights
# Calculate class proportions
y_bar <- mean(data_model_all$populism)

# Create balanced weights to give equal importance to both classes
data_model_all <- data_model_all %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,           # Upweight minority class
      (1 / (1 - y_bar)) / 2      # Downweight majority class
    )
  )


# Verify: both classes now have equal effective sample size
data_model_all %>%
  group_by(populism) %>%
  dplyr::summarize(
    n = n(),
    sum_weights = sum(balanced_weight),
    effective_n = sum_weights
  )

colnames(data_model_all)
data_model_all <- data_model_all %>%
  select(-foreign_trade, -lands_water)

# STEP 2: Run the model
## Run weighted model
model_1_weighted <- glm(
  populism ~ 
    period + policy + ideology_cluster3 + word_count +
    left:cr + right:cr +
    left:le + right:le +
    left:lc + right:lc +
    left:sw + right:sw,
  data = data_model_all,
  family = binomial(link = "logit"),
  weights = balanced_weight
)


# STEP 3: Get Clustered Sandwich Standard Errors (REQUIRED for weighted models)
summary_m1_weighted_clustered <- coeftest(
  model_1_weighted, 
  vcov = vcovCL(model_1_weighted, 
                cluster = ~ username, 
                type = "HC1")
)

# Display results with clustered SEs
print(summary_m1_weighted_clustered)

# Save for later use
summary_m1_weighted <- summary_m1_weighted_clustered


# STEP 4: Extract Clustered SEs for Stargazer

# Extract clustered SEs from the coeftest object
clustered_se_weighted <- summary_m1_weighted[, "Std. Error"]

# STEP 5: Display with Stargazer

# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_weighted,
          se = list(clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Weighted Logistic Regression",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors (in parentheses) clustered by candidate. Weighted by inverse class frequency.")


## export unweighted and weighted models together


levels(pop_tweets_22_23$username)
# TABLE PART 1: First half of coefficients

# Get coefficient names
all_coefs <- names(coef(model_1_unweighted))

# Split roughly in half (adjust the number 15 based on your total coefficients)
# You have ~27 coefficients, so split at around 14
first_half <- all_coefs[1:14]

stargazer(model_1_unweighted, model_1_weighted,
          se = list(clustered_se, clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",  # or "text" for preview
          column.labels = c("Unweighted", "Weighted"),
          model.names = FALSE,
          dep.var.labels = "Populist Appeal",
          title = "Logistic Regression Models of Populist Appeals",
          keep = first_half,  # Keep only first half
          omit.stat = c("aic", "ll"),  # Omit to save space
          notes = c("Robust standard errors clustered by candidate in parentheses.",
                    "Weighted model uses inverse class frequency weighting."),
          notes.append = FALSE,
          font.size = "small",
          out = "table_models_part1.tex")

# TABLE PART 2: Second half of coefficients (continued)
# Get coefficient names
second_half <- all_coefs[15:length(all_coefs)]

stargazer(model_1_unweighted, model_1_weighted,
          se = list(clustered_se, clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          column.labels = c("Unweighted", "Weighted"),
          model.names = FALSE,
          dep.var.labels = "",  # Remove to avoid repetition
          title = "Logistic Regression Models of Populist Appeals (Continued)",
          keep = second_half,  # Keep only second half
          add.lines = list(
            c("Observations", "11,746", "11,746"),
            c("Log Likelihood", "-1,873.7", "-9,905.9"),
            c("AIC", "3,801.4", "19,865.8")
          ),
          notes = c("Robust standard errors clustered by candidate in parentheses.",
                    "Weighted model uses inverse class frequency weighting.",
                    "Continued from previous table."),
          notes.append = FALSE,
          font.size = "small",
          out = "table_models_part2.tex")



## TO solve separation issues
library(logistf)
model_firth <- logistf(
    populism ~ 
    period + policy + ideology_cluster3 + word_count +
    left:cr + right:cr +
    left:le + right:le +
    left:lc + right:lc +
    left:sw + right:sw,
  data = data_model_all,
  weights = data_model_all$balanced_weight
)


# Extract Firth results
firth_results <- data.frame(
  term = names(coef(model_firth)),
  estimate = coef(model_firth),
  se = sqrt(diag(vcov(model_firth))),
  lower = model_firth$ci.lower,
  upper = model_firth$ci.upper,
  p = model_firth$prob
)
print(firth_results)

## Export table of robustness model
library(kableExtra)
firth_results <- firth_results %>%
  mutate(
    # Clean term names
    term = gsub("policy", "", term),
    term = gsub("ideology_cluster3", "", term),
    term = gsub("campaign", "", term),
    term = gsub("_", " ", term),
    term = case_match(term,
                      "(Intercept)" ~ "Intercept",
                      "Campaign"    ~ "Campaign",
                      "agriculture" ~ "Agriculture",
                      "banking finance" ~ "Banking/Finance",
                      "civil rights" ~ "Civil Rights",
                      "culture" ~ "Culture",
                      "defense" ~ "Defense",
                      "education" ~ "Education",
                      "energy" ~ "Energy",
                      "environment" ~ "Environment",
                      "health" ~ "Health",
                      "housing issues" ~ "Housing Issues",
                      "international affairs" ~ "International Affairs",
                      "labor employment" ~ "Labor/Employment",
                      "law crime" ~ "Law/Crime",
                      "macroeconomics" ~ "Macroeconomics",
                      "none" ~ "None",
                      "science tech" ~ "Science/Tech",
                      "social welfare" ~ "Social Welfare",
                      "transportation" ~ "Transportation",
                      "Right" ~ "Right",
                      "Left" ~ "Left",
                      "word count" ~ "Word Count",
                      "left:sw" ~ "Left $\\times$ Social Welfare",
                      "left:le" ~ "Left $\\times$ Labor/Employment",
                      "left:lc" ~ "Left $\\times$ Law/Crime",
                      "left:cr" ~ "Left $\\times$ Civil Rights",
                      "right:sw" ~ "Right $\\times$ Social Welfare",
                      "right:le" ~ "Right $\\times$ Labor/Employment",
                      "right:lc" ~ "Right $\\times$ Law/Crime",
                      "right:cr" ~ "Right $\\times$ Civil Rights",
                      .default = term
    ),
    # Format p-values
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    # Stars
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

# Create clean table
firth_table <- firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)

colnames(firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(firth_table, 
      format = "latex", 
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression (Robustness Check)",
      label = "firth") %>%
  kable_styling(font_size = 9, 
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)









### POLICY graphic
me_policy <- avg_comparisons(
  model_1_weighted,
  variables = "policy",
  vcov = vcovCL(model_1_weighted, cluster = ~username, type = "HC1")
)


me_df <- as.data.frame(me_policy) %>%
  mutate(
    estimate = estimate,
    conf.low_new = conf.high,
    conf.high_new = conf.low,
    conf.low = conf.low_new,
    conf.high = conf.high_new,
    policy_label = gsub("_", " ", contrast),
    policy_label = gsub(" - Government Operations", "", policy_label),
    policy_label = tools::toTitleCase(policy_label)
  )

figure8 <- ggplot(me_df, aes(x = reorder(policy_label, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, fill = "white", stroke = 1.2, shape = 21) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Increase in Pr(Populist Appeal)\nof Government Operations over each topic",
    subtitle = "H2: Government operations as populist baseline"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )


print(figure8)
ggsave("figure8.png", height = 14, width = 21, units = "cm", dpi = 700)




## Individual models
## Individual models
## Individual models
## Individual models

## Bolsonaro
bolsonaro <- data_model_all %>% filter(username == "jairbolsonaro")
bolsonaro <- bolsonaro %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
bolsonaro$policy <- relevel(bolsonaro$policy, ref = "government_operations")
y_bar <- mean(bolsonaro$populism)
bolsonaro <- bolsonaro %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
bolsonaro_model <- glm(
  populism ~ period + policy + word_count,
  data = bolsonaro,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(bolsonaro_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: Bolsonaro",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
bolsonaro_firth <- logistf(
  populism ~ period + policy + word_count,
  data = bolsonaro,
  weights = bolsonaro$balanced_weight
)

summary(bolsonaro_firth)

bolsonaro_firth_results <- data.frame(
  term = names(coef(bolsonaro_firth)),
  estimate = coef(bolsonaro_firth),
  se = sqrt(diag(vcov(bolsonaro_firth))),
  lower = bolsonaro_firth$ci.lower,
  upper = bolsonaro_firth$ci.upper,
  p = bolsonaro_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

bolsonaro_firth_table <- bolsonaro_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(bolsonaro_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(bolsonaro_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: Bolsonaro (Robustness Check)",
      label = "firth_bolsonaro") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)







unique(data_model_all$username)
## Lula
lula <- data_model_all %>% filter(username == "LulaOficial")
lula <- lula %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
lula$policy <- relevel(lula$policy, ref = "government_operations")
y_bar <- mean(lula$populism)
lula <- lula %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
lula_model <- glm(
  populism ~ period + policy + word_count,
  data = lula,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(lula_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: lula",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
lula_firth <- logistf(
  populism ~ period + policy + word_count,
  data = lula,
  weights = lula$balanced_weight
)

summary(lula_firth)

lula_firth_results <- data.frame(
  term = names(coef(lula_firth)),
  estimate = coef(lula_firth),
  se = sqrt(diag(vcov(lula_firth))),
  lower = lula_firth$ci.lower,
  upper = lula_firth$ci.upper,
  p = lula_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

lula_firth_table <- lula_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(lula_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(lula_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: lula (Robustness Check)",
      label = "firth_lula") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)





unique(data_model_all$username)
## ciro
ciro <- data_model_all %>% filter(username == "Ciro Gomes")
ciro <- ciro %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
ciro$policy <- relevel(ciro$policy, ref = "government_operations")
y_bar <- mean(ciro$populism)
ciro <- ciro %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
ciro_model <- glm(
  populism ~ period + policy + word_count,
  data = ciro,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(ciro_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: ciro",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
ciro_firth <- logistf(
  populism ~ period + policy + word_count,
  data = ciro,
  weights = ciro$balanced_weight
)

summary(ciro_firth)

ciro_firth_results <- data.frame(
  term = names(coef(ciro_firth)),
  estimate = coef(ciro_firth),
  se = sqrt(diag(vcov(ciro_firth))),
  lower = ciro_firth$ci.lower,
  upper = ciro_firth$ci.upper,
  p = ciro_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

ciro_firth_table <- ciro_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(ciro_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(ciro_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: ciro (Robustness Check)",
      label = "firth_ciro") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)



unique(data_model_all$username)
## pericles
pericles <- data_model_all %>% filter(username == "Leonardo Pericles")
pericles <- pericles %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
pericles$policy <- relevel(pericles$policy, ref = "government_operations")
y_bar <- mean(pericles$populism)
pericles <- pericles %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
pericles_model <- glm(
  populism ~ period + policy + word_count,
  data = pericles,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(pericles_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: pericles",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
pericles_firth <- logistf(
  populism ~ period + policy + word_count,
  data = pericles,
  weights = pericles$balanced_weight
)

summary(pericles_firth)

pericles_firth_results <- data.frame(
  term = names(coef(pericles_firth)),
  estimate = coef(pericles_firth),
  se = sqrt(diag(vcov(pericles_firth))),
  lower = pericles_firth$ci.lower,
  upper = pericles_firth$ci.upper,
  p = pericles_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

pericles_firth_table <- pericles_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(pericles_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(pericles_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: pericles (Robustness Check)",
      label = "firth_pericles") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)




unique(data_model_all$username)
## tebet
tebet <- data_model_all %>% filter(username == "Simone Tebet")
tebet <- tebet %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
tebet$policy <- relevel(tebet$policy, ref = "government_operations")
y_bar <- mean(tebet$populism)
tebet <- tebet %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
tebet_model <- glm(
  populism ~ period + policy + word_count,
  data = tebet,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(tebet_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: tebet",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
tebet_firth <- logistf(
  populism ~ period + policy + word_count,
  data = tebet,
  weights = tebet$balanced_weight
)

summary(tebet_firth)

tebet_firth_results <- data.frame(
  term = names(coef(tebet_firth)),
  estimate = coef(tebet_firth),
  se = sqrt(diag(vcov(tebet_firth))),
  lower = tebet_firth$ci.lower,
  upper = tebet_firth$ci.upper,
  p = tebet_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

tebet_firth_table <- tebet_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(tebet_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(tebet_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: tebet (Robustness Check)",
      label = "firth_tebet") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)





unique(data_model_all$username)
## davila
davila <- data_model_all %>% filter(username == "Felipe Davila")
davila <- davila %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
davila$policy <- relevel(davila$policy, ref = "government_operations")
y_bar <- mean(davila$populism)
davila <- davila %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
davila_model <- glm(
  populism ~ period + policy + word_count,
  data = davila,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(davila_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: davila",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
davila_firth <- logistf(
  populism ~ period + policy + word_count,
  data = davila,
  weights = davila$balanced_weight
)

summary(davila_firth)

davila_firth_results <- data.frame(
  term = names(coef(davila_firth)),
  estimate = coef(davila_firth),
  se = sqrt(diag(vcov(davila_firth))),
  lower = davila_firth$ci.lower,
  upper = davila_firth$ci.upper,
  p = davila_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

davila_firth_table <- davila_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(davila_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(davila_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: davila (Robustness Check)",
      label = "firth_davila") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)



unique(data_model_all$username)
## manzano
manzano <- data_model_all %>% filter(username == "Sofia Manzano")
manzano <- manzano %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
manzano$policy <- relevel(manzano$policy, ref = "government_operations")
y_bar <- mean(manzano$populism)
manzano <- manzano %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
manzano_model <- glm(
  populism ~ period + policy + word_count,
  data = manzano,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(manzano_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: manzano",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
manzano_firth <- logistf(
  populism ~ period + policy + word_count,
  data = manzano,
  weights = manzano$balanced_weight
)

summary(manzano_firth)

manzano_firth_results <- data.frame(
  term = names(coef(manzano_firth)),
  estimate = coef(manzano_firth),
  se = sqrt(diag(vcov(manzano_firth))),
  lower = manzano_firth$ci.lower,
  upper = manzano_firth$ci.upper,
  p = manzano_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

manzano_firth_table <- manzano_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(manzano_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(manzano_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: manzano (Robustness Check)",
      label = "firth_manzano") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)




unique(data_model_all$username)
## lucia
lucia <- data_model_all %>% filter(username == "Vera Lucia")
lucia <- lucia %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
lucia$policy <- relevel(lucia$policy, ref = "government_operations")
y_bar <- mean(lucia$populism)
lucia <- lucia %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
lucia_model <- glm(
  populism ~ period + policy + word_count,
  data = lucia,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(lucia_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: lucia",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
lucia_firth <- logistf(
  populism ~ period + policy + word_count,
  data = lucia,
  weights = lucia$balanced_weight
)

summary(lucia_firth)

lucia_firth_results <- data.frame(
  term = names(coef(lucia_firth)),
  estimate = coef(lucia_firth),
  se = sqrt(diag(vcov(lucia_firth))),
  lower = lucia_firth$ci.lower,
  upper = lucia_firth$ci.upper,
  p = lucia_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

lucia_firth_table <- lucia_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(lucia_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(lucia_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: lucia (Robustness Check)",
      label = "firth_lucia") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)


unique(data_model_all$username)
## soraya
soraya <- data_model_all %>% filter(username == "Soraya Thronicke")
soraya <- soraya %>%
  group_by(policy) %>%
  filter(n() / nrow(.) >= 0.01) %>%
  ungroup() %>%
  mutate(policy = droplevels(policy))
soraya$policy <- relevel(soraya$policy, ref = "government_operations")
y_bar <- mean(soraya$populism)
soraya <- soraya %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,
      (1 / (1 - y_bar)) / 2
    )
  )

## Main model
soraya_model <- glm(
  populism ~ period + policy + word_count,
  data = soraya,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

stargazer(soraya_model,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Logistic Regression: soraya",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors in parentheses.")

## Firth robustness check
soraya_firth <- logistf(
  populism ~ period + policy + word_count,
  data = soraya,
  weights = soraya$balanced_weight
)

summary(soraya_firth)

soraya_firth_results <- data.frame(
  term = names(coef(soraya_firth)),
  estimate = coef(soraya_firth),
  se = sqrt(diag(vcov(soraya_firth))),
  lower = soraya_firth$ci.lower,
  upper = soraya_firth$ci.upper,
  p = soraya_firth$prob
) %>%
  mutate(
    term = gsub("policy", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = tools::toTitleCase(term),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )

soraya_firth_table <- soraya_firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)
colnames(soraya_firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(soraya_firth_table,
      format = "latex",
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression: soraya (Robustness Check)",
      label = "firth_soraya") %>%
  kable_styling(font_size = 9,
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)



## Weighted regression -- Individual models
# Get all unique coefficient names across models
model_list <- list(
  bolsonaro_model, lula_model, ciro_model, tebet_model,
  pericles_model, manzano_model, lucia_model, davila_model, soraya_model
)

# Try with model list
stargazer(model_list,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Individual Candidate Logistic Regression Models",
          dep.var.labels = "Populist Appeal",
          column.labels = c("Bolsonaro", "Lula", "Ciro", "Tebet",
                            "Pericles", "Manzano", "V. Lucia", "D'Avila", "Thronicke"),
          model.numbers = FALSE,
          font.size = "tiny",
          column.sep.width = "1pt",
          notes = "Standard errors in parentheses.",
          notes.align = "l")

# Try with model list
stargazer(model_list,
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          title = "Individual Candidate Logistic Regression Models",
          dep.var.labels = "Populist Appeal",
          column.labels = c("Bolsonaro", "Lula", "Ciro", "Tebet",
                            "Pericles", "Manzano", "V. Lucia", "D'Avila", "Thronicke"),
          model.numbers = FALSE,
          font.size = "tiny",
          column.sep.width = "1pt",
          notes = "Standard errors in parentheses.",
          notes.align = "l")










### Second R&R: 
### Models reported on the paper:
### Models reported on the paper:
### Models reported on the paper:
### Models reported on the paper:
### Models reported on the paper:
### Models reported on the paper:
levels(pop_tweets_22_23$ideology_cluster3)

table(pop_tweets_22_23$ideology_cluster3)


levels(pop_tweets_22_23$policy_reduced)

table(pop_tweets_22_23$policy_reduced)


# center as baseline
pop_tweets_22_23 <- pop_tweets_22_23 %>%
  mutate(ideology_cluster3 = relevel(factor(ideology_cluster3), ref = "Center")) # theoretical choice

levels(pop_tweets_22_23$ideology_cluster4)


# UNWEIGHTED MODEL
model_1_unweighted_report <- glm(
  populism ~ 
    period + policy_reduced + ideology_cluster3 + word_count +
    ideology_cluster3:policy_reduced,     # H3a, H3b → expected positive # H4a, H4b → expected negative
  data = pop_tweets_22_23,
  family = binomial(link = "logit"),
)

# Get clustered standard errors by username
summary_m1_clustered <- coeftest(
  model_1_unweighted_report, 
  vcov = vcovCL(model_1_unweighted_report, 
                cluster = ~ username, 
                type = "HC1")
)

# Display results with clustered SEs
print(summary_m1_clustered)

# CORRECT: This object contains coefficients, clustered SEs, z-values, and p-values
# The p-values ARE based on the clustered SEs
summary_m1_unweighted <- summary_m1_clustered

# To view it nicely:
print(summary_m1_unweighted)

# Extract clustered SEs from the coeftest object
clustered_se <- summary_m1_unweighted[, "Std. Error"]

# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_unweighted_report,
          se = list(clustered_se),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")



# WEIGHTED MODEL
# Weighted main model -- all candidates
# STEP 1: Class Balancing Weights
# Calculate class proportions
y_bar <- mean(data_model_all$populism)


## new df for weighted model
data_model_all <- pop_tweets_22_23
# Create balanced weights to give equal importance to both classes
data_model_all <- data_model_all %>%
  mutate(
    balanced_weight = ifelse(
      populism == 1,
      (1 / y_bar) / 2,           # Upweight minority class
      (1 / (1 - y_bar)) / 2      # Downweight majority class
    )
  )


# Verify: both classes now have equal effective sample size
data_model_all %>%
  group_by(populism) %>%
  dplyr::summarize(
    n = n(),
    sum_weights = sum(balanced_weight),
    effective_n = sum_weights
  )



# STEP 2: Run the model
## Run weighted model
model_1_weighted_report <- glm(
  populism ~ 
    period + policy_reduced + ideology_cluster3 + word_count +
    ideology_cluster3:policy_reduced,     # H3a, H3b → expected positive # H4a, H4b → expected negative
  data = data_model_all,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

# STEP 3: Get Clustered Sandwich Standard Errors (REQUIRED for weighted models)
summary_m1_weighted_clustered <- coeftest(
  model_1_weighted_report, 
  vcov = vcovCL(model_1_weighted_report, 
                cluster = ~ username, 
                type = "HC1")
)

# Display results with clustered SEs
print(summary_m1_weighted_clustered)

# Save for later use
summary_m1_weighted <- summary_m1_weighted_clustered


# STEP 4: Extract Clustered SEs for Stargazer

# Extract clustered SEs from the coeftest object
clustered_se_weighted <- summary_m1_weighted[, "Std. Error"]

# STEP 5: Display with Stargazer

# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_weighted_report,
          se = list(clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Weighted Logistic Regression",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors (in parentheses) clustered by candidate. Weighted by inverse class frequency.")



# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_weighted_report,
          se = list(clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          title = "Weighted Logistic Regression",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors (in parentheses) clustered by candidate. Weighted by inverse class frequency.")



## export unweighted and weighted models together
# Anchor main effects so they don't match interactions
first_half_regex <- paste0("^", first_half, "$")

# TABLE PART 1: Main effects only
stargazer(model_1_unweighted_report, model_1_weighted_report,
          se = list(clustered_se, clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          column.labels = c("Unweighted", "Weighted"),
          model.names = FALSE,
          dep.var.labels = "Populist Appeal",
          title = "Logistic Regression Models of Populist Appeals",
          keep = first_half_regex,
          omit.stat = c("aic", "ll"),
          notes = c("Robust standard errors clustered by candidate in parentheses.",
                    "Weighted model uses inverse class frequency weighting.",
                    "Interaction terms reported in Table X (continued)."),
          notes.append = FALSE,
          font.size = "small",
          out = "table_models_part1.tex")

# TABLE PART 2: Interaction terms only
stargazer(model_1_unweighted_report, model_1_weighted_report,
          se = list(clustered_se, clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          column.labels = c("Unweighted", "Weighted"),
          model.names = FALSE,
          dep.var.labels = "",
          title = "Logistic Regression Models of Populist Appeals (Continued)",
          keep = ":",
          omit.stat = c("n", "aic", "ll"),
          add.lines = list(
            c("Observations", "11,885", "11,885"),
            c("Log Likelihood", round(logLik(model_1_unweighted_report), 1), 
              round(logLik(model_1_weighted_report), 1)),
            c("AIC", round(AIC(model_1_unweighted_report), 1), 
              round(AIC(model_1_weighted_report), 1))
          ),
          notes = c("Robust standard errors clustered by candidate in parentheses.",
                    "Weighted model uses inverse class frequency weighting.",
                    "Continued from previous table."),
          notes.append = FALSE,
          font.size = "small",
          out = "table_models_part2.tex")



# Campaign anova
library(car)

linearHypothesis(model_1_weighted_report, 
                 "periodCampaign - periodPre-Campaign = 0",
                 vcov = vcovCL(model_1_weighted_report, cluster = ~username, type = "HC1"))



## Plots:
library(broom)
library(ggplot2)


# PLOT TOTAL CONDITIONAL DIFFERENCE
me_interactions <- avg_comparisons(
  model_1_weighted_report,
  variables = "ideology_cluster3",
  by = "policy_reduced",
  vcov = vcovCL(model_1_weighted_report, cluster = ~username, type = "HC1")
) %>%
  as.data.frame() %>%
  filter(!policy_reduced %in% c("other", "government_operations"))


figure_total_conditional_effect <- ggplot(me_interactions, aes(x = policy_reduced, y = estimate, 
                            color = contrast, linetype = contrast)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.4),
                  size = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) gsub("_", " ", tools::toTitleCase(x))) +
  scale_color_manual(values = c("Left - Center" = "black", 
                                "Right - Center" = "gray50")) +
  scale_linetype_manual(values = c("Left - Center" = "solid", 
                                   "Right - Center" = "dashed")) +
  labs(x = "Policy Domain",
       y = "Difference in Pr(Populist Appeal)\nvs. Center",
       color = "Contrast",
       linetype = "Contrast",
       #title = "Policy × Ideology Interactions (H3 & H4)") 
)+
  theme_minimal()

ggsave("figure_total_conditional_effect.png", height = 14, width = 21, units = "cm", dpi = 700)


## PLOT INTERACTIONS
me_interactions <- avg_comparisons(
  model_1_weighted_report,
  variables = "policy_reduced",
  by = "ideology_cluster3",
  vcov = vcovCL(model_1_weighted_report, cluster = ~username, type = "HC1")
) %>%
  as.data.frame() %>%
  filter(!grepl("other", contrast, ignore.case = TRUE))

figure_interactions <- ggplot(me_interactions, aes(x = contrast, y = estimate, 
                                                   color = ideology_cluster3, linetype = ideology_cluster3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  position = position_dodge(width = 0.4),
                  size = 0.6) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = function(x) {
    x <- gsub(" - government_operations", "", x)
    x <- gsub("_", " ", x)
    tools::toTitleCase(x)
  }) +
  scale_color_manual(values = c("Center" = "gray70", "Left" = "black", "Right" = "gray40")) +
  scale_linetype_manual(values = c("Center" = "dotted", "Left" = "solid", "Right" = "dashed")) +
  labs(x = "Policy Domain (vs. Government Operations)",
       y = "Change in Pr(Populist Appeal)",
       color = "Ideology",
       linetype = "Ideology") +
  theme_minimal()

ggsave("figure_interactions.png", height = 14, width = 21, units = "cm", dpi = 700)



## TO solve separation issues
## TO solve separation issues
## TO solve separation issues
## TO solve separation issues
library(logistf)
model_firth <- logistf(
  populism ~ 
    period + policy_reduced + ideology_cluster3 + word_count +
    ideology_cluster3:policy_reduced,     # H3a, H3b → expected positive # H4a, H4b → expected negative
  data = data_model_all,
  weights = data_model_all$balanced_weight
)


# Extract Firth results
firth_results <- data.frame(
  term = names(coef(model_firth)),
  estimate = coef(model_firth),
  se = sqrt(diag(vcov(model_firth))),
  lower = model_firth$ci.lower,
  upper = model_firth$ci.upper,
  p = model_firth$prob
)
print(firth_results)

## Export table of robustness model
library(kableExtra)
firth_results <- firth_results %>%
  mutate(
    # Clean term names step by step
    term = gsub("policy_reduced", "", term),
    term = gsub("ideology_cluster3", "", term),
    term = gsub("period", "", term),
    term = gsub("_", " ", term),
    term = trimws(term),
    term = case_match(term,
                      "(Intercept)"          ~ "Intercept",
                      "Pre-Campaign"         ~ "Pre-Campaign",
                      "Campaign"             ~ "Campaign",
                      "other"                ~ "Other",
                      "civil rights"         ~ "Civil Rights",
                      "labor employment"     ~ "Labor/Employment",
                      "law crime"            ~ "Law/Crime",
                      "social welfare"       ~ "Social Welfare",
                      "Right"                ~ "Right",
                      "Left"                 ~ "Left",
                      "word count"           ~ "Word Count",
                      "other:Right"          ~ "Right $\\times$ Other",
                      "civil rights:Right"   ~ "Right $\\times$ Civil Rights",
                      "labor employment:Right" ~ "Right $\\times$ Labor/Employment",
                      "law crime:Right"      ~ "Right $\\times$ Law/Crime",
                      "social welfare:Right" ~ "Right $\\times$ Social Welfare",
                      "other:Left"           ~ "Left $\\times$ Other",
                      "civil rights:Left"    ~ "Left $\\times$ Civil Rights",
                      "labor employment:Left" ~ "Left $\\times$ Labor/Employment",
                      "law crime:Left"       ~ "Left $\\times$ Law/Crime",
                      "social welfare:Left"  ~ "Left $\\times$ Social Welfare",
                      .default = term
    ),
    p_formatted = ifelse(p < 0.001, "$<$0.001", sprintf("%.3f", p)),
    stars = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    se_fmt = sprintf("(%.3f)", se),
    ci_fmt = sprintf("[%.3f, %.3f]", lower, upper)
  )



# Create clean table
firth_table <- firth_results %>%
  select(term, estimate_fmt, se_fmt, ci_fmt, p_formatted)

colnames(firth_table) <- c("Term", "Estimate", "SE", "95\\% CI", "$p$-value")

kable(firth_table, 
      format = "latex", 
      booktabs = TRUE,
      escape = FALSE,
      row.names = FALSE,
      caption = "Firth Penalized Logistic Regression (Robustness Check)",
      label = "firth") %>%
  kable_styling(font_size = 9, 
                latex_options = c("hold_position")) %>%
  footnote(general = "Firth penalized likelihood estimates. Confidence intervals based on profile penalized likelihood.",
           general_title = "Note:",
           footnote_as_chunk = TRUE,
           escape = FALSE)





## Campaign and policy graphics
# plot — update model name, variable name, and label cleaning
me_policy <- avg_comparisons(
  model_1_weighted_report,
  variables = "policy_reduced",
  vcov = vcovCL(model_1_weighted_report, cluster = ~username, type = "HC1")
)

me_df <- as.data.frame(me_policy) %>%
  mutate(
    estimate = estimate,
    conf.low_new = conf.high,
    conf.high_new = conf.low,
    conf.low = conf.low_new,
    conf.high = conf.high_new,
    policy_label = gsub("_", " ", contrast),
    policy_label = gsub(" - government operations", "", policy_label, ignore.case = TRUE),
    policy_label = tools::toTitleCase(policy_label)
  )


me_df <- as.data.frame(me_policy) %>%
  mutate(
    policy_label = gsub("_", " ", contrast),
    policy_label = gsub(" - government operations", "", policy_label, ignore.case = TRUE),
    policy_label = tools::toTitleCase(policy_label)
  )


figure_policy <- ggplot(me_df, aes(x = reorder(policy_label, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 3, fill = "white", stroke = 1.2, shape = 21) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, linewidth = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Increase in Pr(Populist Appeal)\nof Government Operations over each topic",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )


print(figure_policy)
ggsave("figure_policy.png", height = 14, width = 21, units = "cm", dpi = 700)

#campaign
me_period <- avg_comparisons(
  model_1_weighted_report,
  variables = "period",
  vcov = vcovCL(model_1_weighted_report, cluster = ~username, type = "HC1")
)

me_period_df <- as.data.frame(me_period) %>%
  mutate(
    period_label = gsub(" - .*", "", contrast)
  )

me_period_df$period_label <- factor(me_period_df$period_label, 
                                    levels = c("Pre-Campaign","Campaign"))


figure_campaign <- ggplot(me_period_df, aes(x = reorder(period_label), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(size = 4, fill = "white", stroke = 1.5, shape = 21) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.15, linewidth = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Increase in Pr(Populist Appeal)\nof baseline period over each period",
    subtitle = " "
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())


print(figure_campaign)

ggsave("figure_campaign.png", height = 14, width = 21, units = "cm", dpi = 700)



## Partial populism
pop_tweets_22_23 <- pop_tweets_22_23 %>% 
  mutate(
    pop_partial = ifelse(ae + pc > 0,1,0)
  )

pop_tweets_22_23 <- pop_tweets_22_23 %>% 
  mutate(
    only_pop_partial = ifelse(ae + pc == 1,1,0)
  )

## partial populism tests
# UNWEIGHTED MODEL
model_1_unweighted_partialpop<- glm(
  pop_partial ~ 
    period + policy_reduced + ideology_cluster3 + word_count +
    ideology_cluster3:policy_reduced,     # H3a, H3b → expected positive # H4a, H4b → expected negative
  data = pop_tweets_22_23,
  family = binomial(link = "logit"),
)

# Get clustered standard errors by username
summary_m1_clustered <- coeftest(
  model_1_unweighted_partialpop, 
  vcov = vcovCL(model_1_unweighted_partialpop, 
                cluster = ~ username, 
                type = "HC1")
)

# Display results with clustered SEs
print(summary_m1_clustered)

# CORRECT: This object contains coefficients, clustered SEs, z-values, and p-values
# The p-values ARE based on the clustered SEs
summary_m1_unweighted <- summary_m1_clustered

# To view it nicely:
print(summary_m1_unweighted)

# Extract clustered SEs from the coeftest object
clustered_se <- summary_m1_unweighted[, "Std. Error"]

# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_unweighted_partialpop,
          se = list(clustered_se),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text")



# WEIGHTED MODEL
# Weighted main model -- all candidates
# STEP 1: Class Balancing Weights
# Calculate class proportions


## new df for weighted model
data_model_all <- pop_tweets_22_23
# Create balanced weights to give equal importance to both classes
y_bar <- mean(data_model_all$pop_partial)


data_model_all <- data_model_all %>%
  mutate(
    balanced_weight = ifelse(
      pop_partial == 1,
      (1 / y_bar) / 2,           # Upweight minority class
      (1 / (1 - y_bar)) / 2      # Downweight majority class
    )
  )


# Verify: both classes now have equal effective sample size
data_model_all %>%
  group_by(pop_partial) %>%
  dplyr::summarize(
    n = n(),
    sum_weights = sum(balanced_weight),
    effective_n = sum_weights
  )


unique(data_model_all$balanced_weight)

data_model_all <- data_model_all %>%
  mutate(
    ideology_cluster3 = relevel(factor(ideology_cluster3), ref = "Center"),
    policy_reduced = relevel(factor(policy_reduced), ref = "government_operations")
  )
# STEP 2: Run the model
## Run weighted model
model_1_weighted_partialpop <- glm(
  pop_partial ~ 
    period + policy_reduced + ideology_cluster3 + word_count +
    ideology_cluster3:policy_reduced,     # H3a, H3b → expected positive # H4a, H4b → expected negative
  data = data_model_all,
  family = binomial(link = "logit"),
  weights = balanced_weight
)

# STEP 3: Get Clustered Sandwich Standard Errors (REQUIRED for weighted models)
summary_m1_weighted_clustered <- coeftest(
  model_1_weighted_partialpop, 
  vcov = vcovCL(model_1_weighted_partialpop, 
                cluster = ~ username, 
                type = "HC1")
)

# Display results with clustered SEs
print(summary_m1_weighted_clustered)

# Save for later use
summary_m1_weighted <- summary_m1_weighted_clustered


# STEP 4: Extract Clustered SEs for Stargazer

# Extract clustered SEs from the coeftest object
clustered_se_weighted <- summary_m1_weighted[, "Std. Error"]

# STEP 5: Display with Stargazer

# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_weighted_partialpop,
          se = list(clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "text",
          title = "Weighted Logistic Regression",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors (in parentheses) clustered by candidate. Weighted by inverse class frequency.")



# Use stargazer with the MODEL object, but override SEs
stargazer(model_1_weighted_partialpop,
          se = list(clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          title = "Weighted Logistic Regression",
          dep.var.labels = "Populist Appeal",
          notes = "Standard errors (in parentheses) clustered by candidate. Weighted by inverse class frequency.")



## export unweighted and weighted models together
# Anchor main effects so they don't match interactions
first_half_regex <- paste0("^", first_half, "$")

# TABLE PART 1: Main effects only
stargazer(model_1_unweighted_partialpop, model_1_weighted_partialpop,
          se = list(clustered_se, clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          column.labels = c("Unweighted", "Weighted"),
          model.names = FALSE,
          dep.var.labels = "Populist Appeal",
          title = "Logistic Regression Models of Populist Appeals",
          keep = first_half_regex,
          omit.stat = c("aic", "ll"),
          notes = c("Robust standard errors clustered by candidate in parentheses.",
                    "Weighted model uses inverse class frequency weighting.",
                    "Interaction terms reported in Table X (continued)."),
          notes.append = FALSE,
          font.size = "small",
          out = "table_models_part1.tex")

# TABLE PART 2: Interaction terms only
stargazer(model_1_unweighted_partialpop, model_1_weighted_partialpop,
          se = list(clustered_se, clustered_se_weighted),
          star.cutoffs = c(0.05, 0.01, 0.001),
          type = "latex",
          column.labels = c("Unweighted", "Weighted"),
          model.names = FALSE,
          dep.var.labels = "",
          title = "Logistic Regression Models of Populist Appeals (Continued)",
          keep = ":",
          omit.stat = c("n", "aic", "ll"),
          add.lines = list(
            c("Observations", "11,885", "11,885"),
            c("Log Likelihood", round(logLik(model_1_unweighted_report), 1), 
              round(logLik(model_1_weighted_report), 1)),
            c("AIC", round(AIC(model_1_unweighted_report), 1), 
              round(AIC(model_1_weighted_report), 1))
          ),
          notes = c("Robust standard errors clustered by candidate in parentheses.",
                    "Weighted model uses inverse class frequency weighting.",
                    "Continued from previous table."),
          notes.append = FALSE,
          font.size = "small",
          out = "table_models_part2.tex")






### Antagonism of populism. Separate sample for manual annotation of antagonism
set.seed(123)
target_n <- round(511 * 0.20)

# Calculate sample sizes per domain
sample_sizes <- pop_tweets_22_23 %>%
  filter(populism == 1) %>%
  count(policy_reduced) %>%
  mutate(
    proportional_n = round(n * target_n / 511),
    sample_n = pmax(10, proportional_n),
    sample_n = pmin(n, sample_n)
  )

print(sample_sizes)

# Sample using the calculated sizes
populist_sample <- pop_tweets_22_23 %>%
  filter(populism == 1) %>%
  group_by(policy_reduced) %>%
  nest() %>%
  left_join(sample_sizes %>% select(policy_reduced, sample_n), by = "policy_reduced") %>%
  mutate(sampled = map2(data, sample_n, ~ slice_sample(.x, n = .y))) %>%
  select(policy_reduced, sampled) %>%
  unnest(sampled) %>%
  ungroup()

nrow(populist_sample)

populist_sample %>%
  count(policy_reduced) %>%
  mutate(pct = n / sum(n) * 100)

write_xlsx(populist_sample, "sample_antagonism_validation.xlsx")



sum(pop_tweets_22_23$populism)


