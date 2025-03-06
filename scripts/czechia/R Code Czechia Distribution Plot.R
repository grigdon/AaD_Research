#|warning: FALSE
library(endorse)
library(tidyverse)
library(haven)
library(ggpubr)
library(missForest)
library(forcats)
library(reshape2)
data_cz <- read_sav("C:\\Users\\chris\\Downloads\\Data Siroky\\Data\\Czechia\\data-Czechia\\DataFile_Uncivil Society.sav")
questions <- c("CD","Q10AA", "Q10AB", "Q10AC", "Q10BA", "Q10BB", "Q10BC")
label_map <- c("Q10AA" = "Spending on Roma_control",
               "Q10BA" = "Spending on Roma_experiment",
               "Q10AB" = "Euroscepticism_control",
               "Q10BB" = "Euroscepticism_experiment",
               "Q10AC" = "Laxer Gun Control_control",
               "Q10BC" = "Laxer Gun Control_experiment"
)
data_cz_questions <- data_cz[questions]
data_cz_questions <- mutate(data_cz_questions, across(2:7, ~ recode(as.numeric(.),
                                                                    `1` = 4L,
                                                                    `2` = 3L,
                                                                    `3` = 2L,
                                                                    `4` = 1L,
                                                                    `9` = 9L,
                                                                    `7` = 7L)
)
)

#|warning: FALSE
prop <- function(x, y){
  return(sum(x == y, na.rm=TRUE)/sum(!is.na(x), na.rm = TRUE))
}
h <- data.frame("Strongly Disagree" = apply(data_cz_questions[2:7], 2, prop, y = 1),
                "Disagree" = apply(data_cz_questions[2:7], 2, prop, y = 2),
                "Agree" = apply(data_cz_questions[2:7], 2, prop, y = 3),
                "Strongly Agree" = apply(data_cz_questions[2:7], 2, prop, y = 4),
                #"NA" = apply(data_cz_questions[2:7], 2, prop, y = 7),
                "DK" = apply(data_cz_questions[2:7], 2, prop, y = 9)
)
h <- rownames_to_column(h)
h$rowname <- factor(label_map[h$rowname], levels = label_map)
mh <- melt(h)
endorse_plot <- ggplot()+ geom_bar(data = mh, aes(x = rowname, y=value, fill= variable), position="stack", stat="identity")+ coord_flip() +  theme(legend.position="bottom") + ggtitle("Czechia Endorsement") + xlab("Question") + ylab("Percent Response") + labs(fill = "Response")
endorse_plot


# Prepare the data for density plotting
# First, separate control and treatment questions
control_data <- data_cz_questions %>%
  select(Q10AA, Q10AB, Q10AC) %>%
  gather(key = "question", value = "response") %>%
  mutate(
    group = "Control",
    question = case_when(
      question == "Q10AA" ~ "Spending on Roma",
      question == "Q10AB" ~ "Euroscepticism",
      question == "Q10AC" ~ "Laxer Gun Control"
    )
  )

treatment_data <- data_cz_questions %>%
  select(Q10BA, Q10BB, Q10BC) %>%
  gather(key = "question", value = "response") %>%
  mutate(
    group = "Treatment",
    question = case_when(
      question == "Q10BA" ~ "Spending on Roma",
      question == "Q10BB" ~ "Euroscepticism",
      question == "Q10BC" ~ "Laxer Gun Control"
    )
  )

# Combine the data
all_data <- rbind(control_data, treatment_data) %>%
  filter(response != 9, response != 7) %>%  # Remove DK (9) and NA (7) responses for density plot
  mutate(response = as.numeric(as.character(response)))  # Convert to numeric

# Calculate means for each question and group
means_data <- all_data %>%
  group_by(question, group) %>%
  summarise(
    mean_response = mean(response, na.rm = TRUE),
    sd_response = sd(response, na.rm = TRUE),
    .groups = 'drop'
  )

# Create separate plots for each question
question_plots <- list()
questions <- unique(all_data$question)

for(q in questions) {
  subset_data <- all_data %>% filter(question == q)
  q_means <- means_data %>% filter(question == q)
  
  question_plots[[q]] <- ggplot(subset_data, aes(x = response, fill = group)) +
    geom_density(alpha = 0.5) +
    geom_vline(data = q_means,
               aes(xintercept = mean_response, color = group),
               linetype = "dashed",
               size = 1) +
    theme_minimal() +
    labs(title = q,
         subtitle = sprintf("Control Mean: %.2f (SD: %.2f)\nTreatment Mean: %.2f (SD: %.2f)",
                            q_means$mean_response[q_means$group == "Control"],
                            q_means$sd_response[q_means$group == "Control"],
                            q_means$mean_response[q_means$group == "Treatment"],
                            q_means$sd_response[q_means$group == "Treatment"]),
         x = "Response Level (1 = Strongly Disagree, 4 = Strongly Agree)",
         y = "Density",
         fill = "Group",
         color = "Group Mean") +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2") +
    theme(legend.position = "bottom",
          plot.title = element_text(face = "bold"),
          axis.text = element_text(size = 10))
}

# Arrange all plots in a grid
ggarrange(plotlist = question_plots, 
          ncol = 1, 
          nrow = 3,
          common.legend = TRUE,
          legend = "bottom")

# Print detailed summary statistics
for(q in questions) {
  cat("\nSummary for", q, ":\n")
  subset_data <- all_data %>% filter(question == q)
  
  # Calculate statistics for each group
  for(g in c("Control", "Treatment")) {
    group_data <- subset_data %>% filter(group == g)
    cat("\n", g, ":\n")
    cat("Mean:", round(mean(group_data$response, na.rm = TRUE), 2), "\n")
    cat("SD:", round(sd(group_data$response, na.rm = TRUE), 2), "\n")
    cat("Median:", round(median(group_data$response, na.rm = TRUE), 2), "\n")
    cat("N:", sum(!is.na(group_data$response)), "\n")
  }
}