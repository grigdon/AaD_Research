library(endorse)
library(tidyverse)
library(haven)
library(ggpubr)
library(missForest)
library(forcats)
library(reshape2)
library(haven)
library(mice)
library(Amelia)

Base_final_CJ_client <- read_sav("C:\\Users\\chris\\Downloads\\Data Siroky\\Data\\Poland2\\data-poland\\Base_final_+CJ_client.sav")
data_pl <- Base_final_CJ_client
questions <- c("id","Q10A", "Q10B", "Q10C", "Q10D", "Q10E", "Q10F")
label_map <- c("Q10A" = "Spending on Roma_control",
               "Q10B" = "Euroscepticism_control",
               "Q10C" = "Laxer Gun Control_control",
               "Q10D" = "Spending on Roma_experiment",
               "Q10E" = "Euroscepticism_experiment",
               "Q10F" = "Laxer Gun Control_experiment"
)
data_pl_questions <- data_pl[questions]
data_pl_questions <- mutate(data_pl_questions, across(2:7, ~ as.factor(recode(as.numeric(.),
                                                                              `1` = 4L,
                                                                              `2` = 3L,
                                                                              `3` = 2L,
                                                                              `4` = 1L,
                                                                              `9` = 99L)
)
)
)

prop <- function(x, y){
  return(sum(x == y, na.rm=TRUE)/sum(!is.na(x), na.rm = TRUE))
}
h <- data.frame("Strongly Disagree" = apply(data_pl_questions[2:7], 2, prop, y = 1),
                "Disagree" = apply(data_pl_questions[2:7], 2, prop, y = 2),
                "Agree" = apply(data_pl_questions[2:7], 2, prop, y = 3),
                "Strongly Agree" = apply(data_pl_questions[2:7], 2, prop, y = 4),
                "DK" = apply(data_pl_questions[2:7], 2, prop, y = 99),
                "NA" = apply(data_pl_questions[2:7], 2, prop, y = NA)
)
h <- rownames_to_column(h)
h$rowname <- factor(label_map[h$rowname], levels = label_map)
mh <- melt(h)
endorse_plot <- ggplot()+ geom_bar(data = mh, aes(x = rowname, y=value, fill= variable), position="stack", stat="identity")+ coord_flip() +  theme(legend.position="bottom") + ggtitle("Poland Endorsement") + xlab("Question") + ylab("Percent Response") + labs(fill = "Response")
endorse_plot

# Prepare the data for density plotting
# First, separate control and treatment questions
control_data <- data_pl_questions %>%
  select(Q10A, Q10B, Q10C) %>%
  gather(key = "question", value = "response") %>%
  mutate(
    group = "Control",
    question = case_when(
      question == "Q10A" ~ "Spending on Roma",
      question == "Q10B" ~ "Euroscepticism",
      question == "Q10C" ~ "Laxer Gun Control"
    )
  )

treatment_data <- data_pl_questions %>%
  select(Q10D, Q10E, Q10F) %>%
  gather(key = "question", value = "response") %>%
  mutate(
    group = "Treatment",
    question = case_when(
      question == "Q10D" ~ "Spending on Roma",
      question == "Q10E" ~ "Euroscepticism",
      question == "Q10F" ~ "Laxer Gun Control"
    )
  )

# Combine the data
all_data <- rbind(control_data, treatment_data) %>%
  filter(response != "99") %>%  # Remove DK responses for density plot
  mutate(response = as.numeric(as.character(response)))  # Convert to numeric

# Calculate means for each question and group
means_data <- all_data %>%
  group_by(question, group) %>%
  summarise(mean_response = mean(response, na.rm = TRUE), .groups = 'drop')

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
         subtitle = sprintf("Control Mean: %.2f, Treatment Mean: %.2f",
                            q_means$mean_response[q_means$group == "Control"],
                            q_means$mean_response[q_means$group == "Treatment"]),
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

# Print summary statistics
for(q in questions) {
  cat("\nSummary for", q, ":\n")
  subset_data <- all_data %>% filter(question == q)
  
  # Print means
  cat("Control Mean:", round(mean(subset_data$response[subset_data$group == "Control"], na.rm = TRUE), 2), "\n")
  cat("Treatment Mean:", round(mean(subset_data$response[subset_data$group == "Treatment"], na.rm = TRUE), 2), "\n")
  
  # Print standard deviations
  cat("Control SD:", round(sd(subset_data$response[subset_data$group == "Control"], na.rm = TRUE), 2), "\n")
  cat("Treatment SD:", round(sd(subset_data$response[subset_data$group == "Treatment"], na.rm = TRUE), 2), "\n")
}