library(forcats)
library(reshape2)
library(ggpubr)
library(ggplot2)
library(haven)
library(dplyr)

# Read dataset from SPSS file
data_hu <- read_sav("~/projects/AaD_Research/datasets/scrubbed_datasets/hungary_scrubbed.sav")

questions <- c("id","A10A_1_control_reversed", "A10A_2_control_reversed", "A10A_3_control_reversed", "A10B_1_experiment_reversed", "A10B_2_experiment_reversed", "A10B_3_experiment_reversed")

data_hu_questions <- data_hu[questions]

# Define a function to calculate the proportion of responses equal to y
prop <- function(x, y) {
  sum(x == y, na.rm = TRUE) / sum(!is.na(x))
}

# Calculate proportions for each response category (1 to 4) for questions 2 to 7
h <- data.frame(
  "Strongly Disagree" = apply(data_hu_questions[2:7], 2, prop, y = 1),
  "Disagree"          = apply(data_hu_questions[2:7], 2, prop, y = 2),
  "Agree"             = apply(data_hu_questions[2:7], 2, prop, y = 3),
  "Strongly Agree"    = apply(data_hu_questions[2:7], 2, prop, y = 4)
)
# Add row names and set a specific order
h$rowname <- rownames(h)

h$rowname <- factor(h$rowname, 
                    levels = c("A10A_1_control_reversed", "A10B_1_experiment_reversed",
                               "A10A_2_control_reversed", "A10B_2_experiment_reversed",
                               "A10A_3_control_reversed", "A10B_3_experiment_reversed"),
                    labels = c("Control A", "Experimental A",
                               "Control B", "Experimental B",
                               "Control C", "Experimental C"))

mh <- melt(h, id.vars = "rowname")

#===============================
# Calculate Means for Each Group
#===============================
stacked_plot <- ggplot(mh, aes(x = rowname, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack", color = "black", size = 0.2) +
  coord_flip() +
  labs(title = "Survey Response Distribution",
       subtitle = "Control vs. Experimental Conditions",
       x = "Survey Question",
       y = "Proportion of Responses",
       fill = "Response Category") +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

if(!dir.exists("~/projects/AaD_Research/output/plots/hungary/dist"))
  stop("Directory not found!")

ggsave(filename = "~/projects/AaD_Research/output/plots/hungary/dist/hungary_stacked_bar_graph.pdf", 
       plot = stacked_plot,
       width = 10, height = 7, device = "pdf")

#===============================
# Calculate Means for Each Group
#===============================
control_means <- data_hu_questions %>%
  select(contains("control")) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))
treatment_means <- data_hu_questions %>%
  select(contains("experiment")) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))

# Reshape the means into a long format and add a grouping variable
control_long <- melt(control_means, variable.name = "question", value.name = "mean_response")
control_long$group <- "Control"
treatment_long <- melt(treatment_means, variable.name = "question", value.name = "mean_response")
treatment_long$group <- "Experimental"

# Combine the two groups
mean_df <- rbind(control_long, treatment_long)

# Extract question number (1, 2, 3) which corresponds to questions A, B, C
# For Hungary data, format is A10A_1_control_reversed, A10A_2_control_reversed, etc.
mean_df$question_number <- as.numeric(substr(mean_df$question, 6, 6))

# Map question numbers to letters
mean_df$question_type <- factor(mean_df$question_number,
                                levels = c(1, 2, 3),
                                labels = c("Question A", "Question B", "Question C"))

# Create the grouped bar graph
compare_bar_graph <- ggplot(mean_df, aes(x = question_type, y = mean_response, fill = group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, color = "black", size = 0.2) +
  labs(title = "Comparison of Mean Survey Responses",
       subtitle = "Control vs. Experimental Conditions",
       x = "Question",
       y = "Mean Response (1 = Strongly Disagree, 4 = Strongly Agree)",
       fill = "Group") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 14),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_blank()
  )

if(!dir.exists("~/projects/AaD_Research/output/plots/hungary/dist"))
  stop("Directory not found!")

ggsave(filename = "~/projects/AaD_Research/output/plots/hungary/dist/hungary_compare_bar_graph.pdf", 
       plot = compare_bar_graph,
       width = 10, height = 7, device = "pdf")

# clears env variables

rm(list = ls())