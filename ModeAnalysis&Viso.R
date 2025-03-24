### Feelings vs. Reason: GPT Decision-making 
### Decision mode consistency analysis
### Programmed by Feng XIAO (2025.3.17)
### This R script requires one excel file: 'rawdata'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','boot','ggplot2')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Compute proportion of choices for each decision mode
compute_prop <- function(data, mode_col){
  unique_choices <- unique(na.omit(data[[mode_col]]))  
  choice_counts <- table(factor(data[[mode_col]], levels = c("a", "b", "c")))  
  total <- sum(choice_counts)  
  
  prop_a <- if ("a" %in% unique_choices) choice_counts["a"] / total else 0
  prop_b <- if ("b" %in% unique_choices) choice_counts["b"] / total else 0
  prop_c <- if ("c" %in% unique_choices) choice_counts["c"] / total else 0
  
  if ("c" %in% unique_choices) {
    return(c(A = prop_a, B = prop_b, C = prop_c)) 
  } else {
    return(c(A = prop_a, B = prop_b))  
  }
}

### Bootstrap function
bootstrap_function <- function(data, indices) {
  boot_sample <- data[indices, ]
  p_feeling_boot <- compute_prop(boot_sample, 1)
  p_reason_boot  <- compute_prop(boot_sample, 2)
  p_overall_boot <- compute_prop(boot_sample, 3)
  
  D_overall_feeling_boot <- sqrt(sum((p_overall_boot - p_feeling_boot)^2))
  D_overall_reason_boot  <- sqrt(sum((p_overall_boot - p_reason_boot)^2))
  
  return(D_overall_reason_boot - D_overall_feeling_boot)
}

### Analysis
########################################################################################################
scenario_names <- c('P600life','P6kin','P400dollar','N600life','N6kin','N400dollar','Druggist','Trolley')
## GPT-3.5
p_gpt3.5 <- read_excel('rawdata.xlsx', sheet = 'P_gpt3.5', na = '---')
n_gpt3.5 <- read_excel('rawdata.xlsx', sheet = 'N_gpt3.5', na = '---')
m_gpt3.5 <- read_excel('rawdata.xlsx', sheet = 'Moral_gpt3.5', na = '---')
P_gpt3.5 <- p_gpt3.5[,-1]
N_gpt3.5 <- n_gpt3.5[,-1]
M_gpt3.5 <- m_gpt3.5[,-1]
Merge_gpt3.5 <- cbind(P_gpt3.5, N_gpt3.5, M_gpt3.5)
num_columns <- ncol(Merge_gpt3.5)
num_scenarios <- num_columns / 3  
gpt3.5_df <- data.frame(Scenario = scenario_names,
                        Delta_D = numeric(num_scenarios),
                        CI_Lower = numeric(num_scenarios),
                        CI_Upper = numeric(num_scenarios),
                        stringsAsFactors = FALSE)

set.seed(666)
for (i in seq(1, ncol(Merge_gpt3.5), by = 3)) {
  scenario_index <- (i - 1) / 3 + 1  
  scenario_name <- scenario_names[scenario_index]  
  scenario_data <- Merge_gpt3.5[, i:(i+2)]  
  
  p_feeling <- compute_prop(scenario_data, 1)
  p_reason  <- compute_prop(scenario_data, 2)
  p_overall <- compute_prop(scenario_data, 3)
  
  D_overall_feeling <- sqrt(sum((p_overall - p_feeling)^2))
  D_overall_reason  <- sqrt(sum((p_overall - p_reason)^2))
  delta_D <- D_overall_reason - D_overall_feeling
  
  B <- 10000
  boot_results <- boot(scenario_data, statistic = bootstrap_function, R = B)
  ci <- boot.ci(boot_results, type = "perc")
  
  ci_lower <- ci$percent[4]
  ci_upper <- ci$percent[5]
  
  gpt3.5_df[scenario_index, ] <- list(Scenario = scenario_name,
                                      Delta_D = delta_D,
                                      CI_Lower = ci_lower,
                                      CI_Upper = ci_upper)
}

## GPT-4
p_gpt4 <- read_excel('rawdata.xlsx', sheet = 'P_gpt4', na = '---')
n_gpt4 <- read_excel('rawdata.xlsx', sheet = 'N_gpt4', na = '---')
m_gpt4 <- read_excel('rawdata.xlsx', sheet = 'Moral_gpt4', na = '---')
P_gpt4 <- p_gpt4[,-1]
N_gpt4 <- n_gpt4[,-1]
M_gpt4 <- m_gpt4[,-1]
Merge_gpt4 <- cbind(P_gpt4, N_gpt4, M_gpt4)
num_columns <- ncol(Merge_gpt4)
num_scenarios <- num_columns / 3  
gpt4_df <- data.frame(Scenario = scenario_names,
                      Delta_D = numeric(num_scenarios),
                      CI_Lower = numeric(num_scenarios),
                      CI_Upper = numeric(num_scenarios),
                      stringsAsFactors = FALSE)

set.seed(666)
for (i in seq(1, ncol(Merge_gpt4), by = 3)) {
  scenario_index <- (i - 1) / 3 + 1  
  scenario_name <- scenario_names[scenario_index]  
  scenario_data <- Merge_gpt4[, i:(i+2)]  
  
  p_feeling <- compute_prop(scenario_data, 1)
  p_reason  <- compute_prop(scenario_data, 2)
  p_overall <- compute_prop(scenario_data, 3)
  
  D_overall_feeling <- sqrt(sum((p_overall - p_feeling)^2))
  D_overall_reason  <- sqrt(sum((p_overall - p_reason)^2))
  delta_D <- D_overall_reason - D_overall_feeling
  
  B <- 10000
  boot_results <- boot(scenario_data, statistic = bootstrap_function, R = B)
  ci <- boot.ci(boot_results, type = "perc")
  
  ci_lower <- ci$percent[4]
  ci_upper <- ci$percent[5]
  
  gpt4_df[scenario_index, ] <- list(Scenario = scenario_name,
                                    Delta_D = delta_D,
                                    CI_Lower = ci_lower,
                                    CI_Upper = ci_upper)
}

## Human
p_human <- read_excel('rawdata.xlsx', sheet = 'P_human', na = '---')
n_human <- read_excel('rawdata.xlsx', sheet = 'N_human', na = '---')
m_human <- read_excel('rawdata.xlsx', sheet = 'Moral_human', na = '---')
P_human <- p_human[,-1]
N_human <- n_human[,-1]
M_human <- m_human[,-1]
Merge_human <- cbind(P_human, N_human, M_human)
num_columns <- ncol(Merge_human)
num_scenarios <- num_columns / 3  
human_df <- data.frame(Scenario = scenario_names,
                       Delta_D = numeric(num_scenarios),
                       CI_Lower = numeric(num_scenarios),
                       CI_Upper = numeric(num_scenarios),
                       stringsAsFactors = FALSE)

set.seed(666)
for (i in seq(1, ncol(Merge_human), by = 3)) {
  scenario_index <- (i - 1) / 3 + 1  
  scenario_name <- scenario_names[scenario_index]  
  scenario_data <- Merge_human[, i:(i+2)]  
  
  p_feeling <- compute_prop(scenario_data, 1)
  p_reason  <- compute_prop(scenario_data, 2)
  p_overall <- compute_prop(scenario_data, 3)
  
  D_overall_feeling <- sqrt(sum((p_overall - p_feeling)^2))
  D_overall_reason  <- sqrt(sum((p_overall - p_reason)^2))
  delta_D <- D_overall_reason - D_overall_feeling
  
  B <- 10000
  boot_results <- boot(scenario_data, statistic = bootstrap_function, R = B)
  ci <- boot.ci(boot_results, type = "perc")
  
  ci_lower <- ci$percent[4]
  ci_upper <- ci$percent[5]
  
  human_df[scenario_index, ] <- list(Scenario = scenario_name,
                                     Delta_D = delta_D,
                                     CI_Lower = ci_lower,
                                     CI_Upper = ci_upper)
}

gpt3.5_df <- gpt3.5_df %>% mutate(Agent = "GPT-3.5")
gpt4_df   <- gpt4_df %>% mutate(Agent = "GPT-4")
human_df  <- human_df %>% mutate(Agent = "Humans")
combined_df <- bind_rows(gpt3.5_df, gpt4_df, human_df)
write.xlsx(combined_df, 
           file = 'Outputs/ModeAnalysis.xlsx',
           rowNames = FALSE)

### Plotting
########################################################################################################
combined_df$Scenario <- factor(combined_df$Scenario, levels = c('P600life','N600life','P6kin','N6kin','P400dollar',
                                                                'N400dollar','Druggist','Trolley'))
combined_df$Agent <- factor(combined_df$Agent, levels = c('Humans','GPT-3.5','GPT-4'))
p<-ggplot(combined_df, aes(y = Scenario, x = Delta_D, group = Agent)) +
  geom_point(size = 1, shape = 21, fill = "black", color = "black", stroke = 0.4) +  
  geom_errorbar(aes(xmin = CI_Lower, xmax = CI_Upper), 
                width = 0.15, 
                color = "black", 
                size = 0.3, 
                alpha = 0.8) + 
  geom_vline(xintercept = 0, linetype = 'dashed', size = 0.4, color = "#d73027") +  
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 7, color = "black"), 
    axis.text = element_text(size = 7, color = "black"),  
    strip.text = element_text(size = 7, face = "bold", color = "black"),  
    plot.margin = margin(t = 2, r = 5, b = 2, l = 5) 
  ) +
  scale_x_continuous(
    limits = c(-2, 2),  
    breaks = seq(-2, 2, by = 1),  
    expand = c(0, 0)  
  ) +
  labs(y = NULL, x = "Euclidean distance difference") +  
  guides(color = "none", fill="none") +  
  facet_wrap(~ Agent, ncol = 3, strip.position = "top") 
ggsave('ModeViso.pdf', plot = p, width = 7, height = 3, dpi = 600)