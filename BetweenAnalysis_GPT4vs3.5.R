### Feelings vs. Reason: GPT Decision-making 
### Between-group analysis: GPT4 vs. GPT3.5
### Programmed by Feng XIAO (2025.3.17)
### This R script requires one excel file: 'rawdata'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','meta')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Analysis
rd <- read_excel('rawdata.xlsx', sheet = 'Summary2', na = '---')
epsilon <- 1 #continuity correction
rd_gpt <- rd %>%
  mutate(
    GPT4_gamble = GPT4_gamble + epsilon,
    `GPT4_sure-thing` = `GPT4_sure-thing` + epsilon,
    GPT3.5_gamble = GPT3.5_gamble + epsilon,
    `GPT3.5_sure-thing` = `GPT3.5_sure-thing` + epsilon
  )
# Positive frames
p_gpt <- rd_gpt %>% filter(description == 'positive')
p_gpt <- p_gpt %>%
  mutate(
    OR = (GPT4_gamble / `GPT4_sure-thing`) / (GPT3.5_gamble / `GPT3.5_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT4_gamble + 1 / `GPT4_sure-thing` + 1 / GPT3.5_gamble + 1 / `GPT3.5_sure-thing`),
  )

p_gpt_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(p_gpt$frame)

for (frame in frames) {
  frame_data <- p_gpt %>% filter(frame == !!frame)
  
  meta_analysis <- metagen(
    TE = frame_data$log_OR, #logOR for analysis
    seTE = frame_data$SE,
    studlab = frame_data$frame,
    comb.fixed = FALSE,
    comb.random = TRUE,
    method.tau = "DL"
  ) # Perform meta-analysis for the current frame
  
  log_AOR <- meta_analysis$TE.random
  log_AOR_SE <- meta_analysis$seTE.random
  
  p_gpt_aor <- rbind(p_gpt_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

p_meta_analysis <- metagen(
  TE = p_gpt_aor$log_AOR,
  seTE = p_gpt_aor$log_AOR_SE,
  studlab = p_gpt_aor$frame, 
  byvar = p_gpt$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(p_meta_analysis)

frame_p <- data.frame(
  mode = p_meta_analysis$byvar,
  frame = p_meta_analysis$studlab,
  log_AOR = p_meta_analysis$TE,
  lower_CI = p_meta_analysis$lower,
  upper_CI = p_meta_analysis$upper
)
frame_p$description <- 'positive'
subgroup_p <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_p$description <- 'positive'
sum_p <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'positive'
)
mode_p <- rbind(subgroup_p,sum_p)
# Negative frames
n_gpt <- rd_gpt %>% filter(description == 'negative')
n_gpt <- n_gpt %>%
  mutate(
    OR = (GPT4_gamble / `GPT4_sure-thing`) / (GPT3.5_gamble / `GPT3.5_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT4_gamble + 1 / `GPT4_sure-thing` + 1 / GPT3.5_gamble + 1 / `GPT3.5_sure-thing`),
  )

n_gpt_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(n_gpt$frame)

for (frame in frames) {
  frame_data <- n_gpt %>% filter(frame == !!frame)
  
  meta_analysis <- metagen(
    TE = frame_data$log_OR, #logOR for analysis
    seTE = frame_data$SE,
    studlab = frame_data$frame,
    comb.fixed = FALSE,
    comb.random = TRUE,
    method.tau = "DL"
  ) # Perform meta-analysis for the current frame
  
  log_AOR <- meta_analysis$TE.random
  log_AOR_SE <- meta_analysis$seTE.random
  
  n_gpt_aor <- rbind(n_gpt_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

n_meta_analysis <- metagen(
  TE = n_gpt_aor$log_AOR,
  seTE = n_gpt_aor$log_AOR_SE,
  studlab = n_gpt_aor$frame, 
  byvar = n_gpt$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(n_meta_analysis)

frame_n <- data.frame(
  mode = n_meta_analysis$byvar,
  frame = n_meta_analysis$studlab,
  log_AOR = n_meta_analysis$TE,
  lower_CI = n_meta_analysis$lower,
  upper_CI = n_meta_analysis$upper
)
frame_n$description <- 'negative'
subgroup_n <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_n$description <- 'negative'
sum_n <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'negative'
)
mode_n <- rbind(subgroup_n,sum_n)
# Moral frames
m_gpt <- rd_gpt %>% filter(description == 'moral')
m_gpt <- m_gpt %>%
  mutate(
    OR = (GPT4_gamble / `GPT4_sure-thing`) / (GPT3.5_gamble / `GPT3.5_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT4_gamble + 1 / `GPT4_sure-thing` + 1 / GPT3.5_gamble + 1 / `GPT3.5_sure-thing`),
  )

m_gpt_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(m_gpt$frame)

for (frame in frames) {
  frame_data <- m_gpt %>% filter(frame == !!frame)
  
  meta_analysis <- metagen(
    TE = frame_data$log_OR, #logOR for analysis
    seTE = frame_data$SE,
    studlab = frame_data$frame,
    comb.fixed = FALSE,
    comb.random = TRUE,
    method.tau = "DL"
  ) # Perform meta-analysis for the current frame
  
  log_AOR <- meta_analysis$TE.random
  log_AOR_SE <- meta_analysis$seTE.random
  
  m_gpt_aor <- rbind(m_gpt_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

m_meta_analysis <- metagen(
  TE = m_gpt_aor$log_AOR,
  seTE = m_gpt_aor$log_AOR_SE,
  studlab = m_gpt_aor$frame, 
  byvar = m_gpt$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(m_meta_analysis)

frame_m <- data.frame(
  mode = m_meta_analysis$byvar,
  frame = m_meta_analysis$studlab,
  log_AOR = m_meta_analysis$TE,
  lower_CI = m_meta_analysis$lower,
  upper_CI = m_meta_analysis$upper
)
frame_m$description <- 'moral'
subgroup_m <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_m$description <- 'moral'
sum_m <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'moral'
)
mode_m <- rbind(subgroup_m,sum_m)

## Log OR for each frame
frame_table <- rbind(frame_p,frame_n,frame_m)
frame_table$frame <- factor(frame_table$frame, levels =  c('600 lives (feeling)','600 lives (reason)','600 lives (overall)',
                                                           '6 kin (feeling)','6 kin (reason)','6 kin (overall)',
                                                           '400 dollars (feeling)','400 dollars (reason)','400 dollars (overall)',
                                                           'DRUGGIST (feeling)','DRUGGIST (reason)','DRUGGIST (overall)',
                                                           'TROLLEY (feeling)','TROLLEY (reason)','TROLLEY (overall)'
))
frame_table$mode <- factor(frame_table$mode, levels = c('Feeling','Reason','Overall'))

write.xlsx(frame_table, file = 'Outputs/BetweenAnalysis_frame (GPT4-GPT3.5).xlsx', 
           rowNames = FALSE)