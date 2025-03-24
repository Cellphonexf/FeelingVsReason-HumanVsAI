### Feelings vs. Reason: GPT Decision-making 
### Between-group analysis: Human vs. AI
### Programmed by Feng XIAO (2025.3.17)
### This R script requires one excel file: 'rawdata'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx','meta')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Analysis
rd <- read_excel('rawdata.xlsx', sheet = 'Summary1', na = '---')
epsilon <- 1 #continuity correction
## GPT-3.5
rd_gpt3.5 <- rd %>% filter(model == 'GPT-3.5')
rd_gpt3.5 <- rd_gpt3.5 %>%
  mutate(
    GPT_gamble = GPT_gamble + epsilon,
    `GPT_sure-thing` = `GPT_sure-thing` + epsilon,
    H_gamble = H_gamble + epsilon,
    `H_sure-thing` = `H_sure-thing` + epsilon
  )
# Positive frames
p_gpt3.5 <- rd_gpt3.5 %>% filter(description == 'positive')
p_gpt3.5 <- p_gpt3.5 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

p_gpt3.5_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(p_gpt3.5$frame)

for (frame in frames) {
  frame_data <- p_gpt3.5 %>% filter(frame == !!frame)
  
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
  
  p_gpt3.5_aor <- rbind(p_gpt3.5_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

p_meta_analysis <- metagen(
  TE = p_gpt3.5_aor$log_AOR,
  seTE = p_gpt3.5_aor$log_AOR_SE,
  studlab = p_gpt3.5_aor$frame, 
  byvar = p_gpt3.5$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(p_meta_analysis)

frame_p_3.5 <- data.frame(
  mode = p_meta_analysis$byvar,
  frame = p_meta_analysis$studlab,
  log_AOR = p_meta_analysis$TE,
  lower_CI = p_meta_analysis$lower,
  upper_CI = p_meta_analysis$upper
)
frame_p_3.5$description <- 'positive'
frame_p_3.5$subject <- 'GPT-3.5'
subgroup_p_3.5 <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_p_3.5$description <- 'positive'
subgroup_p_3.5$subject <- 'GPT-3.5'
sum_p_3.5 <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'positive',
  subject = 'GPT-3.5'
)
mode_p_3.5 <- rbind(subgroup_p_3.5,sum_p_3.5)
# Negative frames
n_gpt3.5 <- rd_gpt3.5 %>% filter(description == 'negative')
n_gpt3.5 <- n_gpt3.5 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

n_gpt3.5_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(n_gpt3.5$frame)

for (frame in frames) {
  frame_data <- n_gpt3.5 %>% filter(frame == !!frame)
  
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
  
  n_gpt3.5_aor <- rbind(n_gpt3.5_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

n_meta_analysis <- metagen(
  TE = n_gpt3.5_aor$log_AOR,
  seTE = n_gpt3.5_aor$log_AOR_SE,
  studlab = n_gpt3.5_aor$frame, 
  byvar = n_gpt3.5$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(n_meta_analysis)

frame_n_3.5 <- data.frame(
  mode = n_meta_analysis$byvar,
  frame = n_meta_analysis$studlab,
  log_AOR = n_meta_analysis$TE,
  lower_CI = n_meta_analysis$lower,
  upper_CI = n_meta_analysis$upper
)
frame_n_3.5$description <- 'negative'
frame_n_3.5$subject <- 'GPT-3.5'
subgroup_n_3.5 <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_n_3.5$description <- 'negative'
subgroup_n_3.5$subject <- 'GPT-3.5'
sum_n_3.5 <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'negative',
  subject = 'GPT-3.5'
)
mode_n_3.5 <- rbind(subgroup_n_3.5,sum_n_3.5)
# Moral frames
m_gpt3.5 <- rd_gpt3.5 %>% filter(description == 'moral')
m_gpt3.5 <- m_gpt3.5 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

m_gpt3.5_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(m_gpt3.5$frame)

for (frame in frames) {
  frame_data <- m_gpt3.5 %>% filter(frame == !!frame)
  
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
  
  m_gpt3.5_aor <- rbind(m_gpt3.5_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

m_meta_analysis <- metagen(
  TE = m_gpt3.5_aor$log_AOR,
  seTE = m_gpt3.5_aor$log_AOR_SE,
  studlab = m_gpt3.5_aor$frame, 
  byvar = m_gpt3.5$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(m_meta_analysis)

frame_m_3.5 <- data.frame(
  mode = m_meta_analysis$byvar,
  frame = m_meta_analysis$studlab,
  log_AOR = m_meta_analysis$TE,
  lower_CI = m_meta_analysis$lower,
  upper_CI = m_meta_analysis$upper
)
frame_m_3.5$description <- 'moral'
frame_m_3.5$subject <- 'GPT-3.5'
subgroup_m_3.5 <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_m_3.5$description <- 'moral'
subgroup_m_3.5$subject <- 'GPT-3.5'
sum_m_3.5 <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'moral',
  subject = 'GPT-3.5'
)
mode_m_3.5 <- rbind(subgroup_m_3.5,sum_m_3.5)

## GPT-4
rd_gpt4 <- rd %>% filter(model == 'GPT-4')
rd_gpt4 <- rd_gpt4 %>%
  mutate(
    GPT_gamble = GPT_gamble + epsilon,
    `GPT_sure-thing` = `GPT_sure-thing` + epsilon,
    H_gamble = H_gamble + epsilon,
    `H_sure-thing` = `H_sure-thing` + epsilon
  )
# Positive frames
p_gpt4 <- rd_gpt4 %>% filter(description == 'positive')
p_gpt4 <- p_gpt4 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

p_gpt4_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(p_gpt4$frame)

for (frame in frames) {
  frame_data <- p_gpt4 %>% filter(frame == !!frame)
  
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
  
  p_gpt4_aor <- rbind(p_gpt4_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

p_meta_analysis <- metagen(
  TE = p_gpt4_aor$log_AOR,
  seTE = p_gpt4_aor$log_AOR_SE,
  studlab = p_gpt4_aor$frame, 
  byvar = p_gpt4$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(p_meta_analysis)

frame_p_4 <- data.frame(
  mode = p_meta_analysis$byvar,
  frame = p_meta_analysis$studlab,
  log_AOR = p_meta_analysis$TE,
  lower_CI = p_meta_analysis$lower,
  upper_CI = p_meta_analysis$upper
)
frame_p_4$description <- 'positive'
frame_p_4$subject <- 'GPT-4'
subgroup_p_4 <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_p_4$description <- 'positive'
subgroup_p_4$subject <- 'GPT-4'
sum_p_4 <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'positive',
  subject = 'GPT-4'
)
mode_p_4 <- rbind(subgroup_p_4,sum_p_4)
# Negative frames
n_gpt4 <- rd_gpt4 %>% filter(description == 'negative')
n_gpt4 <- n_gpt4 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

n_gpt4_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(n_gpt4$frame)

for (frame in frames) {
  frame_data <- n_gpt4 %>% filter(frame == !!frame)
  
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
  
  n_gpt4_aor <- rbind(n_gpt4_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

n_meta_analysis <- metagen(
  TE = n_gpt4_aor$log_AOR,
  seTE = n_gpt4_aor$log_AOR_SE,
  studlab = n_gpt4_aor$frame, 
  byvar = n_gpt4$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(n_meta_analysis)

frame_n_4 <- data.frame(
  mode = n_meta_analysis$byvar,
  frame = n_meta_analysis$studlab,
  log_AOR = n_meta_analysis$TE,
  lower_CI = n_meta_analysis$lower,
  upper_CI = n_meta_analysis$upper
)
frame_n_4$description <- 'negative'
frame_n_4$subject <- 'GPT-4'
subgroup_n_4 <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_n_4$description <- 'negative'
subgroup_n_4$subject <- 'GPT-4'
sum_n_4 <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'negative',
  subject = 'GPT-4'
)
mode_n_4 <- rbind(subgroup_n_4,sum_n_4)
# Moral frames
m_gpt4 <- rd_gpt4 %>% filter(description == 'moral')
m_gpt4 <- m_gpt4 %>%
  mutate(
    OR = (GPT_gamble / `GPT_sure-thing`) / (H_gamble / `H_sure-thing`),
    log_OR = log(OR),
    SE = sqrt(1 / GPT_gamble + 1 / `GPT_sure-thing` + 1 / H_gamble + 1 / `H_sure-thing`),
  )

m_gpt4_aor <- data.frame(
  frame = character(),
  log_AOR = numeric(),
  log_AOR_SE = numeric()
)
frames <- unique(m_gpt4$frame)

for (frame in frames) {
  frame_data <- m_gpt4 %>% filter(frame == !!frame)
  
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
  
  m_gpt4_aor <- rbind(m_gpt4_aor, data.frame(
    frame = frame,
    log_AOR = log_AOR,
    log_AOR_SE = log_AOR_SE
  ))
}

m_meta_analysis <- metagen(
  TE = m_gpt4_aor$log_AOR,
  seTE = m_gpt4_aor$log_AOR_SE,
  studlab = m_gpt4_aor$frame, 
  byvar = m_gpt4$mode,
  comb.fixed = FALSE,
  comb.random = TRUE,
  method.tau = "DL"  #DerSimonian-Laird methods for REM
)
sum_meta <- summary(m_meta_analysis)

frame_m_4 <- data.frame(
  mode = m_meta_analysis$byvar,
  frame = m_meta_analysis$studlab,
  log_AOR = m_meta_analysis$TE,
  lower_CI = m_meta_analysis$lower,
  upper_CI = m_meta_analysis$upper
)
frame_m_4$description <- 'moral'
frame_m_4$subject <- 'GPT-4'
subgroup_m_4 <- data.frame(
  mode = names(sum_meta$TE.random.w),
  log_AOR = sum_meta$TE.random.w,
  lower_CI = sum_meta$lower.random.w,
  upper_CI = sum_meta$upper.random.w
)
subgroup_m_4$description <- 'moral'
subgroup_m_4$subject <- 'GPT-4'
sum_m_4 <- data.frame(
  mode = 'Total',
  log_AOR = sum_meta$TE.random,
  lower_CI = sum_meta$lower.random,
  upper_CI = sum_meta$upper.random,
  description = 'moral',
  subject = 'GPT-4'
)
mode_m_4 <- rbind(subgroup_m_4,sum_m_4)

## Log OR for each frame
frame_table <- rbind(frame_p_3.5,frame_p_4,frame_n_3.5,frame_n_4,frame_m_3.5,frame_m_4)
frame_table$frame <- factor(frame_table$frame, levels =  c('600 lives (feeling)','600 lives (reason)','600 lives (overall)',
                                                           '6 kin (feeling)','6 kin (reason)','6 kin (overall)',
                                                           '400 dollars (feeling)','400 dollars (reason)','400 dollars (overall)',
                                                           'DRUGGIST (feeling)','DRUGGIST (reason)','DRUGGIST (overall)',
                                                           'TROLLEY (feeling)','TROLLEY (reason)','TROLLEY (overall)'
))
frame_table$mode <- factor(frame_table$mode, levels = c('Feeling','Reason','Overall'))
frame_table$subject <- factor(frame_table$subject, levels = c('GPT-3.5','GPT-4'))
frame_table$description <- factor(frame_table$description, levels = c('positive','negative','moral'))

write.xlsx(frame_table, file = 'Outputs/BetweenAnalysis_frame (AI-Humans).xlsx', 
           rowNames = FALSE)