### Feelings vs. Reason: GPT Decision-making 
### Within-group analysis: proportion z-tests
### Programmed by Feng XIAO (2025.3.17)
### This R script requires one excel file: 'rawdata'

### Preparation
package_list <- c('tidyr','dplyr','readxl','openxlsx')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Proportion z-test (positive vs. negative)
z.prop = function(x1, x2, n1, n2){
  numerator = (x1/n1) - (x2/n2)
  p.common = (x1 + x2) / (n1 + n2)
  denominator = sqrt(p.common * (1 - p.common) * (1/n1 + 1/n2))
  z.prop.ris = numerator / denominator
  p_value = 2 * (1 - pnorm(abs(z.prop.ris)))
  margin_of_error = qnorm(0.975) * denominator
  ci_lower = z.prop.ris - margin_of_error
  ci_upper = z.prop.ris + margin_of_error
  return(list(z = z.prop.ris, p = p_value, ci_low = ci_lower, ci_up = ci_upper))
}

### Proportion z-test (moral frame: DRUGGGIST)
z.prop.three = function(x1, x2, x3, n){
  p1 = x1 / n
  p2 = x2 / n
  p3 = x3 / n
  
  p_common = (x1 + x2 + x3) / (3 * n)
  se = sqrt(p_common * (1 - p_common) * (2 / n))
  
  z12 = (p1 - p2) / se #steal vs. try
  p_value12 = 2 * (1 - pnorm(abs(z12)))
  ci_lower12 = z12 - qnorm(0.975) * se
  ci_upper12 = z12 + qnorm(0.975) * se
  
  z13 = (p1 - p3) / se #steal vs. quit
  p_value13 = 2 * (1 - pnorm(abs(z13)))
  ci_lower13 = z13 - qnorm(0.975) * se
  ci_upper13 = z13 + qnorm(0.975) * se
  
  z23 = (p2 - p3) / se #try vs. quit
  p_value23 = 2 * (1 - pnorm(abs(z23)))
  ci_lower23 = z23 - qnorm(0.975) * se
  ci_upper23 = z23 + qnorm(0.975) * se
  
  return(list(
    z12 = z12, p_value12 = p_value12, ci_low12 = ci_lower12, ci_up12 = ci_upper12,
    z13 = z13, p_value13 = p_value13, ci_low13 = ci_lower13, ci_up13 = ci_upper13,
    z23 = z23, p_value23 = p_value23, ci_low23 = ci_lower23, ci_up23 = ci_upper23
  ))
}

### Analysis
########################################################################################################
## GPT-3.5
p_gpt3.5 <- read_excel('rawdata.xlsx', sheet = 'P_gpt3.5', na = '---')
n_gpt3.5 <- read_excel('rawdata.xlsx', sheet = 'N_gpt3.5', na = '---')
m_gpt3.5 <- read_excel('rawdata.xlsx', sheet = 'Moral_gpt3.5', na = '---')
P_gpt3.5 <- p_gpt3.5[,-1]
N_gpt3.5 <- n_gpt3.5[,-1]
M_gpt3.5 <- m_gpt3.5[,-1]

cTable_gpt3.5 <- data.frame(frame = c('600 lives (feeling)','600 lives (reason)','600 lives (overall)',
                                      '6 kin (feeling)','6 kin (reason)','6 kin (overall)',
                                      '400 dollars (feeling)','400 dollars (reason)','400 dollars (overall)',
                                      'DRUGGIST (feeling)','DRUGGIST (reason)','DRUGGIST (overall)',
                                      'TROLLEY (feeling)','TROLLEY (reason)','TROLLEY (overall)'),
                      type = c('GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5',
                               'GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5','GPT-3.5',
                               'GPT-3.5','GPT-3.5','GPT-3.5'),
                      domain = c('Terrorist scenario','Terrorist scenario','Terrorist scenario',
                                 'Disease scenario','Disease scenario','Disease scenario',
                               'Monetary scenario','Monetary scenario','Monetary scenario',
                               'Druggist dilemma','Druggist dilemma','Druggist dilemma',
                               'Trolley dilemma','Trolley dilemma','Trolley dilemma'),
                      z12 = numeric(15),
                      p12 = numeric(15),
                      ci_low12 = numeric(15),
                      ci_up12 = numeric(15),
                      z13 = numeric(15),
                      p13 = numeric(15),
                      ci_low13 = numeric(15),
                      ci_up13 = numeric(15),
                      z23 = numeric(15),
                      p23 = numeric(15),
                      ci_low23 = numeric(15),
                      ci_up23 = numeric(15)) #For paired frame: 1-RS, 2-RA; For TROLLEY: 1-throw, 2-not throw

for (i in 1:ncol(P_gpt3.5)) {
  a_p <- sum(P_gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(P_gpt3.5[,i] == 'b', na.rm = TRUE)+1
  a_n <- sum(N_gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_n <- sum(N_gpt3.5[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt3.5$z12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$z
  cTable_gpt3.5$p12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$p
  cTable_gpt3.5$ci_low12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_low
  cTable_gpt3.5$ci_up12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_up
} ##Paired frames: RS (positive vs. negative)

for (i in 1:3) {
  a_p <- sum(M_gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(M_gpt3.5[,i] == 'b', na.rm = TRUE)+1
  c_p <- sum(M_gpt3.5[,i] == 'c', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt3.5$z12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z12
  cTable_gpt3.5$p12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value12
  cTable_gpt3.5$ci_low12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low12
  cTable_gpt3.5$ci_up12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up12
  
  cTable_gpt3.5$z13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z13
  cTable_gpt3.5$p13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value13
  cTable_gpt3.5$ci_low13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low13
  cTable_gpt3.5$ci_up13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up13
  
  cTable_gpt3.5$z23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z23
  cTable_gpt3.5$p23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value23
  cTable_gpt3.5$ci_low23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low23
  cTable_gpt3.5$ci_up23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up23
} ##DRUGGIST frame

for (i in 4:6) {
  a_p <- sum(M_gpt3.5[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(M_gpt3.5[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt3.5$z12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$z
  cTable_gpt3.5$p12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$p
  cTable_gpt3.5$ci_low12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_low
  cTable_gpt3.5$ci_up12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_up
} ##TROLLEY frame (throw vs. not throw)

write.xlsx(cTable_gpt3.5, 
           file = 'Outputs/WithinAnalysis_GPT3.5.xlsx',
           rowNames = FALSE)
###############################################################################################################
## GPT-4
p_gpt4 <- read_excel('rawdata.xlsx', sheet = 'P_gpt4', na = '---')
n_gpt4 <- read_excel('rawdata.xlsx', sheet = 'N_gpt4', na = '---')
m_gpt4 <- read_excel('rawdata.xlsx', sheet = 'Moral_gpt4', na = '---')
P_gpt4 <- p_gpt4[,-1]
N_gpt4 <- n_gpt4[,-1]
M_gpt4 <- m_gpt4[,-1]

cTable_gpt4 <- data.frame(frame = c('600 lives (feeling)','600 lives (reason)','600 lives (overall)',
                                    '6 kin (feeling)','6 kin (reason)','6 kin (overall)',
                                    '400 dollars (feeling)','400 dollars (reason)','400 dollars (overall)',
                                    'DRUGGIST (feeling)','DRUGGIST (reason)','DRUGGIST (overall)',
                                    'TROLLEY (feeling)','TROLLEY (reason)','TROLLEY (overall)'),
                          type = c('GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4',
                                   'GPT-4','GPT-4','GPT-4','GPT-4','GPT-4','GPT-4',
                                   'GPT-4','GPT-4','GPT-4'),
                          domain = c('Terrorist scenario','Terrorist scenario','Terrorist scenario',
                                     'Disease scenario','Disease scenario','Disease scenario',
                                     'Monetary scenario','Monetary scenario','Monetary scenario',
                                     'Druggist dilemma','Druggist dilemma','Druggist dilemma',
                                     'Trolley dilemma','Trolley dilemma','Trolley dilemma'),
                          z12 = numeric(15),
                          p12 = numeric(15),
                          ci_low12 = numeric(15),
                          ci_up12 = numeric(15),
                          z13 = numeric(15),
                          p13 = numeric(15),
                          ci_low13 = numeric(15),
                          ci_up13 = numeric(15),
                          z23 = numeric(15),
                          p23 = numeric(15),
                          ci_low23 = numeric(15),
                          ci_up23 = numeric(15)) #For paired frame: 1-RS, 2-RA; For TROLLEY: 1-throw, 2-not throw

for (i in 1:ncol(P_gpt4)) {
  a_p <- sum(P_gpt4[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(P_gpt4[,i] == 'b', na.rm = TRUE)+1
  a_n <- sum(N_gpt4[,i] == 'a', na.rm = TRUE)+1
  b_n <- sum(N_gpt4[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4$z12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$z
  cTable_gpt4$p12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$p
  cTable_gpt4$ci_low12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_low
  cTable_gpt4$ci_up12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_up
}

for (i in 1:3) {
  a_p <- sum(M_gpt4[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(M_gpt4[,i] == 'b', na.rm = TRUE)+1
  c_p <- sum(M_gpt4[,i] == 'c', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4$z12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z12
  cTable_gpt4$p12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value12
  cTable_gpt4$ci_low12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low12
  cTable_gpt4$ci_up12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up12
  
  cTable_gpt4$z13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z13
  cTable_gpt4$p13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value13
  cTable_gpt4$ci_low13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low13
  cTable_gpt4$ci_up13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up13
  
  cTable_gpt4$z23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z23
  cTable_gpt4$p23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value23
  cTable_gpt4$ci_low23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low23
  cTable_gpt4$ci_up23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up23
} ##DRUGGIST frame

for (i in 4:6) {
  a_p <- sum(M_gpt4[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(M_gpt4[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_gpt4$z12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$z
  cTable_gpt4$p12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$p
  cTable_gpt4$ci_low12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_low
  cTable_gpt4$ci_up12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_up
} ##TROLLEY frame (throw vs. not throw)

write.xlsx(cTable_gpt4, 
           file = 'Outputs/WithinAnalysis_GPT4.xlsx',
           rowNames = FALSE)
###############################################################################################################
## Human
p_human <- read_excel('rawdata.xlsx', sheet = 'P_human', na = '---')
n_human <- read_excel('rawdata.xlsx', sheet = 'N_human', na = '---')
m_human <- read_excel('rawdata.xlsx', sheet = 'Moral_human', na = '---')
P_human<- p_human[,-1]
N_human <- n_human[,-1]
M_human <- m_human[,-1]

cTable_human <- data.frame(frame = c('600 lives (feeling)','600 lives (reason)','600 lives (overall)',
                                     '6 kin (feeling)','6 kin (reason)','6 kin (overall)',
                                     '400 dollars (feeling)','400 dollars (reason)','400 dollars (overall)',
                                     'DRUGGIST (feeling)','DRUGGIST (reason)','DRUGGIST (overall)',
                                     'TROLLEY (feeling)','TROLLEY (reason)','TROLLEY (overall)'),
                           type = c('Human','Human','Human','Human','Human','Human',
                                    'Human','Human','Human','Human','Human','Human',
                                    'Human','Human','Human'),
                           domain = c('Terrorist scenario','Terrorist scenario','Terrorist scenario',
                                      'Disease scenario','Disease scenario','Disease scenario',
                                      'Monetary scenario','Monetary scenario','Monetary scenario',
                                      'Druggist dilemma','Druggist dilemma','Druggist dilemma',
                                      'Trolley dilemma','Trolley dilemma','Trolley dilemma'),
                           z12 = numeric(15),
                           p12 = numeric(15),
                           ci_low12 = numeric(15),
                           ci_up12 = numeric(15),
                           z13 = numeric(15),
                           p13 = numeric(15),
                           ci_low13 = numeric(15),
                           ci_up13 = numeric(15),
                           z23 = numeric(15),
                           p23 = numeric(15),
                           ci_low23 = numeric(15),
                           ci_up23 = numeric(15)) #For paired frame: 1-RS, 2-RA; For TROLLEY: 1-throw, 2-not throw

for (i in 1:ncol(P_human)) {
  a_p <- sum(P_human[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(P_human[,i] == 'b', na.rm = TRUE)+1
  a_n <- sum(N_human[,i] == 'a', na.rm = TRUE)+1
  b_n <- sum(N_human[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_human$z12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$z
  cTable_human$p12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$p
  cTable_human$ci_low12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_low
  cTable_human$ci_up12[i] <- z.prop(b_p, b_n, a_p+b_p, a_n+b_n)$ci_up
}

for (i in 1:3) {
  a_p <- sum(M_human[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(M_human[,i] == 'b', na.rm = TRUE)+1
  c_p <- sum(M_human[,i] == 'c', na.rm = TRUE)+1 #plus-one corrections
  cTable_human$z12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z12
  cTable_human$p12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value12
  cTable_human$ci_low12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low12
  cTable_human$ci_up12[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up12
  
  cTable_human$z13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z13
  cTable_human$p13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value13
  cTable_human$ci_low13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low13
  cTable_human$ci_up13[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up13
  
  cTable_human$z23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$z23
  cTable_human$p23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$p_value23
  cTable_human$ci_low23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_low23
  cTable_human$ci_up23[i+9] <- z.prop.three(a_p, b_p, c_p, a_p+b_p+c_p)$ci_up23
} ##DRUGGIST frame

for (i in 4:6) {
  a_p <- sum(M_human[,i] == 'a', na.rm = TRUE)+1
  b_p <- sum(M_human[,i] == 'b', na.rm = TRUE)+1 #plus-one corrections
  cTable_human$z12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$z
  cTable_human$p12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$p
  cTable_human$ci_low12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_low
  cTable_human$ci_up12[i+9] <- z.prop(b_p, a_p, a_p+b_p, a_p+b_p)$ci_up
} ##TROLLEY frame (throw vs. not throw)

write.xlsx(cTable_human, 
           file = 'Outputs/WithinAnalysis_Human.xlsx',
           rowNames = FALSE)