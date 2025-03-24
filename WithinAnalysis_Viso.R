### Feelings vs. Reason: GPT Decision-making 
### Visualization: within-group analysis
### Programmed by Feng XIAO (2025.3.17)
### This R script requires one excel file: 'Plotting_WithinGroup'

### Preparation
package_list <- c('tidyr','dplyr','readxl', 'ggplot2','scales','cowplot',
                  'reshape2','ggbreak','patchwork','gridExtra')
lapply(package_list, require, character.only = TRUE)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Visualization for within-group analysis
# Terrorist scenario
rd1 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Terrorist scenario', na = '---')
rd1$frame <- factor(rd1$frame, levels = c('feeling','reason','overall'))
p1a<-ggplot(data=rd1, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('Humans') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p1b<-ggplot(data=rd1, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p1c<-ggplot(data=rd1, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking (%)') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p1 <- grid.arrange(p1a, p1b, p1c, ncol = 1)
# Disease scenario
rd2 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Disease scenario', na = '---')
rd2$frame <- factor(rd2$frame, levels = c('feeling','reason','overall'))
p2a<-ggplot(data=rd2, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('Humans') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p2b<-ggplot(data=rd2, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p2c<-ggplot(data=rd2, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p2 <- grid.arrange(p2a, p2b, p2c, ncol = 1)
# Monetary scenario
rd3 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Monetary scenario', na = '---')
rd3$frame <- factor(rd3$frame, levels = c('feeling','reason','overall'))
p3a<-ggplot(data=rd3, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('Humans') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p3b<-ggplot(data=rd3, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p3c<-ggplot(data=rd3, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Risk-seeking') +
  scale_color_manual(values = c('negative'='#4169E1','positive'='#B22222')) +
  scale_shape_manual(values = c('negative' = 15, 'positive' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  geom_hline(yintercept = 0.5, linetype = 'dashed', size = 0.4,
             color = "black") + 
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p3 <- grid.arrange(p3a, p3b, p3c, ncol = 1)
# Druggist dilemma
rd4 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Druggist dilemma', na = '---')
rd4$frame <- factor(rd4$frame, levels = c('feeling','reason','overall'))
p4a<-ggplot(data=rd4, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('try'='#4169E1','steal'='#B22222',
                                'quit'='#6F4F28')) +
  scale_shape_manual(values = c('try' = 15, 'steal' = 16, 
                                'quit' = 18)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('Humans') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p4b<-ggplot(data=rd4, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('try'='#4169E1','steal'='#B22222',
                                'quit'='#6F4F28')) +
  scale_shape_manual(values = c('try' = 15, 'steal' = 16, 
                                'quit' = 18)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p4c<-ggplot(data=rd4, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('try'='#4169E1','steal'='#B22222',
                                'quit'='#6F4F28')) +
  scale_shape_manual(values = c('try' = 15, 'steal' = 16, 
                                'quit' = 18)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p4 <- grid.arrange(p4a, p4b, p4c, ncol = 1)
# Trolley dilemma
rd5 <- read_excel('Plotting_WithinGroup.xlsx', sheet = 'Trolley dilemma', na = '---')
rd5$frame <- factor(rd5$frame, levels = c('feeling','reason','overall'))
p5a<-ggplot(data=rd5, aes(x=frame, y=human, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame, y=human, group=description, color=description), size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(human), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('not throw'='#4169E1','throw'='#B22222')) +
  scale_shape_manual(values = c('not throw' = 15, 'throw' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) scales::percent(abs(x), accuracy = 1),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('Humans') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p5b<-ggplot(data=rd5, aes(x=frame, y=gpt3.5, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt3.5,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt3.5), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('not throw'='#4169E1','throw'='#B22222')) +
  scale_shape_manual(values = c('not throw' = 15, 'throw' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-3.5') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    axis.line.x = element_blank(),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p5c<-ggplot(data=rd5, aes(x=frame, y=gpt4, fill=description,
                          condition=factor(1))) +
  geom_line(aes(x=frame,y=gpt4,group=description,color=description),size=0.4) +
  geom_point(aes(color=description, shape=description), size=0.8) +
  geom_text(aes(label=scales::percent(abs(gpt4), accuracy = 1), color=description), size=2,
            vjust = 2) +
  labs(x = NULL, y = 'Choice preference (%)') +
  scale_color_manual(values = c('not throw'='#4169E1','throw'='#B22222')) +
  scale_shape_manual(values = c('not throw' = 15, 'throw' = 16)) +
  theme_classic() +
  scale_y_continuous(labels = function(x) percent(abs(x)),
                     expand = c(0, 0),
                     limits = c(0, 1),
                     breaks = seq(0, 1, by = 0.25)
  ) +
  guides(color = "none", fill="none", shape="none") +
  ggtitle('GPT-4') +
  theme(
    axis.line = element_line(colour = "black", size = 0.4),
    axis.title = element_text(size = 6, color = "black"),
    axis.text = element_text(size = 6, color = "black"),
    plot.title = element_text(face = 'italic', size = 6,
                              color = "black", vjust = -0.5),
    plot.margin = margin(t = -2)
  )
p5 <- grid.arrange(p5a, p5b, p5c, ncol = 1)
p_stu3 <- grid.arrange(p1, p2, p3, p4, p5, ncol = 5)
ggsave('WithinViso.pdf', plot = p_stu3, width = 7, height = 3)