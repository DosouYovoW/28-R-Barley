theme_nice <- function(){
  theme_bw()+
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        plot.title = element_text(size = 20),
        strip.text = element_text(size = 18))
}

lsd_test_01_mod <- function(data, treatment, outcome) {
  
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- agricolae::LSD.test(anova_test, "treatment", group = TRUE)
  
  dt <- tz|>
    group_by(treatment)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r, fill = treatment)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) +
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1) +
    geom_text(aes(label = paste0(gr)), 
              vjust = -1.2, size =5) +
    theme_bw()
  
  return(result)
}

lsd_test_01 <- function(data, treatment, outcome) {
  
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- agricolae::HSD.test(anova_test, "treatment", group = TRUE)
  
  dt <- tz|>
    group_by(treatment)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r, fill = treatment)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.5) +
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1) +
    geom_text(aes(label = paste0(gr)), 
              hjust = -0.5, vjust = -0.5, size =5) +
    theme_bw()
  
  return(result)
}

lsd_test_01_1 <- function(data, treatment, outcome) {
  
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- agricolae::LSD.test(anova_test, "treatment", group = TRUE)
 # lsd_tes <- agricolae::HSD.test(anova_test, "treatment", group = TRUE)
  
  return(lsd_tes)
}

lsd_test_01_2 <- function(data, treatment, outcome) {
  
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- agricolae::LSD.test(anova_test, "treatment", group = TRUE)
  #lsd_tes <- agricolae::HSD.test(anova_test, "treatment", group = TRUE)
  
  dt <- tz|>
    group_by(treatment)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  return(dt)
}

lsd_test_01_3 <- function(data, treatment, outcome) {
  
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- LSD.test(anova_test, "treatment", group = TRUE)
  
  dt <- tz|>
    group_by(treatment)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  p_values <- lsd_tes#$treatment [, "p adj"]
  #print(p_values)
  
  return(p_values)
}

lsd_test_02_mod <- function(data) {
  tz <- data %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(outcome, group, treatment) %>% 
    drop_na()
  
  anova_test <- aov(outcome ~ treatment*group, tz)
  lsd_tes <- agricolae::HSD.test(anova_test, c("treatment", "group"), group = TRUE)
  
  dt <- tz|>
    group_by(treatment, group)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1,
                  position = position_dodge(width = 0.8)) +
    geom_text(aes(label = paste0(gr)), vjust = -1.2, size =5,
              position = position_dodge(width = 0.8)) +
    theme_bw() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))
  result
  return(result)
}

lsd_test_02 <- function(data) {
  tz <- data %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(outcome, group, treatment) %>% 
    drop_na()
  
  anova_test <- aov(outcome ~ treatment + group, tz)
  lsd_tes <- agricolae::HSD.test(anova_test, c("treatment", "group"), group = TRUE)
  
  dt <- tz|>
    group_by(treatment, group)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1,
                  position = position_dodge(width = 0.8)) +
    geom_text(aes(label = paste0(gr)), hjust = -0.5, vjust = -0.1, size =5,
              position = position_dodge(width = 0.8)) +
    theme_bw() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))
  result
  return(result)
}


lsd_test_03 <- function(data) {
  
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- LSD.test(anova_test, "treatment", group = TRUE)
  
  dt <- tz|>
    group_by(treatment)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r)) +
    geom_point(size = 4)+
    geom_point(data = dt %>% filter(mean_r == max(dt$mean_r)), 
               aes(treatment, mean_r), color = "red",
               size = 4, show.legend = F) +
    geom_smooth(method = "loess", se=F)+
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1) +
    geom_text(aes(label = paste0(gr)), vjust = -2.8, size =4) +
    geom_text_repel(data = dt %>% filter(mean_r == max(dt$mean_r)), 
                    aes(label = paste(treatment, "g/kg seed")), nudge_x = .7,
                    color = "red")+
    theme_bw()+
    theme(legend.position = "none")
  
  return(result)
}

lsd_test_04 <- function(data) {
  tz <- data %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(outcome, group, treatment) %>% 
    drop_na()
  
  anova_test <- aov(outcome ~ treatment*group, tz)
  lsd_tes <- LSD.test(anova_test, c("treatment", "group"), group = TRUE)
  
  dt <- tz|>
    group_by(treatment, group)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    facet_wrap(vars(group), ncol = 2)+
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1,
                  position = position_dodge(width = 0.8)) +
    geom_text(aes(label = paste0(gr,"\n(",round(mean_r,1),")")), vjust = -1, size =6,
              position = position_dodge(width = 0.8)) +
    theme_bw() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))
  result
  return(result)
}

lsd_test_05 <- function(data) {
  tz <- data %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(outcome, group, treatment) %>% 
    drop_na()
  
  anova_test <- aov(outcome ~ treatment*group, tz)
  lsd_tes <- LSD.test(anova_test, c("treatment", "group"), group = TRUE)
  
  dt <- tz|>
    group_by(treatment, group)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    facet_wrap(vars(group), ncol = 5)+
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1,
                  position = position_dodge(width = 0.8)) +
    geom_text(aes(label = paste0(gr,"\n(",round(mean_r,1),")")), vjust = -1, size =6,
              position = position_dodge(width = 0.8)) +
    theme_bw() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))
  result
  return(result)
}

lsd_test_06 <- function(data){
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- LSD.test(anova_test, "treatment", group = TRUE, alpha = 0.05)
 # lsd_tes <- agricolae::HSD.test(anova_test, "treatment", group = TRUE, alpha = 0.05)
  lsd_value <- lsd_tes$statistics$LSD
  aov_fn <- summary(anova_test)

  p_value <- aov_fn[[1]]$`Pr(>F)`[1]
  
  treatment <- c("lsd_value", "p_value")
  mean_r <- c(lsd_value, p_value)
  data_02 <- data.frame(treatment, mean_r)
  
  dt <- tz|>
    group_by(treatment)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), 
              SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups 
  dt <- dt %>% 
  mutate(number = case_when(treatment == "No_PGR" ~ 1,
                            treatment == "M62.5_21-24" ~ 2,
                            treatment == "M62.5_30-32" ~ 3,
                            treatment == "M62.5_37" ~ 4,
                            treatment == "M125_30-32" ~ 5,
                            treatment == "M125_37" ~ 6,
                            treatment == "M62.5_21-24 + M62.5_30-32" ~ 7,
                            treatment == "M62.5_21-24 + M62.5_37" ~ 8,
                            .default = 9)) %>% 
    arrange(number) %>% 
    mutate(mean_se = paste(round(mean_r, 1), "+/-", round(SE_r,1))) %>% 
    bind_rows(data_02)
  
  return(dt)
}

lsd_test_07 <- function(data){
  tz <- data |>
    dplyr::select(treatment, outcome)|>
    drop_na()
  
  anova_test <- aov(outcome ~ treatment, tz)
  lsd_tes <- LSD.test(anova_test, "treatment", group = TRUE, alpha = 0.1)
  lsd_value <- lsd_tes$statistics$LSD
  aov_fn <- summary(anova_test)
  
  p_value <- aov_fn[[1]]$`Pr(>F)`[1]
  
  treatment <- c("lsd_value", "p_value")
  mean_r <- c(lsd_value, p_value)
  data_02 <- data.frame(treatment, mean_r)
  
  dt <- tz|>
    group_by(treatment)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), 
              SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups 
  dt <- dt %>% 
    mutate(number = case_when(treatment == "No_PGR" ~ 1,
                              treatment == "M62.5_21-24" ~ 2,
                              treatment == "M62.5_30-32" ~ 3,
                              treatment == "M62.5_37" ~ 4,
                              treatment == "M125_30-32" ~ 5,
                              treatment == "M125_37" ~ 6,
                              treatment == "M62.5_21-24 + M62.5_30-32" ~ 7,
                              treatment == "M62.5_21-24 + M62.5_37" ~ 8,
                              .default = 9)) %>% 
    arrange(number) %>% 
    mutate(mean_se = paste(round(mean_r, 1), "+/-", round(SE_r,1))) %>% 
    bind_rows(data_02)
  
  return(dt)
}

lsd_test_08 <- function(data) {
  tz <- data %>% 
    mutate(treatment = as.factor(treatment)) %>% 
    dplyr::select(outcome, group1, group2, treatment) %>% 
    drop_na()
  
  anova_test <- aov(outcome ~ treatment*group1*group2, tz)
  lsd_tes <- agricolae::LSD.test(anova_test, c("treatment", "group1", "group2"), group = TRUE)
  
  dt <- tz|>
    group_by(treatment, group1, group2)|>
    summarise(mean_r = mean(outcome, na.rm = TRUE), SE_r = sd(outcome, na.rm = TRUE) / sqrt(n())) |>
    arrange(desc(mean_r))
  
  dt$gr <- lsd_tes$groups$groups
  
  result <- dt |>
    ggplot(aes(treatment, mean_r, color = group1)) +
  #  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    geom_point(position = position_dodge(width = 0.5))+
    geom_line(position = position_dodge(width = 0.5))+
    facet_wrap(vars(group2), ncol = 3)+
    geom_errorbar(aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r), width = 0.1,
                  position = position_dodge(width = 0.5)) +
    geom_text(aes(label = paste0(gr)), vjust = -1, size =2,
              position = position_dodge(width = 0.5)) +
    theme_bw() +
    theme(axis.title = element_text(size = 12),
          axis.text = element_text(size = 12))
  result
  return(result)
}


