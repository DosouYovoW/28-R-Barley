lsd_test_f2_01 <- function(data, treatment, outcome) {
  # Prepare data
  tz <- data |>
    dplyr::select({{treatment}}, {{outcome}}) |>
    drop_na()
  
  # Rename for formula
  colnames(tz) <- c("treatment", "outcome")
  
  # Run ANOVA
  anova_test <- aov(outcome ~ treatment, data = tz)
  p_val <- summary(anova_test)[[1]]$`Pr(>F)`[1]
  
  if (p_val > 0.05) {
    # If not significant, return fake LSD.test object with "a" for all
    means <- tz %>%
      group_by(treatment) %>%
      summarise(mean = mean(outcome), SE = sd(outcome) / sqrt(n()), .groups = "drop") %>%
      mutate(groups = "a")
    
    # Mimic the structure of agricolae::LSD.test output
    result <- list(
      statistics = list(LSD = NA, p.value = p_val),
      groups = means %>% rename(mean_r = mean, SE_r = SE)
    )
    return(result)
  } else {
    # If significant, run LSD test
    lsd_tes <- agricolae::LSD.test(anova_test, "treatment", group = TRUE)
    return(lsd_tes)
  }
}


lsd_test_f2_02 <- function(data, treatment, outcome) {
  
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

lsd_test_f2_03 <- function(data, treatment, outcome) {
  
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


lsd_test_f2_04 <- function(data){
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


lsd_test_f2_05 <- function(data) {
  tz <- data |>
    dplyr::select(treatment, outcome) |>
    drop_na()
  
  # Run ANOVA
  anova_test <- aov(outcome ~ treatment, tz)
  aov_fn <- summary(anova_test)
  p_value <- aov_fn[[1]]$`Pr(>F)`[1]
  
  # Group means and SE
  dt <- tz |>
    group_by(treatment) |>
    summarise(
      mean_r = mean(outcome, na.rm = TRUE),
      SE_r = sd(outcome, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    ) |>
    arrange(desc(mean_r))
  
  if (is.na(p_value) || p_value > 0.05) {
    # Non-significant: assign all "a"
    dt$gr <- "a"
    lsd_value <- NA
  } else {
    # Significant: do LSD test at alpha = 0.1
    lsd_tes <- agricolae::LSD.test(anova_test, "treatment", group = TRUE, alpha = 0.05)
    lsd_value <- lsd_tes$statistics$LSD
    
    # Join grouping from LSD.test
    grouping_df <- lsd_tes$groups %>%
      rownames_to_column("treatment") %>%
      select(treatment, groups) %>%
      rename(gr = groups)
    
    dt <- left_join(dt, grouping_df, by = "treatment")
  }
  
  # Assign numeric ordering
  dt <- dt %>%
    mutate(number = case_when(
      treatment == "No_PGR" ~ 1,
      treatment == "M62.5_21-24" ~ 2,
      treatment == "M62.5_30-32" ~ 3,
      treatment == "M62.5_37" ~ 4,
      treatment == "M125_30-32" ~ 5,
      treatment == "M125_37" ~ 6,
      treatment == "M62.5_21-24 + M62.5_30-32" ~ 7,
      treatment == "M62.5_21-24 + M62.5_37" ~ 8,
      .default = 9
    )) %>%
    arrange(number) %>%
    mutate(mean_se = paste(round(mean_r, 1), "+/-", round(SE_r, 1)))
  
  # Add LSD and p-value as footer
  data_02 <- data.frame(
    treatment = c("lsd_value", "p_value"),
    mean_r = c(lsd_value, p_value),
    SE_r = NA,
    gr = NA,
    number = NA,
    mean_se = NA
  )
  
  dt <- bind_rows(dt, data_02)
  
  return(dt)
}







lsd_test_09_hsd <- function(data) {
  tz <- data %>%
    mutate(
      treatment = as.factor(treatment),
      group = as.factor(group),
      interaction = interaction(treatment, group)
    ) %>%
    select(outcome, treatment, group, interaction) %>%
    drop_na()
  
  # One-way ANOVA on interaction
  anova_test <- aov(outcome ~ interaction, data = tz)
  
  # HSD test on the interaction
  hsd_tes <- agricolae::HSD.test(anova_test, "interaction", group = TRUE)
  
  # Summary stats
  dt <- tz %>%
    group_by(treatment, group) %>%
    summarise(
      mean_r = mean(outcome, na.rm = TRUE),
      SE_r = sd(outcome, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Extract groups and merge
  grouping_df <- hsd_tes$groups %>%
    rownames_to_column("interaction") %>%
    separate(interaction, into = c("treatment", "group"), sep = "\\.") %>%
    mutate(across(everything(), as.character)) %>%
    rename(gr = groups)
  
  dt <- dt %>%
    left_join(grouping_df, by = c("treatment", "group")) %>%
    arrange(desc(mean_r))
  
  # Plot
  result <- dt %>%
    ggplot(aes(x = treatment, y = mean_r, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
    geom_errorbar(
      aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r),
      width = 0.1,
      position = position_dodge(width = 0.8)
    ) +
    geom_text(
      aes(label = gr),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 5
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12)
    )
  
  return(result)
}

lsd_test_09_lsd <- function(data) {
  tz <- data %>%
    mutate(
      treatment = as.factor(treatment),
      group = as.factor(group),
      interaction = interaction(treatment, group)
    ) %>%
    select(outcome, treatment, group, interaction) %>%
    drop_na()
  
  # One-way ANOVA on the interaction term
  anova_test <- aov(outcome ~ interaction, data = tz)
  p_value <- summary(anova_test)[[1]]$`Pr(>F)`[1]  # Extract F-test p-value
  
  # Summary stats
  dt <- tz %>%
    group_by(treatment, group) %>%
    summarise(
      mean_r = mean(outcome, na.rm = TRUE),
      SE_r = sd(outcome, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  # Apply LSD.test only if significant
  if (!is.na(p_value) && p_value <= 0.05) {
    lsd_tes <- agricolae::LSD.test(anova_test, "interaction", group = TRUE)
    
    # Process grouping from LSD result
    grouping_df <- lsd_tes$groups %>%
      rownames_to_column("interaction") %>%
      separate(interaction, into = c("treatment", "group"), sep = "\\.") %>%
      mutate(across(everything(), as.character)) %>%
      rename(gr = groups)
    
    dt <- dt %>%
      left_join(grouping_df, by = c("treatment", "group"))
  } else {
    dt$gr <- "a"
  }
  
  # Sort and plot
  dt <- dt %>% arrange(desc(mean_r))
  
  result <- dt %>%
    ggplot(aes(x = treatment, y = mean_r, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.5) +
    geom_errorbar(
      aes(ymin = mean_r - SE_r, ymax = mean_r + SE_r),
      width = 0.1,
      position = position_dodge(width = 0.8)
    ) +
    geom_text(
      aes(label = gr),
      position = position_dodge(width = 0.8),
      vjust = -0.5,
      size = 5
    ) +
    theme_bw() +
    theme(
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12)
    )
  
  return(result)
}

lsd_test_09_lsd_table <- function(data) {
  # Prepare data and interaction
  tz <- data %>%
    mutate(
      treatment = as.factor(treatment),
      group = as.factor(group),
      interaction = interaction(treatment, group)
    ) %>%
    select(outcome, treatment, group, interaction) %>%
    drop_na()
  
  # Run one-way ANOVA on interaction
  anova_test <- aov(outcome ~ interaction, data = tz)
  p_value <- summary(anova_test)[[1]]$`Pr(>F)`[1]
  
  # Group means and SE
  dt <- tz %>%
    group_by(treatment, group) %>%
    summarise(
      mean_r = mean(outcome, na.rm = TRUE),
      SE_r = sd(outcome, na.rm = TRUE) / sqrt(n()),
      .groups = "drop"
    )
  
  if (!is.na(p_value) && p_value <= 0.05) {
    lsd_tes <- agricolae::LSD.test(anova_test, "interaction", group = TRUE, alpha = 0.05)
    
    # Safely extract LSD value
    lsd_value <- if (!is.null(lsd_tes$statistics) && "LSD" %in% names(lsd_tes$statistics)) {
      lsd_tes$statistics$LSD
    } else {
      NA
    }
    
    # Process group letters
    grouping_df <- lsd_tes$groups %>%
      rownames_to_column("interaction") %>%
      tidyr::separate(interaction, into = c("treatment", "group"), sep = "\\.") %>%
      mutate(across(everything(), as.character)) %>%
      rename(gr = groups)
    
    # Merge and add LSD/p-value
    dt <- dt %>%
      left_join(grouping_df, by = c("treatment", "group")) %>%
      mutate(
        p_value = round(p_value, 4),
        LSD_value = round(lsd_value, 3)
      )
  } else {
    dt <- dt %>%
      mutate(
        gr = "a",
        p_value = round(p_value, 4),
        LSD_value = NA
      )
  }
  
  return(dt)
}
