lsd_test_f2_01 <- function(data, alpha = 0.05) {
  
  tz <- data %>%
    dplyr::select(treatment, outcome) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(
      treatment = as.character(treatment),
      outcome   = as.numeric(outcome)
    )
  
  # Guardrails
  if (dplyr::n_distinct(tz$treatment) < 2 || nrow(tz) < 2) {
    return(tibble::tibble(
      treatment = character(),
      mean = numeric(),
      SE = numeric(),
      groups = character(),
      p.value = NA_real_,
      LSD = NA_real_
    ))
  }
  
  anova_test <- aov(outcome ~ treatment, data = tz)
  p_val <- summary(anova_test)[[1]][["Pr(>F)"]][1]
  
  # Always compute means/SE the same way
  means_tbl <- tz %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(
      mean = mean(outcome, na.rm = TRUE),
      SE   = sd(outcome, na.rm = TRUE) / sqrt(dplyr::n()),
      .groups = "drop"
    )
  
  # If not significant: all "a"
  if (is.na(p_val) || p_val > alpha) {
    return(means_tbl %>%
             dplyr::mutate(
               groups  = "a",
               p.value = p_val,
               LSD     = NA_real_
             ))
  }
  
  # If significant: use agricolae groups, but return a tibble
  lsd <- agricolae::LSD.test(anova_test, "treatment", group = TRUE)
  
  groups_tbl <- tibble::as_tibble(lsd$groups, rownames = "treatment") %>%
    dplyr::transmute(
      treatment = as.character(treatment),
      groups    = as.character(groups)
    )
  
  means_tbl %>%
    dplyr::left_join(groups_tbl, by = "treatment") %>%
    dplyr::mutate(
      p.value = p_val,
      LSD     = lsd$statistics$LSD
    )
}
