suppressMessages(require(dplyr))

sample_names_data <- c("jane", "jane", "madison", "madison")
sample_years_ssa  <- c(rep(c(1930, 2010), 2))
sample_years_ipums  <- c(rep(c(1830, 1880), 2))

sample_names_df <- data_frame(names = sample_names_data,
                              years = sample_years_ssa)

sample_names_df_big <- bind_rows(sample_names_df, sample_names_df)
