# Plot contribution to deviance explained, Fig 3
# JGM

library (tidyverse)


# CSV of individual variable contribution to deviance explained. Made this table in Step1_Fit_GAMs. Focusing on the difference between dev explained in the full model and dev explained in a model with each variable dropped. That column is called "Diff_f"
var_imp <- read_csv ("Models/var_imp_sp_Rug_tw_LL.csv")


var_imp %>%
  distinct() %>%
  mutate (Var = case_when (
    Var == "s(tow_depth_begin)" ~ "tow depth",
    Var == "s(bottom_temp)" ~ "BT",
    Var == "s(surface_temp)" ~ "SST",
    TRUE ~ as.character (Var)
  )) %>%
  ggplot (aes (x = reorder(Var, Diff_f, median), y = Diff_f)) +
  geom_boxplot () +
  geom_jitter (size = 0.75) +
  geom_hline (yintercept = 0, col = "red", lty = 2) +
  coord_flip () +
  theme_bw() +
  labs (x = "", y = "% deviance explained, full model - covariate dropped model") +
  theme (
    axis.text = element_text (size = 8),
    axis.title = element_text (size = 10)

  )

ggsave ("Figures/Var_imp_boxplot.eps", width = 85, height = 127.5, units = "mm", dpi = 300)
