# Debugging what look like some inconsistencies in the tier 2 (averaging) runs (n_bias = 2.0)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(magrittr)
file.copy(from = "input.par", to = "input.par.copy", overwrite = TRUE)

# Run a batch with alternate lower percentile for N_min
write_inits(par_name = "lower_tail", par_val = formatC(0.30, format = "f", digits = 3), infile = "input.par", outfile = "input.par")

# Edit all files
setwd(R_code_dir)
source("PBR_create_input_files.R") # Create input files for each trial (n = 72)
setwd(FORTRAN_code_dir)
lapply(X = all_file_names, FUN = run_batch, n_sims = 100) # run base case trials

### ----------------------------------------------------------------------------
# # Checking
# calc_n_min(n_best = 3000, cv_n = c(0.2, 0.8), z_score = abs(qnorm(0.20)))
# calc_n_min(n_best = 3000, cv_n = c(0.2, 0.8), z_score = abs(qnorm(0.30)))
# 0.02 * calc_n_min(n_best = 3000, cv_n = c(0.2, 0.8), z_score = abs(qnorm(0.20)))
# 0.02 * calc_n_min(n_best = 3000, cv_n = c(0.2, 0.8), z_score = abs(qnorm(0.30)))

# Edit template input.par file to correspond with Tier 3 MSE (NMFS Weighted arithmetic average)
write_inits(par_name = "tier", par_val = formatC(2, format = "d"), infile = "input.par", outfile = "input.par")
write_inits(par_name = "n_yrs_avg", par_val = formatC(1, format = "d"), infile = "input.par", outfile = "input.par")
# write_inits(par_name = "S_adult", par_val = formatC(0.98, format = "f", digits = 3), infile = "input.par", outfile = "input.par")

# Edit remaining files to be Tier 3, using input.par as boiler-plate
# setwd(R_code_dir)
# source("PBR_create_input_files.R") # Create input files for each trial (n = 72)

# Run base case trials
# substr(all_file_names, start = 5, stop = 6) %>% subset(. == "0A" | . == "0B")
# base_case_trials = which(substr(all_file_names, start = 5, stop = 6) %in% c("0A", "0B"))
# base_case_trials = all_file_names[base_case_trials]; base_case_trials
# setwd(FORTRAN_code_dir)
# lapply(X = base_case_trials, FUN = run_batch, n_sims = 100) # run base case trials

getwd()
# Batch N_min runs for base case trials c.f. Wade (1998) Fig. 4 ----------------
library(dplyr) # note: Package MASS has select function that can mask dplyr::select and cause errors
library(magrittr)
# Edit template input.par file to correspond with Tier 3 MSE (NMFS Weighted arithmetic average)
tier = 2      # tier 2 = standard PBR; tier 3 = weighted arithmetic average (NMFS GAMMS); tier 4 = weighted by time and precision (e.g. SC/65b/AWMP/04)
n_yrs_avg = 1 # number of years to average over for data-rich tiers
a_rec = 0     # age at recruitment to human caused mortality
cv_nn = 1.0
write_inits(par_name = "tier", par_val = formatC(tier, format = "d"), infile = "input.par", outfile = "input.par")
write_inits(par_name = "n_yrs_avg", par_val = formatC(n_yrs_avg, format = "d"), infile = "input.par", outfile = "input.par")
write_inits(par_name = "a_r", par_val = formatC(a_rec, format = "d"), infile = "input.par", outfile = "input.par")
write_inits(par_name = "cv_n ", par_val = formatC(cv_nn, format = "f", digits = 2), infile = "input.par", outfile = "input.par")
write_inits(par_name = "cv_n_true", par_val = formatC(cv_nn, format = "f", digits = 2), infile = "input.par", outfile = "input.par")

depl_Cet0A = batch_nmin(base_case_file = "input.par") # matrix depl with Nmin percentile in column
plot_depl_nmin(depl_Cet0A, title = paste("Tier : ", tier, " (a_r = ", a_rec, "; n_yrs_avg = ", n_yrs_avg, ")", sep = ""))

depl_Cet0A = batch_nmin(base_case_file = "Cet_0A.txt") # matrix depl with Nmin percentile in column
plot_depl_nmin(depl_Cet0A, title = "Cet_0A")

View(batch_nmin)

depl_Cet0B = batch_nmin(base_case_file = "Cet_0B.txt")
plot_depl_nmin(depl_Cet0B, title = "Cet_0B")

depl_Pin0A = batch_nmin(base_case_file = "Pin_0A.txt")
plot_depl_nmin(depl_Pin0A, title = "Pin_0A")

depl_Pin0B = batch_nmin(base_case_file = "Pin_0B.txt")
plot_depl_nmin(depl_Pin0B, title = "Pin_0B")

# Try munging and ggplotting in Hadley-verse
library(tidyr) # For function `gather`
depl_df <- depl_Cet0A %>% gather(Nmin, depl) %>% group_by(Nmin) %>%
  summarise(lower = quantile(depl, 0.05), median = median(depl), upper = quantile(depl, 0.95)) %>%
  gather(., key = quantile, value = value, -Nmin)

ggplot(data = depl_df, aes(x = Nmin, y = value, group = quantile)) +
  # geom_ribbon(aes(ymin = filter(depl_df, quantile == "lower")$value, ymax = filter(depl_df, quantile == "upper")$value, alpha = 0.20))
  geom_line() + mytheme_bw +
  geom_hline(yintercept = 0.50, color = "red", linetype = 2, size = 1.25) +
  labs(x = expression(italic(N[MIN])*" Percentile"), y = "Final Depletion") +
  geom_point(data = filter(depl_df, quantile == "median"), aes(x = Nmin, y = value), size = 1.5)
# geom_line(aes(x = Nmin, y = value, group = Nmin))

# Batch N_min runs for Tier 3 (weighted average approach of NMFS 2005)
#  But have a look at 8 yr survey interval
depl_Cet6A = batch_nmin(base_case_file = "Cet_6A.txt")
plot_depl_nmin(depl_Cet6A, title = "Cet_6A")

depl_Cet6B = batch_nmin(base_case_file = "Cet_6B.txt")
plot_depl_nmin(depl_Cet6B, title = "Cet_6B")

depl_Pin6A = batch_nmin(base_case_file = "Pin_6A.txt")
plot_depl_nmin(depl_Pin6A, title = "Pin_6A")

depl_Pin6B = batch_nmin(base_case_file = "Pin_6B.txt")
plot_depl_nmin(depl_Pin6B, title = "Pin_6B")

### ----------------------------------------------------------------------------
# This approach works for creating batch plots for checking
# plots_survey = lapply(all_out_names, survey_plot_agg, read_data = FALSE, sim_n = 1, stock_id = 1)
# ggsave("surv_agg_tmp.pdf", do.call(marrangeGrob, c(plots_survey, list(nrow = 2, ncol = 2))))
plots_spaghetti = lapply(all_out_names, ribbon_depletion_plot, n_traj = 30, default_data = FALSE, stock_id = 1)
ggsave("tier_3_plots_spaghetti.pdf", scale = 1.5, do.call(marrangeGrob, c(plots_spaghetti[1:8], list(nrow = 2, ncol = 2))))
# system("open plots_spaghetti.pdf")
system("open tier_3_plots_spaghetti.pdf")

pdf(file = "plots_spaghetti.pdf", plots_spaghetti[1:4])

is.list(plots_spaghetti)
length(plots_spaghetti)

### ----------------------------------------------------------------------------
file.copy(from = "input.par.copy", to = "input.par", overwrite = TRUE)

