#===#===#===#===#===#===#===#===#===#===#
# Start from scratch -- this code should make reproducible examples
# OS: Mac OS 10.10.4
# R version 3.2.0 (2015-04-16)
# Author: John R. Brandon
# Notes:
#  1. `create_file_list` & `file_check` functions defined in PBR_FileIO.R
#===#===#===#===#===#===#===#===#===#===#
library(dplyr)
library(magrittr)
library(ggplot2)
library(readr)
library(stringr)
library(tidyr)   # For function `seperate`: Split trial_id column into two columns with taxa and trial factors

rm(list = ls())  # clear workspace

# -- Fetch parameters from input.par file ---------------------------------------
FORTRAN_code_dir = "~/Documents/2015 Work/PBR Tier System/Code/PBR Netbeans/PBR Netbeans" # You'll need to change this (apologies for hard-coding)
R_code_dir = "~/Documents/2015 Work/PBR Tier System/Code/R Code" # You'll need to change this (apologies for hard-coding)
setwd(R_code_dir)
source(file = "PBR_FileIO.R") # Reads the same input file being used by Fortran code

# -- Create input files for each trial ------------------------------------------
getwd() # Check -- PBR_FileIO.R changes working directory to Fortran Code Directory
file.copy(from = "input.par", to = "input_par_copy.txt", overwrite = TRUE) # copy existing input.par file to temp file
setwd(R_code_dir)
source("PBR_create_input_files.R") # Create input files for each trial (n = 72)
# -- Functions to work with Fortran output: plotting etc ------------------------
setwd(R_code_dir)
source("PBR_fortran_output.R")
# -- Run a batch job ------------------------------------------------------------
setwd(R_code_dir)
source("PBR_batch.R") # Functions for running batches
setwd(FORTRAN_code_dir)

# All files: cetacean and pinniped
# lapply(X = all_file_names, FUN = run_batch, n_sims = 100) # run all trials

# Batch run of all cetacean trials ---------------------------------------------
getwd()
setwd("/Users/johnbrandon/Documents/2015 Work/PBR Tier System/Code/pbrr/R")
source("PBR_create_input_files.R") # Create input files for each trial (n = 72)
lapply(X = cet_file_names, FUN = run_batch, n_sims = 100) # run all trials

# lapply(X = pin_file_names, FUN = run_batch, n_sims = 100) # run all trials

# -- Compile depletion statistics -----------------------------------------------
#df_depl = compile_depl_stats(file_names = all_out_names, stock_id = 1, lower_percentile = 0.05, upper_percentile = 0.95)  # compile depletion statistics
# df_depl = compile_depl_stats(file_names = pin_out_names, stock_id = 1, lower_percentile = 0.05, upper_percentile = 0.95)  # compile depletion statistics
df_depl = compile_depl_stats(file_names = cet_out_names, stock_id = 1, lower_percentile = 0.05, upper_percentile = 0.95)  # compile depletion statistics

# Create some additional columns which are trial factors
df_depl = tbl_df(df_depl)
df_depl %<>% mutate(taxa = substr(trial_id, start = 1, stop = 3),
                    trial_n = substr(trial_id, start = 5, stop = 5),
                    trial_l = substr(trial_id, start = 6, stop = 6),
                    CV_N = ifelse(trial_l %in% c("A", "C"), "a_low", "b_high"))
df_depl_cet = df_depl %>% filter(taxa == "Cet")
df_depl_cet_ab = df_depl_cet %>% filter(trial_l == "A" | trial_l == "B")
df_depl_cet_cd = df_depl_cet %>% filter(trial_l == "C" | trial_l == "D")
df_depl_pin = df_depl %>% filter(taxa == "Pin")
df_depl_pin_ab = df_depl_pin %>% filter(trial_l == "A" | trial_l == "B")
df_depl_pin_cd = df_depl_pin %>% filter(trial_l == "C" | trial_l == "D")

trial_names = list( # expressions for facet labels in zeh plots
  'a_low' = expression(CV[N]*" = 0.20"),
  'b_high' = expression(CV[N]*" = 0.80")
)

trial_labeller = function(variable, value){ # wrapper func labels in zeh plots
  return(trial_names[value])
}

# -- Plot depletion across trials (Zeh plots), c.f. Wade (1998) Fig 7.
tmp_cet_plt = ggplot(data = df_depl_cet_cd, aes(x = trial_n, y = median)) + geom_point() + coord_cartesian(ylim = c(0,1.1))
tmp_cet_plt +
  facet_grid(CV_N ~ ., labeller = trial_labeller) + geom_errorbar(aes(ymin = lower_p, ymax = upper_p), width = 0.25) +
  geom_point(data = df_depl_cet_ab, aes(x = trial_n, y = lower_p), shape = 6) +
  geom_segment(aes(x = 0, y = 0.5, xend = 7.5, yend = 0.5), size = 1, colour = "red", linetype = "dashed") +
  geom_segment(aes(x = 7.5, y = 0.45, xend = 8.5, yend = 0.45), size = 1, colour = "red", linetype = "dashed") +
  geom_segment(aes(x = 8.5, y = 0.70, xend = 9.5, yend = 0.70), size = 1, colour = "red", linetype = "dashed") +
  xlab("Trial Number") + ylab("Depletion") + ylim(0, 1.5) +
  ggtitle(expression("Cetacean Depletion After 100 Years: "*F[R]*" = 0.50")) + mytheme_bw

tmp_pin_plt = ggplot(data = df_depl_pin_cd, aes(x = trial_n, y = median)) + geom_point() + coord_cartesian(ylim = c(0., 1.1))
tmp_pin_plt +
  geom_point(data = df_depl_pin_ab, aes(x = trial_n, y = lower_p), shape = 6) +
  facet_grid(CV_N ~ ., labeller = trial_labeller) + geom_errorbar(aes(ymin = lower_p, ymax = upper_p), width = 0.25) +
  geom_segment(aes(x = 0, y = 0.5, xend = 7.5, yend = 0.5), size = 1, colour = "red", linetype = "dashed") +
  geom_segment(aes(x = 7.5, y = 0.45, xend = 8.5, yend = 0.45), size = 1, colour = "red", linetype = "dashed") +
  geom_segment(aes(x = 8.5, y = 0.70, xend = 9.5, yend = 0.70), size = 1, colour = "red", linetype = "dashed") +
  xlab("Trial Number") + ylab("Depletion") + ggtitle(expression("Pinniped Depletion After 100 Years: "*F[R]*" = 0.50")) + mytheme_bw

# -- Depletion vs. N_min percentile plots ---------------------------------------
setwd(FORTRAN_code_dir)
# todo: call write inits editing the 'tier' parameter
depl_matrix_cet0a_tier2 = batch_nmin(base_case_file = "Cet_0A.txt")
plot_depl_nmin(depl_matrix_cet0a_tier2, expression("Cetacean: "*CV[N]*" = 0.20"))
# todo: call write inits editing the 'tier' parameter
depl_matrix_cet0a_tier3 = batch_nmin(base_case_file = "Cet_0A.txt")
plot_depl_nmin(depl_matrix_cet0a_tier3, expression("Cetacean: "*CV[N]*" = 0.20"))

depl_matrix_cet0a_tier4 = batch_nmin(base_case_file = "Cet_0A.txt")
plot_depl_nmin(depl_matrix_cet0a_tier4, expression("Cetacean: "*CV[N]*" = 0.20"))

plot_depl_nmin(depl_matrix_cet0a, expression("Cetacean: "*CV[N]*" = 0.20"))
depl_matrix_cet0b = batch_nmin(base_case_file = "Cet_0B.txt")
plot_depl_nmin(depl_matrix_cet0b, title = expression("Cetacean: "*CV[N]*" = 0.80"))
depl_matrix_pin0a = batch_nmin(base_case_file = "Pin_0A.txt")
plot_depl_nmin(depl_matrix_pin0a, title = expression("Pinniped: "*CV[N]*" = 0.20"))
depl_matrix_pin0b = batch_nmin(base_case_file = "Pin_0B.txt")
plot_depl_nmin(depl_matrix_pin0b, title = expression("Pinniped: "*CV[N]*" = 0.80"))

# Check out some spaghetti trajectory plots ------------------------------------
# View(df_depl_pin_cd) # upper_p for trial 8 over 100% of K
setwd(R_code_dir)
source("PBR_fortran_output.R")
run_batch(file_names = "Cet_3C.txt", n_sims = 100)
run_batch(file_names = "input.par", n_sims = 100)
ribbon_depletion_plot(n_traj = 30)
system("./main")
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_0A.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_0B.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_0C.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_0D.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_3A.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_3B.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_3C.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_3D.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Pin_8D.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_8D.out", stock_id = 1)
ribbon_depletion_plot(n_traj = 30, default_data = FALSE, N_agg = "N_agg_Cet_7A.out", stock_id = 1)

# ------------------------------------------------------------------------------
# -- Plot inter-survey variation in PBR to compare standard tier (1) with another tier
plot_prb_variation(default_data = FALSE, N_agg = "N_agg_Cet_0A.out", stock_id = 1)

# See the `Worm_plots_overlapping.R` script for PBR through time worm plot code

# ------------------------------------------------------------------------------
# Working on debugging biased R_max trial
file.copy(from = "input.par", to = "input_par_copy.txt", overwrite = TRUE) # boiler-plate input.par provided
file.copy(from = "Cet_3C.txt", to = "input.par", overwrite = TRUE)
foo = calc_n_min(n_best = 3933.3842, cv_n = 0.20, z_score = 0.842)
0.02 * 0.5 * foo

file.copy(from = "input_par_copy.txt", to = "input.par", overwrite = TRUE) # boiler-plate input.par provided

# ------------------------------------------------------------------------------
# Example plots from two stock simulation
write_inits(par_name = "n_stocks", par_val = formatC(2, format = "d"), # Set to two stocks
            infile = "input.par", outfile = "input.par")
setwd(R_code_dir)
source("PBR_create_input_files.R") # Create input files for each trial (n = 72)
system("./main") # Run two stock scenario
lapply(X = cet_file_names, FUN = run_batch, n_sims = 100) # run cetacean trials
# plot_2stock_depl(read_data = FALSE, N_agg = "N_agg_Cet_0B.out") # first simulation
plot_2stock_depl(read_data = FALSE, N_agg = "N_aggregated.out") # first simulation

write_inits(par_name = "n_stocks", par_val = formatC(1, format = "d"),
            infile = "input.par", outfile = "input.par")
