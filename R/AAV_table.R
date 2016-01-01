# Code to compile numbers for table comparing Absolute Annual Variation (AAV) statistic
# -- Create input files for each trial ------------------------------------------
getwd() # Check -- PBR_FileIO.R changes working directory to Fortran Code Directory
file.copy(from = "input.par", to = "input_par_copy.txt", overwrite = TRUE) # copy existing input.par file to temp file
write_inits(par_name = "tier", par_val = formatC(2, format = "d"),
            infile = "input.par", outfile = "input.par") # Make sure set to Tier 2 (Standard)
setwd(R_code_dir)
source("PBR_create_input_files.R") # Create input files for each trial (n = 72)

# Run Cetacean Base Case
lapply(X = cet_file_names, FUN = run_batch, n_sims = 100) # run all trials

# run_batch(file_names = "Cet_0A.txt")
pars = read_inits(input_file = "Cet_0A.txt")
N_agg = read.table("N_agg_Cet_1A.out", header = TRUE, stringsAsFactors = FALSE)
N_agg = read.table("N_agg_Cet_2A.out", header = TRUE, stringsAsFactors = FALSE)
N_agg = read.table("N_agg_Cet_3A.out", header = TRUE, stringsAsFactors = FALSE)
N_agg = read.table("N_agg_Cet_4A.out", header = TRUE, stringsAsFactors = FALSE)
N_agg = read.table("N_agg_Cet_5A.out", header = TRUE, stringsAsFactors = FALSE)
N_agg = read.table("N_agg_Cet_6A.out", header = TRUE, stringsAsFactors = FALSE)
N_agg = read.table("N_agg_Cet_7A.out", header = TRUE, stringsAsFactors = FALSE)
N_agg = read.table("N_agg_Cet_8A.out", header = TRUE, stringsAsFactors = FALSE)
cet_out_names = substr(cet_file_names, 1, 6)
cet_out_names = paste(cet_out_names, ".out", sep="")
cet_out_names = cet_out_names[(which(substr(cet_out_names,6,6)=="C"))]
for (ii in 1:length(cet_out_names)){
  file_name = paste("N_agg_", cet_out_names[ii], sep = "")
  N_agg = read.table(file_name, header = TRUE, stringsAsFactors = FALSE)
  N_agg = tbl_df(N_agg)
  N_agg_sims = N_agg %>% filter(., sim > 0, stock == 1) # exclude the reference set
  final_depl = N_agg_sims %>% filter(., yr == 100) %>% select(., depl_yr_stock) %>% arrange(., depl_yr_stock)
  lower_5th[ii] = final_depl[5,1]; lower_5th
  lower_5th[ii] = lower_5th[ii] / 0.50

  N_agg = N_agg %>% filter(., stock == 1, n_hat_yr > 0, sim > 0) # only years with abundance estimates / updated PBR
  pbr_df = N_agg %>% mutate(
    pbr_naive = 0.5 * pars$R_max * pars$F_r[1] * calc_n_min(n_best = n_hat_yr, cv_n = pars$cv_n[1], z_score = qnorm(pars$lower_tail))
  )
  pbr_df = pbr_df %>% group_by(., sim) %>% mutate(pbr_naive_dif = pbr_naive - lag(pbr_naive), pbr_yr_dif = pbr_yr_sim - lag(pbr_yr_sim))

  aav_naive[ii] = sum(abs(pbr_df$pbr_naive_dif), na.rm = TRUE) / sum(pbr_df$pbr_naive, na.rm = TRUE); aav_naive
  aav_tier[ii] = sum(abs(pbr_df$pbr_yr_dif), na.rm = TRUE) / sum(pbr_df$pbr_yr_sim, na.rm = TRUE); aav_tier
}

max(aav_tier) / max(aav_naive)
min(lower_5th)

N_agg = tbl_df(N_agg)
# unique(N_agg$yr) # check
# Get lower 5th percentile
N_agg$stock = as.factor(N_agg$stock) # Convert stock ID number into factor
# Start extracting relevant data
N_agg_sims = N_agg %>% filter(., sim > 0, stock == 1) # exclude the reference set
final_depl = N_agg_sims %>% filter(., yr == 100) %>% select(., depl_yr_stock) %>% arrange(., depl_yr_stock)
lower_5th = final_depl[5,1]; lower_5th
lower_5th / 0.5

N_agg = N_agg %>% filter(., stock == 1, n_hat_yr > 0, sim > 0) # only years with abundance estimates / updated PBR
pbr_df = N_agg %>% mutate(
  pbr_naive = 0.5 * pars$R_max * pars$F_r[1] * calc_n_min(n_best = n_hat_yr, cv_n = pars$cv_n[1], z_score = qnorm(pars$lower_tail))
)
pbr_df = pbr_df %>% group_by(., sim) %>% mutate(pbr_naive_dif = pbr_naive - lag(pbr_naive), pbr_yr_dif = pbr_yr_sim - lag(pbr_yr_sim))

aav_naive = sum(abs(pbr_df$pbr_naive_dif), na.rm = TRUE) / sum(pbr_df$pbr_naive, na.rm = TRUE); aav_naive
aav_tier = sum(abs(pbr_df$pbr_yr_dif), na.rm = TRUE) / sum(pbr_df$pbr_yr_sim, na.rm = TRUE); aav_tier

aav_tier / aav_naive

pbr_df = pbr_df %>% select(sim, pbr_naive_dif, pbr_yr_dif)
View(pbr_df)
pbr_df = pbr_df %>% gather(tier, dif, -sim) # gather reshapes data.frame in preperation for ggplot

pbr_df = pbr_df %>% filter(., !is.na(dif)) %>% mutate(tier = as.factor(tier)) #, dif = abs(dif)
pbr_df = pbr_df %>% mutate(., pbr_over_k = dif / pars$KK[1], aav = abs(pbr_over_k))

# run all cetacean trials ------------------------------------------------------
# lapply(X = cet_file_names, FUN = run_batch, n_sims = 100)
