# Worm plot for single trial
# worm_plot(n_traj = 30, default_data = TRUE, N_agg = "N_aggregated.out", stock_id = 1)

# Overlapping worm plots
# Run base case for Tier 2 -----------------------------------------------------
write_inits(par_name = "cv_n", par_val = formatC(0.20, format = "f", digits = 3),
            infile = "input.par", outfile = "input.par")
write_inits(par_name = "tier", par_val = formatC(2, format = "d"),
            infile = "input.par", outfile = "input.par")
run_batch(file_names = "input.par")

tier2 = read.table(file = "N_aggregated.out", header = TRUE)

# Run base case for Tier 3 -----------------------------------------------------
write_inits(par_name = "tier", par_val = formatC(3, format = "d"),
            infile = "input.par", outfile = "input.par")
write_inits(par_name = "n_yrs_avg", par_val = formatC(9, format = "d"),
            infile = "input.par", outfile = "input.par")
write_inits(par_name = "lower_tail", par_val = formatC(0.20, format = "f", digits = 3),
            infile = "input.par", outfile = "input.par")
run_batch(file_names = "input.par")
tier3 = read.table(file = "N_aggregated.out", header = TRUE)

# Run base case for Tier 4 -----------------------------------------------------
write_inits(par_name = "tier", par_val = formatC(4, format = "d"),
            infile = "input.par", outfile = "input.par")
write_inits(par_name = "n_yrs_avg", par_val = formatC(99, format = "d"),
            infile = "input.par", outfile = "input.par")
write_inits(par_name = "lower_tail", par_val = formatC(0.20, format = "f", digits = 3),
            infile = "input.par", outfile = "input.par")
tier4 = read.table(file = "N_aggregated.out", header = TRUE)

# Compare Tier 2 and 3 ---------------------------------------------------------
tier2_tmp = tier2 %>% tbl_df(.) %>% mutate(tier = 2) %>% filter(., sim %in% sample(1:pars$n_sims, 25))
tier3_tmp = tier3 %>% tbl_df(.) %>% mutate(tier = 3) %>% filter(., sim %in% sample(1:pars$n_sims, 25))
tier2and3 = rbind(tier2_tmp, tier3_tmp)
tier2and3 %<>% filter(., stock == 1) %>% mutate(stock = as.factor(stock))
tier2and3  %<>% filter(yr > 1) %>% mutate(pbr_yr_sim_rescaled = pbr_yr_sim / pars$KK[1])
tier2and3  %<>% mutate(tier = as.factor(tier), sim = as.factor(sim), tiersim = as.factor(paste(tier,sim,sep="")))
levels(tier2and3$tier)[levels(tier2and3$tier) == "2"] = "Single Estimate"
levels(tier2and3$tier)[levels(tier2and3$tier) == "3"] = "GAMMS Avg."
names(tier2and3)[names(tier2and3) == "tier"] = "Tier"

# ggplot(data = filter(tier2and3, Tier == "Single Estimate"), aes(x = yr, y = pbr_yr_sim_rescaled, group = tiersim)) +
#   geom_line(col = "green") + mytheme_bw + labs(x = "Year", y = "PBR / K") +
#   coord_cartesian(ylim = c(0, max(tier2and3$pbr_yr_sim_rescaled)*1.05)) +
#   theme(legend.position = c(0,0), legend.justification = c(0,0), legend.title=element_blank())

# ggplot(data = filter(tier2and3, Tier == "Single Estimate"), aes(x = yr, y = pbr_yr_sim_rescaled, group = tiersim, colour = Tier)) +
#   geom_line() + mytheme_bw + labs(x = "Year", y = "PBR / K") +
#   scale_color_manual(values = c("green")) +
#   coord_cartesian(ylim = c(0, max(tier2and3$pbr_yr_sim_rescaled)*1.05)) +
#   theme(legend.position = c(0,1), legend.justification = c(0,1), legend.title=element_blank())

ggplot(data = tier2and3, aes(x = yr, y = pbr_yr_sim_rescaled, colour = Tier, group = tiersim, alpha = Tier)) +
  geom_line() + mytheme_bw +
  scale_alpha_manual(values = c(0.8, 0.8)) +
  scale_color_manual(values = c("green", "blue")) +
  # scale_color_manual(values = c("darkgray", "black")) +
  labs(x = "Year", y = "PBR / K") + coord_cartesian(ylim = c(0, max(tier2and3$pbr_yr_sim_rescaled)*1.05)) +
  theme(legend.position = c(0,1), legend.justification = c(0,1), legend.title=element_blank())

    # scale_colour_discrete(name = "method", labels=c("First", "Second"))

# Compare Tier 2 and 4 ---------------------------------------------------------
tier2_tmp = tier2 %>% tbl_df(.) %>% mutate(tier = 2) %>% filter(., sim %in% sample(1:pars$n_sims, 25))
tier4_tmp = tier4 %>% tbl_df(.) %>% mutate(tier = 4) %>% filter(., sim %in% sample(1:pars$n_sims, 25))
tier2and4 = rbind(tier2_tmp, tier4_tmp)
tier2and4 %<>% filter(., stock == 1) %>% mutate(stock = as.factor(stock))
tier2and4  %<>% filter(yr > 1) %>% mutate(pbr_yr_sim_rescaled = pbr_yr_sim / pars$KK[1])
tier2and4  %<>% mutate(tier = as.factor(tier), sim = as.factor(sim), tiersim = as.factor(paste(tier,sim,sep="")))
levels(tier2and4$tier)[levels(tier2and4$tier) == "2"] = "Single Estimate"
levels(tier2and4$tier)[levels(tier2and4$tier) == "4"] = "Data Rich"
names(tier2and4)[names(tier2and4) == "tier"] = "Tier"

ggplot(data = tier2and4, aes(x = yr, y = pbr_yr_sim_rescaled, colour = Tier, group = tiersim, alpha = Tier)) +
  geom_line() + mytheme_bw +
  scale_alpha_manual(values = c(0.8, 0.8)) +
  scale_color_manual(values = c("green", "blue")) +
  # scale_color_manual(values = c("darkgray", "black")) +
  labs(x = "Year", y = "PBR / K") + coord_cartesian(ylim = c(0, max(tier2and3$pbr_yr_sim_rescaled)*1.05)) +
  theme(legend.position = c(0,1), legend.justification = c(0,1), legend.title=element_blank())

