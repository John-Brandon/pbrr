#
# Plot N_min percentile that meets MMPA management objective as a function
#  of the CV of the abundance estimates
#

# Read in data.frame with solutions to Nmin percentile as function of CV_N -----
setwd("~/Documents/2015 work/PBR Tier System/Code/pbrr/R")
snmin = read.csv(file = "solve_Nmin.csv")
snmin %<>% tbl_df() %>% mutate(Tier = as.factor(Tier))

# Plot N_min precentile solution as function of CV_N ---------------------------
plt = ggplot(data = snmin, aes(x = CV, y = Nmin, group = Tier, color = Tier))
plt = plt + geom_line(lwd = 2) + mytheme_bw
# plt
# plt + labs(y = expression(N[min]*" percentile"), x = "CV_N*\n (*includes shrinkage for averaging: Tier 2 & 3)")
plt = plt + labs(y = expression(N[MIN]*" Percentile"), x = expression(CV[N]), title = expression(N[MIN]*" Percentile That Meets Management Objective"))
plt = plt + scale_x_continuous(breaks = seq(0.2, 1.0, by = 0.2))
plt = plt + scale_color_manual(values = c("green", "blue", "orange"))
# plt + theme(legend.position = c(0.8, 0.2))
plt = plt + theme(legend.position = c(0.19, 0.85))
# plt = plt + guides(color = guide_legend(reverse = TRUE))
plt

