# without max bmi, by sex, cycle A
pdf("../description/shared_figs/bmiTrajCycleA_0.pdf", width = 7, height = 5)
par(mfrow=c(1,2), mar=c(3.5, 4, 0, 1))
set.seed(114)
plot_bmi_traj_f(n_a=5, sex_a = "m", deb_type_a = c("bmi"), verbose_a = F )
plot_bmi_traj_f(n_a=5, sex_a = "f", deb_type_a = c("bmi"), verbose_a = F )
mtext("BMI", side = 2, line = -1.1, outer = T, at=0.56, cex=1.2)

pdf("../description/shared_figs/bmiTrajCycleA_1.pdf", width = 7, height = 5)
par(mfrow=c(1,2), mar=c(3.5, 4, 0, 1))
set.seed(114)
plot_bmi_traj_f(n_a=5, sex_a = "m", deb_type_a = c("bmi", "bmi_deb1"), verbose_a = F )
plot_bmi_traj_f(n_a=5, sex_a = "f", deb_type_a = c("bmi", "bmi_deb1"), verbose_a = F, add_legend_a = T)
mtext("BMI", side = 2, line = -1.1, outer = T, at=0.56, cex=1.2)

pdf("../description/shared_figs/bmiTrajCycleA_2.pdf", width = 7, height = 5)
par(mfrow=c(1,2), mar=c(3.5, 4, 0, 1))
set.seed(114)
plot_bmi_traj_f(n_a=5, sex_a = "m", deb_type_a = c("bmi", "bmi_deb2") )
plot_bmi_traj_f(n_a=5, sex_a = "f", deb_type_a = c("bmi", "bmi_deb2"), add_legend_a = T)
mtext("BMI", side = 2, line = -1.1, outer = T, at=0.56, cex=1.2)

graphics.off()
rm(plot_bmi_traj_f)
