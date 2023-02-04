


install.packages(c(
  "DeclareDesign",
  "fabricatr",
  "randomizr",
  "estimatr",
  "DesignLibrary"
))


library(DesignLibrary)
library(DeclareDesign)
library(randomizr)
library(estimatr)
library(fabricatr)

#Control:
Control_sample_1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.04,0.01,0.03,0.02,0.01,0.01)
#Treatment:
Treat_sample_1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.01,0.0428499,0.03,0.03,0,0.02,0.01)



output <- two_arm_designer(
  N = 1700,
  assignment_prob = 0.5,
  control_mean = mean(Control_sample_1),
  control_sd = sd(Control_sample_1),
  ate = sd(Control_sample_1)*0.1,
  treatment_mean = mean(Control_sample_1) + sd(Control_sample_1)*0.1,
  treatment_sd = sd(Treat_sample_1),
  rho = 1,
  args_to_fix = NULL
)

output


