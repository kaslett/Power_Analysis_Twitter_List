##############################################################################
# File-Name: code_power_analysis_FCing_Twitter.r
# Date: 10-01-2022
# author: Tiago Ventura and Kevin Aslett
# Purpose: Main script to run power analysis and estimate sample size for the
#          WhatsApp Experiments in Brazil.
# Data in: source_graph.r -- r code with parameters for costumization of the plots
# Data out: sims_power.Rdata
#           a data frame with simulations for power analysis
#           and all the designs from declare design used to run the simulations
# output: power_no_covariate.png - power results without covariates
#         power_covariate.png  - power results with covariates (low correlation)
# status:
# Machine: MacOS High Sierra
##############################################################################


# Call packages -----------------------------------------------------------
#install.packages("pacman") # package management
pacman::p_load(here, DeclareDesign, tidyverse, fabricatr, wesanderson, tidyr, patchwork)



# Declare Experimental Design ---------------------------------------------

# Declare Designs allows one to simulate research designs in code and
# simulate them in order to understand their properties. Our interest here
# is to calculate the sample size for our experiment given a particular 
# set of parameters -- most important are confidence levels and effect size. 

# the example below is inspired from the declaredesing library for a two arm experiment
# link: https://declaredesign.org/r/designlibrary/articles/two_arm.html

# This is also another design I took as a reference
# https://book.declaredesign.org/experimental-causal.html

## Step 1: Define the Model ------------------------------------------------------------------
# Set parameters for you population
population <- declare_population(N = N, # sample size
                                 draw_multivariate(c(X, u_0) ~ mvrnorm(
                                   n = N,
                                   mu = c(control_mean, control_mean),
                                   Sigma = matrix(c(1, rho2, rho2 , 1), 2, 2)
                                 )), # error term for the control, and some X we observe with error
                                 u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(1 - rho^2))) # error term treatment groups
# notice here the errror terms are correlated allowing us also to model 
# the contributtions of covariates for the precision of the treatment effects.

# Define potential outcomes
potential_outcomes <- declare_potential_outcomes(Y ~ (1 - Z) * (u_0 * control_sd + control_mean) + Z * (u_1 * treatment_sd + 
                                                                                                          treatment_mean))


# Step 2: Define Inquiry --------------------------------------------------
## Define which estimand you are looking for. 
estimand <- declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0))


# Step 3: Data strategy ---------------------------------------------------
# declare your research design interventions. 
# here we are working with a complete randomization assignment. 

assignment <- declare_assignment(Z = complete_ra(N, prob = assignment_prob))
reveal_Y <- declare_reveal()


# Step 4: Declare your estimator ------------------------------------------
estimator_no_cov <- declare_estimator(Y ~ Z, method=lm, inquiry = estimand, label="no_covariate" )
estimator_cov <- declare_estimator(Y ~ Z + X, metho=lm, inquiry = estimand, label="with_covariate" )

# Write a function with the design ------------------------------------------------------

# Functon to combine all the steps from the declared design

two_arm_design <- function(
  N, # sample size
  assignment_prob, # share in the treatment
  control_mean, # mean in the control group
  control_sd, # sd in the control group
  treatment_mean, #mean in the treatment group
  treatment_sd, # sd in the treatment
  rho, # correlation between error terms 
  rho2) # correlation between covariates and error term)
{
  # start the declare design here
  # basically repeating all the steps from above
  model <- declare_population(N = N, 
                              draw_multivariate(c(X, u_0) ~ MASS::mvrnorm(
                                n = N,
                                mu = c(control_mean, control_mean),
                                Sigma = matrix(c(1, rho2, rho2 , 1), 2, 2)
                              )), 
                              u_1 = rnorm(n = N, mean = rho * u_0, sd = sqrt(1 - rho^2))) + 
    declare_potential_outcomes(Y ~ (1 - Z) * (u_0 * control_sd + control_mean) + Z * (u_1 * treatment_sd + 
                                                                                        treatment_mean)) +
    declare_inquiry(ATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_assignment(Z = complete_ra(N, prob = assignment_prob)) +
    declare_reveal() +
    declare_estimator(Y ~ Z, 
                      .method = lm_robust,
                      .summary = tidy, 
                      inquiry = estimand, 
                      label="no_covariate" ) +
    declare_estimator(Y ~ Z + X,
                      .method = lm_robust,
                      .summary = tidy, 
                      inquiry = estimand, 
                      label="with_covariate" )
  
  
}

# Define a set of parameters to run a trial

diagnosis <- diagnose_design(two_arm_design(N= 1000,
                                            assignment_prob = 0.5,
                                            control_mean = 0.9,
                                            control_sd = 0.16,
                                            treatment_mean = 0.902,
                                            treatment_sd = 0.16,
                                            rho = 1,
                                            rho2 = 0.5), sims = 100)

# check results
diagnosis %>% 
  reshape_diagnosis(select = "Power")


# Simulate many results ---------------------------------------------------

# Alcott et al (2020) will be considered for the power estimation of this paper. 

# All the results of the paper are outcome presented with 
# variables Y normalized so that the Control group standard deviation equals 1. 
# Therefore, we can adopt pretty much directly the model above declared. 

# See here link for the appendix: https://assets.aeaweb.org/asset-server/files/11625.pdf
# Table 10 and Table 11 present the standardized results. 

# See below some closely related to our covariates

# Fake news Knowledge: -0.06
# News Knowledge: -0.12
# Party Affective Polarization: -0.06
# Issue Polarization: -0.10
# Polarization index: -0.16
# News Knowledge index: -0.19
# subjective well-being index: 0.09

# For Asimovic et al

# Fake News Knowledge: -  0.27
# Outgroup Polarization: 






##########################################################

############### Discernment ##############################

##########################################################


#Numbers based off of Jeffrey Bowles Experiment:

#Grid with sample size and effect size
#Number of those sampled in the population from 100 to 2000:
#Effect Size is predicted at 0.025 to 0.2

grid <- expand_grid(n=seq(300, 2500, by =100),
                    eff=seq(0.025, 0.4, by =0.025))

# Define quantities I want to analyze in the simulations
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)

# run the simulations (row-wise operations)
grid <- grid %>% 
  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
                                             assignment_prob = 0.5,
                                             control_mean = 0,
                                             control_sd = 1,
                                             treatment_mean = .y,
                                             treatment_sd = 1,
                                             rho = 1,
                                             rho2 = .5)), # declare the mode
         diagnosis=map(model, ~ diagnose_design(.x, 
                                                diagnosands=diagnosands,
                                                sims=100,#500
                                                bootstrap_sims = 100)), # run simulation 500
         res=map(diagnosis, "diagnosands_df")) # get results


# Unnest to a single dataframe
# load(here("output", "sims_power.Rdata"))

res <- grid %>% 
  unnest(res) %>%
  ungroup() %>%
  dplyr::select(-model, -design)

save(res, file="sims_power.Rdata")

# Cleaning the variables

res<- res  %>%
  mutate(N_fct=as.factor(n),
         eff=as.factor(eff), 
         power=ifelse(power>0.79, "Power > 80%", "Power < 80%")) %>%
  mutate(estimator=str_to_title(str_replace_all(estimator, "_", " "))) %>%
  dplyr::select(N_fct, eff, estimator, power)



# Simulate many results ---------------------------------------------------
source("code", "source_graph.r")
pal <- wes_palette("Zissou1", n=5)

#load(file=here("output", "sims_power.Rdata"))

# no covariate adjustment
ggplot(res %>% 
         filter(estimator=="No Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.1, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=10)) 

ggsave("Discernment_sims_no_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")


# with covariate adjustment

ggplot(res %>% 
         filter(estimator=="With Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.1, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=22)) 

ggsave("Discernment_sims_with_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")








####################################################################################################################

############################## Proportion of Fact-Checking Organizations ###########################################

####################################################################################################################

#Grid with sample size and effect size
#Number of those sampled in the population from 100 to 2500:
#Effect Size is predicted at 0.025 to 0.2

grid <- expand_grid(n=seq(300, 2500, by =100),
                    eff=seq(0.002, 0.02, by =0.002))

# Define quantities I want to analyze in the simulations
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)

# run the simulations (row-wise operations)

grid <- grid %>% 
  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
                                             assignment_prob = 0.5,
                                             control_mean = 0,
                                             control_sd = 0.03,
                                             treatment_mean = .y,
                                             treatment_sd = 0.04,
                                             rho = 1,
                                             rho2 = .5)), # declare the mode
         diagnosis=map(model, ~ diagnose_design(.x, 
                                                diagnosands=diagnosands,
                                                sims=100,#500
                                                bootstrap_sims = 100)), # run simulation 500
         res=map(diagnosis, "diagnosands_df")) # get results


# Unnest to a single dataframe
# load(here("output", "sims_power.Rdata"))

res <- grid %>% 
  unnest(res) %>%
  ungroup() %>%
  dplyr::select(-model, -design)

save(res, file="sims_power.Rdata")

# Cleaning the variables

res<- res  %>%
  mutate(N_fct=as.factor(n),
         eff=as.factor(eff), 
         power=ifelse(power>0.79, "Power > 80%", "Power < 80%")) %>%
  mutate(estimator=str_to_title(str_replace_all(estimator, "_", " "))) %>%
  dplyr::select(N_fct, eff, estimator, power)



# Simulate many results ---------------------------------------------------
source("code", "source_graph.r")
pal <- wes_palette("Zissou1", n=5)

#load(file=here("output", "sims_power.Rdata"))

# no covariate adjustment
ggplot(res %>% 
         filter(estimator=="No Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=10)) 

ggsave("Prop_Fcer_followers_sims_no_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")


# with covariate adjustment

ggplot(res %>% 
         filter(estimator=="With Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=22)) 

ggsave("Prop_Fcer_followers_sims_with_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")





####################################################################################################################

############################## Average News Quality Shared on Twitter #############################################

####################################################################################################################

#Grid with sample size and effect size
#Number of those sampled in the population from 100 to 2500:
#Effect Size is predicted at 0.025 to 0.2

#grid <- expand_grid(n=seq(100, 500, by =100),
#                    eff=seq(0.001, 0.010, by =0.001))
grid <- expand_grid(n=seq(300, 2500, by =100),
                    eff=seq(0.002, 0.02, by =0.002))


# Define quantities I want to analyze in the simulations
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)

# run the simulations (row-wise operations)
#grid <- grid %>% 
#  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
#                                             assignment_prob = 0.5,
#                                             control_mean = 0,
#                                             control_sd = 0.10,
#                                             treatment_mean = .y,
#                                             treatment_sd = 0.10,
#                                             rho = 1,
#                                             rho2 = .5)), # declare the mode
#         diagnosis=map(model, ~ diagnose_design(.x, 
#                                                diagnosands=diagnosands,
#                                                sims=100,#500
#                                                bootstrap_sims = 100)), # run simulation 500
#         res=map(diagnosis, "diagnosands_df")) # get results






grid <- grid %>% 
  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
                                             assignment_prob = 0.5,
                                             control_mean = 0,
                                             control_sd = 0.12,
                                             treatment_mean = .y,
                                             treatment_sd = 0.12,
                                             rho = 1,
                                             rho2 = .5)), # declare the mode
         diagnosis=map(model, ~ diagnose_design(.x, 
                                                diagnosands=diagnosands,
                                                sims=100,#500
                                                bootstrap_sims = 100)), # run simulation 500
         res=map(diagnosis, "diagnosands_df")) # get results



# Unnest to a single dataframe
# load(here("output", "sims_power.Rdata"))

res <- grid %>% 
  unnest(res) %>%
  ungroup() %>%
  dplyr::select(-model, -design)

save(res, file="sims_power.Rdata")

# Cleaning the variables

res<- res  %>%
  mutate(N_fct=as.factor(n),
         eff=as.factor(eff), 
         power=ifelse(power>0.79, "Power > 80%", "Power < 80%")) %>%
  mutate(estimator=str_to_title(str_replace_all(estimator, "_", " "))) %>%
  dplyr::select(N_fct, eff, estimator, power)



# Simulate many results ---------------------------------------------------
source("code", "source_graph.r")
pal <- wes_palette("Zissou1", n=5)

#load(file=here("output", "sims_power.Rdata"))

# no covariate adjustment
ggplot(res %>% 
         filter(estimator=="No Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.007, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=10)) 

ggsave("News_Quality_Shared_sims_no_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")


# with covariate adjustment

ggplot(res %>% 
         filter(estimator=="With Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.007, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=22)) 

ggsave("News_Quality_Shared_sims_with_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")








####################################################################################################################

############################## Mainstream Trust and Affective Polarization #############################################

####################################################################################################################

#Grid with sample size and effect size
#Number of those sampled in the population from 100 to 2500:
#Effect Size is predicted at 0.025 to 0.2

#grid <- expand_grid(n=seq(100, 500, by =100),
#                    eff=seq(0.001, 0.010, by =0.001))
grid <- expand_grid(n=seq(300, 2500, by =100),
                    eff=seq(0.002, 0.02, by =0.002))


# Define quantities I want to analyze in the simulations
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)

# run the simulations (row-wise operations)
#grid <- grid %>% 
#  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
#                                             assignment_prob = 0.5,
#                                             control_mean = 0,
#                                             control_sd = 0.10,
#                                             treatment_mean = .y,
#                                             treatment_sd = 0.10,
#                                             rho = 1,
#                                             rho2 = .5)), # declare the mode
#         diagnosis=map(model, ~ diagnose_design(.x, 
#                                                diagnosands=diagnosands,
#                                                sims=100,#500
#                                                bootstrap_sims = 100)), # run simulation 500
#         res=map(diagnosis, "diagnosands_df")) # get results






grid <- grid %>% 
  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
                                             assignment_prob = 0.5,
                                             control_mean = 0,
                                             control_sd = 0.3,
                                             treatment_mean = .y,
                                             treatment_sd = 0.3,
                                             rho = 1,
                                             rho2 = .5)), # declare the mode
         diagnosis=map(model, ~ diagnose_design(.x, 
                                                diagnosands=diagnosands,
                                                sims=100,#500
                                                bootstrap_sims = 100)), # run simulation 500
         res=map(diagnosis, "diagnosands_df")) # get results



# Unnest to a single dataframe
# load(here("output", "sims_power.Rdata"))

res <- grid %>% 
  unnest(res) %>%
  ungroup() %>%
  dplyr::select(-model, -design)

save(res, file="sims_power.Rdata")

# Cleaning the variables

res<- res  %>%
  mutate(N_fct=as.factor(n),
         eff=as.factor(eff), 
         power=ifelse(power>0.79, "Power > 80%", "Power < 80%")) %>%
  mutate(estimator=str_to_title(str_replace_all(estimator, "_", " "))) %>%
  dplyr::select(N_fct, eff, estimator, power)



# Simulate many results ---------------------------------------------------
source("code", "source_graph.r")
pal <- wes_palette("Zissou1", n=5)

#load(file=here("output", "sims_power.Rdata"))

# no covariate adjustment
ggplot(res %>% 
         filter(estimator=="No Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.007, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=10)) 

ggsave("Mstrm_Trust_Shared_sims_no_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")


# with covariate adjustment

ggplot(res %>% 
         filter(estimator=="With Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.007, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=22)) 

ggsave("Mstrm_Trust_sims_with_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")



####################################################################################################################

############################################### Political Cynicism #################################################

####################################################################################################################

#Grid with sample size and effect size
#Number of those sampled in the population from 100 to 2500:
#Effect Size is predicted at 0.025 to 0.2

#grid <- expand_grid(n=seq(100, 500, by =100),
#                    eff=seq(0.001, 0.010, by =0.001))
grid <- expand_grid(n=seq(300, 2500, by =100),
                    eff=seq(0.002, 0.02, by =0.002))


# Define quantities I want to analyze in the simulations
diagnosands <- declare_diagnosands(
  bias = mean(estimate - estimand),
  rmse = sqrt(mean((estimate - estimand)^2)),
  power = mean(p.value <= 0.05)
)

# run the simulations (row-wise operations)
#grid <- grid %>% 
#  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
#                                             assignment_prob = 0.5,
#                                             control_mean = 0,
#                                             control_sd = 0.10,
#                                             treatment_mean = .y,
#                                             treatment_sd = 0.10,
#                                             rho = 1,
#                                             rho2 = .5)), # declare the mode
#         diagnosis=map(model, ~ diagnose_design(.x, 
#                                                diagnosands=diagnosands,
#                                                sims=100,#500
#                                                bootstrap_sims = 100)), # run simulation 500
#         res=map(diagnosis, "diagnosands_df")) # get results






grid <- grid %>% 
  mutate(model=map2(n, eff, ~ two_arm_design(N = .x, 
                                             assignment_prob = 0.5,
                                             control_mean = 0,
                                             control_sd = 0.15,
                                             treatment_mean = .y,
                                             treatment_sd = 0.15,
                                             rho = 1,
                                             rho2 = .5)), # declare the mode
         diagnosis=map(model, ~ diagnose_design(.x, 
                                                diagnosands=diagnosands,
                                                sims=100,#500
                                                bootstrap_sims = 100)), # run simulation 500
         res=map(diagnosis, "diagnosands_df")) # get results



# Unnest to a single dataframe
# load(here("output", "sims_power.Rdata"))

res <- grid %>% 
  unnest(res) %>%
  ungroup() %>%
  dplyr::select(-model, -design)

save(res, file="sims_power.Rdata")

# Cleaning the variables

res<- res  %>%
  mutate(N_fct=as.factor(n),
         eff=as.factor(eff), 
         power=ifelse(power>0.79, "Power > 80%", "Power < 80%")) %>%
  mutate(estimator=str_to_title(str_replace_all(estimator, "_", " "))) %>%
  dplyr::select(N_fct, eff, estimator, power)



# Simulate many results ---------------------------------------------------
source("code", "source_graph.r")
pal <- wes_palette("Zissou1", n=5)

#load(file=here("output", "sims_power.Rdata"))

# no covariate adjustment
ggplot(res %>% 
         filter(estimator=="No Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.007, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=10)) 

ggsave("Pol_Cynicism_Shared_sims_no_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")


# with covariate adjustment

ggplot(res %>% 
         filter(estimator=="With Covariate"), 
       aes(x=N_fct,y=eff, 
           fill=fct_rev(power)))+
  geom_tile(colour="gray95",size=0.5, alpha=.8)  +
  guides(fill=guide_legend(title="Power Results"))+ 
  labs(x="Number of Observations",
       y="") +
  scale_fill_manual(values=c(pal[1], pal[5]))  +
  facet_grid(~ estimator) +
  geom_hline(yintercept=0.007, linetype="dashed", 
             color = "red", size=2) +
  theme(axis.text.x = element_text(angle=45, hjust=1, size=10), 
        strip.text = element_text(color = "#22211d",
                                  size = 14, face="italic"), 
        plot.caption = element_text(size=22)) 

ggsave("Pol_Cynicism_sims_with_covariate.png",  width = 14, height = 8, units = "in", pointsize = 12, bg = "white")




