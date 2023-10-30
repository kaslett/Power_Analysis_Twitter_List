##############################################################################
# File-Name: 01_power_analysis.r
# Date: 2023
# author: Kevin Aslett
# Purpose: analysis with bootstrapping data
# Data in: "./data/all_data_processed_processed.csv" csv format
# Machine: MacOS Monterrey
##############################################################################

#Load in libraries:
library(tidyverse)
library(fabricatr)
library(randomizr)
library(estimatr)
library(DesignLibrary)

#Read in treatment data:
data <- read_csv('./data/all_data_processed_processed.csv')

#create binary treatment variable:
data$Z <- ifelse(data$exp == 'treatment',1,0)

#create outcome variable (affective polarization)

data_t <- data %>% filter(exp == 'treatment')
data_c <- data %>% filter(exp == 'control')

#Set size of population:
num_population = 10000

#Set size of sample:
num_sample = 1000

#Set cohen's d
cohen_d = 0.2

variable = data_c$w3_affective_polarization


######### Functions ##########

create_pvalues_pos <- function(variable,
                             cohen_d,
                             num_population,
                             num_sample,
                             max_num){
  
  sd_y_0_hat = sd(variable[data_c$Z == 0],na.rm=T)
  
  mde_hat_full_study = cohen_d*sd_y_0_hat
  
  design <- declare_model(N = num_population,
                          U = sample(variable,num_population,replace=T),
                          potential_outcomes(Y ~ mde_hat_full_study * Z + U)) +
    declare_inquiry(PATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_sampling(S = complete_rs(N, n = num_sample), legacy = FALSE) +
    declare_assignment(Z = complete_ra(N, prob = 0.5), legacy = FALSE) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z),
                        Y = ifelse(Y > max_num,max_num,Y)) +
    declare_estimator(Y ~ Z)
  
  draw_estimates(design)
}

create_pvalues_neg <- function(variable,
                               cohen_d,
                               num_population,
                               num_sample,
                               min_num){
  
  sd_y_0_hat = sd(variable[data_c$Z == 0],na.rm=T)
  
  mde_hat_full_study = cohen_d*sd_y_0_hat
  
  design <- declare_model(N = num_population,
                          U = sample(variable,num_population,replace=T),
                          potential_outcomes(Y ~ - mde_hat_full_study * Z + U)) +
    declare_inquiry(PATE = mean(Y_Z_1 - Y_Z_0)) +
    declare_sampling(S = complete_rs(N, n = num_sample), legacy = FALSE) +
    declare_assignment(Z = complete_ra(N, prob = 0.5), legacy = FALSE) +
    declare_measurement(Y = reveal_outcomes(Y ~ Z),
                        Y = ifelse(Y < min_num,min_num,Y)) +
    declare_estimator(Y ~ Z)
  
  draw_estimates(design)
}

columns = c("d_variable","sample_size","power") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns


######################################################################

####################### H1: Truth Discernment ########################

######################################################################

for(num_sample in seq(from = 100, to = 2000, by = 100)){
  for(x in 1:2000){
    if(x == 1){
      row_data  <- create_pvalues_pos(data_c$w3_belief_accuracy,
                                    cohen_d,
                                    num_population,
                                    num_sample,
                                    10) 
    }
    if(x > 1){
      new_row <- create_pvalues_pos(data_c$w3_belief_accuracy,
                                  cohen_d,
                                  num_population,
                                  num_sample,
                                  10) 
      
      row_data<- rbind(row_data,new_row)
    }
  }
  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  df <- rbind(df,c('Truth Discernment',num_sample,mean(row_data$stat_sig95)))
}




######################################################################

####################### H2: News Media Literacy ######################

######################################################################

for(num_sample in seq(from = 100, to = 2000, by = 100)){
  
  for(x in 1:2000){
    if(x == 1){
      row_data  <- create_pvalues_pos(data_c$w3_misinfo_literacy_zcore,
                                    cohen_d,
                                    num_population,
                                    num_sample,
                                    12) 
    }
    if(x > 1){
      new_row <- create_pvalues_pos(data_c$w3_misinfo_literacy_zcore,
                                  cohen_d,
                                  num_population,
                                  num_sample,
                                  12) 
      
      row_data<- rbind(row_data,new_row)
      
    }
    
  }
  
  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  df <- rbind(df,c('News Media Literacy',num_sample,mean(row_data$stat_sig95)))
}



######################################################################

####################### H3: Conspiracy Belief ########################

######################################################################


for(num_sample in seq(from = 100, to = 2000, by = 100)){
  
  for(x in 1:2000){
    if(x == 1){
      row_data  <- create_pvalues_neg(data_c$w3_cons_zcore,
                                    cohen_d,
                                    num_population,
                                    num_sample,
                                    -8) 
    }
    if(x > 1){
      new_row <- create_pvalues_neg(data_c$w3_cons_zcore,
                                  cohen_d,
                                  num_population,
                                  num_sample,
                                  -8) 
      
      row_data<- rbind(row_data,new_row)
    }
  }
  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  df <- rbind(df,c('Conspiracy Belief',num_sample,mean(row_data$stat_sig95)))
}

######################################################################

####################### H4: Aff. Polarization Party ######################

######################################################################




#for(num_sample in seq(from = 100, to = 2000, by = 100)){
  
#  for(x in 1:2000){
#    if(x == 1){
#      row_data  <- create_pvalues_neg(data_c$w3_affective_polarization,
#                                    cohen_d,
#                                    num_population,
#                                    num_sample,
#                                   0) 
#    }
#    if(x > 1){
#      new_row <- create_pvalues_neg(data_c$w3_affective_polarization,
#                                  cohen_d,
#                                  num_population,
#                                  num_sample,
#                                  0) 
      
#      row_data<- rbind(row_data,new_row)
      
#    }
    
#  }
  
#  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  
#  mean(row_data$stat_sig95)
  
  
#  df <- rbind(df,c('Affective Polarization (Party)',num_sample,mean(row_data$stat_sig95)))
#}



######################################################################

  ####################### H5: Media Trust ########################

######################################################################

for(num_sample in seq(from = 100, to = 2000, by = 100)){
  
  for(x in 1:2000){
    if(x == 1){
      row_data  <- create_pvalues_neg(data_c$w3_ext_media_score,
                                    cohen_d,
                                    num_population,
                                    num_sample,
                                    0) 
    }
    if(x > 1){
      new_row <- create_pvalues_neg(data_c$w3_ext_media_score,
                                  cohen_d,
                                  num_population,
                                  num_sample,
                                  0) 
      
      row_data<- rbind(row_data,new_row)
      
    }
    
  }
  
  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  df <- rbind(df,c('Media Trust',num_sample,mean(row_data$stat_sig95)))
}



######################################################################

####################### H6: Political Cynicism ########################

######################################################################

for(num_sample in seq(from = 100, to = 2000, by = 100)){
  
  for(x in 1:2000){
    if(x == 1){
      row_data  <- create_pvalues_pos(data_c$w3_polcyn_zcore,
                                    cohen_d,
                                    num_population,
                                    num_sample,
                                    6) 
    }
    if(x > 1){
      new_row <- create_pvalues_pos(data_c$w3_polcyn_zcore,
                                  cohen_d,
                                  num_population,
                                  num_sample,
                                  6) 
      
      row_data<- rbind(row_data,new_row)
      
    }
    
  }
  
  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  df <- rbind(df,c('Political Cynicism',num_sample,mean(row_data$stat_sig95)))
}


######################################################################

  ####################### H7: SM Cynicism ########################

######################################################################

for(num_sample in seq(from = 100, to = 2000, by = 100)){
  for(x in 1:2000){
    if(x == 1){
      row_data  <- create_pvalues_pos(data_c$w3_sm_cyn_zcore,
                                    cohen_d,
                                    num_population,
                                    num_sample,
                                    9) 
    }
    if(x > 1){
      new_row <- create_pvalues_pos(data_c$w3_sm_cyn_zcore,
                                  cohen_d,
                                  num_population,
                                  num_sample,
                                  9) 
      
      row_data<- rbind(row_data,new_row)
      
    }
    
  }
  
  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  df <- rbind(df,c('Social Media Cynicism',num_sample,mean(row_data$stat_sig95)))
}


colnames(df) <- c('Dependent_Variable','Sample_Size','Power')

df$Dependent_Variable <- as.character(df$Dependent_Variable)

df$Sample_Size <- as.character(df$Sample_Size)
df$Sample_Size <- as.numeric(df$Sample_Size)

df$Power <- as.character(df$Power)
df$Power <- as.numeric(df$Power)

df_fig_1 <- df


ggplot(df_fig_1,aes(y=Power,x=Sample_Size,group=Dependent_Variable)) +
  geom_line(size=1.5,aes(color=Dependent_Variable),alpha=0.8 ) +
  theme_classic() + 
  geom_hline(yintercept=0.95, linetype="dashed", 
             color = "black", size=2) +
  ylab('Power') +
  xlab('Sample Size') +
  ggtitle('Power by Sample Size for an Effect Size of 0.2 (Cohen\'s D)') +
  scale_color_discrete(name = "Dependent Variable")

ggsave('./figures/Power_Analysis_95.pdf',width = 14, height = 8)


######################################################################

######################## Minimum Detectable Effect ###################

######################################################################



######################################################################

####################### H1: Truth Discernment ########################

######################################################################

cohen_list_minus_1 = c(NA,1,0.95,0.9,0.85,0.8,0.75,0.7,0.65,0.6,0.55,0.5,0.475,0.45,0.425,
               0.4,0.375,0.35,0.325,0.3,0.29,0.28,0.27,0.26,0.25,0.24,0.23,0.22,0.21,0.2,
               0.19,0.18,0.17,0.16,0.15,0.14,0.13,0.12,0.11)

cohen_list = c(1,0.95,0.9,0.85,0.8,0.75,0.7,0.65,0.6,0.55,0.5,0.475,0.45,0.425,
               0.4,0.375,0.35,0.325,0.3,0.29,0.28,0.27,0.26,0.25,0.24,0.23,0.22,0.21,0.2,
               0.19,0.18,0.17,0.16,0.15,0.14,0.13,0.12,0.11,0.1)

columns = c("d_variable","sample_size","mde") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

stop_num = 0


num_sample = 200

set_num = 1
for(num_sample in seq(from = 200, to = 2000, by = 100)){
  stop_num = 0
  for(y in set_num:length(cohen_list)){
    cohen_d = cohen_list[y]
    cohen_d_2 = cohen_list_minus_1[y]
    if(stop_num == 0){
  for(x in 1:2000){
    if(x == 1){
      row_data  <- create_pvalues_pos(data_c$w3_belief_accuracy,
                                      cohen_d,
                                      num_population,
                                      num_sample,
                                      10) 
    }
    if(x > 1){
      new_row <- create_pvalues_pos(data_c$w3_belief_accuracy,
                                    cohen_d,
                                    num_population,
                                    num_sample,
                                    10) 
      
      row_data<- rbind(row_data,new_row)
    }
  }
  set_num = y
  row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
  print(paste0('Sample: ',as.character(num_sample),', MDE: ',as.character(cohen_d),', Power: ',as.character(mean(row_data$stat_sig95))))
  if(mean(row_data$stat_sig95) < 0.95){
  df <- rbind(df,c('Truth Discernment',num_sample,cohen_d_2))
  stop_num = 1
  }
    }
  }
}


df_1 <- df

colnames(df_1) = c("d_variable","sample_size","mde") 

df_1$mde <- as.numeric(df_1$mde)
df_1$sample_size <- as.numeric(df_1$sample_size)


coefficient = 0.17

ggplot(df_1,aes(y=mde,x=sample_size)) +
  geom_line(size=0.5) +
  geom_hline(yintercept=coefficient, linetype="dashed", color = "red") +
  theme_bw() + 
  ylab('Minimum Detectable Effect Size\n') +
  xlab('\nSample Size') +
  ylim(0,1.0) +
  scale_color_discrete(name = "Cohen's D") +
  geom_text(aes(0,coefficient,label = coefficient, vjust = -1,color='red'),size=3) +
  theme(legend.position="none")

ggsave('./figures/mde_truth_discernment_95.pdf',width = 14, height = 8,units='cm')


######################################################################

####################### H2: News Media Literacy ######################

######################################################################

columns = c("d_variable","sample_size","mde") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

stop_num = 0

set_num = 1


for(num_sample in seq(from = 200, to = 2000, by = 100)){
  stop_num = 0
  for(y in set_num:length(cohen_list)){
    cohen_d = cohen_list[y]
    cohen_d_2 = cohen_list_minus_1[y]
    if(stop_num == 0){
      for(x in 1:2000){
        if(x == 1){
          row_data  <- create_pvalues_pos(data_c$w3_misinfo_literacy_zcore,
                                          cohen_d,
                                          num_population,
                                          num_sample,
                                          12) 
        }
        if(x > 1){
          new_row <- create_pvalues_pos(data_c$w3_misinfo_literacy_zcore,
                                        cohen_d,
                                        num_population,
                                        num_sample,
                                        12) 
          row_data<- rbind(row_data,new_row)
        }
      }
      set_num = y
      row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
      print(paste0('Sample: ',as.character(num_sample),', MDE: ',as.character(cohen_d),', Power: ',as.character(mean(row_data$stat_sig95))))
      if(mean(row_data$stat_sig95) < 0.95){
        df <- rbind(df,c('News Media Literacy',num_sample,cohen_d_2))
        stop_num = 1
      }
    }
  }
}

df_2 <- df

colnames(df_2) = c("d_variable","sample_size","mde") 

df_2$mde <- as.numeric(df_2$mde)
df_2$sample_size <- as.numeric(df_2$sample_size)


coefficient = 0.25

ggplot(df_2,aes(y=mde,x=sample_size)) +
  geom_line(size=0.5) +
  geom_hline(yintercept=coefficient, linetype="dashed", color = "red") +
  theme_bw() + 
  ylab('Minimum Detectable Effect Size\n') +
  xlab('\nSample Size') +
  ylim(0,1.0) +
  scale_color_discrete(name = "Cohen's D") +
  geom_text(aes(0,coefficient,label = coefficient, vjust = -1,color='red'),size=3) +
  theme(legend.position="none")

ggsave('./figures/mde_media_literacy_95.pdf',width = 14, height = 8,units='cm')


######################################################################

####################### H3: Conspiracy Belief ########################

######################################################################

columns = c("d_variable","sample_size","mde") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

stop_num = 0

set_num = 1
for(num_sample in seq(from = 200, to = 2000, by = 100)){
  stop_num = 0
  for(y in set_num:length(cohen_list)){
    cohen_d = cohen_list[y]
    cohen_d_2 = cohen_list_minus_1[y]
    if(stop_num == 0){
      for(x in 1:2000){
        if(x == 1){
          row_data  <- create_pvalues_neg(data_c$w3_cons_zcore,
                                          cohen_d,
                                          num_population,
                                          num_sample,
                                          -8) 
        }
        if(x > 1){
          new_row <- create_pvalues_neg(data_c$w3_cons_zcore,
                                        cohen_d,
                                        num_population,
                                        num_sample,
                                        -8) 
          
          row_data<- rbind(row_data,new_row)
        }
      }
      set_num = y
      row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
      print(paste0('Sample: ',as.character(num_sample),', MDE: ',as.character(cohen_d),', Power: ',as.character(mean(row_data$stat_sig95))))
      if(mean(row_data$stat_sig95) < 0.95){
        df <- rbind(df,c('Conspiracy Belief',num_sample,cohen_d_2))
        stop_num = 1
      }
    }
  }
}


df_3 <- df

colnames(df_3) = c("d_variable","sample_size","mde") 

df_3$mde <- as.numeric(df_3$mde)
df_3$sample_size <- as.numeric(df_3$sample_size)

coefficient = 0.09

ggplot(df_3,aes(y=mde,x=sample_size)) +
  geom_line(size=0.5) +
  geom_hline(yintercept=coefficient, linetype="dashed", color = "red") +
  theme_bw() + 
  ylab('Minimum Detectable Effect Size\n') +
  xlab('\nSample Size') +
  ylim(0,1.0) +
  scale_color_discrete(name = "Cohen's D") +
  geom_text(aes(0,coefficient,label = coefficient, vjust = -1,color='red'),size=3) +
  theme(legend.position="none")

ggsave('./figures/mde_conspiracy_belief_95.pdf',width = 14, height = 8,units='cm')


######################################################################

####################### H4: Aff. Polarization Party ######################

######################################################################


columns = c("d_variable","sample_size","mde") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

stop_num = 0

set_num = 1
for(num_sample in seq(from = 200, to = 2000, by = 100)){
  stop_num = 0
  for(y in set_num:length(cohen_list)){
    cohen_d = cohen_list[y]
    cohen_d_2 = cohen_list_minus_1[y]
    if(stop_num == 0){
      for(x in 1:2000){
        if(x == 1){
          row_data  <- create_pvalues_neg(data_c$w3_affective_polarization,
                                          cohen_d,
                                          num_population,
                                          num_sample,
                                          0) 
        }
        if(x > 1){
          new_row <- create_pvalues_neg(data_c$w3_affective_polarization,
                                        cohen_d,
                                        num_population,
                                        num_sample,
                                        0) 
          
          row_data<- rbind(row_data,new_row)
          
        }
        
      }
      set_num = y
      row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
      print(paste0('Sample: ',as.character(num_sample),', MDE: ',as.character(cohen_d),', Power: ',as.character(mean(row_data$stat_sig95))))
      if(mean(row_data$stat_sig95) < 0.95){
        df <- rbind(df,c('Affective Polarization (Party)',num_sample,cohen_d_2))
        stop_num = 1
      }
    }
  }
}



df_4 <- df

colnames(df_4) = c("d_variable","sample_size","mde") 

df_4$mde <- as.numeric(df_4$mde)
df_4$sample_size <- as.numeric(df_4$sample_size)




coefficient = 0.17

ggplot(df_4,aes(y=mde,x=sample_size)) +
  geom_line(size=0.5) +
  geom_hline(yintercept=coefficient, linetype="dashed", color = "red") +
  theme_bw() + 
  ylab('Minimum Detectable Effect Size\n') +
  xlab('\nSample Size') +
  ylim(0,1.0) +
  scale_color_discrete(name = "Cohen's D") +
  geom_text(aes(0,coefficient,label = coefficient, vjust = -1,color='red'),size=3) +
  theme(legend.position="none")

ggsave('./figures/mde_affective_polarization_95.pdf',width = 14, height = 8,units='cm')


######################################################################

####################### H5: Media Trust ########################

######################################################################


columns = c("d_variable","sample_size","mde") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

stop_num = 0

set_num = 1
for(num_sample in seq(from = 200, to = 2000, by = 100)){
  stop_num = 0
  for(y in set_num:length(cohen_list)){
    cohen_d = cohen_list[y]
    cohen_d_2 = cohen_list_minus_1[y]
    if(stop_num == 0){
      for(x in 1:2000){
        if(x == 1){
          row_data  <- create_pvalues_neg(data_c$w3_ext_media_score,
                                          cohen_d,
                                          num_population,
                                          num_sample,
                                          0) 
        }
        if(x > 1){
          new_row <- create_pvalues_neg(data_c$w3_ext_media_score,
                                        cohen_d,
                                        num_population,
                                        num_sample,
                                        0) 
          
          row_data<- rbind(row_data,new_row)
          
        }
        
      }
      set_num = y
      row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
      print(paste0('Sample: ',as.character(num_sample),', MDE: ',as.character(cohen_d),', Power: ',as.character(mean(row_data$stat_sig95))))
      if(mean(row_data$stat_sig95) < 0.95){
        df <- rbind(df,c('Media Trust',num_sample,cohen_d_2))
        stop_num = 1
      }
    }
  }
}


df_5 <- df

colnames(df_5) = c("d_variable","sample_size","mde") 

df_5$mde <- as.numeric(df_5$mde)
df_5$sample_size <- as.numeric(df_5$sample_size)



coefficient = 0.33

ggplot(df_5,aes(y=mde,x=sample_size)) +
  geom_line(size=0.5) +
  geom_hline(yintercept=coefficient, linetype="dashed", color = "red") +
  theme_bw() + 
  ylab('Minimum Detectable Effect Size\n') +
  xlab('\nSample Size') +
  ylim(0,1.0) +
  scale_color_discrete(name = "Cohen's D") +
  geom_text(aes(0,coefficient,label = coefficient, vjust = -1,color='red'),size=3) +
  theme(legend.position="none")

ggsave('./figures/mde_media_trust_95.pdf',width = 14, height = 8,units='cm')





######################################################################

####################### H6: Political Cynicism ########################

######################################################################


columns = c("d_variable","sample_size","mde") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

stop_num = 0

set_num = 1
for(num_sample in seq(from = 200, to = 2000, by = 100)){
  stop_num = 0
  for(y in set_num:length(cohen_list)){
    cohen_d = cohen_list[y]
    cohen_d_2 = cohen_list_minus_1[y]
    if(stop_num == 0){
      for(x in 1:2000){
        if(x == 1){
          row_data  <- create_pvalues_pos(data_c$w3_polcyn_zcore,
                                          cohen_d,
                                          num_population,
                                          num_sample,
                                          6) 
        }
        if(x > 1){
          new_row <- create_pvalues_pos(data_c$w3_polcyn_zcore,
                                        cohen_d,
                                        num_population,
                                        num_sample,
                                        6) 
          
          row_data<- rbind(row_data,new_row)
          
        }
        
      }
      set_num = y
      row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
      print(paste0('Sample: ',as.character(num_sample),', MDE: ',as.character(cohen_d),', Power: ',as.character(mean(row_data$stat_sig95))))
      if(mean(row_data$stat_sig95) < 0.95){
        df <- rbind(df,c('Political Cynicism',num_sample,cohen_d_2))
        stop_num = 1
      }
    }
  }
}


df_6 <- df

colnames(df_6) = c("d_variable","sample_size","mde") 

df_6$mde <- as.numeric(df_6$mde)
df_6$sample_size <- as.numeric(df_6$sample_size)



coefficient = 0.13

ggplot(df_6,aes(y=mde,x=sample_size)) +
  geom_line(size=0.5) +
  geom_hline(yintercept=coefficient, linetype="dashed", color = "red") +
  theme_bw() + 
  ylab('Minimum Detectable Effect Size\n') +
  xlab('\nSample Size') +
  ylim(0,1.0) +
  scale_color_discrete(name = "Cohen's D") +
  geom_text(aes(0,coefficient,label = coefficient, vjust = -1,color='red'),size=3) +
  theme(legend.position="none")


ggsave('./figures/mde_pol_cynicism_95.pdf',width = 14, height = 8,units='cm')




######################################################################

####################### H7: SM Cynicism ########################

######################################################################


columns = c("d_variable","sample_size","mde") 
df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
colnames(df) = columns

stop_num = 0

set_num = 1
for(num_sample in seq(from = 200, to = 2000, by = 100)){
  stop_num = 0
  for(y in set_num:length(cohen_list)){
    cohen_d = cohen_list[y]
    cohen_d_2 = cohen_list_minus_1[y]
    if(stop_num == 0){
      for(x in 1:2000){
        if(x == 1){
          row_data  <- create_pvalues_pos(data_c$w3_sm_cyn_zcore,
                                          cohen_d,
                                          num_population,
                                          num_sample,
                                          9) 
        }
        if(x > 1){
          new_row <- create_pvalues_pos(data_c$w3_sm_cyn_zcore,
                                        cohen_d,
                                        num_population,
                                        num_sample,
                                        9) 
          
          row_data<- rbind(row_data,new_row)
          
        }
        
      }
      set_num = y
      row_data$stat_sig95 <- ifelse(row_data$p.value < 0.05,1,0)
      print(paste0('Sample: ',as.character(num_sample),', MDE: ',as.character(cohen_d),', Power: ',as.character(mean(row_data$stat_sig95))))
      if(mean(row_data$stat_sig95) < 0.95){
        df <- rbind(df,c('Social Media Cynicism',num_sample,cohen_d_2))
        stop_num = 1
      }
    }
  }
}
      
df_7 <- df

colnames(df_7) = c("d_variable","sample_size","mde") 

df_7$mde <- as.numeric(df_7$mde)
df_7$sample_size <- as.numeric(df_7$sample_size)


coefficient = 0.19

ggplot(df_7,aes(y=mde,x=sample_size)) +
  geom_line(size=0.5) +
  geom_hline(yintercept=coefficient, linetype="dashed", color = "red") +
  theme_bw() + 
  ylab('Minimum Detectable Effect Size\n') +
  xlab('\nSample Size') +
  ylim(0,1.0) +
  scale_color_discrete(name = "Cohen's D") +
  geom_text(aes(0,coefficient,label = coefficient, vjust = -1,color='red'),size=3) +
  theme(legend.position="none")

ggsave('./figures/mde_sm_cynicism_95.pdf',width = 14, height = 8,units='cm')












