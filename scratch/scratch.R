










#######################################################################################

########### Proportion of Fact-Checking Organizations follows by account  #############

#######################################################################################

#Name variable:
variable_name <- 'Proportion of Fact-Checking Organizations follows by account'

#Predicted data:
#Smallest Effect Size (Cohen's D = 0.1):
#Control:
Control_sample_1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.04,0.01,0.03,0.02,0.01,0.01)
#Treatment:
Treat_sample_1 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.01,0.0428499,0.03,0.03,0,0.02,0.01)


#Medium Effect Size (Cohen's D = 0.15):
#Control:
Control_sample_2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.04,0.01,0.03,0.02,0.01,0.01)
#Treatment:
Treat_sample_2 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.01,0.05429,0.03,0.03,0,0.02,0.01)

#Largest Effect Size (Cohen's D = 0.20):
#Control:
Control_sample_3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.04,0.01,0.03,0.02,0.01,0.01)
#Treatment:
Treat_sample_3 <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0.01,0.0657,0.03,0.03,0,0.02,0.01)


power_analysis(Control_sample_1,
               Treat_sample_1,
               Control_sample_2,
               Treat_sample_2,
               Control_sample_3,
               Treat_sample_3,
               variable_name,
               'Prop_FCers_Following.png')



################################################################

########### Average News Rating Shared on Twitter  #############

#################################################################


#Name variable:
variable_name <- 'Average News Rating Shared on Twitter'

#Predicted data:
#Smallest Effect Size (Cohen's D = 0.1):
#Control:
Control_sample_1 <- na.omit(df_2$Average_domain_NewsG_Score)
#Treatment:
Treat_sample_1 <- na.omit(df_2$Average_domain_NewsG_Score)
Treat_sample_1 <- Treat_sample_1 + sd(Control_sample_1)*0.1


#Medium Effect Size (Cohen's D = 0.15):
#Control:
Control_sample_2 <- na.omit(df_2$Average_domain_NewsG_Score)
#Treatment:
Treat_sample_2 <- na.omit(df_2$Average_domain_NewsG_Score)
Treat_sample_2 <- Treat_sample_2 + sd(Control_sample_2)*0.15


#Largest Effect Size (Cohen's D = 0.20):
#Control:
Control_sample_3 <- na.omit(df_2$Average_domain_NewsG_Score)
#Treatment:
Treat_sample_3 <- na.omit(df_2$Average_domain_NewsG_Score)
Treat_sample_3 <- Treat_sample_3 + sd(Control_sample_3)*0.2


power_analysis(Control_sample_1,
               Treat_sample_1,
               Control_sample_2,
               Treat_sample_2,
               Control_sample_3,
               Treat_sample_3,
               variable_name,
               'Avg_News_Rating_Shared_Twitter.png')





################################################################

####################### Belief in Misinfo  ######################

################################################################



#Name variable:
variable_name <- 'Belief in Misinformation'

#Predicted data:
#Smallest Effect Size (Cohen's D = 0.1):
#Control:
Control_sample_1 <- na.omit(df_1$Covid_Misinfo_Index_w)
#Treatment:
Treat_sample_1 <- na.omit(df_1$Covid_Misinfo_Index_w)
Treat_sample_1 <- Treat_sample_1 + sd(Control_sample_1)*0.1


#Medium Effect Size (Cohen's D = 0.15):
#Control:
Control_sample_2 <- na.omit(df_1$Covid_Misinfo_Index_w)
#Treatment:
Treat_sample_2 <- na.omit(df_1$Covid_Misinfo_Index_w)
Treat_sample_2 <- Treat_sample_2 + sd(Control_sample_2)*0.15


#Largest Effect Size (Cohen's D = 0.20):
#Control:
Control_sample_3 <- na.omit(df_1$Covid_Misinfo_Index_w)
#Treatment:
Treat_sample_3 <- na.omit(df_1$Covid_Misinfo_Index_w)
Treat_sample_3 <- Treat_sample_3 + sd(Control_sample_3)*0.2


power_analysis(Control_sample_1,
               Treat_sample_1,
               Control_sample_2,
               Treat_sample_2,
               Control_sample_3,
               Treat_sample_3,
               variable_name,
               'Belief_in_Misinfo.png')

