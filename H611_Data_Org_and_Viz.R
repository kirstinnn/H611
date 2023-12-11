################################################################################
setwd("C:/Users/kirst/Box/H 611/H611_Sys_Rev_Final_Figs")

# DATA ORG AND PREP FOR H611 SYSTEMATIC REVIEW 

# LOAD PACKAGES
require(tidyverse); # DATA MANAGEMENT 
require(ggthemes); require(ggpubr); require(ggpattern) # DATA VIZ 

################################################################################

# FIGURE 2 - BOXPLOTS OF TCN
# CREATE DATAFRAME USING INFO FROM EXTRACTION TABLE

data_exp <- data.frame(
  study = c("Study 1", "Study 2a", "Study 2b", "Study 3a", "Study 3b", "Study 4", "Study 5"),
  mean = c(0, 0.01, 0, 0.03, 0.02, 0.00, 0.0),
  sd = c(3.06, 1.2, 1.0, 1.45, 1.30, 1.64, 3.7),
  min = c(-12.50, -6.5, -5.3, -5.50, -5.90, -10.10, -29.9),
  perc25 = c(-1.90, -0.6, -0.56, -0.70, -0.60, -0.70, -1.7),
  median = c(0.2, 0.1, 0, 0.20, 0.20, 0.20, 0.2),
  perc75 = c(1.90, 0.8, 0.56, 1.00, 0.80, 1.00, 2.0),
  max = c(13.80, 5.0, 5.8, 4.20, 3.70, 6.20, 24.7)
)

data_exp$study<- factor(data_exp$study, levels  = c("Study 1", 
                                                            "Study 2a", "Study 2b", "Study 3a",
                                                            "Study 3b", "Study 4", "Study 5"), 
                            labels = c("Cheng; Zhu (2014)",
                                       "A. Guo (2011)", "B. Guo (2011)", "A. Lin (2013)",
                                       "B. Lin (2013)", 
                                       "Xiao (2021)", "Zhan (2017)"))



# PLOT

fig2 <- ggplot(data_exp, aes(x = "", y = median)) +
  geom_boxplot(
    aes(ymin = min, lower = perc25, middle = median, upper = perc75, ymax = max),
    stat = "identity", position = position_dodge(width = 0.75),
    fill = "light blue", color = "black", alpha = 0.7
  ) +
  geom_point(aes(y = mean), color = "red", position = position_dodge(width = 0.75), size = 2) +
  labs(
    x = "",
    y = expression("TCN (" * degree * "C)")
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black", size = 12),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.line.x = element_line(color = "black", linewidth = 0.5),
    axis.line.y = element_line(color = "black", linewidth   = 0.5),
    legend.position = "none"
  ) +
  facet_wrap(~study, nrow = 1)
fig2

################################################################################

# FIGURE 3 - BOXPLOTS OF MORTALITY DATA 
# CREATE DATAFRAME USING INFO FROM EXTRACTION TABLE
data_outcome <- data.frame(
  study = c("Study 1", "Study 2a", "Study 2b", "Study 3a", "Study 3b", "Study 4", "Study 5"),
  mean = c(4.44, 16, 138, 30.19, 17.64, 31, 2014.5),
  sd = c(2.42, 4.4, 13.4, 6.18, 4.76, 7, 164.3),
  min = c(0, 1, 95, 15.00, 6.00, 2, 1661),
  perc25 = c(3.00, 13, 12.9, 26.00, 14.00, 26, 1892),
  median = c(4.00, 15, 138, 30.00, 17.00, 30, 1979),
  perc75 = c(6.00, 18, 147, 34.00, 21.00, 35, 2107),
  max = c(16.00, 43, 217, 54.00, 33.00, 70, 2813)
)


data_outcome$study<- factor(data_outcome$study, levels  = c("Study 1", 
                                              "Study 2a", "Study 2b", "Study 3a",
                                              "Study 3b", "Study 4", "Study 5"), 
                     labels = c("Cheng; Zhu (2014)",
                                "A. Guo (2011)", "B. Guo (2011)", "A. Lin (2013)",
                                "B. Lin (2013)", 
                                "Xiao (2021)", "Zhan (2017)"))


fig3 <- ggplot(data_outcome, aes(y = median)) +
  geom_boxplot(
    aes(x = 1, ymin = min, lower = perc25, middle = median, upper = perc75, ymax = max),
    stat = "identity", position = position_dodge(width = 0.75),
    fill = "light blue", color = "black", alpha = 0.7
  ) +
  geom_point(aes(x = 1, y = mean), color = "red", position = position_dodge(width = 0.75), size = 2) +
  labs(
    x = "",
    y = "Total daily deaths"
  ) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black", size = 12),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.line.x = element_line(color = "black", size = 0.5),
    axis.line.y = element_line(color = "black", size = 0.5),
    legend.position = "none"
  ) +
  facet_wrap(~study, scales = "free_y", nrow = 1)
fig3

################################################################################

# FIGURE 4 SUMMARY RISK OF BIAS 

robdf <- data.frame(
  Study = c("Study 1", "Study 2a", "Study2b", "Study 3a", "Study 3b", "Study 4", "Study 5"),
  SB = c(1, 1, 1, 1,1, 1, 1),
  CB = c(2.11, 1.56, 1.67, 1.56, 1.56, 1.89, 2),
  DB = c(2, 2.09, 2.09, 2, 2, 2.09, 2),
  SRB = c(1, 1, 1, 1, 1, 1, 1),
  COI = c(1,3,3,3,3,1,1),
  A = c(2.25, 2.25, 2.25, 2, 2, 2, 2),
  O = c(4, 4, 4, 4, 4, 4, 4),
  Overall = c(2, 1.93, 1.97, 1.86, 1.86, 1.93, 1.93)
)

 robdf$Study<- factor(robdf$Study, levels  = c("Study 1", 
                                   "Study 2a", "Study2b", "Study 3a",
                                   "Study 3b", "Study 4", "Study 5"), 
                      labels = c("Cheng; Zhu (2014)",
                                 "A. Guo (2011)", "B. Guo (2011)", "A. Lin (2013)",
                                 "B. Lin (2013)", 
                                 "Xiao (2021)", "Zhan (2017)"))
 
 
df_long <- gather(robdf, key = "Domain", value = "RoB_Score", -Study)


df_long$Domain
df_long$Domain <- factor(df_long$Domain, levels = c("SB", "CB", "DB",
                                                       "SRB", "COI", "A", "O", "Overall"),
                            labels = c("Selection bias", "Confounding bias", 
                                       "Detection bias", "Selective reporting bias", "Conflict of interest",
                                       "Analysis", "Other", "Overall risk of bias"))
df_long


fig4 <- ggplot(df_long, aes(x = factor(Study), y = Domain, fill = RoB_Score)) +
  geom_tile(color = "white") +
  scale_y_discrete(limits = rev) +
  scale_fill_gradient(low = "green2", high = "red2", limits = c(1, 4),
                      breaks = c(1, 2, 3, 4), 
                      labels = c("Definitely low risk", "Probably low risk", "Probably high risk", "Definitely high risk"),
                      guide = guide_colorbar(reverse = TRUE)) +
  theme_minimal() +
  theme(legend.position = "right", axis.text = element_text(colour = "black", size = 12,),
        axis.text.x = element_text(angle = 90),
        legend.text = element_text(vjust = 0.5, colour = "black", size = 12),
        legend.title = element_blank()) +
  scale_x_discrete(position = "top") + 
  xlab("") + 
  ylab("")
fig4

################################################################################

# FIGURE 5 - BARPLOT OF OVERALL ROB ACROSS STUDIES 

overall_rob <- data.frame(
  risk = c("Definitely low risk", "Probably low risk", "Probably high risk", "Definitely high risk"),
  SB= c(100, 0, 0, 0),
  CB = c(0, 100, 0, 0),
  DB = c(0, 100, 0, 0),
  SRB = c(100, 0, 0, 0),
 COI = c(42.86, 0,  57.14, 0),
  A = c(0, 100, 0, 0),
  O = c(0, 0, 0, 100)
)



category_colors <- c("Definitely low risk" = "green2",
                     "Probably low risk" = "yellow2",
                     "Probably high risk" = "orange2",
                     "Definitely high risk" = "red2")



overall_rob_long <-  gather(overall_rob, key = "Domain", value = "RoB_Score", - risk)

overall_rob_long$Domain <- factor(overall_rob_long$Domain, levels = c("SB", "CB", "DB",
                                                                      "SRB", "COI", "A", "O"),
                                  labels = c("Selection bias", "Confounding bias", 
                                             "Detection bias", "Selective reporting bias", "Conflict of interest",
                                             "Analysis", "Other"))


fig5 <- ggplot(data = overall_rob_long, aes(x = Domain, y = RoB_Score, fill = risk)) + 
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() + 
  scale_fill_manual(values = category_colors, breaks = c("Definitely low risk",
                                                         "Probably low risk",
                                                         "Probably high risk",
                                                         "Definitely high risk"))+
  scale_x_discrete(limits = rev(levels(overall_rob_long$Domain))) +
  xlab("") +
  ylab("Overall risk of bias across studies (%)") +
  theme_minimal() +
  theme(legend.title = element_blank(),
      legend.text = element_text(colour = "black", size =12), 
      axis.text = element_text(colour = "black", size = 12))
fig5

################################################################################

# FIGURE 6 - EFFECT EST FOREST PLOT - NON-ACCIDENTAL MORTALITY 
# NB - DF INCLUDES DATA USED FOR LATER SYNTHESES 
  
effest <- data.frame(
    Study = c("Study 1", "Study 2a.1", "Study 2a.2", "Study2b.1", "Study2b.2", "Study 3a.1", "Study 3a.2", "Study 3b.1",
              "Study 3b.2", "Study 4", "Study 5.1", "Study 5.2"),
    measurementmet = c("Degree increase", "Degree decrease", "Degree increase",
                        "Degree decrease", "Degree increase",
                        "1st percentile", "99th percentile",
                        "1st percentile", "99th percentile",
                        "99th percentile", "1st percentile", "99th percentile"),
    samplesize = c(8111, 12364, 0, 177384, 0, 23099, 0, 13504, 0, 65333, 10302030, 0), # NB 0 FOR WHEN THEY REPEAT RRS BY TWO THRESHOLDS
   effest = c(1.069, 1.157, 1.198, 1.133, 1.039, 0.63, 1.31, 0.43, 1.46, 1.331, 0.63, 1.46),
   lowerlim = c(1.013,1.024, 0.997, 1.053, 0.971, 0.46, 1.04, 0.33, 1.15, 1.180, 0.59, 1.39),
   upperlim = c(1.129, 1.307, 1.438, 1.219, 1.112, 0.88, 1.66, 0.56, 1.84, 1.500, 0.68, 1.54),
   tcnmet = c("max", "mean", "mean", "mean","mean", "mean", "mean","mean", "mean", "mean", "mean", "mean"),
   lagcumul = c("no", "no", "no", "no", "no", "yes", "yes", "yes", "yes","yes", "yes", "yes"),
   lagrange = c("0-21", "0-3", "0-3", "0-3", "0-3", "0-28", "0-28", "0-28", "0-28", "0-7", "0-21", "0-21"),
   statmod = c("Poisson generalized linear regression", "General additive Poisson models", "General additive Poisson models",
               "General additive Poisson models", "General additive Poisson models", "Quasi-Poisson generalized linear regression",
               "Quasi-Poisson generalized linear regression", "Quasi-Poisson generalized linear regression", 
               "Quasi-Poisson generalized linear regression", "Quasi-Poisson generalized linear regression",
               "3-stage design", "3-stage design"),
   dlnm = c("yes", "no", "no", "no", "no", "yes", "yes", "yes", "yes", "yes", "yes", "yes"),
   pm2.5 = c("no", "no", "no", "no", "no", "no", "no", "no", "no", "yes", "no", "no"),
   pm10 = c("no", "yes", "yes", "no", "no", "yes", "yes", "yes", "yes", "yes",  "no", "no"),
   O3 = c("no", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "no", "no"),
   NO2 = c("no", "no", "no", "no", "no", "yes", "yes", "yes", "yes", "yes", "no", "no"),
   SO2 = c("no", "no", "no", "no", "no", "yes", "yes", "yes", "yes", "yes", "no", "no"), 
   CO = c("no", "no", "no", "no", "no", "no", "no", "no", "no", "yes", "no", "no"),
   RH = c("yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "no", "no", "no"),
   tempsome = c("yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes", "yes")
  )
  
  
  effest$Study <- factor(effest$Study, levels = c("Study 1", "Study 2a.1", "Study 2a.2",
                                                  "Study2b.1", "Study2b.2", "Study 3a.1", "Study 3a.2",
                                                  "Study 3b.1", "Study 3b.2", "Study 4", "Study 5.1", "Study 5.2"), 
                         labels = c("Cheng; Zhu (5°)",
                                    "A. Guo (>3°)", "A. Guo (>3°)", 
                                    "B. Guo (>3°)",  "B. Guo (>3°)",
                                    "A. Lin", "A. Lin", 
                                    "B. Lin", "B. Lin" , 
                                    "Xiao", 
                                    "Zhan", "Zhan"))
  
  
  fig6 <- ggplot(effest, aes(x = effest, y = factor(Study, levels = rev(levels(Study))), xmin = lowerlim, xmax = upperlim)) +
    geom_pointrange(aes(shape = lagcumul))  +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    geom_text(aes(label = sprintf("%.2f (%.2f, %.2f)", effest, lowerlim, upperlim)),
              vjust = 1.5, hjust = 0) +
    xlab("Relative risk (95% CIs)") +
    ylab("") +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black", size = 12),
      axis.title = element_text(colour = "black", size = 12),
      strip.text = element_text(colour = "black", size = 12, face = "bold"),
      legend.position = "top"
    ) +
    scale_shape_manual(
      name = "Cumulated over lag days",
      values = c("no" = 19, "yes" = 17),
      labels = c("No", "Yes")
    ) +
    facet_wrap(~measurementmet, scales = "free_y", drop = TRUE, ncol = 1) 
fig6

  

################################################################################

# INDIVIDUAL STUDIES - MORTALITY TYPES - AT LAG 0 ONLY 
# FIG 7

# STUDY 5 EXCLUDED BECAUSE IT ONLY HAS CUMULATED ESTS
effestbydiffmortcause <- data.frame(
  Study = c("Study 1_Non_Acc_1", "Study 1_Cardiovasc_1", "Study 1_Resp_1",
            "Study 1_Non_Acc_99", "Study 1_Cardiovasc_99", "Study 1_Resp_99",
            "Study 2a_Non_Acc_Decrease", "Study 2a_Non_Acc_Increase", "Study 2a_Cardiovasc_Decrease", "Study 2a_Cardiovac_Increase", "Study 2a_Resp_Increase", "Study 2a_Resp_Decrease",
            "Study 2b_Non_Acc_Decrease", "Study 2b_Non_Acc_Increase", "Study 2b_Cardiovasc_Decrease", "Study 2b_Cardiovac_Increase", "Study 2b_Resp_Increase", "Study 2b_Resp_Decrease",
            "Study 3a_Non_Acc_1", "Study 3a_Cardiovasc_1", "Study 3a_Resp1",
            "Study 3a_Non_Acc_99", "Study 3a_Cardiovasc_99", "Study 3a_Resp_99",
            "Study 3b_Non_Acc_1", "Study 3b_Cardiovasc_1", "Study 3b_Resp1",
            "Study 3b_Non_Acc_99", "Study 3b_Cardiovasc_99", "Study 3b_Resp_99",
            "Study4_Non_Acc_99", "Study 4_Cardiovasc_99", "Study 4_Resp_99"),
  outcome = c("Non-accidental", "Cardiovascular", "Respiratory",
              "Non-accidental", "Cardiovascular", "Respiratory",
              "Non-accidental", "Non-accidental", "Cardiovascular", "Cardiovascular", "Respiratory", "Respiratory",
              "Non-accidental", "Non-accidental", "Cardiovascular", "Cardiovascular", "Respiratory", "Respiratory",
              "Non-accidental", "Cardiovascular", "Respiratory",
              "Non-accidental", "Cardiovascular", "Respiratory",
              "Non-accidental", "Cardiovascular", "Respiratory",
              "Non-accidental", "Cardiovascular", "Respiratory",
              "Non-accidental", "Cardiovascular", "Respiratory"),
  measurementmet = c("1st percentile", "1st percentile", "1st percentile",
                     "99th percentile", "99th percentile", "99th percentile",
                     "Degree decrease", "Degree increase", "Degree decrease", "Degree increase", "Degree decrease", "Degree increase",
                     "Degree decrease", "Degree increase", "Degree decrease", "Degree increase", "Degree decrease", "Degree increase",
                     "1st percentile", "1st percentile", "1st percentile",
                     "99th percentile", "99th percentile", "99th percentile",
                     "1st percentile", "1st percentile", "1st percentile",
                     "99th percentile", "99th percentile", "99th percentile",
                     "99th percentile", "99th percentile", "99th percentile"),
  
  effest = c(0.91, 0.76, 0.92, 
             1.00, 1.15, 0.86,
             1.157, 1.198,
             1.115, 1.353,
             1.202, 1.608,
             1.133, 1.039,
             1.252, 1.031, 
             1.006, 1.002,
             0.90, 0.95, 0.83,
             1.09, 1.18, 1.07,
             0.87, 0.87, 0.87,
             1.05, 1.06, 0.95,
             1.086, 1.117, 1.071),
  lowerlim = c(0.82, 0.58, 0.74,
               0.91, 0.91, 0.71,
               1.024, 0.997, 
               0.923, 1.033,
               0.774, 0.925,
               1.053, 0.971,
               1.131, 0.933, 
               0.767, 0.792,
               0.83, 0.69, 0.64,
               1.02, 1.03, 0.91,
               0.80, 0.78, 0.69,
               0.99, 0.98, 0.80,
               1.058, 1.073, 0.976),
  upperlim = c(1.00, 1.00, 1.15,
               1.09, 1.46, 1.05,
               1.307, 1.438, 
               1.347, 1.772, 
               1.867, 2.794,
               1.219, 1.112,
               1.386, 1.140,
               1.321, 1.266,
               0.98, 1.04, 1.06,
               1.17, 1.37, 1.26,
               0.94, 0.96, 1.09, 
               1.12, 1.14, 1.12,
               1.115, 1.163, 1.175))

effestbydiffmortcause$Study <- factor(effestbydiffmortcause$Study, levels
                                      = c("Study 1_Non_Acc_1", "Study 1_Cardiovasc_1", "Study 1_Resp_1",
                                            "Study 1_Non_Acc_99", "Study 1_Cardiovasc_99", "Study 1_Resp_99",
                                            "Study 2a_Non_Acc_Decrease", "Study 2a_Non_Acc_Increase", "Study 2a_Cardiovasc_Decrease", "Study 2a_Cardiovac_Increase", "Study 2a_Resp_Increase", "Study 2a_Resp_Decrease",
                                            "Study 2b_Non_Acc_Decrease", "Study 2b_Non_Acc_Increase", "Study 2b_Cardiovasc_Decrease", "Study 2b_Cardiovac_Increase", "Study 2b_Resp_Increase", "Study 2b_Resp_Decrease",
                                            "Study 3a_Non_Acc_1", "Study 3a_Cardiovasc_1", "Study 3a_Resp1",
                                            "Study 3a_Non_Acc_99", "Study 3a_Cardiovasc_99", "Study 3a_Resp_99",
                                            "Study 3b_Non_Acc_1", "Study 3b_Cardiovasc_1", "Study 3b_Resp1",
                                            "Study 3b_Non_Acc_99", "Study 3b_Cardiovasc_99", "Study 3b_Resp_99",
                                            "Study4_Non_Acc_99", "Study 4_Cardiovasc_99", "Study 4_Resp_99"), 
                       labels = c("Cheng; Zhu", "Cheng; Zhu", "Cheng; Zhu",
                                  "Cheng; Zhu", "Cheng; Zhu", "Cheng; Zhu",
                                  "A. Guo",  "A. Guo",  "A. Guo",  "A. Guo","A. Guo",  "A. Guo",
                                  "B. Guo", "B. Guo", "B. Guo", "B. Guo", "B. Guo", "B. Guo",
                                  "A. Lin", "A. Lin", "A. Lin", 
                                  "A. Lin", "A. Lin", "A. Lin", 
                                  "B. Lin", "B. Lin" , "B. Lin" ,
                                  "B. Lin" , "B. Lin" , "B. Lin" ,
                                  "Xiao", "Xiao", "Xiao"))

 fig7 <- ggplot(effestbydiffmortcause, aes(x = effest, y = factor(Study, levels = rev(levels(Study))), xmin = lowerlim, xmax = upperlim, fill = outcome)) +
  geom_pointrange(aes(color = outcome), position = position_dodge(width = 0.5)) + 
   scale_color_manual(values = c("darkorange1", "darkolivegreen3", "cornflowerblue")) + 
   geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
   geom_text(aes(label = sprintf("%.2f (%.2f, %.2f)", effest, lowerlim, upperlim)),
             vjust = -0.3, hjust = -0.5, position = position_dodge(width = 0.5), size = 2.8) +
   xlab("Relative risk (95% CIs)") +
   ylab("") +
   xlim(0.0, 3.0) +
   facet_wrap(~measurementmet, scales = "free_y", drop = T) +
   theme_bw() +
   theme(axis.text = element_text(colour = "black", size = 12),
         axis.title = element_text(colour = "black", size = 12),
         strip.text = element_text(colour = "black", size = 12, face = "bold"),
         legend.position = "top",
         legend.title = element_blank()) +
 facet_wrap(~measurementmet,  scales = "free_y", drop = TRUE)
fig7
 
 
###############################################################################

# INDIVIDUAL STUDIES - EFFECT MOD INDIVIDUAL -- AGE -- LAG 0
# FOR NON-ACCIDENTAL MORTALITY
# DIRECTION & SIGNIFICANCE ONLY, NO EFFECT ESTS 


effestemsage <- data.frame(
  Study = c("Study1_Young_1", "Study1_Old_1",
            "Study1_Young_99", "Study1_Old_99",
            "Study2a_Young_Decrease", "Study2a_Old_Decrease", "Study2a_Oldest_Decrease",
            "Study2a_Young_Increase", "Study2a_Old_Increase", "Study2a_Oldest_Decrease",
            "Study2b_Young_Decrease", "Study2b_Old_Decrease", "Study2b_Oldest_Decrease",
            "Study2b_Young_Increase", "Study2b_Old_Increase", "Study2b_Oldest_Decrease",
            "Study4_Young", "Study4_Old"),
  effectmodder = c("<65", "65+",
                   "<65", "65+",
                   "<65", "65-74", "75+",
                   "<65", "65-74", "75+",
                   "<65", "65-74", "75+",
                   "<65", "65-74", "75+",
                   "<65", "65+"),
  measurementmet = c("1st percentile", "1st percentile", 
                     "99th percentile", "99th percentile",
                     "Degree decrease", "Degree decrease", "Degree decrease",
                     "Degree increase", "Degree increase", "Degree increase",
                     "Degree decrease", "Degree decrease", "Degree decrease",
                     "Degree increase", "Degree increase", "Degree increase",
                     "99th percentile", "99th percentile"), 
  direction = c("Protective", "Protective",
                "Risk", "Protective",
                "Risk", "Risk", "Risk",
                "Risk", "Risk", "Risk",
                "Protective", "Risk", "Risk",
                "Risk", "Risk", "Risk",
                "Risk", "Risk"),
  statsig = c("No", "Yes", 
              "No", "No",
              "No", "Yes", "No",
              "Yes", "No", "No",
              "No", "No", "Yes",
              "No", "No", "No",
              "Yes", "Yes"))


effestemsage$Study <- factor(effestemsage$Study, levels
                                      = c("Study1_Young_1", "Study1_Old_1",
                                          "Study1_Young_99", "Study1_Old_99",
                                          "Study2a_Young_Decrease", "Study2a_Old_Decrease", "Study2a_Oldest_Decrease",
                                          "Study2a_Young_Increase", "Study2a_Old_Increase", "Study2a_Oldest_Decrease",
                                          "Study2b_Young_Decrease", "Study2b_Old_Decrease", "Study2b_Oldest_Decrease",
                                          "Study2b_Young_Increase", "Study2b_Old_Increase", "Study2b_Oldest_Decrease",
                                          "Study4_Young", "Study4_Old"),
                                      labels = c("Cheng; Zhu", "Cheng; Zhu",
                                                 "Cheng; Zhu", "Cheng; Zhu", 
                                                 "A. Guo",  "A. Guo",  "A. Guo",  
                                                 "A. Guo","A. Guo",  "A. Guo",
                                                 "B. Guo", "B. Guo", "B. Guo", 
                                                 "B. Guo", "B. Guo", "B. Guo",
                                                 "Xiao", "Xiao"))

effestemsage <- effestemsage %>%
  group_by(measurementmet, effectmodder, direction, statsig) %>%
  summarise(num_studies = n())


fig8 <- ggplot(effestemsage, aes(x = direction, y = interaction(effectmodder, statsig, lex.order = TRUE), fill = effectmodder, pattern = statsig)) +
  geom_tile_pattern(color = "white", pattern_density = 0.05, pattern_angle = 45) +
  facet_wrap(~measurementmet, scales = "free_y") +
  scale_fill_manual(values = c("<65" = "green3", "65+" = "orange2", "65-74" = "purple3", "75+" = "pink3"),
                    guide = guide_legend(title = "Age groups (years)",override.aes = list(pattern = "none"))) +
  geom_point(aes(x = direction, y = interaction(effectmodder, statsig, lex.order = TRUE)), shape = 21, size = 8, color = "black", fill = "gray") +
  scale_pattern_manual(values = c("No" = "stripe", "Yes" = "pch"),
                       guide = guide_legend(title = "Statistically significant", override.aes = list(fill = "lightgray"))) +
  geom_text(aes(label = num_studies), vjust = 0.35, color = "black", size = 3.5, fontface = "bold") +
  geom_vline(xintercept = 1.5, linetype = "solid", color = "black", size = 1) +
  theme_bw() +
  theme(
    axis.text = element_text(colour = "black", size = 12),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title = element_text(colour = "black", size = 12),
    strip.text = element_text(colour = "black", size = 12, face = "bold"),
    legend.position = "top"
  ) +
  labs(x="", y="")

fig8

  
#################################################################################

# INDIVIDUAL STUDIES - EFFECT MOD INDIVIDUAL --  SEX -- AT LAG 0
# IF LAG 0 EST NOT PROVIDED; STUDY NOT INCLUDED 
# FOR NON-ACCIDENTAL MORTALITY
# DIRECTION & SIGNIFICANCE ONLY, NO EFFECT ESTS 

# FIG 9 

effestemsex <- data.frame(
  Study = c("Study 1 Male_1", "Study 1 Male_99", 
            "Study 1 Female_1", "Study 1 Female_99",
            "Study 2 Male Low", "Study 2 Male High",
            "Study 2 Female Low", "Study 2 Female High",
            "Study 3a Male_1", "Study 3a Male_99",
            "Study 3a Female_1", "Study 3a Female_99",
            "Study 3b Male_1", "Study 3b Male_99",
            "Study 3b Female_1", "Study 3b Female_99"), 
  effectmodder = c("Male", "Male",
                   "Female", "Female",
                   "Male", "Male",
                   "Female", "Female",
                   "Male", "Male",
                   "Female", "Female",
                   "Male", "Male", 
                   "Female", "Female"), # NB COLOR BY EM 
  measurementmet = c("1st percentile", "99th percentile", 
                     "1st percentile", "99th percentile",
                     "Degree decrease", "Degree increase",
                     "Degree decrease", "Degree increase",
                     "1st percentile", "99th percentile", 
                     "1st percentile", "99th percentile",
                     "1st percentile", "99th percentile",
                     "1st percentile", "99th percentile"),
  direction = c("Protective" , "Risk",
                "Protective", "Protective",
                "Risk", "Risk",
                "Risk", "Risk",
                "Protective", "Risk",
                "Protective", "Risk",
                "Protective", "Risk",
                "Protective", "Risk"),
  statsig = c("No", "No",
              "No", "No",
              "No", "No",
              "No", "No", 
              "Yes", "Yes",
              "No", "No",
              "No", "No",
              "Yes", "No"))
              
              
            


effestemsex$Study <- factor(effestemsex$Study, levels = c("Study 1 Male_1", "Study 1 Male_99", 
                                                          "Study 1 Female_1", "Study 1 Female_99",
                                                          "Study 2 Male Low", "Study 2 Male High",
                                                          "Study 2 Female Low", "Study 2 Female High",
                                                          "Study 3a Male_1", "Study 3a Male_99",
                                                          "Study 3a Female_1", "Study 3a Female_99",
                                                          "Study 3b Male_1", "Study 3b Male_99",
                                                          "Study 3b Female_1", "Study 3b Female_99"), 
                       labels = c("Cheng; Zhu", "Cheng; Zhu", "Cheng; Zhu", "Cheng; Zhu",
                                  "A. Guo", "A. Guo", "A. Guo", "A. Guo",
                                  "A. Lin", "A. Lin", "A. Lin", "A. Lin", 
                                  "B. Lin", "B. Lin", "B. Lin", "B. Lin"))



effestemsex <- effestemsex %>%
  group_by(measurementmet, effectmodder, direction, statsig) %>%
  summarise(num_studies = n())
  
fig9 <-  ggplot(effestemsex, aes(x = direction, y = interaction(effectmodder, statsig, lex.order = TRUE), fill = effectmodder, pattern = statsig)) +
    geom_tile_pattern(color = "white", pattern_density = 0.05, pattern_angle = 45) +
    facet_wrap(~measurementmet, scales = "free_y", drop = TRUE) +
    scale_fill_manual(values = c("Female" = "hotpink", "Male" = "dodgerblue"),
                      guide = guide_legend(title = "",override.aes = list(pattern = "none"), order =1)) +
    geom_point(aes(x = direction, y = interaction(effectmodder, statsig, lex.order = TRUE)), shape = 21, size = 8, color = "black", fill = "gray") +
    scale_pattern_manual(values = c("No" = "stripe", "Yes" = "pch"),
                         guide = guide_legend(title = "Statistically significant", override.aes = list(fill = "lightgray"))) +
    geom_text(aes(label = num_studies), vjust = 0.35, color = "black", size = 3.5, fontface = "bold") +
    theme_bw() +
    theme(
      axis.text = element_text(colour = "black", size = 12),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_text(colour = "black", size = 12),
      strip.text = element_text(colour = "black", size = 12, face = "bold"),
      legend.position = "top"
    ) +
    labs(x="", y="") 
  
fig9

  
# SEASONAL ANALYSES - DIRECTIONAL ONLY 
# AT LAG 0 # DIFFERENTISH DEFS OF SEASON 

effestemseason <- data.frame(
  Study = c("Study_1_Warm", "Study_1_Cold",
            "Study_4_Warm", "Study_4_Cold"), 
  effectmodder = c("Warm", "Cold", 
                   "Warm", "Cold"), 
  measurementmet = c("25th percentile", "25th percentile",
                     "99th percentile", "99th percentile"),
 direction = c("Risk", "Risk",
               "Risk", "Risk"), 
  statsig = c("No", "Yes",
              "Yes", "Yes"))
effestemseason



effestemseason <- effestemseason%>%
  group_by(measurementmet, effectmodder, direction, statsig) %>%
  summarise(num_studies = n())

# NO PROTECTIVES - NO PLOT NEEDED 

################################################################################

# SYNTHESIS 

#################################################################################

# SUMMARY OF FINDINGS 1 - EXPOSURE ASSESSMENT - TCN METRIC 

effestexpmetric <- effest %>% group_by(tcnmet, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestexpmetric

# GET SAMPLE SIZES 

effestexp_ss <- effest %>% group_by(tcnmet) %>% 
  summarise(sumss = sum(samplesize))
effestexp_ss

################################################################################

# SUMMARY OF FINDINGS 2 STATS METHODS 

# SUBFINDING 1 - STAT MODELS - GENERAL METHOD

effeststats <- effest %>% group_by(statmod, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

view(effeststats)

# GET SAMPLE SIZES 

effesteststats <- effest %>%
  distinct(Study, statmod, .keep_all = TRUE) %>%  
  group_by(statmod) %>%
  summarise(sumss = sum(samplesize), total_count = n())

# SUBFINDING 1 CONT. - STAT MODELS - DLNMS 

effeststats_dlnm <- effest %>% group_by(dlnm, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effeststats_dlnm %>% filter(dlnm == "yes")


# GET SAMPLE SIZES 

effestestdlnm_ss <- effest %>%
  distinct(Study, dlnm, .keep_all = TRUE) %>%  
  group_by(dlnm) %>%
  summarise(sumss = sum(samplesize), total_count = n())

effestestdlnm_ss %>% filter(dlnm == "yes")
################################################################################

# SUBFINDING 2 - AIR POLLUTANTS 

######### PM2.5 

effestpm2.5 <- effest %>% group_by(pm2.5, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestpm2.5 %>% filter(pm2.5 == "yes")

# GET SAMPLE SIZES 

effestpm2.5_ss <- effest %>%
  distinct(Study, pm2.5, .keep_all = TRUE) %>%  
  group_by(pm2.5) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effestpm2.5_ss %>% filter(pm2.5 == "yes")

######### PM10 

effestpm10<- effest %>% group_by(pm10, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestpm10 %>% filter(pm10 == "yes")

# GET SAMPLE SIZES 


effestpm10_ss <- effest %>%
  distinct(Study, pm10, .keep_all = TRUE) %>%  
  group_by(pm10) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effestpm10_ss %>% filter(pm10 == "yes")


######### O3

effestO3 <- effest %>% group_by(O3, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestO3 %>% filter(O3 == "yes")

# GET SAMPLE SIZES 


effestO3_ss <- effest %>%
  distinct(Study, O3, .keep_all = TRUE) %>%  
  group_by(O3) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effestO3_ss %>% filter(O3== "yes")



#########  NO2 

effestNO2 <- effest %>% group_by(NO2, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestNO2 %>% filter(NO2 == "yes")

# GET SAMPLE SIZES 

effestNO2_ss <- effest %>%
  distinct(Study, NO2, .keep_all = TRUE) %>%  
  group_by(NO2) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effestNO2_ss %>% filter(NO2== "yes")

######### SO2 

effestSO2 <- effest %>% group_by(SO2, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestSO2 %>% filter(SO2 == "yes")

# GET SAMPLE SIZES 

effestSO2_ss <- effest %>%
  distinct(Study, SO2, .keep_all = TRUE) %>%  
  group_by(SO2) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effestSO2_ss %>% filter(SO2== "yes")

######### CO 

effestCO <- effest %>% group_by(CO, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestCO %>% filter(CO == "yes")

# GET SAMPLE SIZES 

effestCO_ss <- effest %>%
  distinct(Study, CO, .keep_all = TRUE) %>%  
  group_by(CO) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effestCO_ss %>% filter(CO== "yes")

################################################################################

# SUBFINDING 3 - OTHER METEOROLOGICAL CONFOUNDERS 

######### REL. HUMIDITY

effestRH <- effest %>% group_by(RH, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestRH %>% filter(RH == "yes")

# GET SAMPLE SIZES 

effestRH_ss <- effest %>%
  distinct(Study, RH, .keep_all = TRUE) %>%  
  group_by(RH) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effestRH_ss %>% filter(RH== "yes")


######### ANY OTHER TEMP. VAR (e.g., moving average, etc.)

effesttemp <- effest %>% group_by(tempsome, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effesttemp %>% filter(tempsome == "yes")

# GET SAMPLE SIZES 

effesttemp_ss <- effest %>%
  distinct(Study, tempsome, .keep_all = TRUE) %>%  
  group_by(tempsome) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effesttemp_ss %>% filter(tempsome== "yes")


######### LAG DAYS 

effestlag <- effest %>% group_by(lagrange, measurementmet) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))


effestlagrange_ss <- effest %>%
  distinct(Study, lagrange, .keep_all = TRUE) %>%  
  group_by(lagrange) %>%
  summarise(sumss = sum(samplesize), total_count = n())
effesttemp_ss %>% filter(tempsome== "yes")
################################################################################

# SUMMARY FINDINGS 3 - MORTALITY OUTCOMES AT LAG 0 

effestallmort <- effestbydiffmortcause %>% group_by(measurementmet, outcome) %>% 
  summarise(medianRRs = median(effest), medianlowCI = median(lowerlim), medianhighCI = median(upperlim),
            meanRRs = mean(effest), meanlowCI = mean(lowerlim), meanhighCI = mean(upperlim),
            minRRs = min(effest), minlowCI = min(lowerlim), minhighCI = min(upperlim),
            maxRRs = max(effest), maxlowCI = max(lowerlim), maxhighCI = max(upperlim),
            perc_25RR = quantile(effest, .25), perc_25lowCI = quantile(lowerlim, .25), perc_25highCI = quantile(upperlim, .25),
            perc_75RR = quantile(effest, .75), perc_75lowCI = quantile(lowerlim, .75), perc_75highCI = quantile(upperlim, .75))

effestallmort %>% filter(outcome == "Non-accidental")
effestallmort %>% filter(outcome == "Cardiovascular")
effestallmort %>% filter(outcome == "Respiratory")

# GET STUDY NUMBER - NO SAMPLE SIZES

effestlall_mort_ss <- effestbydiffmortcause %>%
  distinct(Study, outcome, .keep_all = TRUE) %>%  
  group_by(outcome) %>%
  summarise( total_count = n())


################################################################################

# SUMMARY OF FINDINGS - RRS BY SEX (NB NO SAMPLE SIZES)
# 3 STUDIES INCLUDED

effestemsex %>% filter(effectmodder == "Male") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))

effestemsex %>% filter(effectmodder == "Female") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))

# AGE


effestemsage %>% filter(effectmodder == "<65") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))

effestemsage %>% filter(effectmodder == "65+") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))

effestemsage %>% filter(effectmodder == "65-74") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))

effestemsage %>% filter(effectmodder == "75+") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))

# SEASON


effestemseason %>% filter(effectmodder == "Warm") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))


effestemseason %>% filter(effectmodder == "Cold") %>% 
  group_by(measurementmet, direction) %>% 
  summarise(count = sum(num_studies))
