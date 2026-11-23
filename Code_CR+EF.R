library(MplusAutomation)
library(kableExtra)
library(ggthemes)
library(table1)
library(emmeans)
library(sjPlot)
library(tidyverse)
library(lmerTest)

theme_set(theme_classic())
theme_update(axis.line = element_line(color = "black", linewidth = 1.5),
        #axis.line.y = element_line(color = "black", linewidth = 1),
        axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 13, face = "bold"),
        strip.text = element_text(size = 13, face = "bold"),
        legend.position = "bottom",
        legend.background = element_rect(color = NA),
        legend.direction = "horizontal"
      #  legend.title = element_blank()
        )


#dir.create("Saved Figures")
#dir.create("Saved Data")
fig.path = "./Saved Figures"  # Folder where we save figures.
data.path = "./Saved Data/"   # Folder where we save used datasets

dt <- readxl::read_excel("../03 Data/PACt_EF_CR_GlobalCog_Data_6Feb25.xlsx",
                         sheet = "Sheet1",
                         na = c("NA","",".")) %>% 
  mutate(mci = ifelse(rand_strata == "MCI",1,0)) %>% 
  mutate(id = str_remove(id_New,"PAC01_CMH_")) %>% 
  mutate(id = as.factor(substr(id,1,7)) ) %>% 
  mutate(mid = as.numeric(id)) %>% 
  mutate(time = as.numeric(as.character(timepoint))) %>% 
  arrange(mid, time) %>% 
  relocate(id_New, id, mid, time, timepoint,mci) 
  
names(dt)<- str_remove(names(dt),"_highest_level")

saveRDS(dt, paste0(data.path,
                   "Original_Data_",
                   format(Sys.time(), '%d_%B_%Y'),
                   ".rds"))

# head(dt) %>% as.data.frame()
# count(dt, time,timepoint)
# count(dt, mci,rand_strata)
# length(unique(dt$mid))

dt2 <- dt %>% 
  select(-randomization,-rand_strata,-id_New,-id) %>% 
  mutate(month = ifelse(time == 0,0,
                       ifelse(time == 1, 2,(time-1)*12))) %>% 
  filter(time < 4) %>% 
  select(-quality_bad, -dlpfc_l_952, -dlpfc_r_952) %>% 
  left_join(dt %>% 
              filter(time == 0) %>% 
              select(mid, quality_bad, dlpfc_l_952, dlpfc_r_952,
                     globalcog_composite,verbalmemory_composite) %>% 
              rename(gc0 = globalcog_composite,
                     vm0 = verbalmemory_composite)) %>% 
  mutate(ATT = rowMeans(select(.,pressure,gulf), na.rm = T),  
         MM = rowMeans(select(.,elephant,paris,characters,
                                objects, remember, restaurant, 
                                words, voicemial), na.rm = T),
         VS = rowMeans(select(.,figures,points,turning), na.rm = T),
         EFL = rowMeans(select(.,basketball,hurray,files,
                               find,private,hereald,
                               nback,doubleplay,stars,hanoi), na.rm = T)) %>% 
  mutate(EF = ifelse(dlpfc_l_952< median(dt$dlpfc_l_952, na.rm = T),
                     "Low","High")) %>% 
  mutate(age70 = (age_enrollment_years - 70)/10,
         edu = factor(sdemo_sbjct_highest_ed2, 
                      labels = c("<Undergrad.",
                                 "Undergrad.",
                                 "Grad."))) %>% 
  mutate(Undergrad = ifelse(sdemo_sbjct_highest_ed2 == 2,1,0),
         Grad = ifelse(sdemo_sbjct_highest_ed2 == 3,1,0),
         EF_High = ifelse(EF == "High",1,0),
         mdd = 1 - mci,
         dx = factor(mci,labels = c("MDD","MCI") ),
         year = month/12) %>% 
  relocate(mid,month,EF,EFL,ATT,VS,MM,age70,edu,Undergrad:mdd)

dtmodel <- dt2 %>% filter(time > 0, mid != 66)

dtwide <- dt2 %>% 
  mutate(edu = as.numeric(edu)) %>% 
  select(mid,time,
         age70,edu,Undergrad,Grad,mci,
         dlpfc_l_952,dlpfc_r_952,EF_High,
         EFL, ATT,VS,MM,
         globalcog_composite,verbalmemory_composite) %>% 
  rename(GC = globalcog_composite,
         VM = verbalmemory_composite,
         ef_l = dlpfc_l_952,
         ef_r = dlpfc_r_952) %>% 
  pivot_wider(id_cols = c(mid,age70,edu,Undergrad,Grad,mci,
                          ef_l,ef_r,EF_High),
              names_from = time,
              values_from = c(EFL,ATT,VS,MM,GC,VM)) %>% 
  mutate(group = ifelse(mci==0 & EF_High == 0,1,
                        ifelse(mci==0 & EF_High == 1,2,
                               ifelse(mci==1 & EF_High == 0,3,4
                                      ))))

dtwide[is.na(dtwide) | is.nan(as.matrix(dtwide))] <- 9999

#write.table(dtwide, "mdata.csv",sep = ",",
#          row.names = F, col.names = F)

# added group variable
#write.table(dtwide, "mdata2.csv",sep = ",",
#          row.names = F, col.names = F)

m1 <- lmer(EFL ~ gc0 + age70 + edu + dx *EF*year + (1 + year|mid),
           data = dtmodel, REML = T)

sjPlot::tab_model(m1,
                  dv.labels = c("EFL ~ dx * EF * Year"))

# drop1(m1, scope = "dx:EF:year", test = "LRT")

em <- emmeans(m1, ~dx*EF*year, at = list(year = c(2/12,1,2,3))) %>% 
  as.data.frame()

ggplot(em, aes(x = year, y = emmean, color = EF))+
  geom_errorbar(linewidth = 2, width = 0,
                aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(width = 0.2))+
  geom_point(size = 3, 
             position = position_dodge(width = 0.2))+
  geom_line(aes(group = EF),
            position = position_dodge(width = 0.2))+
  scale_color_grey(start = 0.6, end = 0.8)+
  scale_x_continuous(breaks = 0:6,labels = 0:6)+
  ylim(0,25)+
  facet_wrap(~dx)+
  labs(y = "Marginal EFL means",
       x = "Time in years from baseline",
       title = "EFL by EF (Low,High)",
       subtitle = "Adjsuted for education, age, baseline cognition")

out1 <- readModels("replication crosslagged model (edu) year 1 to 3 mgrp (143).out")

p1 <- out1$parameters$unstandardized

pf1 <- p1 %>% 
  filter(param %in% c("EFL1","AT1","VS1","MM1") |
           param %in% c("EFL2","AT2","VS2","MM2")) %>% 
  filter(str_detect(paramHeader,"GC") )

pc1 <- out1$parameters$ci.unstandardized

pcf1 <- pc1 %>% 
  filter(param %in% c("EFL1","AT1","VS1","MM1") |
           param %in% c("EFL2","AT2","VS2","MM2"))  %>% 
  filter(str_detect(paramHeader,"GC") )

pall1 <- pf1 %>% 
  bind_cols(pcf1 %>% 
              select(-paramHeader,-param,-est, -Group)) %>% 
  mutate(Model = "FIML Yr 1-3")

pfd1 <- p1 %>% 
  filter(param %in% c("D_EFL","D_AT","D_VS","D_MM") )

pcfd1 <- pc1 %>% 
  filter(param %in%  c("D_EFL","D_AT","D_VS","D_MM"))

palld1 <- pfd1 %>% 
  bind_cols(pcfd1 %>% 
              select(-paramHeader,-param,-est, -Group)) %>% 
  select(-paramHeader, -Group) %>% 
  mutate(Model = "FIML Yr 1-3")

out3 <- readModels("replication crosslagged model (edu) year 1 to 3 mgrp3 -66 adj dx.out")

p3 <- out3$parameters$unstandardized

pf3 <- p3 %>% 
  filter(param %in% c("EFL1","AT1","VS1","MM1") |
           param %in% c("EFL2","AT2","VS2","MM2")) %>% 
  filter(str_detect(paramHeader,"GC") )

pc3 <- out3$parameters$ci.unstandardized

pcf3 <- pc3 %>% 
  filter(param %in% c("EFL1","AT1","VS1","MM1") |
           param %in% c("EFL2","AT2","VS2","MM2"))  %>% 
  filter(str_detect(paramHeader,"GC") )

pall3 <- pf3 %>% 
  bind_cols(pcf3 %>% 
              select(-paramHeader,-param,-est, -Group)) %>% 
  mutate(Model = "FIML Yr 1-3 Adj DX")

pfd3 <- p3 %>% 
  filter(param %in% c("D_EFL","D_AT","D_VS","D_MM") )

pcfd3 <- pc3 %>% 
  filter(param %in%  c("D_EFL","D_AT","D_VS","D_MM"))

palld3 <- pfd3 %>% 
  bind_cols(pcfd3 %>% 
              select(-paramHeader,-param,-est, -Group)) %>% 
  select(-paramHeader, -Group) %>% 
  mutate(Model = "FIML Yr 1-3 Adj DX")

out1 <- readModels("replication crosslagged model (edu) year 1 to 3 mgrp2 constrained -66.out")

p1 <- out1$parameters$unstandardized

pf1 <- p1 %>% 
  filter(param %in% c("EFL1","AT1","VS1","MM1") |
           param %in% c("EFL2","AT2","VS2","MM2")) %>% 
  filter(str_detect(paramHeader,"GC") )

pc1 <- out1$parameters$ci.unstandardized

pcf1 <- pc1 %>% 
  filter(param %in% c("EFL1","AT1","VS1","MM1") |
           param %in% c("EFL2","AT2","VS2","MM2"))  %>% 
  filter(str_detect(paramHeader,"GC") )

pall1 <- pf1 %>% 
  bind_cols(pcf1 %>% 
              select(-paramHeader,-param,-est, -Group)) %>% 
  mutate(Model = "FIML Yr 1-3")

pfd1 <- p1 %>% 
  filter(param %in% c("EFL2","D_EFL_D","D_EFL_C","DD") ) %>% 
  filter(paramHeader %in% c("GC2.ON","New.Additional.Parameters"))

pcfd1 <- pc1 %>% 
  filter(param %in% c("EFL2","D_EFL_D","D_EFL_C","DD") ) %>% 
  filter(paramHeader %in% c("GC2.ON","New.Additional.Parameters"))

palld1 <- pfd1 %>% 
  bind_cols(pcfd1 %>% 
              select(-paramHeader,-param,-est, -Group)) %>% 
  mutate(param2 = c("EFL Slope (MDD EF=Low)",
                   "EFL Slope (MDD EF=High)",
                   "EFL Slope (MCI EF=Low)",
                   "EFL Slope (MCI EF=High)",
                   "EF High-Low (MDD)",
                   "EF High-Low (MCI)",
                   "EF High-Low(MDD - MCI)")) %>% 
  mutate(Model = "FIML Yr 1-3")

ggplot(dt3 %>% 
         filter(Model != "Overall", 
                Missing == "FIML",
                Model == "Interaction"), 
       aes(x = DX, y = est, shape = EF, color = EF))+
  geom_pointrange(aes(ymin =low2.5, ymax = up2.5 ),
                  position = position_dodge(width = 0.5))+
  geom_hline(yintercept = 0, linetype = "dashed")+
  scale_color_grey(start = 0.1, end = 0.8)+
  labs(y = "Estimate EFL --> GC effect",
       x = "",
       title = "Effect Comparions",
       subtitle = "FIML years 1 to 3, Model with Interaction") +
  theme_minimal()
