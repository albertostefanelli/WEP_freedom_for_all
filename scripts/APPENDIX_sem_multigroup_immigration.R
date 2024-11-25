library(here)
source(here("scripts","data_preparation.R"))

dat2wayMC <- indProd(data_clean, 
                     c("q49_1","q49_3","q49_4"), 
                     c("q67_1","q67_2","q67_3","q67_4","q67_5"),
                     match = FALSE,
                     meanC = TRUE,
                     residualC= FALSE,
                     doubleMC = FALSE
)

names(dat2wayMC)

cfa_lr_cond <- '

cfa_populisma =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteff =~ "c1"*q41_1 + "c2"*q41_2 + "c3"*q41_5
cfa_imm =~ "i1"*q49_1 + "i2"*q49_3 + "i3"*q49_4 + "i4"*q49_2
cfa_int  =~ "i1p1"*q49_1.q67_1 + "i1p2"*q49_1.q67_2 + "i1p3"*q49_1.q67_3 + "i1p4"*q49_1.q67_4 + "i1p5"*q49_1.q67_5 +
        "i2p1"*q49_3.q67_1 + "i2p2"*q49_3.q67_2 + "i2p3"*q49_3.q67_3 + "i2p4"*q49_3.q67_4 + "i2p5"*q49_3.q67_5 +
        "i3p1"*q49_4.q67_1 + "i3p2"*q49_4.q67_2 + "i3p3"*q49_4.q67_3 + "i3p4"*q49_4.q67_4 + "i3p5"*q49_4.q67_5


# cfa_imm cov set to 0 as in interaction 
cfa_imm ~~ 0*cfa_populisma
# set covariance with interaction term to 0
cfa_int ~~ 0*cfa_imm + 0*cfa_populisma + 0*cfa_inteff  
# estimate variance latent vars
cfa_populisma + cfa_imm + cfa_int + cfa_inteff  ~ NA*1
# identify latent means by fixing manifest intercept to 1 
q67_1  + q49_1.q67_1 + q49_1 + q41_1  ~ 0*1 

expq80value ~ cfa_populisma + 
cfa_int + 
cfa_imm +
q81 +
region +
q2 + 
age6 +
edu3n_2 +
edu3n_3 +
cfa_inteff
'

sem_lr_free <- sem(cfa_lr_cond,
                   dat2wayMC,
                   group = "expq80cond")

summary(sem_lr_free)

slope_immigrants <- probe2WayMC(sem_lr_free, nameX = c("cfa_populisma", "cfa_imm", "cfa_int"),
                                nameY = "expq80value", 
                                modVar = "cfa_imm", valProbe = seq(-2, 2, 0.01), group="Immigrants") %>% 
  .[["SimpleSlope"]] %>%
  mutate(
    conf_low = est - qnorm((1 + 0.95) / 2) * se,
    conf_high = est + qnorm((1 + 0.95) / 2) * se,
    group = "Immigrants"
  )


slope_multinationals <- probe2WayMC(sem_lr_free, nameX = c("cfa_populisma", "cfa_imm", "cfa_int"),
                                    nameY = "expq80value", 
                                    modVar = "cfa_imm", valProbe = seq(-2, 2, 0.01), group="Multinationals") %>% 
  .[["SimpleSlope"]] %>%
  mutate(
    conf_low = est - qnorm((1 + 0.95) / 2) * se,
    conf_high = est + qnorm((1 + 0.95) / 2) * se,
    group = "Multinationals"
  )

binded_imm <- rbind(slope_immigrants, slope_multinationals)

plot_affective <- ggplot(binded_imm, aes(x = cfa_imm, y = est)) + 
  geom_ribbon(aes(ymin=conf_low, ymax=conf_high),
              alpha=0.2) + 
  facet_wrap(~group) + 
  geom_line() +
  theme_classic() +
  geom_hline(yintercept = 0, colour='grey40', linetype="dashed", size=0.5) +
  ylab("Slope of populist attitudes for allowing a speech against [multinationals/immigrants]") +
  xlab("Anti-immigration") +
  theme(axis.title = element_text(face="bold"),strip.text.x=element_text(face="bold"))

plot_affective

ggsave("figures/appendix_sem_anti_immigration.png", 
       width = 30, height = 20, units = "cm") 

