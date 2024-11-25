library(here)
source(here("scripts","data_preparation.R"))



dat2wayMC <- indProd(data_clean, 
                     c("q75_7","q40_3"), 
                     c("q67_1","q67_2","q67_3","q67_4","q67_5"),
                     match = FALSE,
                     meanC = TRUE,
                     residualC= FALSE,
                     doubleMC = FALSE
)


cfa_lr_cond <- '

cfa_populism =~ "p1"*q67_1 + "p2"*q67_2 + "p3"*q67_3 + "p4"*q67_4 + "p5"*q67_5
cfa_inteffa =~ "c1"*q41_1 + "c2"*q41_2 + "c3"*q41_5
cfa_egal =~ "e1"*q75_7 + "e2"*q40_3
cfa_int  =~ "e1p1"*q75_7.q67_1 + "e1p2"*q75_7.q67_2 + "e1p3"*q75_7.q67_3 + "e1p4"*q75_7.q67_4 + "e1p5"*q75_7.q67_5 +
            "e2p1"*q40_3.q67_1 + "e2p2"*q40_3.q67_2 + "e2p3"*q40_3.q67_3 + "e2p4"*q40_3.q67_4 + "e2p5"*q40_3.q67_5

# cfa_egal cov set to 0 as in interaction
cfa_egal ~~ 0*cfa_populism
# set covariance with interaction term to 0
cfa_int ~~ 0*cfa_egal + 0*cfa_populism + 0*cfa_inteffa  
# estimate variance latent vars
cfa_populism + cfa_egal + cfa_int + cfa_inteffa ~ NA*1
# identify latent means by fixing manifest intercept to 1 
q67_1 + q75_7.q67_1 + q41_1 + q75_7 ~ 0*1 

expq80value ~ cfa_populism + 
cfa_int + 
cfa_egal +
cfa_inteffa +
q81 +
region +
q2 + 
age6 +
edu3n_2 +
edu3n_3

'

sem_lr_free <- sem(cfa_lr_cond,
                   dat2wayMC,
                   group = "expq80cond",
                   meanstructure = TRUE
)

summary(sem_lr_free, standardized=TRUE)

slope_immigrants <- probe2WayMC(sem_lr_free, nameX = c("cfa_populism", "cfa_egal", "cfa_int"),
                                nameY = "expq80value", 
                                modVar = "cfa_egal", valProbe = seq(-2, 2, 0.01), group="Immigrants") %>% 
  .[["SimpleSlope"]] %>%
  mutate(
    conf_low = est - qnorm((1 + 0.95) / 2) * se,
    conf_high = est + qnorm((1 + 0.95) / 2) * se,
    group = "Immigrants"
  )


slope_multinationals <- probe2WayMC(sem_lr_free, nameX = c("cfa_populism", "cfa_egal", "cfa_int"),
                                    nameY = "expq80value", 
                                    modVar = "cfa_egal", valProbe = seq(-2, 2, 0.01), group="Multinationals") %>% 
  .[["SimpleSlope"]] %>%
  mutate(
    conf_low = est - qnorm((1 + 0.95) / 2) * se,
    conf_high = est + qnorm((1 + 0.95) / 2) * se,
    group = "Multinationals"
  )

binded_imm <- rbind(slope_immigrants, slope_multinationals)

plot_affective <- ggplot(binded_imm, aes(x = cfa_egal, y = est)) + 
  geom_ribbon(aes(ymin=conf_low, ymax=conf_high),
              alpha=0.2) + 
  facet_wrap(~group) + 
  geom_line() +
  theme_classic() +
  geom_hline(yintercept = 0, colour='grey40', linetype="dashed", size=0.5) +
  ylab("Slope of populist attitudes for allowing a speech against [multinationals/immigrants]") +
  xlab("Economic egalitarianism") +
  theme(axis.title = element_text(face="bold"),strip.text.x=element_text(face="bold"))

plot_affective

ggsave("figures/appendix_sem_egalitarianism.png", 
       width = 30, height = 20, units = "cm") 
