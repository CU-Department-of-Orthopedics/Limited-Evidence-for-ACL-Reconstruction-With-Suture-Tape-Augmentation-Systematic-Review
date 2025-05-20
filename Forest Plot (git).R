## Forest Plots 

library(readxl)
library(esc)
library(metafor)
library(tidyverse)

# Post-Op KOOS 

dat1 <- ...

dat_ef <- esc_mean_sd(
  grp1m = dat1$Group1_mean, 
  grp2m = dat1$Group2_mean, 
  grp1sd = dat1$Group1_SD, 
  grp2sd = dat1$Group2_SD, 
  grp1n = dat1$n_Group1, 
  grp2n = dat1$n_Group2,
  es.type = "d"
)

dat1$ef <- dat_ef$es
dat1$ef_se <- dat_ef$se

ma1 <- rma.mv(ef, ef_se, slab = Study, data = dat1, random = ~1|Study/Subgroup, method = "REML") 

library(meta)

meta_obj <- metagen(ef, ef_se,  data = dat1,
                    studlab = Study, comb.fixed = F, comb.random = FALSE, byvar = Subgroup)

forest(meta_obj, leftcols = c("Study"), rightcols = c("TE", "ci"), xlab = "Post-Op Mean Difference Effect Size")

## Graft Failure Rates 

dat2 <- ...

dat_ef2 <- esc_bin_prop(
  prop1event = dat2$Group1_proportion, 
  prop2event = dat2$Group2_proportion, 
  grp1n = dat2$n_Group1, 
  grp2n = dat2$n_Group2,
  es.type = "or"
)

dat2$ef <- dat_ef2$es
dat2$ef_se <- dat_ef2$se

ma2 <- rma(yi = ef,vi =  ef_se, data = dat2, method = "REML")

summary(ma2)

forest(
  x = ma2,
  slab = paste0(dat2$Study, " (", dat2$Group1, "-", dat2$Group2, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "");mtext("Post-Op Odds Ratio Effect Size", side = 1, line = 1)


## Return to Sport 

dat3 <- ...

dat_ef3 <- esc_bin_prop(
  prop1event = dat3$Group1_proportion, 
  prop2event = dat3$Group2_proportion, 
  grp1n = dat3$n_Group1, 
  grp2n = dat3$n_Group2,
  es.type = "or"
)

dat3$ef <- dat_ef3$es
dat3$ef_se <- dat_ef3$se

ma3 <- rma(yi = ef,vi =  ef_se, data = dat3, method = "REML")

summary(ma3)

forest(
  x = ma3,
  slab = paste0(dat3$Study, " (", dat3$Group1, "-", dat3$Group2, ")"),
  addfit = F,
  header = c("Study", "Effect Size [95% CI]"),
  xlab = "");mtext("Post-Op Odds Ratio Effect Size", side = 1, line = 1)


