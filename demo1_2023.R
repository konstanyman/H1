setwd("D:/R/")    
library(foreign)
dat<-read.spss("demoaineisto2015r.sav", to.data.frame=TRUE)
attach(dat)

#6 ensimm�ist� havaintoa
head (dat)

# kuvailevat tunnusluvut ryhmitt�in
with(dat, tapply(oma_tulo, taltyyt,summary))

# laatikko-jana-kuvio

boxplot(oma_tulo~taltyyt)
plot(oma_tulo~taltyyt, data=dat, main="tyytyv�isyys taloudelliseen tilanteeseen")

# Normaalijakaumatestaus 
with(dat, tapply(oma_tulo, taltyyt,shapiro.test))

# hajontojen yht�suuruustestaus
bartlett.test(oma_tulo~taltyyt)
 
# Yksisuuntainen varianssianalyysi, hajonnat ovat yht�suuret
anova(lm(oma_tulo~taltyyt))

# Yksisuuntainen varianssianalyysi, hajonnat ovat erisuuret
oneway.test(oma_tulo~taltyyt)

# monivertailu bonferroni-korjauksella, hajonnat yht�suuret
pairwise.t.test(oma_tulo,taltyyt, p.adj="bonferroni")

# monivertailu holm-korjauksella, hajonnat erisuuret
pairwise.t.test(oma_tulo,taltyyt, pool.sd=F) 

# Ep�parametrinen yksisuuntainen malli sijaintien erolle
kruskal.test(oma_tulo~taltyyt)

# U-testit holm-korjauksella
pairwise.wilcox.test(oma_tulo,taltyyt)

#yksinkertaiset kontrastit, LSD
pairwise.t.test(oma_tulo,taltyyt, p.adj="none")
