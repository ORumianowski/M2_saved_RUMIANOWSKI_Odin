setwd("C:/Users/33763/Documents/CMRi/0_Github/MetaGull/MetaGull")

library(tidyverse)
library(readxl)



# marray ------------------------------------------------------------------
# P1 ----------------------------------------------------------------------
# load(file = "1_formatage/real_dataset/period_123/CH_p1_3_1605.Rda")
# CH = CH[!rowSums(CH)==0, ]
# save(CH, file = "CH_p1_1805.Rda")
# n.colony = 5
# y = CH 
# marr = IPMbook::marray(y, unobs=n.colony+1) 
# save(marr, file = "marr_p1_1805.Rda")

# P2 ----------------------------------------------------------------------
load(file = "1_formatage/real_dataset/period_1234/CH_p3_4_2805.Rda")
CH = CH[!rowSums(CH)==0, ]
CH = rbind(CH,
           matrix(c(0,0,0, 3), ncol = 4),
           matrix(c(0,0,0, 4), ncol = 4))
save(CH, file = "CH_p3_4_assec_2805.Rda")
n.colony = 7
y = CH 
marr = IPMbook::marray(y, unobs=n.colony+1) 
save(marr, file = "marr_p3_4_2805.Rda")

# P3 ----------------------------------------------------------------------
load(file = "1_formatage/real_dataset/period_1234/CH_p4_4_2805.Rda")
# Suppression des lecture de bagues sur colonies quasi-absentes -----------
# on retire les lectures de bagues
CH[CH[, 8] == 13, 8] = 0 # sur la colonie 7 en 2017
CH[CH[, 8] == 12, 8] = 0 # sur la colonie 6 en 2017
CH[CH[, 7] == 13, 7] = 0 # sur la colonie 7 en 2016

# on retire les baguages sur ces colonies
CH[CH[, 8] == 5, 8] = 0
CH[CH[, 8] == 4, 8] = 0
CH[CH[, 7] == 5, 7] = 0

CH = CH[-which(rowSums(CH) == 0),]

save(CH, file = "CH_p4_4_assec_2805.Rda")

n.colony = 8
y = CH 
marr = IPMbook::marray(y, unobs=n.colony+1) 
save(marr, file = "marr_p4_4_2805.Rda")


# B -----------------------------------------------------------------------
# Toutes les colonies
c("LR_E", "MA_E", "PC_E","V5_E", "VS_E","LA_E","SV_E", "WV_E", "SZ_E", "AE")
# P1
c("LR_E", "MA_E", "PC_E","V5_E", "AE")
# P2 
c("LR_E", "MA_E", "VS_E","LA_E","SV_E", "WV_E", "AE")
# P3
c("LR_E", "MA_E", "VS_E","LA_E","SV_E", "WV_E", "SZ_E", "AE")
# Attention le AE ne regroupe les mmêmes colonies selon la période
# P1 ----------------------------------------------------------------------
# load(file = "1_formatage/real_dataset/period_123/B_p123_0905.Rda")
# B = B[,1:21]
# B[5,] = colSums(B[5:10,])
# B = B[1:5,]
# save(B, file = "B_p1_1605.Rda")
# P3 ----------------------------------------------------------------------
load(file = "1_formatage/real_dataset/period_123/B_p123_0905.Rda")
B = B[c(1:2, 5:8, 10),22:25]
save(B, file = "B_p3_4_2805.Rda")
# P4 ----------------------------------------------------------------------
load(file = "1_formatage/real_dataset/period_123/B_p123_0905.Rda")
B = B[c(1:2, 5:10),25:34]
save(B, file = "B_p4_4_2805.Rda")


