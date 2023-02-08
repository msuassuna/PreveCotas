###
### SAH BRANCO
###

Modelos <- c(14710000, 14620000, 14515000, 14527000)
# Ests %>%  filter(str_detect(Codigo, "^14"))
# CARACARAÍ 36H
Prev_CARACARAI_36_1 <- function(x1, x2, a1 = 0.5942, a2 = 0.4706){
  a1 * x1 +  a2 * x2 - 5.4 - 5
}

# BOA VISTA 17H 1 - BOA VISTA E FAZENDA PASSARÃO
Prev_BOAVISTA_17_1 <- function(x1, x2, a1 = 0.8581, a2 = 0.1138){
  a1 * x1 +  a2 * x2 - 32.22
}

# BOA VISTA 17H 2 - BOA VISTA E PONTE TACUTU
Prev_BOAVISTA_17_2 <- function(x1, x2, a1 = 0.6747, a2 = 0.1035){
  a1 * x1 +  a2 * x2 + 92.61
}

# BOA VISTA 17H 3 - FAZENDA PASSARÃO E PONTE TACUTU
Prev_BOAVISTA_17_3 <- function(x1, x2, a1 = 0.8419, a2 = 0.3015){
  a1 * x1 +  a2 * x2 - 555.5
}

# BOA VISTA 17H 4 - BOA VISTA,FAZENDA PASSARÃO E PONTE TACUTU
Prev_BOAVISTA_17_4 <- function(x1, x2, a1 = 0.4883, a2 = 0.2628, a3 =  0.1359){
  a1 * x1 +  a2 * x2 +  a3 * x3  - 93.48
}

###
### SAH ACRE
###

# Ests %>%  filter(str_detect(Codigo, "^13"))
Modelos <- c(Modelos, 13600002, 13550000, 13470000)
# RIO BRANCO 30H
Cota_RBR_30H <- function(CRBR,CRBR_5H,CRBR_10H,CRBR_15H,CRBR_20H,
                         CXPU,CXPU_5H,CXPU_10H,CXPU_15H,CXPU_20H,CXPU_25H,CXPU_30H){
  return(3.498*CRBR-
    2.089*CRBR_5H-0.841*CRBR_10H+0.002*CRBR_15H+0.367*CRBR_20H+
    0.667*CXPU-0.573*CXPU_5H+0.224*CXPU_10H+0.0015*CXPU_15H-
    0.043*CXPU_20H+0.038*CXPU_25H-0.238*CXPU_30H)
}

# RIO BRANCO 24H
Cota_RBR_24H <- function(CRBR,CRBR_4H,CRBR_12H,CRBR_16H,CRBR_20H,
                         CXPU,CXPU_4H,CXPU_8H,CXPU_12H,CXPU_16H,CXPU_20H,CXPU_24H){
  return(3.771*CRBR-2.439*CRBR_4H-0.779*CRBR_12H-0.004*CRBR_16H+
           0.401*CRBR_20H+0.423*CXPU-0.351*CXPU_4H+0.129*CXPU_8H+
           0.039*CXPU_12H-0.032*CXPU_16H+0.288*CXPU_20H-0.424*CXPU_24H)
}

# RIO BRANCO 12H
Cota_RBR_12H <- function(CRBR,CRBR_2H,CRBR_4H,CRBR_6H,CRBR_8H,CRBR_10H,CRBR_12H,
                         CXPU,CXPU_2H,CXPU_4H,CXPU_6H,CXPU_8H,CXPU_10H,CXPU_12H){
  return(2.917*CRBR-0.416*CRBR_2H-0.733*CRBR_4H-0.516*CRBR_6H-0.346*CRBR_8H-
           0.261*CRBR_10H+0.329*CRBR_12H+0.212*CXPU-0.178*CXPU_2H-0.006*CXPU_4H-
           0.073*CXPU_6H+0.035*CXPU_8H+0.489*CXPU_10H-0.435*CXPU_12H)
}

# RIO BRANCO 6H
Cota_RBR_6H <- function(CRBR,CRBR_1H,CRBR_2H,CRBR_3H,CRBR_4H,CRBR_5H,CRBR_6H,
                         CXPU,CXPU_1H,CXPU_2H,CXPU_3H){
  return(1.8873*CRBR+0.1498*CRBR_1H-0.1420*CRBR_2H-0.1262*CRBR_3H-0.1898*CRBR_4H-
           0.1525*CRBR_5H-0.437*CRBR_6H+0.0189*CXPU+0.0196*CXPU_1H+0.0358*CXPU_2H-
           0.0594*CXPU_3H)
}


# XAPURI 18H
Cota_XPU_18H <- function(CXPU,CXPU_3H,CXPU_6H,CXPU_9H,CXPU_12H,
                         CXPU_15H,CBRA,CBRA_3H,CBRA_6H,CBRA_9H,CBRA_18H){
  return(4.084*CXPU-4.46*CXPU_3H+1.293*CXPU_6H+0.125*CXPU_9H-0.323*CXPU_12H+
           0.216*CXPU_15H+1.175*CBRA-0.61*CBRA_3H-0.113*CBRA_6H-0.216*CBRA_9H-
           0.142*CBRA_18H)
}

# XAPURI 12H
Cota_XPU_12H <- function(CXPU,CXPU_2H,CXPU_4H,CXPU_6H,CXPU_8H,CXPU_12H,
                         CBRA,CBRA_2H,CBRA_4H,CBRA_6H,CBRA_8H,CBRA_10H){
  return(5.0126*CXPU-4.5311*CXPU_2H-0.2475*CXPU_4H+0.6991*CXPU_6H-0.0245*CXPU_8H+
           0.0436*CXPU_12H+0.4869*CBRA-0.1691*CBRA_2H+0.0072*CBRA_4H+0.1146*CBRA_6H+
           0.0547*CBRA_8H-0.4261*CBRA_10H)
}

# XAPURI 6H
Cota_XPU_6H <- function(CXPU,CXPU_1H,CXPU_2H,CXPU_4H,CXPU_5H,
                        CBRA,CBRA_1H){
  return(4.9456*CXPU-2.2562*CXPU_1H-1.9854*CXPU_2H-0.5492*CXPU_4H+0.816*CXPU_5H+
           0.4297*CBRA-0.3874*CBRA_1H)
}

###
### SAH MADEIRA
###

# Ests %>%  filter(str_detect(Codigo, "^15"))
Modelos <- c(Modelos, 15400000, 15318000, 15326000)

# Completo: 30 horas PV = 1.022 * (JJB+MN) + 327.222
# PORTO VELHO 30H
Vazao_PVH_30H_1 <- function(QMNJ, QJJB){
  1.022 * (QMNJ+QJJB) + 327.222
}


Vazao_PVH_30H_2 <- function(QJJB){
  1.09 * (QJJB) - 71.78
}

# PORTO VELHO 18H
Cota_PVH <- function(CPVL, CJJP){
  
  H <- c(-132.2084 + 0.9183 *  CPVL + 0.1574 * CJJP,
         -263.7869 + 0.8369 *  CPVL + 0.3141 * CJJP,
         -386.8603 + 0.7606 *  CPVL + 0.4609 * CJJP,
         -501.4529 + 0.6894 *  CPVL + 0.5977 * CJJP,
         -566.8211 + 0.6488 *  CPVL + 0.6756 * CJJP,
         -626.7778 + 0.6115 *  CPVL + 0.7471 * CJJP,
         -679.5839 + 0.5783 *  CPVL + 0.8104 * CJJP,
         -723.3222 + 0.5505 *  CPVL + 0.8631 * CJJP,
         -760.1787 + 0.5271 *  CPVL + 0.9073 * CJJP,
         -788.9693 + 0.5086 *  CPVL + 0.9420 * CJJP,
         -814.2375 + 0.4919 *  CPVL + 0.9729 * CJJP,
         -836.1171 + 0.4772 *  CPVL + 0.9997 * CJJP)
  return(H)

}


# PORTO VELHO 24H
Cota_PVH_24_1 <- function(x1, x2, a1 = 1.014, a2 = -0.059){
  a1 * x1 +  a2 * x2
}

# PORTO VELHO 48H
Cota_PVH_48H <- function(x1, x2, a1 = 1.019, a2 = -0.098){
  a1 * x1 +  a2 * x2
}

# PORTO VELHO 72H
Cota_PVH_72H <- function(x1, x2, a1 = 1.005, a2 = -0.100){
  a1 * x1 +  a2 * x2
}

###
### SAH SÃO FRANCISCO
###

# Ests %>%  filter(str_detect(Codigo, "^46"))
Modelos <- c(Modelos, 41135000, 41020002, 41090002, 42210000, 43200000,
             44200000, 44290003, 44500000, 45298000, 45480000,
             45960001, 46360000)

# SÃO FRANCISCO 16H
Vazao_SAF_16H <- function(QSRM, QSRM_3,QSAF, QSAF_3){
  1.0011 * QSAF + 3.5471 * (QSRM-QSRM_3) + 0.7649 * (QSAF - QSAF_3)
}

# PEDRA DE MARIA DA CRUZ 15H
Vazao_PMC_15H <- function(QSAF){
  0.87236 * QSAF + 523.34
}

# MANGA 26H
Vazao_MAN_26H_1 <- function(QPMC){
  1.0636 * QPMC
}
Vazao_MAN_26H_2 <- function(QMAN, QPMC, QPMC_26){
  QMAN + 1.0326 * (QPMC - QPMC_26)
}

# CARINHANHA
Vazao_CAR_14H_1 <- function(QMAN){
  0.8405 * QMAN + 265.993
}

Vazao_CAR_14H_2 <- function(QCAR,QMAN,QMAN_14){
  QCAR + 0.6617 * (QMAN + QMAN_14)
}

# SÃO ROMÃO
Vazao_SAR_9H_1 <- function(QCDM, QCDM_3, QSRM, QSRM_3){
  0.9997 * QSRM + 1.944 * (QCDM - QCDM_3) + 1.3674 * (QSRM - QSRM_3)
}

Vazao_SAR_9H_2 <- function(QCDM, QPOA_7){
  1.22997 *  QCDM + 0.5815686 * QPOA_7
}

Vazao_SAR_9H_3 <- function(QSAR, QCDM, QCDM_9, QPOA, QPOA_16){
  QSAR + 1.13794 * (QCDM - QCDM_9) + 0.110226 * (QPOA - QPOA_16)
}

# CACHOEIRA MANTEIGA
VAZAO_CDM_34H <- function(QPIR, QPIR_3, QCDM, QCDM_3){
  0.3415 * QPIR + 0.7927 * QCDM + 3.2757 * (QPIR - QPIR_3) + 5.8408 * (QCDM - QCDM_3) - 300
}

# PIRAPORA
VAZAO_PIR_20H_1 <- function(QUTM, QSPP_1){
  0.92076 * QUTM + 1.06553 * QSPP_1 + 119.08575 + 100
}

VAZAO_PIR_20H_2 <- function(QPIR, QUTM, QUTM_20, QSPP, QSPP_20){
  QPIR + 0.1841 * (QUTM - QUTM_20) + 1.0192 * (QSPP - QSPP_20) 
}

# JUAZEIRO
VAZAO_JZR_16H <- function(QJZR,QSOB,QSOB_16){
  QJZR + (QSOB - QSOB_16)
}


# BOM JESUS DA LAPA
COTA_BJL_24H <- function(CBJL,CBJL_24,CCAR,CCAR_24){
  CBJL + 0.414 * (CCAR - CCAR_24) + 0.525 * (CBJL - CBJL_24)
}

COTA_BJL_48H <- function(CBJL,CBJL_48,CCAR,CCAR_48){
  CBJL - 0.125 * (CCAR - CCAR_48) + 0.998 * (CBJL - CBJL_48)
}

# CARINHANHA
VAZAO_CARI_24H <- function(QCAR,QCAR_24,QMAN,QMAN_24){
  QCAR + 0.742 * (QCAR - QCAR_24) + 0.215 * (QMAN - QMAN_24)
}

# PEDRAS DE MARIA DA CRUZ
VAZAO_PMC_15H <- function(QSAF){
  0.87236 * QSAF + 523.34
}

# MORPARA
VAZAO_MPRA_72H <- function(QBJL,QBJL_3,QPN,QPN_3,QMORP){
  QS_3 <- QBJL_3 + QPN_3
  QS <- QBJL + QPN
  QMORP + 1.077 * (QS - QS_3)
}


Modelos <- c(Modelos, 66825000, 67100000, 66070004, 66970000, 66125000)