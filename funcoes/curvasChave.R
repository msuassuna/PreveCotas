###
### SAH PARAGUAI
###

#### Cáceres ####
curvaChave_NQ_CACERES <- function(N){
  if(15 <= N & N < 320){
    N <- N/100
    a <- 92.98
    h0 <- -0.89
    n <- 1.328
    return(a * (N - h0) ^ n)
  } else if (320 <= N & N <= 600) {
    N <- N/100
    a <- 4.3461
    h0 <- -2.36
    n <- 2.876
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_CACERES(15)
curvaChave_QN_CACERES <- function(Q){
  if(97.95121 <= Q & Q < 603.858){
    a <- 92.98
    h0 <- -0.89
    n <- 1.328
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (603.858 <= Q & Q <= 1951.484) {
    a <- 4.3461
    h0 <- -2.36
    n <- 2.876
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_CACERES(97.95121)

#### Cuiabá ####
curvaChave_NQ_CUIABA <- function(N){
  if(-30 <= N & N < 211){
    N <- N/100
    a <- 30.0318
    h0 <- -1.83
    n <- 2.12
    return(a * (N - h0) ^ n)
  } else if (211 <= N & N <= 1100) {
    N <- N/100
    a <- 44.5381
    h0 <- -2.16
    n <- 1.731
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_CUIABA(-30)
curvaChave_QN_CUIABA <- function(Q){
  if(73.98219 <= Q & Q < 549.5438){
    a <- 30.0318
    h0 <- -1.83
    n <- 2.12
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (549.5438 <= Q & Q <= 3856.236) {
    a <- 44.5381
    h0 <- -2.16
    n <- 1.731
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_CUIABA(3856.236)

#### Cuiabá ####

#### Coxim ####
curvaChave_NQ_COXIM <- function(N){
  if(340 <= N & N < 470){
    N <- N/100
    a <- 16.25
    h0 <- 1.27
    n <- 2.9
    return(a * (N - h0) ^ n)
  } else if (470 <= N & N <= 520) {
    N <- N/100
    a <- 16.277
    h0 <- 1.89
    n <- 3.456
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_COXIM(520)
curvaChave_QN_COXIM <- function(Q){
  if(145.5976 <= Q & Q < 578.5001){
    a <- 16.25
    h0 <- 1.27
    n <- 2.9
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (578.5001 <= Q & Q <= 1018.826) {
    a <- 16.277
    h0 <- 1.89
    n <- 3.456
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_COXIM(1018.826)

#### Coxim ####

#### Miranda ####
curvaChave_NQ_MIRANDA <- function(N){
  if(110 <= N & N < 274){
    N <- N/100
    a <- 16.43
    h0 <- 0
    n <- 1.297
    return(a * (N - h0) ^ n)
  } else if (274 <= N & N <= 552) {
    N <- N/100
    a <- 12.2982
    h0 <- -0.16
    n <- 1.5
    return(a * (N - h0) ^ n)
  } else if (552 <= N & N <= 870) {
    N <- N/100
    a <- 107.4931
    h0 <- 4.12
    n <- 1.3
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_MIRANDA(870)
curvaChave_QN_MIRANDA <- function(Q){
  if(18.5919 <= Q & Q < 60.73493){
    a <- 16.43
    h0 <- 0
    n <- 1.297
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (60.73493 <= Q & Q <= 166.4808) {
    a <- 12.2982
    h0 <- -0.16
    n <- 1.5
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (166.4808 <= Q & Q <= 777.1515) {
    a <- 107.4931
    h0 <- 4.12
    n <- 1.3
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_MIRANDA(777.1515)

#### Miranda ####

#### Aquidauana ####
curvaChave_NQ_AQUIDAUANA <- function(N){
  if(150 <= N & N < 251){
    N <- N/100
    a <- 23.6219
    h0 <- 0.23
    n <- 1.203
    return(a * (N - h0) ^ n)
  } else if (251 <= N & N <= 660) {
    N <- N/100
    a <- 47.7909
    h0 <- 1.24
    n <- 1.2
    return(a * (N - h0) ^ n)
  } else if (660 <= N & N <= 850) {
    N <- N/100
    a <- 42.334
    h0 <- 2.8
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else if (850 <= N & N <= 1060) {
    N <- N/100
    a <- 83
    h0 <- 2.69
    n <- 1.2
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_AQUIDAUANA(1060)
curvaChave_QN_AQUIDAUANA <- function(Q){
  if(31.49131 <= Q & Q < 63.66631){
    a <- 23.6219
    h0 <- 0.23
    n <- 1.203
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (63.66631 <= Q & Q <= 358.3793) {
    a <- 47.7909
    h0 <- 1.24
    n <- 1.2
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (358.3793 <= Q & Q <= 685.6288) {
    a <- 42.334
    h0 <- 2.8
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (685.6288 <= Q & Q <= 992.8642) {
    a <- 83
    h0 <- 2.69
    n <- 1.2
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_AQUIDAUANA(992.8642)

#### Aquidauana ####

#### Porto Murtinho ####
curvaChave_NQ_PM <- function(N){
  if(50 <= N & N < 349){
    N <- N/100
    a <- 37.717
    h0 <- -2.82
    n <- 2.069
    return(a * (N - h0) ^ n)
  } else if (349 <= N & N <= 638) {
    N <- N/100
    a <- 1.893
    h0 <- -6.61
    n <- 2.942
    return(a * (N - h0) ^ n)
  } else if (638 <= N & N <= 680) {
    N <- N/100
    a <- 2.355
    h0 <- -4.39
    n <- 3.082
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_PM(680)
curvaChave_QN_PM <- function(Q){
  if(451.6186 <= Q & Q < 1705.549){
    a <- 37.717
    h0 <- -2.82
    n <- 2.069
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1705.549 <= Q & Q <= 3575.928) {
    a <- 1.893
    h0 <- -6.61
    n <- 2.942
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (3575.928 <= Q & Q <= 4022.4) {
    a <- 2.355
    h0 <- -4.39
    n <- 3.082
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_PM(4022.4)

#### Porto Murtinho ####

###
### SAH BRANCO
###

# Curvas-chave Rio Madeira

#### Caracaraí ####
curvaChave_NQ_CARACARAI <- function(N){
  if(-30 <= N & N < 584){
    N <- N/100
    a <- 377.331
    h0 <- -0.49
    n <- 1.47
    return(a * (N - h0) ^ n)
  } else if (584 <= N & N <= 1150) {
    N <- N/100
    a <- 32.747
    h0 <- -3.47
    n <- 2.31
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_CARACARAI(1150)
curvaChave_QN_CARACARAI <- function(Q){
  if(32.84659 <= Q & Q < 5668.202){
    a <- 377.331
    h0 <- -0.49
    n <- 1.47
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (5668.202 <= Q & Q <= 16979.85) {
    a <- 32.747
    h0 <- -3.47
    n <- 2.31
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}

#### Boa Vista ####
curvaChave_NQ_BOAVISTA <- function(N){
  if(-70 <= N & N <= 1100){
    N <- N/100
    a <- 139.73
    h0 <- -1.47
    n <- 1.897
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_BOAVISTA(1100)
curvaChave_QN_BOAVISTA <- function(Q){
  if(85.10647 <= Q & Q < 16755.15){
    a <- 139.73
    h0 <- -1.47
    n <- 1.897
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}

###
### SAH ACRE
###

#### Rio Branco ####
curvaChave_NQ_RB <- function(N){
  if(100 <= N & N < 1050){
    N <- N/100
    a <- 14.3751
    h0 <- -0.08
    n <- 1.748
    return(a * (N - h0) ^ n)
  } else if (1050 <= N & N <= 2000) {
    N <- N/100
    a <- 0.0549
    h0 <- -6.65
    n <- 3.41
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}

curvaChave_QN_RB <- function(Q){
  if(16.44508 <= Q & Q < 888){
    a <- 14.3751
    h0 <- -0.08
    n <- 1.748
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (888 <= Q & Q <= 3992.086) {
    a <- 0.0549
    h0 <- -6.65
    n <- 3.41
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}


###
### SAH MADEIRA
###

# Curvas-chave Rio Madeira

#### Porto Velho ####
curvaChave_NQ_PV <- function(N){
  if(120 <= N & N < 1700){
    N <- N/100
    a <- 109
    h0 <- -3.7
    n <- 1.969
    return(a * (N - h0) ^ n)
  } else if (1700 <= N & N <= 1977) {
    N <- N/100
    a <- 1455.3913
    h0 <- 5.99
    n <- 1.407
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}

curvaChave_QN_PV <- function(Q){
  if(2491.28 <= Q & Q < 42537.75){
    a <- 109
    h0 <- -3.7
    n <- 1.969
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (42537.75 <= Q & Q <= 58331.54) {
    a <- 1455.3913
    h0 <- 5.99
    n <- 1.407
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}


#### Morada Nova Jusante ####
curvaChave_NQ_MNJ <- function(N){
  if(650 <= N & N < 871){
    N <- N/100
    a <- 10.5791
    h0 <- 5.43
    n <- 1.8
    return(a * (N - h0) ^ n)
  } else if (871 <= N & N <= 987) {
    N <- N/100
    a <- 0.9821
    h0 <- 4.5
    n <- 3.141
    return(a * (N - h0) ^ n)
  } else if (987 <= N & N <= 1900) {
    N <- N/100
    a <- 4.345
    h0 <- 6.33
    n <- 3
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}

curvaChave_NQ_MNJ(1900)

curvaChave_QN_MNJ <- function(Q){
  if(11.94922 <= Q & Q < 89.74821){
    a <- 10.5791
    h0 <- 5.43
    n <- 1.8
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (89.74821 <= Q & Q <= 192.7547) {
    a <- 0.9821
    h0 <- 4.5
    n <- 3.141
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (192.7547 <= Q & Q <= 8837.301) {
    a <- 4.345
    h0 <- 6.33
    n <- 3
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_MNJ(8837.301)
###
### SAH SÃO FRANCISCO
###

#### SÃO FRANCISCO ####
curvaChave_NQ_SAOF <- function(N){
  if(110 <= N & N < 800){
    N <- N/100
    a <- 192.299
    h0 <- -0.05
    n <- 1.518
    return(a * (N - h0) ^ n)
  } else if (800 <= N & N <= 1250) {
    N <- N/100
    a <- 570.437
    h0 <- 3.8
    n <- 1.448
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_SAOF(1250)
curvaChave_QN_SAOF <- function(Q){
  if(237.7477 <= Q & Q < 4556.933){
    a <- 192.299
    h0 <- -0.05
    n <- 1.518
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (4556.933 <= Q & Q <= 13080.72) {
    a <- 570.437
    h0 <- 3.8
    n <- 1.448
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_SAOF(8200)

#### PEDRA DE MARIA DA CRUZ 15H ####
curvaChave_NQ_PMC <- function(N){
  if(50 <= N & N < 830){
    N <- N/100
    a <- 144.668
    h0 <- -0.75
    n <- 1.605
    return(a * (N - h0) ^ n)
  } else if (830 <= N & N <= 1050) {
    N <- N/100
    a <- 626.421
    h0 <- 4.39
    n <- 1.518
    return(a * (N - h0) ^ n)
  } else if (1050 <= N & N <= 1250) {
    N <- N/100
    a <- 392.5684
    h0 <- 3.04
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_PMC(1250)
curvaChave_QN_PMC <- function(Q){
  if(206.9727 <= Q & Q < 4963.53){
    a <- 144.668
    h0 <- -0.75
    n <- 1.605
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (4963.53 <= Q & Q <= 9774.102) {
    a <- 626.421
    h0 <- 4.39
    n <- 1.518
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (9774.102 <= Q & Q <= 14300.17) {
    a <- 392.5684
    h0 <- 3.04
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_PMC(14300.17)


#### MANGA ####

curvaChave_NQ_MAN <- function(N){
  if(76 <= N & N < 450){
    N <- N/100
    a <- 242.588
    h0 <- -0.22
    n <- 1.4
    return(a * (N - h0) ^ n)
  } else if (450 <= N & N <= 1200) {
    N <- N/100
    a <- 97.5
    h0 <- -0.69
    n <- 1.872
    return(a * (N - h0) ^ n)
  } else if (1200 <= N & N <= 1350) {
    N <- N/100
    a <- 227.6
    h0 <- 0
    n <- 1.573
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_MAN(1050)
curvaChave_QN_MAN <- function(Q){
  if(235.8228 <= Q & Q < 2127.149){
    a <- 242.588
    h0 <- -0.22
    n <- 1.4
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (2127.149 <= Q & Q <= 11341.87) {
    a <- 97.5
    h0 <- -0.69
    n <- 1.872
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (11341.87 <= Q & Q <= 13651.72) {
    a <- 227.6
    h0 <- 0
    n <- 1.573
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_MAN(8962.198)


#### CARINHANHA ####

curvaChave_NQ_CAR <- function(N){
  if(20 <= N & N < 220){
    N <- N/100
    a <- 193.5471
    h0 <- -0.9
    n <- 1.634
    return(a * (N - h0) ^ n)
  } else if (220 <= N & N <= 930) {
    N <- N/100
    a <- 33.8351
    h0 <- -2.9
    n <- 2.205
    return(a * (N - h0) ^ n)
  } else if (930 <= N & N <= 1100) {
    N <- N/100
    a <- 613.9999
    h0 <- 5.86
    n <- 2.117
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_CAR(1100)
curvaChave_QN_CAR <- function(Q){
  if(226.1634 <= Q & Q < 1229.022){
    a <- 193.5471
    h0 <- -0.9
    n <- 1.634
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1229.022 <= Q & Q <= 8409.901) {
    a <- 33.8351
    h0 <- -2.9
    n <- 2.205
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (8409.901 <= Q & Q <= 19646.14) {
    a <- 613.9999
    h0 <- 5.86
    n <- 2.117
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_CAR(226.1634)




#### SÃO ROMÃO ####
curvaChave_NQ_SAR <- function(N){
  if(100 <= N & N < 322){
    N <- N/100
    a <- 181.342
    h0 <- -0.06
    n <- 1.599
    return(a * (N - h0) ^ n)
  } else if (322 <= N & N <= 676) {
    N <- N/100
    a <- 255.4036
    h0 <- 0.49
    n <- 1.55
    return(a * (N - h0) ^ n)
  } else if (676 <= N & N <= 1000) {
    N <- N/100
    a <- 442.2627
    h0 <- 0.91
    n <- 1.3
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_SAR(1000)
curvaChave_QN_SAR <- function(Q){
  if(199.0501 <= Q & Q < 1211.376){
    a <- 181.342
    h0 <- -0.06
    n <- 1.599
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1211.376 <= Q & Q <= 4395.332) {
    a <- 255.4036
    h0 <- 0.49
    n <- 1.55
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (4395.332 <= Q & Q <= 7794.95) {
    a <- 442.2627
    h0 <- 0.91
    n <- 1.3
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_SAR(7794.95)


#### CACHOEIRA MANTEIGA ####
curvaChave_NQ_CDM <- function(N){
  if(80 <= N & N < 150){
    N <- N/100
    a <- 165.538
    h0 <- -0.35
    n <- 1.42
    return(a * (N - h0) ^ n)
  } else if (150 <= N & N <= 500) {
    N <- N/100
    a <- 88.551
    h0 <- -1.04
    n <- 1.608
    return(a * (N - h0) ^ n)
  } else if (500 <= N & N <= 1153) {
    N <- N/100
    a <- 139.995
    h0 <- 0.51
    n <- 1.621
    return(a * (N - h0) ^ n)
  } else if (1153 <= N & N <= 1350) {
    N <- N/100
    a <- 273.0296
    h0 <- 4.04
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_CDM(80)
curvaChave_QN_CDM <- function(Q){
  if(201.8778 <= Q & Q < 396.4296){
    a <- 165.538
    h0 <- -0.35
    n <- 1.42
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (396.4296 <= Q & Q <= 1596.25) {
    a <- 88.551
    h0 <- -1.04
    n <- 1.608
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1596.25 <= Q & Q <= 6846.82) {
    a <- 139.995
    h0 <- 0.51
    n <- 1.621
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (6846.82 <= Q & Q <= 9945.704) {
    a <- 273.0296
    h0 <- 4.04
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_CDM(9945.704)

#### PIRAPORA ####
curvaChave_NQ_PIR <- function(N){
  if(110 <= N & N < 200){
    N <- N/100
    a <- 277.416
    h0 <- 0.55
    n <- 1.739
    return(a * (N - h0) ^ n)
  } else if (200 <= N & N <= 300) {
    N <- N/100
    a <- 64.429
    h0 <- -0.1
    n <- 2.839
    return(a * (N - h0) ^ n)
  } else if (300 <= N & N <= 500) {
    N <- N/100
    a <- 1076.1799
    h0 <- 1.68
    n <- 1.426
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_PIR(388)
curvaChave_QN_PIR <- function(Q){
  if(98.08943 <= Q & Q < 529.4951){
    a <- 277.416
    h0 <- 0.55
    n <- 1.739
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (529.4951 <= Q & Q <= 1599.769) {
    a <- 64.429
    h0 <- -0.1
    n <- 2.839
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1599.769 <= Q & Q <= 5957.003) {
    a <- 1076.1799
    h0 <- 1.68
    n <- 1.426
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_PIR(3283.576)


#### JUAZEIRO ####

curvaChave_NQ_JUA <- function(N){
  if(73 <= N & N < 347){
    N <- N/100
    a <- 460.2134
    h0 <- -0.16
    n <- 1.632
    return(a * (N - h0) ^ n)
  } else if (347 <= N & N <= 650) {
    N <- N/100
    a <- 4.9987
    h0 <- -5.64
    n <- 3
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_JUA(650)
curvaChave_QN_JUA <- function(Q){
  if(380.508 <= Q & Q < 3779.307){
    a <- 460.2134
    h0 <- -0.16
    n <- 1.632
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (3779.307 <= Q & Q <= 8943.616) {
    a <- 4.9987
    h0 <- -5.64
    n <- 3
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_JUA(380.508)

#### SANTA MARIA DA BOA VISTA ####

curvaChave_NQ_SAN <- function(N){
  if(10 <= N & N < 202){
    N <- N/100
    a <- 72.4587819603533
    h0 <- -1.96
    n <- 2.597
    return(a * (N - h0) ^ n)
  } else if (202 <= N & N <= 451) {
    N <- N/100
    a <- 636.8981
    h0 <- -0.4
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else if (451 <= N & N <= 650) {
    N <- N/100
    a <- 1233.5022
    h0 <- 1.26
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_SAN(650)
curvaChave_QN_SAN <- function(Q){
  if(473.3738 <= Q & Q < 2619.235){
    a <- 72.4587819603533
    h0 <- -1.96
    n <- 2.597
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (2619.235 <= Q & Q <= 8124.569) {
    a <- 636.8981
    h0 <- -0.4
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (8124.569 <= Q & Q <= 17461.04) {
    a <- 1233.5022
    h0 <- 1.26
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_SAN(17461.04)


#### MATA DE SAO JOSE ####

curvaChave_NQ_MAT <- function(N){
  if(300 <= N & N <= 500) {
    N <- N/100
    a <- 354.470331075841
    h0 <- 1.96
    n <- 2.184
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_MAT(300)
curvaChave_QN_MAT <- function(Q){
  if(386.1719 <= Q & Q <= 4019.535) {
    a <- 354.470331075841
    h0 <- 1.96
    n <- 2.184
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_MAT(4019.535)


#### IBO ####

curvaChave_NQ_IBO <- function(N){
  if(90 <= N & N < 206){
    N <- N/100
    a <- 31.0243096763812
    h0 <- -1.33
    n <- 3.416
    return(a * (N - h0) ^ n)
  } else if (206 <= N & N <= 451) {
    N <- N/100
    a <- 626.4627
    h0 <- -0.01
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else if (451 <= N & N <= 650) {
    N <- N/100
    a <- 532.7975
    h0 <- -0.49
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_IBO(650)
curvaChave_QN_IBO <- function(Q){
  if(480.2988 <= Q & Q < 2006.54){
    a <- 31.0243096763812
    h0 <- -1.33
    n <- 3.416
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (2006.54 <= Q & Q <= 7000.301) {
    a <- 626.4627
    h0 <- -0.01
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (7000.301 <= Q & Q <= 11959.85) {
    a <- 532.7975
    h0 <- -0.49
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_IBO(480.2988)


#### PIRANHAS ####

curvaChave_NQ_PIRA <- function(N){
  if(-200 <= N & N < 208){
    N <- N/100
    a <- 1.73
    h0 <- -8.35
    n <- 2.95
    return(a * (N - h0) ^ n)
  } else if (208 <= N & N <= 621) {
    N <- N/100
    a <- 5.7914
    h0 <- -6.72
    n <- 2.625
    return(a * (N - h0) ^ n)
  } else if (621 <= N & N <= 950) {
    N <- N/100
    a <- 3.16
    h0 <- -7.56
    n <- 2.793
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_PIRA(950)
curvaChave_QN_PIRA <- function(Q){
  if(403.8579 <= Q & Q < 1746.025){
    a <- 1.73
    h0 <- -8.35
    n <- 2.95
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1746.025 <= Q & Q <= 4794.349) {
    a <- 5.7914
    h0 <- -6.72
    n <- 2.625
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (4794.349 <= Q & Q <= 8721.767) {
    a <- 3.16
    h0 <- -7.56
    n <- 2.793
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_PIRA(403.8579)


#### PAO DE ACUCAR ####

curvaChave_NQ_PAO <- function(N){
  if(-20 <= N & N < 151){
    N <- N/100
    a <- 18.8709989626696
    h0 <- -3.46
    n <- 2.786
    return(a * (N - h0) ^ n)
  } else if (151 <= N & N <= 400) {
    N <- N/100
    a <- 220.7281
    h0 <- -1.97
    n <- 1.61
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_PAO(400)
curvaChave_QN_PAO <- function(Q){
  if(507.714 <= Q & Q < 1643.615){
    a <- 18.8709989626696
    h0 <- -3.46
    n <- 2.786
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1643.615 <= Q & Q <= 3919.009) {
    a <- 220.7281
    h0 <- -1.97
    n <- 1.61
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_PAO(3919.009)


#### TRAIPU ####

curvaChave_NQ_TRA <- function(N){
  if(0 <= N & N < 300){
    N <- N/100
    a <- 2.03914909615011
    h0 <- -4.72
    n <- 3.537
    return(a * (N - h0) ^ n)
  } else if (300 <= N & N <= 700) {
    N <- N/100
    a <- 308.407
    h0 <- -0.98
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_TRA(700)
curvaChave_QN_TRA <- function(Q){
  if(493.3803 <= Q & Q < 2811.494){
    a <- 2.03914909615011
    h0 <- -4.72
    n <- 3.537
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (2811.494 <= Q & Q <= 8557.144) {
    a <- 308.407
    h0 <- -0.98
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_TRA(493.3803)

#### PROPIA ####

curvaChave_NQ_PRO <- function(N){
  if(0 <= N & N < 171){
    N <- N/100
    a <- 50.9709578396728
    h0 <- -2.39
    n <- 2.541
    return(a * (N - h0) ^ n)
  } else if (171 <= N & N <= 254) {
    N <- N/100
    a <- 180.8408
    h0 <- -2.55
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else if (254 <= N & N <= 426) {
    N <- N/100
    a <- 153.5527
    h0 <- -1.45
    n <- 2
    return(a * (N - h0) ^ n)
  } else if (426 <= N & N <= 550) {
    N <- N/100
    a <- 294.6573
    h0 <- -1.84
    n <- 1.566
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_PRO(550)
curvaChave_QN_PRO <- function(Q){
  if(466.4787 <= Q & Q < 1838.027){
    a <- 50.9709578396728
    h0 <- -2.39
    n <- 2.541
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1838.027 <= Q & Q <= 2443.683) {
    a <- 180.8408
    h0 <- -2.55
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (2443.683 <= Q & Q <= 5006.448) {
    a <- 153.5527
    h0 <- -1.45
    n <- 2
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (5006.448 <= Q & Q <= 6683.397) {
    a <- 294.6573
    h0 <- -1.84
    n <- 1.566
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_PRO(466.4787)


#### MORPARA ####

curvaChave_NQ_MPR <- function(N){
  if(130 <= N & N < 484){
    N <- N/100
    a <- 265.6
    h0 <- 0.44
    n <- 1.45
    return(a * (N - h0) ^ n)
  } else if (484 <= N & N <= 791) {
    N <- N/100
    a <- 14.3
    h0 <- -1.94
    n <- 2.649
    return(a * (N - h0) ^ n)
  } else if (791 <= N & N <= 850) {
    N <- N/100
    a <- 514.7998
    h0 <- 3.21
    n <- 1.6
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_MPR(850)
curvaChave_QN_MPR <- function(Q){
  if(213.4277 <= Q & Q < 2276.478){
    a <- 265.6
    h0 <- 0.44
    n <- 1.45
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (2276.478 <= Q & Q <= 6122.779) {
    a <- 14.3
    h0 <- -1.94
    n <- 2.649
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (6122.779 <= Q & Q <= 7398.905) {
    a <- 514.7998
    h0 <- 3.21
    n <- 1.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_MPR(7398.905)


#### SANTO HIPÓLITO ####

curvaChave_NQ_SHPT <- function(N){
  if(45 <= N & N < 150){
    N <- N/100
    a <- 88.5833
    h0 <- 0.16
    n <- 1.4749
    return(a * (N - h0) ^ n)
  } else if (150 <= N & N <= 350) {
    N <- N/100
    a <- 88.3596
    h0 <- 0.15
    n <- 1.4468
    return(a * (N - h0) ^ n)
  } else if (350 <= N & N <= 1100) {
    N <- N/100
    a <- 197.0631
    h0 <- 1.0786
    n <- 1.0708
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_SHPT(1100)
curvaChave_QN_SHPT <- function(Q){
  if(14.27061 <= Q & Q < 136.402){
    a <- 88.5833
    h0 <- 0.16
    n <- 1.4749
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (136.402 <= Q & Q <= 508.0291) {
    a <- 88.3596
    h0 <- 0.15
    n <- 1.4468
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (508.0291 <= Q & Q <= 2300.041) {
    a <- 197.0631
    h0 <- 1.0786
    n <- 1.0708
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_SHPT(2300.041)




###
### SAH DOCE
###

#### Governador Valadares ####
curvaChave_NQ_GVALADARES <- function(N){
  if(70 <= N & N < 268){
    N <- N/100
    a <- 326.155
    h0 <- 0.51
    n <- 1.5146
    return(a * (N - h0) ^ n)
  } else if (268 <= N & N <= 378) {
    N <- N/100
    a <- 360.29
    h0 <- 0.8
    n <- 1.7011
    return(a * (N - h0) ^ n)
  } else if (378 <= N & N <= 700) {
    N <- N/100
    a <- 311.885
    h0 <- 0.45
    n <- 1.664
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_GVALADARES(700)
curvaChave_QN_GVALADARES <- function(Q){
  if(26.36479 <= Q & Q < 1054.441){
    a <- 326.155
    h0 <- 0.51
    n <- 1.5146
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1054.441 <= Q & Q <= 2308.562) {
    a <- 360.29
    h0 <- 0.8
    n <- 1.7011
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (2308.562 <= Q & Q <= 7115.739) {
    a <- 311.885
    h0 <- 0.45
    n <- 1.664
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_GVALADARES(7115.739)
#### Governador Valadares ####


#### Colatina ####
curvaChave_NQ_COLATINA <- function(N){
  if(60 <= N & N < 350){
    N <- N/100
    a <- 326.674
    h0 <- 0.39
    n <- 1.478
    return(a * (N - h0) ^ n)
  } else if (350 <= N & N <= 600) {
    N <- N/100
    a <- 387.029
    h0 <- 0.48
    n <- 1.364
    return(a * (N - h0) ^ n)
  } else if (600 <= N & N <= 1000) {
    N <- N/100
    a <- 455.256
    h0 <- 0.7
    n <- 1.3
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_COLATINA(1000)
curvaChave_QN_COLATINA <- function(Q){
  if(32.53529 <= Q & Q < 1747.725){
    a <- 326.674
    h0 <- 0.39
    n <- 1.478
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1747.725 <= Q & Q <= 3978.766) {
    a <- 387.029
    h0 <- 0.48
    n <- 1.364
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (3978.766 <= Q & Q <= 8265.774) {
    a <- 455.256
    h0 <- 0.7
    n <- 1.3
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_COLATINA(3978.766)
#### Colatina ####


###
### SAH URUGUAI
###

#### Alegrete ####
curvaChave_NQ_ALEGRETE <- function(N){
  if(20 <= N & N < 107){
    N <- N/100
    a <- 5.7602
    h0 <- -0.1
    n <- 4.152
    return(a * (N - h0) ^ n)
  } else if (107 <= N & N <= 1268) {
    N <- N/100
    a <- 18.8232
    h0 <- 0.32
    n <- 1.631
    return(a * (N - h0) ^ n)
  } else if (1268 <= N & N <= 1422) {
    N <- N/100
    a <- 5.4351
    h0 <- -0.1
    n <- 2.095
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_ALEGRETE(1422)
curvaChave_QN_ALEGRETE <- function(Q){
  if(0.03885483 <= Q & Q < 11.77385){
    a <- 5.7602
    h0 <- -0.1
    n <- 4.152
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (11.77385 <= Q & Q <= 1137.04) {
    a <- 18.8232
    h0 <- 0.32
    n <- 1.631
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (1137.04 <= Q & Q <= 1435.186) {
    a <- 5.4351
    h0 <- -0.1
    n <- 2.095
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_ALEGRETE(1435.186)
#### Alegrete ####


###
### SAH MURIÁE
###

#### Cardoso Moreira ####
curvaChave_NQ_CMOREIRA <- function(N){
  if(10 <= N & N < 48){
    N <- N/100
    a <- 48.1684
    h0 <- -0.19
    n <- 2.6
    return(a * (N - h0) ^ n)
  } else if (48 <= N & N <= 176) {
    N <- N/100
    a <- 25.1531
    h0 <- -0.33
    n <- 1.858
    return(a * (N - h0) ^ n)
  } else if (176 <= N & N <= 811) {
    N <- N/100
    a <- 28.6536
    h0 <- -0.5
    n <- 1.52
    return(a * (N - h0) ^ n)
  } else if (811 <= N & N <= 1000) {
    N <- N/100
    a <- 64.0532
    h0 <- 4.37
    n <- 1.871
    return(a * (N - h0) ^ n)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_NQ_CMOREIRA(1000)
curvaChave_QN_CMOREIRA <- function(Q){
  if(1.927512 <= Q & Q < 17.00422){
    a <- 48.1684
    h0 <- -0.19
    n <- 2.6
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (17.00422 <= Q & Q <= 98.95171) {
    a <- 25.1531
    h0 <- -0.33
    n <- 1.858
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (98.95171 <= Q & Q <= 755.7601) {
    a <- 28.6536
    h0 <- -0.5
    n <- 1.52
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else if (755.7601 <= Q & Q <= 1624.584) {
    a <- 64.0532
    h0 <- 4.37
    n <- 1.871
    return((((Q/a) ^ (1/n)) + h0)*100)
  } else {
    print("Não existe curva-chave definida na cota informada")
    return(NA)
  }
}
curvaChave_QN_CMOREIRA(1624.584)
#### Cardoso Moreira ####