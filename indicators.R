
## ----, data_loading, echo=FALSE------------------------------------------
mvt  <- read.table("data/mvt.txt", 
                   sep = ",", dec = ".", header = TRUE)
rec  <- read.table("data/rec.txt", 
                   sep = ",", dec = ".", header = TRUE)
ais  <- read.table("data/ais.txt", 
                   sep = ",", dec = ".", header = TRUE)
anim <- read.table("data/anim.txt", 
                   sep = ",", dec = ".", header = TRUE)


## ----, dataset_anim------------------------------------------------------
head(anim)


## ----, dataset_milk_recording--------------------------------------------
head(rec)


## ----, dataset_AI--------------------------------------------------------
head(ais)


## ------------------------------------------------------------------------
head(mvt)


## ----, mvt_causeIN_table-------------------------------------------------
table(mvt$cause_IN)


## ----, mvt_causeOUT_table------------------------------------------------
table(mvt$cause_OUT)


## ----, start_end-dates---------------------------------------------------
start_date <- "2006-01-01"
end_date   <- "2006-12-31"


## ----, lactation_list_create---------------------------------------------
lac <- rec[, c("herdID", "cowID", "date_calv", "parity")]
lac <- lac[-which(duplicated(lac)),]


## ----lactation_select_nullip---------------------------------------------
lac_p1 <- lac[lac$parity == 1, -match("parity", colnames(lac))]


## ----age_calving1_dob----------------------------------------------------
lac_p1 <- merge(lac_p1, anim[, c("cowID", "date_birth")], by = "cowID", all.x = TRUE)


## ----age_calving1--------------------------------------------------------
lac_p1$age_calv1 <- with(lac_p1, as.numeric(as.Date(date_calv) - as.Date(date_birth)))


## ----age_calv1_dist------------------------------------------------------
hist(lac_p1$age_calv1,
     main = "Histogram of ages at first calving",
     xlab = "Age at first calving (days)")


## ----age_calv1_percentiles-----------------------------------------------
quantile(lac_p1$age_calv1, c(.25, .5, .75), na.rm = TRUE)


## ----calv_int_date_pcalv-------------------------------------------------
lac <- lac[order(lac$cowID, as.Date(lac$date_calv)),]
lac$pcowID <- c(NA, lac$cowID[-nrow(lac)])
lac$date_pcalv <- c(NA, as.character(lac$date_calv)[-nrow(lac)])
lac$calv_int <- with(lac, ifelse(cowID == pcowID,
                                 as.numeric(as.Date(date_calv) - as.Date(date_pcalv)),
                                 NA))
lac <- lac[, -match(c("pcowID", "date_pcalv"), colnames(lac))]
lac <- lac[order(lac$herdID, as.Date(lac$date_calv)),]


## ----calv_int_dist-------------------------------------------------------
hist(lac$calv_int,
     main = "Histogram of calving intervals",
     xlab = "Calving intervals (days)")


## ----calv_int_percentiles------------------------------------------------
quantile(lac$calv_int, c(.25, .5, .75), na.rm = TRUE)


## ----scc_threshold-------------------------------------------------------
scc_T <- 200


## ----scc_aboveT_all------------------------------------------------------
with(rec, length(scc[scc > scc_T]) / length(scc))


## ------------------------------------------------------------------------
with(rec[as.Date(rec$date_mr) >= start_date & as.Date(rec$date_mr) <= end_date,], 
           length(scc[scc > scc_T]) / length(scc))


## ------------------------------------------------------------------------
with(rec[as.Date(rec$date_mr) >= start_date & as.Date(rec$date_mr) <= end_date,], 
           tapply(scc, herdID, function(x) length(x[x > scc_T]) / length(x)))


## ----raised_scc_mkvar----------------------------------------------------
rec <- rec[order(rec$cowID, as.Date(rec$date_mr)),]
rec$Dim <- with(rec, as.numeric(as.Date(date_mr) - as.Date(date_calv)))
rec$pcowID   <- c(NA, rec$cowID[-nrow(rec)])
rec$pdate_mr <- c(NA, as.character(rec$date_mr)[-nrow(rec)])
rec$pdate_mr <- with(rec, ifelse(cowID == pcowID, pdate_mr, NA))
rec$pDim     <- c(NA, rec$Dim[-nrow(rec)])
rec$pDim     <- with(rec, ifelse(cowID == pcowID, pDim, NA))
rec$pscc     <- c(NA, rec$scc[-nrow(rec)])
rec$pscc     <- with(rec, ifelse(cowID == pcowID, pscc, NA))

rec <- rec[, -match(c("pcowID", "pdate_mr"), colnames(rec))]


## ----prais_lac_all-------------------------------------------------------
with(rec[!is.na(rec$scc) & !is.na(rec$pscc) & rec$Dim > rec$pDim,],
  length(scc[scc >= scc_T & pscc < scc_T]) / length(scc[pscc < scc_T]))


## ----prais_lac_startend--------------------------------------------------
with(rec[as.Date(rec$date_mr) >= start_date & as.Date(rec$date_mr) <= end_date & !is.na(rec$scc) & !is.na(rec$pscc) & rec$Dim > rec$pDim,],
  length(scc[scc >= scc_T & pscc < scc_T]) / length(scc[pscc < scc_T]))


## ----prais_lac_startend_herd---------------------------------------------
rec$Irais <- rep(NA)

rec$Irais[!is.na(rec$scc) & !is.na(rec$pscc)] <- with(rec[!is.na(rec$scc) & !is.na(rec$pscc),], ifelse(pscc < 200, 0, Irais))

rec$Irais[!is.na(rec$Irais)] <- with(rec[!is.na(rec$Irais),],
                                    ifelse(scc >= 200, 1, 0))

with(rec[!is.na(rec$Irais) & rec$Dim > rec$pDim,], 
     tapply(Irais, herdID, function(x) length(x[x == 1]) / length(x)))


## ----prais_dp_startend_herd----------------------------------------------
with(rec[!is.na(rec$Irais) & rec$Dim < rec$pDim,], 
     tapply(Irais, herdID, function(x) length(x[x == 1]) / length(x)))


## ----pcur_dp_startend_herd-----------------------------------------------
rec$Icur <- rep(NA)

rec$Icur[!is.na(rec$scc) & !is.na(rec$pscc)] <- with(rec[!is.na(rec$scc) & !is.na(rec$pscc),], ifelse(pscc >= 200, 0, Icur))

rec$Icur[!is.na(rec$Icur)] <- with(rec[!is.na(rec$Icur),],
                                    ifelse(scc < 200, 1, 0))

with(rec[!is.na(rec$Icur) & rec$Dim < rec$pDim,], 
     tapply(Icur, herdID, function(x) length(x[x == 1]) / length(x)))


## ----fpr_mk--------------------------------------------------------------
rec$fpr <- with(rec, bfat / prot)


## ----fpr_ket-------------------------------------------------------------
fpr_ket_T     <- 1.4
fpr_ket_lbDim <- 30
fpr_ket_ubDim <- 100


## ----fpr_ket_all---------------------------------------------------------
with(rec[!is.na(rec$fpr) & rec$Dim >= fpr_ket_lbDim & rec$Dim <= fpr_ket_ubDim,], 
     length(fpr[fpr > fpr_ket_T]) / length(fpr))


## ----fpr_ket_startend----------------------------------------------------
with(rec[!is.na(rec$fpr) & rec$Dim >= fpr_ket_lbDim & rec$Dim <= fpr_ket_ubDim,], 
     length(fpr[fpr > fpr_ket_T]) / length(fpr))


## ----fpr_ket_startend_herd-----------------------------------------------
with(rec[!is.na(rec$fpr) & rec$Dim >= fpr_ket_lbDim & rec$Dim <= fpr_ket_ubDim,], 
     tapply(fpr, herdID, function(x) length(x[x > fpr_ket_T]) / length(x)))


## ----fpr_sara------------------------------------------------------------
fpr_sara_T <- 1.0


## ----fpr_sara_all--------------------------------------------------------
with(rec[!is.na(rec$fpr) ,], 
     length(fpr[fpr < fpr_sara_T]) / length(fpr))


## ----fpr_sara_startend---------------------------------------------------
with(rec[!is.na(rec$fpr) & as.Date(rec$date_mr) >= start_date & as.Date(rec$date_mr) <= end_date, ], 
     length(fpr[fpr < fpr_sara_T]) / length(fpr))


## ----fpr_sara_startend_herd----------------------------------------------
with(rec[!is.na(rec$fpr) & as.Date(rec$date_mr) >= start_date & as.Date(rec$date_mr) <= end_date, ], 
     tapply(fpr, herdID, function(x) length(x[x < fpr_sara_T]) / length(x)))



