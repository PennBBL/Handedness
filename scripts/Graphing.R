#Graphing

#Set Working Directory & Import Data
setwd("/Users/zsheff/Documents/Handedness/Graphing")
Demo <- read.csv("n1601_demographics_go1_20161212.csv")
Pcasl <- read.csv("n2416_jlfAntsCTIntersectionPcaslValues_20170404.csv")
Mprage <- read.csv("n2416_jlfAntsCTIntersectionVol_20170412.csv")
PQa <- read.csv("n2416_PcaslQaData_20170404.csv")
TQa <- read.csv("n2416_t1QaData_20170516.csv")

#Reassign Variables
#0 - Right | 1 - Left || 0 - Male | 1 - Female
Demo$handednessv2 <- Demo$handednessv2 - 1
Demo$sex <- Demo$sex - 1
Demo[,14] <- (Demo[,9])^2; colnames(Demo)[14] <- "age2"

#Participant List: Exclusion & Group Assignment
Parts <- Demo[!
  (Demo$scanid %in%
    c(PQa$scanid[PQa$pcaslExclude==1],
    TQa$scanid[TQa$t1Exclude==1])),
  c("bblid", "scanid", "sex", "ageAtScan1","age2","handednessv2")]
colnames(Parts) <- c("bblid","scanid","sex","age","age2","hand")
Parts$group <- ifelse((Parts$sex==0 & Parts$age<156), "M1",
  ifelse((Parts$sex==0 & Parts$age>=156 & Parts$age<216), "M2",
    ifelse((Parts$sex==0 & Parts$age>=216), "M3",
      ifelse((Parts$sex==1 & Parts$age<156), "F1",
        ifelse((Parts$sex==1 & Parts$age>=156 & Parts$age<216),  "F2",
          ifelse((Parts$sex==1 & Parts$age>=216), "F3", NA))))))

#Average Left & Right
CBF <- data.frame(bblid=Pcasl$bblid, scanid=Pcasl$scanid, qa=PQa$pcaslRelMeanRMSMotion,
  Thalamus=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Thalamus_Proper","pcasl_jlf_cbf_R_Thalamus_Proper")]),
  Putamen=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Putamen","pcasl_jlf_cbf_R_Putamen")]),
  Caudate=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Caudate","pcasl_jlf_cbf_R_Caudate")]),
  Pallidum=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Pallidum","pcasl_jlf_cbf_R_Pallidum")]),
  Accumbens=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Accumbens_Area","pcasl_jlf_cbf_R_Accumbens_Area")]),
  PHG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PHG","pcasl_jlf_cbf_R_PHG")]),
  Hippocampus=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Hippocampus","pcasl_jlf_cbf_R_Hippocampus")]),
  PIns=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PIns","pcasl_jlf_cbf_R_PIns")]),
  SCA=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_SCA","pcasl_jlf_cbf_R_SCA")]),
  AIns=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_AIns","pcasl_jlf_cbf_R_AIns")]),
  ACgG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_ACgG","pcasl_jlf_cbf_R_ACgG")]),
  PCgG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PCgG","pcasl_jlf_cbf_R_PCgG")]),
  Ent=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Ent","pcasl_jlf_cbf_R_Ent")]),
  Amygdala=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Amygdala","pcasl_jlf_cbf_R_Amygdala")]),
  MCgG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MCgG","pcasl_jlf_cbf_R_MCgG")]),
  FO=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_FO","pcasl_jlf_cbf_R_FO")]),
  MFC=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MFC","pcasl_jlf_cbf_R_MFC")]),
  MOrG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MOrG","pcasl_jlf_cbf_R_MOrG")]),
  POrG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_POrG","pcasl_jlf_cbf_R_POrG")]),
  OrIFG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_OrIFG","pcasl_jlf_cbf_R_OrIFG")]),
  TrIFG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_TrIFG","pcasl_jlf_cbf_R_TrIFG")]),
  AOrG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_AOrG","pcasl_jlf_cbf_R_AOrG")]),
  OpIFG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_OpIFG","pcasl_jlf_cbf_R_OpIFG")]),
  GRe=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_GRe","pcasl_jlf_cbf_R_GRe")]),
  FRP=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_FRP","pcasl_jlf_cbf_R_FRP")]),
  LOrG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_LOrG","pcasl_jlf_cbf_R_LOrG")]),
  PrG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PrG","pcasl_jlf_cbf_R_PrG")]),
  MSFG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MSFG","pcasl_jlf_cbf_R_MSFG")]),
  SMC=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_SMC","pcasl_jlf_cbf_R_SMC")]),
  MFG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MFG","pcasl_jlf_cbf_R_MFG")]),
  SFG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_SFG","pcasl_jlf_cbf_R_SFG")]),
  FuG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_FuG","pcasl_jlf_cbf_R_FuG")]),
  PT=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PT","pcasl_jlf_cbf_R_PT")]),
  PP=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PP","pcasl_jlf_cbf_R_PP")]),
  ITG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_ITG","pcasl_jlf_cbf_R_ITG")]),
  CO=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_CO","pcasl_jlf_cbf_R_CO")]),
  MTG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MTG","pcasl_jlf_cbf_R_MTG")]),
  TMP=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_TMP","pcasl_jlf_cbf_R_TMP")]),
  STG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_STG","pcasl_jlf_cbf_R_STG")]),
  TTG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_TTG","pcasl_jlf_cbf_R_TTG")]),
  PCu=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PCu","pcasl_jlf_cbf_R_PCu")]),
  PoG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PoG","pcasl_jlf_cbf_R_PoG")]),
  AnG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_AnG","pcasl_jlf_cbf_R_AnG")]),
  PO=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_PO","pcasl_jlf_cbf_R_PO")]),
  SPL=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_SPL","pcasl_jlf_cbf_R_SPL")]),
  MPrG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MPrG","pcasl_jlf_cbf_R_MPrG")]),
  SMG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_SMG","pcasl_jlf_cbf_R_SMG")]),
  MPoG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MPoG","pcasl_jlf_cbf_R_MPoG")]),
  IOG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_IOG","pcasl_jlf_cbf_R_IOG")]),
  CUN=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Cun","pcasl_jlf_cbf_R_Cun")]),
  LiG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_LiG","pcasl_jlf_cbf_R_LiG")]),
  OFuG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_OFuG","pcasl_jlf_cbf_R_OFuG")]),
  MOG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_MOG","pcasl_jlf_cbf_R_MOG")]),
  Calc=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_Calc","pcasl_jlf_cbf_R_Calc")]),
  OCP=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_OCP","pcasl_jlf_cbf_R_OCP")]),
  SOG=rowMeans(Pcasl[,c("pcasl_jlf_cbf_L_SOG","pcasl_jlf_cbf_R_SOG")]))
Vol <- data.frame(bblid=Mprage$bblid, scanid=Mprage$scanid, qa=TQa$averageManualRating,
  Thalamus=rowMeans(Mprage[,c("mprage_jlf_vol_L_Thalamus_Proper","mprage_jlf_vol_R_Thalamus_Proper")]),
  Putamen=rowMeans(Mprage[,c("mprage_jlf_vol_L_Putamen","mprage_jlf_vol_R_Putamen")]),
  Caudate=rowMeans(Mprage[,c("mprage_jlf_vol_L_Caudate","mprage_jlf_vol_R_Caudate")]),
  Pallidum=rowMeans(Mprage[,c("mprage_jlf_vol_L_Pallidum","mprage_jlf_vol_R_Pallidum")]),
  Accumbens=rowMeans(Mprage[,c("mprage_jlf_vol_L_Accumbens_Area","mprage_jlf_vol_R_Accumbens_Area")]),
  PHG=rowMeans(Mprage[,c("mprage_jlf_vol_L_PHG","mprage_jlf_vol_R_PHG")]),
  Hippocampus=rowMeans(Mprage[,c("mprage_jlf_vol_L_Hippocampus","mprage_jlf_vol_R_Hippocampus")]),
  PIns=rowMeans(Mprage[,c("mprage_jlf_vol_L_PIns","mprage_jlf_vol_R_PIns")]),
  SCA=rowMeans(Mprage[,c("mprage_jlf_vol_L_SCA","mprage_jlf_vol_R_SCA")]),
  AIns=rowMeans(Mprage[,c("mprage_jlf_vol_L_AIns","mprage_jlf_vol_R_AIns")]),
  ACgG=rowMeans(Mprage[,c("mprage_jlf_vol_L_ACgG","mprage_jlf_vol_R_ACgG")]),
  PCgG=rowMeans(Mprage[,c("mprage_jlf_vol_L_PCgG","mprage_jlf_vol_R_PCgG")]),
  Ent=rowMeans(Mprage[,c("mprage_jlf_vol_L_Ent","mprage_jlf_vol_R_Ent")]),
  Amygdala=rowMeans(Mprage[,c("mprage_jlf_vol_L_Amygdala","mprage_jlf_vol_R_Amygdala")]),
  MCgG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MCgG","mprage_jlf_vol_R_MCgG")]),
  FO=rowMeans(Mprage[,c("mprage_jlf_vol_L_FO","mprage_jlf_vol_R_FO")]),
  MFC=rowMeans(Mprage[,c("mprage_jlf_vol_L_MFC","mprage_jlf_vol_R_MFC")]),
  MOrG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MOrG","mprage_jlf_vol_R_MOrG")]),
  POrG=rowMeans(Mprage[,c("mprage_jlf_vol_L_POrG","mprage_jlf_vol_R_POrG")]),
  OrIFG=rowMeans(Mprage[,c("mprage_jlf_vol_L_OrIFG","mprage_jlf_vol_R_OrIFG")]),
  TrIFG=rowMeans(Mprage[,c("mprage_jlf_vol_L_TrIFG","mprage_jlf_vol_R_TrIFG")]),
  AOrG=rowMeans(Mprage[,c("mprage_jlf_vol_L_AOrG","mprage_jlf_vol_R_AOrG")]),
  OpIFG=rowMeans(Mprage[,c("mprage_jlf_vol_L_OpIFG","mprage_jlf_vol_R_OpIFG")]),
  GRe=rowMeans(Mprage[,c("mprage_jlf_vol_L_GRe","mprage_jlf_vol_R_GRe")]),
  FRP=rowMeans(Mprage[,c("mprage_jlf_vol_L_FRP","mprage_jlf_vol_R_FRP")]),
  LOrG=rowMeans(Mprage[,c("mprage_jlf_vol_L_LOrG","mprage_jlf_vol_R_LOrG")]),
  PrG=rowMeans(Mprage[,c("mprage_jlf_vol_L_PrG","mprage_jlf_vol_R_PrG")]),
  MSFG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MSFG","mprage_jlf_vol_R_MSFG")]),
  SMC=rowMeans(Mprage[,c("mprage_jlf_vol_L_SMC","mprage_jlf_vol_R_SMC")]),
  MFG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MFG","mprage_jlf_vol_R_MFG")]),
  SFG=rowMeans(Mprage[,c("mprage_jlf_vol_L_SFG","mprage_jlf_vol_R_SFG")]),
  FuG=rowMeans(Mprage[,c("mprage_jlf_vol_L_FuG","mprage_jlf_vol_R_FuG")]),
  PT=rowMeans(Mprage[,c("mprage_jlf_vol_L_PT","mprage_jlf_vol_R_PT")]),
  PP=rowMeans(Mprage[,c("mprage_jlf_vol_L_PP","mprage_jlf_vol_R_PP")]),
  ITG=rowMeans(Mprage[,c("mprage_jlf_vol_L_ITG","mprage_jlf_vol_R_ITG")]),
  CO=rowMeans(Mprage[,c("mprage_jlf_vol_L_CO","mprage_jlf_vol_R_CO")]),
  MTG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MTG","mprage_jlf_vol_R_MTG")]),
  TMP=rowMeans(Mprage[,c("mprage_jlf_vol_L_TMP","mprage_jlf_vol_R_TMP")]),
  STG=rowMeans(Mprage[,c("mprage_jlf_vol_L_STG","mprage_jlf_vol_R_STG")]),
  TTG=rowMeans(Mprage[,c("mprage_jlf_vol_L_TTG","mprage_jlf_vol_R_TTG")]),
  PCu=rowMeans(Mprage[,c("mprage_jlf_vol_L_PCu","mprage_jlf_vol_R_PCu")]),
  PoG=rowMeans(Mprage[,c("mprage_jlf_vol_L_PoG","mprage_jlf_vol_R_PoG")]),
  AnG=rowMeans(Mprage[,c("mprage_jlf_vol_L_AnG","mprage_jlf_vol_R_AnG")]),
  PO=rowMeans(Mprage[,c("mprage_jlf_vol_L_PO","mprage_jlf_vol_R_PO")]),
  SPL=rowMeans(Mprage[,c("mprage_jlf_vol_L_SPL","mprage_jlf_vol_R_SPL")]),
  MPrG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MPrG","mprage_jlf_vol_R_MPrG")]),
  SMG=rowMeans(Mprage[,c("mprage_jlf_vol_L_SMG","mprage_jlf_vol_R_SMG")]),
  MPoG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MPoG","mprage_jlf_vol_R_MPoG")]),
  IOG=rowMeans(Mprage[,c("mprage_jlf_vol_L_IOG","mprage_jlf_vol_R_IOG")]),
  CUN=rowMeans(Mprage[,c("mprage_jlf_vol_L_Cun","mprage_jlf_vol_R_Cun")]),
  LiG=rowMeans(Mprage[,c("mprage_jlf_vol_L_LiG","mprage_jlf_vol_R_LiG")]),
  OFuG=rowMeans(Mprage[,c("mprage_jlf_vol_L_OFuG","mprage_jlf_vol_R_OFuG")]),
  MOG=rowMeans(Mprage[,c("mprage_jlf_vol_L_MOG","mprage_jlf_vol_R_MOG")]),
  Calc=rowMeans(Mprage[,c("mprage_jlf_vol_L_Calc","mprage_jlf_vol_R_Calc")]),
  OCP=rowMeans(Mprage[,c("mprage_jlf_vol_L_OCP","mprage_jlf_vol_R_OCP")]),
  SOG=rowMeans(Mprage[,c("mprage_jlf_vol_L_SOG","mprage_jlf_vol_R_SOG")]))

#Scale Data
CBF <- CBF[CBF$scanid %in% Parts$scanid,]
CBF[,3:59] <- scale(CBF[,3:59], center=TRUE, scale=TRUE)
Vol <- Vol[Vol$scanid %in% Parts$scanid,]
Vol[,3:59] <- scale(Vol[,3:59], center=TRUE, scale=TRUE)
Parts$age <- scale(Parts$age, center=TRUE, scale=TRUE)
Parts$age2 <- scale(Parts$age2, center=TRUE, scale=TRUE)

#Separate Data
M1CBF <- Parts[Parts$group=="M1",c("bblid", "scanid","age","age2","hand")]
  M1CBF[,5:63] <- CBF[CBF$scanid %in% M1CBF$scanid,2:59]
M2CBF <- Parts[Parts$group=="M2",c("bblid", "scanid","age","age2","hand")]
  M2CBF[,5:63] <- CBF[CBF$scanid %in% M2CBF$scanid,2:59]
M3CBF <- Parts[Parts$group=="M3",c("bblid", "scanid","age","age2","hand")]
  M3CBF[,5:63] <- CBF[CBF$scanid %in% M3CBF$scanid,2:59]
F1CBF <- Parts[Parts$group=="F1",c("bblid", "scanid","age","age2","hand")]
  F1CBF[,5:63] <- CBF[CBF$scanid %in% F1CBF$scanid,2:59]
F2CBF <- Parts[Parts$group=="F2",c("bblid", "scanid","age","age2","hand")]
  F2CBF[,5:63] <- CBF[CBF$scanid %in% F2CBF$scanid,2:59]
F3CBF <- Parts[Parts$group=="F3",c("bblid", "scanid","age","age2","hand")]
  F3CBF[,5:63] <- CBF[CBF$scanid %in% F3CBF$scanid,2:59]
M1Vol <- Parts[Parts$group=="M1",c("bblid", "scanid","age","age2","hand")]
  M1Vol[,5:63] <- Vol[Vol$scanid %in% M1Vol$scanid,2:59]
M2Vol <- Parts[Parts$group=="M2",c("bblid", "scanid","age","age2","hand")]
  M2Vol[,5:63] <- Vol[Vol$scanid %in% M2Vol$scanid,2:59]
M3Vol <- Parts[Parts$group=="M3",c("bblid", "scanid","age","age2","hand")]
  M3Vol[,5:63] <- Vol[Vol$scanid %in% M3Vol$scanid,2:59]
F1Vol <- Parts[Parts$group=="F1",c("bblid", "scanid","age","age2","hand")]
  F1Vol[,5:63] <- Vol[Vol$scanid %in% F1Vol$scanid,2:59]
F2Vol <- Parts[Parts$group=="F2",c("bblid", "scanid","age","age2","hand")]
  F2Vol[,5:63] <- Vol[Vol$scanid %in% F2Vol$scanid,2:59]
F3Vol <- Parts[Parts$group=="F3",c("bblid", "scanid","age","age2","hand")]
  F3Vol[,5:63] <- Vol[Vol$scanid %in% F3Vol$scanid,2:59]

#Prepare Final Data Frame
Final <- data.frame(
  effect=rep(NA,times=672),
  region1=rep(c("Thalamus", "Putamen", "Caudate", "Pallidum", "Accumbens",
    "PHG", "Hippocampus", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amygdala", "MCgG",
    "FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG",
    "PrG", "MSFG", "SMC", "MFG", "SFG",
    "FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG",
    "PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG",
    "IOG", "CUN", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG"), times=12),
  region2=rep(c(rep("BasGang", times=5),
    rep("Limbic", times=10),
    rep("FrontOrb", times=11),
    rep("FrontDors", times=5),
    rep("Temporal", times=9),
    rep("Parietal", times=8),
    rep("Occipital", times=8)),times=12),
  group=rep(c(rep("M1",times=56),
    rep("M2",times=56),
    rep("M3",times=56),
    rep("F1",times=56),
    rep("F2",times=56),
    rep("F3",times=56)),times=2),
  metric=rep(c("CBF","Vol"),each=336))

#Run Models
Final[1,1] <- lm(Thalamus ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[2,1] <- lm(Putamen ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[3,1] <- lm(Caudate ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[4,1] <- lm(Pallidum ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[5,1] <- lm(Accumbens ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[6,1] <- lm(PHG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[7,1] <- lm(Hippocampus ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[8,1] <- lm(PIns ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[9,1] <- lm(SCA ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[10,1] <- lm(AIns ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[11,1] <- lm(ACgG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[12,1] <- lm(PCgG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[13,1] <- lm(Ent ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[14,1] <- lm(Amygdala ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[15,1] <- lm(MCgG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[16,1] <- lm(FO ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[17,1] <- lm(MFC ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[18,1] <- lm(MOrG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[19,1] <- lm(POrG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[20,1] <- lm(OrIFG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[21,1] <- lm(TrIFG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[22,1] <- lm(AOrG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[23,1] <- lm(OpIFG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[24,1] <- lm(GRe ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[25,1] <- lm(FRP ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[26,1] <- lm(LOrG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[27,1] <- lm(PrG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[28,1] <- lm(MSFG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[29,1] <- lm(SMC ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[30,1] <- lm(MFG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[31,1] <- lm(SFG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[32,1] <- lm(FuG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[33,1] <- lm(PT ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[34,1] <- lm(PP ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[35,1] <- lm(ITG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[36,1] <- lm(CO ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[37,1] <- lm(MTG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[38,1] <- lm(TMP ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[39,1] <- lm(STG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[40,1] <- lm(TTG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[41,1] <- lm(PCu ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[42,1] <- lm(PoG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[43,1] <- lm(AnG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[44,1] <- lm(PO ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[45,1] <- lm(SPL ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[46,1] <- lm(MPrG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[47,1] <- lm(SMG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[48,1] <- lm(MPoG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[49,1] <- lm(IOG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[50,1] <- lm(CUN ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[51,1] <- lm(LiG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[52,1] <- lm(OFuG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[53,1] <- lm(MOG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[54,1] <- lm(Calc ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[55,1] <- lm(OCP ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[56,1] <- lm(SOG ~ age + age2 + hand + qa, M1CBF)$coefficients["hand"]
Final[57,1] <- lm(Thalamus ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[58,1] <- lm(Putamen ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[59,1] <- lm(Caudate ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[60,1] <- lm(Pallidum ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[61,1] <- lm(Accumbens ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[62,1] <- lm(PHG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[63,1] <- lm(Hippocampus ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[64,1] <- lm(PIns ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[65,1] <- lm(SCA ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[66,1] <- lm(AIns ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[67,1] <- lm(ACgG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[68,1] <- lm(PCgG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[69,1] <- lm(Ent ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[70,1] <- lm(Amygdala ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[71,1] <- lm(MCgG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[72,1] <- lm(FO ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[73,1] <- lm(MFC ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[74,1] <- lm(MOrG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[75,1] <- lm(POrG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[76,1] <- lm(OrIFG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[77,1] <- lm(TrIFG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[78,1] <- lm(AOrG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[79,1] <- lm(OpIFG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[80,1] <- lm(GRe ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[81,1] <- lm(FRP ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[82,1] <- lm(LOrG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[83,1] <- lm(PrG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[84,1] <- lm(MSFG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[85,1] <- lm(SMC ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[86,1] <- lm(MFG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[87,1] <- lm(SFG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[88,1] <- lm(FuG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[89,1] <- lm(PT ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[90,1] <- lm(PP ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[91,1] <- lm(ITG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[92,1] <- lm(CO ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[93,1] <- lm(MTG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[94,1] <- lm(TMP ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[95,1] <- lm(STG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[96,1] <- lm(TTG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[97,1] <- lm(PCu ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[98,1] <- lm(PoG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[99,1] <- lm(AnG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[100,1] <- lm(PO ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[101,1] <- lm(SPL ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[102,1] <- lm(MPrG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[103,1] <- lm(SMG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[104,1] <- lm(MPoG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[105,1] <- lm(IOG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[106,1] <- lm(CUN ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[107,1] <- lm(LiG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[108,1] <- lm(OFuG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[109,1] <- lm(MOG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[110,1] <- lm(Calc ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[111,1] <- lm(OCP ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[112,1] <- lm(SOG ~ age + age2 + hand + qa, M2CBF)$coefficients["hand"]
Final[113,1] <- lm(Thalamus ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[114,1] <- lm(Putamen ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[115,1] <- lm(Caudate ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[116,1] <- lm(Pallidum ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[117,1] <- lm(Accumbens ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[118,1] <- lm(PHG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[119,1] <- lm(Hippocampus ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[120,1] <- lm(PIns ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[121,1] <- lm(SCA ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[122,1] <- lm(AIns ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[123,1] <- lm(ACgG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[124,1] <- lm(PCgG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[125,1] <- lm(Ent ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[126,1] <- lm(Amygdala ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[127,1] <- lm(MCgG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[128,1] <- lm(FO ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[129,1] <- lm(MFC ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[130,1] <- lm(MOrG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[131,1] <- lm(POrG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[132,1] <- lm(OrIFG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[133,1] <- lm(TrIFG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[134,1] <- lm(AOrG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[135,1] <- lm(OpIFG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[136,1] <- lm(GRe ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[137,1] <- lm(FRP ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[138,1] <- lm(LOrG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[139,1] <- lm(PrG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[140,1] <- lm(MSFG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[141,1] <- lm(SMC ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[142,1] <- lm(MFG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[143,1] <- lm(SFG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[144,1] <- lm(FuG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[145,1] <- lm(PT ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[146,1] <- lm(PP ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[147,1] <- lm(ITG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[148,1] <- lm(CO ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[149,1] <- lm(MTG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[150,1] <- lm(TMP ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[151,1] <- lm(STG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[152,1] <- lm(TTG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[153,1] <- lm(PCu ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[154,1] <- lm(PoG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[155,1] <- lm(AnG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[156,1] <- lm(PO ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[157,1] <- lm(SPL ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[158,1] <- lm(MPrG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[159,1] <- lm(SMG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[160,1] <- lm(MPoG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[161,1] <- lm(IOG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[162,1] <- lm(CUN ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[163,1] <- lm(LiG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[164,1] <- lm(OFuG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[165,1] <- lm(MOG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[166,1] <- lm(Calc ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[167,1] <- lm(OCP ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[168,1] <- lm(SOG ~ age + age2 + hand + qa, M3CBF)$coefficients["hand"]
Final[169,1] <- lm(Thalamus ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[170,1] <- lm(Putamen ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[171,1] <- lm(Caudate ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[172,1] <- lm(Pallidum ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[173,1] <- lm(Accumbens ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[174,1] <- lm(PHG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[175,1] <- lm(Hippocampus ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[176,1] <- lm(PIns ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[177,1] <- lm(SCA ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[178,1] <- lm(AIns ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[179,1] <- lm(ACgG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[180,1] <- lm(PCgG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[181,1] <- lm(Ent ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[182,1] <- lm(Amygdala ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[183,1] <- lm(MCgG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[184,1] <- lm(FO ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[185,1] <- lm(MFC ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[186,1] <- lm(MOrG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[187,1] <- lm(POrG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[188,1] <- lm(OrIFG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[189,1] <- lm(TrIFG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[190,1] <- lm(AOrG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[191,1] <- lm(OpIFG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[192,1] <- lm(GRe ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[193,1] <- lm(FRP ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[194,1] <- lm(LOrG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[195,1] <- lm(PrG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[196,1] <- lm(MSFG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[197,1] <- lm(SMC ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[198,1] <- lm(MFG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[199,1] <- lm(SFG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[200,1] <- lm(FuG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[201,1] <- lm(PT ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[202,1] <- lm(PP ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[203,1] <- lm(ITG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[204,1] <- lm(CO ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[205,1] <- lm(MTG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[206,1] <- lm(TMP ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[207,1] <- lm(STG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[208,1] <- lm(TTG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[209,1] <- lm(PCu ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[210,1] <- lm(PoG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[211,1] <- lm(AnG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[212,1] <- lm(PO ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[213,1] <- lm(SPL ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[214,1] <- lm(MPrG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[215,1] <- lm(SMG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[216,1] <- lm(MPoG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[217,1] <- lm(IOG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[218,1] <- lm(CUN ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[219,1] <- lm(LiG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[220,1] <- lm(OFuG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[221,1] <- lm(MOG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[222,1] <- lm(Calc ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[223,1] <- lm(OCP ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[224,1] <- lm(SOG ~ age + age2 + hand + qa, F1CBF)$coefficients["hand"]
Final[225,1] <- lm(Thalamus ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[226,1] <- lm(Putamen ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[227,1] <- lm(Caudate ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[228,1] <- lm(Pallidum ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[229,1] <- lm(Accumbens ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[230,1] <- lm(PHG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[231,1] <- lm(Hippocampus ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[232,1] <- lm(PIns ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[233,1] <- lm(SCA ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[234,1] <- lm(AIns ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[235,1] <- lm(ACgG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[236,1] <- lm(PCgG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[237,1] <- lm(Ent ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[238,1] <- lm(Amygdala ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[239,1] <- lm(MCgG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[240,1] <- lm(FO ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[241,1] <- lm(MFC ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[242,1] <- lm(MOrG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[243,1] <- lm(POrG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[244,1] <- lm(OrIFG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[245,1] <- lm(TrIFG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[246,1] <- lm(AOrG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[247,1] <- lm(OpIFG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[248,1] <- lm(GRe ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[249,1] <- lm(FRP ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[250,1] <- lm(LOrG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[251,1] <- lm(PrG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[252,1] <- lm(MSFG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[253,1] <- lm(SMC ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[254,1] <- lm(MFG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[255,1] <- lm(SFG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[256,1] <- lm(FuG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[257,1] <- lm(PT ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[258,1] <- lm(PP ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[259,1] <- lm(ITG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[260,1] <- lm(CO ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[261,1] <- lm(MTG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[262,1] <- lm(TMP ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[263,1] <- lm(STG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[264,1] <- lm(TTG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[265,1] <- lm(PCu ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[266,1] <- lm(PoG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[267,1] <- lm(AnG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[268,1] <- lm(PO ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[269,1] <- lm(SPL ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[270,1] <- lm(MPrG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[271,1] <- lm(SMG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[272,1] <- lm(MPoG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[273,1] <- lm(IOG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[274,1] <- lm(CUN ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[275,1] <- lm(LiG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[276,1] <- lm(OFuG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[277,1] <- lm(MOG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[278,1] <- lm(Calc ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[279,1] <- lm(OCP ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[280,1] <- lm(SOG ~ age + age2 + hand + qa, F2CBF)$coefficients["hand"]
Final[281,1] <- lm(Thalamus ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[282,1] <- lm(Putamen ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[283,1] <- lm(Caudate ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[284,1] <- lm(Pallidum ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[285,1] <- lm(Accumbens ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[286,1] <- lm(PHG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[287,1] <- lm(Hippocampus ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[288,1] <- lm(PIns ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[289,1] <- lm(SCA ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[290,1] <- lm(AIns ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[291,1] <- lm(ACgG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[292,1] <- lm(PCgG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[293,1] <- lm(Ent ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[294,1] <- lm(Amygdala ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[295,1] <- lm(MCgG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[296,1] <- lm(FO ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[297,1] <- lm(MFC ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[298,1] <- lm(MOrG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[299,1] <- lm(POrG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[300,1] <- lm(OrIFG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[301,1] <- lm(TrIFG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[302,1] <- lm(AOrG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[303,1] <- lm(OpIFG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[304,1] <- lm(GRe ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[305,1] <- lm(FRP ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[306,1] <- lm(LOrG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[307,1] <- lm(PrG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[308,1] <- lm(MSFG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[309,1] <- lm(SMC ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[310,1] <- lm(MFG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[311,1] <- lm(SFG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[312,1] <- lm(FuG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[313,1] <- lm(PT ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[314,1] <- lm(PP ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[315,1] <- lm(ITG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[316,1] <- lm(CO ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[317,1] <- lm(MTG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[318,1] <- lm(TMP ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[319,1] <- lm(STG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[320,1] <- lm(TTG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[321,1] <- lm(PCu ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[322,1] <- lm(PoG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[323,1] <- lm(AnG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[324,1] <- lm(PO ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[325,1] <- lm(SPL ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[326,1] <- lm(MPrG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[327,1] <- lm(SMG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[328,1] <- lm(MPoG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[329,1] <- lm(IOG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[330,1] <- lm(CUN ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[331,1] <- lm(LiG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[332,1] <- lm(OFuG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[333,1] <- lm(MOG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[334,1] <- lm(Calc ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[335,1] <- lm(OCP ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[336,1] <- lm(SOG ~ age + age2 + hand + qa, F3CBF)$coefficients["hand"]
Final[337,1] <- lm(Thalamus ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[338,1] <- lm(Putamen ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[339,1] <- lm(Caudate ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[340,1] <- lm(Pallidum ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[341,1] <- lm(Accumbens ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[342,1] <- lm(PHG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[343,1] <- lm(Hippocampus ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[344,1] <- lm(PIns ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[345,1] <- lm(SCA ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[346,1] <- lm(AIns ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[347,1] <- lm(ACgG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[348,1] <- lm(PCgG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[349,1] <- lm(Ent ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[350,1] <- lm(Amygdala ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[351,1] <- lm(MCgG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[352,1] <- lm(FO ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[353,1] <- lm(MFC ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[354,1] <- lm(MOrG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[355,1] <- lm(POrG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[356,1] <- lm(OrIFG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[357,1] <- lm(TrIFG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[358,1] <- lm(AOrG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[359,1] <- lm(OpIFG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[360,1] <- lm(GRe ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[361,1] <- lm(FRP ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[362,1] <- lm(LOrG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[363,1] <- lm(PrG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[364,1] <- lm(MSFG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[365,1] <- lm(SMC ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[366,1] <- lm(MFG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[367,1] <- lm(SFG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[368,1] <- lm(FuG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[369,1] <- lm(PT ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[370,1] <- lm(PP ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[371,1] <- lm(ITG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[372,1] <- lm(CO ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[373,1] <- lm(MTG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[374,1] <- lm(TMP ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[375,1] <- lm(STG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[376,1] <- lm(TTG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[377,1] <- lm(PCu ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[378,1] <- lm(PoG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[379,1] <- lm(AnG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[380,1] <- lm(PO ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[381,1] <- lm(SPL ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[382,1] <- lm(MPrG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[383,1] <- lm(SMG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[384,1] <- lm(MPoG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[385,1] <- lm(IOG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[386,1] <- lm(CUN ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[387,1] <- lm(LiG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[388,1] <- lm(OFuG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[389,1] <- lm(MOG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[390,1] <- lm(Calc ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[391,1] <- lm(OCP ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[392,1] <- lm(SOG ~ age + age2 + hand + qa, M1Vol)$coefficients["hand"]
Final[393,1] <- lm(Thalamus ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[394,1] <- lm(Putamen ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[395,1] <- lm(Caudate ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[396,1] <- lm(Pallidum ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[397,1] <- lm(Accumbens ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[398,1] <- lm(PHG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[399,1] <- lm(Hippocampus ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[400,1] <- lm(PIns ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[401,1] <- lm(SCA ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[402,1] <- lm(AIns ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[403,1] <- lm(ACgG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[404,1] <- lm(PCgG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[405,1] <- lm(Ent ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[406,1] <- lm(Amygdala ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[407,1] <- lm(MCgG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[408,1] <- lm(FO ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[409,1] <- lm(MFC ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[410,1] <- lm(MOrG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[411,1] <- lm(POrG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[412,1] <- lm(OrIFG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[413,1] <- lm(TrIFG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[414,1] <- lm(AOrG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[415,1] <- lm(OpIFG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[416,1] <- lm(GRe ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[417,1] <- lm(FRP ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[418,1] <- lm(LOrG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[419,1] <- lm(PrG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[420,1] <- lm(MSFG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[421,1] <- lm(SMC ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[422,1] <- lm(MFG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[423,1] <- lm(SFG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[424,1] <- lm(FuG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[425,1] <- lm(PT ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[426,1] <- lm(PP ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[427,1] <- lm(ITG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[428,1] <- lm(CO ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[429,1] <- lm(MTG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[430,1] <- lm(TMP ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[431,1] <- lm(STG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[432,1] <- lm(TTG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[433,1] <- lm(PCu ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[434,1] <- lm(PoG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[435,1] <- lm(AnG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[436,1] <- lm(PO ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[437,1] <- lm(SPL ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[438,1] <- lm(MPrG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[439,1] <- lm(SMG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[440,1] <- lm(MPoG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[441,1] <- lm(IOG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[442,1] <- lm(CUN ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[443,1] <- lm(LiG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[444,1] <- lm(OFuG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[445,1] <- lm(MOG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[446,1] <- lm(Calc ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[447,1] <- lm(OCP ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[448,1] <- lm(SOG ~ age + age2 + hand + qa, M2Vol)$coefficients["hand"]
Final[449,1] <- lm(Thalamus ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[450,1] <- lm(Putamen ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[451,1] <- lm(Caudate ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[452,1] <- lm(Pallidum ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[453,1] <- lm(Accumbens ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[454,1] <- lm(PHG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[455,1] <- lm(Hippocampus ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[456,1] <- lm(PIns ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[457,1] <- lm(SCA ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[458,1] <- lm(AIns ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[459,1] <- lm(ACgG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[460,1] <- lm(PCgG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[461,1] <- lm(Ent ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[462,1] <- lm(Amygdala ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[463,1] <- lm(MCgG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[464,1] <- lm(FO ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[465,1] <- lm(MFC ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[466,1] <- lm(MOrG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[467,1] <- lm(POrG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[468,1] <- lm(OrIFG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[469,1] <- lm(TrIFG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[470,1] <- lm(AOrG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[471,1] <- lm(OpIFG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[472,1] <- lm(GRe ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[473,1] <- lm(FRP ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[474,1] <- lm(LOrG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[475,1] <- lm(PrG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[476,1] <- lm(MSFG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[477,1] <- lm(SMC ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[478,1] <- lm(MFG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[479,1] <- lm(SFG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[480,1] <- lm(FuG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[481,1] <- lm(PT ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[482,1] <- lm(PP ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[483,1] <- lm(ITG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[484,1] <- lm(CO ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[485,1] <- lm(MTG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[486,1] <- lm(TMP ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[487,1] <- lm(STG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[488,1] <- lm(TTG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[489,1] <- lm(PCu ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[490,1] <- lm(PoG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[491,1] <- lm(AnG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[492,1] <- lm(PO ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[493,1] <- lm(SPL ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[494,1] <- lm(MPrG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[495,1] <- lm(SMG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[496,1] <- lm(MPoG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[497,1] <- lm(IOG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[498,1] <- lm(CUN ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[499,1] <- lm(LiG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[500,1] <- lm(OFuG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[501,1] <- lm(MOG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[502,1] <- lm(Calc ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[503,1] <- lm(OCP ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[504,1] <- lm(SOG ~ age + age2 + hand + qa, M3Vol)$coefficients["hand"]
Final[505,1] <- lm(Thalamus ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[506,1] <- lm(Putamen ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[507,1] <- lm(Caudate ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[508,1] <- lm(Pallidum ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[509,1] <- lm(Accumbens ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[510,1] <- lm(PHG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[511,1] <- lm(Hippocampus ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[512,1] <- lm(PIns ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[513,1] <- lm(SCA ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[514,1] <- lm(AIns ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[515,1] <- lm(ACgG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[516,1] <- lm(PCgG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[517,1] <- lm(Ent ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[518,1] <- lm(Amygdala ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[519,1] <- lm(MCgG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[520,1] <- lm(FO ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[521,1] <- lm(MFC ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[522,1] <- lm(MOrG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[523,1] <- lm(POrG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[524,1] <- lm(OrIFG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[525,1] <- lm(TrIFG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[526,1] <- lm(AOrG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[527,1] <- lm(OpIFG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[528,1] <- lm(GRe ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[529,1] <- lm(FRP ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[530,1] <- lm(LOrG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[531,1] <- lm(PrG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[532,1] <- lm(MSFG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[533,1] <- lm(SMC ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[534,1] <- lm(MFG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[535,1] <- lm(SFG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[536,1] <- lm(FuG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[537,1] <- lm(PT ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[538,1] <- lm(PP ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[539,1] <- lm(ITG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[540,1] <- lm(CO ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[541,1] <- lm(MTG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[542,1] <- lm(TMP ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[543,1] <- lm(STG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[544,1] <- lm(TTG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[545,1] <- lm(PCu ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[546,1] <- lm(PoG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[547,1] <- lm(AnG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[548,1] <- lm(PO ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[549,1] <- lm(SPL ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[550,1] <- lm(MPrG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[551,1] <- lm(SMG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[552,1] <- lm(MPoG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[553,1] <- lm(IOG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[554,1] <- lm(CUN ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[555,1] <- lm(LiG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[556,1] <- lm(OFuG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[557,1] <- lm(MOG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[558,1] <- lm(Calc ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[559,1] <- lm(OCP ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[560,1] <- lm(SOG ~ age + age2 + hand + qa, F1Vol)$coefficients["hand"]
Final[561,1] <- lm(Thalamus ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[562,1] <- lm(Putamen ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[563,1] <- lm(Caudate ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[564,1] <- lm(Pallidum ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[565,1] <- lm(Accumbens ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[566,1] <- lm(PHG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[567,1] <- lm(Hippocampus ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[568,1] <- lm(PIns ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[569,1] <- lm(SCA ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[570,1] <- lm(AIns ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[571,1] <- lm(ACgG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[572,1] <- lm(PCgG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[573,1] <- lm(Ent ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[574,1] <- lm(Amygdala ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[575,1] <- lm(MCgG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[576,1] <- lm(FO ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[577,1] <- lm(MFC ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[578,1] <- lm(MOrG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[579,1] <- lm(POrG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[580,1] <- lm(OrIFG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[581,1] <- lm(TrIFG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[582,1] <- lm(AOrG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[583,1] <- lm(OpIFG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[584,1] <- lm(GRe ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[585,1] <- lm(FRP ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[586,1] <- lm(LOrG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[587,1] <- lm(PrG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[588,1] <- lm(MSFG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[589,1] <- lm(SMC ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[590,1] <- lm(MFG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[591,1] <- lm(SFG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[592,1] <- lm(FuG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[593,1] <- lm(PT ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[594,1] <- lm(PP ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[595,1] <- lm(ITG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[596,1] <- lm(CO ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[597,1] <- lm(MTG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[598,1] <- lm(TMP ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[599,1] <- lm(STG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[600,1] <- lm(TTG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[601,1] <- lm(PCu ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[602,1] <- lm(PoG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[603,1] <- lm(AnG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[604,1] <- lm(PO ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[605,1] <- lm(SPL ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[606,1] <- lm(MPrG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[607,1] <- lm(SMG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[608,1] <- lm(MPoG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[609,1] <- lm(IOG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[610,1] <- lm(CUN ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[611,1] <- lm(LiG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[612,1] <- lm(OFuG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[613,1] <- lm(MOG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[614,1] <- lm(Calc ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[615,1] <- lm(OCP ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[616,1] <- lm(SOG ~ age + age2 + hand + qa, F2Vol)$coefficients["hand"]
Final[617,1] <- lm(Thalamus ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[618,1] <- lm(Putamen ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[619,1] <- lm(Caudate ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[620,1] <- lm(Pallidum ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[621,1] <- lm(Accumbens ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[622,1] <- lm(PHG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[623,1] <- lm(Hippocampus ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[624,1] <- lm(PIns ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[625,1] <- lm(SCA ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[626,1] <- lm(AIns ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[627,1] <- lm(ACgG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[628,1] <- lm(PCgG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[629,1] <- lm(Ent ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[630,1] <- lm(Amygdala ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[631,1] <- lm(MCgG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[632,1] <- lm(FO ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[633,1] <- lm(MFC ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[634,1] <- lm(MOrG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[635,1] <- lm(POrG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[636,1] <- lm(OrIFG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[637,1] <- lm(TrIFG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[638,1] <- lm(AOrG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[639,1] <- lm(OpIFG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[640,1] <- lm(GRe ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[641,1] <- lm(FRP ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[642,1] <- lm(LOrG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[643,1] <- lm(PrG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[644,1] <- lm(MSFG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[645,1] <- lm(SMC ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[646,1] <- lm(MFG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[647,1] <- lm(SFG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[648,1] <- lm(FuG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[649,1] <- lm(PT ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[650,1] <- lm(PP ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[651,1] <- lm(ITG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[652,1] <- lm(CO ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[653,1] <- lm(MTG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[654,1] <- lm(TMP ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[655,1] <- lm(STG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[656,1] <- lm(TTG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[657,1] <- lm(PCu ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[658,1] <- lm(PoG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[659,1] <- lm(AnG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[660,1] <- lm(PO ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[661,1] <- lm(SPL ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[662,1] <- lm(MPrG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[663,1] <- lm(SMG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[664,1] <- lm(MPoG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[665,1] <- lm(IOG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[666,1] <- lm(CUN ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[667,1] <- lm(LiG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[668,1] <- lm(OFuG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[669,1] <- lm(MOG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[670,1] <- lm(Calc ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[671,1] <- lm(OCP ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]
Final[672,1] <- lm(SOG ~ age + age2 + hand + qa, F3Vol)$coefficients["hand"]

#Plot
#justify x labels right
#freex
library(ggplot2)
plot <- ggplot(Final, aes(fill=group, x=region1, y=effect)) +
  geom_bar(position="dodge", color="black", stat="identity") +
  scale_fill_manual(
    values=c("#FCB0C0", "#FA1D83", "#BF0005", "#BFD9FF", "#4C9AEA", "#000078"),
    labels=c("Female Children", "Female Adolescents", "Female Adults",
      "Male Children", "Male Adolescents", "Female Adults")) +
  labs(x=NULL, y="Effect Size", fill="Group") +
  theme_linedraw() +
  theme(legend.position="bottom", axis.text.x = element_text(angle=45, hjust=.95)) +
  scale_y_continuous(breaks=seq(-0.5, 0.5, 0.1)) +
  facet_grid(metric ~ region2, scales = "free", space="free_x")
