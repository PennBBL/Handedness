#Set Working Directory & Import Data
setwd("/Users/zsheff/Documents/Handedness/Graphing")
Demo <- read.csv("n1601_demographics_go1_20161212.csv")
Pcasl <- read.csv("n2416_jlfAntsCTIntersectionPcaslValues_20170404.csv")
Mprage <- read.csv("n2416_jlfAntsCTIntersectionVol_20170412.csv")
PQa <- read.csv("n2416_PcaslQaData_20170404.csv")
TQa <- read.csv("n2416_t1QaData_20170516.csv")

#Define Region Variables
regions_full <- c("Thalamus_Proper", "Putamen", "Caudate", "Pallidum",
  "Accumbens_Area", "PHG", "Hippocampus", "PIns", "SCA", "AIns", "ACgG", "PCgG",
  "Ent", "Amygdala", "MCgG", "FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG",
  "AOrG", "OpIFG", "GRe", "FRP", "LOrG", "PrG", "MSFG", "SMC", "MFG", "SFG",
  "FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG", "PCu", "PoG",
  "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG", "IOG", "Cun", "LiG", "OFuG", "MOG",
  "Calc", "OCP", "SOG")
regions <- regions_full; regions[c(1,5)] <- c("Thalamus", "Accumbens")

#Reassign Variables
#0 - Right | 1 - Left || 0 - Male | 1 - Female
Demo$handednessv2 <- Demo$handednessv2 - 1; Demo$sex <- Demo$sex - 1
Demo[,14] <- (Demo[,9])^2; colnames(Demo)[14] <- "age2"

#Participant List
Data <- Demo[!
  (Demo$scanid %in%
    c(PQa$scanid[PQa$pcaslExclude==1],
    TQa$scanid[TQa$t1Exclude==1])),
  c("bblid", "scanid", "sex", "ageAtScan1","age2","handednessv2")]
colnames(Data) <- c("bblid","scanid","sex","age","age2","hand")
Data$group <- ifelse((Data$sex==0 & Data$age<156), "M1",
  ifelse((Data$sex==0 & Data$age>=156 & Data$age<216), "M2",
    ifelse((Data$sex==0 & Data$age>=216), "M3",
      ifelse((Data$sex==1 & Data$age<156), "F1",
        ifelse((Data$sex==1 & Data$age>=156 & Data$age<216),  "F2",
          ifelse((Data$sex==1 & Data$age>=216), "F3", NA))))))
Data$tqa <- TQa[TQa$scanid %in% Data$scanid, "averageManualRating"]
Data$pqa <- PQa[PQa$scanid %in% Data$scanid, "pcaslRelMeanRMSMotion"]
Pcasl <- Pcasl[Pcasl$scanid %in% Data$scanid,]
Mprage <- Mprage[Mprage$scanid %in% Data$scanid,]

#Average Left & Right
z = 10
for(x in regions_full){
  Data[,z] <- rowMeans(Mprage[,c(paste0("mprage_jlf_vol_L_", x), paste0("mprage_jlf_vol_R_", x))])
  z = z + 1
}
colnames(Data)[10:65] = c(paste0(regions,"_Vol"))
for(x in regions_full){
  y = 1
  while(y <= nrow(Pcasl)){
    Data[y,z] <-
      ((Pcasl[y,paste0("pcasl_jlf_cbf_L_", x)] *
      Mprage[y,paste0("mprage_jlf_vol_L_", x)]) +
      (Pcasl[y,paste0("pcasl_jlf_cbf_R_", x)] *
      Mprage[y,paste0("mprage_jlf_vol_R_", x)])) /
      (Mprage[y,paste0("mprage_jlf_vol_R_", x)] +
      Mprage[y,paste0("mprage_jlf_vol_R_", x)])
    y = y + 1
  }
  z = z + 1
}
colnames(Data)[66:121] = c(paste0(regions,"_CBF"))

#Scale Data
Data[,c(4,5,9:121)] <- scale(Data[,c(4,5,9:121)], center=TRUE, scale=TRUE)

Final <- data.frame(effect=rep(NA,times=672), region1=rep(NA,times=672),
  region2=rep(NA,times=672), group=rep(NA,times=672), metric=rep(NA,times=672))

w = 1
for(x in c("CBF", "Vol")){
  ifelse(x == "CBF", qa <- "pqa",
    ifelse(x == "Vol", qa <- "tqa", NA
    )
  )
  for(y in c("M1", "M2", "M3", "F1", "F2", "F3")){
    for(z in regions){
      Final[w,"effect"] <-
        lm(as.formula(paste0(z, "_", x, " ~ age + age2 + hand + ", qa)),
          Data[Data$group %in% y,])$coefficients["hand"]
      Final[w, "region1"] <- z
      Final[w, "region2"] <-
        ifelse(z %in% c("Thalamus", "Putamen", "Caudate", "Pallidum", "Accumbens"), "BasGang",
          ifelse(z %in% c("PHG", "Hippocampus", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent", "Amygdala", "MCgG"), "Limbic",
            ifelse(z %in% c("FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe", "FRP", "LOrG"), "FrontOrb",
              ifelse(z %in% c("PrG", "MSFG", "SMC", "MFG", "SFG"), "FrontDors",
                ifelse(z %in% c("FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG"), "Temporal",
                  ifelse(z %in% c("PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG"), "Parietal",
                    ifelse(z %in% c("IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG"), "Occipital", NA
                    )
                  )
                )
              )
            )
          )
        )
      Final[w,"group"] <- y
      Final[w,"metric"] <- x
      w = w + 1
    }
  }
}

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
  facet_grid(metric ~ region2, scales = "free", space="free_x")
