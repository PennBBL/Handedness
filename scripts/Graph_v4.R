### Create effect size plots for right versus left handers on brain regions,
### controlling for age, age^2, QA
### Zachary Heffernan
### April 5, 2021

#Set Working Directory & Import Data
setwd("/Users/zsheff/Documents/Handedness")
Demo <- read.csv("data/n1601_demographics_go1_20161212.csv")
Pcasl <- read.csv("data/n2416_jlfAntsCTIntersectionPcaslValues_20170404.csv")
Mprage <- read.csv("data/n2416_jlfAntsCTIntersectionVol_20170412.csv")
PQa <- read.csv("data/n2416_PcaslQaData_20170404.csv")
TQa <- read.csv("data/n2416_t1QaData_20170516.csv")

#Regions to Lobes mapping
regions_lobes <- data.frame(
  regions = c("Thalamus_Proper", "Putamen", "Caudate", "Pallidum",
      "Accumbens_Area",
    "PHG", "Hippocampus", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent",
      "Amygdala", "MCgG",
    "FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe",
      "FRP", "LOrG",
    "PrG", "MSFG", "SMC", "MFG", "SFG",
    "FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG",
    "PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG",
    "IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG"),
  lobes = c(rep("BasGang",times=5), rep("Limbic",times=10),
    rep("FrontOrb",times=11), rep("FrontDors",times=5), rep("Temporal",times=9),
    rep("Parietal",times=8), rep("Occipital",times=8)))

#Reassign Variables & Add Age^2; 0 - Right | 1 - Left; 0 - Male | 1 - Female
Demo$handednessv2 <- Demo$handednessv2 - 1
Demo$sex <- Demo$sex - 1
Demo$age2 <- (Demo$ageAtScan1)^2

#Participant List & Concatenate Raw Data
Data <- Demo[(Demo$scanid %in% TQa[TQa$t1Exclude==0, "scanid"]),
  c("bblid", "scanid", "sex", "ageAtScan1","age2","handednessv2")]
Data <- merge(Data, TQa[c("scanid", "averageManualRating")], by = "scanid")
Data <- merge(Data, PQa[c("scanid", "pcaslRelMeanRMSMotion", "pcaslExclude")],
  by = "scanid")
Data$group <- ifelse((Data$sex==0 & Data$ageAtScan1<156), "M1",
  ifelse((Data$sex==0 & Data$ageAtScan1>=156 & Data$ageAtScan1<216), "M2",
    ifelse((Data$sex==0 & Data$ageAtScan1>=216), "M3",
      ifelse((Data$sex==1 & Data$ageAtScan1<156), "F1",
        ifelse((Data$sex==1 & Data$ageAtScan1>=156 & Data$ageAtScan1<216), "F2",
          ifelse((Data$sex==1 & Data$ageAtScan1>=216), "F3", NA))))))
Pcasl <- Pcasl[Pcasl$scanid %in% Data$scanid,]
Mprage <- Mprage[Mprage$scanid %in% Data$scanid,]

#Average Left & Right
for(x in regions_lobes$regions){
  Data[,paste0(x,"_Vol")] <-
    rowMeans(Mprage[,c(paste0("mprage_jlf_vol_L_", x),
      paste0("mprage_jlf_vol_R_", x))])
  Data[paste0(x,"_CBF")] <-
    ((Pcasl[paste0("pcasl_jlf_cbf_L_", x)] *
    Mprage[paste0("mprage_jlf_vol_L_", x)]) +
    (Pcasl[paste0("pcasl_jlf_cbf_R_", x)] *
    Mprage[paste0("mprage_jlf_vol_R_", x)])) /
    (Mprage[paste0("mprage_jlf_vol_L_", x)] +
    Mprage[paste0("mprage_jlf_vol_R_", x)])
}

#Scale Data
Data[,!(names(Data) %in% c("scanid", "bblid", "sex", "handednessv2", "group",
  "pcaslExclude"))] <- scale(Data[,!(names(Data) %in% c("scanid", "bblid",
  "sex", "handednessv2", "group", "pcaslExclude"))], center=TRUE, scale=TRUE)

#Create Final Data Frame
Final <- expand.grid(regions = regions_lobes$regions,
  group = c("M1", "M2", "M3", "F1", "F2", "F3"), metric = c("CBF", "Vol"))
Final["lobes"] <- rep(regions_lobes$lobes, times=12)

#Run Models
x = 1
for(y in c("M1", "M2", "M3", "F1", "F2", "F3")){
  for(z in regions_lobes$regions){
    Final[x,"effect"] <- lm(as.formula(paste0(z,
      "_CBF ~ ageAtScan1 + age2 + handednessv2 + pcaslRelMeanRMSMotion")),
      Data[Data$group==y & Data$pcaslExclude==0,])$coefficients["handednessv2"]
    x = x + 1
  }
}
for(y in c("M1", "M2", "M3", "F1", "F2", "F3")){
  for(z in regions_lobes$regions){
    Final[x,"effect"] <- lm(as.formula(paste0(
      z, "_Vol ~ ageAtScan1 + age2 + handednessv2 + averageManualRating")),
      Data[Data$group %in% y,])$coefficients["handednessv2"]
    x = x + 1
  }
}

#Generate Plot
library(ggplot2)
plot <- ggplot(Final, aes(fill=group, x=regions, y=effect)) +
  geom_bar(position="dodge", color="black", stat="identity") +
  scale_fill_manual(
    values=c("#FCB0C0", "#FA1D83", "#BF0005", "#BFD9FF", "#4C9AEA", "#000078"),
    labels=c("Female Children", "Female Adolescents", "Female Adults",
      "Male Children", "Male Adolescents", "Female Adults")) +
  labs(x=NULL, y="Effect Size", fill="Group") +
  theme_linedraw() +
  theme(legend.position="bottom", axis.text.x = element_text(angle=45,
  hjust=.95)) + facet_grid(metric ~ lobes, scales = "free", space="free_x")

#Save Plot
pdf(paste0('/Users/zsheff/Documents/Handedness/plots/plot', Sys.Date(), '.pdf'),
  width=14, height=7)
	plot
	dev.off()
