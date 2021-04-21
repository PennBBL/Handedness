### Create effect size plots for right versus left handers on brain regions,
### controlling for age, age^2, QA
### Zachary Heffernan
### April 21, 2021

#Set Working Directory & Import Data
setwd("/Users/zsheff/Documents/Handedness")
Demo <- read.csv("data/n1601_demographics_go1_20161212.csv")
Pcasl <- read.csv("data/n2416_jlfAntsCTIntersectionPcaslValues_20170404.csv")
Mprage <- read.csv("data/n2416_jlfAntsCTIntersectionVol_20170412.csv")
PQa <- read.csv("data/n2416_PcaslQaData_20170404.csv")
TQa <- read.csv("data/n2416_t1QaData_20170516.csv")

#Regions to Lobes mapping
names <- data.frame(
  full = c("Thalamus_Proper", "Putamen", "Caudate", "Pallidum",
      "Accumbens_Area",
    "PHG", "Hippocampus", "PIns", "SCA", "AIns", "ACgG", "PCgG", "Ent",
      "Amygdala", "MCgG",
    "FO", "MFC", "MOrG", "POrG", "OrIFG", "TrIFG", "AOrG", "OpIFG", "GRe",
      "FRP", "LOrG",
    "PrG", "MSFG", "SMC", "MFG", "SFG",
    "FuG", "PT", "PP", "ITG", "CO", "MTG", "TMP", "STG", "TTG",
    "PCu", "PoG", "AnG", "PO", "SPL", "MPrG", "SMG", "MPoG",
    "IOG", "Cun", "LiG", "OFuG", "MOG", "Calc", "OCP", "SOG"),
  regions = c("Thalamus", "Putamen", "Caudate", "Pallidum", "Accumbens",
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
Demo$sex <- Demo$sex - 1; Demo$age2 <- (Demo$ageAtScan1)^2
Demo$ageAtScan1_2 <- Demo$ageAtScan1; Demo$age2_2 <- Demo$age2
colnames(Demo)[colnames(Demo) %in% c("ageAtScan1", "ageAtScan1_2", "age2",
  "age2_2")] <- c("age1_Vol", "age1_CBF", "age2_Vol", "age2_CBF")

#Concatenate Raw Data
Data <- merge(Demo[c("bblid", "scanid", "sex", "age1_Vol", "age1_CBF",
  "age2_Vol", "age2_CBF", "handednessv2")], TQa[c("scanid", "t1Exclude",
  "averageManualRating")], by = "scanid")
Data <- merge(Data, PQa[c("scanid", "pcaslRelMeanRMSMotion", "pcaslExclude")],
  by = "scanid")

#Group Designation
Data$group <- ifelse((Data$sex==0 & Data$age1_Vol<156), "M1",
  ifelse((Data$sex==0 & Data$age1_Vol>=156 & Data$age1_Vol<216), "M2",
    ifelse((Data$sex==0 & Data$age1_Vol>=216), "M3",
      ifelse((Data$sex==1 & Data$age1_Vol<156), "F1",
        ifelse((Data$sex==1 & Data$age1_Vol>=156 & Data$age1_Vol<216), "F2",
          ifelse((Data$sex==1 & Data$age1_Vol>=216), "F3", NA))))))

#Average Left & Right
for(x in names$full){
  Data[,paste0(x,"_Vol")] <-
    rowMeans(Mprage[Mprage$scanid %in% Data$scanid,c(paste0("mprage_jlf_vol_L_", x),
      paste0("mprage_jlf_vol_R_", x))])
  Data[,paste0(x,"_CBF")] <-
    ((Pcasl[Pcasl$scanid %in% Data$scanid, paste0("pcasl_jlf_cbf_L_", x)] *
    Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_L_", x)]) +
    (Pcasl[Pcasl$scanid %in% Data$scanid, paste0("pcasl_jlf_cbf_R_", x)] *
    Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_R_", x)])) /
    (Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_L_", x)] +
    Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_R_", x)])
}

#Scale Data
for(x in names$full){
  Data[Data$t1Exclude == 0, names(Data) == paste0(x, "_Vol")] <-
    scale(Data[Data$t1Exclude == 0, names(Data) == paste0(x, "_Vol")],
    center=TRUE, scale=TRUE)
  Data[Data$pcaslExclude == 0, names(Data) == paste0(x, "_CBF")] <-
    scale(Data[Data$pcaslExclude == 0, names(Data) == paste0(x, "_CBF")],
    center=TRUE, scale=TRUE)
}
Data[Data$pcaslExclude == 0, names(Data) %in% c("pcaslRelMeanRMSMotion",
  "age1_CBF", "age2_CBF")] <- scale(Data[Data$pcaslExclude == 0, names(Data) %in%
  c("pcaslRelMeanRMSMotion", "age1_CBF", "age2_CBF")], center=TRUE, scale=TRUE)
Data[Data$t1Exclude == 0, names(Data) %in% c("averageManualRating", "age1_Vol",
  "age2_Vol")] <- scale(Data[Data$t1Exclude == 0, names(Data) %in%
  c("averageManualRating", "age1_Vol", "age2_Vol")], center=TRUE, scale=TRUE)

#Create Final Data Frame
Final <- expand.grid(regions = names$regions,
  group = c("F1", "F2", "F3", "M1", "M2", "M3"), metric = c("CBF", "Vol"))
Final["lobes"] <- rep(names$lobes, times=12)

#Run Models
w = 1
for(x in c("CBF", "Vol")){
  ifelse(x=="CBF", c(exclude <-Data$pcaslExclude, qa <-"pcaslRelMeanRMSMotion"),
    ifelse(x=="Vol", c(exclude <-Data$t1Exclude, qa <-"averageManualRating"), NA
    )
  )
  for(y in c("F1", "F2", "F3", "M1", "M2", "M3")){
    for(z in names$full){
      Final[w,"effect"] <- lm(as.formula(paste0(z,
        "_", x, " ~ age1_", x, " + age2_", x, " + handednessv2 + ", qa)),
        Data[Data$group == y & exclude == 0,])$coefficients["handednessv2"]
      w = w + 1
    }
  }
}

#Generate Plot
library(ggplot2)
plot <- ggplot(Final, aes(fill=group, x=regions, y=effect)) +
  geom_bar(position="dodge", color="black", stat="identity") +
  scale_fill_manual(
    values=c("#FCB0C0", "#FA1D83", "#BF0005", "#BFD9FF", "#4C9AEA", "#000078"),
    labels=c("Female Children", "Female Adolescents", "Female Adults",
      "Male Children", "Male Adolescents", "Male Adults"),
      guide = guide_legend(nrow=1)) +
  labs(x=NULL, y="Effect Size", fill="Group") +
  theme_linedraw() +
  theme(legend.position="bottom", axis.text.x = element_text(angle=45,
  hjust=.95)) + facet_grid(metric ~ lobes, scales = "free", space="free_x")

#Save Plot
pdf(paste0('/Users/zsheff/Documents/Handedness/plots/plot', Sys.Date(), '.pdf'),
  width=14, height=7)
	plot
	dev.off()
