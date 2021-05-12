### Create effect size plots for right versus left handers on average and
### laterality index for brain regions, controlling for age, age^2, QA
### Zachary Heffernan
### May 12, 2021

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

#Average & Laterality; Transformation; Scale
for(x in names$full){
  Data[,paste0(x, "_avg_Vol")] <-
    rowMeans(Mprage[Mprage$scanid %in% Data$scanid,c(paste0("mprage_jlf_vol_L_", x),
      paste0("mprage_jlf_vol_R_", x))])
  Data[,paste0(x, "_avg_CBF")] <-
    ((Pcasl[Pcasl$scanid %in% Data$scanid, paste0("pcasl_jlf_cbf_L_", x)] *
    Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_L_", x)]) +
    (Pcasl[Pcasl$scanid %in% Data$scanid, paste0("pcasl_jlf_cbf_R_", x)] *
    Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_R_", x)])) /
    (Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_L_", x)] +
    Mprage[Mprage$scanid %in% Data$scanid, paste0("mprage_jlf_vol_R_", x)])
  for(y in c("Vol", "CBF")){
    ifelse(y=="Vol", c(a <- Mprage, b <- "mprage_jlf_vol", c <- Data$t1Exclude),
      ifelse(y=="CBF", c(a <- Pcasl, b <- "pcasl_jlf_cbf",
        c <- Data$pcaslExclude), NA
      )
    )
    Data[,paste0(x, "_lat_", y)] <-
      (2 * (a[a$scanid %in% Data$scanid, paste0(b, "_L_", x)] -
      a[a$scanid %in% Data$scanid, paste0(b, "_R_", x)])) /
      (a[a$scanid %in% Data$scanid, paste0(b, "_L_", x)] +
      a[a$scanid %in% Data$scanid, paste0(b, "_R_", x)])
    Data[,paste0(x, "_lat_", y)] <- asin(sqrt(Data[,paste0(x, "_lat_", y)]+2)-1)
    for(z in c("_avg_", "_lat_")){
      Data[c == 0, names(Data) == paste0(x, z, y)] <- scale(Data[c == 0,
        names(Data) == paste0(x, z, y)], center=TRUE, scale=TRUE)
    }
  }
}

for(x in c("CBF", "Vol")){
  ifelse(x=="Vol", c(a <- Mprage, b <- "averageManualRating",
    c <- Data$t1Exclude),
    ifelse(x=="CBF", c(a <- Pcasl, b <- "pcaslRelMeanRMSMotion",
      c <- Data$pcaslExclude), NA
    )
  )
  Data[c == 0, names(Data) %in% c(b, paste0("age1_", x), paste0("age2_", x))] <-
    scale(Data[c == 0, names(Data) %in% c(b, paste0("age1_", x),
    paste0("age2_", x))], center=TRUE, scale=TRUE)
}

#Create Final Data Frame
Final <- expand.grid(regions = names$regions,
  group = c("F1", "F2", "F3", "M1", "M2", "M3"),
  metric = c("CBF", "Volume"),
  combo = c("Average", "Laterality"))
Final["lobes"] <- rep(names$lobes, times=24)

###Run Models
v = 1
for(w in c("avg_", "lat_")){
  for(x in c("CBF", "Vol")){
    ifelse(x=="CBF", c(exclude <-Data$pcaslExclude, qa <-"pcaslRelMeanRMSMotion"),
      ifelse(x=="Vol", c(exclude <-Data$t1Exclude, qa <-"averageManualRating"), NA
      )
    )
    for(y in c("F1", "F2", "F3", "M1", "M2", "M3")){
      for(z in names$full){
        Final[v,"effect"] <- lm(as.formula(paste0(z,
          "_", w, x, " ~ age1_", x, " + age2_", x, " + handednessv2 + ", qa)),
          Data[Data$group == y & exclude == 0,])$coefficients["handednessv2"]
        v = v + 1
      }
    }
  }
}

###Generate Plot
library(ggplot2); library(ggh4x)
plot_gm <- ggplot(Final, aes(fill=group, x=regions, y=effect)) +
  geom_bar(position="dodge", color="black", stat="identity") +
  scale_fill_manual(
    values=c("#FCB0C0", "#FA1D83", "#BF0005", "#BFD9FF", "#4C9AEA", "#000078"),
    labels=c("Female Children", "Female Adolescents", "Female Adults",
      "Male Children", "Male Adolescents", "Male Adults"),
      guide = guide_legend(nrow=1)) +
  labs(x=NULL, y="Effect Size", fill="Group") +
  theme_linedraw() + scale_y_continuous(limits = c(-1, 0.6), breaks = seq(-1, 0.6, 0.2)) +
  theme(legend.position="bottom", axis.text.x = element_text(angle=45,
  hjust=.95)) + facet_nested(combo + metric ~ lobes, scales = "free", space="free_x")

#Save Plot
pdf(paste0('/Users/zsheff/Documents/Handedness/plots/plot_gm', Sys.Date(), '.pdf'),
  width=14, height=8.5)
	plot_gm
	dev.off()
