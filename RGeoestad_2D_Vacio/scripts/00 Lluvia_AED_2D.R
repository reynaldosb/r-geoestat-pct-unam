#### ------- RGeoestad script in 2D ------- ####


#--------------------------------------------------------------#
#                   Data Manipulation                          #
#--------------------------------------------------------------#
# Reading Data File in ASCII format (.txt)
# -999.25 for non available values

Data_File <- read.table(file="data/lluvia.txt",header=TRUE,na.strings="-999.25")

# Creates a folder to store images
dir.create(paste(getwd(),"/Results/AED", sep=""))

XCoord<-Data_File$UTM_X_m
YCoord<-Data_File$UTM_Y_m
Radar_mm<-Data_File$Radar_mm
Pluv_mm<-Data_File$Pluv_mm
  
#--------------------------------------------------------------#
#               Exploratory Data Analysis                      #
#--------------------------------------------------------------#


###------------ Univariate Data Analysis---------------------###

# Basic Statistics Estimation 

# Basic Statistics
XCoord_Stat<-Estadisticas(XCoord)
YCoord_Stat<-Estadisticas(YCoord)
Radar_mm_Stat<-Estadisticas(Radar_mm)
Pluv_mm_Stat<-Estadisticas(Pluv_mm)

# Basic Statistics in a single table
Data_File_Stat <- Val_Estadisticos(Data_File)

############  Radar_mm data
# Histogram and Boxplot
# Save plot as image in png format
png("Results/AED/Radar_mm_HistBoxPlotCounts7bins.png", bg = "white")
HistBoxplot(x=Radar_mm, mean = Radar_mm_Stat[5,2], median = Radar_mm_Stat[4,2], main ="",  
            xlab = "Radar_mm (ppm)", ylab = "Frecuencia Absoluta (conteo)", AbsFreq = TRUE, PercentFreq = FALSE,
            nbin = 9)
dev.off()
png("Results/AED/Radar_mm_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Radar_mm, mean = Radar_mm_Stat[5,2], median = Radar_mm_Stat[4,2], main ="", 
            xlab = "Radar_mm (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

# Distributional Outliers Detection and Identification
Radar_mm_outliers<-OutliersPos(Radar_mm)
Data_File[Radar_mm_outliers,c(1,2,3)] # Shows the outliers
# Variable without distributional outliers

Radar_mm_out<-Radar_mm[-Radar_mm_outliers]

# Data Analysis without Outliers

# Make the previous analysis without outliers

## OUTLIERS

# Basic Statistics
Radar_mm_out_Stat<-Estadisticas(Radar_mm_out)

# Histogram and Boxplot
png("Results/AED/Radar_mm_out_HistBoxPlotCounts.png", bg = "white")
HistBoxplot(x=Radar_mm_out, mean = Radar_mm_out_Stat[5,2], median = Radar_mm_out_Stat[4,2], main ="",  
            xlab = "Radar_mm_out (ppm)", ylab = "Frecuencia Absoluta (conteo)", AbsFreq = TRUE, PercentFreq = FALSE,
            nbin = 9)
dev.off()
png("Results/AED/Radar_mm_out_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Radar_mm_out, mean = Radar_mm_out_Stat[5,2], median = Radar_mm_out_Stat[4,2], main ="", 
            xlab = "Radar_mm_out (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

# Distributional Outliers Detection and Identification
Radar_mm_out_outliers<-OutliersPos(Radar_mm_out)

#---------------------Variable Transformations--------------------------------#
# SQUARE ROOT TRANSFORMATION
Data_File$Radar_mm_Sqrt<-sqrt(Radar_mm)
Radar_mm_Sqrt <- Data_File$Radar_mm_Sqrt

# Basic Statistics
Radar_mm_Sqrt_Stat<-Estadisticas(Radar_mm_Sqrt)

# Basic Statistics in a single table
Data_File_Stat <- Val_Estadisticos(Data_File)

# Histogram and Boxplot
png("Results/AED/Radar_mm_Sqrt_HistBoxPlotCounts.png", bg = "white")
HistBoxplot(x=Radar_mm_Sqrt, mean = Radar_mm_Sqrt_Stat[5,2], median = Radar_mm_Sqrt_Stat[4,2], main ="",  
            xlab = "Radar_mm_Sqrt (ppm)", ylab = "Frecuencia Absoluta (conteo)", AbsFreq = TRUE, PercentFreq = FALSE,
            nbin = 9)
dev.off()
png("Results/AED/Radar_mm_Sqrt_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Radar_mm_Sqrt, mean = Radar_mm_Sqrt_Stat[5,2], median = Radar_mm_Sqrt_Stat[4,2], main ="", 
            xlab = "Radar_mm_Sqrt (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

# Distributional Outliers Detection and Identification

Radar_mm_Sqrt_outliers<-OutliersPos(Radar_mm_Sqrt)
Data_File[Radar_mm_Sqrt_outliers,c(1,2,5)] # Shows the outliers
# Variable without distributional outliers
Radar_mm_Sqrt_out<-Radar_mm_Sqrt[-Radar_mm_Sqrt_outliers]

# no more outliers detected
OutliersPos(Radar_mm_Sqrt_out)

# LOGARITMIC TRANSFORMATION

Data_File$Radar_mm_Log<-log(Radar_mm)
Radar_mm_Log <- Data_File$Radar_mm_Log

# Basic Statistics
Radar_mm_Log_Stat<-Estadisticas(Radar_mm_Log)

# Basic Statistics in a single table
Data_File_Stat <- Val_Estadisticos(Data_File)

# Histogram and Boxplot
png("Results/AED/Radar_mm_Log_HistBoxPlotCounts.png", bg = "white")
HistBoxplot(x=Radar_mm_Log, mean = Radar_mm_Log_Stat[5,2], median = Radar_mm_Log_Stat[4,2], main ="",  
            xlab = "Radar_mm_Log (ppm)", ylab = "Frecuencia Absoluta (conteo)", AbsFreq = TRUE, PercentFreq = FALSE,
            nbin = 9)
dev.off()
png("Results/AED/Radar_mm_Log_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Radar_mm_Log, mean = Radar_mm_Log_Stat[5,2], median = Radar_mm_Log_Stat[4,2], main ="", 
            xlab = "Radar_mm_Log (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

# Distributional Outliers Detection and Identification
Radar_mm_Log_outliers<-OutliersPos(Radar_mm_Log)
Data_File[Radar_mm_Log_outliers,c(1,2,6)] # Shows the outliers
# Variable without distributional outliers
Radar_mm_Log_out<-Radar_mm_Log[-Radar_mm_Log_outliers]
# Data Analysis without Outliers

# Basic Statistics
Radar_mm_Log_out_Stat<-Estadisticas(Radar_mm_Log_out)

png("Results/AED/Radar_mm_Log_out_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Radar_mm_Log_out, mean = Radar_mm_Log_out_Stat[5,2], median = Radar_mm_Log_out_Stat[4,2], main ="", 
            xlab = "Radar_mm_Log_out (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

# Distributional Outliers Detection and Identification 2
Radar_mm_Log_outliers2<-OutliersPos(Radar_mm_Log_out)
Radar_mm_Log_out[Radar_mm_Log_outliers2]

# Variable without distributional outliers 2
Radar_mm_Log_out2<-Radar_mm_Log_out[-Radar_mm_Log_outliers2]


# Data Analysis without Outliers 2
# Basic Statistics in separated tables
Radar_mm_Log_out2_Stat<-Estadisticas(Radar_mm_Log_out2)

png("Results/AED/Radar_mm_Log_out2_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Radar_mm_Log_out2, mean = Radar_mm_Log_out2_Stat[5,2], median = Radar_mm_Log_out2_Stat[4,2], main ="", 
            xlab = "Radar_mm_Log_out2 (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

############  Pluv_mm data
# Basic Statistics
Pluv_mm_Stat <- Estadisticas(Pluv_mm)
png("Results/AED/Pluv_mm_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Pluv_mm, mean = Pluv_mm_Stat[5,2], median = Pluv_mm_Stat[4,2], main ="", 
            xlab = "Pluv_mm (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

Pluv_mm_Log <- log(Pluv_mm)
# Basic Statistics
Pluv_mm_Log_Stat <- Estadisticas(Pluv_mm_Log)
png("Results/AED/Pluv_mm_Log_HistBoxPlotFreq.png", bg = "white")
HistBoxplot(x=Pluv_mm_Log, mean = Pluv_mm_Log_Stat[5,2], median = Pluv_mm_Log_Stat[4,2], main ="", 
            xlab = "Pluv_mm_Log (ppm)", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE,
            nbin = 9)
dev.off()

###------------ Bivariate Data Analysis---------------------###
cor(Radar_mm , Pluv_mm, method = "pearson")
cor(Radar_mm , Pluv_mm, method = "spearman")

# Scatterplot with linear correlation coefficient
# Radar_mm is the independent variable (x-axis)
# Pluv_mm is the dependent variable (y-axis)
png("Results/AED/Radar_mm-Pluv_mm_ScatterPlot.png", bg = "white")
ScatterPlot(Radar_mm , Pluv_mm, 9, 
            Xmin = Radar_mm_Stat[2,2], Xmax = Radar_mm_Stat[7,2], 
            Ymin = Pluv_mm_Stat[2,2],Ymax = Pluv_mm_Stat[7,2], 
            XLAB = "Radar_mm (ppm)", YLAB = "Pluv_mm (ppm)")
dev.off()

png("Results/AED/Radar_mm-Pluv_mm_ScatterPlot_Out.png", bg = "white")
Radar_mm_Pluv_mm_BivariateOut<-OutliersCountTwo(XCoord, YCoord, 
                                     Radar_mm, Pluv_mm,
                                     XLAB = "Radar_mm (ppm)", YLAB = "Pluv_mm (ppm)") 
dev.off()


###------------ Linear Regression Analysis----------------------------###

# Scatterplot with regression line

png("Results/AED/Radar_mm-Pluv_mm_ScatterPlotReg.png", bg = "white")
scaterplotReg(Radar_mm , Pluv_mm, 9, 
              Xmin = Radar_mm_Stat[2,2], Xmax = Radar_mm_Stat[7,2], 
              Ymin = Pluv_mm_Stat[2,2],Ymax = Pluv_mm_Stat[7,2], 
              XLAB = "Radar_mm (ppm)", YLAB = "Pluv_mm (ppm)")
dev.off()

# Linear Regression Parameters
B0 <- -0.29
B1 <- 0.96

# Regression line and Residual Calculation
X<-Radar_mm
Y<-Pluv_mm

Y_Regression <- B0 + B1*X
Y_Residual <- Y-Y_Regression

# Residual Statistical Analysis
# Basic Statistics
Y_Residual_Stat<-Estadisticas(Y_Residual)

# Histogram and Boxplot 1
png("Results/AED/Pluv_mm_Residual_HistBoxPlot.png", bg = "white")
HistBoxplot(x=Y_Residual, mean = Y_Residual_Stat[5,2], median = Y_Residual_Stat[4,2], main ="", 
            xlab = "Residuos Pluv_mm", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE )
dev.off()


# Fitting a Normal Distribution
png("Results/AED/Pluv_mm_Residual_Fit.png")
FitDistr2_Residual_normal<-FitDistribution(data = Y_Residual, DISTR="norm", BREAKS = "Sturges", col = "darkgray", DistName = "Normal")
dev.off()

# Hypothesis Tests for Normality
FD_HT_Residual_normal<-FitDistr2_Residual_normal$x

# Normal Fitting Parameters
FD_FP_Residual_normal<-FitDistr2_Residual_normal$y

# Histogram Plotting with Fitting Distribution Curve
PARA_Residual_normal <- list(mean = as.numeric(FD_FP_Residual_normal[1,1]), sd = as.numeric(FD_FP_Residual_normal[2,1]))
HistModel(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, breaks = "Sturges", freq = FALSE, main ="Histograma y Ajuste", xlab = "Pluv_mm Residuos (ppm)", 
          ylab = "Densidad", colCurve =  "red", col = "darkgray")


# Cummulative Distribution Function with Fitting Curve
CDF(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, col = "gray", main = "DPA y Ajuste", xlab = "Pluv_mm Residuos (ppm)", 
    lcol = "red", lwd = 2)

# QQ-Plot
QQplot(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, col = "gray", main = "QQ-Plot", xlab = "Cuantiles teóricos", 
       lcol = "red", lwd = 2)

# PP-Plot
PPplot(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, col = "gray", main = "PP-Plot", xlab = "Probabilidades teóricas", 
       lcol = "red", lwd = 2)


###------------ Bivariate Analysis: Y vs Y_Residual ---------------------###

# Scatterplot with linear correlation coefficient
# Y_Residual is the independent variable
X<-Y_Residual
# Pluv_mm is the dependent variable
Y<-Pluv_mm

png("Results/AED/Pluv_mm-Residual_ScatterPlot.png", bg = "white")
scaterplot(X, Y, 9, 
           Xmin = Y_Residual_Stat[2,2], Xmax = Y_Residual_Stat[7,2], 
           Ymin = Pluv_mm_Stat[2,2],Ymax = Pluv_mm_Stat[7,2], XLAB = "Residuos Pluv_mm (ppm)", YLAB = "Pluv_mm (ppm)")
dev.off()

###------------ Bivariate Data Analysis---------------------###
######## TRANSFORMED DATA

png("Results/AED/Radar_mm_Log-Pluv_mm_Log_ScatterPlot.png", bg = "white")
ScatterPlot(Radar_mm_Log , Pluv_mm_Log, 9, 
            Xmin = Radar_mm_Log_Stat[2,2], Xmax = Radar_mm_Log_Stat[7,2], 
            Ymin = Pluv_mm_Log_Stat[2,2],Ymax = Pluv_mm_Log_Stat[7,2], 
            XLAB = "Radar_mm_Log (ppm)", YLAB = "Pluv_mm_Log (ppm)")
dev.off()

png("Results/AED/Radar_mm_Log-Pluv_mm_Log_ScatterPlot_Out.png", bg = "white")
Radar_mm_Log_Pluv_mm_Log_BivariateOut<-OutliersCountTwo(XCoord, YCoord, 
                                                        Radar_mm_Log, Pluv_mm_Log,
                                                        XLAB = "Radar_mm_Log (ppm)", YLAB = "Pluv_mm_Log (ppm)") 
dev.off()

###------------ Linear Regression Analysis----------------------------###

# Scatterplot with regression line

png("Results/AED/Radar_mm_Log-Pluv_mm_Log_ScatterPlotReg.png", bg = "white")
scaterplotReg(Radar_mm_Log , Pluv_mm_Log, 9, 
              Xmin = Radar_mm_Log_Stat[2,2], Xmax = Radar_mm_Log_Stat[7,2], 
              Ymin = Pluv_mm_Log_Stat[2,2],Ymax = Pluv_mm_Log_Stat[7,2], 
              XLAB = "Radar_mm_Log (ppm)", YLAB = "Pluv_mm_Log (ppm)")
dev.off()

# Linear Regression Parameters

B0 <- -0.34
B1 <- 0.92

# Regression line and Residual Calculation
X<-Radar_mm_Log
Y<-Pluv_mm_Log

Y_Regression <- B0 + B1*X
Y_Residual <- Y-Y_Regression

# Residual Statistical Analysis

# Basic Statistics
Y_Residual_Stat<-Estadisticas(Y_Residual)

# Histogram and Boxplot
png("Results/AED/Pluv_mm_Log_Residual_HistBoxPlot.png", bg = "white")
HistBoxplot(x=Y_Residual, mean = Y_Residual_Stat[5,2], median = Y_Residual_Stat[4,2], main ="", 
            xlab = "Residuos Pluv_mm_Log", ylab = "Frecuencia Relativa (%)", AbsFreq = FALSE, PercentFreq = TRUE )
dev.off()


# Fitting a Normal Distribution

FitDistr2_Residual_normal<-FitDistribution(data = Y_Residual, DISTR="norm", BREAKS = "Sturges", col = "darkgray", DistName = "Normal")

# Hypothesis Tests for Normality

FD_HT_Residual_normal<-FitDistr2_Residual_normal$x

# Normal Fitting Parameters

FD_FP_Residual_normal<-FitDistr2_Residual_normal$y

# Histogram Plotting with Fitting Distribution Curve

PARA_Residual_normal <- list(mean = as.numeric(FD_FP_Residual_normal[1,1]), sd = as.numeric(FD_FP_Residual_normal[2,1]))
HistModel(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, breaks = "Sturges", freq = FALSE, main ="Histograma y Ajuste", xlab = "Pluv_mm Residuos (ppm)", 
          ylab = "Densidad", colCurve =  "red", col = "darkgray")


# Cummulative Distribution Function with Fitting Curve
CDF(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, col = "gray", main = "DPA y Ajuste", xlab = "Pluv_mm Residuos (ppm)", 
    lcol = "red", lwd = 2)

# QQ-Plot
QQplot(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, col = "gray", main = "QQ-Plot", xlab = "Cuantiles te?ricos", 
       lcol = "red", lwd = 2)

# PP-Plot
PPplot(x = Y_Residual, distr = "norm", para = PARA_Residual_normal, col = "gray", main = "PP-Plot", xlab = "Probabilidades te?ricas", 
       lcol = "red", lwd = 2)


###------------ Bivariate Analysis: Y vs Y_Residual ---------------------###

# Scatterplot with linear correlation coefficient

# Y_Residual is the independent variable
X<-Y_Residual
# Pluv_mm_Log is the dependent variable
Y<-Pluv_mm_Log

png("Results/AED/Pluv_mm_Log-Residual_ScatterPlot.png", bg = "white")
scaterplot(X, Y, 9, 
           Xmin = Y_Residual_Stat[2,2], Xmax = Y_Residual_Stat[7,2], 
           Ymin = Pluv_mm_Log_Stat[2,2],Ymax = Pluv_mm_Log_Stat[7,2], XLAB = "Residuos Pluv_mm_Log (ppm)", YLAB = "Pluv_mm_Log (ppm)")
dev.off()

#--------------------------------------------------------------#
#                    Spatial Distribution                      #
#--------------------------------------------------------------#

png("Results/AED/Radar_mm_Spatial_Distr.png", bg = "white")
DEspacial(XCoord, YCoord, Radar_mm,
          "X (UTM)", 'Y (UTM)', 'Radar_mm (ppm)', 'Radar_mm Spatial Distribution ')
dev.off()

png("Results/AED/Pluv_mm_Spatial_Distr.png", bg = "white")
DEspacial(XCoord, YCoord, Pluv_mm,
          'X (UTM)', 'Y (UTM)', 'Pluv_mm (ppm)', 'Pluv_mm Spatial Distribution ')
dev.off()
