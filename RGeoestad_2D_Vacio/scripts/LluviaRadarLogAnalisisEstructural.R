#--------------------------------------------------------------#
#                    Spatial Distribution                      #
#--------------------------------------------------------------#
dir.create(paste(getwd(),"/Results/AnalisisEstructuralRadar", sep=""))

png("Results/AnalisisEstructuralRadar/Radar_mm_Log_Spatial_Distr.png", bg = "white")
DEspacial(XCoord, YCoord, Radar_mm_Log,
          'X (Km)', 'Y (Km)', 'Radar_mm_Log (ppm)', 'Radar_mm_Log Spatial Distribution')
dev.off()

#--------------------------------------------------------------#
#               Trend (Stationarity) Analysis                  #
#--------------------------------------------------------------#

# Median Regression Analysis in X and Y directions for variable Radar_mm_Log
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_Trend_X_Y.png", bg = "white")
GDirecciones(XCoord, YCoord, Radar_mm_Log)
dev.off()

# Estimation of the experimental variogram
X_rng<-XCoord_Stat[8,2]
Y_rng<-YCoord_Stat[8,2]
N_lags<-14
lag_value <- sqrt(X_rng*X_rng+Y_rng*Y_rng)/(2*N_lags)
DistMin<-min(dist(Data_File[,1:2])) # Minimum distance in data
DistMax<-max(dist(Data_File[,1:2])) # Minimum distance in data
lag_value<-max((DistMax/2)/N_lags, DistMin)
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_VarioEstimation.png", bg = "white")
Radar_mm_Log_VarioEstimation<-Variograma(XCoord, YCoord, 
                                         Radar_mm_Log, 0, 90, N_lags, lag_value, 1, "Variograma Adireccional de Radar_mm_Log")
dev.off()

# Displaying the variogram estimated values
Radar_mm_Log_VarioEstimation

# Polynomial Trend Surface Estimation (degree = 1)
pol_degree=1
Radar_mm_Log_Detrended_1<-Trend(XCoord, YCoord, 
                                Radar_mm_Log, pol_degree) 

# Median Regression Analysis in X and Y directions for residuals Radar_mm_Log_Detrended_1
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_Detrended_1.png", bg = "white")
GDirecciones(Radar_mm_Log_Detrended_1[,1], Radar_mm_Log_Detrended_1[,2], Radar_mm_Log_Detrended_1[,3])
dev.off()

# Estimation of the experimental variogram (Detrended_1)
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_Detrended_1_VarioEstimation.png", bg = "white")
Radar_mm_Log_Detrended_1_VarioEstimation<-Variograma(Radar_mm_Log_Detrended_1[,1], Radar_mm_Log_Detrended_1[,2], 
                                                     Radar_mm_Log_Detrended_1[,3], 0, 90, N_lags, lag_value, 1, 
                                                     "Variograma Adireccional de Radar_mm_Log Residuos 1")
dev.off()

# Polynomial Trend Surface Estimation (degree = 2)
pol_degree=2
Radar_mm_Log_Detrended_2<-Trend(XCoord, YCoord, 
                                Radar_mm_Log, pol_degree) 

# Median Regression Analysis in X and Y directions for residuals Radar_mm_Log_Detrended_1
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_Detrended_2.png", bg = "white")
GDirecciones(Radar_mm_Log_Detrended_2[,1], Radar_mm_Log_Detrended_2[,2], Radar_mm_Log_Detrended_2[,3])
dev.off()

# Estimation of the experimental variogram (Detrended_2)
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_Detrended_2_VarioEstimation.png", bg = "white")
Radar_mm_Log_Detrended_2_VarioEstimation<-Variograma(Radar_mm_Log_Detrended_2[,1], Radar_mm_Log_Detrended_2[,2], 
                                                     Radar_mm_Log_Detrended_2[,3], 0, 90, N_lags, lag_value, 1, 
                                                     "Variograma Adireccional de Radar_mm_Log Residuos 2")
dev.off()

#--------------------------------------------------------------#
#       Univariate Variography (Structural)  Modeling          #
#--------------------------------------------------------------#

# Estimation of the experimental variogram
# Just showing because is already save above
Radar_mm_Log_VarioEstimation<-Variograma(XCoord, YCoord, 
                                         Radar_mm_Log, 135, 22.5, N_lags, lag_value, 1, "Variograma Adireccional de Radar_mm_Log_Log")
# Displaying the variogram estimated values
Radar_mm_Log_VarioEstimation

# Directional Variograms in 4 directions (0, 45, 90 and 135)
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_Vario4DEstimation.png", bg = "white")
Radar_mm_Log_VarioEstimation4D<-Variograma4D(XCoord, YCoord, 
                                         Radar_mm_Log, 0, 45, 90, 135, 22.5, N_lags, lag_value, 1, "Variogramas Direccionales de Radar_mm_Log")
dev.off()
# Displaying the four directional estimated variogram values
Radar_mm_Log_VarioEstimation4D

# Automatic Variogram Model Fitting
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_VarioAllModelEstimation.png", bg = "white")
Radar_mm_Log_AllModelVarioFit<-AllModel(XCoord, YCoord, 
                                    Radar_mm_Log, 0, 90, N_lags, lag_value, 1, "Ajustes del Variograma Adireccional de Radar_mm_Log")
dev.off()
# Displaying Automatic Variogram Model Fitting
Radar_mm_Log_AllModelVarioFit

# Best Automatic Variogram Model Fitting
#VariogramBestModel<-BestModel(Data[,1], Data[,2], Data[,3], 0, 90, 10, 150, 1, 'Elevation_of_seam Variogram')
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_VarioBestModelEstimation.png", bg = "white")
Radar_mm_Log_BestModelVarioFit<-BestModel(XCoord, YCoord, 
                                      Radar_mm_Log, 0, 90, N_lags, lag_value, 1, "Mejor Ajuste del Variograma Adireccional de Radar_mm_Log")
dev.off()
# Displaying Best Automatic Variogram Model Fitting
Radar_mm_Log_BestModelVarioFit

# Manual Variogram Model Fitting
#modelos de variograma (1- exponential, 2- spherical, 3- gaussian)
vario_model<- 1
nugget<- .17
sill_and_nugget<- 1.25
rank <- 20000

Radar_mm_Log_vario_model<- 1
Radar_mm_Log_nugget<- 0.17
Radar_mm_Log_sill_and_nugget<- 1.25
Radar_mm_Log_rank <- 20000
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_VarioEyeEstimation.png", bg = "white")
Radar_mm_Log_EyeModelVarioFit<-EyeModel(XCoord, YCoord, 
                                    Radar_mm_Log, 0, 90, N_lags, lag_value, 1, 
                                    Radar_mm_Log_vario_model, Radar_mm_Log_nugget, Radar_mm_Log_sill_and_nugget, Radar_mm_Log_rank,
                                    "Ajuste Manual del Variograma Adireccional de Radar_mm_Log")
dev.off()

# Variogram Model Cross Validation
library(gstat)
library(geoR)
Radar_mm_Log_CrossValid<- CrossValidation(XCoord, YCoord, 
                                      Radar_mm_Log, vario_model, nugget, sill_and_nugget, rank)

# Statistical Analysis of differences (Z-Z*)

# Basic Statistics in a single table
Radar_mm_Log_CrossValid_Sta <- Val_Estadisticos(Radar_mm_Log_CrossValid[1:102,c(3,4,5)])

# Histogram and Boxplot 1
png("Results/AnalisisEstructuralRadar/Radar_mm_Log-Radar_mm_Log+_HistBoxPlot1.png", bg = "white")
HistBoxplot(x=Radar_mm_Log_CrossValid[,5], mean = Radar_mm_Log_CrossValid_Sta[5,3], median = Radar_mm_Log_CrossValid_Sta[4,3], main ="", 
            xlab = "Radar_mm_Log-Radar_mm_Log* (ppm)", ylab = "Frecuencia Absoluta (conteo)", AbsFreq = TRUE, PercentFreq = FALSE )
dev.off()

# Spatial Distribution of differences (Z-Z*)
png("Results/AnalisisEstructuralRadar/Radar_mm_Log_CrossValid_Spatial_Distr.png", bg = "white")
DEspacial(Radar_mm_Log_CrossValid[,1], Radar_mm_Log_CrossValid[,2], Radar_mm_Log_CrossValid[,5],
          'X (Km)', 'Y (Km)', 'Radar_mm_Log-Radar_mm_Log* (ppm)', 'Radar_mm_Log - Radar_mm_Log*')
dev.off()

pos1 <- which(Radar_mm_Log_CrossValid[,5] > 1.3)
pos0 <- which(Radar_mm_Log_CrossValid[,5] < -2)

# Scatterplot with linear correlation coefficient

# Radar_mm_Log is the independent variable
X<-Radar_mm_Log_CrossValid[,3]
# Radar_mm_Log* is the dependent variable
Y<-Radar_mm_Log_CrossValid[,4]

png("Results/AnalisisEstructuralRadar/Radar_mm_Log-Radar_mm_Log+_ScatterPlot.png", bg = "white")
scaterplotReg(Radar_mm_Log_CrossValid[,3] , Radar_mm_Log_CrossValid[,4], 9, 
            Xmin = Radar_mm_Log_CrossValid_Sta[2,1], Xmax = Radar_mm_Log_CrossValid_Sta[7,1], 
            Ymin = Radar_mm_Log_CrossValid_Sta[2,2],Ymax = Radar_mm_Log_CrossValid_Sta[7,2], 
            XLAB = "Radar_mm_Log (ppm)", YLAB = "Radar_mm_Log* (ppm)")
dev.off()

scaterplotReg(Radar_mm_Log_CrossValid[,3] , Radar_mm_Log_CrossValid[,5], 9, 
            Xmin = Radar_mm_Log_CrossValid_Sta[2,1], Xmax = Radar_mm_Log_CrossValid_Sta[7,1], 
            Ymin = Radar_mm_Log_CrossValid_Sta[2,2],Ymax = Radar_mm_Log_CrossValid_Sta[7,2], 
            XLAB = "Radar_mm_Log (ppm)", YLAB = "Radar_mm_Log* (ppm)")