#--------------------------------------------------------------#
#                    Spatial Distribution                      #
#--------------------------------------------------------------#
dir.create(paste(getwd(),"/Results/AnalisisEstructural", sep=""))

png("Results/AnalisisEstructural/Pluv_mm_Log_Spatial_Distr.png", bg = "white")
DEspacial(XCoord, YCoord, Pluv_mm_Log,
          'X (Km)', 'Y (Km)', 'Pluv_mm_Log (ppm)', 'Pluv_mm_Log Spatial Distribution ')
dev.off()

#--------------------------------------------------------------#
#               Trend (Stationarity) Analysis                  #
#--------------------------------------------------------------#

# Median Regression Analysis in X and Y directions for variable Pluv_mm_Log
png("Results/AnalisisEstructural/Pluv_mm_Log_Trend_X_Y.png", bg = "white")
GDirecciones(XCoord, YCoord, Pluv_mm_Log)
dev.off()

# Estimation of the experimental variogram
N_lags<-10
DistMin<-min(dist(Data_File[,1:2])) # Minimum distance in data
DistMax<-max(dist(Data_File[,1:2])) # Maximum distance in data
lag_value<-max((DistMax/2)/N_lags, DistMin)
png("Results/AnalisisEstructural/Pluv_mm_Log_VarioEstimation.png", bg = "white")
Pluv_mm_Log_VarioEstimation<-Variograma(XCoord, YCoord, 
                                         Pluv_mm_Log, 0, 90, 1*N_lags, lag_value, 1, "Variograma Adireccional de Pluv_mm_Log")
dev.off()

# Displaying the variogram estimated values
Pluv_mm_Log_VarioEstimation

# Polynomial Trend Surface Estimation (degree = 1)
pol_degree=1
Pluv_mm_Log_Detrended_1<-Trend(XCoord, YCoord, 
                                Pluv_mm_Log, pol_degree) 

# Median Regression Analysis in X and Y directions for residuals Pluv_mm_Log_Detrended_1
png("Results/AnalisisEstructural/Pluv_mm_Log_Detrended_1.png", bg = "white")
GDirecciones(Pluv_mm_Log_Detrended_1[,1], Pluv_mm_Log_Detrended_1[,2], Pluv_mm_Log_Detrended_1[,3])
dev.off()

# Estimation of the experimental variogram (Detrended_1)
png("Results/AnalisisEstructural/Pluv_mm_Log_Detrended_1_VarioEstimation.png", bg = "white")
Pluv_mm_Log_Detrended_1_VarioEstimation<-Variograma(Pluv_mm_Log_Detrended_1[,1], Pluv_mm_Log_Detrended_1[,2], 
                                                     Pluv_mm_Log_Detrended_1[,3], 0, 90, N_lags, lag_value, 1, 
                                                     "Variograma Adireccional de Pluv_mm_Log Residuos 1")
dev.off()

# Polynomial Trend Surface Estimation (degree = 2)
pol_degree=2
Pluv_mm_Log_Detrended_2<-Trend(XCoord, YCoord, 
                                Pluv_mm_Log, pol_degree) 

# Median Regression Analysis in X and Y directions for residuals Pluv_mm_Log_Detrended_1
png("Results/AnalisisEstructural/Pluv_mm_Log_Detrended_2.png", bg = "white")
GDirecciones(Pluv_mm_Log_Detrended_2[,1], Pluv_mm_Log_Detrended_2[,2], Pluv_mm_Log_Detrended_2[,3])
dev.off()

# Estimation of the experimental variogram (Detrended_2)
png("Results/AnalisisEstructural/Pluv_mm_Log_Detrended_2_VarioEstimation.png", bg = "white")
Pluv_mm_Log_Detrended_2_VarioEstimation<-Variograma(Pluv_mm_Log_Detrended_2[,1], Pluv_mm_Log_Detrended_2[,2], 
                                                     Pluv_mm_Log_Detrended_2[,3], 0, 90, N_lags, lag_value, 1, 
                                                     "Variograma Adireccional de Pluv_mm_Log Residuos 2")
dev.off()

#--------------------------------------------------------------#
#       Univariate Variography (Structural)  Modeling          #
#--------------------------------------------------------------#

# Estimation of the experimental variogram
# Just showing because is already save above
Pluv_mm_Log_VarioEstimation<-Variograma(XCoord, YCoord, 
                                         Pluv_mm_Log, 0, 90, N_lags, lag_value, 1, "Variograma Adireccional de Pluv_mm_Log_Log")

# Displaying the variogram estimated values
Pluv_mm_Log_VarioEstimation

# Directional Variograms in 4 directions (0, 45, 90 and 135)
png("Results/AnalisisEstructural/Pluv_mm_Log_Vario4DEstimation.png", bg = "white")
Pluv_mm_Log_VarioEstimation4D<-Variograma4D(XCoord, YCoord, 
                                         Pluv_mm_Log, 0, 45, 90, 135, 22.5, N_lags, lag_value, 1, "Variogramas Direccionales de Pluv_mm_Log")
dev.off()

# Displaying the four directional estimated variogram values
Pluv_mm_Log_VarioEstimation4D

# Automatic Variogram Model Fitting
png("Results/AnalisisEstructural/Pluv_mm_Log_VarioAllModelEstimation.png", bg = "white")
Pluv_mm_Log_AllModelVarioFit<-AllModel(XCoord, YCoord, 
                                    Pluv_mm_Log, 0, 90, N_lags, lag_value, 1, "Ajustes del Variograma Adireccional de Pluv_mm_Log")
dev.off()

# Displaying Automatic Variogram Model Fitting
Pluv_mm_Log_AllModelVarioFit

# Best Automatic Variogram Model Fitting
#VariogramBestModel<-BestModel(Data[,1], Data[,2], Data[,3], 0, 90, 10, 150, 1, 'Elevation_of_seam Variogram')
png("Results/AnalisisEstructural/Pluv_mm_Log_VarioBestModelEstimation.png", bg = "white")
Pluv_mm_Log_BestModelVarioFit<-BestModel(XCoord, YCoord, 
                                      Pluv_mm_Log, 0, 90, N_lags, lag_value, 1, "Mejor Ajuste del Variograma Adireccional de Pluv_mm_Log")
dev.off()
# Displaying Best Automatic Variogram Model Fitting
Pluv_mm_Log_BestModelVarioFit

# Manual Variogram Model Fitting
#modelos de variograma (1- exponential, 2- spherical, 3- gaussian)
vario_model<- 2 # 2
nugget<- 0.08 # 0.08
sill_and_nugget<- 1.1 # 1.1
rank <- 15000 # 15000
png("Results/AnalisisEstructural/Pluv_mm_Log_VarioEyeEstimation.png", bg = "white")
Pluv_mm_Log_EyeModelVarioFit<-EyeModel(XCoord, YCoord, 
                                    Pluv_mm_Log, 0, 90, N_lags, lag_value, 1, 
                                    vario_model, nugget, sill_and_nugget, rank, "Ajuste Manual del Variograma Adireccional de Pluv_mm_Log")
dev.off()

# Variogram Model Cross Validation
library(gstat)
Pluv_mm_Log_CrossValid<- CrossValidation(XCoord, YCoord, 
                                      Pluv_mm_Log, vario_model, nugget, sill_and_nugget, rank)

# Displaying
Pluv_mm_Log_CrossValid

## Statistical Analysis of differences (Z-Z*)

# Basic Statistics in a single table
Pluv_mm_Log_CrossValid_Sta<- Val_Estadisticos(Pluv_mm_Log_CrossValid[,c(3,4,5)])

# Histogram and Boxplot 1
png("Results/AnalisisEstructural/Pluv_mm_Log-Pluv_mm_Log+_HistBoxPlot1.png", bg = "white")
HistBoxplot(x=Pluv_mm_Log_CrossValid[,5], mean = Pluv_mm_Log_CrossValid_Sta[5,3], median = Pluv_mm_Log_CrossValid_Sta[4,3], main ="", 
            xlab = "Pluv_mm_Log-Pluv_mm_Log* (ppm)", ylab = "Frecuencia Absoluta (conteo)", AbsFreq = TRUE, PercentFreq = FALSE )
dev.off()

# Spatial Distribution of differences (Z-Z*)
png("Results/AnalisisEstructural/Pluv_mm_Log_CrossValid_Spatial_Distr.png", bg = "white")
DEspacial(Pluv_mm_Log_CrossValid[,1], Pluv_mm_Log_CrossValid[,2], Pluv_mm_Log_CrossValid[,5],
          'X (Km)', 'Y (Km)', 'Pluv_mm_Log-Pluv_mm_Log* (ppm)', 'Pluv_mm_Log - Pluv_mm_Log*')
dev.off()

# Scatterplot with linear correlation coefficient

# Pluv_mm_Log is the independent variable
X<-Pluv_mm_Log_CrossValid[,3] # the same as Pluv_mm_Log
# Pluv_mm_Log* is the dependent variable
Y<-Pluv_mm_Log_CrossValid[,4]

# Basic Statistics of the estimation (Pluv_mm_Log*)
Pluv_mm_Log_CrossValid_Stat <- Estadisticas(Pluv_mm_Log_CrossValid[,4])

png("Results/AnalisisEstructural/Pluv_mm_Log-Pluv_mm_Log+_ScatterPlot.png", bg = "white")
ScatterPlot(Pluv_mm_Log, Pluv_mm_Log_CrossValid[,4], 9, 
            Xmin = Pluv_mm_Log_Stat[2,2], Xmax = Pluv_mm_Log_Stat[7,2], 
            Ymin = Pluv_mm_Log_CrossValid_Stat[2,2],Ymax = Pluv_mm_Log_CrossValid_Stat[7,2], 
            XLAB = "Pluv_mm_Log (ppm)", YLAB = "Pluv_mm_Log* (ppm)")
dev.off()