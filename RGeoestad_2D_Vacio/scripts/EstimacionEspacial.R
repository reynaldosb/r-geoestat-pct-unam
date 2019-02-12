#--------------------------------------------------------------#
#                 Univariate Spatial Estimation                #
#--------------------------------------------------------------#

dir.create(paste(getwd(),"/Results/EstimacionEspacial", sep=""))
library(gstat)
library(geoR)

######## Radar_mm_Log
#modelos de variograma (1- exponential, 2- spherical, 3- gaussian)
Radar_mm_Log_vario_model<- 1
Radar_mm_Log_nugget<- 0.17
Radar_mm_Log_sill_and_nugget<- 1.25
Radar_mm_Log_rank <- 20000

minPoints<-4
maxPoints<-10

library(gstat)
library(geoR)
library(sp)

# "CoorX" X axis coordinates
# "CoorY" Y axis coordinates
# "Prop1" property
# "Modelo" can take values 1 (Exponential), 2 (Spherical) or 3 (Gaussian).
# "SillYNugget" Sill
# "Alcance" variogram model range
# "minPar" is the minimum number of points to do the estimation
# "maxPar" is the maximu number of points to do the estimation
# "Xmin" the minimum X value
# "Xmax" the maximum X value
# "Ymin" the minimum Y value
# "Ymax" the maximum Y value
# "TX" # width of the cells in the X direction for kriging estimation
# "TY" # width of the cells in the Y direction for kriging estimation
# "InvT" is the transformation made:0 for no transformation, 1 for "log", 2 for "square root", and 3 for "inverse" transformatio
# "NameX" is the name of the X axis
# "NameY" is the name of the Y axis
png("Results/EstimacionEspacial/Radar_mm_Log_Kriging.png", bg = "white")
Radar_mm_Log_OrdKrig <- KrigingOrd(CoorX = XCoord, CoorY = YCoord, 
                             Prop1 = Radar_mm_Log, Modelo = Radar_mm_Log_vario_model, Nugget = Radar_mm_Log_nugget, 
                             SillYNugget = Radar_mm_Log_sill_and_nugget, Alcance = Radar_mm_Log_rank,
                             minPar = minPoints, maxPar = maxPoints, 
                             Xmin = XCoord_Stat[2,2], Xmax = XCoord_Stat[7,2], 
                             Ymin = YCoord_Stat[2,2], Ymax = YCoord_Stat[7,2], 
                             TX=DistMin, TY=DistMin, InvT=1, NameX="X (Km)", 
                             NameY="Y (Km)", Titulo1="Kriging Radar_mm_Log (ppm)", 
                             Titulo2="SD Radar_mm_Log (ppm)") 
dev.off()
head(Radar_mm_Log_OrdKrig)
# en esta parte se rializan los gráficos anteriores de manera separada 
source(file.choose()) # choose PlotGridCells.R
png("Results/EstimacionEspacial/Radar_mm_Log_Kriging_Est.png", bg = "white")
PlotGridCells(Z=Radar_mm_Log_OrdKrig[,3],
              x=Radar_mm_Log_OrdKrig[,1:2],
              PointsCoord = cbind(XCoord, YCoord),
              grid = list(x = unique(Radar_mm_Log_OrdKrig[,1]),
                          y = unique(Radar_mm_Log_OrdKrig[,2])),
              xlab = "X (Km)", ylab = "Y (Km)",
              main = "Kriging Est Radar_mm_Log (ppm)")
#points(Data_File(,1:2), cex=3, pech=)
dev.off()

png("Results/EstimacionEspacial/Radar_mm_Log_Kriging_SD.png", bg = "white")
PlotGridCells(Z=Radar_mm_Log_OrdKrig[,4],
              x=Radar_mm_Log_OrdKrig[,1:2],
              PointsCoord = cbind(XCoord, YCoord),
              grid = list(x = unique(Radar_mm_Log_OrdKrig[,1]),
                          y = unique(Radar_mm_Log_OrdKrig[,2])),
              xlab = "X (Km)", ylab = "Y (Km)",
              main = "Kriging SD Radar_mm_Log (ppm)")
dev.off()
# en el caso de tener un valor de anisotopria de 2 a 3, correr esta parte
# "malla" is the length of the squared grid cells. Instead of "TX" and "TY"
# "MaxAnis" refers to the main axis direction: it is the angle for the principal direction of continuity (measured in degrees, clockwise from positive Y, i.e. North)
# "proporcion" is the anisotropy ratio, the ratio of the minor range to the major range (a value between 0 and 1)
png("Results/EstimacionEspacial/Radar_mm_Log_KrigingAnis.png", bg = "white")
Pluv_mm_Log_OrdKrig <- KrigingOrdAnis(CoorX = XCoord, CoorY = YCoord, 
               Prop1 = Radar_mm_Log, Modelo = Radar_mm_Log_vario_model, Nugget = Radar_mm_Log_nugget, 
               SillYNugget = Radar_mm_Log_sill_and_nugget, Alcance = Radar_mm_Log_rank,
               minPar = minPoints, maxPar = maxPoints,
               malla = DistMin, InvT = 1, MaxAnis = 0, proporcion = 0.5,
               NameX="X (Km)", 
               NameY="Y (Km)", Titulo1="Anisotropic Kriging Radar_mm_Log (ppm)", 
               Titulo2="SD Radar_mm_Log (ppm)")
dev.off()

######## Pluv_mm_Log, aquí mejor modelo del análisis univariada
Pluv_mm_Log_vario_model<- 2
Pluv_mm_Log_nugget<- 0.3
Pluv_mm_Log_sill_and_nugget<- 1.12
Pluv_mm_Log_rank <- 20000
png("Results/EstimacionEspacial/Pluv_mm_Log_Kriging.png", bg = "white")
Pluv_mm_Log_OrdKrig <- KrigingOrd(CoorX = XCoord, CoorY = YCoord, 
                                   Prop1 = Pluv_mm_Log, Modelo = Pluv_mm_Log_vario_model, Nugget = Pluv_mm_Log_nugget, 
                                   SillYNugget = Pluv_mm_Log_sill_and_nugget, Alcance = Pluv_mm_Log_rank,
                                   minPar = minPoints, maxPar = maxPoints, 
                                   Xmin = XCoord_Stat[2,2], Xmax = XCoord_Stat[7,2], 
                                   Ymin = YCoord_Stat[2,2], Ymax = YCoord_Stat[7,2], 
                                   TX=DistMin, TY=DistMin, InvT=1, NameX="X (Km)", 
                                   NameY="Y (Km)", Titulo1="Kriging Pluv_mm_Log (ppm)", 
                                   Titulo2="SD Pluv_mm_Log (ppm)") 
dev.off()
head(Pluv_mm_Log_OrdKrig)

#--------------------------------------------------------------#
#       Bivariate Variography (Structural)  Modeling           #
#--------------------------------------------------------------#

# Estimation of the experimental variogram
X_rng<-XCoord_Stat[8,2]
Y_rng<-YCoord_Stat[8,2]
N_lags<-14
DistMin<-min(dist(Data_File[,1:2])) # Minimum distance in data
DistMax<-max(dist(Data_File[,1:2])) # Maximum distance in data
lag_value<-3000 #max((DistMax/2)/N_lags, DistMin)
png("Results/EstimacionEspacial/Radar_mm_Log_Pluv_mm_Log_CrossVario.png", bg = "white")
Radar_mm_Log_Pluv_mm_Log_CrossVario<-CrossVariograma(CoorX = XCoord, CoorY = YCoord, 
                                             P1 = Radar_mm_Log, P2 = Pluv_mm_Log, 
                                             NInt = N_lags, lags = lag_value,
                                             Direccion = 0, Tol = 90, 
                                             NomP1 = 'Radar_mm_Log Variogram',
                                             NomP2 = 'Pluv_mm_Log Variogram',
                                             NomP1P2 = 'Radar_mm_Log-Pluv_mm_Log CrossVariogram')
dev.off()

# Radar_mm_Log
Radar_mm_Log_vario_model<- 2 #modelo del variograma 1 exponencial, 2 esférico, 3 gaussiano
Radar_mm_Log_nugget<- 0.2
Radar_mm_Log_sill_and_nugget<- 0.86
Radar_mm_Log_rank <- 20000
# Pluv_mm_Log
Pluv_mm_Log_vario_model<- 2
Pluv_mm_Log_nugget<- 0.2
Pluv_mm_Log_sill_and_nugget<- 1.1
Pluv_mm_Log_rank <- 20000
# Cross-variogram
Radar_mm_Log_Pluv_mm_Log_vario_model<- 2 # spherical
Radar_mm_Log_Pluv_mm_Log_nugget<- 0.1
Radar_mm_Log_Pluv_mm_Log_sill_and_nugget<- .8
Radar_mm_Log_Pluv_mm_Log_rank <- 20000

png("Results/EstimacionEspacial/Radar_mm_Log_Pluv_mm_Log_CrossVarioModel.png", bg = "white")
ModelVariogram(XCoord, YCoord, 
               Radar_mm_Log, Pluv_mm_Log, 
               N_lags, lag_value, 0, 90, 
               Radar_mm_Log_Pluv_mm_Log_vario_model, 
               Radar_mm_Log_sill_and_nugget, Pluv_mm_Log_sill_and_nugget, Radar_mm_Log_Pluv_mm_Log_sill_and_nugget, 
               Radar_mm_Log_nugget, Pluv_mm_Log_nugget, Radar_mm_Log_Pluv_mm_Log_nugget, Radar_mm_Log_Pluv_mm_Log_rank, 
               'Radar_mm_Log Variogram', 'Pluv_mm_Log Variogram', 'Radar_mm_Log-Pluv_mm_Log CrossVariogram')
dev.off()

# Verificar que se cumple que el modelo es positivo definido!!!!, revisar diapositiva num 13
NugetMatrix <- matrix(c(Pluv_mm_Log_nugget,
                        Radar_mm_Log_Pluv_mm_Log_nugget,
                        Radar_mm_Log_Pluv_mm_Log_nugget,
                        Radar_mm_Log_nugget), ncol = 2)
det(NugetMatrix) #entre más alejado de cero, es mejor
SemiVariMatrix1 <- c(Pluv_mm_Log_sill_and_nugget,
                     Radar_mm_Log_Pluv_mm_Log_sill_and_nugget,
                     Radar_mm_Log_Pluv_mm_Log_sill_and_nugget,
                     Radar_mm_Log_sill_and_nugget)
SillMatrix <- matrix(SemiVariMatrix1, ncol = 2)
Sill_NuggetMatrix <- SillMatrix - NugetMatrix
which(Sill_NuggetMatrix < 0) # which(Model2 < 0, arr.ind = T)
det(Sill_NuggetMatrix) #determinante siempre mayor que zero

# pdfResults <- matrix(c(.2, .17, .17, .2), ncol = 2, byrow = TRUE)
# det(pdfResults)

png("Results/EstimacionEspacial/Radar_mm_Log_Pluv_mm_Log_CrossValidation.png", bg = "white")
Pluv_mm_Log_CrossValid2<- CrossValidation2(XCoord, YCoord, 
                                        Pluv_mm_Log, Radar_mm_Log, 
                                        N_lags, lag_value, Radar_mm_Log_Pluv_mm_Log_vario_model, 
                                        Pluv_mm_Log_sill_and_nugget, Radar_mm_Log_sill_and_nugget, Radar_mm_Log_Pluv_mm_Log_sill_and_nugget, 
                                        Pluv_mm_Log_nugget, Radar_mm_Log_nugget, Radar_mm_Log_Pluv_mm_Log_nugget, Radar_mm_Log_Pluv_mm_Log_rank)
dev.off()
head(Pluv_mm_Log_CrossValid2)

# Basic Statistics in a single table
Pluv_mm_Log_CrossValid2_Sta <- Val_Estadisticos(Pluv_mm_Log_CrossValid2[,c(3,4,5)])

# Histogram and Boxplot 1
png("Results/EstimacionEspacial/Pluv_mm_Log-Pluv_mm_Log+_HistBoxPlot1CoKrig.png", bg = "white")
HistBoxplot(x=Pluv_mm_Log_CrossValid2[,5], mean = Pluv_mm_Log_CrossValid2_Sta[5,3], median = Pluv_mm_Log_CrossValid2_Sta[4,3], main ="", 
            xlab = "Pluv_mm_Log-Pluv_mm_Log* (ppm)", ylab = "Frecuencia Absoluta (conteo)", AbsFreq = TRUE, PercentFreq = FALSE )
dev.off()

# CoKrigingCrossValFit <- FitDistribution(data = Pluv_mm_Log_CrossValid2[,5], DISTR="norm", BREAKS = "Sturges", col = "darkgray", DistName = "Normal")

# Spatial Distribution of differences (Z-Z*)
png("Results/AnalisisEstructural/Pluv_mm_Log_CoKrigingCrossValid_Spatial_Distr.png", bg = "white")
DEspacial(Pluv_mm_Log_CrossValid2[,1], Pluv_mm_Log_CrossValid2[,2], Pluv_mm_Log_CrossValid2[,5],
          'X (Km)', 'Y (Km)', 'Pluv_mm_Log-Pluv_mm_Log* (ppm)', 'Pluv_mm_Log - Pluv_mm_Log*')
dev.off()

#--------------------------------------------------------------#
#                 Bivariate Spatial Estimation                 #
#--------------------------------------------------------------#
png("Results/EstimacionEspacial/Radar_mm_Log_CoKrig.png", bg = "white")
Radar_mm_Log_CoKrig <- CoKrigingOrd(XCoord, YCoord, 
                              Pluv_mm_Log, Radar_mm_Log,
                              Radar_mm_Log_Pluv_mm_Log_vario_model, 
                              Pluv_mm_Log_sill_and_nugget, Radar_mm_Log_sill_and_nugget, Radar_mm_Log_Pluv_mm_Log_sill_and_nugget, 
                              Pluv_mm_Log_nugget, Radar_mm_Log_nugget,
                              Radar_mm_Log_Pluv_mm_Log_nugget, Radar_mm_Log_Pluv_mm_Log_rank, 
                              minPar = minPoints, maxPar = maxPoints,
                              Xmin = XCoord_Stat[2,2], Xmax = XCoord_Stat[7,2], 
                              Ymin = YCoord_Stat[2,2], Ymax = YCoord_Stat[7,2],
                              TX=DistMin, TY=DistMin, InvT=1, NameX="X (Km)", 
                              NameY="Y (Km)", Titulo1="CoKriging Pluv_mm_Log (ppm)", 
                              Titulo2="SD Pluv_mm_Log (ppm)")
dev.off()