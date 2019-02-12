#' @title This function plots several graphics
#' @inheritParams fields::as.image
#' @param "x" Locations of image values
#' @param asp the y/x aspect ratio, see ?plot.window.
#' @param legend.strip indicates if a legend strip (color scale bar) will be plotted or not
#' @param AutoOrientation" Determines the best orientation (vertical or horizontal) for the legend strip (color scale bar). "horizontal" of "image.plot" from package "fields"  is not used when "AutoOrientation = TRUE"
#' @param ... further arguments passed to "image.plot" from package "fields" (if legend.strip=TRUE) or to "image.default" from package "graphics". See ?image.plot to see the arguments
#' @param PointsCoord are the coordinates (as a 2-column data.frame) of points to be plotted over the grid. If "PointsCoord" is provided, it means that point must be plotted
#' @param PointsPar 3-length vector specifying the points plotting parameters: pch, col, cex.
#' @param grid A list with components "x" and "y" of equally spaced values describing the centers of the grid points. If "grid" is provided, it means that grid must be plotted
#' @param colGrid is the color of the grid. Set it to "transparent" if not it is not desired to plot the grid
#' @param win An object of class "owin" (Window) (see "as.owin" from spatstat package), maybe created with the function "owin"  from spatstat package. This is the window (boundary) of data
#' @param HighlightCells this is not needed. It is only because image.plot needs a Z-value."points.default()" also can be useful to highlight grid cells. This section can be useful to highlight outliers for example.
#' @param GridCellsHiCells a two column vector giving the midcells position of the grid to highlight.
#' @param colHiCells grid cells highlighting color . Default to "black"
#' @param Text is a vector with characters to be plotted in the center of the (maybe some) grid cells. "Text" is 1-column vector. If "Text=TRUE" the values of Z are plotted in the centers of the grid cells
#' @param GridCellsText" a two column vector giving the midcells position of the grid where "Text" is defined. The number of rows in "Text" must be equal to the number of rows in "GridCellsText". "GridCellsText" can or cannot be the same as the grid. Maybe only some cells are required to have text. Change the name of this argument to "TextCoordinates"
#' @param TextPar Are the parameters to plot \code{Text}. 2-length list specifying following plotting parameters (?par): col, cex. If cex="auto", the "Text" with the maximum number of characters are fitted (horizontally) to the whole grid cell
#' @export

# Arguments
# "Z" Values of image
# "x" Locations of image values
# "asp" the y/x aspect ratio, see ?plot.window.
# "legend.strip" indicates if a legend strip (color scale bar) will be plotted or not
# "AutoOrientation" Determines the best orientation (vertical or horizontal) for the legend strip (color scale bar). "horizontal" of "image.plot" from package "fields"  is not used when "AutoOrientation = TRUE"
# "horizontal" logical for check the aspect ratio of the plot
# "PointsCoord" are the coordinates (as a 2-column data.frame) of points to be plotted over the grid. If "PointsCoord" is provided, it means that point must be plotted
# "PointsPar" 3-length vector specifying the points plotting parameters: pch, col, cex.
# "nrow" Number of rows in image matrix ( x-axis direction)
# "ncol" Number of columns in image matrix ( y-axis direction)
# "weights" If two or more values fall into the same pixel a weighted average is used to represent the pixel value. Default is equal weights.
# "na.rm" If true NA?s are removed from the Z vector.
# "nx" Same as nrow
# "ny" Same as ncol
# "Grid" A list with components "x" and "y" of equally spaced values describing the centers of the grid points. If "Grid" is provided, it means that grid must be plotted
# "colGrid" is the color of the grid. Set it to "transparent" if not it is not desired to plot the grid
# "win" An object of class "owin" (Window) (see "as.owin" from spatstat package), maybe created with the function "owin"  from spatstat package. This is the window (boundary) of data
# "boundary.grid" logical. is the boundary of the grid to be highlighted?
# "HighlightCells" this is not needed. It is only because image.plot needs a Z-value."points.default()" also can be useful to highlight grid cells. This section can be useful to highlight outliers for example.
# "GridCellsHiCells" a two column vector giving the midcells position of the grid to highlight.
# "colHiCells" Grid cells highlighting color . Default to "black"
# "Text" is a vector with characters to be plotted in the center of the (maybe some) grid cells. "Text" is 1-column vector. If "Text=TRUE" the values of Z are plotted in the centers of the grid cells
# "GridCellsText" a two column vector giving the midcells position of the grid where "Text" is defined. The number of rows in "Text" must be equal to the number of rows in "GridCellsText". "GridCellsText" can or cannot be the same as the grid. Maybe only some cells are required to have text. Change the name of this argument to "TextCoordinates"
# "TextPar" Are the parameters to plot "Text". 2-length list specifying following plotting parameters (?par): col, cex. If cex="auto", the "Text" with the maximum number of characters are fitted (horizontally) to the whole grid cell
# "..." further arguments passed to "image.plot" from package "fields" (if legend.strip=TRUE) or to "image.default" from package "graphics". See ?image.plot to see the arguments


# Value
# a plot
# List of 5:
# 1)  "GridBreaks" the breaks for the grid
# 2)  "xMids" the "x" coordinates of the midpoints of each rectangular cell
# 3)  "yMids" the "y" coordinates of the midpoints of each rectangular cell
# 4)  "Coordinates" the given coordinates of points
# 5)  "boundary" the boundary. Object of class owin

# DETAILS 

PlotGridCells <-function(Z,x,...,
                         
                         # fields::as.image arguments:
                         grid=NULL,
                         weights=rep(1, length(Z)),
                         na.rm=FALSE, nx=64,ny=64,
                         boundary.grid=FALSE,
                         nrow=NULL, ncol=NULL,
                         
                         asp=1,legend.strip = TRUE,
                         AutoOrientation = TRUE, horizontal = FALSE,
                         PointsCoord=NULL,
                         PointsPar=list(pch=20,col="black", cex=0.1),
                         colGrid = "transparent", win = NULL,
                         HighlightCells = 1,
                         GridCellsHiCells = NULL, colHiCells="black",
                         Text = NULL, GridCellsText = NULL,
                         TextPar = list(col="black", cex=0.6))
{
  library(fields)
  library (spatstat)
  
  if (!is.null(grid)) {
    xwidth <- grid$x[2]-grid$x[1]
    ywidth <- grid$y[2]-grid$y[1]
    xgridShift <- grid$x + 0.5 * xwidth
    ygridShift <- grid$y + 0.5 * ywidth
    xTess <- c(xgridShift[1] - xwidth, xgridShift)
    yTess <- c(ygridShift[1] - ywidth, ygridShift)
    GridTess<-tess(xgrid = xTess, ygrid = yTess)
  } else {
    GridTess<-NULL
  }
  
  Data <- fields::as.image(Z=Z,x=x,grid=grid,
                         boundary.grid=boundary.grid,
                         nrow=nrow, ncol=ncol,weights=weights,
                         na.rm=na.rm, nx=nx,ny=ny)
  
#   fields::image.plot(Data)
#   fields::discretize.image
  
if (legend.strip) {
    # Automatic detection of the best orientation (vertical or horizontal) of the legend strip (color scale bar)
    Shortest<-"x" # let?s say that "x" is the shortest direction and check it below
    if (!is.null(grid)) {
      xr<-range(xTess)
      yr<-range(yTess)
    } else {
      xr<-range(x[,1])
      yr<-range(x[,2])
    }
    Xrange<-xr[2]-xr[1]
    Yrange<-yr[2]-yr[1]
    if (Xrange >= Yrange) Shortest<-"y" # this "if" gets the direction (x or y) of the shortest range
    if (Shortest=="y" && AutoOrientation) horizontal= TRUE
    
    temp <- list(...) # "temp" stores the arguments passed as "..."
    
    fields::image.plot(Data,asp=asp, horizontal = horizontal,...)
    
    if (!is.null(HighlightCells) && !is.null(GridCellsHiCells)){
      imaest<-as.image(Z = HighlightCells,x = GridCellsHiCells, grid = GridIm)
      image.plot(imaest, col = colHiCells, axes=FALSE, horizontal = PalettePos , add = T, nlevel = nlevel)
      image.plot(...,asp=asp, horizontal = horizontal, legend.only = TRUE)
    }
    if (!is.null(GridTess)) plot.tess(GridTess,add=TRUE, col=colGrid, main="")
    if (!is.null(PointsCoord)) points.default(x=PointsCoord[,1],y=PointsCoord[,2], pch=PointsPar$pch, col = PointsPar$col,cex = PointsPar$cex, main="")
    if (!is.null(win)) plot.owin(win, add=T, main="")
    
    if (is.logical(Text)) {
      Text<-Z
      GridCellsText<-x
    }
    
    if (!is.null(Text) && !is.null(GridCellsText)) {
      # Automatic size checking for plotting text completely inside a cell
      # char.size <- c(strwidth("fas"), strheight("fas"))
      # char.size <- par()$cxy # See "pin" from ?par
      # next, "char.size" is the percentage of the characater size relative to the device dimension. compare with "plt" from ?par
      # char.size <- par()$cin/par()$din # "cin" R.O.; character size (width, height) in inches. "din" R.O.; the device dimensions, (width,height), in inches
      # n<-max(nchar(Text)) # "n" is the maximum number of characters in "Text"
      # if(TextPar$cex=="auto") TextPar$cex<-AutoCex # An AUTOmatic size (completely inside a grid cell) of the text
      text.default(GridCellsText[,1],GridCellsText[,2],labels=Text,col=TextPar$col, cex=TextPar$cex) # shows the labels of the cells
    }
    
  } # else { image.default(...) # 
  invisible(Data)
}
############### EXAMPLES
# set.seed(123)
# x<-  runif(100)
# y<-2*runif(100)
# plot.default(cbind(x,y), asp=1, pch=20, cex=0.8)
# RES <- SquareGrid2D(Coordinates=cbind(x,y),n=2,
#                      pch=20, cex=0.8,
#                      colGrid="lightblue",
#                      main="malla de 5 x tanto")
# str(RES)
# Grid<-list(x=RES$xMids,y=RES$yMids)
# Positions<-as.data.frame(cbind(c(0.25, 0.75, 0.25), 
#                                c(0.25, 1.25, 1.75)))
# 
# # Next, "grid" points are assumed to be the grid midpoints
# Res <- PlotGridCells(Z=c(3,5,7),x=Positions, grid=Grid)
# str(Res)
# Res <- PlotGridCells(Z=c(3,7,6),x=Positions, grid=Grid, colGrid ="transparent")
# Res <- PlotGridCells(Z=c(3,7,6),x=Positions, grid=Grid, PointsCoord=cbind(x,y))

# horizontal Plotting
# set.seed(123)
# y<-  rnorm(100,2,2)
# x<-2*rnorm(100,0,3)
# plot.default(cbind(x,y), asp=1, pch=20, cex=0.8)
# RES<-SquareGrid2D(Coordinates=cbind(x,y),n=5,pch=20, cex=0.8, colGrid="lightblue", main="malla de 11 x 5")
# str(RES)
# Grid<-list(x=RES$GridBreaks$xbreaks,y=RES$GridBreaks$ybreaks)
# Positions<-as.data.frame(cbind(c(1.1,2,3),c(-1,2,5)))
#Positions[,1]
# Res <- PlotGridCells(Z=c(3,7,6),x=Positions, grid=Grid, boundary.grid=TRUE, PointsCoord=cbind(x,y))

# Not working:
# x<- 1:20; y<- 11:15; z<- outer( x,y,"+")
# str(z)
# PlotGridCells2(x,y,z,PointsCoord=cbind(1:3,11:13),main="prueasr lsajf \n ?lae")
# image.default(x,y,z, asp=1)
# points.default(cbind(c(5,8,15), c(10,13,12)), pch=c(20,21,22))

# x<- 10:15; y<- 1:20; z<- outer( x,y,"+")
# PlotGridCells2(x,y,z,main="prueasr lsajf \n lae")


######### IMPROVEMTS
# Accepts data.frame, vectors and list
# Accepts the same arguments as a "imege.default" function
# See also the function "levelplot" from package "lattice"
# Add text to the cells centers. Estimate the maximum "cex" for the text to fit completely inside the cell
# legend.strip = FALSE
# if it is desired to highlight some cells, then "image.default(, add=T)" or "points.default()"  can be useful. Add it to this function.