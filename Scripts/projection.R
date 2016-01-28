projectinput <- function(DEM, prj_string){
  #projects input DEM
  #args: DEM (raster), projection string(text)
  #output: projected DEM
	if (is.na(projection(DEM))) {
		projection(DEM) <- prj_string
		return(DEM)
	}
}