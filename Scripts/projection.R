projectinput <- function(DEM, prj_string){
	if (is.na(projection(DEM))) {
		projection(DEM) <- prj_string
		return(DEM)
	}
}