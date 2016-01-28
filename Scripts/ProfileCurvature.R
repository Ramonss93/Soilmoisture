DEMderiv<-function(data,attr,method){
	#Title: Function for computing terrain attributes from a raster
	#args: data(rasterDEM0, attr(string with prof.curv), method=evans)
  #Author: Fabio Veronesi
	#License: Creative Commons - Attribution-NonCommercial (CC BY-NC) - http://creativecommons.org/
	
	cellValue=res(data)[1]
	z1<-neighb[1]
	z2<-neighb[2]
	z3<-neighb[3]
	z4<-neighb[4]
	z6<-neighb[6]
	z7<-neighb[7]
	z8<-neighb[8]
	z9<-neighb[9]
	z5<-neighb[5]
	r=(z1+z3+z4+z6+z7+z9-(2*(z2+z5+z8)))/(3*(cellValue^2))
	t=(z1+z2+z3+z7+z8+z9-(2*(z4+z5+z6)))/(3*(cellValue^2))
	s=(z3+z7-z1-z9)/(4*(cellValue^2))
	p=(z3+z6+z9-z1-z4-z7)/(6*cellValue)
	q=(z1+z2+z3-z7-z8-z9)/(6*cellValue)
	if(paste(attr)=="slope"){result= atan(sqrt(p^2+q^2)) }                 
	else{
		if(paste(attr)=="aspect"){result= 180-atan2(q,p)+90*(p/abs(p)) }                         
		else{
			if(paste(attr)=="plan.curvature"){result= -(q^2*r-2*p*q*s+p^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2))}      
			else{
				if(paste(attr)=="prof.curvature"){result= -(p^2*r+2*p*q*s+q^2*t)/((p^2+q^2)*sqrt(1+p^2+q^2)^3)}  
			}}}
	c(result)
}

cellValue=res(data)[1]

