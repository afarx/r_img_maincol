library('imager')
library('colorspace')


# Define Funciton
imtobar <- function(im,n){
  # room(resize)
  zoomim <- resize(im,round(width(im)/10),round(height(im)/10))
  # blur
  zoomim<- isoblur(zoomim,2)
  
  # convert to pixels*3 (r,g,b)
  u <- RGB(
    as.data.frame(R(zoomim))$value,
    as.data.frame(G(zoomim))$value,
    as.data.frame(B(zoomim))$value
  )
  
  #convert to LAB
  ulab <- as(u,"LAB")
  uu <- as.matrix(ulab@coords)
  #k-means
  km <- kmeans(uu,n)
  km$size
  
  # 得到聚类中心
  mainc <- km$centers
  
  col <- hex(LAB(mainc))
  
  # draw
  par(mfrow=c(1,2))
  plot(im)
  barplot(sort(as.numeric(km$size)),col = col,yaxt="n",xaxt="n")
  
}

# load and plot
png(file="e1b.png",width=600,height=300)
im <- load.image('e1.png')
imtobar(im,5)
dev.off()





