######################
#R scripts: HGS2Cord
#
#Function: Read coordinates from HGS and can be used in ppk2facg
######################

.libPaths("C:/software/Rpackages")
library(scatterplot3d)
library(lattice)
library(rgl)

#change to your working direction/instruction should be written in one single #line
setwd("C:\\Projects\\HGS_PilotPoints_Tutorial\\SteadyState")

#read data from HGS output file mmmmmm----mesh.dat
nodes_cor<-read.table("nodes_cord.txt")   # xy and z coordiantes of the nodes
length(nodes_cor[,1])
colnames(nodes_cor)<-c("Node","X","Y","Z")
elements_ind<-read.table("element_ind.txt") # Number of nodes are used to describe the rectangle mesh element
length(elements_ind[,1])
colnames(elements_ind)<-c("Element","Node1","Node2","Node3","Node4","Node5","Node6")

#Number of nodes are used to describe the prism mesh element
nodes_prism<-elements_ind[1,2:7]
for(i in 2:length(elements_ind[,1]))
{
  nodes_prism[i,]<-elements_ind[i,2:7]  
}

# generate matrix without values for the element center position in x,y and z direction
xmean<-matrix(NaN,length(elements_ind[,1]),1)
ymean<-matrix(NaN,length(elements_ind[,1]),1)
zmean<-matrix(NaN,length(elements_ind[,1]),1)

# write coordinates for each node of the rectangle and calculate the center position within the rectangle element
center<-rbind(nodes_cor[nodes_rectangle[1,1],2:4],nodes_cor[nodes_rectangle[1,2],2:4],nodes_cor[nodes_rectangle[1,3],2:4],nodes_cor[nodes_rectangle[1,4],2:4],nodes_cor[nodes_rectangle[1,5],2:4],nodes_cor[nodes_rectangle[1,6],2:4])
xmean[1]<-mean(center[,1])
ymean[1]<-mean(center[,2])
zmean[1]<-mean(center[,3])

for (i in 1:length(elements_ind[,1]))
{
  center<-rbind(nodes_cor[nodes_rectangle[i,1],2:4],nodes_cor[nodes_rectangle[i,2],2:4],nodes_cor[nodes_rectangle[i,3],2:4],nodes_cor[nodes_rectangle[i,4],2:4],nodes_cor[nodes_rectangle[i,5],2:4],nodes_cor[nodes_rectangle[i,6],2:4])
  xmean[i]<-mean(center[,1])
  ymean[i]<-mean(center[,2])
  zmean[i]<-mean(center[,3])
}

# combine all values to a single matrix
element_center<-cbind(xmean,ymean,zmean)
XYandZone<-cbind(xmean,ymean,rep(1,length(ymean)))

#write new output file for ppk2facg
write.table(element_center,"element_center.txt",row.names=FALSE,col.names=FALSE)
write.table(XYandZone,"output.xyz",row.names=FALSE,col.names=FALSE)


