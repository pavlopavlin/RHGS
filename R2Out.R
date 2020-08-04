######################
#R scripts: R2Out
#
#Function: Read heads from observation wells and write all heads in 1 file
######################


#change to your working direction
setwd("C:/Projects/supporting information_new/SteadyState")

head1<-read.table("Calibratedo.observation_well_flow.well01.dat",skip=2)[,1]
head2<-read.table("Calibratedo.observation_well_flow.well02.dat",skip=2)[,1]
head3<-read.table("Calibratedo.observation_well_flow.well03.dat",skip=2)[,1]
head4<-read.table("Calibratedo.observation_well_flow.well04.dat",skip=2)[,1]
head5<-read.table("Calibratedo.observation_well_flow.well05.dat",skip=2)[,1]
head6<-read.table("Calibratedo.observation_well_flow.well06.dat",skip=2)[,1]
head7<-read.table("Calibratedo.observation_well_flow.well07.dat",skip=2)[,1]
head8<-read.table("Calibratedo.observation_well_flow.well08.dat",skip=2)[,1]
head9<-read.table("Calibratedo.observation_well_flow.well09.dat",skip=2)[,1]
head10<-read.table("Calibratedo.observation_well_flow.well10.dat",skip=2)[,1]
head11<-read.table("Calibratedo.observation_well_flow.well11.dat",skip=2)[,1]
head12<-read.table("Calibratedo.observation_well_flow.well12.dat",skip=2)[,1]


y<-rbind(head1,head2,head3,head4,head5,head6,head7,head8,head9,head10,head11,head12)


head<-matrix(NaN,12,1)
for (i in 1:12)
{head[i]<-paste(y[i])}


write.table(head,"heads.out",col.names=FALSE,row.names=FALSE,quote=FALSE)