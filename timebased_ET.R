###########################################################################################
# Time based model with ET applied for irrigation			     	  
# Kati W. Migliaccio (PhD), klwhite@ufl.edu; Tanh Nguyen (PhD candidate),ntntanh@gmail.com 
# Tropical Research and Education Center, University of Florida, U.S.A
# 18905 SW 280 St, Homestead, FL 33031, Phone: 305-246-7000, Fax: 305-246-7003
###########################################################################################

# Read excel file
path<-"C:/Users/ntntanh/Desktop/Migliaccio"
setwd(path)
data<-read.csv("time-base-trial.csv", header = TRUE,sep=",")

# Convert column to variable
year		= data$Year
month		= data$Month #month
hr 		= data$Hour
Rhr		= data$Rhr #cm
Ihr		= data$Ihr
soil		= data$Soil # soil type
ET0		= data$ET0 # crop evapotranspiration
Ihrschedule	= data$Ihrschedule

# Set up initial valuetime 
ihr		= 2 # started irrigation hour 
lhr		= 200#length (hr)# last irrigation hour 
# Set variable
k		= rep(NA,lhr)
kc		= rep(NA,lhr)
ET		= rep(NA,lhr)
WB		= rep(NA,lhr)
PERC		= rep(NA,lhr)
Q		= rep(NA,lhr)
InF		= rep(NA,lhr)
Fc		= rep(NA,lhr)
SWC		= rep(NA,lhr)
delta		= rep(NA,lhr)
f		= rep(NA,lhr)
F.vector	= rep(NA,lhr)
Ihret		= rep(NA,lhr)
Re		= rep(NA,lhr)
Ick1		= rep(NA,lhr)
Ick2		= rep(NA,lhr)
AWR		= rep(NA,lhr)
AWRstep1	= rep(NA,lhr)
AWRstep2	= rep(NA,lhr)
AWRstep3	= rep(NA,lhr)
Loss		= rep(NA,lhr)
Perloss	= rep(NA,lhr)#percentage loss

# Initial values
# Set crop coefficient 
# soil type = 'sandy loam'
K 		= 1.09 # Hydraulic conductivity 
Psi 		= 11.01 # pressure head for weting front ?
thetae 	= 0.41 #
FC 		= 0.16 # field capacity (cm/cm)
RD 		= 7.62 # root depth (cm)
WP		=0.06
MAD		=0.5
A = 1
k[1] 		= 0.71
k[2] 		= 0.79
k[3] 		= 0.78
k[4] 		= 0.86
k[5] 		= 0.99
k[6] 		= 0.86
k[7] 		= 0.86
k[8] 		= 0.90
k[9] 		= 0.87
k[10] 	= 0.86
k[11] 	= 0.84
k[12] 	= 0.71
SWC[1]	= 0.75*FC*RD #SWC0
ET[1] 	=k[month[1]]*ET0[1] 
AWR[1]	=0
AWRstep1[1]	=0
AWRstep2[1]	=0
AWRstep3[1]	=0
Ick1[1]	=0
Ick2[1]	=0
WB[1] 	= 0
Re[1]		= 0
Q[1] 		= 0
InF[1] 	= 0
PERC[1] 	= 0
Ihret[1]	= 0
for (i in ihr: lhr) 
{	
	ET[i]<-ET0[i]*k[month[i]]
	
	#Calculate Re
	if (Rhr[i]> (RD*FC)-SWC[i-1]) {
		Re[i]=RD*FC-SWC[i-1]
	} else {Re[i]= Rhr[i]
	}
	#Check if irrigation is scheduled
	if (Ihrschedule[i]==1){
		Ick1[i]=1
	} else {Ick1[i]=0
	}
	
	#Check if irrigation occurs
	if (AWR[i-1]> (FC-WP)*RD*MAD) {
		Ick2[i]=1
	} else {
		Ick2[i]=0
	}
	# Check if irrigation occurs
	if (Ick1[i]+Ick2[i]==2) {
		Ihret[i]= AWR[i-1]
	} else {
		Ihret[i]=0		
	}
	#Calcualte AWR
	if (Ick1[i]+Ick2[i]==2) {
		AWRstep1[i]=ET[i-1]-Re[i-1]
	} else {
		AWRstep1[i]=ET[i-1]-Re[i-1]+ AWR[i-1]
	}
	if (Rhr[i-1]>0&&Re[i-1]==0){
		AWRstep2[i]=0 
	} else {
		AWRstep2[i]=1 
	}

	if (AWRstep1[i]<0|| AWRstep2[i]==0){
		AWR[i]=0
	} else {
		AWR[i]=AWRstep1[i]
	}
	
	

	WB[i] = Rhr[i] + Ihret[i]
	
	
	if (WB[i]> 0){ 

		delta[i]= thetae-(SWC[i-1]/RD)
		del = delta[i]	
		foo<-function (x) 	x - Psi*del*log(1+x/(Psi*del))-1*K
		ko<-uniroot(foo, lower = 0.001, upper = 100,c(1,10))$root
		F.vector[i]=ko
		Fc[i] = F.vector [i]
		f[i]=K*(1+(Psi*delta[i]/F.vector[i]))
	
		if (WB[i]<f[i]*1){
			InF[i]=WB[i]
			Q[i]=0 
		} else {
			InF[i]=f[i]*1
			Q[i]=WB[i]-f[i]*1 
		}
	
		if (SWC[i-1]+InF[i]>FC*RD){ 
			PERC[i]=SWC[i-1]+InF[i]-(FC*RD)
		} else {PERC[i]=0
		}

	} else {

		Q[i]=0
		InF[i]=0
		PERC[i]=0 
	}

	
	# Calculate SWC
	if (PERC[i]>0) {
		SWC[i]=FC*RD
	} else if (SWC[i-1]-ET[i]+InF[i]<WP*RD*0.1) {
		SWC[i]=WP*RD*0.1
	} else if (SWC[i-1]-ET[i]+InF[i]>FC*RD)	{
		SWC[i]=FC*RD
	} else {
		SWC[i]=SWC[i-1]+InF[i]-ET[i]
	}
	# Calculate water losses
	Loss [i]= abs((Q[i]+PERC[i]-Rhr[i]))
	Perloss[i]=Loss[i]/Ihr[i]

	
}

outputET<-data.frame(hour=hr[1:lhr],Rhr=Rhr[1:lhr],Re=Re[1:lhr],Ihrschedule=Ihrschedule[1:lhr],Ick1=Ick1[1:lhr],Ick2=Ick2[1:lhr],
	SWC=SWC[1:lhr],ET=ET[1:lhr],AWRstep1=AWRstep1[1:lhr],AWRstep2=AWRstep2[1:lhr],AWR=AWR[1:lhr],
	Ihret=Ihret[1:lhr],WB=WB[1:lhr],delta=delta[1:lhr],F=Fc[1:lhr],f=f[1:lhr],Q=Q[1:lhr],
	InF=InF[1:lhr],PERC=PERC[1:lhr])

outputET
write.table(outputET,"ET.xls",append=FALSE,quote=TRUE,sep="\t",na="NA",dec=".",row.names=FALSE,col.names=TRUE)

#showET<-read.table("ET.xls",header=TRUE)
#showET