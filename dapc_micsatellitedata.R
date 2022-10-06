# To do DAPC. First make sure u installed dartR properly
#make sure wd is all the way in the folder needed
library(adegenet)
library(devtools)
library(StAMPP)
library(readr)

install.packages("dartR")

#### if dartR has issues ###
install.packages("data.table", dependencies=TRUE) #coz dartR could not install this
library(data.table)
install.packages("SNPRelate") #coz dartR could not install this
library(SNPRelate)

## convert micsat data to dataframe ###
install.packages("poppr")
library(poppr)
eucpop=read.genalex("5regions_euc_genalex.csv")  ## .csv must be in genalex format

## convert to genind for dapc ##
well=genclone2genind(eucpop)

#Start DAPC

gp<-find.clusters(wellplay,max.n.clust = 30)
#it will ask you to choose the number of PCs to retain, just choose a value higher than your max
#then it will ask you to choose the number of clusters, choose the number with the lowest BIC score

#chose 164 and 7 clusters


names(gp)

head(gp$Kstat,10) #gives the BIC score for different 'K's..here we ask for first 10 Ks

gp$stat

head(gp$grp,10)

grp$size

table(pop(wellplay), gp$grp)

table.value(table(pop(wellplay), gp$grp), col.lab=paste("inf", 1:3), row.lab=paste("ori", 1:5))
#shows you which of your populations match with the clusters (good result if same spp group under same cluster [or if all diff species should have 1 per cluster])
#inf 1:5 --- 5 is number of clusters I chose
#ori 1:16 --- 16 is number of populations I have in datatset



# Do DAPC validation first so you can choose the correct no. of PCs

dapc1<-dapc(wellplay,gp$grp)
60
# Choose the number PCs to retain (>=1): -> choose what your validation says
#Choose the number discriminant functions to retain (>=1): --> for clusters less than 10 use all eigen values that are displayed
scatter(dapc1)

myCol<-c("purple","orange","yellow","red","blue","pink")
scatter(dapc1, posi.da="bottomright", bg="gray40", pch=17:22, col=myCol)

myCol <- c("darkblue","purple","green","orange","red","blue","pink")
scatter(dapc1, posi.da="bottomright", bg="grey",
        pch=17:22, cstar=0, col=myCol)


scatter(dapc1, scree.da=FALSE, bg="white", pch=20, cell=0, cstar=0, col=myCol, solid=.4,
        cex=3,clab=0, leg=TRUE, txt.leg=paste("Cluster",1:7))


leg.txt <- c("1","a+mo","pe+mo","c","pop","bos","qu+mi")


# Validation for DAPC - HOW MANY PCs to retain? #read adegenet tutorial pg.37 for more details
x <- wellplay
mat <- as.matrix(tab(x, NA.method="mean"))

grp <- pop(x)
xval <- xvalDapc(mat, grp, n.pca.max = 268, training.set = 0.9,
                 result = "groupMean", center = TRUE, scale = FALSE,
                 n.pca = NULL, n.rep = 30, xval.plot = TRUE) #n.pca.max changes based on max no. of individuals
xval[2:6] #gives details of PC numbers

#Choose PC that has highest success and lowest MSE; Lowest MSE is the most important criteria
#60 was correct here

#to find which ind is grouping in which cluster
assignplot(dapc1, subset=1:50) #shows first 50 ind
assignplot(dapc1, subset=51:101)
assignplot(dapc1, subset=102:152)
assignplot(dapc1, subset=153:203)
assignplot(dapc1, subset=204:268)




#to prepare input for fastSTRUCTURE

gl2faststructure(gli, outfile = "strcu.str", probar = TRUE)



gl2faststructure(gli, outfile= "file.fs", probar = FALSE)



write.csv(gli, file = "ttest.csv")
