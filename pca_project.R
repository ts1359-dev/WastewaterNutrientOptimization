rm(list = ls())
#TASK1
library(readxl)
library(FactoMineR)
library(factoextra)
library(powerplus)
DATA <- as.data.frame(read_excel("C:/Users/vt135/Downloads/Data analysis for env engr/Class project/Data analysis project data.xlsx", sheet="pca_analys"))
library(robustHD)
ZDATA = standardize(DATA,centerFun = mean,scaleFun = sd)


CONSTRAINTS = as.data.frame(read_excel("C:/Users/vt135/Downloads/Data analysis for env engr/Class project/Data analysis project data.xlsx", sheet="pca_analys(2)"))

CDATA = standardize(CONSTRAINTS,centerFun = mean,
                    scaleFun = sd)
n = nrow(CDATA) 
COR_C = (1/(n-1))*(t(CDATA)%*%as.matrix(CDATA))
INV = Matpow(COR_C,-1) 
VIF = diag(INV)
VIF
#remove c14 and repeat
CDATA2 = as.matrix(cbind(CDATA[,1:5],CDATA[,7:8],CDATA[,11:12],CDATA[,15],CDATA[,19:20],CDATA[,22:24],
                         CDATA[,26:31],CDATA[,33:34]))
cnames2 = c('EQT_Cap','DAF_COD_Removal','DAF_EffPO4','DAF_EffNH3_N', 'DAF_EffNO3_N','ATone_DO','ATone_pH', 'ATtwo_DO','ATtwo_pH','ATthree_DO','ATfour_DO','ATfour_pH','ATfour_SVI',
            'ATfive_DO','ATfive_pH','ATfive_SVI','ATsix_DO','ATsix_pH','ATsix_Temp','ATsix_SVI','SRT','RASNO3_N','RASNH3_N')
colnames(CDATA2)<- cnames2
COR_C = (1/(n-1))*(t(CDATA2)%*%CDATA2)
INV = Matpow(COR_C,-1)  
VIF = diag(INV)
VIF

u = ncol(CDATA2)+1
B = matrix(NA,u,ncol(ZDATA))

MASTER = as.data.frame(cbind(ZDATA,CDATA2))
for (i in 1:ncol(ZDATA)){
  FIT = lm(MASTER[,i] ~ MASTER$EQT_Cap+MASTER$DAF_COD_Removal+MASTER$DAF_EffPO4+MASTER$DAF_EffNH3_N+MASTER$DAF_EffNO3_N+MASTER$ATone_DO+MASTER$ATone_pH+MASTER$ATtwo_DO+
             MASTER$ATtwo_pH+MASTER$ATthree_DO+
             MASTER$ATfour_DO+MASTER$ATfour_pH+MASTER$ATfour_SVI+MASTER$ATfive_DO+
             MASTER$ATfive_pH+ MASTER$ATfive_SVI+ MASTER$ATsix_DO+ MASTER$ATsix_pH+ MASTER$ATsix_Temp+MASTER$ATsix_SVI+MASTER$SRT+MASTER$RASNO3_N+MASTER$RASNH3_N)
  B[,i] = FIT$coefficients
}
PREDICTED = matrix(NA,n,ncol(ZDATA))
for (i in 1:ncol(ZDATA)){
  PREDICTED[,i] = B[1,i]+B[2,i]*CDATA2[,1]+B[3,i]*CDATA2[,2]+B[4,i]*CDATA2[,3]+B[5,i]*CDATA2[,4]+B[6,i]*CDATA2[,5]+B[7,i]*CDATA2[,6]+
    B[8,i]*CDATA2[,7]+B[9,i]*CDATA2[,8]+B[10,i]*CDATA2[,9]+B[11,i]*CDATA2[,10]+B[12,i]*CDATA2[,11]+B[13,i]*CDATA2[,12]+B[14,i]*CDATA2[,13]+
    B[15,i]*CDATA2[,14]+B[16,i]*CDATA2[,15]+B[17,i]*CDATA2[,16]+B[18,i]*CDATA2[,17]+B[19,i]*CDATA2[,18]+B[20,i]*CDATA2[,19]+B[21,i]*CDATA2[,20]
  +B[22,i]*CDATA2[,21]+B[23,i]*CDATA2[,22]+B[24,i]*CDATA2[,23]
}
print(PREDICTED[1:5, 1:5])
PREDICTED = as.data.frame(PREDICTED)
CDATA_PRED = standardize(PREDICTED)
colnames(CDATA_PRED)<-c('NH3-N','NO3-N')
PCfit = PCA(CDATA_PRED,graph = FALSE,ncp = 2)
EIGVEC = PCfit$svd$V
EIGVEC
SCORES = as.matrix(CDATA_PRED)%*%as.matrix(EIGVEC) 
SCORES

v = nrow(CDATA_PRED)
w = as.matrix(CDATA_PRED)
A = (1/(v-1))*crossprod(w)
e <- eigen(A)
lam_ord = e$values
percV = lam_ord/sum(lam_ord)
percV

L = PCfit$eig[,1]
Cmat = matrix(NA,ncol(CDATA2),2)

for (i in 1:(u-1)){
  c1 = sqrt(L[1]/sum(L)) #VE Correction for PC1
  c2 = sqrt(L[2]/sum(L)) #VE Correction for PC2
  Cmat[i,1:2] = c(cor(CDATA2[,i],SCORES[,1])*c1, 
                  cor(CDATA2[,i],SCORES[,2])*c2)
}
cnames = c('EQT_Cap','DAF_COD_Removal','DAF_EffPO4','DAF_EffNH3_N', 'DAF_EffNO3_N','ATone_DO','ATone_pH', 'ATtwo_DO','ATtwo_pH','ATthree_DO','ATfour_DO','ATfour_pH','ATfour_SVI',
           'ATfive_DO','ATfive_pH','ATfive_SVI','ATsix_DO','ATsix_pH','ATsix_Temp','ATsix_SVI','SRT','RASNO3_N','RASNH3_N')
Cmat = as.data.frame(Cmat)
rownames(Cmat)<- cnames
p2=fviz_pca_biplot(PCfit,repel = TRUE,label = "var",
                col.var = "black")
fviz_add(p2,as.data.frame(Cmat*4.5),geom = "arrow",
         repel = TRUE)

