## SCRIPT USED IN:
##Basílio, M.P.; Pereira, V.; Costa, H.G.; Santos, M.; Ghosh, A. A Systematic Review of the Applications of Multi-Criteria Decision Aid Methods (1977-2022). Electronics 2022, 11, 1720. https://doi.org/10.3390/electronics11111720

library(bibliometrix) 

#Prepare and Merge SCOPUS files
#Prepare and Merge WoS files


file1<-c("C:\\WoS_29_04_2021\\savedrecs1.bib")
file2<-c("C:\\WoS_29_04_2021\\savedrecs2.bib")
file3<-c("C:\\WoS_29_04_2021\\savedrecs3.bib")
file4<-c("C:\\WoS_29_04_2021\\savedrecs4.bib")
file5<-c("C:\\WoS_29_04_2021\\savedrecs5.bib")
file6<-c("C:\\WoS_29_04_2021\\savedrecs6.bib")
file7<-c("C:\\WoS_29_04_2021\\savedrecs7.bib")
file8<-c("C:\\WoS_29_04_2021\\savedrecs8.bib")
file9<-c("C:\\WoS_29_04_2021\\savedrecs9.bib")
file10<-c("C:\\WoS_29_04_2021\\savedrecs10.bib")
file11<-c("C:\\WoS_29_04_2021\\savedrecs11.bib")
file12<-c("C:\\WoS_29_04_2021\\savedrecs12.bib")
file13<-c("C:\\WoS_29_04_2021\\savedrecs13.bib")
file14<-c("C:\\WoS_29_04_2021\\savedrecs14.bib")
file15<-c("C:\\WoS_29_04_2021\\savedrecs15.bib")
file16<-c("C:\\WoS_29_04_2021\\savedrecs16.bib")
file17<-c("C:\\WoS_29_04_2021\\savedrecs17.bib")
file18<-c("C:\\WoS_29_04_2021\\savedrecs18.bib")
file19<-c("C:\\WoS_29_04_2021\\savedrecs19.bib")
file20<-c("C:\\WoS_29_04_2021\\savedrecs20.bib")
file21<-c("C:\\WoS_29_04_2021\\savedrecs21.bib")
file22<-c("C:\\WoS_29_04_2021\\savedrecs22.bib")
file23<-c("C:\\WoS_29_04_2021\\savedrecs23.bib")
file24<-c("C:\\WoS_29_04_2021\\savedrecs24.bib")
file25<-c("C:\\WoS_29_04_2021\\savedrecs25.bib")
file26<-c("C:\\WoS_29_04_2021\\savedrecs26.bib")
file27<-c("C:\\WoS_29_04_2021\\savedrecs27.bib")
file28<-c("C:\\WoS_29_04_2021\\savedrecs28.bib")
file29<-c("C:\\WoS_29_04_2021\\savedrecs29.bib")
file30<-c("C:\\WoS_29_04_2021\\savedrecs30.bib")
file31<-c("C:\\WoS_29_04_2021\\savedrecs31.bib")
file32<-c("C:\\WoS_29_04_2021\\savedrecs32.bib")
file33<-c("C:\\Scopus_21_04_2021\\scopus1.bib")
file34<-c("C:\\Scopus_21_04_2021\\scopus2.bib")
file35<-c("C:\\Scopus_21_04_2021\\scopus3.bib")
file36<-c("C:\\Scopus_21_04_2021\\scopus4.bib")
file37<-c("C:\\Scopus_21_04_2021\\scopus5.bib")
file38<-c("C:\\Scopus_21_04_2021\\scopus6.bib")
file39<-c("C:\\Scopus_21_04_2021\\scopus7.bib")
file40<-c("C:\\Scopus_21_04_2021\\scopus8.bib")
file41<-c("C:\\Scopus_21_04_2021\\scopus9.bib")
file42<-c("C:\\Scopus_21_04_2021\\scopus10.bib")
file43<-c("C:\\Scopus_21_04_2021\\scopus11.bib")
file44<-c("C:\\Scopus_21_04_2021\\scopus12.bib")
file45<-c("C:\\Scopus_21_04_2021\\scopus13.bib")
file46<-c("C:\\Scopus_21_04_2021\\scopus14.bib")
file47<-c("C:\\Scopus_21_04_2021\\scopus15.bib")
file48<-c("C:\\Scopus_21_04_2021\\scopus16.bib")
file49<-c("C:\\Scopus_21_04_2021\\scopus17.bib")
file50<-c("C:\\Scopus_21_04_2021\\scopus18.bib")
file51<-c("C:\\Scopus_21_04_2021\\scopus19.bib")
file52<-c("C:\\Scopus_21_04_2021\\scopus20.bib")
file53<-c("C:\\Scopus_21_04_2021\\scopus21.bib")
file54<-c("C:\\Scopus_21_04_2021\\scopus22.bib")


Q1 <- convert2df(file = file1, dbsource = "isi", format = "bibtex")
Q2 <- convert2df(file = file2, dbsource = "isi", format = "bibtex")
Q3 <- convert2df(file = file3, dbsource = "isi", format = "bibtex")
Q4 <- convert2df(file = file4, dbsource = "isi", format = "bibtex")
Q5 <- convert2df(file = file5, dbsource = "isi", format = "bibtex")
Q6 <- convert2df(file = file6, dbsource = "isi", format = "bibtex")
Q7 <- convert2df(file = file7, dbsource = "isi", format = "bibtex")
Q8 <- convert2df(file = file8, dbsource = "isi", format = "bibtex")
Q9 <- convert2df(file = file9, dbsource = "isi", format = "bibtex")
Q10 <- convert2df(file = file10, dbsource = "isi", format = "bibtex")
Q11 <- convert2df(file = file11, dbsource = "isi", format = "bibtex")
Q12 <- convert2df(file = file12, dbsource = "isi", format = "bibtex")
Q13 <- convert2df(file = file13, dbsource = "isi", format = "bibtex")
Q14 <- convert2df(file = file14, dbsource = "isi", format = "bibtex")
Q15 <- convert2df(file = file15, dbsource = "isi", format = "bibtex")
Q16 <- convert2df(file = file16, dbsource = "isi", format = "bibtex")
Q17 <- convert2df(file = file17, dbsource = "isi", format = "bibtex")
Q18 <- convert2df(file = file18, dbsource = "isi", format = "bibtex")
Q19 <- convert2df(file = file19, dbsource = "isi", format = "bibtex")
Q20 <- convert2df(file = file20, dbsource = "isi", format = "bibtex")
Q21 <- convert2df(file = file21, dbsource = "isi", format = "bibtex")
Q22 <- convert2df(file = file22, dbsource = "isi", format = "bibtex")
Q23 <- convert2df(file = file23, dbsource = "isi", format = "bibtex")
Q24 <- convert2df(file = file24, dbsource = "isi", format = "bibtex")
Q25 <- convert2df(file = file25, dbsource = "isi", format = "bibtex")
Q26 <- convert2df(file = file26, dbsource = "isi", format = "bibtex")
Q27 <- convert2df(file = file27, dbsource = "isi", format = "bibtex")
Q28 <- convert2df(file = file28, dbsource = "isi", format = "bibtex")
Q29 <- convert2df(file = file29, dbsource = "isi", format = "bibtex")
Q30 <- convert2df(file = file30, dbsource = "isi", format = "bibtex")
Q31 <- convert2df(file = file31, dbsource = "isi", format = "bibtex")
Q32 <- convert2df(file = file32, dbsource = "isi", format = "bibtex")
Q33 <- convert2df(file = file33, dbsource = "scopus", format = "bibtex")
Q34 <- convert2df(file = file34, dbsource = "scopus", format = "bibtex")
Q35 <- convert2df(file = file35, dbsource = "scopus", format = "bibtex")
Q36 <- convert2df(file = file36, dbsource = "scopus", format = "bibtex")
Q37 <- convert2df(file = file37, dbsource = "scopus", format = "bibtex")
Q38 <- convert2df(file = file38, dbsource = "scopus", format = "bibtex")
Q39 <- convert2df(file = file39, dbsource = "scopus", format = "bibtex")
Q40 <- convert2df(file = file40, dbsource = "scopus", format = "bibtex")
Q41 <- convert2df(file = file41, dbsource = "scopus", format = "bibtex")
Q42 <- convert2df(file = file42, dbsource = "scopus", format = "bibtex")
Q43 <- convert2df(file = file43, dbsource = "scopus", format = "bibtex")
Q44 <- convert2df(file = file44, dbsource = "scopus", format = "bibtex")
Q45 <- convert2df(file = file45, dbsource = "scopus", format = "bibtex")
Q46 <- convert2df(file = file46, dbsource = "scopus", format = "bibtex")
Q47 <- convert2df(file = file47, dbsource = "scopus", format = "bibtex")
Q48 <- convert2df(file = file48, dbsource = "scopus", format = "bibtex")
Q49 <- convert2df(file = file49, dbsource = "scopus", format = "bibtex")
Q50 <- convert2df(file = file50, dbsource = "scopus", format = "bibtex")
Q51 <- convert2df(file = file51, dbsource = "scopus", format = "bibtex")
Q52 <- convert2df(file = file52, dbsource = "scopus", format = "bibtex")
Q53 <- convert2df(file = file53, dbsource = "scopus", format = "bibtex")
Q54 <- convert2df(file = file54, dbsource = "scopus", format = "bibtex")


#Mesclar arquivos  da WoS

M<-mergeDbSources(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10,Q11,Q12,Q13,Q14,Q15,Q16,Q17,Q18,Q19,Q20,Q21,Q22,Q23,Q24,Q25,Q26,Q27,Q28,Q29,Q30,Q31,Q32,Q33,Q34,Q35,Q36,Q37,Q38,Q39,Q40,Q41,Q42,Q43,Q44,Q45,Q46,Q47,Q48,Q49,Q50,Q51,Q52,Q53,Q54)

View(M)

#####
results <- biblioAnalysis(M3, sep = ";")
#Functions summary and plot
options(width=7000)

S <- summary(object = results, k = 30, pause = FALSE)
plot(x = results, k = 15, pause = FALSE)


#Analysis of Cited References
CR <- citations(M, field = "countries", sep = ";")
cbind(CR$Cited[1:200])
CR1 <- citations(M, field = "article", sep = ";")
cbind(CR1$Cited[1:10])
#write.xls(cbind(CR1$Cited),"ArticlePlus.csv")
CR <- citations(M, field = "authors", sep = ";")
cbind(CR$Cited[1:10])
#write.csv(cbind(CR$Cited[1:68]),"author1.csv")
CR <- localCitations(M, sep = ";")
CR$Authors[1:25,]
CR$Papers[1:25,]
write.csv(CR$Authors[1:25,],"CRauthors.csv")
write.csv(CR$Papers[1:25,],"CRpapers1.csv")

#Authors' Dominance ranking
DF <- dominance(results, k = 40)
View(DF)
write.csv(DF,"Dominance1.csv")

#Authors' h-index

indices <- Hindex(M, field = "author", elements="WANG X", sep = ";", years = 54)
#indices <- Hindex(M, field = "source", elements="INTERNATIONAL JOURNAL OF INFORMATION TECHNOLOGY \\& DECISION MAKING", sep=";", years=54)
# Bornmann's impact indices:
indices$H

# Bornmann's citations
ZZ=indices$CitationList
write.csv(ZZ,"ZZ.csv")
authors=gsub(","," ",names(results$Authors)[1:11])
indices <- Hindex(M, field = "author", elements=authors, sep = ";", years = 54)
indices$H
write.csv(indices$H,"IndiceHauthors1.csv")

#Top-Authors' Productivity over the Time
topAU <- authorProdOverTime(M,k = 10, graph = TRUE)
#####
## Table: Author's productivity per year
head(topAU$dfAU)
## Table: Auhtor's documents list
##head(topAU$dfPapersAU)
#Lotka's Law coefficient estimation
L <- lotka(results)
# Author Productivity. Empirical Distribution
L$AuthorProd
write.csv(L$AuthorProd,"authorproduc.csv")
# Beta coefficient estimate
L$Beta
# Constant
L$C
# Goodness of fit
L$R2
# P-value of K-S two sample test
L$p.value
# Observed distribution
Observed=L$AuthorProd[,3]
# Theoretical distribution with Beta = 2
Theoretical=10^(log10(L$C)-2*log10(L$AuthorProd[,1]))
plot(L$AuthorProd[,1],Theoretical,type="l",col="red",ylim=c(0, 1), xlab="Articles",ylab="Freq. of Authors",main="Scientific Productivity")
lines(L$AuthorProd[,1],Observed,col="blue")
legend(x="topright",c("Theoretical (B=2)","Observed"),col=c("red","blue"),lty =c(1,1,1),cex=0.6,bty="n")

#Bibliographic network matrices
#Bipartite networks
A <- cocMatrix(M, Field = "AU_UN", sep = ";")
View(A)
UNIVERSITY<-sort(Matrix::colSums(A), decreasing = TRUE)[1:4800]
View(UNIVERSITY)
write.csv(UNIVERSITY,"UNIVERSITY4.csv")
#Citation network
A <- cocMatrix(M, Field = "CR", sep = ". ")
#Author network
A <- cocMatrix(M, Field = "SO", sep = ";")
View(A)
#Country network
M <- metaTagExtraction(M, Field = "AU_UN", sep = ";")
# A <- cocMatrix(M, Field = "AU_CO", sep = ";")
#Author keyword network
A <- cocMatrix(M, Field = "DE", sep = ";")
#Keyword Plus network
A <- cocMatrix(M, Field = "ID", sep = ";")

#Bibliographic coupling
NetMatrix1 <- biblioNetwork(M, analysis = "coupling", network = "references", sep = ";")
NetMatrix2 <- biblioNetwork(M, analysis = "coupling", network = "authors", sep = ";")
net1=networkPlot(NetMatrix1, normalize = NULL, weighted=NULL, n = 20,degree=60, Title = "References' Coupling",type = "kamada",size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
net2=networkPlot(NetMatrix2, normalize = NULL, weighted=NULL, n = 20,degree=100, Title = "Authors' Coupling",type = "kamada",size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
net2VOSviewer(net1, vos.path = NULL)
net2VOSviewer(net2, vos.path = NULL)
netstat <- networkStat(NetMatrix2)
summary(netstat, k=110)
#Bibliographic co-citation
# NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "references", sep = ". ")
#Bibliographic collaboration
M4 <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix3 <- biblioNetwork(M4, analysis = "collaboration", network = "authors", sep = ";")
NetMatrix4 <- biblioNetwork(M4, analysis = "collaboration", network = "countries", sep = ";")
net3=networkPlot(NetMatrix3, normalize = NULL, weighted=NULL, n = 20,degree=10, Title = "Authors' Collaboration",type = "auto",size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
net4=networkPlot(NetMatrix4, normalize = NULL, weighted=NULL, n = 20,degree=30, Title = "Countries' Collaboration",type = "kamada",halo=FALSE,cluster="louvain",size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.color=TRUE,label.n=15,label.cex=F)

networkPlot(NetMatrix3, normalize = NULL, weighted=NULL, n = 20,degree=10, Title = "Authors' Collaboration",type = "circle",size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.n=10,label.cex=F)
networkPlot(NetMatrix4, normalize = NULL, weighted=NULL, n = 20,degree=30, Title = "Countries' Collaboration",type = "kamada",halo=FALSE,cluster="louvain",size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.color=TRUE,label.n=15,label.cex=F)

net2VOSviewer(net3)
net2VOSviewer(net4, vos.path = NULL)
netstat <- networkStat(NetMatrix3, stat="all", type="degree")
summary(netstat, k=144)

#Descriptive analysis of network graph characteristics
# An example of a classical keyword co-occurrences network
NetMatrix5 <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
net5=networkPlot(NetMatrix5, normalize = NULL, weighted=NULL, n = 20,degree=400, Title = "",type = "kamada",halo=FALSE,cluster="louvain",size=5,size.cex=T,remove.multiple=TRUE,labelsize=0.8,label.color=TRUE,label.n=10,label.cex=F)
net2VOSviewer(net5)
netstat <- networkStat(NetMatrix)
summary(netstat, k=110)

#Visualizing bibliographic networks
##Country Scientific Collaboration

# Create a country collaboration network
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")
#net=networkPlot(NetMatrix, n = 50, Title = "Country Collaboration", type = "circle",
                size=TRUE, remove.multiple=FALSE,labelsize=0.5,cluster="none")
networkPlot(NetMatrix, normalize = NULL, n = 100, degree = 0,
            Title = "Country Collaboration", type = "auto", label = TRUE, labelsize = 1,
            label.cex = FALSE, label.color = FALSE, label.n = NULL,
            halo = FALSE, cluster = "louvain", vos.path = NULL, size = 3,
            size.cex = FALSE, curved = FALSE, noloops = TRUE,
            remove.multiple = TRUE, remove.isolates = FALSE, weighted = NULL,
            edgesize = 1, edges.min = 0, alpha = 0.5, verbose = TRUE)
netstat <- networkStat(NetMatrix)
netstat <- networkStat(NetMatrix, stat = "all", type = "degree")
summary(netstat,k=110)

# Create a Edu collaboration network
M <- metaTagExtraction(M, Field = "AU_UN", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration",  network = "universities", sep = ";")
net=networkPlot(NetMatrix,  n = 100, degree = 200,Title = "Edu collaboration",type = "auto", size=10,size.cex=T,edgesize = 3,labelsize=0.6)
net=networkPlot(NetMatrix, normalize = NULL, n = 50, degree = 120,
            Title = "Edu collaboration", type = "kamada", label = TRUE, labelsize = 1,
            label.cex = FALSE, label.color = FALSE, label.n =20,
            halo = FALSE, cluster = "louvain", vos.path = NULL, size = 3,
            size.cex = FALSE, curved = FALSE, noloops = TRUE,
            remove.multiple = TRUE, remove.isolates = FALSE, weighted = NULL,
            edgesize = 1, edges.min = 0, alpha = 0.5, verbose = TRUE)
net2VOSviewer(net)
netstat <- networkStat(NetMatrix, stat = "all", type = "degree")
summary(netstat,k=110)


# Create a Authors collaboration network
M <- metaTagExtraction(M, Field = "CR_AU", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")

net<-networkPlot(NetMatrix, normalize = NULL, n = 100, degree = 340,
            Title = "", type = "mds", label = TRUE, labelsize = 1,
            label.cex = FALSE, label.color = FALSE, label.n = NULL,
            halo = FALSE, cluster = "louvain", vos.path = NULL, size = 3,
            size.cex = FALSE, curved = FALSE, noloops = TRUE,
            remove.multiple = TRUE, remove.isolates = FALSE, weighted = NULL,
            edgesize = 1, edges.min = 0, alpha = 0.5, verbose = TRUE)
net2VOSviewer(net, vos.path = NULL)

net=networkPlot(NetMatrix, n = 50, Title = "", type = "circle",
                size=TRUE, remove.multiple=FALSE,labelsize=0.5,cluster="none")
netstat <- networkStat(NetMatrix, stat="all")
summary(netstat,k=15)
# Source a Authors collaboration network
M <- metaTagExtraction(M, Field = "CR_SO", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "source", sep = ";")
net<-networkPlot(NetMatrix, normalize = NULL, n = 100, degree = 50,
            Title = "Source Collaboration", type = "kamada", label = TRUE, labelsize = 1,
            label.cex = FALSE, label.color = FALSE, label.n = 10,
            halo = FALSE, cluster = "louvain", vos.path = NULL, size = 3,
            size.cex = FALSE, curved = FALSE, noloops = TRUE,
            remove.multiple = TRUE, remove.isolates = FALSE, weighted = NULL,
            edgesize = 1, edges.min = 0, alpha = 0.5, verbose = TRUE)
net2VOSviewer(net, vos.path = NULL)
net=networkPlot(NetMatrix, n = 50, Title = "Author Collaboration", type = "circle",
                size=TRUE, remove.multiple=FALSE,labelsize=0.5,cluster="none")
net=networkPlot(NetMatrix, n = 50, Title = "Source Collaboration", type = "circle",
                size=TRUE, remove.multiple=FALSE,labelsize=0.5,cluster="none")
netstat <- networkStat(NetMatrix)
summary(netstat,k=15)
# Plot the network
net=networkPlot(NetMatrix, n = dim(NetMatrix)[1], Title = "Country Collaboration", type = "circle",
                size=TRUE, remove.multiple=FALSE,labelsize=0.5,cluster="none")
#Co-Citation Network

# Create a co-citation network
M <- metaTagExtraction(M, Field = "CR_AU", sep = ";")
NetMatrix <- biblioNetwork(M, analysis = "co-citation", network = "authors", sep = ";")
networkPlot(NetMatrix, n =200,degree = 200, Title = "", cluster="edge_betweenness", type = "sphere", size.cex=TRUE, size=20, remove.multiple=FALSE, labelsize=0.7,edgesize = 10, edges.min=5)
net2VOSviewer(net, vos.path = NULL)
netstat <- networkStat(NetMatrix, stat="all")
summary(netstat,k=15)

#Keyword co-occurrences
KeywordGrowth(M,Tag="ID", sep=";", top=10, cdf=TRUE)
R=KeywordGrowth(M,Tag="ID", sep=";", top=10, cdf=TRUE)
plot(R)

write.csv(KeywordGrowth(M,Tag="DE", sep=";", top=10, cdf=TRUE),"KeywordGrowth_DE.csv")
write.csv(KeywordGrowth(M,Tag="ID", sep=";", top=10, cdf=TRUE),"KeywordGrowth_ID.csv")
keywordAssoc(M,sep=";", n=10, excludeKW=NA)

# Create keyword co-occurrences network
NetMatrix9 <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords",n=250, sep = ";")
net9=networkPlot(NetMatrix9, normalize=NULL,degree=300, Title = "Keyword Co-occurrences", type = "fruchterman", size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=3,label.cex=TRUE,label.n=30,edges.min=2)
net2VOSviewer(net9, vos.path = NULL)

netstat <- networkStat(NetMatrix9)
summary(netstat,k=15)



#Co-Word Analysis: The conceptual structure of a field
# Conceptual Structure using keywords (method="CA")
CS <- conceptualStructure(M, method="MCA", field="ID", minDegree=200, clust="auto", stemming=FALSE, labelsize=8,documents=20)
#Historical Direct Citation Network
# Create a historical citation network
options(width=130)
histResults <- histNetwork(M, min.citations = 10, sep = ";")
# Plot a historical co-citation network
net <- histPlot(histResults, n=30, size = 8, labelsize=5)
#thematicMap
Map=thematicMap(M, field = "DE", n = 500, minfreq = 2,
                stemming = FALSE, size = 0.5, n.labels=1, repel = FALSE)
plot(Map$map)


threeFieldsPlot(M, fields = c("AU", "DE", "SO"), n = c(10, 10, 10))

#thematicEvolution
data(scientometrics, package = "bibliometrixData")
years=c(1986, 1995,2004,2013)
#years=c(1991, 2001, 2011)
nexus <- thematicEvolution(M,field="DE",years=years,n=500,minFreq=2)
plotThematicEvolution(nexus$Nodes,nexus$Edges)

years=c(2015,2021)
thematicEvolution(M, field = "ID", years=years, n = 250, minFreq = 2, size = 0.5, ngrams = 1, stemming = FALSE, n.labels = 1, repel = TRUE)
plot(Map$map)

data(scientometrics, package = "bibliometrixData")
topSO=sourceGrowth(M, top=5, cdf=TRUE)
topSO
# Plotting results
#Not run:
install.packages("reshape2")
library(reshape2)
library(ggplot2)
DF=melt(topSO, id='Year')
ggplot(DF,aes(Year,value, group=variable, color=variable))+geom_line()
#End(Not run)

