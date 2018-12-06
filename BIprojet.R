library(stringr)
library(tm)

mat=read.csv("/Users/jzk/Documents/M2/BI/DBLPOnlyCitationOct19.txt")
mat=readLines("/Users/jzk/Documents/M2/BI/DBLPOnlyCitationOct19.txt",encoding="utf-8")
#mat=read.csv("/Users/jzk/Documents/M2/BI/out.txt",encoding="UTF-8")
dim(mat)[1]
substring(mat$X1632442[1], 3)
substring(mat$X1632442[1], 1, 2) == "#*"


d <- function(str){
  return(str!="")
}

f <- function(n){
  df=data.frame(Venue=character(),
                Year=character(), 
                Authers=character(),
                Title=character(),
                NbrAuther=integer(),
                Id=integer(),
                ListCitation=character(),
                NbrCitation=integer(),
                stringsAsFactors=FALSE) 
  for(i in 1:(dim(n)[1])){
    if(i%%1000==0)
      print(i)
    if(substring(n$X1632442[i], 1, 2) == "#*"){
      Title <- toString(substring(n$X1632442[i], 3))
      j = i+1
      Authers <- toString(substring(n$X1632442[j], 3))
      while (substring(n$X1632442[j+1], 1, 1) != "#"){
        j = j+1
        Authers=paste(Authers," , ",n$X1632442[j])
      }
      i = j-1
      while(substring(n$X1632442[i+2], 1, 2) != "#t")
        i = i+1
      Year <- toString(substring(n$X1632442[i+2], 3))
      c = i+3
      Venue <- toString(substring(n$X1632442[i+3], 2))
      Id <- toString(substring(n$X1632442[i+4], 7))
      i = i+5
      ListCitation=""
      if(substring(n$X1632442[i], 1, 2) == "#%" && i < dim(n)[1] ){
        ListCitation <- toString(substring(n$X1632442[i], 3))
        while(substring(n$X1632442[i+1], 1, 2) == "#%" ){
          i=i+1
          ListCitation=paste(ListCitation," , ",substring(n$X1632442[i], 3))
        }
      }
      NbrAuther <- str_count(Authers,",")+1
      NbrCitation <- str_count(ListCitation,",")+d(ListCitation)
      if(substring(n$X1632442[c], 1, 6) == "#cSTOC" || substring(n$X1632442[c], 1, 7) == "#cSIGIR")
        df = rbind(df,data.frame(Venue,Year,Authers,Title,NbrAuther,Id,ListCitation,NbrCitation))
    }
  }
  return (df)
}

df=f(mat)
dim(df)
write.csv(df,"/Users/jzk/Documents/M2/BI/articles.csv")
View(df)
######################
#####################
mat=read.csv("/Users/jzk/Documents/M2/BI/articles.csv")
wordVC <- as.character(mat$Title)
corpus <- (VectorSource(wordVC))
corpus <- Corpus(corpus)
summary(corpus)
tdm <- TermDocumentMatrix(corpus)
inspect(tdm)
dtm = DocumentTermMatrix(corpus)
inspect(dtm)
dtmBig=matrix(dtm,nrow=dtm$nrow,ncol=dtm$ncol,dimnames =dtm$dimnames)
dtmdf=as.data.frame(dtmBig)

fun <- function(){
  vect=594377
  for(i in 2:5828)
    vect=c(vect,df$Id[i])
  return(vect)
}
Id=fun()
dtmdf=cbind(Id,dtmdf)
write.csv(dtmdf,"/Users/jzk/Documents/M2/BI/dtm.csv")
dtmdf=read.csv("/Users/jzk/Documents/M2/BI/dtm.csv")


#######Matrice auteurs
fun2 <- function(n){
  vect=""
  for(i in 1:length(n)){
    for(j in 1:str_count(n[i],",")){
      vect=c(vect,strsplit(n[i],split=',', fixed=TRUE)[[1]][j])
    }
  }
  return(vect)
}
a=fun2(as.character(mat$Authers))
dicAuthers= sort(unique(a))
dfAuthersId = data.frame("Auther"=dicAuthers, "Id"=c(1:length(dicAuthers)))


fun3 <- function(n,df){
  vect=""
  for(i in 1:length(df$Id))  ###pour  tous les auteurs on intialise ?? vide
    vect=paste(vect,"")
  for(i in 1:length(n)){  ##pour  tous les articles
    for(j in 1:str_count(n[i],",")){ ###pour tous les auteurs cit??s dans un article
      a= df$Id[which(str_detect(df$Auther, gsub("([^A-Za-z. ])+", " ", x =strsplit(n[i],split=',', fixed=TRUE)[[1]][j])))] ###on renvoie l'id de l'auteur
      if (identical(a, integer(0))){
        a=0
      }
      else{
        vect[as.integer(a)]=paste(vect[as.integer(a)],",")
        vect[as.integer(a)]=paste(vect[as.integer(a)],as.character(i))
      }
    }
  }
  for(i in 1:length(vect)){
    vect[i]=substring(vect[i], 6)
    vect[i]=gsub(" ", "", vect[i])
    vect[i]=paste(" ",vect[i])
    if (substring(vect[i],1,1) ==",")
      vect[i]=substring(vect[i], 2)
    if (substring(vect[i],1,1) ==" ")
      vect[i]=substring(vect[i], 3)
    if (substring(vect[i],1,2) =="NA")
      vect[i]=sample(1:length(df$Id),1)
  }
  return(as.character(vect))
  
}

dfAuthersId$Id[which(str_detect(dfAuthersId$Auther, gsub("([^A-Za-z0-9 ])+", "", x =strsplit(as.character(mat$Authers[3]),split=',', fixed=TRUE)[[1]][1344])))]
dfAuthersArticle=fun3(as.character(mat$Authers),dfAuthersId)
dfAuthers=cbind(dfAuthersId,dfAuthersArticle)
View(dfAuthers)
write.csv(dfAuthers,"/Users/jzk/Documents/M2/BI/Authers.csv")


#nb articles par auteur:
fun4 <- function(){
  vect=1
  for(i in 2:4355)
    vect=c(vect,str_count(dfAuthers$dfAuthersArticle[i],",")+1)
  return(vect)
}
NbArticles=fun4()
dfAuthers=cbind(dfAuthers,NbArticles)



#####Last matrice
wordVC <- as.character(mat$ListCitation)
corpus <- (VectorSource(wordVC))
corpus <- Corpus(corpus)
summary(corpus)
tdm <- TermDocumentMatrix(corpus)
inspect(tdm)
docdoc = DocumentTermMatrix(corpus)
inspect(docdoc)
dtmBig=matrix(docdoc,nrow=docdoc$nrow,ncol=docdoc$ncol,dimnames =docdoc$dimnames)
docdocdf=as.data.frame(dtmBig)
docdocdf=cbind(Id,docdocdf)
write.csv(docdocdf,"/Users/jzk/Documents/M2/BI/docdoc.csv")
View(docdocdf)


######Reduction de la taille des matrices via tfidf ````
mat=read.csv("/Users/jzk/Documents/M2/BI/articles.csv")
dtmdf=read.csv("/Users/jzk/Documents/M2/BI/dtm.csv")
dfAuthers=read.csv("/Users/jzk/Documents/M2/BI/Authers.csv")
docdocdf=read.csv("/Users/jzk/Documents/M2/BI/docdoc.csv")

###reduction matrice document termes
dim(dtmdf)
head(dtmdf)
View(dtmdf[,c(-1,-2)])

term.frequency=function(row){ ###Calcul TF
  row/sum(row)
}

inverse.doc.frequency=function(col){ ####Calcul IDF
  corpus.size=length(col)
  doc.count=length(which(col > 0))
  
  log10(corpus.size/(1+doc.count))
}

tf.idf=function(tf,idf){
  tf * idf
}

data_GpM=dtmdf
data_GpM.df <- apply(data_GpM, 1, term.frequency)
data_GpM.idf <- apply(data_GpM, 2, inverse.doc.frequency)
data_Gpm.tfidf <-  apply(data_GpM.df, 2, tf.idf, idf = data_GpM.idf)

####reduction de la taille de la matrice pour les tests avant production #### 
rangMaladies=order(rowSums(data_Gpm.tfidf),decreasing=T)[c(1:1000)];length(rangMaladies) #### ON prend les 500 premi??res maladies importantes dans le tf-idf
data_used=t(data_GpM[,rangMaladies]);dim(data_used) #### matrice 500x non normalis??e
data_used=data_used[,which(colSums(data_used) > 0)];dim(data_used);View(data_used)
dim(t(data_used))
dtmdfred=t(data_used)
write.csv(dtmdfred,"/Users/jzk/Documents/M2/BI/dtmReduced.csv")

dim(docdocdf)
head(docdocdf)
View(docdocdf[,c(-1,-2)])

data_GpM = docdocdf
data_GpM.df <- apply(data_GpM, 1, term.frequency)
data_GpM.idf <- apply(data_GpM, 2, inverse.doc.frequency)
data_Gpm.tfidf <-  apply(data_GpM.df, 2, tf.idf, idf = data_GpM.idf)

rangMaladies=order(rowSums(data_Gpm.tfidf),decreasing=T)[c(1:1000)];length(rangMaladies) #### ON prend les 500 premi??res maladies importantes dans le tf-idf
data_used=t(data_GpM[,rangMaladies]);dim(data_used) #### matrice 500x16591 non normalis??e
data_used=data_used[,which(colSums(data_used) > 0)];dim(data_used);View(data_used)
data_GpM.df <- apply(data_used, 1, term.frequency)#### on refait un tfidf sur la matrice pr??c??dente pour r??cup??rer les indices des g??nes importants sur les maladies restantes
data_GpM.idf <- apply(data_used, 2, inverse.doc.frequency)
data_Gpm.tfidf <-  apply(data_GpM.df, 2, tf.idf, idf = data_GpM.idf);dim(data_Gpm.tfidf)
rangGenes=order(rowSums(data_Gpm.tfidf),decreasing=T)[c(1:1000)];length(rangGenes) ### on prend les 6000 g??nes les plus important
data_used=data_used[,rangGenes];dim(data_used)####on a une matrice 1500x6000 avec des trucs repr??sentatifs
View(t(data_used))
write.csv(t(data_used),"/Users/jzk/Documents/M2/BI/docdocReduced.csv")

library(skmeans)
k.max <- 15
wss <- sapply(2:k.max, 
              function(k){skmeans(dtmdfred, k,control = list(verbose = TRUE))$value})
wss
plot(2:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Value of the criterion")
findElbow(wss) ####nombre de clusters pour la methode Skmeans sur le jeu de donn??es.


library(cluster)
library(fpc)
d=dtmdfred[,c(-1,-4,-5)]
d=d[ rowSums(d)!=0, ] 
k=skmeans(d, 4,control = list(verbose = TRUE))
plotcluster(dtmdfred, k$cluster)

dim(dtmdfred)
View(dtmdfred)
##extraction des termes pour chaque groupe
k$v
y=kmeans(dtmdfred[,c(-1,-4,-5)],4)
y$cluster
plotcluster(dtmdfred, k$cluster)
table(k$cluster,y$cluster)

dim(y$centers)
y$centers[1]
df=cbind(df,k$cluster[-1])
subset4<-subset(df,df$`k$cluster[-1]`==4)
write.csv(subset4,"/Users/jzk/Documents/M2/BI/subset4.csv")
write.csv(k$prototypes[1,],"/Users/jzk/Documents/M2/BI/subset1w.csv")




########Second way with abstract for bonus if time
df<-data.frame(Title=character(),Authers=character(),Year=character(),Venue=character(),Id=character(),ListCitation=character(),Abstract=character(),NbrAuther=integer(),NbrCitation=integer())
dfnames<-data.frame(Title=character(),Authers=character(),Year=character(),Venue=character(),Id=character(),ListCitation=character(),Abstract=character(),NbrAuther=integer(),NbrCitation=integer())


for (i in 1:length(data)) {
  if(substr(data[i],1,6)=="#cSTOC" | substr(data[i],1,7)=="#cSIGIR"){
    print(i)
    df2<-cbind(data[i-3],data[i-2],data[i-1],data[i],data[i+1])
    countAuthers= str_count(data[i-2],",")+1
    j=i+2
    str=""
    abs=""
    boo=0
    while(substr(data[j],1,2)!="#*"){
      if(substr(data[j],1,2)=="#%"){
        str=paste(str,data[j])
        if(substr(data[j+1],1,2)=="#%"){
          str=paste(str,",")
          boo=1
        }
      }
      if(substr(data[j],1,2)=="#!"){
        abs=paste(abs,data[j])
        if(substr(data[j+1],1,2)=="#!"){
          abs=paste(abs,",")
        }
      }
      j=j+1
    }
    countCitations = str_count(str,",")+boo
    df2<-cbind(df2,str,abs,countAuthers,countCitations)
    df<-rbind(df,df2)
  }
}

names(df)<-names(dfnames)
df$Title<-as.character(df$Title);df$Authers<-as.character(df$Authers);df$Year<-as.character(df$Year);df$Venue<-as.character(df$Venue);df$Id<-as.character(df$Id);df$ListCitation<-as.character(df$ListCitation);df$Abstract<-as.character(df$Abstract)
df[,1]<-str_remove(df[,1],"[#]");df[,1]<-str_remove(df[,1],"[*]");df[,2]<-str_remove(df[,2],"[#]");df[,2]<-str_remove(df[,2],"[@]")
df[,3]<-str_remove(df[,3],"[#]");df[,3]<-str_remove(df[,3],"[t]");df[,4]<-str_remove(df[,4],"[#]");df[,4]<-str_remove(df[,4],"[c]")
df[,5]<-str_remove_all(df[,5],"[index]");df[,5]<-str_remove(df[,5],"[#]");df[,6]<-str_remove_all(df[,6],"[#]");df[,6]<-str_remove_all(df[,6],"[%]")
df[,7]<-str_remove(df[,7],"[#]");df[,7]<-str_remove(df[,7],"[!]")
