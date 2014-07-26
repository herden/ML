>download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", destfile="./ML1Pro/pml-training.csv",method="curl")
> DataTrain<-read.csv("./ML1Pro/pml-training.csv")
> head(DataTrain)download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", destfile="./ML1Pro/pml-testing.csv",method="curl")
DataTest<-read.csv("./ML1Pro/pml-testing.csv")
DataTrain<-read.csv("./ML1Pro/pml-training.csv")
asd<-subset(DataTrain, select=colMeans(is.na(DataTrain)) == 0)
DataTrain2 <- DataTrain[, -seq(from = 1, to = 8, by = 1)]
DataTrain3<-subset(DataTrain2, select=colMeans(is.na(DataTrain2)) == 0)
removeColumns <-nearZeroVar(DataTrain3) 
DataTrain3 <-DataTrain3[, -removeColumns]
dim(DataTrain3)

set.seed(1832)
asd <- createDataPartition(y = DataTrain3$classe, p = 0.1, list = F)
asd1<- DataTrain3[asd, ]
asd2<- DataTrain3[-asd, ]
asd4 <- train(classe ~ ., data = asd1, method = "rf")

IObj<-varImp(asd4)
plot(IObj, main = "Top 20 Importance Variable", top = 20)

set.seed(12345)


asd3<-asd2[sample(1:nrow(asd2),500),]

error<-confusionMatrix(asd3$classe, predict(asd4,asd3))
error

outOfSampleError <- 1 - error$overall[1];
names(outOfSampleError) <- "Out of Sample Error"
outOfSampleError

DataTest$classe <- 1:nrow(DataTest);
predict(asd4,DataTest)




keep <- colSums(DF!=0) != 0
keep <- keep[keep == TRUE]
DF<- DF[,names(keep[!is.na(names(keep))])]
