library(rpart)
library(randomForest)
library(e1071)
library(caret)
library(class)

#데이터 저장
ucla = read.csv('http://stats.idre.ucla.edu/stat/data/binary.csv')

#admin을 분류로 실행하기 위해 factor로 변환 
ucla$admit = factor(ucla$admit)

install.packages("caTools")
library(caTools)

idx = sample.split(ucla$admit, SplitRatio=0.6)
Train = ucla[ idx,]
Test  = ucla[!idx,]
nrow( Train )
nrow( Test  )


#사용할 모델 제작
#결정트리
r = rpart(admit~., data = Train)
#tree 50개
Forest50 = randomForest(admit~., data = Train, ntree = 50)
#tree 1000개
Forest1000 = randomForest(admit~., data = Train, ntree = 1000)
#polynomial
SvmP = svm(admit~., data = Train, kernel = 'polynomial')
#radial
SvmR = svm(admit~., data = Train, kernel = 'radial')
#knn
KnnP = knn(Train[,1:4], Test, Train$admit, k = 5)

#예측
#결정트리 예측
RpartP = predict(r, Test, type = 'class')
#50개 예측
Forest50P = predict(Forest50, Test, type = 'class')
#1000개 예측
Forest1000P = predict(Forest1000, Test, type = 'class')
#polynomial 모델 예측
SvmP.P = predict(SvmP, Test, type = 'class')
#Radial 모델 예측
SvmR.P = predict(SvmR, Test, type = 'class')

#혼동행렬
#결정트리
confusionMatrix(RpartP, Test$admit)
#50개
confusionMatrix(Forest50P, Test$admit)
#1000개
confusionMatrix(Forest1000P, Test$admit)
#svm polynomial 모델
confusionMatrix(SvmP.P, Test$admit)
#svm Radial 모델
confusionMatrix(SvmR.P, Test$admit)
#knn 혼동행렬
confusionMatrix(KnnP, Test$admit)
