# 1. mushroom 데이터 읽기

mushroom <- read.table("agaricus-lepiota.data", sep = ",", na.strings = "?", as.is = FALSE)
str(mushroom)

colnames(mushroom)<- c("classes","cap.shape","cap.surface","cap.color","bruises",
                       "odor","gill.attachment","gill.spacing","gill.size",
                       "gill.color","stalk.shape","stalk.root","stalk.surface.above.ring",
                       "stalk.surface.below.ring","stalk.color.above.ring",
                       "stalk.color.below.ring","veil.type","veil.color","ring.number",
                       "ring.type","spore.print.color","population","habitat")

summary(mushroom) #veil.type(17번째 열)은 모두 p인 것을 확인
mushroom <- mushroom[,-17]

apply(mushroom, 2, function(x){sum(is.na(x))})
# stalk-root(12번째 열)에 2480개의 결측값이 있음을 확인


# 2. 전처리(방법1 컬럼별 동일한 문자값이 가장 많은 k개만큼 추출 -> 값을 대체 -> 원핫인코딩)

# 1) V12에 있는 NA값 처리

st.d <- mushroom[,-12]
root <- mushroom[[12]]

na.idx <- which(is.na(root))

na.d <- st.d[na.idx,]
st.d <- st.d[-na.idx,]

for(i in 1:length(na.idx)){
  dist <- colSums(apply(st.d, 1, function(x){x == na.d[i,]}))
  res <- table(head(na.omit(root[order(dist)]),5))
  root[na.idx[i]] <- names(res[which.max(res)])
}

mushroom$stalk.root <- root

## 문제점 : 봐야할 데이터가 너무 많아 위의 for문을 처리하는데 시간이 엄청 걸린다

# 2) 문자 -> 수치화

old.colname <- colnames(mushroom[,-1])

for(i in old.colname){
  level <- unique(mushroom[[i]])
  for(j in level){
    new <- paste(i, j, sep = ".")
    mushroom[new] <- ifelse(mushroom[i]==j,1,0)
  }
}

colnames(mushroom)

one.hot.encoding <- mushroom[,c(1,23:ncol(mushroom))]




# 2. 전처리(방법2 컬럼별 문자를 원핫인코딩 -> knn -> ?값을 대체)

mushroom <- read.table("agaricus-lepiota.data", sep = ",", na.strings = "?", as.is = FALSE)
str(mushroom)

colnames(mushroom)<- c("classes","cap.shape","cap.surface","cap.color","bruises",
                       "odor","gill.attachment","gill.spacing","gill.size",
                       "gill.color","stalk.shape","stalk.root","stalk.surface.above.ring",
                       "stalk.surface.below.ring","stalk.color.above.ring",
                       "stalk.color.below.ring","veil.type","veil.color","ring.number",
                       "ring.type","spore.print.color","population","habitat")

summary(mushroom) #veil.type(17번째 열)은 모두 p인 것을 확인
mushroom <- mushroom[,-17]

apply(mushroom, 2, function(x){sum(is.na(x))})

# 이 mushroom은 level이 1개인 17번 열을 제거만 한 상태


# 1) 컬럼별 문자 원핫인코딩


old.colname <- colnames(mushroom[,-c(1,12)]) 
#class와 결측값존재하는 stalk.root 제외한 상태

for(i in old.colname){
  level <- unique(mushroom[[i]])
  for(j in level){
    new <- paste(i, j, sep = ".")
    mushroom[new] <- ifelse(mushroom[i]==j,1,0)
  }
}

colnames(mushroom)

one.hot.encoding <- mushroom[,c(12,23:ncol(mushroom))]

# 2) train 데이터와 test 데이터 생성
str(one.hot.encoding[[1]]) #factor
one.hot.encoding[[1]] <- as.character(one.hot.encoding[[1]]) #chr로 변환

idx <- which(is.na(mushroom$stalk.root))
train <- one.hot.encoding[-idx,]  #결측값 가지지 않는 경우
na.part <- one.hot.encoding[idx,]  #결측값 가지는 경우


# 결측값 가지지 않는 경우 내에서 다시 train과 test를 나눴다

set.seed(0310)
nrow(train)*0.7 #3950.8
tr_idx <- sample(1:nrow(train), 3950)
train_tr <- train[tr_idx,-1]
str(train_tr)
train_te <- train[-tr_idx,-1]

labels <- train[,1]
train_labels <- labels[tr_idx]
test_labels <- labels[-tr_idx]


# 3) 트레인 데이터로 knn 모델 생성

library(class)
train_te_pred <- knn(train = train_tr,
                     test = train_te,
                     cl = train_labels,
                     k=5)
train_te_pred

# 4) 테스트 데이터로 테스트

sum(train_te_pred==test_labels) / length(test_labels)  # 0.9988194

# 5) NA값을 knn 적용하여 예측값 넣기

na.part <- na.part[,-1]

na.part_pred <- knn(train = train_tr,
                     test = na.part,
                     cl = train_labels,
                     k=5)
na.part_pred <- as.character(na.part_pred)
na.part_pred

one.hot.encoding[idx,][[1]] <- na.part_pred
one.hot.encoding[idx,][[1]]

# 6) stalk.root열도 원핫인코딩 진행

level <- unique(one.hot.encoding[[1]]) #"e" "c" "b" "r"
for(j in level){
  new <- paste("stalk.root", j, sep = ".")
  one.hot.encoding[new] <- ifelse(one.hot.encoding[[1]]==j,1,0)
}

names(one.hot.encoding)

# 전처리 끝낸 자료

new.mushroom <- cbind(class=mushroom[,1], one.hot.encoding[,-1]) #class열 factor
new.mushroom$class <- as.character(new.mushroom$class) #class열 chr로 변경

# 3. train data : test data = 7:3으로 나누기

set.seed(0311)
nrow(new.mushroom)*0.7 #5686.8
final_idx <- sample(1:nrow(new.mushroom), 5687)
train <- new.mushroom[final_idx,-1]
test <- new.mushroom[-final_idx,-1]

train_labels <- new.mushroom$class[final_idx]
test_labels <- new.mushroom$class[-final_idx]

# 4. knn을 이용한 모델 생성

test_pred <- knn(train = train,
                 test = test,
                 cl = train_labels,
                 k=4)
train_te_pred

# 5. 정확도 확인

sum(test_pred==test_labels) / length(test_labels)
