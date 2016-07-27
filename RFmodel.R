library(data.table)
library(bit64)
library(ranger)
library(plotly)
library(Metrics)
library(stats)
library(forecast)

rm(list=ls())
source("model.R")
user.test <- fread("data/tianchi_fresh_comp_train_user.csv")
user.test[, c("time", "hour") := tstrsplit(unlist(time)," ")]
user.test[, usertoitem := paste(user_id, item_id, sep = '_')]
user.item = user.test[behavior_type==4, .(user_item=unique(usertoitem))]
buyer.lovers.line = floor(user.test[behavior_type==4, .N]/user.test[, length(unique(user_id))])+1
favourite.item.line = floor(mean(user.test[behavior_type==4, .N, by=.(item_id)][N>1, ]$N))+1
user.test[, buyer.lover := c(0)]
user.test[user_id %in% user.test[behavior_type==4, .N, by=user_id][N>buyer.lovers.line,]$user_id, ]$buyer.lover <- 1
user.test[, favourite.item := c(0)]
user.test[item_id %in% user.test[behavior_type==4, .N, by=item_id][N>favourite.item.line,]$item_id, ]$favourite.item <- 1
user.test= user.test[, -c("user_geohash", "item_category", "user_id", "item_id"), with=F]
user = copy(user.test)


user.target = user[,.(usertoitem, behavior_type, time = as.Date(time), buyer.lover, favourite.item)]
user.target[, freq := .N, by = names(user.target)]
user.target = user.target[!duplicated(user.target), ]
purchase.user.item = user.target[behavior_type==3, .(is_purchasecart=freq), by=.(time, usertoitem)]
user.target = merge(user.target, purchase.user.item, by=c("usertoitem", "time"), all.x=T)
buy.user.item = user.target[behavior_type==4, .(is_buy=freq), by=.(time, usertoitem)]
user.target = merge(user.target, buy.user.item, by=c("usertoitem", "time"), all.x=T)
user.target[is.na(user.target), ] <- 0
pro.to.buy = user[((hour==22)|(hour==23))&(behavior_type==3), .(has_desire=.N, behavior_type), 
                       by=.(usertoitem, time=as.Date(time))][, .(usertoitem, time, has_desire)]
pro.to.buy = pro.to.buy[!duplicated(pro.to.buy)]
user.target = merge(user.target, pro.to.buy, by=c("usertoitem", "time"), all.x = T)
user.target[is.na(user.target), ] <- 0
user.target = user.target[!duplicated(user.target, by = names(user.target))]


training.data = dcast(user.target, usertoitem + time ~ behavior_type, value.var="freq", fill=0, fun.aggregate=sum)
training.data = merge(training.data, user.target[, -c("behavior_type", "freq"), with=F], 
                      by = c("usertoitem", "time"), all.x = T)
training.data = training.data[!duplicated(training.data,by=names(training.data)), ]
training.data[, label:=ifelse(training.data$`4`>0, 1, 0)]


maxtime = user.target[, max(time)]
val.label.starttime = maxtime
val.predictors.starttime = maxtime - as.difftime(1, units = 'days')
train.label.starttime = maxtime - as.difftime(1, units = 'days')
train.predictors.starttime = maxtime - as.difftime(2, units = 'days')
final.predictors.starttime = maxtime

predictors.train = getPredictors(training.data, train.predictors.starttime)
labels.train = getLabels(training.data, train.label.starttime)
training.dataset = getTrainingData(predictors.train, labels.train)
for(i in 1:28){
    train.label.starttime = train.label.starttime - as.difftime(1, units = 'days')
    train.predictors.starttime = train.predictors.starttime - as.difftime(1, units = 'days')
    predictors.train = getPredictors(training.data, train.predictors.starttime)
    labels.train = getLabels(training.data, train.label.starttime)
    training.dataset = rbind(training.dataset, getTrainingData(predictors.train, labels.train), fill=T)
}
training.dataset[is.na(training.dataset)] = 0
print(summary(training.dataset[, as.factor(label)]))

training.dataset = rbind(training.dataset[label==1], 
                         training.dataset[label==0][sample(.N, training.dataset[label==1, .N] * 15)])
orders.time.series = training.dataset[label==1, .N, by=.(time)]
setkey(orders.time.series, time)
predict.orders.one = floor(arima(orders.time.series$N)$coef)
predict.orders.two = floor(ets(orders.time.series$N)$fit$par[2])
predict.orders = floor((predict.orders.one+predict.orders.two)/2)
training.dataset = training.dataset[, -("time"), with=F]


predictors.val = getPredictors(training.data, val.predictors.starttime)
labels.val = getLabels(training.data, val.label.starttime)
val.dataset = getTrainingData(predictors.val, labels.val)
val.dataset[is.na(val.dataset)] = 0
print(summary(val.dataset[, as.factor(label)]))


feature.set = list(train=training.dataset, val=val.dataset)
model = trainRFModel(feature.set$train)
feature.set$val[, fitted := predictRFModel(model, feature.set$val)]
setorder(feature.set$val, fitted)
feature.set$val[, id := 1:.N]
keypoints = quantile(feature.set$val[label==1, id], c(0, 0.25, 0.5, 0.75, 1))


data.for.plotting = data.table(
    id = seq(1, 6),
    y = c("25-75", "25-75", "50-100", "50-100", "0-100", "0-100"),
    x = c(keypoints[2], keypoints[4], keypoints[3], keypoints[5], keypoints[1], keypoints[5])
)
plot_ly(x = c(0, nrow(feature.set$val)), y = c("ALL", "ALL"), mode = "markers",
        marker = list(color="red"), name="ALL", showlegend = F, themes="Catherine") %>%
    add_trace(x = c(1, nrow(feature.set$val)), y = c("ALL", "ALL"), mode = "lines",
              showlegend = F, line = list(color = "red")) %>%
    add_trace(data = data.for.plotting,
              x = x, y = y, mode = "lines",
              group = y, showlegend = F, line = list(color = "gray")) %>%
    add_trace(data = data.for.plotting,
              x = x, y = y, mode = "markers", name="", showlegend = F) %>%
    add_trace(x = rep(nrow(feature.set$val), 2),
              y = c("ALL", "0-100(p)"),
              mode = "lines", showlegend = F,
              line=list(dash="dot", width=2, aplha=0.5, color='rgba(152, 0, 0, .5)')) %>%
    add_trace(x=feature.set$val[label==1, id], y=rep("0-100(p)", nrow(feature.set$val[label==1])),
              mode = "markers", showlegend = F) %>%
    layout(
        xaxis = list(title = "Ranked Propensity to Buy (low to high)",
                     range = c(1, nrow(feature.set$val)+10)),
        yaxis = list(title=""),
        margin = list(l = 65),
        width=800, height=200)

predictors.dataset = getPredictors(training.data, final.predictors.starttime)
predictors.dataset[, fitted := predictRFBuyers(model, predictors.dataset)]
setorder(predictors.dataset, fitted)
predictors.dataset = predictors.dataset[, .SD[.N:1]]
# version 1
result = predictors.dataset[fitted >= 0.55, ][, .(usertoitem)]
# version 2
result = predictors.dataset[1:predict.orders][, .(usertoitem)]


result <- result[, tstrsplit(unlist(usertoitem),"_"), by = .(usertoitem)]
setnames(result, names(result), c("usertoitem", "user_id", "item_id"))
result.list = result[, .(user_id=as.integer(user_id), item_id=as.integer(item_id))]
result.list = result.list[!duplicated(result.list,by=names(result.list))]

tmp <- evalModel(model, val.dataset, predictRFModel)

write.csv(x = result.list, file = paste(paste('tianchi_mobile_recommendation_predict_RF',as.Date(Sys.time()),sep="_"),'.csv',
                                        sep = ""), row.names = FALSE, fileEncoding = "UTF-8")

# explore result
predict.tianchi <- fread(paste(paste('tianchi_mobile_recommendation_predict_RF',as.Date(Sys.time()),sep="_"),'.csv',sep = ""))
example <- fread("data/选手结果数据样例.csv")
