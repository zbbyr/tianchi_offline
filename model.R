#' @import MASS
#' @export
trainLRModel <- function(training.dataset){
    model = glm(label ~ ., training.dataset, family=binomial)
    return(model)
}

#' @export
predictLRModel <- function(model, dataset){
    return(predict(model, dataset[, -"label", with=F], type="response"))
}

trainRFModel <- function(training.dataset){
    model = ranger(factor(label) ~ ., data=training.dataset, probability = T, write.forest = T)
    return(model)
}

#' @export
predictRFModel <- function(model, dataset){
    return(predict(model, dataset[, -"label", with=F])$predictions[,2])
}

predictRFBuyers <- function(model, dataset){
    return(predict(model, dataset)$predictions[,2])
}

predictLRBuyers <- function(model, dataset){
    return(predict(model, dataset, type="response"))
}

getLabels <- function(training.data, starttime){
    labels <- training.data[(time==starttime) & (label==1), .(usertoitem, label)]
    return(labels)
}

getPredictors <- function(data, starttime){
    predictors = data[time==starttime, -("label"), with = F]
    return(predictors)
}

getTrainingData <- function(predictors, labels){
    training.dataset = merge(predictors, labels, by="usertoitem", all.x=T)
    training.dataset[is.na(label), label := 0]
    return(training.dataset)
}

#' @export
evalModel <- function(model, dataset, predictFunc){
    dataset = copy(dataset)
    dataset[, fitted := predictFunc(model, dataset)]
    setorder(dataset, -fitted)
    dataset[, id := 1:.N]
    print(paste("AUC:", AUC(dataset[, label], dataset[, fitted])))
    print(paste("Log Loss:", logLoss(dataset[, label], dataset[, fitted])))
    print(paste("# of positive samples:",  dataset[label==1, .N]))
    print(sprintf("Rank of the least likely positive sample #%d/%d", dataset[label==1, max(id)], dataset[, .N]))
    print(quantile(dataset[label==1, id], c(0.25, 0.5, 0.75)))
    return(dataset)
}

AUC <- function (actual, predicted) {
    r <- rank(predicted)
    n_pos <- sum(actual == 1)
    n_neg <- length(actual) - n_pos
    auc <- (sum(r[actual == 1]) - n_pos * (n_pos + 1)/2)/n_pos/n_neg
    auc
}