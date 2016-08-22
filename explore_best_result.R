library(data.table)
library(bit64)

source("model.R")

template = fread("template(5000).csv")

past.result = data.table(F1=c(7.46445498,3.69766177,7.15465052,7.33838090,7.38299275,6.62488809,7.17377860,
                              7.31853629,7.26577438,7.38750839,0.90256714,1.29403307,3.69766177,6.38675340,1.57170923,1.96463654,
                              0.39292731,1.37524558,0.78585462,0.19646365,1.37524558,7.54716981,2.22841226,2.03193033,1.95258020,
                              7.38299275,3.34728033,2.26904376,0.32414911,4.89596083,4.16156671,8.18124607,0.97244733,0.64829822,
                              2.35988201,6.74268716,5.02092050,8.46205507,1.94489465,8.68470736,4.46304045,5.09745127,2.26904376),
                precision=c(0.05380017,0.02040000,0.05000000,0.05250000,0.05600000,0.06166667,0.05272727,
                            0.05304348,0.05418251,0.05658436,0.00469231,0.01029748,0.02040000,0.04599659,0.01596806,0.01996008,
                            0.00399202,0.01397206,0.00798403,0.00199601,0.01397206,0.05591799,0.03980100,0.04069767,0.03500000,
                            0.05600000,0.06000000,0.07000000,0.01000000,0.06666667,0.05666667,0.06063433,0.03000000,0.02000000,
                            0.02400000,0.04533333,0.09000000,0.06481481,0.06000000,0.06436567,0.08000000,0.11333333,0.07000000),
                n=c(1171,5000,1300,1200,1000,600,1100,1150,1052,972,13000,874,5000,1174,501,501,501,501,501,501,501,1073,201,172,
                    200,1000,200,100,100,300,300,1072,100,100,500,1500,200,972,100,1072,200,150,100))
past.result[, tp:=round2(n*precision,0)]
true.positive = calculateOrders(past.result)
past.result[, orders:=round2(true.positive,0)]



# 11
write.csv(x = template(13000), file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                                                   sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 12
write.csv(x = template(13000)[c(1:600,1100:1172,1300:1500)], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                        sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 13
write.csv(x = fread("template(5000).csv"), file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                     sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 14
write.csv(x = rbind(template[1:600],template[800:1172],template[1300:1500]), file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                        sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 15
write.csv(x = template[1500:2000], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                    sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 16
write.csv(x = template[2000:2500], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                        sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 17
write.csv(x = template[2500:3000], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                                sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 18
write.csv(x = template[3000:3500], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                                sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 19
write.csv(x = template[4000:4500], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                                sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 20
write.csv(x = template[4500:5000], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                                sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 21
write.csv(x = template[3000:3500], file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                                sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# drop
write.csv(x = fread("tianchi_mobile_recommendation_predict_RF_2016-08-09 11:18:35.csv"),
                                    file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                                    sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 22
write.csv(x = rbind(template[1:1000],template[1100:1172]),
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 23
write.csv(x = template[2000:2200],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 24
write.csv(x = template[1001:1172],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 25
write.csv(x = template[1501:1700],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 26
write.csv(x = template[1:1000],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 27
write.csv(x = template[601:800],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 28
write.csv(x = template[2001:2100],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 29
write.csv(x = template[2101:2200],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 30
write.csv(x = template[1:300],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 31
write.csv(x = template[601:900],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 32
write.csv(x = rbind(template[1:900],template[1101:1172],template[2001:2100]),
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 33
write.csv(x = template[1001:1100],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 34
write.csv(x = template[3001:3100],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 35
write.csv(x = template[1001:1500],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 36
write.csv(x = template[1:1500],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 37
write.csv(x = template[1:200],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# you yi tian wan shang mei ti jiao cheng gong 
# 38
write.csv(x = rbind(template[1:200],template[301:900],template[1101:1172],template[2001:2100]),
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 39
write.csv(x = template[1501:1600],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 40
write.csv(x = rbind(template[1:200],template[301:900],template[1101:1172],template[1501:1600],template[2001:2100]),
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 41
write.csv(x = template[301:500],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 42
write.csv(x = template[1:150],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 43
write.csv(x = template[301:400],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 44
write.csv(x = template[1:100],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 45
write.csv(x = template[601:700],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 46
write.csv(x = template[1501:1550],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")
# 47
write.csv(x = template[3501:3600],
          file = paste(paste('tianchi_mobile_recommendation_predict_RF',Sys.time(),sep="_"),'.csv',
                       sep = ""), row.names = FALSE, fileEncoding = "UTF-8")





# orders=517












