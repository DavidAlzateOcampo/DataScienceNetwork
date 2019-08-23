library(openxlsx);
library(rvest);
library(xml2);
library(tseries);
library(dplyr);
library(ggplot2);
library(quantmod);
library(directlabels);

library(stringr)
library(tidyr)

getModels<-function(X
                    ,Xi
                    ,Y
                    ,cantidadDias=90
                    ,horizonDias=15
                    ,stockName = 'GM'){
  library(caret)
  library(doParallel)
  no_cores <- detectCores() - 1  
  registerDoParallel(cores=no_cores) 
  
  initialWindowVal<-length(Y)-cantidadDias
  
  myTimeControl <- trainControl(method = "timeslice"
                                ,initialWindow = initialWindowVal
                                ,horizon = horizonDias
                                ,fixedWindow = FALSE
                                ,allowParallel = TRUE
                                ,classProbs=TRUE
                                ,summaryFunction = twoClassSummary
  )
  tuneLength.num <- 10
  library(caTools)
  LogitBoost.mod <- caret::train(x=X
                                 ,y=Y
                                 ,method = 'LogitBoost'
                                 ,family = "gaussian"
                                 ,metric = "ROC"
                                 ,preProc = c("center", "scale")
                                 ,trControl = myTimeControl
                                 ,tuneLength=tuneLength.num
  )
  plot(LogitBoost.mod)
  LogitBoost.predicted.value <- predict(LogitBoost.mod,newdata=Xi)
  LogitBoost.predicted.value
  
  #0.58
  library(kernlab)
  svmRadial.mod <- caret::train(x=X
                                ,y=Y
                                ,method = 'svmRadial'
                                ,family = "gaussian"
                                ,metric = "ROC"
                                ,preProc = c("center", "scale")
                                ,trControl = myTimeControl
                                ,tuneLength=tuneLength.num
  )
  plot(svmRadial.mod)
  svmRadial.predicted.value <- predict(svmRadial.mod,newdata=Xi)
  svmRadial.predicted.value
  
  #0.61
  cforest.mod<-caret::train(x=X
                            ,y=Y
                            ,method = 'cforest'
                            ,metric = "ROC"
                            ,preProc = c("center", "scale")
                            ,trControl = myTimeControl
                            ,tuneLength=tuneLength.num
  )
  
  plot(cforest.mod)
  cforest.predicted.value <- predict(cforest.mod,newdata=Xi)
  cforest.predicted.value
  
  
  library(kernlab)
  gbm.mod <- caret::train(x=X
                          ,y=Y
                          ,method = 'gbm'
                          ,metric = "ROC"
                          ,preProc = c("center", "scale")
                          ,trControl = myTimeControl
                          ,tuneLength=tuneLength.num
  )
  plot(gbm.mod)
  gbm.mod
  gbm.predicted.value <- predict(gbm.mod,newdata=Xi)
  gbm.predicted.value
  
  
  LogitBoost.predicted.value <- predict(LogitBoost.mod,newdata=Xi)
  TMP<-LogitBoost.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  LogitBoost.ROC<-TMP[1,c("ROC")]
  LogitBoost.Sens<-TMP[1,c("Sens")]
  LogitBoost.Spec<-TMP[1,c("Spec")]
  
  
  svmRadial.predicted.value  <- predict(svmRadial.mod,newdata=Xi)
  TMP<-svmRadial.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  svmRadial.ROC<-TMP[1,c("ROC")]
  svmRadial.Sens<-TMP[1,c("Sens")]
  svmRadial.Spec<-TMP[1,c("Spec")]
  
  cforest.predicted.value    <- predict(cforest.mod,newdata=Xi)
  TMP<-cforest.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  cforest.ROC<-TMP[1,c("ROC")]
  cforest.Sens<-TMP[1,c("Sens")]
  cforest.Spec<-TMP[1,c("Spec")]
  
  gbm.predicted.value        <- predict(gbm.mod,newdata=Xi)
  TMP<-gbm.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  gbm.ROC<-TMP[1,c("ROC")]
  gbm.Sens<-TMP[1,c("Sens")]
  gbm.Spec<-TMP[1,c("Spec")]
  
  predictedDF     = data.frame(
    stockName       = stockName  
    ,LogitBoost      = as.character(LogitBoost.predicted.value)
    ,LogitBoost.ROC  = LogitBoost.ROC
    ,LogitBoost.Sens = LogitBoost.Sens
    ,LogitBoost.Spec = LogitBoost.Spec
    
    ,svmRadial       = as.character(svmRadial.predicted.value)
    ,svmRadial.ROC   = svmRadial.ROC
    ,svmRadial.Sens  = svmRadial.Sens
    ,svmRadial.Spec  = svmRadial.Spec
    
    ,cforest         = as.character(cforest.predicted.value)
    ,cforest.ROC     = cforest.ROC
    ,cforest.Sens    = cforest.Sens
    ,cforest.Spec    = cforest.Spec    
    
    ,gbm             = as.character(gbm.predicted.value)
    ,gbm.ROC         = gbm.ROC
    ,gbm.Sens        = gbm.Sens
    ,gbm.Spec        = gbm.Spec
  )
  
  all<-list(LogitBoost.mod=LogitBoost.mod
            ,svmRadial.mod =svmRadial.mod
            ,cforest.mod   =cforest.mod
            ,gbm.mod       =gbm.mod
            ,predicted     =predictedDF          
  )
  return(all)
  
}

getCandle<-function(DF){
  l=length(DF$Close) # Numero de Datos
  for (i in 1:l){
    if(is.na(DF$Close[i])|is.na(DF$Open[i])){
      DF$Candle[i] = -1;
    }else{
      if(DF$Close[i] > DF$Open[i]){
        DF$Candle[i] = 1;
      } else {
        DF$Candle[i] = 0;
      }
    }
  }
  return(DF)
}

drops <- c("Fecha","stockName","Candle","CandleL1","Candle.Predicted")
cantidadDias<-60
horizonDias<-5
optimus<-c("Open","High","Low","Close","Volume","FechaNum",
           "AdjustedL1","WeightedClose","AvgPRice",
           "RetDL1","RetD.HighL1","RetD.LowL1","IntradayRet",
           "WeeklyRet","Vol.ema5","Price.Lag1.Vol","ema5",
           "ema5vs20","breakema5vs20","dema5","evwma5",
           "zlema5","AverageRet","macd.ema","macd.sma",
           "bb20","RSI","adx","cci","atr","ad","cmf",
           "obv","stochOSC","stochWPR","ult.osc")
getStocks<-function(stockName
                    ,ini_d="2012-01-01"
                    ,end_d="2017-06-23"
                    ,lastDays=45){
  require(stringr)
  require(fTrading)
  require(quantmod)
  con <- url("https://finance.yahoo.com")
  if(!inherits(try(open(con), silent = TRUE), "try-error")) {
    close(con)
    yahoo_p<-"yahoo";
    by_day<-"d";
    getSymbols(stockName,from=as.Date("2012-01-01"),to=as.Date("2017-06-30"))
    switch(stockName,
           GM   ={ gspc<-GM   },
           F    ={ gspc<-F    },
           AAPL ={ gspc<-AAPL },
           AMZN ={ gspc<-AMZN },
           C    ={ gspc<-C    },
           COP  ={ gspc<-COP  },
           GE   ={ gspc<-GE   },
           XOM  ={ gspc<-XOM  },
           GM   ={ gspc<-GM   },
           GOOG ={ gspc<-GOOG },
           HAL  ={ gspc<-HAL  },
           JNJ  ={ gspc<-JNJ  },
           JPM  ={ gspc<-JPM  },
           PG   ={ gspc<-PG   },
           stop("There was not assigned an stock"))
    
    names(gspc)<-c("Open"
                   ,"High"
                   ,"Low"
                   ,"Close"
                   ,"Volume"
                   ,"Adjusted")
    
    gspc<-getCandle(gspc)
    gspc$CandleL1 <- lag.xts(gspc$Candle, k = 1)
    gspc$Candle.Predicted <- lag.xts(gspc$Candle, k = -1)
    
    gspc$AdjustedL1 <- lag.xts(gspc$Adjusted, k = 1)
    gspc$AdjustedL2 <- lag.xts(gspc$Adjusted, k = 2)
    gspc$AdjustedL3 <- lag.xts(gspc$Adjusted, k = 3)
    gspc$AdjustedL4 <- lag.xts(gspc$Adjusted, k = 4)
    
    gspc$AvgPRice <- (gspc$Open + gspc$Low + gspc$High + gspc$Close)/4  # weighted close http://www.fmlabs.com/reference/default.htm?url=DI.htm
    gspc$WeightedClose <- (gspc$Low + gspc$High + gspc$Close*2)/4
    
    
    gspc$RetD <- dailyReturn(gspc$Adjusted)
    gspc$RetDL1 <- lag(gspc$RetD, k = 1)
    gspc$RetDL2 <- lag(gspc$RetD, k = 2)
    gspc$RetDL3 <- lag(gspc$RetD, k = 3)
    gspc$RetDL4 <- lag(gspc$RetD, k = 4)
    gspc$RetD.High <- dailyReturn(gspc$High)
    gspc$RetD.HighL1 <- lag(gspc$RetD.High, k = 1)
    gspc$RetD.HighL2 <- lag(gspc$RetD.High, k = 2)
    gspc$RetD.HighL3 <- lag(gspc$RetD.High, k = 3)
    gspc$RetD.HighL4 <- lag(gspc$RetD.High, k = 4)
    gspc$RetD.Low <- dailyReturn(gspc$Low)
    gspc$RetD.LowL1 <- lag(gspc$RetD.Low, k = 1)
    gspc$RetD.LowL2 <- lag(gspc$RetD.Low, k = 2)
    gspc$RetD.LowL3 <- lag(gspc$RetD.Low, k = 3)
    gspc$RetD.LowL4 <- lag(gspc$RetD.Low, k = 4)
    gspc$IntradayRet <- (gspc$Close -  gspc$Open) / gspc$Open 
    gspc$WeeklyRet <- (gspc$Close -  lag(gspc$Open, k = 5)) / lag(gspc$Open, k = 5)
    gspc$AverageRet <- (EMA(gspc[,"Close"], 5) - EMA(gspc[,"Open"], 5))/EMA(gspc[,"Open"], 5)
    
    gspc$Vol.ema20 <- EMA(gspc[,"Volume"], 20) # Promedio movil de volumen
    gspc$Vol.ema5 <- EMA(gspc[,"Volume"], 5) # Promedio movil de volumen
    gspc$Price.Lag1.Vol <- (lag(gspc$Volume,k=1)/lag(gspc$Vol.ema5, k=1)) # Volumen del d?a anterior sobre el promedio de los ultimos 5 dias
    
    gspc$ema20 <- EMA(gspc[,"Adjusted"], 20)
    gspc$ema5 <- EMA(gspc[,"Adjusted"], 5)
    gspc$ema5vs20 <- ifelse((EMA(gspc[,"Adjusted"], 5) > EMA(gspc[,"Adjusted"], 20)),1,0)
    gspc$breakema5vs20 <- gspc$ema5vs20 - lag(gspc$ema5vs20,k=1)
    
    gspc$dema20 <- DEMA(gspc[,"Adjusted"], 20)
    gspc$dema5 <- DEMA(gspc[,"Adjusted"], 5)
    gspc$evwma20 <- EVWMA(gspc[,"Adjusted"], gspc[,"Volume"], 20)
    gspc$evwma5 <- EVWMA(gspc[,"Adjusted"], gspc[,"Volume"], 5)
    gspc$zlema20 <- ZLEMA(gspc[,"Adjusted"], 20)
    gspc$zlema5 <- ZLEMA(gspc[,"Adjusted"], 5)
    gspc$alma <- ALMA(gspc[,"Adjusted"])
    gspc$hma <- HMA(gspc[,"Adjusted"])
    
    gspc$ema20V <- gspc$ema20 / gspc$Adjusted 
    gspc$dema20V <- gspc$dema20 / gspc$Adjusted
    gspc$evwma20V <- gspc$evwma20 / gspc$Adjusted
    gspc$zlema20V <- gspc$zlema20 / gspc$Adjusted
    gspc$alma <- gspc$alma / gspc$Adjusted
    gspc$hma <-  gspc$hma / gspc$Adjusted
    
    gspc$macd.ema <- MACD(gspc[,"Adjusted"], nFast=12, nSlow=26, nSig=9, maType="EMA") # MACD (Moving Average Convergence Divergence) 
    gspc$macd.sma = MACD(gspc$Adjusted, nFast=12, nSlow=26, nSig=9, maType="SMA")
    
    gspc$bb20 = BBands(gspc$Adjusted, sd=2.0) # Bollinger Bands
    
    gspc$bb_dn <- gspc$dn / gspc$Adjusted  
    gspc$bb_mavg <- gspc$mavg / gspc$Adjusted  
    gspc$bb_up <- gspc$up / gspc$Adjusted
    
    gspc$RSI = RSI(gspc$Adjusted, n=14)# RSI - Relative Strength Indicator Bollinger Bands
    gspc$adx <- ADX(gspc[,c("High","Low","Close")]) # Chaikin Accumulation / Distribution
    gspc$cci <- CCI(gspc[,c("High","Low","Close")])# Commodity Channel Index
    gspc$atr <- ATR(gspc[,c("High","Low","Close")], n=14) # Average True Range
    gspc$ad <- chaikinAD(gspc[,c("High","Low","Close")], gspc[,"Volume"]) # Chaikin Accumulation / Distribution
    gspc$cmf <- CMF(gspc[,c("High","Low","Close")], gspc[,"Volume"]) # Chaikin Money Flow
    gspc$obv <- OBV(gspc[,c("Adjusted")], gspc[,"Volume"]) # On Balance Volume (OBV)
    gspc$stochOSC <- stoch(gspc[,c("High","Low","Close")])# Stochastic Oscillator / Stochastic Momentum Index
    gspc$stochWPR <- WPR(gspc[,c("High","Low","Close")])
    gspc$ult.osc <- ultimateOscillator(gspc[,c("High","Low","Close")])# The Ultimate Oscillator
    
    tmp<-ADX(gspc,10)
    gspc$DIp <- tmp$DIp
    gspc$DIn <- tmp$DIn
    gspc$DX <- tmp$DX
    gspc$ADX <- tmp$ADX
    rm(tmp)
    gspcDF<-data.frame(gspc,stockName=stockName)
    gspcDF<- cbind(gspcDF
                   ,StopLossBUY  =(gspcDF$High-gspcDF$Open)/mean(gspcDF$Close,na.rm=T)
                   ,StopLossSELL =(gspcDF$Open-gspcDF$Low)/mean(gspcDF$Close,na.rm=T)
                   ,maxDiff = (gspcDF$High-gspcDF$Low)/mean(gspcDF$Close,na.rm=T)
                   ,Rentabilidad = (gspcDF$Open-gspcDF$Close)/mean(gspcDF$Close,na.rm=T) )
    
    gspcDF <- add_rownames(gspcDF, "Fecha")
    gspcDF$Fecha<-as.Date(gspcDF$Fecha)
    gspcDF$FechaNum<-as.numeric(gspcDF$Fecha)
    iniDays<-dim(gspcDF)[1]-lastDays
    endDays<-dim(gspcDF)[1]
    gspcDF_End<-gspcDF[iniDays:endDays,]
    
    summaryTmp<-as.data.frame(summary(gspcDF_End))
    summaryTmp$Var1<-stockName
    summaryTmp<-separate(summaryTmp
                         ,Freq
                         ,into = c("statType", "value")
                         ,sep = ":")
    all<-list(DF=gspcDF,summaryLastDays=summaryTmp)
  }
  return(all)
}
# Se cargan los modelos guardados de las variables anteriores
  load("~/Downloads/Models/PGtmp.Rda")
  load("~/Downloads/Models/JPM2017-06-19 01:49:09.Rda")
  load("~/Downloads/Models/C2017-06-19 01_46_36.Rda")
  load("~/Downloads/Models/JNJ2017-06-19 00_59_46.Rda")
  load("~/Downloads/Models/GM2017-06-19 01:49:09.Rda")
  load("~/Downloads/Models/XOM2017-06-19 00_11_07.Rda")
  load("~/Downloads/Models/GE2017-06-18 23_27_03.Rda")
  load("~/Downloads/Models/GOOG2017-06-18 22_33_27.Rda")
  load("~/Downloads/Models/HAL2017-06-18 21_46_29.Rda")
  load("~/Downloads/Models/COP2017-06-18 21_02_07.Rda")
  load("~/Downloads/Models/AAPL2017-06-18 19_10_48.Rda")
  load("~/Downloads/Models/AMZN2017-06-18 20_20_18.Rda")

getDailyValues<-function(obj_ALL,stockName,dateObj){
  
  optimus<-c("Open","High","Low","Close","Volume","FechaNum",
             "AdjustedL1","WeightedClose","AvgPRice",
             "RetDL1","RetD.HighL1","RetD.LowL1","IntradayRet",
             "WeeklyRet","Vol.ema5","Price.Lag1.Vol","ema5",
             "ema5vs20","breakema5vs20","dema5","evwma5",
             "zlema5","AverageRet","macd.ema","macd.sma",
             "bb20","RSI","adx","cci","atr","ad","cmf",
             "obv","stochOSC","stochWPR","ult.osc")
  
  numericDay  <- as.numeric(dateObj)
  allSTOCK    <- getStocks(stockName)
  stockDF     <- allSTOCK$DF
  stockUnique <- stockDF[stockDF$FechaNum == numericDay,]
  
  Xi<-stockUnique[,optimus]
  LogitBoost.mod  <- obj_ALL$LogitBoost.mod
  svmRadial.mod   <- obj_ALL$svmRadial.mod
  cforest.mod     <- obj_ALL$cforest.mod
  gbm.mod         <- obj_ALL$gbm.mod
  
  LogitBoost.predicted.value <- predict(LogitBoost.mod,newdata=Xi)
  TMP<-LogitBoost.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  LogitBoost.ROC<-TMP[1,c("ROC")]
  LogitBoost.Sens<-TMP[1,c("Sens")]
  LogitBoost.Spec<-TMP[1,c("Spec")]
  
  
  svmRadial.predicted.value  <- predict(svmRadial.mod,newdata=Xi)
  TMP<-svmRadial.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  svmRadial.ROC<-TMP[1,c("ROC")]
  svmRadial.Sens<-TMP[1,c("Sens")]
  svmRadial.Spec<-TMP[1,c("Spec")]
  
  cforest.predicted.value    <- predict(cforest.mod,newdata=Xi)
  TMP<-cforest.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  cforest.ROC<-TMP[1,c("ROC")]
  cforest.Sens<-TMP[1,c("Sens")]
  cforest.Spec<-TMP[1,c("Spec")]
  
  gbm.predicted.value        <- predict(gbm.mod,newdata=Xi)
  TMP<-gbm.mod$results; 
  TMP<-TMP[with(TMP, order(-ROC)), ];
  gbm.ROC<-TMP[1,c("ROC")]
  gbm.Sens<-TMP[1,c("Sens")]
  gbm.Spec<-TMP[1,c("Spec")]
  
  predictedDF     = data.frame(
    stockName       = stockName  
    ,LogitBoost      = as.character(LogitBoost.predicted.value)
    ,LogitBoost.ROC  = LogitBoost.ROC
    ,LogitBoost.Sens = LogitBoost.Sens
    ,LogitBoost.Spec = LogitBoost.Spec
    
    ,svmRadial       = as.character(svmRadial.predicted.value)
    ,svmRadial.ROC   = svmRadial.ROC
    ,svmRadial.Sens  = svmRadial.Sens
    ,svmRadial.Spec  = svmRadial.Spec
    
    ,cforest         = as.character(cforest.predicted.value)
    ,cforest.ROC     = cforest.ROC
    ,cforest.Sens    = cforest.Sens
    ,cforest.Spec    = cforest.Spec    
    
    ,gbm             = as.character(gbm.predicted.value)
    ,gbm.ROC         = gbm.ROC
    ,gbm.Sens        = gbm.Sens
    ,gbm.Spec        = gbm.Spec
  )
  return(predictedDF)
}
# Dia a predecir
today<-as.Date("2017-06-23")
rm(tmpDF.pred)
tmpDF.pred<-getDailyValues(JPM_All,"JPM",today)
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(PG_All,"PG",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(C_All,"C",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(JNJ_All,"JNJ",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(GM_All,"GM",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(XOM_All,"XOM",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(GE_All,"GE",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(GOOG_All,"GOOG",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(HAL_All,"HAL",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(COP_All,"COP",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(AAPL_All,"AAPL",today))
tmpDF.pred<-rbind(tmpDF.pred,getDailyValues(AMZN_All,"AMZN",today))


write.csv(tmpDF.pred,
file=paste0("All.Analysis."
             ,as.character(Sys.time())
             ,".csv"))
