.libPaths()
.libPaths("C:/Program Files/R/R-3.6.1/library")


Packages <- c('readxl', vars','AER','sarima','dynlm','forecast','readxl','stargazer','scales','quantmod','urca','tseries','TTR','dplyr','bvartools','rsample','mda','confidence','ggplot2','fUnitRoots')
lapply(Packages, library, character.only = TRUE)




Arrival_stats2 <- read_excel("H:/Arrival stats2.xlsx")
View(Arrival_stats2)
Arrival_stats2$Cov19_Interventions<-as.factor(Arrival_stats2$Cov19_Interventions)

window <- 12
season_duration <- 12

# Converting a data.frame into ts
Macau_Arrivals_time_series = ts(Arrival_stats2[, c(2, 5, 7, 9#:29
											#,33:36
											)
								],frequency = season_duration, start = c(2010, 7))
   
                               
#test stationarity around a zero-mean with no trend
adf0<-ur.df(Macau_Arrivals_time_series[,"Guangdong"],type="none",lag=1 #,selectlags = c("Fixed", "AIC", "BIC")
			)

test<-ur.df(Macau_Arrivals_time_series[,"Guangdong"],type="trend",lag=0 #,selectlags = c("Fixed", "AIC", "BIC")
			)
summary(test)			
#test stationarity around a non-zero mean with no trend
adf1 <- ur.df(Macau_Arrivals_time_series[,"Guangdong"],type="trend",lag=2 #,selectlags = c("Fixed", "AIC", "BIC")
			)
#test stationarity around a trend with an intercept
adf2<-ur.df(Macau_Arrivals_time_series[,"Guangdong"],type="drift",lag=1 #,selectlags = c("Fixed", "AIC", "BIC")
			)
summary(adf1)
summary(adf2)
                                
                                
                                #var_model <- vars::VAR(time_series)
                                
                                var_model <- vars::VAR(BoxCox(time_series, lambda = 0))
                                
                                summary(var_model)
                                
                                var_model %>%
                                  forecast::forecast(window) %>%
                                  forecast::autoplot() +
                                  #scale_y_continuous(limits = c(0, 100)) +
                                  theme(panel.grid = element_line(linetype = "dashed")) + facet_wrap( ~
                                                                                                        series, scales = "fixed", ncol = 2)
                                
                                
                                #ret <- var_model %>%
                                #  forecast::forecast(window)
                                #x<-confidence:::backtransform(as.data.frame(ret$forecast$Guangdong), type = c( "log"))
                                
                                #x2<-as.data.frame(forecast::forecast(var_model, h = 12)$forecast$Guangdong
                                #                 , year = trunc(time(forecast::forecast(var_model, h = 12,level = c(80, 95))$forecast$Guangdong)))
                                
                                
                                #backtransformation to get forecast numbers
                                confidence:::backtransform(as.data.frame(
                                  forecast::forecast(var_model, h = 12, level = c(80, 95))$forecast$Guangdong
                                ),
                                type = c("log"))
                                
                                
                                #plot only forecast values
                                autoplot(ts(
                                  confidence:::backtransform(
                                    as.data.frame(
                                      forecast::forecast(var_model, h = 12, level = c(80, 95))$forecast$Guangdong
                                    ),
                                    type = c("log")
                                  ),
                                  frequency = season_duration,
                                  start = c(2020, 6)
                                ))
                                
                                
                                x <- autoplot(#confidence:::backtransform(
                                  forecast::forecast(var_model, h = 12)$forecast$Guangdong
                                  							, type = c("log"))
                                  							#)
                                  
                                  
                                  x +
                                    xlab("Time (year)") + ylab("Arrival GuangDong") +
                                    theme_bw() +
                                    theme(plot.title = element_text(hjust = 0.5)) +
                                    scale_x_continuous(breaks = seq(from = 2010, to =  2020, by = 1))
