library(BGVAR)
data <- read.csv('mobility/ cases_mobility_activity.csv')
model.1<-bgvar(Data=data,
                    W=W.trade0012,
                    saves=100,
                    burns=100,
                    plag=1,
                    prior="NG",
                    hyperpara=NULL, 
                    SV=TRUE,
                    thin=1,
                    trend=TRUE,
                    h=0,
                    save.country.store=FALSE,
                    multithread=FALSE,
                    eigen=1.05
                    )
