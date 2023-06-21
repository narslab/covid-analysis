#cov-mob TODO:
#- calculate log likelihood of All models
#	- choose highest
#	- plot them on a graph
#- calculate irf, fevd for all models
#
#
#
#rm results/model_loglikelihood.txt
#echo "" > results/model_loglikelihood.txt

library(BGVAR)
library(scatterplot3d)
library(ggplot2)
library(tidyverse)

generateLogLikelihood <- function(models.path) { # models.path = "models"
	files <- list.files(path=models.path, pattern="*.RDS", full.names=TRUE, recursive=FALSE)
	columns = c("model_name","log_likelihood") 
	df = data.frame(matrix(nrow = 0, ncol = length(columns))) 
	colnames(df) = columns
	lapply(files, function(x) {
		model <- readRDS(x)
		model.name <- sub(".RDS","",sub("models/","",x))
		log.likelihood <- logLik(model)
		model.loglik <- paste(model.name, ", log likelihood:", log.likelihood)
		print(model.loglik)
		df_row <- rbind(df, data.frame("model_name" = model.name, "log_likelihood" = log.likelihood))
		df[nrow(df)+1,] <<- c(model.name, log.likelihood)
	})
	df.sorted <- df[df$log_likelihood != "-Inf",] #drop negative infinity values
	df.sorted <- df.sorted[order(df.sorted$log_likelihood,decreasing=TRUE,na.last=TRUE),] #sort in descending order
	write.csv(df.sorted, "results/model_ll.csv", row.names = FALSE)
}

generate3DScatterDataFrame <- function(model.ll) { # model.ll = "results/model_ll.csv"
	df <- read.csv(model.ll)
	lags <- substring(df[,1], first=6)
	x.val <- c()
	y.val <- c()
	z.val <- df$log_likelihood
	lapply(lags, function(x) {
		p <- as.integer(strsplit(x, split="_")[[1]][1])
		q <- as.integer(strsplit(x, split="_")[[1]][2])
		x.val <<- c(x.val,p)
		y.val <<- c(y.val,q)
	})
	df_3d_scatter <- data.frame(x.val, y.val, z.val)
	df_3d_scatter <- df_3d_scatter[order(df_3d_scatter$z.val,decreasing=TRUE, na.last=TRUE),]
	colnames(df_3d_scatter) = c("p_lag","q_lag","log_likelihood")
	write.csv(df_3d_scatter, "results/scatterplot3d_input.csv", row.names = FALSE)
	print(df_3d_scatter)
}

plot_3d_top_n <- function(csv_input,n) { #csv_input = "results/scatterplot3d_input.csv"
	df <- read.csv(csv_input)
	#df$log_likelihood <- as.double(format(df$log_likelihood, scientific = F))
	graphics.off()
	png(filename = "../../figures/model_lag_3d_scatter.png", width = 8, height = 6, units = "in", res = 300)
	my_color <- ifelse(df$log_likelihood > -215000, "red", "black")
	my_color <- head(my_color,n) # determine how many points to display
	s3d <- scatterplot3d(head(df,n),
	        color = my_color,
	        #main="3D scatter plot of model lag and log likelihood",
				  xlab = "p lag",
				  ylab = "q lag",
				  zlab = "log likelihood",
				  lab=c(4,4,4),
				  type="h")
	#my.lm <- lm(df$log_likelihood ~ df$p_lag + df$q_lag) 
	#s3d$plane3d(my.lm, lty.box = "solid")
	dev.off()
	head(df,n)
}
plot_3d_top_n("../../results/scatterplot3d_input.csv",15)

remove_outliers <- function(x, na.rm = TRUE, ...) {
  # Helper function to identify outliers (>75pctl/<25pctl)
  # https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset/4788102#4788102
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

plot_2d_scatter <- function(csv_input) {
  df <- read.csv(csv_input)
  df1 <- df
  df1$log_likelihood <- remove_outliers(df1$log_likelihood)
  df1 <- na.omit(df1)  
  df1 %>%
    mutate(highlight_flag = ifelse(log_likelihood > -215000, T, F)) %>%
    ggplot(aes(x=p_lag, y=log_likelihood)) +
    geom_point(aes(color= highlight_flag)) +
    scale_color_manual(values = c('#595959', 'red'))
  
}

plot_2d_scatter("../../results/scatterplot3d_input.csv")



# model_fcast <- function(x) { 
# 	model <- readRDS(x)
# 	fcast.name <- sub(" ","",paste(sub(".RDS","",sub("models/","",x)),"_forecast.RDS"))
# 	dir.name <- sub(" ","",paste("models/",fcast.name))
# 	print(dir.name)
# 	fcast <- predict(model, n.ahead=20, save.store=TRUE)
# 	saveRDS(fcast,dir.name)
# 	print(paste("Forecast for", sub(".RDS","",sub("models/","",x)),"is complete."))
# 	lps.model <- lps(fcast)
# 	rmse.model <- rmse(fcast)
# 	print(paste("lps:",toString(lps.model)))
# 	print(paste("rmse:",toString(rmse.model)))
# }
# model_fcast("models/model7.RDS")
