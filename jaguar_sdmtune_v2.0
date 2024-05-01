#Jaguar - Script v2.0 - 

library (SDMtune)
library (terra)
library (enmSdmX)
library (flexsdm)
library (usdm)
library (ENMeval)
library (Fangorn)
library (zeallot)

# Import predictor raster files
predictor_filenames = list.files("jaguar_variables", pattern = ".tif", all.files = T, full.names = T)
predictors = rast (predictor_filenames)

plot(predictors)

# Actual modelling #

# Import species presence data
jaguar <- read.csv(file.choose(), header = TRUE, sep = ",")

# Read in background points from CSV file
bg = read.csv(file.choose(), header = TRUE, sep = ",")

# Prepare species distribution data
data <- prepareSWD(species = "Panthera onca", p = jaguar, 
                   a = bg, env = predictors)


# Split data in training, validation and testing datasets
c(train, test) %<-% trainValTest(data,
                                    test = 0.3,
                                    only_presence = TRUE, 
                                    seed = 7)


#Blocos espaciais
jaguar_folds <- get.block(occs = train@coords[train@pa == 1, ],
                          bg = train@coords[train@pa == 0, ],
                          orientation= "lat_lon")

# Train model
model <- train(method = "Maxent", data = train,  folds = jaguar_folds, reg = 1, iter = 500, seed = 7)

#Optimize model hyperparameters
h <- list(reg = seq(0.1, 5, 0.1), fc = c("l","lq", "lqph"))
op <- optimizeModel(model, hypers = h, metric = "tss", pop = 100, gen = 5, seed = 7)

# List the models with the best performance
op@results
op@results[order(-op@results$test_TSS), ]

# Replace 'your_file_name.csv' with the desired file name
write.csv(op@results[order(op@results$test_TSS), ], file = 'optimize_modelos_7.5.csv', row.names = FALSE)

#Extract best models (SDMmodel)

#Best -TSS

lq_0.6 <- op@models[[1]]
lq_3.6_60 <- combineCV(lq_3.6_60)


#Best LQ

#model <- op@models[[5]]
model <- combineCV(model)

#Best l

#l_3.8_60 <- op@models[[74]]
#l_3.8_60 <- combineCV(l_3.8_60)


#List models
models <- list (lq_3.6_60 = lq_3.6_60,
                model = model)

#Automate analysis for the 3 bests models
palantiris(op, test, predictors, jaguar, bg, "maxSSS", remove_prediction = TRUE , distance = "7.5")

rivendell(op, test, predictors, jaguar, bg, "maxSSS", remove_prediction = TRUE , identifier = "20")
