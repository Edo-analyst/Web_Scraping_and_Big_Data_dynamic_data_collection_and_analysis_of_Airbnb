library(tidyverse)
library(ggplot2)
library(glmnet)
library(tree)
library(ranger)

#options(scipen = 999)
#options(scipen = 0)
#load("data_modelli.RData")
#dati_def <- dati_def %>% filter(maximum_nights <= 1125) 

load("last_quarter.RData")

dati<-last_quarter
rm(last_quarter)
dati$price=NULL  # use log-price
dati$id=NULL
dati$host_id=NULL
dati$latitude=NULL
dati$longitude=NULL
dati$id_period=NULL
dati$house_id_num=NULL
dati$neighbourhood_cleansed=NULL

str(dati)

#dati_rid <- dati %>% filter(maximum_nights<=30) 
dati_rid <- dati
rm(dati)
table(dati_rid$city)

##MODELS VENICE---------------------------------------------------------------
city <-dati_rid %>% 
  filter(city == "Venice") %>% 
  select(!city)

set.seed(1)
acaso = sample(1:nrow(city), 0.70*nrow(city))
sss = city[acaso, ]
vvv = city[-acaso,]

#+++++++++++++++++++++++
# Linear Model ------------------------
#+++++++++++++++++++++++
model_full <- lm(lp~., data = city)
summary(model_full)
model_step <- step(model_full, direction = "both")
summary(model_step)

p.lm = predict(model_step,vvv)
mspe.lm <- mean((p.lm-vvv$lp)^2)
mspe.lm

#+++++++++++++++++++++++
# Lasso and Elastic Net ------------------------
#+++++++++++++++++++++++

#Lasso
x = model.matrix(lp~., data=sss)

mod.lasso=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                    lambda.min.ratio=1e-8)
plot(mod.lasso)

lambda.grid = exp(seq(-7, -3, length = 150))

mod.lasso=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                    lambda=lambda.grid)
plot(mod.lasso)

Xv = model.matrix(lp~., data=vvv)

p.lasso = predict(mod.lasso, Xv[,-1], s = "lambda.1se")
mspe.lasso <- mean((p.lasso-vvv$lp)^2)
mspe.lasso


log(mod.lasso$lambda.1se)
log(mod.lasso$lambda.min)  # smaller

mod.lasso$lambda.min
mod.lasso$lambda.1se

coef(mod.lasso)

#Adaptive Lasso
lm_ad <- lm(lp ~ x[,-1] , data = sss)
coef(lm_ad)
pesi <- 1/abs(coef(lm_ad))
mod.lasso_ad=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                       lambda.min.ratio=1e-8, penalty.factor = pesi[-1] )
plot(mod.lasso_ad)

lambda.grid_ad = exp(seq(-8, 1, length = 150))
mod.lasso_ad=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                       lambda=lambda.grid_ad, penalty.factor = pesi[-1] )

plot(mod.lasso_ad)

p.lasso_ad = predict(mod.lasso_ad, Xv[,-1], s = "lambda.1se")
mspe.lasso_ad <- mean((p.lasso_ad-vvv$lp)^2)
mspe.lasso_ad


log(mod.lasso_ad$lambda.1se)
log(mod.lasso_ad$lambda.min)  # smaller

mod.lasso_ad$lambda.min
mod.lasso_ad$lambda.1se

coef(mod.lasso_ad)

#Elastic Net with adaptive weights
elast_net=cv.glmnet(x[,-1], sss$lp,alpha=0.5, 
                    lambda.min.ratio=1e-8, penalty.factor = pesi[-1] )
plot(elast_net)

lambda.grid_el = exp(seq(-8, 1, length = 150))
elast_net=cv.glmnet(x[,-1], sss$lp,alpha=0.5, 
                    lambda=lambda.grid_el, penalty.factor = pesi[-1] )

plot(elast_net)

p.elast = predict(elast_net, Xv[,-1], s = "lambda.1se")
mspe.elast <- mean((p.elast-vvv$lp)^2)
mspe.elast

log(elast_net$lambda.1se)
log(elast_net$lambda.min)  # smaller

elast_net$lambda.1se
elast_net$lambda.min

coef(elast_net)

####REGRESSION TREES---------------

library(tree)
# Tuning: grow a very deep tree, then prune it.
# Can be done with a validation set or cross-validation.

# Estimate a very deep tree (using mindev and minsize)
m_tree = tree(lp~.,
              data = sss, control = tree.control(nobs = NROW(sss),
                                                 minsize = 2, mindev = 0.0005))
#' control sets tree growth parameters, here using CV
#' tree.control decides when to stop growing
#' minsize = minimum observations to split a node
#' mindev = stop splitting if R2 improvement is smaller than this

# Deep tree = hard to interpret = not useful
plot(m_tree)
text(m_tree, pretty = 4)
#' model overfits, we want to prune it
#' use CV for pruning

set.seed(1234)
prune = cv.tree(m_tree, K = 5)
plot(prune)
#' tuning parameter is tree size
#' adjust it using CV
str(prune)
#' list indexed by size with error measures (focus on deviance)

# Zoom
plot(prune, xlim = c(0,30))
#J = prune$size[which.min(prune$dev)]
J <- rev(prune$size)[which(rev(prune$dev) == min(prune$dev))[1]]
J
abline(v = J, lty = 2, col = 2)

m_tree_b = prune.tree(m_tree, best = J)  # prune tree at selected size
plot(m_tree_b)
text(m_tree_b, pretty = 4, cex=0.6)

#' Tree features: easy interpretability
#' Estimates step functions (not smooth), steep relationships are hard
#' Considers interactions, many of them
#' Predictions are discrete, limited to leaf values
head(predict(m_tree_b, vvv))
#' Multivariate function is simple 
#' Only recursive splits (split a subspace of variables)
#' Tree helps with curse of dimensionality
#' Only returns mean CV error, 1se CV method difficult to apply

p.tree = predict(m_tree_b,vvv)
mspe.tree<- mean((p.tree-vvv$lp)^2)
mspe.tree

#+++++++++++++++++++++++
# Random Forest ------------------------
#+++++++++++++++++++++++

library(ranger)
mod_rf = ranger(lp~., data = city)


# 1] choose minimum mtry
set.seed(123)
rf_all = lapply(1:12, function(l) ranger(lp~., data = sss, mtry = l))
err_rf = unlist(lapply(rf_all, function(x) x$prediction.error))
plot(err_rf, xlab = "Sampled variables", ylab = "Out-of-bag error", type = "l")
mtry_opt <- which.min(err_rf) 
mtry_opt
abline(v = mtry_opt, lty = 2, col = 2)
# For this mtry, check number of trees
ntr = seq(10,350,by=10)
ntr
set.seed(123)
rf_tree = lapply(ntr, function(l) ranger(lp~., data = sss, mtry = mtry_opt, num.trees = l,
                                         importance = "impurity"))

err_rf = unlist(lapply(rf_tree, function(x) x$prediction.error))
plot(ntr, err_rf, type = "l")

num <- ntr[which.min(err_rf)]
num
abline(v = num, lty = 2, col = 2)

alb <- ranger(lp~., data = sss, mtry = mtry_opt , num.trees = num,
              importance = "impurity")
alb

# Variable importance 
vimp = ranger::importance(alb)

dotchart(vimp[order(vimp)])
# Sort variable importance values
vimp_ordered <- vimp[order(vimp, decreasing = T)]

# Load ggplot2
library(ggplot2)

# Convert vimp_ordered to a data frame
vimp_df <- data.frame(
  Variable = names(vimp_ordered),
  Importanza = as.numeric(vimp_ordered)
)

# Order variables so most important are on top
vimp_df$Variable <- factor(vimp_df$Variable, levels = rev(names(sort(vimp_ordered, decreasing = TRUE))))

# Plot with smaller names
ggplot(vimp_df, aes(x = Variable, y = Importanza)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "white") + # Bars with border
  theme_minimal() + # Clean theme
  labs(x = NULL, y = "Importance") + # Axis labels
  theme(axis.text.x = element_text(size = 12),  # X-axis label size
        axis.text.y = element_text(size = 8),   # Reduce label size
        axis.title = element_text(size = 14)) +
  coord_flip() # Horizontal bars

pr.rf = predict(alb, data = vvv, type = "response") 
mspe.rf = mean( (pr.rf$pred - vvv$lp)^2 )
mspe.rf

#+++++++++++++++++++++++
# Model results ------------------------
#+++++++++++++++++++++++
grep("mspe\\.", ls(), value = T)

errori_or = sapply(grep("mspe\\.", ls(), value = T), get)
errori_or
# More readable names
nn = names(errori_or)
names(errori_or) = nn

knitr::kable(cbind("MSPE" = errori_or), digits = 5)

