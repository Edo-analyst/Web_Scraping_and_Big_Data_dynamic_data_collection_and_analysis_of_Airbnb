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
dati$price=NULL  #uso lp
dati$id=NULL
dati$host_id=NULL
dati$house_id=NULL
dati$latitude=NULL
dati$longitude=NULL
dati$id_period=NULL
dati$house_id2=NULL
dati$house_id_num=NULL
dati$neighbourhood_cleansed=NULL

str(dati)

#dati_rid <- dati %>% filter(maximum_nights<=30) 
dati_rid <- dati
rm(dati)
table(dati_rid$city)


##MODELLI NAPOLI---------------------------------------------------------------
city <-dati_rid %>% 
  filter(city == "Naples") %>% 
  select(!city)

set.seed(1)
acaso = sample(1:nrow(city), 0.70*nrow(city))
sss = city[acaso, ]
vvv = city[-acaso,]

#+++++++++++++++++++++++
# Modello Lineare ------------------------
#+++++++++++++++++++++++
model_full <- lm(lp~., data = city)
summary(model_full)
model_step <- step(model_full, direction = "both")
summary(model_step)

p.lm = predict(model_step,vvv)
mspe.lm <- mean((p.lm-vvv$lp)^2)
mspe.lm

#+++++++++++++++++++++++
# Lasso ed elastic net ------------------------
#+++++++++++++++++++++++

#Lasso
x = model.matrix(lp~., data=sss)

mod.lasso=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                    lambda.min.ratio=1e-8)
plot(mod.lasso)

lambda.grid = exp(seq(-7.5, -4, length = 150))

mod.lasso=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                    lambda=lambda.grid)
plot(mod.lasso)

Xv = model.matrix(lp~., data=vvv)

p.lasso = predict(mod.lasso, Xv[,-1], s = "lambda.1se")
mspe.lasso <- mean((p.lasso-vvv$lp)^2)
mspe.lasso


log(mod.lasso$lambda.1se)
log(mod.lasso$lambda.min)  #più piccolo

mod.lasso$lambda.min
mod.lasso$lambda.1se

coef(mod.lasso)

#Lasso adattivo
lm_ad <- lm(lp ~ x[,-1] , data = sss)
coef(lm_ad)
pesi <- 1/abs(coef(lm_ad))
mod.lasso_ad=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                       lambda.min.ratio=1e-8, penalty.factor = pesi[-1] )
plot(mod.lasso_ad)

lambda.grid_ad = exp(seq(-5.5, 1, length = 150))
mod.lasso_ad=cv.glmnet(x[,-1], sss$lp,alpha=1, 
                       lambda=lambda.grid_ad, penalty.factor = pesi[-1] )

plot(mod.lasso_ad)

p.lasso_ad = predict(mod.lasso_ad, Xv[,-1], s = "lambda.1se")
mspe.lasso_ad <- mean((p.lasso_ad-vvv$lp)^2)
mspe.lasso_ad


log(mod.lasso_ad$lambda.1se)
log(mod.lasso_ad$lambda.min)  #più piccolo

mod.lasso_ad$lambda.min
mod.lasso_ad$lambda.1se

coef(mod.lasso_ad)

#elastic net con pesi adattivi
elast_net=cv.glmnet(x[,-1], sss$lp,alpha=0.5, 
                    lambda.min.ratio=1e-8, penalty.factor = pesi[-1] )
plot(elast_net)

lambda.grid_el = exp(seq(-4.5, 2, length = 150))
elast_net=cv.glmnet(x[,-1], sss$lp,alpha=0.5, 
                    lambda=lambda.grid_el, penalty.factor = pesi[-1] )

plot(elast_net)

p.elast = predict(elast_net, Xv[,-1], s = "lambda.1se")
mspe.elast <- mean((p.elast-vvv$lp)^2)
mspe.elast

log(elast_net$lambda.1se)
log(elast_net$lambda.min)  #più piccolo

elast_net$lambda.1se
elast_net$lambda.min

coef(elast_net)

####ALBERI DI REGRESSIONE---------------

library(tree)
# Regolazione: crescita di un albero molto fitto, e poi potatura.
# Puo' essere condotta tramite un insieme di convalida, oppure convalida incrociata.

# Stima di albero molto fitto (tramite mindev e minsize)
m_tree = tree(lp~.,
              data = sss, control = tree.control(nobs = NROW(sss),
                                                 minsize = 2, mindev = 0.0005))
#' control ci dice come controllare la crescita dell'albero, qui uso CV
#' tree.control ci permette di controllare quando avanti vogliamo andare nel far crescere
#' minsize=quanto piccolo deve essere un nodo, num osservazioni minime per dividere un nodo 
#' minsize = 1 -->una foglia per ogni osservazione
#' mindev, quanto R2 spiego e mi fermo quando ne spiego meno di quello che ho indicato

# Chiaramente albero fitto = si fatica a leggere = non utile
plot(m_tree)
text(m_tree, pretty = 4)
#' modello che sovradatta, vogliamo potarlo
#' usiamo CV per potarlo

set.seed(1234)
prune = cv.tree(m_tree, K = 5)
plot(prune)
#' param di regolazione e' la sua dimensione (size)
#' qui la regoliamo tramite CV
str(prune)
#' e' una lista che indicizza rispetto a size alcune misure di errore (mi interessa la devianza)

# Zoom
plot(prune, xlim = c(0,30))
#J = prune$size[which.min(prune$dev)]
J <- rev(prune$size)[which(rev(prune$dev) == min(prune$dev))[1]]
J
abline(v = J, lty = 2, col = 2)

m_tree_b = prune.tree(m_tree, best = J)  #taglia l'albero in corrispondenza di una certa altezza
plot(m_tree_b)
text(m_tree_b, pretty = 4, cex=0.6)

#' particolarita' albero: facile interpretabilita'
#' stima funzioni a gradini (non funz liscie) quindi una relazioni molto ripida e' difficile da stimare
#' connsidera le interazioni, ne considera molte
#' previsioni discrete, non sara' mai un valore diverso dai valori finali delle foglie
head(predict(m_tree_b, vvv))
#' la funz multivariata stimata (funz a gradini) e' molto semplice 
#' puo' fare solo divisioni ricorsive (dopo aver fatto uno split mi concentro su uno sottospazio delle esplicative )
#' l'albero cerca di combattere la maledizione della dimensionalita' con questa restrizione dello spazio
#' ritorna solo l'err di convalida medio, quindi e' difficile usare il metodo CV 1se

p.tree = predict(m_tree_b,vvv)
mspe.tree<- mean((p.tree-vvv$lp)^2)
mspe.tree

#+++++++++++++++++++++++
# Random Forest ------------------------
#+++++++++++++++++++++++

library(ranger)
mod_rf = ranger(lp~., data = city)


# 1] scegliamo il valore minimo di mtry
set.seed(123)
rf_all = lapply(1:12, function(l) ranger(lp~., data = sss, mtry = l))
err_rf = unlist(lapply(rf_all, function(x) x$prediction.error))
plot(err_rf, xlab = "Variabili campionate", ylab = "Errore out-of-bag", type = "l")
mtry_opt <- which.min(err_rf) 
mtry_opt
abline(v = mtry_opt, lty = 2, col = 2)
# Per quel valore di mtry, controlliamo che il numero di alberi
# sia sufficiente
ntr = seq(10,400,by=10)
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

# Importanza delle variabili 
vimp = ranger::importance(alb)

dotchart(vimp[order(vimp)])
# Ordina i valori di importanza delle variabili
vimp_ordered <- vimp[order(vimp, decreasing = T)]

# Carica ggplot2
library(ggplot2)

# Converti vimp_ordered in un data frame
vimp_df <- data.frame(
  Variable = names(vimp_ordered),
  Importanza = as.numeric(vimp_ordered)
)

# Ordina le variabili per avere le più importanti in alto
vimp_df$Variable <- factor(vimp_df$Variable, levels = rev(names(sort(vimp_ordered, decreasing = TRUE))))

# Crea il grafico con nomi più piccoli
ggplot(vimp_df, aes(x = Variable, y = Importanza)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "white") + # Barre con bordo
  theme_minimal() + # Tema pulito
  labs(x = NULL, y = "Importanza") + # Etichette degli assi
  theme(axis.text.x = element_text(size = 12),  # Dimensione etichette asse X
        axis.text.y = element_text(size = 8),   # **Riduci la dimensione dei nomi**
        axis.title = element_text(size = 14)) +
  coord_flip() # Ruota il grafico per barre orizzontali

# Normalizza i valori di importanza
vimp_df$ImportanzaRelativa <- vimp_df$Importanza / max(vimp_df$Importanza)

# Crea il grafico con l'importanza relativa
ggplot(vimp_df, aes(x = Variable, y = ImportanzaRelativa)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "white") + # Barre con bordo
  theme_minimal() + # Tema pulito
  labs(x = NULL, y = "Importanza") + # Etichetta asse y modificata
  theme(axis.text.x = element_text(size = 12),  # Dimensione etichette asse X
        axis.text.y = element_text(size = 8),   # Riduci la dimensione dei nomi
        axis.title = element_text(size = 14)) +
  coord_flip() # Ruota il grafico per barre orizzontali

pr.rf = predict(alb, data = vvv, type = "response") 
mspe.rf = mean( (pr.rf$pred - vvv$lp)^2 )
mspe.rf


#+++++++++++++++++++++++
# Risultati dei modelli ------------------------
#+++++++++++++++++++++++
grep("mspe\\.", ls(), value = T)

errori_or = sapply(grep("mspe\\.", ls(), value = T), get)
errori_or
# Nomi piu' leggibili
nn = names(errori_or)
names(errori_or) = nn

knitr::kable(cbind("MSPE" = errori_or), digits = 5)





