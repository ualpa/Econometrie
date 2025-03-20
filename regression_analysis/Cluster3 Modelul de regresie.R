#Cluster 3 - Importanța caracteristicilor:
#Healthy life expectancy         0.407265
#Freedom to make life choices    0.225509
#Social support                  0.163168


# Cuprins:
#   1. Regresia simpla
#       Bonitatea modelului (R-squared si R-squared ajustat)
#       Calitatea ajustarii (criteriile informationale Akaike, Schwarz, Hannan-Quinn)
#   2. Testul t pentru semnificatia coeficientilor
#   3. Testul F pentru semnificatia unui coeficient
#   4. Ipoteza de normalitate a reziduurilor
#       Testul Jarque-Bera pentru normalitate
#       Graficul 'Residuals vs Fitted'
#       Graficul 'Q-Q plot'
#       Histograma reziduurilor
#       Asimetria
#       Boltirea
#       Boxplot
#       Distanta Cook
#   5. Analiza grafica a heteroscedasticitatii
#   6. Teste de heteroscedasticitate
#       Testul Breusch-Pagan
#       Testul White
#   7. Ipoteza de non-autocorelare
#       Metoda grafica de identificare a autocorelarii (ACF)
#       Testul Durbin-Watson
#       Testul Breusch-Godfrey
#       Corectarea non-autocorelarii
#       Retestarea ipotezei
#   8. Ipotezele modelului de regresie
#       Modelul este liniar in parametri
#       Nr de observatii > nr variabile independente
#       Modelul de regresie este corect specificat    
#       Variabilitatea in x este pozitiva
#       Media reziduurilor este 0
#       Testare multicoliniaritate
#       Reziduurile nu sunt corelate cu variabilele independente
#       Reziduurile sunt homoscedastice
#       Reziduurile nu sunt autocorelate
#       Reziduurile sunt normal distribuite
#   9. Predictie, indicatori de acuratete, variabile dummy
#  10. Metode de regularizare
#       Regresia Ridge
#       Regresia LASSO
#       Regresia Elastic Net

# Setarea mediului de lucru
rm(list = ls()) 
directory <-"C:/Users/gladi/OneDrive/Desktop/ProiectEconometrie/"
install.packages("mltools")
install.packages("MLmetrics")

# Incarcarea librariilor
library(tidyverse)
library(caret) #pachetul caret este folosit pentru a facilita antrenarea si evaluarea modelelor de invatare automata. 
#Acesta ofera o interfata unificata pentru o gama larga de algoritmi de invatare automata si metode de selectie si evaluare a modelelor
library(mltools) #cuprinde o colectie de functii utile pentru invatarea automata, in sepcial in analiza exploratorie a datelor
library(MLmetrics) #libraria MLmetrics include o colectie de functii utilizate pentru evaluarea performantei modelelor de regresie sau clasificare

# Instalarea si activarea pachetelor
PackageNames <- c("tidyverse", "stargazer", "magrittr", "lmtest", "sandwich", "whitestrap", 
                  "car", "tseries", "olsrr", "moments","ICglm","ggplot2","DataCombine")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}


########################################################### REGRESIA MULTIPLA ########################################################### 
# Citirea datelor
ladderScore <- read.csv(paste0(directory, "Cluster3_updated.csv"))

# Afisarea datelor
ladderScore %>% 
  select(Ladder.score, Log.GDP.per.capita, Social.support, Healthy.life.expectancy,Freedom.to.make.life.choices,Generosity,Perceptions.of.corruption,lLadder.score, lSocial.support, lHealthy.life.expectancy,lFreedom.to.make.life.choices,lGenerosity,lPerceptions.of.corruption)

# Regresia multipla
# wage = beta0 + beta1*educ + beta2*exper + u
model_simple <- lm(Ladder.score ~ Social.support,ladderScore)
summary(model_simple)


# Bonitatea modelului (R-squared si R-squared ajustat) ----------------------------------------------------------------------------------------------------------------------------------------------------------------
# Regresie multipla cu 3 regresori
summary(model_simple)
summary(model_simple)$r.squared
# Aproximativ 37.02% din variatia variabilei dependente este explicata de variabilele independente din model
summary(model_simple)$adj.r.squared
# Modelul explică aproximativ 33.31% din variația variabilei dependente


# Calitatea ajustarii -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Akaike (AIC), Schwarz (BIC) si Hannan-Quinn (HQC) 

# Criteriul Akaike
aic <- AIC(model_simple)
cat("AIC (Akaike):", aic, "/n")

# Criteriul Schwarz 
bic <- BIC(model_simple)
cat("BIC (Schwarz):", bic, "/n")

# Criteriul Hannan - Quinn 
hqic <- HQIC(model_simple)
cat("HQIC (Hannan-Quinn):", hqic, "/n")

########################################################### TESTUL T ########################################################### 
# Testul t 
# Testare de ipoteze
# Ipotezele testului t
# H0: miu = miu0
# H1: miu diferit de miu0

(coefficient <- coef(model_simple)["Social.support"])

# Afisarea erorii standard
(se <- vcov(model_simple) %>% 
    diag %>% 
    sqrt %>% 
    .["Social.support"])

# Calcularea testului t = coeficient/se
(tstat <- coefficient/se)

# Gradele de libertate (n-k-1)
(df_r <- model_simple$df.residual)

qt(p = 0.975, df = df_r, lower.tail = TRUE)

#t statistic = 3.160868  > t critic = 2.109816 deci respingem ipoteza nula si acceptam ipoteza alternativa ca acest coeficient este semnificativ statistic pentru un nivel de incredere de 95%

########################################################### TESTUL F ########################################################### 
# Model restricționat: doar interceptul
model_restricted <- lm(Ladder.score ~ 1, data = ladderScore)

SSR_r <- sum(resid(model_restricted)^2)
SSR_ur <- sum(resid(model_simple)^2)

# Grade de libertate
q <- length(coef(model_simple)) - 1 
df_ur <- model_simple$df.residual    

# Calculul F-statistic
F_stat <- ((SSR_r - SSR_ur) / q) / (SSR_ur / df_ur)
F_stat

# Valoare critică F pentru nivel de semnificație de 5%
F_critical <- qf(0.95, df1 = q, df2 = df_ur)
F_critical

# p-value pentru F-test
F_pvalue <- pf(F_stat, df1 = q, df2 = df_ur, lower.tail = FALSE)
F_pvalue

# p-value =  0.005708174 este mult mai mic decat pragul de 0.05 deci confirma respingerea ipotezei nule
#Atfel, modelul complet este semnificativ mai bun decat modelul restrictionat

####################################################### IPOTEZA DE NORMALITATE A REZIDUURILOR #######################################################
# Testul Jarque-Bera pentru normalitate ------------------------------------------------------------------------------------------------------------------------
jarque.bera.test(ladderScore$Ladder.score)
# p-value = 4.506e-08 este mult mai mic decat pragul de 0.05 deci respingem ipoteza nula ca datele urmeaza o distributie normala => va fi nevoie de o transformare a datelor pentru a le normaliza

# Graficul 'Residuals vs Fitted' ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(model_simple)
# Linia rosie este aproximativ dreapta si aproape de zero, atunci se poate presupune ca reziduurile sunt normal distribuite 

# Graficul 'Q-Q plot'---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(model_simple)
# Avem cateva puncte extreme deci inseamna ca distributia nu este una normala (11, 64, 65)

# Histograma reziduurilor ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ols_plot_resid_hist(model_simple)

ladderScore %<>% mutate(uhat = resid(model_simple))
ggplot(data = ladderScore) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))
# Histograma arata asimetrie la stanga deoarece observam valori extreme in jurul valorii de -1, -0.5, 0.5

#Asimetria ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
skewness(ladderScore$uhat)
# Valoarea negativa = 1.275855 negative ne indica faptul ca distributia 
# noastra este centrata la stanga (asimetria)

# Boltirea (kurtosis) ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
kurtosis(ladderScore$uhat)
# kurtosis = 4.547875 => distributie leptocurtica
# In urma testarii coeficientului de asimetrie si a boltirii putem trage concluzia
# ca distributia reziduurilor este asimetrica la stanga si leptocurtica => 
# reziduurile nu sunt normal distribuite

# Boxplot -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
boxplot(model_simple$residuals, main="Box Plot reziduuri")
#avem o singura valoare extrema

# Testul Jarque-Bera pentru normalitate ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
jarque.bera.test(ladderScore$uhat)
# deoarece p-value < 0.05 => reziduurile nu sunt normal distribuite

#distanta Cook ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ols_plot_cooksd_bar(model_simple) 
ladderScore_cook <- ladderScore[-c(19), ]
#reestimam modelul
model_simple_C <- lm(Ladder.score ~ Social.support,ladderScore_cook)
summary(model_simple_C)
ladderScore_cook %<>% mutate(uhat_cook = resid(model_simple_C)) 
ols_plot_cooksd_bar(model_simple_C) 


ladderScore_cook <- ladderScore_cook[-c(18), ]
#reestimam modelul
model_simple_C <- lm(Ladder.score ~ Social.support,ladderScore_cook)
summary(model_simple_C)
ladderScore_cook %<>% mutate(uhat_cook = resid(model_simple_C)) 
ols_plot_cooksd_bar(model_simple_C) 

ladderScore_cook <- ladderScore_cook[-c(14), ]
#reestimam modelul
model_simple_C <- lm(Ladder.score ~ Social.support,ladderScore_cook)
summary(model_simple_C)
ladderScore_cook %<>% mutate(uhat_cook = resid(model_simple_C)) 
ols_plot_cooksd_bar(model_simple_C) 
# eliminam din model valorile outlier pana nu mai avem


# Testam cu Jarque-Bera
jarque.bera.test(ladderScore_cook$uhat) # p-value > 0.1 => reziduurile sunt normal
# distribuite si vom reface toti pasii pentru a demonstra si grafic 
# Pas 1 - Graficul 'Residuals vs Fitted'
ols_plot_resid_fit(model_simple_C)
# Pas 2 - Graficul Q-Q plot
ols_plot_resid_qq(model_simple_C)
# Pas 3 - Histograma reziduurilor
ols_plot_resid_hist(model_simple_C)
# Pas 4 - Boxplotul reziduurilor
ols_plot_resid_box(model_simple_C)
# Pas 5 - Testarea cu ajutorul diferitelor test de normalitate
ols_test_normality(model_simple_C) 
#==> ipoteza de normalitate este validata


####################################################### ANALIZA GRAFICA A HETEROSCHEDASTICITAII #######################################################

# Graficul reziduurilor fata de variabila independenta Social.support
ggplot(data = ladderScore_cook, mapping = aes(x = Social.support, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Perceptia asupra suportului social, Social.support')

# Graficul reziduurilor fata de valorile estimate de model
ladderScore_cook %<>% mutate(yhat = fitted(model_simple_C))
ggplot(data = ladderScore_cook, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')

#Ambele grafice indica faptul ca nu avem heteroscedasticitate in date deoarece 
#nu putem identifica vizual 'efect de palnie'


######################################################## TESTE DE HETEROSCHEDASTICITATE #######################################################

# Generam patratul si interactiunea variabilelor independente -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ladderScore_cook %<>% mutate(Social.support = Social.support^2)

ladderScore_cook %<>% mutate(uhat1 = resid(model_simple_C),
                             yhat1 = fitted(model_simple_C), 
                             uhat1sq = uhat1^2, 
                             yhat1sq = yhat1^2)

# Testul Breusch-Pagan -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
bptest(model_simple_C)

# Testul White ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
white_test(model_simple_C) 

# Ambele teste prezinta p-value > 0.1 => la 99% reziduurile sunt homoscedastice
# deci aceasta ipoteza nu este incalcata atunci cand schimbam forma functionala a variabilei dependente.
# Acesta este unul din procedeele de corectie a incalcarii ipotezei de hetero


######################################################## IPOTEZE DE NONAUTOCORELARE ########################################################


# Autocorelarea ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(model_simple_C)
acf(model_simple_C$residuals)

# Testul Durbin-Watson (ordinul 1) ---------------------------------------------------------------------------------------------------------------------------------------------------------------------

dwtest(model_simple_C) # p-value < 0.1 => reziduurile sunt autocorelate

bgtest(model_simple_C) # p-value < 0.1 
bgtest(model_simple_C, order = 2)  # p-value < 0.1 
bgtest(model_simple_C, order = 3)  # p-value < 0.1 
bgtest(model_simple_C, order = 4)  # p-value < 0.1 
# reziduurile sunt autocorelate si la lag superior


# Corectarea autocorelarii - adaugam lag1 ca variabila independenta in modelul original ------------------------------------------------------------------------------------------------------------------------

# Cream un nou set de date 
ladder_data <- data.frame(ladderScore_cook, resid_mod1=model_simple_C$residuals)
# Cream variabila lag1 
ladder_data_1 <- slide(ladder_data, Var="resid_mod1", NewVar = "lag1", slideBy = -1)
ladder_data_2 <- na.omit(ladder_data_1) # eliminam valorile NA
# Reimplementam modelul cu noua variabila lag1 adaugata in model
model2 <- lm(Ladder.score ~ Social.support+ lag1, data=ladder_data_2)
summary(model2)
# Scopul variabilei lag1 este să captureze autocorelația reziduurilor din modelul anterior

# Retestam ipoteza pe noul model
# ACF 
acf(model2$residuals) # autocorelarea a disparut
# Durbin Watson 
dwtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
# Breusch-Godfrey 
bgtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 

######################################################## IPOTEZELE MODELULUI DE REGRESIE ######################################################## 

summary(model2)
# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 
# ladderScore =  5.9313 + 0.4947 * Social.support + 0.7108 * lag1

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model2) > (model2$rank - 1)  #Mai mult decat atat, pentru fiecare variabila indepenta x, setul de date ar trebui sa includa min 15 obs

# Ipoteza 3 - Modelul de regresie este corect specificat
# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

# Ipoteza 4 - Variabilitatea in x este pozitiva (mai mare decat 0)
var(ladder_data_2$Social.support)
var(ladder_data_2$lag1) # toate valorile > 0 => ipoteza acceptata

# Ipoteza 5 - Media reziduurilor este 0
mean(model2$residuals) # medie aproape de 0 => ipoteza acceptata

# Ipoteza 6 - Testare multicoliniaritate
vif(model2) # nu avem valori pt VIF > 10 => ipoteza acceptata

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(ladder_data_2$Social.support, model2$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(ladder_data_2$lag1, model2$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza acceptata  

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model2) # homoscedastic
white_test(model2) # homoscedastic
# => ipoteza acceptata

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model2$residuals) # nu sunt autocorelate
dwtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model2) # p-value > 0.1 => reziduuri nonautocorelate 
# => ipoteza acceptata

# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(model2$residuals)  # p-value > 0.1 => reziduurile sunt normal distribuite
# => ipoteza acceptata

# MODELUL ESTE BUN PENTRU PROGNOZE
########################################################################

################################### PREDICTIE, INDICATORI DE ACURATETE, TERMEN DE INTERACTIUNE, VARIABILE DUMMY ##########################################

# Împărțirea setului de date în setul de antrenare și testare
set.seed(123)  # pentru reproductibilitate
n <- nrow(ladder_data_2)
training.samples <- sample(1:n, size = 0.8 * n, replace = FALSE)  # Selectăm 80% din rânduri
train.data  <- ladder_data_2[training.samples, ] # Setul de antrenare
test.data <- ladder_data_2[-training.samples, ] # Setul de testare

# Model de regresie pentru setul de antrenare    ??????????????????????????????????????
#model_log_corrected_train <- lm(Ladder.score ~ Social.support + lag1, data = train.data)
#summary(model_log_corrected_train)


# Model de regresie pentru setul de antrenare
model_log_corrected_train <- lm(Ladder.score ~ Social.support , data = train.data)
summary(model_log_corrected_train) 

# Predictia modelului pe setul de testare
y_pred <- predict(model_log_corrected_train, newdata = test.data)
y_pred

# RMSE - Root Mean Squared Error
RMSE(y_pred, test.data$Ladder.score)

# MAE - Mean Absolute Error
MAE(y_pred, test.data$Ladder.score)

# MSE - Mean Squared Error
mse(y_pred, test.data$Ladder.score)

# MAPE - Mean Absolute Percentage Error
MAPE(y_pred, test.data$Ladder.score)

# Setul de date pentru prognoză 'out-of-sample'
out_of_sample <- data.frame(Social.support = c(1.9,2.0,2.5),
                            lag1 = c(0.03, 0.01, 0.02))

# Realizăm prognoza folosind modelul antrenat pe datele (model2)
y_pred_outsample <- predict(model2, newdata = out_of_sample)

# Rezultatele prognozei
print(y_pred_outsample)
# Pentru o persoana cu suport social = 1.9 si lag1 = 0.03 scorul estimat de fericire este de 6.892528
# Pentru o persoana cu suport social = 2.0 si lag1 = 0.01 scorul estimat de fericire este de 6.927782
# Pentru o persoana cu suport social = 2.5 si lag1 = 0.02 scorul estimat de fericire este de 7.182245

# Regresia cu variabile dummy --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

lm(Ladder.score ~ Region_Western.Europe, ladder_data_2) %>% summary
#nu este semnificativ 


# Termeni de interactiune cu o alta variabila dummy -----------------------------------------------------------------------------------------------------------------------------------------------------------------
model2.dummy<-lm(Ladder.score ~ Social.support + lag1+ Region_Western.Europe, ladder_data_2)
summary(model2.dummy)
#nu este semnificativ


model2.dummy2<-lm(Ladder.score ~ Social.support + lag1+ Region_North.America.and.ANZ, ladder_data_2)
summary(model2.dummy2)
#nu este semnificativ

model2.dummy3<-lm(Ladder.score ~ Social.support + lag1 + Region_Middle.East.and.North.Africa, ladder_data_2)
summary(model2.dummy3)
#  In acest caz variabila dummy Region_Middle.East.and.North.Africa este semnificativa 


# Ipotezele modelului de regresie --------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(model2.dummy3)
# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 
# ladderScore = 

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model2.dummy3) > (model2.dummy3$rank - 1)  #Mai mult decat atat, pentru fiecare variabila indepenta x, setul de date ar trebui sa includa min 15 obs

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

# Ipoteza 4 - Variabilitatea in x este pozitiva (mai mare decat 0)
var(ladder_data_2$Social.support)
var(ladder_data_2$lag1)
var(ladder_data_2$Region_Middle.East.and.North.Africa)# toate valorile > 0 => ipoteza acceptata

# Ipoteza 5 - Media reziduurilor este 0
mean(model2.dummy3$residuals) # medie aproape de 0 => ipoteza acceptata

# Ipoteza 6 - Testare multicoliniaritate
vif(model2.dummy3) # nu avem valori pt VIF > 10 => ipoteza acceptata

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(ladder_data_2$Social.support, model2.dummy3$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(ladder_data_2$lag1, model2.dummy3$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(ladder_data_2$Region_Middle.East.and.North.Africa, model2.dummy3$residuals)
# => ipoteza acceptata  

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model2.dummy3) # homoscedastic
white_test(model2.dummy3) # homoscedastic
# => ipoteza acceptata

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model2.dummy3$residuals) # nu sunt autocorelate
dwtest(model2.dummy3) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model2.dummy3) # p-value > 0.1 => reziduuri nonautocorelate 
# => ipoteza acceptata

# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(model2.dummy3$residuals)  # p-value > 0.1 => reziduurile sunt normal distribuite
# => ipoteza acceptata

out_of_sample2 <- data.frame(Social.support = c(2.2),
                             lag1 = c(0.02),
                             Region_Middle.East.and.North.Africa=c(1))

# Realizăm prognoza folosind modelul antrenat pe datele (model2)
y_pred_outsample2 <- predict(model2.dummy3, newdata = out_of_sample2)

# Rezultatele prognozei
print(y_pred_outsample2)
# Pentru o persoana cu suport social = 2.2, lag1 = 0.02  si care locuieste in Region_Middle East and North Africa scorul estimat de fericire este de 7.316317

############################################ METODE DE REGULARIZARE ############################################

# Regresia Ridge ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Definim variabila raspuns
y <- ladder_data_2$Ladder.score

# Definim predictorii
x <- data.matrix(ladder_data_2[, c('Social.support', 'lag1', 'Region_Middle.East.and.North.Africa')])

install.packages("glmnet")
library(glmnet)

# Estimam modelul ridge (alpha = 0)
model <- glmnet(x, y, alpha = 0)
summary(model)

# In continuare vom identifica valoarea lui lambda pt care avem MSE minimizat
# utilizand validarea incrucisata (cross validation)
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 0.04621082


# testarea valorii lamda 
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 


# Diagrama Trace 
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:3, legend = colnames(x), cex = .7)


# Prognoze 
y_predicted <- predict(model, s = best_lambda, newx = x)

# Progoza out-of-sample
new <- matrix(c(2.2, 0.03, 1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 67.85766%

# Regresia LASSO ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# SSR + lambda*sum(|beta|)
model <- glmnet(x, y, alpha = 1)

# Din punct de vedere tehnic, vom seta valoarea alpha = 1 pentru 
cv_model <- cv.glmnet(x, y, alpha = 1)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.008459992

# testarea valorii lamda
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) 

# Diagrama Trace 
plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:3, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample
# 'educ', 'exper', 'tenure', 'female','married', 'south'
new <- matrix(c(2.2, 0.03, 1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 69.49427%

# Elastic net regression da eroare pentru ca avem prea putine inregistrari


# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim ==> LASSO cel mai bun

##################################################################
