# Modelul de regresie multipla
# Cluster 2

# Cuprins:
#   1. Regresia multipla
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
#   9. Predictie, indicatori de acuratete, termen de interactiune, variabile dummy
#  10. Metode de regularizare
#       Regresia Ridge
#       Regresia LASSO
#       Regresia Elastic Net

# Setarea mediului de lucru
rm(list = ls()) 
directory <-"C:/Users/gladi/OneDrive/Desktop/ProiectEconometrie/"


# Incarcarea librariilor
library(tidyverse)
library(caret) #pachetul caret este folosit pentru a facilita antrenarea si evaluarea modelelor de invatare automata. 
#Acesta ofera o interfata unificata pentru o gama larga de algoritmi de invatare automata si metode de selectie si evaluare a modelelor
library(mltools) #cuprinde o colectie de functii utile pentru invatarea automata, in sepcial in analiza exploratorie a datelor
library(MLmetrics) #libraria MLmetrics include o colectie de functii utilizate pentru evaluarea performantei modelelor de regresie sau clasificare


# Instalarea si activarea pachetelor
PackageNames <- c("tidyverse", "stargazer", "magrittr", "car","ICglm","lmtest", "sandwich", 
                  "olsrr", "moments","whitestrap","tseries")
for(i in PackageNames){
  if(!require(i, character.only = T)){
    install.packages(i, dependencies = T)
    require(i, character.only = T)
  }
}

########################################################### REGRESIA MULTIPLA ########################################################### 
# Citirea datelor
ladderScore <- read.csv(paste0(directory, "Cluster2_updated.csv"))

# Afisarea datelor
ladderScore %>% 
  select(Ladder.score, Log.GDP.per.capita, Social.support, Healthy.life.expectancy,Freedom.to.make.life.choices,Generosity,Perceptions.of.corruption)

# Regresia multipla
# wage = beta0 + beta1*educ + beta2*exper + u
model_multiple1 <- lm(Ladder.score ~ Social.support+Healthy.life.expectancy+Freedom.to.make.life.choices+Generosity,ladderScore)
summary(model_multiple1)


model_multiple3 <- lm(Ladder.score ~ Log.GDP.per.capita+Social.support+Freedom.to.make.life.choices+Generosity+Perceptions.of.corruption,ladderScore)
summary(model_multiple3)


model_multiple2 <- lm(Ladder.score ~ Log.GDP.per.capita+Social.support+Healthy.life.expectancy,ladderScore)
summary(model_multiple2)

vif(model_multiple2)

model_multiple_ <- lm(Log.GDP.per.capita ~ Ladder.score+ Social.support + Healthy.life.expectancy + Freedom.to.make.life.choices + Generosity + Perceptions.of.corruption,ladderScore)
summary(model_multiple_)

model_multiple_l1 <- lm(Log.GDP.per.capita ~ lLadder.score+ Healthy.life.expectancy + Generosity,ladderScore)
summary(model_multiple_l1)

model_multiple_l2 <- lm(Log.GDP.per.capita ~ lLadder.score+ lHealthy.life.expectancy + Generosity,ladderScore)
summary(model_multiple_l2)

model_multiple_l3 <- lm(Log.GDP.per.capita ~ lLadder.score+ lHealthy.life.expectancy + lGenerosity+lFreedom.to.make.life.choices,ladderScore)
summary(model_multiple_l3)

model_multiple <- lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity,ladderScore)
summary(model_multiple)
# Intercept - Daca lHealthy.life.expectancy si lGenerosity  sunt 0, atunci variabila dependenta Log.GDP.per.capita este estimata a avea o valoare medie de  0.8699
# lHealthy.life.expectancy - Daca logaritm din speranta de o viata sanatoasa creste cu un punct procentual, Log.GDP.per.capita creste cu 0.3230 puncte procentuale pastrand ceilalti termeni constanti
# lGenerosity - Daca logaritm din generozitate creste cu un punct procentual, Log.GDP.per.capita scade cu 0.2118 puncte procentuale pastrand ceilalti termeni constanti
# Cele doua variabile sunt semnificative pentru modelul de regresie ales: lHealthy.life.expectancy (pentru un nivel de semnificatie de 0.01), lGenerosity (pentru un nivel de semnificatie de 0.001)


# Bonitatea modelului (R-squared si R-squared ajustat) ---------------------------------------------------------------------------------------------------------
# Regresie multipla cu 2 regresori
summary(model_multiple)
summary(model_multiple)$r.squared
# Aproximativ 36.46% din variatia variabilei dependente este explicata de variabilele independente din model
summary(model_multiple)$adj.r.squared
# Modelul explică aproximativ 33.57% din variația variabilei dependente


# Calitatea ajustarii ---------------------------------------------------------------------------------------------------------
# Akaike (AIC), Schwarz (BIC) si Hannan-Quinn (HQC) 

# Criteriul Akaike
aic <- AIC(model_multiple)
cat("AIC (Akaike):", aic, "/n")

# Criteriul Schwarz 
bic <- BIC(model_multiple)
cat("BIC (Schwarz):", bic, "/n")

# Criteriul Hannan - Quinn 
hqic <- HQIC(model_multiple)
cat("HQIC (Hannan-Quinn):", hqic, "/n")


########################################################### TESTUL T ########################################################### 
# Testare de ipoteze
# Ipotezele testului t
# H0: miu = miu0
# H1: miu diferit de miu0

(coefficient <- coef(model_multiple)["lHealthy.life.expectancy"])

# Afisarea erorii standard
(se <- vcov(model_multiple) %>% 
    diag %>% 
    sqrt %>% 
    .["lHealthy.life.expectancy"])

# Calcularea testului t = coeficient/se
(tstat <- coefficient/se)

# Gradele de libertate (n-k-1)
(df_r <- model_multiple$df.residual)

qt(p = 0.975, df = df_r, lower.tail = TRUE)
#t statistic = 2.843092 > t critic = 2.015368 deci respingem ipoteza nula si acceptam ipoteza alternativa ca acest coeficient este semnificativ statistic pentru un nivel de incredere de 95%

########################################################### TESTUL F ########################################################### 
# Model restricționat: doar interceptul
model_restricted <- lm(Log.GDP.per.capita ~ 1, data = ladderScore)

SSR_r <- sum(resid(model_restricted)^2)
SSR_ur <- sum(resid(model_multiple)^2)

# Grade de libertate
q <- length(coef(model_multiple)) - 1 
df_ur <- model_multiple$df.residual    

# Calculul F-statistic
F_stat <- ((SSR_r - SSR_ur) / q) / (SSR_ur / df_ur)
F_stat

# Valoare critică F pentru nivel de semnificație de 5%
F_critical <- qf(0.95, df1 = q, df2 = df_ur)
F_critical

# p-value pentru F-test
F_pvalue <- pf(F_stat, df1 = q, df2 = df_ur, lower.tail = FALSE)
F_pvalue

# p-value =  4.637982e-05 este mult mai mic decat pragul de 0.05 deci confirma respingerea ipotezei nule
#Atfel, modelul complet este semnificativ mai bun decat modelul restrictionat


####################################################### IPOTEZA DE NORMALITATE A REZIDUURILOR #######################################################
# Testul Jarque-Bera pentru normalitate ------------------------------------------------------------------------------------------------------------------------
jarque.bera.test(ladderScore$Log.GDP.per.capita)
# p-value = 0.2015 este mult mai mare decat pragul de 0.05 deci acceptam ipoteza alternativa=> datele sunt normalizate

# Graficul 'Residuals vs Fitted' ---------------------------------------------------------------------------------------------------------------------------
plot(model_multiple)

# Graficul 'Q-Q plot'---------------------------------------------------------------------------------------------------------------------
plot(model_multiple)

# Histograma reziduurilor ------------------------------------------------------------------------------------------------------------------
ols_plot_resid_hist(model_multiple)

ladderScore %<>% mutate(uhat = resid(model_multiple))
ggplot(data = ladderScore) +
  theme_bw() +
  geom_histogram(mapping = aes(x = uhat), col = 'grey')+
  xlab('Reziduuri') + 
  ylab('Count') +
  ggtitle('Histograma reziduurilor') + 
  theme(plot.title = element_text(hjust = 0.5))
# Histograma arata asimetrie la stanga deoarece observam valori extreme in jurul valorii de -1

#Asimetria
skewness(ladderScore$uhat)
# Valoarea negativa = -1.436544 ne indica faptul ca distributia 
# noastra este centrata la stanga (asimetria)

# Boltirea (kurtosis) --------------------------------------------------------------------------------------------------------
kurtosis(ladderScore$uhat)
# kurtosis = 7.806934 => distributie leptocurtica
# In urma testarii coeficientului de asimetrie si a boltirii putem trage concluzia
# ca distributia reziduurilor este asimetrica la stanga si leptocurtica => 
# reziduurile nu sunt normal distribuite

# Boxplot -------------------------------------------------------------------------------------------------------
boxplot(model_multiple$residuals, main="Box Plot reziduuri")
#avem o singura valoare extrema

# Testul Jarque-Bera pentru normalitate -------------------------------------------------------------------------------------------------------
jarque.bera.test(ladderScore$uhat)
# deoarece p-value < 0.05 => reziduurile nu sunt normal distribuite


#distanta Cook
ols_plot_cooksd_bar(model_multiple) 
ladderScore_cook <- ladderScore[-c(1), ]
#reestimam modelul
model_multiple_C <- lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity,ladderScore_cook)
summary(model_multiple_C)
ladderScore_cook %<>% mutate(uhat_cook = resid(model_multiple_C)) # extragem reziduurile din model 
ols_plot_cooksd_bar(model_multiple_C) 

ladderScore_cook <- ladderScore_cook[-c(11,24,43,45), ]
#reestimam modelul
model_multiple_C <- lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity,ladderScore_cook)
summary(model_multiple_C)
ladderScore_cook %<>% mutate(uhat_cook = resid(model_multiple_C)) # extragem reziduurile din model 
ols_plot_cooksd_bar(model_multiple_C) 

ladderScore_cook <- ladderScore_cook[-c(7), ]
#reestimam modelul
model_multiple_C <- lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity,ladderScore_cook)
summary(model_multiple_C)
ladderScore_cook %<>% mutate(uhat_cook = resid(model_multiple_C)) # extragem reziduurile din model 
ols_plot_cooksd_bar(model_multiple_C) 

ladderScore_cook <- ladderScore_cook[-c(13), ]
#reestimam modelul
model_multiple_C <- lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity,ladderScore_cook)
summary(model_multiple_C)
ladderScore_cook %<>% mutate(uhat_cook = resid(model_multiple_C)) # extragem reziduurile din model 
ols_plot_cooksd_bar(model_multiple_C) 
#============================

# Testam cu Jarque-Bera
jarque.bera.test(ladderScore_cook$uhat) # p-value > 0.1 => reziduurile sunt normal
# distribuite si vom reface toti pasii pentru a demonstra si grafic 
# Pas 1 - Graficul 'Residuals vs Fitted'
ols_plot_resid_fit(model_multiple_C)
# Pas 2 - Graficul Q-Q plot
ols_plot_resid_qq(model_multiple_C)
# Pas 3 - Histograma reziduurilor
ols_plot_resid_hist(model_multiple_C)
# Pas 4 - Boxplotul reziduurilor
ols_plot_resid_box(model_multiple_C)
# Pas 5 - Testarea cu ajutorul diferitelor test de normalitate
ols_test_normality(model_multiple_C) 
#==> ipoteza de normalitate este validata


####################################################### ANALIZA GRAFICA A HETEROSCHEDASTICITAII #######################################################
# Graficul reziduurilor fata de variabila independenta Healthy life expectancy
ggplot(data = ladderScore_cook, mapping = aes(x = lHealthy.life.expectancy, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Logaritm Healthy life expectancy, lHealthy.life.expectancy')

# Graficul reziduurilor fata de valorile estimate de model
ladderScore_cook %<>% mutate(yhat = fitted(model_multiple_C))
ggplot(data = ladderScore_cook, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')

# Graficul reziduurilor fata de variabila independenta Social.support
ggplot(data = ladderScore_cook, mapping = aes(x = lGenerosity, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Logaritm Generosity, lGenerosity')

# Graficul reziduurilor fata de valorile estimate de model
ladderScore_cook %<>% mutate(yhat = fitted(model_multiple_C))
ggplot(data = ladderScore_cook, mapping = aes(x = yhat, y = uhat)) + 
  theme_bw() +
  geom_point() +
  geom_hline(yintercept = 0, col = 'red') + 
  labs(y = 'Reziduuri', x = 'Valori estimate')

#Ambele grafice indica faptul ca nu avem heteroscedasticitate in date deoarece 
#nu putem identifica vizual 'efect de palnie'

# Testul Breusch-Pagan ----
bptest(model_multiple_C)

# Testul White ----
white_test(model_multiple_C) 

# Ambele teste prezinta p-value > 0.1 => la 99% reziduurile sunt homoscedastice
# deci aceasta ipoteza nu este incalcata atunci cand schimbam forma functionala a variabilei dependente.
# Acesta este unul din procedeele de corectie a incalcarii ipotezei de hetero

######################################################## IPOTEZE DE NONAUTOCORELARE ########################################################

# Autocorelarea -----------------
summary(model_multiple_C)
acf(model_multiple_C$residuals)

# Testul Durbin-Watson (ordinul 1)
# H0: reziduurile nu sunt autocorelate
# H1: reziduurile sunt autocorelate

dwtest(model_multiple_C) # p-value > 0.1 => reziduurile nu sunt autocorelate

bgtest(model_multiple_C) # p-value > 0.1 
bgtest(model_multiple_C, order = 2) # =>
bgtest(model_multiple_C, order = 3)
bgtest(model_multiple_C, order = 4)
# reziduurile nu sunt autocorelate si la lag superior

######################################################## IPOTEZELE MODELULUI DE REGRESIE ######################################################## 
summary(model_multiple_C)
# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 
# ladderScore = 1.30323 + 0.53393 * lHealthy.life.expectancy - 0.10124 * lGenerosity

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model_multiple_C) > (model_multiple_C$rank - 1)  #Mai mult decat atat, pentru fiecare variabila indepenta x, setul de date ar trebui sa includa min 15 obs

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

# Ipoteza 4 - Variabilitatea in x este pozitiva (mai mare decat 0)
var(ladderScore_cook$lHealthy.life.expectancy)
var(ladderScore_cook$lGenerosity) # toate valorile > 0 => ipoteza acceptata

# Ipoteza 5 - Media reziduurilor este 0
mean(model_multiple_C$residuals) # medie aproape de 0 => ipoteza acceptata

# Ipoteza 6 - Testare multicoliniaritate
vif(model_multiple_C) # nu avem valori pt VIF > 10 => ipoteza acceptata

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(ladderScore_cook$lHealthy.life.expectancy, model_multiple_C$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(ladderScore_cook$lGenerosity, model_multiple_C$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza acceptata  #####!!!!p-value = 1?? !!!########

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model_multiple_C) # homoscedastic
white_test(model_multiple_C) # homoscedastic
# => ipoteza acceptata

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model_multiple_C$residuals) # nu sunt autocorelate
dwtest(model_multiple_C) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model_multiple_C) # p-value > 0.1 => reziduuri nonautocorelate 
# => ipoteza acceptata

# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(model_multiple_C$residuals)  # p-value > 0.1 => reziduurile sunt normal distribuite
# => ipoteza acceptata

# MODELUL ESTE BUN PENTRU PROGNOZE
summary(model_multiple_C)
########################################################################


################################### PREDICTIE, INDICATORI DE ACURATETE, TERMEN DE INTERACTIUNE, VARIABILE DUMMY ##########################################
# Impartirea setului de date in setul de antrenare si de testare

set.seed(123) #aceasta comanda este utilizata pentru a intia generatorul de numere aleatoare si asigura reproductibilitatea rezultatelor in cadrul analizei sau a experimentului
training.samples <- ladderScore_cook$Log.GDP.per.capita %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- ladderScore_cook[training.samples, ] #se creeaza un nou dataframe numit train.data care contine 80% din datele initiale pentru antrenament
test.data <- ladderScore_cook[-training.samples, ] #se creeaza un alt dataframe numit test.data care contine datele pentru setul de testare

# Model de regresie pentru setul de antrenare
model_multiple_C_0 <- lm(Log.GDP.per.capita ~ lHealthy.life.expectancy+lGenerosity,data=train.data) 
summary(model_multiple_C_0)

# Predictia modelului pe setul de testare
y_pred <- predict(model_multiple_C_0, newdata = test.data)
y_pred

# RMSE - Root Mean Squared Error => acest indicator de acuratete arata cat de 
# departe se incadreaza predictiile fata de valorile reale masurate folosind
# distanta euclidiana. Un RMSE < 1 => predictie buna 
RMSE(y_pred, test.data$Log.GDP.per.capita)

# MAE - Mean Absolute Error => similar cu RMSE, doar ca MAE este un scor liniar
# ceea ce inseamna ca toate diferentele individuale sunt ponderate in mod egal
# in medie. MAE < 1 => predictie buna 
MAE(y_pred, test.data$Log.GDP.per.capita)

# MSE - Mean Squared Error => masoara in unitati care este patratul variabilei
# tinta si penalizeaza mai mult si mai sever erorile mai mari. MSE < 1 =>
# predictie buna
mse(y_pred, test.data$Log.GDP.per.capita)

# MAPE - Mean Absolute Percentage Error => masura ce indica media dispersiei 
# dintre valorile previzionate si cele reale . MAPE < 1 => predictie buna 
MAPE(y_pred, test.data$Log.GDP.per.capita)

# Setul de date pentru prognoză 'out-of-sample'
out_of_sample <- data.frame(lHealthy.life.expectancy = c(-0.8, -0.69, -0.87),
                            lGenerosity = c(-1.25, -2.75, -3))

# Realizăm prognoza folosind modelul antrenat pe datele (model2)
y_pred_outsample <- predict(model_multiple_C, newdata = out_of_sample)

# Rezultatul prognozei
print(y_pred_outsample)
# Pentru o persoana cu Healthy.life.expectancy = 0.4493 si Generosity = 0.2865 , GDP.per.capita este de 2.725
# Pentru o persoana cu Healthy.life.expectancy = 0.5015 si Generosity =0.064 , GDP.per.capita este de 3.363
# Pentru o persoana cu Healthy.life.expectancy = 0.419 si Generosity = 0.05 , GDP.per.capita este de 3.134


#Regresie cu termeni de interactiune--------------------------------------------------------------

summary(model_multiple_C)
ladderScore_cook %<>% mutate(ladderscorehat2=fitted(model_multiple_C))

#Plot pentru lHealthy.life.expectancy
ggplot(ladderScore_cook,aes(x=lHealthy.life.expectancy))+
  theme_bw()+
  geom_point(aes(y=Log.GDP.per.capita, col='Log GDP per capita'))+
  geom_point(aes(y=ladderscorehat2, col='Predictia liniara'))

#Plot pentru lGenerosity
ggplot(ladderScore_cook,aes(x=lGenerosity))+
  theme_bw()+
  geom_point(aes(y=Log.GDP.per.capita, col='Log GDP per capita'))+
  geom_point(aes(y=ladderscorehat2, col='Predictia liniara'))

ladderScore_cook %<>% mutate(lHealthy.life.expectancyXlGenerosity  = lHealthy.life.expectancy*lGenerosity)

model2.1 <-lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity +lHealthy.life.expectancyXlGenerosity , ladderScore_cook)
summary(model2.1)
#Interactiunea dintre lHealthy.life.expectancyXlGenerosity nu este semnificativa


# Regresia cu variabile dummy--------------------------------------------------------------------------------

lm(Log.GDP.per.capita ~ Region_Sub.Saharan.Africa, ladderScore_cook) %>% summary
#este semnificativ 

model.dummy<-lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity + Region_Sub.Saharan.Africa, ladderScore_cook )
summary(model.dummy)

model.dummy2<-lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity + Region_Middle.East.and.North.Africa, ladderScore_cook )
summary(model.dummy2)

ladderScore_cook %<>% mutate(Region_Sub.Saharan.AfricaXMiddle.East.and.North.Africa  = Region_Sub.Saharan.Africa*Region_Middle.East.and.North.Africa)

model.dummy3<-lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity + Region_South.Asia, ladderScore_cook )
summary(model.dummy3)


model.dummy4<-lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity + Region_Southeast.Asia, ladderScore_cook )
summary(model.dummy4)


model.dummy5<-lm(Log.GDP.per.capita ~ lHealthy.life.expectancy + lGenerosity + Region_Commonwealth.of.Independent.States, ladderScore_cook )
summary(model.dummy5) #este bun :))

# Ipotezele modelului de regresie -----------------
summary(model.dummy5)
# Ipoteza 1 - Este modelul liniar in parametri? 
# Da, deoarece poate fi scris ca o functie liniara 
# ladderScore = 

# Ipoteza 2 - Nr de observatii > nr variabile independente
nobs(model.dummy5) > (model.dummy5$rank - 1)  #Mai mult decat atat, pentru fiecare variabila indepenta x, setul de date ar trebui sa includa min 15 obs

# Ipoteza 3 - Modelul de regresie este corect specificat

# Presupune ca daca variabilele X si Y au o relatie inversa, ecuatia modelului
# sa fie specificata in mod corespunzator => nu este cazul nostru

# Ipoteza 4 - Variabilitatea in x este pozitiva (mai mare decat 0)
var(ladderScore_cook$lHealthy.life.expectancy )
var(ladderScore_cook$lGenerosity)
var(ladderScore_cook$Region_Commonwealth.of.Independent.States)
# toate valorile > 0 => ipoteza acceptata

# Ipoteza 5 - Media reziduurilor este 0
mean(model.dummy5$residuals) # medie aproape de 0 => ipoteza acceptata

# Ipoteza 6 - Testare multicoliniaritate
vif(model.dummy5) # nu avem valori pt VIF > 10 => ipoteza acceptata

# Ipoteza 7 - Reziduurile nu sunt corelate cu variabilele independente
cor.test(ladderScore_cook$lHealthy.life.expectancy, model.dummy5$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(ladderScore_cook$lGenerosity, model.dummy5$residuals) # p-value > 0.1 => nu sunt corelate
cor.test(ladderScore_cook$Region_Commonwealth.of.Independent.States, model.dummy5$residuals) # p-value > 0.1 => nu sunt corelate
# => ipoteza acceptata  

# Ipoteza 8 - Reziduurile sunt homoscedastice
bptest(model.dummy5) # homoscedastic
white_test(model.dummy5) # homoscedastic
# => ipoteza acceptata

# Ipoteza 9 - Reziduurile nu sunt autocorelate
acf(model.dummy5$residuals) # nu sunt autocorelate
dwtest(model.dummy5) # p-value > 0.1 => reziduuri nonautocorelate 
bgtest(model.dummy5) # p-value > 0.1 => reziduuri nonautocorelate 
# => ipoteza acceptata

# Ipoteza 10 -  Reziduurile sunt normal distribuite
jarque.bera.test(model.dummy5$residuals)  # p-value > 0.1 => reziduurile sunt normal distribuite
# => ipoteza acceptata


# Metode de regularizare, algoritmi de selectie a variabilelor

summary(model.dummy5)


#   Regresia LASSO
#   Regresia Elastic Net

# Setul de date pentru prognoză 'out-of-sample'
out_of_sample2 <- data.frame(lHealthy.life.expectancy = c(-0.69),
                             lGenerosity = c(-2.75),
                             Region_Commonwealth.of.Independent.States= c(1))

# Realizăm prognoza folosind modelul antrenat pe datele (model2)
y_pred_outsample2 <- predict(model.dummy5, newdata = out_of_sample2)

# Rezultatul prognozei
print(y_pred_outsample2)
# Pentru o persoana cu Healthy.life.expectancy = 0.5015,Generosity = 0.064 si care locuieste in Regiunea Commonwealth.of.Independent.States GDP per capita 4.437

############################################ METODE DE REGULARIZARE ############################################
#   Regresia Ridge
# Definim variabila raspuns
y <- ladderScore_cook$Log.GDP.per.capita

# Definim predictorii
x <- data.matrix(ladderScore_cook[, c('lHealthy.life.expectancy', 'lGenerosity', 'Region_Commonwealth.of.Independent.States')])

install.packages("glmnet")
library(glmnet)

# Estimam modelul ridge (alpha = 0)
model <- glmnet(x, y, alpha = 0)
summary(model)

# In continuare vom identifica valoarea lui lambda pt care avem MSE minimizat
# utilizand validarea incrucisata (cross validation)
cv_model <- cv.glmnet(x, y, alpha = 0)
best_lambda <- cv_model$lambda.min
best_lambda # 0.017


# testarea valorii lamda 
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 


# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda")
legend("bottomright", lwd = 1, col = 1:3, legend = colnames(x), cex = .7)


# Prognoze 
y_predicted <- predict(model, s = best_lambda, newx = x)

# Progoza out-of-sample
new <- matrix(c(-0.69, -2.75, 1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 60.17%

# Regresia LASSO ----------------------------------------------------------------------------------------
# SSR + lambda*sum(|beta|)
model <- glmnet(x, y, alpha = 1)

# Din punct de vedere tehnic, vom seta valoarea alpha = 1 pentru 
# regresia LASSO. 
cv_model <- cv.glmnet(x, y, alpha = 1)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.0008

# testarea valorii lamda
plot(cv_model) 


# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor  # coeficientii variabilelor 
# daca unul din coeficienti este 0 inseamna ca acea variabila nu este importanta
# si de aceea nu o estimeaza modelul

# Diagrama Trace pentru a vizualiza modul in care estimarile coeficientulilor s-au
# modificat ca urmare a cresterii valorii lui lambda
plot(model, xvar = "lambda",label=T)
legend("bottomright", lwd = 1, col = 1:3, legend = colnames(x), cex = .7)

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample
# 'educ', 'exper', 'tenure', 'female','married', 'south'
new <- matrix(c(-0.69, -2.75, 1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 60.34542%

# Elastic net regression ---------------------------------------------------------------------------------------------------------
# adauga ambele penalitati SSR + lambda*sum(beta^2) + lambda*sum(|beta|)
model <- cv.glmnet(x, y, alpha = 0.5)
cv_model <- cv.glmnet(x, y, alpha = 0.5)

# Valoarea optima a lui lambda
best_lambda <- cv_model$lambda.min
best_lambda # 0.0011


# testarea valorii lamda
plot(cv_model) 

# Reimplementam modelul cu valoarea lamda optima
best_model <- glmnet(x, y, alpha = 0.5, lambda = best_lambda)
coef(best_model) # coeficientii variabilelor 

# Prognoze 
y_predicted <- predict(best_model, s = best_lambda, newx = x)

# Prognoza out-of-sample
# 
new <- matrix(c(-0.69, -2.75, 1), nrow=1, ncol=3) 
predict(best_model, s = best_lambda, newx = new)

# calcularea lui R2
sst <- sum((y - mean(y))^2)
sse <- sum((y_predicted - y)^2)
rsq <- 1 - sse/sst
rsq # 60.3458%

# Vom compara valorile lui rsq si in functie de acestea vom alege modelul cu cea
# mai mare bonitate drept modelul optim ==>Elastic net regression  cel mai bun

##################################################################

