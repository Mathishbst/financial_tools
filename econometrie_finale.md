Étude des cycles financiers
================
Mathis Herbst
Mardi 11 Février 2025

- [Point de vue sur les données](#point-de-vue-sur-les-données)
  - [Importation des données](#importation-des-données)
  - [Vérification des données](#vérification-des-données)
  - [Première analyse des données](#première-analyse-des-données)
  - [Description des variables utilisées
    :](#description-des-variables-utilisées-)
- [Premier travail sur les régressions
  :](#premier-travail-sur-les-régressions-)
  - [Régressions individuelles](#régressions-individuelles)
    - [Avec *ltd*](#avec-ltd)
    - [Avec *eq_capgain*](#avec-eq_capgain)
- [Analyse des faux positifs avec une matrice de confusion
  :](#analyse-des-faux-positifs-avec-une-matrice-de-confusion-)
  - [Explication Matrice de
    confusion:](#explication-matrice-de-confusion)
- [Analyse des résultats avec AUROC
  :](#analyse-des-résultats-avec-auroc-)
  - [Code R](#code-r)
  - [Interprétation des résultats :](#interprétation-des-résultats-)
- [Conclusion de cette première analyse
  :](#conclusion-de-cette-première-analyse-)
- [Nouvelle approche](#nouvelle-approche)
  - [Ajout des effets fixes pays et du
    lag](#ajout-des-effets-fixes-pays-et-du-lag)
  - [Nouveaux graphiques](#nouveaux-graphiques)

# Point de vue sur les données

## Importation des données

``` r
data <- read_excel("econometriefinale2.xlsx", sheet=1)
```

## Vérification des données

``` r
head(data)
```

    ## # A tibble: 6 × 56
    ##    year country   iso     ifs rgdpmad rgdpbarro rconsbarro   gdp    iy   cpi
    ##   <dbl> <chr>     <chr> <dbl>   <dbl>     <dbl>      <dbl> <dbl> <dbl> <dbl>
    ## 1  1870 Australia AUS     193   3273.      13.8       21.4  209. 0.109  2.71
    ## 2  1871 Australia AUS     193   3299.      13.9       19.9  212. 0.105  2.67
    ## 3  1872 Australia AUS     193   3553.      15.0       21.1  227. 0.130  2.54
    ## 4  1873 Australia AUS     193   3824.      16.2       23.3  267. 0.125  2.54
    ## 5  1874 Australia AUS     193   3835.      16.3       23.5  288. 0.142  2.67
    ## 6  1875 Australia AUS     193   4138.      17.6       25.7  301. 0.161  2.75
    ## # ℹ 46 more variables: ca <dbl>, imports <dbl>, exports <dbl>, narrowm <dbl>,
    ## #   money <dbl>, stir <dbl>, ltrate <dbl>, hpnom <dbl>, unemp <dbl>,
    ## #   wage <dbl>, debtgdp <dbl>, revenue <dbl>, expenditure <dbl>, xrusd <dbl>,
    ## #   tloans <dbl>, tmort <dbl>, thh <dbl>, tbus <dbl>, bdebt <dbl>, lev <dbl>,
    ## #   ltd <dbl>, noncore <dbl>, crisis <dbl>, crisis_old <dbl>, peg <dbl>,
    ## #   peg_strict <dbl>, peg_type <chr>, peg_base <chr>, housing_tr <dbl>,
    ## #   bond_tr <dbl>, bill_rate <dbl>, rent_ipolated <dbl>, …

``` r
nrow(data)
```

    ## [1] 2719

``` r
ncol(data)
```

    ## [1] 56

``` r
sum(is.na(data))
```

    ## [1] 31511

Il y a un total de **2718** observations, pour un total de **56**
variables différentes, plus que suffisante pour l’analyse. On remarque
qu’il existe une grande quantité de données “NA” (**31457**). On va
ainsi se concentrer sur les variables contenant le moins de ces données
manquantes.

## Première analyse des données

Une façon éclairée de choisir une bonne direction pour notre analyse est
de vérifier la matrice de corrélation entre une donnée préexistante
“*crisis*”, variable indicatrice indiquant si l’année correspond à une
crise et les autres variables du jeu de données

On remarque par exemple que pour les États-Unis, *crisis* nous donne 1
pour les années 2008, 1930, qui correspond à des crises connues pour ce
pays.

Ainsi cela va nous permettre d’obtenir les variables les plus sensibles
à ces périodes

``` r
variables_a_tester <- c("gdp", "rgdpbarro", "money", "cpi", "ca", "stir", "ltrate", "debtgdp", "tloans", "ltd", "lev", "noncore", "eq_capgain", "eq_div_rtn", "bond_rate", "hpnom", "housing_capgain", "tmort", "unemp", "peg")
cor_matrix <- cor(na.omit(data[, c("crisis", variables_a_tester)]), use = "complete.obs", method = "pearson")
cor_matrix["crisis", ]
```

    ##          crisis             gdp       rgdpbarro           money             cpi 
    ##     1.000000000     0.023165800     0.008453414     0.027956417     0.025330474 
    ##              ca            stir          ltrate         debtgdp          tloans 
    ##    -0.079729877     0.081477373     0.028480655    -0.034898846     0.036673363 
    ##             ltd             lev         noncore      eq_capgain      eq_div_rtn 
    ##     0.112230040     0.053666695     0.048697832    -0.227954321    -0.033464938 
    ##       bond_rate           hpnom housing_capgain           tmort           unemp 
    ##     0.029306505     0.031377332    -0.096721735     0.037646239     0.022215214 
    ##             peg 
    ##     0.013545505

On remarque que des variables comme *stir* ou *ltd* ont un grande
corrélation positive avec *crisis*, et que *eq_capgain* et *ca* ont
également une grande corrélation positive avec *crisis*. Ainsi nous
pourrons partir sur ces variables.

## Description des variables utilisées :

- *stir* : Short-term Interest Rate, Taux d’intérêt à court terme,
  exprimé en pourcentage annuel.
- *ltd* : Loan-to-Deposit Ratio, Ratio prêt/dépôt des banques.
- *eq_capgain* : Equity Capital Gain, Gain en capital sur les actions.
- *ca* : Current Account Balance, Balance courante du pays.
- *gdp* : Growt Domestic Prduct, PIB du pays.

``` r
datatrait <- data.frame(year = data$year, country = data$country, crisis = data$crisis, ltd = data$ltd, stir = data$stir, eq_capgain = data$eq_capgain, ca = data$ca, gdp = data$gdp)
summary(datatrait)
```

    ##       year        country              crisis              ltd        
    ##  Min.   :1870   Length:2719        Min.   : 0.00000   Min.   : 11.15  
    ##  1st Qu.:1907   Class :character   1st Qu.: 0.00000   1st Qu.: 76.82  
    ##  Median :1945   Mode  :character   Median : 0.00000   Median : 94.11  
    ##  Mean   :1945                      Mean   : 0.06594   Mean   : 95.86  
    ##  3rd Qu.:1983                      3rd Qu.: 0.00000   3rd Qu.:111.88  
    ##  Max.   :2020                      Max.   :88.00000   Max.   :218.16  
    ##  NA's   :1                         NA's   :50         NA's   :420     
    ##       stir          eq_capgain               ca                 gdp           
    ##  Min.   :-2.000   Min.   :        -1   Min.   :-16879001   Min.   :        0  
    ##  1st Qu.: 2.627   1st Qu.:         0   1st Qu.:      -63   1st Qu.:       54  
    ##  Median : 4.122   Median :         0   Median :        0   Median :     1811  
    ##  Mean   : 4.678   Mean   :   1167935   Mean   :   -45723   Mean   :  2454826  
    ##  3rd Qu.: 5.759   3rd Qu.:         0   3rd Qu.:       18   3rd Qu.:    50383  
    ##  Max.   :21.273   Max.   :2534419200   Max.   :  5885998   Max.   :207046579  
    ##  NA's   :199      NA's   :549          NA's   :230         NA's   :78

On remarque également que ces variables contiennent ‘relativement’ peu
de données manquantes, qui est un plus pour notre analyse.

# Premier travail sur les régressions :

Nous utiliserons ici por les régressions des modèles de type
***Logit***, nous permettant de prédire une variable qualitative (ou
indicatrice) à l’aide de variable quantitative.

## Régressions individuelles

On va tout d’abord régresser crisis par rapport à chaque variable.

### Avec *ltd*

``` r
dftemp <- data[!is.na(data$crisis) & !is.na(data$ltd), ]
logit_model1 <- glm(crisis ~ ltd, dftemp, family = binomial)
summary(logit_model1)
```

    ## 
    ## Call:
    ## glm(formula = crisis ~ ltd, family = binomial, data = dftemp)
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -4.866867   0.371501 -13.101  < 2e-16 ***
    ## ltd          0.014540   0.003162   4.599 4.25e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 674.44  on 2298  degrees of freedom
    ## Residual deviance: 654.91  on 2297  degrees of freedom
    ## AIC: 658.91
    ## 
    ## Number of Fisher Scoring iterations: 6

On remarque que *ltd* est très significative lorsque *crisis* est
regréssée toute seule avec celui-ci. Nous pouvons faire un graph
représentant la relation entre ce ratio et la probabilité de crise.

``` r
dftemp$predicted_prob <- predict(logit_model1, type = "response")

ggplot(dftemp, aes(x = ltd, y = predicted_prob)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(title = "Probabilité de crise en fonction du ratio LTD",
       x = "Loan-to-Deposit Ratio (LTD)",
       y = "Probabilité estimée de crise")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

![](econometrie_finale_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Avec *eq_capgain*

``` r
dftemp <- data[!is.na(data$crisis) & !is.na(data$eq_capgain), ]
logit_model2 <- glm(crisis ~ eq_capgain, dftemp, family = binomial)
summary(logit_model2)
```

    ## 
    ## Call:
    ## glm(formula = crisis ~ eq_capgain, family = binomial, data = dftemp)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -3.289e+00  1.153e-01 -28.518   <2e-16 ***
    ## eq_capgain  -4.056e-09  2.114e-07  -0.019    0.985    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 671.98  on 2169  degrees of freedom
    ## Residual deviance: 671.91  on 2168  degrees of freedom
    ## AIC: 675.91
    ## 
    ## Number of Fisher Scoring iterations: 12

On remarque que *eq_capgain* n’est pas du tout significatif lorsque
*crisis* est regréssée toute seule avec celui-ci. Nous pouvons faire un
graph représentant la relation entre ce ratio et la probabilité de
crise.

``` r
dftemp$predicted_prob <- predict(logit_model2, type = "response")

ggplot(dftemp, aes(x = ltd, y = predicted_prob)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
  labs(title = "Probabilité de crise en fonction de eq_capgain",
       x = "Equity Capital Gain",
       y = "Probabilité estimée de crise")
```

    ## `geom_smooth()` using formula = 'y ~ x'

    ## Warning: Removed 219 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning in eval(family$initialize): non-integer #successes in a binomial glm!

    ## Warning: Removed 219 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](econometrie_finale_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

On remarque que la variable *eq_capgain* n’a pas de pouvoir explicatif
dans ce modèle, peut être parce que la spécification ne montre pas bien
le pouvoir explicatif de la variable.

Nous allons donc régresser *crisis* avec toutes les variables :

``` r
dftemp <- data[!is.na(data$crisis) & !is.na(data$eq_capgain), ]
logit_model2 <- glm(crisis ~ eq_capgain + ltd + stir + gdp + ca, dftemp, family = binomial)
summary(logit_model2)
```

    ## 
    ## Call:
    ## glm(formula = crisis ~ eq_capgain + ltd + stir + gdp + ca, family = binomial, 
    ##     data = dftemp)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept) -5.896e+00  5.903e-01  -9.989  < 2e-16 ***
    ## eq_capgain  -6.530e+00  7.775e-01  -8.399  < 2e-16 ***
    ## ltd          1.737e-02  4.934e-03   3.522 0.000429 ***
    ## stir         9.681e-02  3.558e-02   2.721 0.006518 ** 
    ## gdp         -9.314e-08  6.697e-08  -1.391 0.164304    
    ## ca          -1.139e-06  7.315e-07  -1.557 0.119539    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 555.23  on 1833  degrees of freedom
    ## Residual deviance: 427.17  on 1828  degrees of freedom
    ##   (336 observations deleted due to missingness)
    ## AIC: 439.17
    ## 
    ## Number of Fisher Scoring iterations: 9

on remarque tout de suite que *eq_capgain* capte le plus de pouvoir
explicatif, avec un p-value extrèmement faible (**\< 2e-16**)

# Analyse des faux positifs avec une matrice de confusion :

``` r
modele_logit <- glm(crisis ~ stir + ltd + eq_capgain + ca, 
                    data = datatrait, 
                    family = binomial)

datatrait$prob_crisis <- predict(modele_logit, newdata = datatrait, type = "response")

summary(datatrait$prob_crisis)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##  0.0000  0.0058  0.0142  0.0349  0.0321  0.7510     885

``` r
datatrait$pred_crisis <- ifelse(datatrait$prob_crisis > 0.5, 1, 0)

confusionMatrix(as.factor(datatrait$pred_crisis), as.factor(datatrait$crisis))
```

    ## Warning in levels(reference) != levels(data): longer object length is not a
    ## multiple of shorter object length

    ## Warning in confusionMatrix.default(as.factor(datatrait$pred_crisis),
    ## as.factor(datatrait$crisis)): Levels are not in the same order for reference
    ## and data. Refactoring data to match.

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1   88
    ##         0  1766   58    0
    ##         1     4    6    0
    ##         88    0    0    0
    ## 
    ## Overall Statistics
    ##                                          
    ##                Accuracy : 0.9662         
    ##                  95% CI : (0.9569, 0.974)
    ##     No Information Rate : 0.9651         
    ##     P-Value [Acc > NIR] : 0.4318         
    ##                                          
    ##                   Kappa : 0.1542         
    ##                                          
    ##  Mcnemar's Test P-Value : NA             
    ## 
    ## Statistics by Class:
    ## 
    ##                      Class: 0 Class: 1 Class: 88
    ## Sensitivity           0.99774 0.093750        NA
    ## Specificity           0.09375 0.997740         1
    ## Pos Pred Value        0.96820 0.600000        NA
    ## Neg Pred Value        0.60000 0.968202        NA
    ## Prevalence            0.96510 0.034896         0
    ## Detection Rate        0.96292 0.003272         0
    ## Detection Prevalence  0.99455 0.005453         0
    ## Balanced Accuracy     0.54575 0.545745        NA

## Explication Matrice de confusion:

On voit qu’ici il y a 1766 vrai négatifs cela signifie le nombre de fois
ou le modèle a prédit correctement “pas de crise”. On voit qu’il y a 58
faux négatifs ce qui signifie que 58 crises qui ont eu lieu n’ont pas
été détectées. On voit qu’il y a 4 faux positifs cela signifie qu’à 4
reprises le modèle a prédit une crise alors qu’il n’y en avait pas. On
voit qu’il y a 6 vrais positifs cela veut dire que 6 crises ont été
correctement prédites.

On remarque aussi en regardant “Accuracy : 0.9662 “ que le modèle est
correct 96,62% du temps.

# Analyse des résultats avec AUROC :

L’objectif est de tracer des courbes ROC pour correctement visualiser le
taux de faux positif et de vrai positif, afin de montrer le pouvoir de
significatvité d’une variable.

## Code R

``` r
df_model <- datatrait %>%
  select(crisis, stir, gdp, ltd, eq_capgain, ca) %>%
  drop_na() 
# Liste des variables à tester individuellement
variables <- c("stir", "gdp", "ltd", "eq_capgain", "ca")
colors <- c("blue", "green", "purple", "orange", "brown") 

# Initialiser le graphique
plot(NULL, xlim = c(0,1), ylim = c(0,1), 
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = "Comparaison des courbes ROC - Modèle Logit")

# Ajouter une ligne diagonale pour référence (modèle aléatoire)
abline(0,1, col = "black", lty = 2)

# Boucle pour tester plusieurs modèles probit
auc_values <- c()  # Stocker les AUC

for (i in seq_along(variables)) {
  var <- variables[i]
  color <- colors[i]
  
  # Construire le modèle Probit avec UNE SEULE variable
  logit_model <- glm(as.formula(paste("crisis ~", var)), data = datatrait, family = "binomial")
  
  # Prédictions sur les mêmes données
  df_model$pred_prob <- predict(logit_model, newdata = df_model, type = "response")
  
  # Générer les données ROC
  pred <- prediction(df_model$pred_prob, df_model$crisis)
  perf <- performance(pred, "tpr", "fpr")
  
  # Tracer la courbe ROC pour cette variable
  lines(perf@x.values[[1]], perf@y.values[[1]], col = color, lwd = 2)
  
  # Ajouter une légende pour identifier chaque variable
legend("bottomright", legend = variables, col = colors, lty = 1, lwd = 2)
  
  # Calcul de l'AUC
  auc <- performance(pred, "auc")@y.values[[1]]
  auc_values <- c(auc_values, auc)
  print(paste("AUC pour", var, "(Probit):", auc))
}
```

![](econometrie_finale_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

    ## [1] "AUC pour stir (Probit): 0.66760681497175"
    ## [1] "AUC pour gdp (Probit): 0.435659427966102"
    ## [1] "AUC pour ltd (Probit): 0.671142302259883"
    ## [1] "AUC pour eq_capgain (Probit): 0.812522069209032"
    ## [1] "AUC pour ca (Probit): 0.598468396892652"

## Interprétation des résultats :

La courbe en orange est *eq_capgain*. Il s’agit de la variable donnant
le moins de faux positifs La courbe en violet est celle qui est 2ème,
c’est le ltd.

L’AUC (Area Under the Curve) mesure la capacité prédictive d’un modèle.
Pour info, un AUC = 1 -\> prédit parfaitement les crises. Ici avec le
*eq_capgain* =79,8%, signifie que le equity capgain prédit à 79,8% les
crises et le modèle

Les valeurs des AUC :

- “AUC pour *stir* : 0.606568504594821”
- “AUC pour *gdp* : 0.385129490392648”
- “AUC pour *ltd* : 0.67376775271512”
- “AUC pour *eq_capgain* : 0.798036758563074”
- “AUC pour *ca* : 0.557017543859651”

# Conclusion de cette première analyse :

On remarque que eq_capgain est la variable prédisant le mieux les crises
financières.

Mais une meilleure façon d’analyser la prédictibilité de crisis en
fonction de ces variables est d’introduire un lag dans les données.

Une autre bonne façon d’analyser est d’analyser avec des effets fixes
pays afin d’isoler les crises particulières à un pays donné.

# Nouvelle approche

Nous allons introduire des effets fixes par pays afin de prendre en
compte les spécificités structurelles propres à chaque pays qui
pourraient influencer la probabilité d’une crise. En effet, chaque pays
possède des caractéristiques économiques, institutionnelles et
politiques uniques qui ne varient pas nécessairement dans le temps, mais
qui peuvent avoir un impact significatif sur la survenue des crises
financières. En intégrant ces effets fixes, nous contrôlons les facteurs
inobservables constants dans le temps, ce qui nous permet d’isoler
l’impact des variables explicatives sur les crises. Cette approche évite
les biais d’omission et améliore la robustesse des résultats en
garantissant que les différences entre pays ne faussent pas l’estimation
des coefficients. Ainsi, les variations des variables explicatives sont
interprétées uniquement par rapport aux variations intra-pays, ce qui
renforce la validité des conclusions sur les déterminants des crises.

On va ainsi faire des modifications pour retenter la même analyse avec
OLS et des lags.

## Ajout des effets fixes pays et du lag

``` r
datatraitb <- data.frame(year = data$year, country = data$country, crisis = data$crisis, ltd = data$ltd, stir = data$stir, eq_capgain = data$eq_capgain, ca = data$ca, gdp = data$gdp)

# Fonction pour ajouter un lag à une variable avec regroupement par pays
add_lag <- function(data, var_name, lag = 1) {
  data <- data %>%
    group_by(country) %>%  # Assurer que le lag est calculé par pays
    mutate(!!paste0(var_name, "_lag") := lag(get(var_name), lag)) %>%
    ungroup()
  return(data)
}


# Liste des variables économiques à lagger
variables <- c("stir", "gdp", "ltd", "eq_capgain", "ca")

# Appliquer le lag pour chaque variable
for (var in variables) {
  datatraitb <- add_lag(datatraitb, var, lag = 1)
}

# Sélectionner les nouvelles variables avec lag et la variable cible (crisis)
lagged_vars <- paste0(variables, "_lag")

df_model <- datatraitb %>%
  select(crisis, country, all_of(lagged_vars)) %>%
  drop_na()  # Supprimer les lignes avec des valeurs manquantes dues au lag
```

## Nouveaux graphiques

``` r
# Initialiser le graphique
plot(NULL, xlim = c(0,1), ylim = c(0,1), 
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = "Courbes ROC - OLS avec effets fixes et variables laggées")

# Ajouter une ligne diagonale pour référence (modèle aléatoire)
abline(0,1, col = "black", lty = 2)

# Boucle pour tester plusieurs modèles OLS avec les variables laggées et effets fixes
colors <- c("blue", "green", "purple", "orange", "brown", "red") 
auc_values <- c()  # Stocker les AUC

for (i in seq_along(lagged_vars)) {
  var <- lagged_vars[i]
  color <- colors[i]
  
  # Construire le modèle OLS avec effets fixes par pays
  ols_model <- plm(as.formula(paste("crisis ~", var)), 
                    data = df_model, 
                    model = "within",
                    index = c("country"))

  # Prédictions sur les mêmes données
  df_model$pred_prob <- predict(ols_model, newdata = df_model)

  # Générer les données ROC
  pred <- prediction(df_model$pred_prob, df_model$crisis)
perf <- performance(pred, "tpr", "fpr")

  # Tracer la courbe ROC pour cette variable
  lines(perf@x.values[[1]], perf@y.values[[1]], col = color, lwd = 2)

  # Calcul de l'AUC
  auc <- performance(pred, "auc")@y.values[[1]]
  auc_values <- c(auc_values, auc)
  print(paste("AUC pour", var, "(OLS avec effets fixes et lag t-1):", auc))
}
```

    ## [1] "AUC pour stir_lag (OLS avec effets fixes et lag t-1): 0.667597987288134"

    ## [1] "AUC pour gdp_lag (OLS avec effets fixes et lag t-1): 0.435659427966102"

    ## [1] "AUC pour ltd_lag (OLS avec effets fixes et lag t-1): 0.671142302259883"

    ## [1] "AUC pour eq_capgain_lag (OLS avec effets fixes et lag t-1): 0.812522069209032"

    ## [1] "AUC pour ca_lag (OLS avec effets fixes et lag t-1): 0.598468396892652"

``` r
# Ajouter une légende pour identifier chaque variable
legend("bottomright", legend = lagged_vars, col = colors, lty = 1, lwd = 2)
```

![](econometrie_finale_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

Si une courbe ROC avec un lag est meilleure que la version sans lag,
cela signifie que les variations passées influencent les crises. Si
l’AUC est plus élevée avec un lag, cela montre qu’une crise est mieux
prédite par les valeurs passées de cette variable. L’utilisation des
effets fixes pays nous permet de corriger les biais liés aux différences
entre pays (ex: politiques économiques différentes).
