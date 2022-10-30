df = readRDS("/Users/artyomkraevskiy/Desktop/Модели с качественными и ограниченными зависимыми переменными/ДЗ1/homework.rds")

head(df)
# Часть 1
#1.1
# Проверим, что выбранные переменные не коррелированы
corr_ser_int <- cor.test(df$series, df$internet,
                         method = "pearson")
corr_ser_tv <- cor.test(df$series, df$TV,
                        method = "pearson")
corr_int_tv <- cor.test(df$internet, df$TV,
                        method = "pearson")
corr_ser_male <- cor.test(df$series, df$male,
                          method = "pearson")
corr_int_male <- cor.test(df$internet, df$male,
                          method = "pearson")
corr_TV_male <- cor.test(df$TV, df$male,
                          method = "pearson")

corr_ser_int
corr_ser_tv
corr_int_tv
corr_ser_male
corr_int_male
corr_TV_male
# Часть 2
#2.1
# Оценим параметры линейной вероятностной модели
model.lm <- lm(formula = sub ~ internet + 
                 series +
                 male + 
                 I(internet * series) +
                 I(series ^ 2),
               data = df)
summary(model.lm)

# 2.3
internet_ef = mean(coef(model.lm)['internet'] + coef(model.lm)['I(internet * series)'] * df$series)
series_ef = mean(coef(model.lm)['series'] + coef(model.lm)['I(internet * series)'] * df$internet + 2 * coef(model.lm)['I(series^2)'] * df$series)
male_ef = coef(model.lm)['male']
data.frame ('Эффект_internet'  = internet_ef,
            'Эффект_series' = series_ef,
            'Эффект_male' = male_ef
)

df$const = 1

# 2.4
x = data.frame('(Intercept)'=df$const,
               internet=df$internet,
               series=df$series,
               male=df$male,
               'I(internet * series)'=df$internet * df$series,
               'I(series^2)'=df$series * df$series)
x = data.matrix(x)
a = solve((t(x) %*% x)) %*% t(x) %*% diag(residuals(model.lm)^2) %*% x %*% solve((t(x) %*% x))


data.frame(
  '(Intercept)'=2 * pt(-abs(coef(model.lm)['(Intercept)']/(a['X.Intercept.', 'X.Intercept.'])^(1/2)),df=4994),
  internet=2 * pt(-abs(coef(model.lm)['internet']/(a['internet', 'internet'])^(1/2)),df=4994),
  series=2 * pt(-abs(coef(model.lm)['series']/(a['series', 'series'])^(1/2)),df=4994),
  male=2 * pt(-abs(coef(model.lm)['male']/(a['male', 'male'])^(1/2)),df=4994),
  'I(internet * series)'=2 * pt(-abs(coef(model.lm)['I(internet * series)']/(a['I.internet...series.', 'I.internet...series.'])^(1/2)),df=4994),
  'I(series^2)'=2 * pt(-abs(coef(model.lm)['I(series^2)']/(a['I.series.2.', 'I.series.2.'])^(1/2)),df=4994)
)

# Часть 3
# 3.1 


library("mvtnorm")  
library("numDeriv") 

model_probit <- glm(formula = sub ~ internet + 
                      series +
                      male + 
                      internet * series +
                      I(series ^ 2),
                    data = df,
               
               family = binomial(link = "probit") )
summary(model_probit)

# 3.3
library("mfx")                                                    
library("margins")

probit_ME <- probitmfx(formula = formula(model_probit),           
                       data = df,                  
                       atmean = FALSE)  
print(probit_ME)


# обратимся к мужчинам
male_frame <- model_probit$data                                     # возьмем изначальный датафрейм и превратим
male_frame$male <- 1                                                # в нем всех индивидов в мужчин
prob_male <- predict(model_probit, newdata = male_frame, type = 'response')            # считаем вероятности занятости для мужчин
# обратимся к женщинам
female_frame <- model_probit$data                                   # возьмем изначальный датафрейм и превратим
female_frame$male <- 0                                              # в нем всех индивидов в женщин
prob_female <- predict(model_probit, newdata = female_frame, type = 'response')        # считаем вероятности занятости для женщин
# оценим предельный эффект переменной male
male_ME <- prob_male - prob_female                                  # для каждого индивида                       
mean(male_ME)                                                       # средний


# Предскажем значение лин. индекса
probit_preds <- predict(model_probit, 
                        data = df
                        )

# 3.5
# Предскажем значение лин. индекса probit
probit_preds <- predict(model_probit, 
                        data = df
)

# Для нормального распределения трешхолд для линейного индекса равен 0.5
threshold_probit = 0
pred_probit <- as.numeric(probit_preds >= threshold_probit)       
correct_probit <- mean(pred_probit == df$sub)   
print(correct_probit)

# Предскажем значение вероятности для линенйной вероятностной модели
lm_prob_preds <- predict(model.lm, 
                        data = df,
                        type = 'response'
                        )
threshold_lm = 0.5
pred_lm <- as.numeric(lm_prob_preds >= threshold_lm)       
correct_lm <- mean(pred_lm == df$sub) 
print(correct_lm)

# Наивный прогноз

sum(df$sub)/length(df$sub)
# 0 - самый распространенный класс
correct_naive <- 1 - (sum(df$sub)/length(df$sub))

df_forecast_comparison <- data.frame('probit' = correct_probit, 
   'linear_model' =correct_lm,
   'naive' = correct_naive
)

# 3.6 
# т.к. функция margins очень нестабильно работает, будем брутфорсно генерить переменные
model_probit <- glm(formula = sub ~ internet + 
                      series +
                      male + 
                      internet * series +
                      age +
                      I(series ^ 2),
                    data = df,
                    
                    family = binomial(link = "probit") )


Artyom <- data.frame(
  'income' = 10000,
  'age' = 21,
  'internet' = 0.7,
  'series' = 5,
  'health' = 'medium',
  'male' = 1,
  'marriage' = 0,
  'residence'  = 'City',
  'cat' = 0,
  'news' = -2,
  'sub' = 0,
  'TV' = 0
)


library('margins')


summary(margins(model_probit, 
                type = "response"
),
level = 0.95
)


margins(model_probit, 
                type = "response",
        at = Artyom
)




# 3.8

cov_MLE <- vcov(model_probit)


# Часть 4.
# 4.1

model_probit <- glm(formula = sub ~ internet + 
                      series +
                      male + 
                      I(internet * series) +
                      I(series ^ 2),
                    data = df,
                    
                    family = binomial(link = "probit") )
summary(model_probit)



library("numDeriv")                                        # численно дифференцирование

# Оценим пробит модель

# Запишем функцию правдоподобия
# для модели со случайно ошибкой
# из распределения Пирсона
ProbitLnLExtended <- function(par,                         # вектор значений параметров
                              y,                           # зависимая переменная 
                              X,                           # матрица независимых переменных
                              is_aggregate = TRUE)         # при TRUE возвращаем логарифм
  # функции правдоподобия, а при
  # FALSE возвращаем вектор вкладов
{
  beta <- matrix(par[-c(1, 2)], ncol = 1)                  # вектор beta коэффициентов и
  theta <- matrix(par[c(1, 2)], ncol = 1)                  # вектор дополнительных параметров  
  # переводим в матрицу с одним столбцом
  y_li <- X %*% beta                                       # оценка линейного индекса
  y_est <- y_li + theta[1] * y_li ^ 2 +                    # оценка математического ожидания 
    theta[2] * y_li ^ 3                             # латентной переменной
  
  n_obs <- nrow(X)                                         # количество наблюдений
  
  L_vec <- matrix(NA, nrow = n_obs,                        # вектор столбец вкладов наблюдений
                  ncol = 1)                                # в функцию правдоподобия
  
  is_y_0 <- (y == 0)                                       # вектор условий (y = 0)
  is_y_1 <- (y == 1)                                       # вектор условий (y = 1)
  
  L_vec[is_y_1] <- pnorm(y_est[is_y_1])                    # вклад наблюдений для которых yi = 1
  L_vec[is_y_0] <- 1 - pnorm(y_est[is_y_0])                # вклад наблюдений для которых yi = 0
  
  lnL_vec <- log(L_vec)                                    # логарифмы вкладов
  
  if(!is_aggregate)                                        # возвращаем вклады
  {                                                        # при необходимости
    return(lnL_vec)
  }
  
  lnL <- sum(lnL_vec)                                      # логарифм функции правдоподобия
  
  return(lnL)
}
# Воспользуемся созданной функцией
# Оценки модели при справедливом ограничении,
# накладываемом нулевой гипотезой
beta.est <- coef(model_probit)                             # достаем оценки из обычной пробит
beta.R <- c(0, 0, beta.est)                                # модели и приравниваем значения
names(beta.R)[c(1, 2)] <- c("theta1", "theta2")            # дополнительных параметров к значениям,
# предполагаемым нулевой гипотезой
print(beta.R)
# Создадим матрицу регрессоров
X.mat <- as.matrix(model.frame(model_probit))              # достаем датафрейм с регрессорами и
X.mat[, 1] <- 1                                            # первращаем его в матрицу, а также
colnames(X.mat)[1] <- "Intercept"                          # заменяем зависимую переменную на константу
head(X.mat, 5)
# Применим функцию
lnL.R <- ProbitLnLExtended(beta.R, df$sub, X.mat)           # считаем логарифм функции правоподобия
# при ограничениях, совпадающую с логарифмом
# функции правдоподобия обычной пробит модели
lnL.R.grad <- grad(func = ProbitLnLExtended,               # считаем градиент данной функции
                   x = beta.R,                             # численным методом
                   y = df$sub, 
                   X = X.mat)
lnL.R.grad <- matrix(lnL.R.grad, ncol = 1)                 # градиент как матрица с одним столбцом
lnL.R.Jac <- jacobian(func = ProbitLnLExtended,            # считаем Якобин данной функции
                      x = beta.R,                          # численным методом
                      y = df$sub, 
                      X = X.mat,
                      is_aggregate = FALSE)
# Реализуем тест
LM.value.1 <- t(lnL.R.grad) %*%                            # считаем статистику теста
  solve(t(lnL.R.Jac) %*% lnL.R.Jac) %*%                    # множителей Лагранжа
  lnL.R.grad
p.value_1 <- 1 - pchisq(LM.value.1, df = 2)                # рассчитываем p-value теста

# множителей Лагранжа
# H0 остатки нормально theta_1, theta_2 = 0 


# 4.2 

# H0: tau = 0

library(glmx)
model_hetprobit <- hetglm(formula = sub ~ internet + 
                      series +
                      male + 
                      I(internet * series) +
                      I(series ^ 2)|
                        TV + age + series,
                    data = df,
                    
                    family = binomial(link = "probit"),
                    link.scale = 'log')
summary(model_probit)


lrtest(model_hetprobit, model_probit)

# В LM тесте нам не нужно выдвигать предпосылки о "длинной" модели 


# 4.3 

model_probit <- glm(formula = sub ~ internet + 
                      series +
                      male +
                      age +
                      I(internet * series) +
                      I(series ^ 2),
                    data = df,
                    
                    family = binomial(link = "probit") )
summary(model_probit)

model_hetprobit <- hetglm(formula = sub ~ internet + 
                            series +
                            male + 
                            I(internet * series) +
                            age +
                            I(series ^ 2)|
                            TV + age + series,
                          data = df,
                          
                          family = binomial(link = "probit"),
                          link.scale = 'log')
summary(model_probit)

# Для вероятности 

beta.est <- model_hetprobit$coefficients$mean              # оценки коэффициентов при переменных
# основного уравнения
tau.est <- model_hetprobit$coefficients$scale   


prob.df <- predict(model_hetprobit, newdata = df,    # оценка вероятности 
                      type = "response")                   # дефолта 
li.df.adj <- predict(model_hetprobit, newdata = df,  # оценка отношения линейного
                        type = "link")                     # индекса к стнадртному
                                                          # отклонению случайно ошибки
sigma.df <- predict(model_hetprobit, newdata = df,   # оценка стандартного
                       type = "scale")                     # отклонения случайной
# ошибки 
model_hetprobit <- hetglm(formula = sub ~ internet + 
                            series +
                            male + 
                            internet * series+
                             age +
                            I(series ^ 2)|
                            TV + age + series,
                          data = df,
                          
                          family = binomial(link = "probit"),
                          link.scale = 'log')
summary(model_probit)

li.df <- li.df.adj * sigma.df

ME.df <- margins(model_hetprobit, 
                    data = df)
summary(ME.df)
ME.pay <- ME.df$dydx_series

ME.pay.1 <- dnorm(li.df, sd = sigma.df) * 
  (beta.est["age"] - 
     li.df * tau.est["age"])

ME.pay.1 

mean(ME.pay.1)

# Теперь для дисперсии ошибки

delta <- 1e-6                                              # приращение                                                 
df.delta <- df
df.delta$series <- df$series + delta                       # приращение по возрасту

var.df.delta <- predict(model_hetprobit, 
                            newdata = df.delta,
                            type = "scale")

var.df <- predict(model_hetprobit, 
                        newdata = df,
                        type = "scale")

ME.pay.var = (var.df.delta - var.df)/delta

mean(ME.pay.var)

#4.4

# 1) Коэффициент при линейной части равняется нулю

# Оценим полную модель
model.probit.F <- glm(formula = sub ~ internet + 
                         series +
                         male + 
                         I(internet * series) +
                         I(series ^ 2),
                       data = df,
                       
                       family = binomial(link = "probit") )


model.probit.R <- glm(formula = sub ~ internet + 
                        male + 
                        I(internet * series) +
                        I(series ^ 2),
                      data = df,
                      
                      family = binomial(link = "probit") )

lrtest(model.probit.F, model.probit.R) 
# длинная модель лучше
# 2) Оба коэф. равны нулю

model.probit.R <- glm(formula = sub ~ internet + 
                        male + 
                        I(internet * series)
                        ,
                      data = df,
                      
                      family = binomial(link = "probit") )

lrtest(model.probit.F, model.probit.R) 
# длинная модель лучше 

# 3) Один в k раз больше другого

model.probit.R <- glm(formula = sub ~ internet + 
                        male + 
                        I(internet * series) +
                        I(series*(1+series)),
                      data = df,
                      
                      family = binomial(link = "probit") )

lrtest(model.probit.F, model.probit.R) 

# для k=1 исходная модель лучше 

# 4) t =0.3

model.probit.R <- glm(formula = sub ~ internet + 
                        I(internet * series) +
                        I(series*(1+series)),
                      offset =I(0.3 * male),
                      data = df,
                      
                      family = binomial(link = "probit") )

lrtest(model.probit.F, model.probit.R) 


# 4.5 

# Альтернатинвый вариант тестирования
model.probit.F <- glm(formula = sub ~ internet + 
                        series +
                        male + 
                        I(internet * series) +
                        I(series ^ 2) +
                        I(male * internet) + 
                        I(male * series) + 
                        I(male * internet * series) +
                        I(male * series ^ 2)
                        ,
                      data = df,
                      
                      family = binomial(link = "probit"))

model.probit.R <- glm(formula = sub ~ internet + 
                        series +
                        male + 
                        I(internet * series) +
                        I(series ^ 2),
                      data = df,
                      
                      family = binomial(link = "probit") )


# с помощью встроенной функции
lrtest(model.probit.F, model.probit.R)

# нужно оценивать две разные модели


# 4.6
# Оценим совместную модель 
install.packages('fastDummies')
library('fastDummies')
df <- dummy_cols(df, select_columns = 'residence')

model.probit.F <- glm(formula = sub ~ internet + 
                        series +
                        male + 
                        I(internet * series) +
                        I(series ^ 2) +
                        I(residence_Capital * internet) +
                        I(residence_Capital * male) + 
                        I(residence_Capital * series) + 
                        I(residence_Capital * internet * series) +
                        I(residence_Capital * series ^ 2)+
                        I(residence_City * internet) + 
                        I(residence_City * male) + 
                        I(residence_City * series) + 
                        I(residence_City * internet * series) +
                        I(residence_City * series ^ 2)
                      ,
                      data = df,
                      
                      family = binomial(link = "probit"))

lrtest(model.probit.F, model.probit.R)

# Модели статистически различимы - нужно оценивать разные модели для каждого типа населенного пункта в отдельности

# Часть 5
# 5.1 

model_logit <- glm(formula = sub ~ internet + 
                      series +
                      male + 
                      internet * series +
                      I(series ^ 2),
                    data = df,
                    
                    family = binomial(link = "logit") )
summary(model_logit)

# 5.2

# В качестве исключения добавим age

model_logit <- glm(formula = sub ~ internet + 
                      series +
                      male + 
                      internet * series +
                      age +
                      I(series ^ 2),
                    data = df,
                    
                    family = binomial(link = "logit") )
summary(model_logit)

coef.logit <- coef(model_logit)
step <- 0.1 

OR.age <- exp(coef.logit["age"] * step)
OR.male <- exp(coef.logit["male"])

# 5.3 

OR.series_particular <- exp(coef.logit["series"] * step +                   
                              coef.logit["I(series^2)"] * step ^ 2 +
                              2 * coef.logit["I(series^2)"] * 
                              Artyom$series * step+
                              coef.logit["internet:series"]* Artyom$internet* step)

OR.internet <- exp(coef.logit["internet"] * step +                   
                     coef.logit["internet:series"]* step * Artyom$series
)
# Часть 6
# 6.1 


library("GJRM")

model_1 <- sub ~ internet + series + age + male + I(internet * series) + I(series^2)
model_2 <- TV ~ age + income + cat                                # для каждого из уравнений системы
model_bp <- gjrm(formula = list(model_1,              # задаем лист, состоящий из 
                                model_2),             # обеих формул
                 data = df,
                 Model = "B",                           # указываем тип модели как систему
                 # бинанрных уравнений
                 margins = c("probit", "probit"),       # задаем маржинальные распределения
                 # случайных ошибок уравнений
                 BivD = "N")                            # задаем тип совместного распределения
# случайных ошибок (копулу)

summary(model_bp)        

# 6.2


rho_est <- model_bp$theta 
rho_est

# 6.3
model_1_unit <- glm(formula = sub ~ internet + series + age + male + I(internet * series) + I(series^2),
                    
                   data = df,
                   
                   family = binomial(link = "probit") )
summary(model_1_unit)

Log_lik_m1 = logLik(model_1_unit)

model_2_unit <- glm(formula = TV ~ age + income + cat,
                    
                    data = df,
                    
                    family = binomial(link = "probit") )
summary(model_2_unit)

Log_lik_m2 = logLik(model_2_unit)

r <- 1          # число ограничений
t <- 2 * (logLik(model_bp) - Log_lik_m1 - Log_lik_m2 )                                   # статистика теста
p.value_LR <- as.numeric(1 - pchisq(t, df = r)) 
p.value_LR
# Отвергаем нулевую гипотезу - оцениваем систему


#6.4

B_sub <- model_bp$coefficients[1:7]
B_TV <- model_bp$coefficients[8:11]

B_sub <- matrix(B_sub, ncol = 1) 
B_TV <- matrix(B_TV, ncol = 1) 

B_sub
B_TV

X_1 <- c(1, 0.7, 5, 21, 1, 3.5, 25)
X_1 <- matrix(X_1, ncol = 1)


z_li_sub <- t(X_1)%*%B_sub
p_sub <- pnorm(z_li_sub) # искомая вероятость

p_sub

#2) Вероятность того, что индивид смотрит телевизор по крайней мере раз в неделю

X_2 <- c(1, 21, 10000, 0) # вектор с рандомными характеристиками индивида
X_2 <- matrix(X_2, ncol = 1)
z_li_TV <- t(X_2)%*%B_TV
p_TV <- pnorm(z_li_TV) # искомая вероятость
p_TV


#3) Вероятность того, что индивид и имеет подписку, и смотрит телевизор не реже раза в неделю

install.packages("pbivnorm") 
library("pbivnorm") 
p_sub_TV <- pbivnorm(x = cbind(z_li_sub, z_li_TV), rho = rho_est) # искомая вероятость

p_sub_TV


#4) Вероятность того, что у индивида имеется подписка, при условии, что он смотрит телевизор реже раза в неделю


p_sub_cond_TV <- p_sub_TV / p_TV # искомая вероятость

p_sub_cond_TV


# 6.5


fx1B1 <- dnorm(z_li_sub)
fx2B2 <- dnorm(z_li_TV)

Fx1B1_px2B2 <- pnorm((z_li_sub - rho_est * z_li_TV)/(1-rho_est^2)^(1/2))
Fx2B2_px1B1 <- pnorm((z_li_TV - rho_est * z_li_sub)/(1-rho_est^2)^(1/2))

B1_age <- model_bp$coefficients[7]
B2_age <- model_bp$coefficients[10]


d_P_TV_1_d_age <- Fx2B2_px1B1 * B2_age

d_P_sub_1_TV_1_d_age <- fx1B1 * Fx2B2_px1B1 * B2_age + fx2B2 * Fx1B1_px2B2 * B1_age 

d_P_sub_1_cond_TV_1_d_age <- (d_P_TV_1_d_age * p_TV - p_sub_TV * d_P_TV_1_d_age)/(p_TV)^2 #искомый предельный эффект 
d_P_sub_1_cond_TV_1_d_age

# Часть 7
# 7.1

install.packages('caret')
library(caret)

confusionMatrix(factor((predict(model.lm, newdata = df) >= 0.5)*1),factor(df$sub))
confusionMatrix(factor((predict(model_probit, newdata = df) >= 0)*1),factor(df$sub))
confusionMatrix(factor((predict(model_logit, newdata = df) >= 0)*1),factor(df$sub))
confusionMatrix(factor((predict(model_bp, newdata = df, 1) >= 0)*1),factor(df$sub))

# 7.2

a = data.frame ('Линейно-вероятностная модель'  = c(BIC(model.lm), AIC(model.lm)),
                'Пробит-регрессия' = c(BIC(model_probit), AIC(model_probit)),
                'Логит-регрессия' = c(BIC(model_logit), AIC(model_logit))
)
row.names(a) = c('BIC', 'AIC')
a















