df <- read.csv("D:/CARIER/Porto/insurance.csv")
str(df)
summary(df)
colSums(is.na(df))
# kolom teks diubah jadi kategori
df$sex <- as.factor(df$sex)
df$smoker <- as.factor(df$smoker)
df$region <- as.factor(df$region)

# kalau belum punya: install.packages("ggplot2")
library(ggplot2)

# 1. distribusi biaya
ggplot(df, aes(x = charges)) +
  geom_histogram(bins = 30, fill = "#1F3A5F", color = "white")

# 2. biaya perokok vs bukan
ggplot(df, aes(x = smoker, y = charges)) +
  geom_boxplot(fill = "#D4A62A")

# 3. BMI vs biaya, diwarnai perokok atau bukan
ggplot(df, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.6)

# bagi data: 80% latih, 20% uji
set.seed(123)
idx <- sample(nrow(df), 0.8 * nrow(df))
train <- df[idx, ]
test <- df[-idx, ]

# empat model
m1 <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train)
m2 <- lm(log(charges) ~ age + sex + bmi + children + smoker + region, data = train)
m3 <- glm(charges ~ age + sex + bmi + children + smoker + region,
          family = Gamma(link = "log"), data = train)
m4 <- glm(charges ~ age + sex + children + region + smoker * bmi,
          family = Gamma(link = "log"), data = train)

# bandingkan error di data uji
rmse <- function(a, p) sqrt(mean((a - p)^2))
hasil <- c(
  linear = rmse(test$charges, predict(m1, test)),
  log_linear = rmse(test$charges, exp(predict(m2, test))),
  gamma = rmse(test$charges, predict(m3, test, type = "response")),
  gamma_interaksi = rmse(test$charges, predict(m4, test, type = "response"))
)
round(hasil, 0)

# model linear dengan interaksi
m5 <- lm(charges ~ age + sex + children + region + smoker * bmi, data = train)

mae <- function(a, p) mean(abs(a - p))
pred <- list(
  linear = predict(m1, test),
  linear_interaksi = predict(m5, test),
  gamma = predict(m3, test, type = "response"),
  gamma_interaksi = predict(m4, test, type = "response")
)
data.frame(
  RMSE = round(sapply(pred, function(p) rmse(test$charges, p))),
  MAE = round(sapply(pred, function(p) mae(test$charges, p)))
)

# ulangi 30 kali dengan pembagian data acak
set.seed(1)
ulang <- replicate(30, {
  i <- sample(nrow(df), 0.8 * nrow(df))
  tr <- df[i, ]; te <- df[-i, ]
  a <- lm(charges ~ age + sex + bmi + children + smoker + region, data = tr)
  b <- lm(charges ~ age + sex + children + region + smoker * bmi, data = tr)
  g <- glm(charges ~ age + sex + bmi + children + smoker + region,
           family = Gamma(link = "log"), data = tr)
  h <- glm(charges ~ age + sex + children + region + smoker * bmi,
           family = Gamma(link = "log"), data = tr)
  c(linear = rmse(te$charges, predict(a, te)),
    linear_interaksi = rmse(te$charges, predict(b, te)),
    gamma = rmse(te$charges, predict(g, te, type = "response")),
    gamma_interaksi = rmse(te$charges, predict(h, te, type = "response")))
})
round(rowMeans(ulang))

# model terpilih, dilatih pada seluruh data untuk interpretasi
m_final <- lm(charges ~ age + sex + children + region + smoker * bmi, data = df)
summary(m_final)

# plot diagnostik residual
par(mfrow = c(2, 2))
plot(m_final)
df$obese <- as.factor(ifelse(df$bmi >= 30, "yes", "no"))

set.seed(1)
ulang2 <- replicate(30, {
  i <- sample(nrow(df), 0.8 * nrow(df))
  tr <- df[i, ]; te <- df[-i, ]
  a <- lm(charges ~ age + sex + children + region + smoker * bmi, data = tr)
  b <- lm(charges ~ age + sex + children + region + smoker * obese, data = tr)
  c <- lm(charges ~ age + sex + children + region + smoker * obese + smoker:bmi, data = tr)
  c(bmi_interaksi = rmse(te$charges, predict(a, te)),
    obese_interaksi = rmse(te$charges, predict(b, te)),
    gabungan = rmse(te$charges, predict(c, te)))
})
round(rowMeans(ulang2))

m_obese <- lm(charges ~ age + sex + children + region + smoker * obese, data = df)
summary(m_obese)

par(mfrow = c(2, 2))
plot(m_obese)

# rata-rata biaya aktual per kelompok
aggregate(charges ~ smoker + obese, data = df, FUN = mean)
table(df$smoker, df$obese)
