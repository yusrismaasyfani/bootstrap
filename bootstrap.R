# Load the required package
library(MASS)
library(boot)
library(readxl)


# Load your data
data = read_excel("E:/skripsyen/data bab 4 fix/klaster5.xlsx")

# Normalisasi data menggunakan Z-Score Scaling
normalized_data <- data
normalized_data[, -1] <- scale(normalized_data[, -1])
print(normalized_data)

# Fungsi untuk menghitung model regresi probit
probit_model <- function(data, indices) {
  d <- data[indices, ] # Resampling data
  model <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, data = d, family = binomial(link = "probit")) 
  return(coef(model)) # Mengembalikan koefisien model
}

# Lakukan bootstrap
boot_results <- boot(data = normalized_data, statistic = probit_model, R = 1000)

# Ambil koefisien dari hasil bootstrap
boot_coef <- boot_results$t

# Hitung p-value untuk setiap koefisien
p_values <- apply(boot_coef, 2, function(x) {
  lower <- quantile(x, 0.025, na.rm = TRUE)
  upper <- quantile(x, 0.975, na.rm = TRUE)
  if (lower * upper > 0) {
    p_value <- 2 * min(mean(x <= 0, na.rm = TRUE), mean(x >= 0, na.rm = TRUE))
  } else {
    p_value <- 2 * min(mean(x <= 0, na.rm = TRUE), mean(x >= 0, na.rm = TRUE))
  }
  return(p_value)
})

# Tampilkan nilai p-value
print(p_values)

# Get estimates, standard errors, and p-values from boot_results
estimates <- colMeans(boot_results$t)
se <- apply(boot_results$t, 2, sd)
p_values <- p_values # Assuming you already calculated p-values

# Combine estimates, standard errors, and p-values into a dataframe
results <- data.frame(estimates, se, p_values)
names(results) <- c("Estimates", "Standard Errors", "P-values")

# Print the results
print(results)




#Load library
library(MASS)

# Buat model probit dengan menggunakan metode probit
probit_model <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, data = normalized_data, family = binomial(link = "probit"))

# Tampilkan ringkasan model
summary(probit_model)

# Load data
data <- read_excel("E:/skripsyen/data bab 4 fix/klaster3.xlsx")

# Normalisasi data menggunakan Z-Score Scaling
normalized_data <- data
normalized_data[, -1] <- scale(normalized_data[, -1])

# Fungsi untuk menghitung model regresi probit
probit_model <- function(data, indices) {
  d <- data[indices, ] # Resampling data
  model <- glm(Y ~ X1 + X2 + X3 + X4 + X5 + X6 + X7, data = d, family = binomial(link = "probit")) 
  return(coef(model)) # Mengembalikan koefisien model
}

# Lakukan bootstrap
boot_results <- boot(data = normalized_data, statistic = probit_model, R = 1000)

# Ambil koefisien dari hasil bootstrap
boot_coef <- boot_results$t

# Hitung p-value untuk setiap koefisien
p_values <- apply(boot_coef, 2, function(x) {
  lower <- quantile(x, 0.025, na.rm = TRUE)
  upper <- quantile(x, 0.975, na.rm = TRUE)
  if (!is.na(lower) && !is.na(upper) && lower * upper > 0) {
    p_value <- 2 * min(mean(x <= 0, na.rm = TRUE), mean(x >= 0, na.rm = TRUE))
  } else {
    p_value <- 1
  }
  return(p_value)
})

# Tampilkan nilai p-value
print(p_values)


# Menghitung akurasi klasifikasi
crosstab <- table(data$Y,fitted(probit_model)>0.5)
crosstab

Akurasi <- ((3+0)/(5+0+0+3))*100
print(paste(Akurasi,"%"))
