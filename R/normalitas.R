uji_normalitas <- function(data, columns = NULL, methods = c("shapiro", "ks", "ad")) {
  if (!requireNamespace("nortest", quietly = TRUE)) {
    stop("Package 'nortest' diperlukan untuk uji Anderson-Darling. Silakan install dengan `install.packages('nortest')`.")
  }

  # Validasi input
  if (!is.data.frame(data)) stop("Data harus berupa data frame.")
  if (is.null(columns)) {
    columns <- names(data)
  }
  columns <- columns[sapply(data[columns], is.numeric)]
  if (length(columns) == 0) stop("Tidak ada kolom numerik untuk diuji.")

  # Hasil akhir
  hasil <- data.frame(Column = character(), Method = character(), Statistic = numeric(), P.Value = numeric(), stringsAsFactors = FALSE)

  # Melakukan uji normalitas untuk setiap kolom dan metode
  for (col in columns) {
    for (method in methods) {
      if (method == "shapiro") {
        test <- shapiro.test(data[[col]])
        hasil <- rbind(hasil, data.frame(Column = col, Method = "Shapiro-Wilk", Statistic = test$statistic, P.Value = test$p.value))
      } else if (method == "ks") {
        test <- ks.test(data[[col]], "pnorm", mean = mean(data[[col]]), sd = sd(data[[col]]))
        hasil <- rbind(hasil, data.frame(Column = col, Method = "Kolmogorov-Smirnov", Statistic = test$statistic, P.Value = test$p.value))
      } else if (method == "ad") {
        test <- nortest::ad.test(data[[col]])
        hasil <- rbind(hasil, data.frame(Column = col, Method = "Anderson-Darling", Statistic = test$statistic, P.Value = test$p.value))
      } else {
        stop("Metode uji tidak dikenali.")
      }
    }
  }

  return(hasil)
}


