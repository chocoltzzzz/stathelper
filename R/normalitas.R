#' Uji Normalitas untuk Kolom Numerik
#'
#' Fungsi ini melakukan uji normalitas untuk kolom numerik dalam dataset menggunakan beberapa metode uji,
#' termasuk Shapiro-Wilk, Kolmogorov-Smirnov, dan Anderson-Darling. Fungsi ini mengembalikan hasil statistik uji
#' dan p-value untuk setiap kolom yang diuji.
#'
#' @param data Data frame yang berisi data yang akan diuji normalitasnya.
#' @param columns Vektor nama kolom yang ingin diuji. Jika `NULL`, semua kolom numerik akan diuji.
#' @param methods Vektor metode uji normalitas yang akan digunakan. Defaultnya adalah `c("shapiro", "ks", "ad")`.
#'                Metode yang dapat dipilih adalah:
#'                - "shapiro" untuk uji Shapiro-Wilk
#'                - "ks" untuk uji Kolmogorov-Smirnov
#'                - "ad" untuk uji Anderson-Darling.
#' @return Data frame dengan hasil uji normalitas, yang mencakup:
#'   - `Column`: Nama kolom yang diuji
#'   - `Method`: Nama metode uji yang digunakan
#'   - `Statistic`: Nilai statistik uji
#'   - `P.Value`: Nilai p dari uji normalitas
#'
#' @details
#' Fungsi ini menggunakan paket `nortest` untuk uji Anderson-Darling, serta fungsi `shapiro.test` dan `ks.test` dari R
#' untuk uji Shapiro-Wilk dan Kolmogorov-Smirnov. Fungsi ini mengembalikan hasil statistik uji dan p-value untuk
#' setiap kolom numerik dalam data frame.
#'
#' @examples
#' # Membuat data contoh
#' data <- data.frame(a = rnorm(100), b = rnorm(100), c = runif(100))
#'
#' # Uji normalitas untuk kolom 'a' dan 'b' dengan metode Shapiro-Wilk dan Anderson-Darling
#' uji_normalitas(data, columns = c("a", "b"), methods = c("shapiro", "ad"))
#'
#' # Uji normalitas untuk semua kolom dengan semua metode
#' uji_normalitas(data)
#'
#' @import nortest
#' @export

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


