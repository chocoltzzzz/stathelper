#' Transformasi Data dengan Metode Box-Cox atau Log
#'
#' Fungsi ini mentransformasikan data numerik menggunakan berbagai nilai parameter lambda,
#' dengan tujuan untuk mendekati distribusi normal. Nilai lambda dipilih berdasarkan nisbah median
#' terhadap IQR (Interquartile Range) dari data yang telah ditransformasi, untuk mendekati nilai
#' nisbah target yang ditentukan.
#'
#' @param data Vektor numerik yang akan ditransformasikan.
#' @param lambdas Vektor nilai lambda yang akan digunakan untuk transformasi. Default adalah c(-2, -1, -0.5, 0, 0.5, 1, 2).
#' @param method Metode transformasi yang digunakan. Saat ini hanya "boxcox" yang tersedia, tetapi log dapat digunakan sebagai fallback. Default adalah "boxcox".
#' @param target_nisbah Nisbah target yang diinginkan antara median dan IQR dari data yang telah ditransformasi. Default adalah 1.
#' @param adjust_negative Apakah nilai negatif dalam data harus disesuaikan agar menjadi positif sebelum transformasi. Default adalah TRUE.
#'
#' @return Sebuah list dengan elemen-elemen berikut:
#'   - `transformed_data`: Data yang telah ditransformasi menggunakan lambda yang optimal.
#'   - `optimal_lambda`: Nilai lambda yang menghasilkan nisbah median terhadap IQR yang paling mendekati target_nisbah.
#'   - `nisbah_table`: Data frame yang menunjukkan nisbah median terhadap IQR untuk setiap nilai lambda yang diuji.
#'   - `shift`: Konstanta yang ditambahkan pada data untuk menghindari nilai negatif jika `adjust_negative` diaktifkan.
#'
#' @details
#' Fungsi ini menggunakan transformasi Box-Cox atau logaritma untuk mentransformasikan data dan mencari
#' nilai lambda yang memberikan nisbah median terhadap IQR yang paling mendekati target yang ditentukan.
#' Jika data mengandung nilai negatif dan `adjust_negative = TRUE`, konstanta akan ditambahkan agar data positif
#' sebelum transformasi dilakukan.
#'
#' @examples
#' # Membuat data contoh
#' data <- rnorm(100, mean = 10, sd = 5)
#'
#' # Menggunakan fungsi transformasi untuk data
#' result <- transform_data(data, lambdas = c(-1, 0, 1), target_nisbah = 1)
#'
#' # Melihat hasil transformasi
#' result$transformed_data
#' result$optimal_lambda
#'
#' @export

transform_data <- function(data, lambdas = c(-2, -1, -0.5, 0, 0.5, 1, 2),
                           method = "boxcox", target_nisbah = 1,
                           adjust_negative = TRUE) {
  if (!is.numeric(data)) {
    stop("Data harus berupa vektor numerik.")
  }

  if (anyNA(data)) {
    stop("Data mengandung nilai NA. Harap bersihkan data terlebih dahulu.")
  }

  shift <- 0
  if (adjust_negative && any(data <= 0)) {
    shift <- abs(min(data)) + 1  # Tambahkan konstanta agar semua nilai positif
    data <- data + shift
    message(sprintf("Data telah disesuaikan dengan penambahan konstanta: %.2f", shift))
  }

  # Fungsi transformasi
  transform_func <- function(x, lambda) {
    if (lambda == 0) {
      return(log(x))
    } else {
      return((x^lambda - 1) / lambda)
    }
  }

  results <- data.frame(lambda = lambdas, nisbah = NA)
  results$transformed_data <- vector("list", length(lambdas))  # Simpan data transformasi

  for (i in seq_along(lambdas)) {
    lambda <- lambdas[i]
    transformed_data <- transform_func(data, lambda)

    median_trans <- median(transformed_data, na.rm = TRUE)
    iqr_trans <- IQR(transformed_data, na.rm = TRUE)  # Sebaran tengah
    nisbah <- abs(median_trans / iqr_trans)  # Menghitung nisbah

    results$nisbah[i] <- nisbah
    results$transformed_data[[i]] <- transformed_data
  }

  optimal_lambda <- results$lambda[which.min(abs(results$nisbah - target_nisbah))]
  optimal_transform <- transform_func(data, optimal_lambda)

  return(list(
    transformed_data = optimal_transform,
    optimal_lambda = optimal_lambda,
    nisbah_table = results,
    shift = shift
  ))
}
