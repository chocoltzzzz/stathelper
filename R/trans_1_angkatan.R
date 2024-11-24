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
