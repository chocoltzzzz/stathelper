#' Visualisasi Data dengan Beberapa Jenis Plot
#'
#' Fungsi ini memungkinkan pembuatan berbagai jenis plot untuk visualisasi data, termasuk histogram,
#' boxplot, scatterplot, pie chart, barplot, density plot, dan heatmap korelasi. Fungsi ini
#' menggunakan `ggplot2` untuk membuat grafik dan `plotly` untuk membuat grafik interaktif.
#'
#' @param data Data frame atau tibble yang berisi data yang akan divisualisasikan.
#' @param type Jenis plot yang ingin dibuat. Pilihan yang valid: "histogram", "boxplot",
#' "scatterplot", "piechart", "barplot", "densityplot", "corrheatmap".
#' @param x Nama kolom data untuk sumbu x (untuk beberapa plot). Diperlukan untuk "histogram",
#' "boxplot", "scatterplot", "piechart", "barplot", dan "densityplot".
#' @param y Nama kolom data untuk sumbu y (untuk scatterplot). Diperlukan untuk "scatterplot".
#'
#' @return Grafik yang dihasilkan dalam bentuk plot interaktif menggunakan `plotly`. Untuk plot
#' pie chart, grafik ditampilkan menggunakan `ggplot2` biasa.
#'
#' @details
#' Fungsi ini mendukung pembuatan grafik untuk berbagai jenis visualisasi data dengan memberikan
#' fleksibilitas untuk memilih kolom yang akan digunakan di sumbu x dan y (untuk scatterplot).
#' Semua plot, kecuali pie chart, disajikan dalam bentuk grafik interaktif menggunakan `plotly`.
#' Pie chart akan ditampilkan menggunakan `ggplot2` dan tidak memiliki interaktivitas.
#'
#' @examples
#' # Membuat data contoh
#' data <- data.frame(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   category = sample(c("A", "B", "C"), 100, replace = TRUE)
#' )
#'
#' # Visualisasi histogram
#' visualize(data, type = "histogram", x = "x")
#'
#' # Visualisasi scatterplot
#' visualize(data, type = "scatterplot", x = "x", y = "y")
#'
#' # Visualisasi pie chart
#' visualize(data, type = "piechart", x = "category")
#'
#' # Visualisasi heatmap korelasi
#' visualize(data, type = "corrheatmap")
#'
#' @export

visualize <- function(data, type, x = NULL, y = NULL) {
  library(ggplot2)
  library(reshape2)
  library(dplyr)
  library(plotly)

  if (type == "histogram" && !is.null(x)) {
    histogram <- ggplot(data, aes(x = .data[[x]])) +
      geom_histogram(binwidth = 1, fill = "steelblue3", color = "black", alpha = 0.8) +
      labs(title = paste("Histogram of", x), x = x, y = "Frequency") +
      theme_minimal()
    ggplotly(histogram)

  } else if (type == "boxplot" && !is.null(x)) {
    if (!(x %in% colnames(data))) {
      stop(paste("Column", x, "does not exist in the dataset"))
    }
    boxplot <- ggplot(data, aes(y = .data[[x]])) +
      geom_boxplot(fill = "steelblue3", color = "black") +
      labs(title = paste("Boxplot of", x), y = x) +
      theme_minimal()
    ggplotly(boxplot)

  } else if (type == "scatterplot" && !is.null(x) && !is.null(y)) {
    if (!(x %in% colnames(data)) || !(y %in% colnames(data))) {
      stop(paste("Columns", x, "or", y, "do not exist in the dataset"))
    }
    scatterplot <- ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
      geom_point(color = "steelblue3", alpha = 0.8) +
      labs(title = paste("Scatterplot of", x, "vs", y), x = x, y = y) +
      theme_minimal()
    ggplotly(scatterplot)

  } else if (type == "piechart" && !is.null(x)) {
    pie_data <- data %>%
      count(.data[[x]]) %>%
      mutate(percentage = n / sum(n) * 100,
             label = paste0(round(percentage, 1), "%"))

    piechart <- ggplot(pie_data, aes(x = "", y = percentage, fill = as.factor(.data[[x]]))) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      labs(title = paste("Pie Chart of", x), x = NULL, y = NULL) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5), color = "white") +
      theme_void() +
      theme(legend.title = element_blank())
    print(piechart)

  } else if (type == "barplot" && !is.null(x)) {
    barplot <- ggplot(data, aes(x = .data[[x]])) +
      geom_bar(fill = "steelblue3", color = "black", alpha = 0.5) +
      labs(title = paste("Bar Plot of", x), x = x, y = "Count") +
      theme_minimal()
    ggplotly(barplot)

  } else if (type == "densityplot" && !is.null(x)) {
    densityplot <- ggplot(data, aes(x = .data[[x]])) +
      geom_density(fill = "steelblue3", color = "black", alpha = 0.5) +
      labs(title = paste("Density Plot of", x), x = x, y = "Density") +
      theme_minimal()
    ggplotly(densityplot)

  } else if (type == "corrheatmap") {
    corr_data <- cor(data, use = "pairwise.complete.obs")
    corr_data_melt <- melt(corr_data)

    heatmap <- ggplot(corr_data_melt, aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red1", mid = "white", midpoint = 0) +
      theme_minimal() +
      labs(title = "Correlation Matrix Heatmap", x = "", y = "") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(heatmap)

  } else {
    stop("Invalid parameters or missing required arguments")
  }
}


