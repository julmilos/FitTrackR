#' @title Obliczanie wag obiektywnych i subiektywnych
#' @description Wyznaczanie wag metodami: "equal", "sd", "gini", "bwm".
#' @param macierz_decyzyjna Rozmyta macierz (liczby trojkatne l, m, u).
#' @param metoda Wybrana metoda.
#' @param ... Dodatkowe parametry dla BWM: nazwy_kryteriow, najlepsze, najgorsze.
#' @export
oblicz_wagi <- function(macierz_decyzyjna, metoda = "equal", ...) {
  n_kryteriow <- ncol(macierz_decyzyjna) / 3
  
  # Defuzzyfikacja macierzy do wag obiektywnych (GMIR)
  .ostra_macierz <- function(m) {
    n_k <- ncol(m) / 3
    o <- matrix(0, nrow(m), n_k)
    for(j in 1:n_k) {
      idx <- (j-1)*3 + 1
      o[,j] <- (m[,idx] + 4*m[,idx+1] + m[,idx+2]) / 6
    }
    return(o)
  }

  wagi_ostre <- switch(tolower(metoda),
    "equal" = rep(1/n_kryteriow, n_kryteriow),
    
    "sd" = {
      ostra <- .ostra_macierz(macierz_decyzyjna)
      sds <- apply(ostra, 2, sd)
      if(sum(sds) == 0) rep(1/n_kryteriow, n_kryteriow) else sds / sum(sds)
    },
    
    "gini" = {
      ostra <- .ostra_macierz(macierz_decyzyjna)
      g_coeffs <- apply(ostra, 2, function(x) {
        n <- length(x)
        if(mean(x) == 0) return(0)
        sum(outer(x, x, function(a, b) abs(a - b))) / (2 * n^2 * mean(x))
      })
      if(sum(g_coeffs) == 0) rep(1/n_kryteriow, n_kryteriow) else g_coeffs / sum(g_coeffs)
    },
    
    "bwm" = {
      args <- list(...)
      if (is.null(args$najlepsze) || is.null(args$najgorsze)) {
        stop("Dla metody BWM musisz podac wektory 'najlepsze' i 'najgorsze'.")
      }
      .silnik_bwm(args$najlepsze, args$najgorsze)
    },
    
    stop("Nieznana metoda. Dostepne: 'equal', 'sd', 'gini', 'bwm'.")
  )
  
  # Rozszerzenie wag na format rozmyty (każde L, M, U dostaje wagę)
  return(rep(wagi_ostre, each = 3))
}

#' @keywords internal
.silnik_bwm <- function(najlepsze, najgorsze) {
  if (!requireNamespace("Rglpk", quietly = TRUE)) {
    stop("Wymagany pakiet 'Rglpk'. Zainstaluj go: install.packages('Rglpk')")
  }
  
  n <- length(najlepsze)
  indeks_ksi <- n + 1
  cel <- c(rep(0, n), 1)
  
  mat_lhs <- matrix(nrow = 0, ncol = n + 1)
  dir_vec <- c()
  rhs_vec <- c()
  
  # Suma wag = 1
  mat_lhs <- rbind(mat_lhs, c(rep(1, n), 0))
  dir_vec <- c(dir_vec, "==")
  rhs_vec <- c(rhs_vec, 1)
  
  idx_best <- which(najlepsze == 1)[1]
  idx_worst <- which(najgorsze == 1)[1]
  
  for(j in 1:n) {
    if(j != idx_best) {
      row1 <- rep(0, n + 1); row1[idx_best] <- 1; row1[j] <- -najlepsze[j]; row1[indeks_ksi] <- -1
      mat_lhs <- rbind(mat_lhs, row1); dir_vec <- c(dir_vec, "<="); rhs_vec <- c(rhs_vec, 0)
      row2 <- row1 * -1; row2[indeks_ksi] <- -1
      mat_lhs <- rbind(mat_lhs, row2); dir_vec <- c(dir_vec, "<="); rhs_vec <- c(rhs_vec, 0)
    }
    if(j != idx_worst) {
      row3 <- rep(0, n + 1); row3[j] <- 1; row3[idx_worst] <- -najgorsze[j]; row3[indeks_ksi] <- -1
      mat_lhs <- rbind(mat_lhs, row3); dir_vec <- c(dir_vec, "<="); rhs_vec <- c(rhs_vec, 0)
      row4 <- row3 * -1; row4[indeks_ksi] <- -1
      mat_lhs <- rbind(mat_lhs, row4); dir_vec <- c(dir_vec, "<="); rhs_vec <- c(rhs_vec, 0)
    }
  }
  
  sol <- Rglpk::Rglpk_solve_LP(cel, mat_lhs, dir_vec, rhs_vec, max = FALSE)
  return(sol$solution[1:n])
}