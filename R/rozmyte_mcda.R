#' @title Rozmyty TOPSIS (Prawidlowa Arytmetyka)
#' @export
metoda_topsis <- function(macierz, typy, wagi) {
  n_alt <- nrow(macierz); k <- ncol(macierz) / 3
  norm <- matrix(0, n_alt, 3*k)
  
  # Liniowa normalizacja rozmyta (zapobiega gubieniu trójkąta)
  for(j in 1:k) {
    idx <- (j-1)*3 + 1
    if(typy[j] == "max") { 
      max_u <- max(macierz[, idx+2])
      norm[, idx:(idx+2)] <- macierz[, idx:(idx+2)] / max_u 
    } else { 
      min_l <- min(macierz[, idx])
      # odwrocenie dla kosztu: min/U, min/M, min/L
      norm[, idx:(idx+2)] <- min_l / macierz[, c(idx+2, idx+1, idx)] 
    }
  }
  
  wazona <- sweep(norm, 2, wagi, "*")
  
  # Punkty idealne dla znormalizowanej macierzy (1*waga to ideal, 0 to anty-ideal)
  piz <- rep(0, 3*k); niz <- rep(0, 3*k)
  for(j in 1:k) {
    idx <- (j-1)*3 + 1
    piz[idx:(idx+2)] <- c(1,1,1) * wagi[idx]
    niz[idx:(idx+2)] <- c(0,0,0)
  }
  
  # Odleglosci Euklidesowe miedzy liczbami rozmytymi
  dp <- rep(0, n_alt); dn <- rep(0, n_alt)
  for(i in 1:n_alt) {
    suma_p <- 0; suma_n <- 0
    for(j in 1:k) {
      idx <- (j-1)*3 + 1
      suma_p <- suma_p + (1/3) * sum((wazona[i, idx:(idx+2)] - piz[idx:(idx+2)])^2)
      suma_n <- suma_n + (1/3) * sum((wazona[i, idx:(idx+2)] - niz[idx:(idx+2)])^2)
    }
    dp[i] <- sqrt(suma_p); dn[i] <- sqrt(suma_n)
  }
  
  cc <- dn / (dp + dn)
  data.frame(Alternatywa = 1:n_alt, D_plus = dp, D_minus = dn, Wynik = cc, Ranking = rank(-cc, ties.method = "first"))
}

#' @title Rozmyty VIKOR
#' @export
metoda_vikor <- function(macierz, typy, wagi, v = 0.5) {
  n_alt <- nrow(macierz); k <- ncol(macierz) / 3
  
  f_star <- rep(0, k); f_minus <- rep(0, k)
  for(j in 1:k) {
    idx <- (j-1)*3 + 1
    if(typy[j] == "max") { 
      f_star[j] <- max(macierz[,idx+2]); f_minus[j] <- min(macierz[,idx]) 
    } else { 
      f_star[j] <- min(macierz[,idx]); f_minus[j] <- max(macierz[,idx+2]) 
    }
  }
  
  S_r <- matrix(0, n_alt, 3); R_r <- matrix(0, n_alt, 3)
  for(i in 1:n_alt) {
    s_i <- c(0,0,0); r_i <- c(0,0,0)
    for(j in 1:k) {
      idx <- (j-1)*3 + 1
      diff_f <- f_star[j] - f_minus[j]; if(diff_f == 0) diff_f <- 1e-9
      
      if(typy[j] == "max") {
        dist <- (f_star[j] - macierz[i, c(idx+2, idx+1, idx)]) / diff_f
      } else {
        dist <- (macierz[i, c(idx, idx+1, idx+2)] - f_star[j]) / diff_f
      }
      
      dist_w <- dist * wagi[idx]
      s_i <- s_i + dist_w
      r_i <- pmax(r_i, dist_w)
    }
    S_r[i, ] <- s_i; R_r[i, ] <- r_i
  }
  
  s_star <- min(S_r[,1]); s_minus <- max(S_r[,3])
  r_star <- min(R_r[,1]); r_minus <- max(R_r[,3])
  
  Q_r <- matrix(0, n_alt, 3)
  for(i in 1:n_alt) {
    t1 <- (S_r[i,] - s_star) / (s_minus - s_star + 1e-9)
    t2 <- (R_r[i,] - r_star) / (r_minus - r_star + 1e-9)
    Q_r[i,] <- v * t1 + (1-v) * t2
  }
  
  S <- (S_r[,1] + 4*S_r[,2] + S_r[,3]) / 6
  R <- (R_r[,1] + 4*R_r[,2] + R_r[,3]) / 6
  Q <- (Q_r[,1] + 4*Q_r[,2] + Q_r[,3]) / 6
  
  data.frame(Alternatywa = 1:n_alt, Def_S = S, Def_R = R, Def_Q = Q, Wynik = Q, Ranking = rank(Q, ties.method = "first"))
}

#' @title Rozmyty WASPAS (Prawidlowa Arytmetyka)
#' @export
metoda_waspas <- function(macierz, typy, wagi, lambda = 0.5) {
  n_alt <- nrow(macierz); k <- ncol(macierz) / 3
  norm <- matrix(0, n_alt, 3*k)
  
  for(j in 1:k) {
    idx <- (j-1)*3 + 1
    if(typy[j] == "max") { 
      max_u <- max(macierz[, idx+2])
      norm[, idx:(idx+2)] <- macierz[, idx:(idx+2)] / max_u 
    } else { 
      min_l <- min(macierz[, idx])
      norm[, idx:(idx+2)] <- min_l / macierz[, c(idx+2, idx+1, idx)] 
    }
  }
  
  wsm_r <- matrix(0, n_alt, 3); wpm_r <- matrix(1, n_alt, 3)
  for(i in 1:n_alt) {
    for(j in 1:k) {
      idx <- (j-1)*3 + 1; w <- wagi[idx]
      wsm_r[i,] <- wsm_r[i,] + norm[i, idx:(idx+2)] * w
      wpm_r[i,] <- wpm_r[i,] * (norm[i, idx:(idx+2)] ^ w)
    }
  }
  
  wsm <- (wsm_r[,1] + 4*wsm_r[,2] + wsm_r[,3]) / 6
  wpm <- (wpm_r[,1] + 4*wpm_r[,2] + wpm_r[,3]) / 6
  Q <- lambda * wsm + (1 - lambda) * wpm
  
  data.frame(Alternatywa = 1:n_alt, WSM = wsm, WPM = wpm, Wynik = Q, Ranking = rank(-Q, ties.method = "first"))
}

#' @keywords internal
.kalkulator_preferencji_neat <- function(d1, d2, d3, d4, typ, q, p, s) {
  # Standardowe przypisywanie wartości funkcji preferencji
  aplikuj_funkcje <- function(mat) {
    if (typ == "usual") { ifelse(mat > 0, 1, 0)
    } else if (typ == "u-shape") { ifelse(mat > q, 1, 0)
    } else if (typ == "v-shape") { ifelse(mat > p, 1, ifelse(mat <= 0, 0, mat / p))
    } else if (typ == "level") { ifelse(mat > p, 1, ifelse(mat > q, 0.5, 0))
    } else if (typ == "linear") { ifelse(mat > p, 1, ifelse(mat <= q, 0, (mat - q) / (p - q)))
    } else if (typ == "gaussian") { ifelse(mat <= 0, 0, 1 - exp(-(mat^2) / (2 * s^2)))
    } else { stop("Nieznany typ funkcji preferencji.") }
  }
  
  P1 <- aplikuj_funkcje(d1); P2 <- aplikuj_funkcje(d2)
  P3 <- aplikuj_funkcje(d3); P4 <- aplikuj_funkcje(d4)

  # Mechanizm NEAT - korekta przybliżeń trapezoidalnych
  if (typ == "usual" || typ == "v-shape" || typ == "gaussian") {
    mask_d2 <- (d1 < 0 & 0 < d2) & ((-d1 / (d2 - d1)) > 0.5); P2[mask_d2] <- 0
    if (typ != "gaussian") { 
        prog <- ifelse(typ=="usual", 0, p)
        mask_d3 <- (d3 < prog & prog < d4) & ((prog - d4) / (d3 - d4) > 0.5); P3[mask_d3] <- 1 
    }
  } else if (typ %in% c("u-shape", "level", "linear")) {
    prog <- ifelse(typ=="u-shape", q, p)
    mask_d2 <- (d1 < q & q < d2) & ((q - d1) / (d2 - d1) > 0.5); P2[mask_d2] <- 0
    mask_d3 <- (d3 < prog & prog < d4) & ((prog - d4) / (d3 - d4) > 0.5); P3[mask_d3] <- 1
  }
  return(list(P1=P1, P2=P2, P3=P3, P4=P4))
}

#' @title Rozmyty PROMETHEE II (Prawidlowa Arytmetyka NEAT)
#' @param macierz Rozmyta macierz decyzyjna.
#' @param parametry_preferencji Ramka danych z kolumnami: Type, q, p, s, Role.
#' @param wagi Wektor wag z funkcji oblicz_wagi.
#' @export
metoda_promethee <- function(macierz, parametry_preferencji, wagi) {
  n_kryt <- ncol(macierz) / 3
  n_alt <- nrow(macierz)
  
  # Defuzyfikacja wag dla metod klasy PROMETHEE (środek ciężkości)
  wagi_ostre <- numeric(n_kryt)
  for(j in 1:n_kryt) {
    idx <- (j-1)*3 + 1
    wagi_ostre[j] <- (wagi[idx] + wagi[idx+1] + wagi[idx+2]) / 3
  }
  wagi_ostre <- wagi_ostre / sum(wagi_ostre)

  Pi_1 <- matrix(0, n_alt, n_alt); Pi_2 <- matrix(0, n_alt, n_alt)
  Pi_3 <- matrix(0, n_alt, n_alt); Pi_4 <- matrix(0, n_alt, n_alt)

  # Obliczanie preferencji dla każdego kryterium
  for (j in 1:n_kryt) {
    typ_p <- as.character(parametry_preferencji[j, "Type"])
    q_val <- as.numeric(parametry_preferencji[j, "q"])
    p_val <- as.numeric(parametry_preferencji[j, "p"])
    s_val <- as.numeric(parametry_preferencji[j, "s"])
    rola  <- as.character(parametry_preferencji[j, "Role"])

    idx <- (j-1)*3 + 1
    col_l <- macierz[, idx]; col_m <- macierz[, idx+1]; col_u <- macierz[, idx+2]

    # Rozmyta różnica (A - B) na trójkątach rozciągniętych do trapezu (L, M, M, U)
    if (rola == "max") {
      d1 <- outer(col_l, col_u, "-"); d2 <- outer(col_m, col_m, "-")
      d3 <- outer(col_m, col_m, "-"); d4 <- outer(col_u, col_l, "-")
    } else {
      # Dla kosztów odwracamy kierunek: Inny - Alternatywa
      d1 <- outer(col_u, col_l, "-") * -1; d2 <- outer(col_m, col_m, "-") * -1
      d3 <- outer(col_m, col_m, "-") * -1; d4 <- outer(col_l, col_u, "-") * -1
    }

    P_list <- .kalkulator_preferencji_neat(d1, d2, d3, d4, typ_p, q_val, p_val, s_val)
    w <- wagi_ostre[j]
    
    # Agregacja wagowa
    Pi_1 <- Pi_1 + (P_list$P1 * w); Pi_2 <- Pi_2 + (P_list$P2 * w)
    Pi_3 <- Pi_3 + (P_list$P3 * w); Pi_4 <- Pi_4 + (P_list$P4 * w)
  }

  diag(Pi_1) <- 0; diag(Pi_2) <- 0; diag(Pi_3) <- 0; diag(Pi_4) <- 0

  # Przepływy (Flows)
  Phi_plus_1 <- rowSums(Pi_1)/(n_alt-1); Phi_plus_2 <- rowSums(Pi_2)/(n_alt-1)
  Phi_plus_3 <- rowSums(Pi_3)/(n_alt-1); Phi_plus_4 <- rowSums(Pi_4)/(n_alt-1)
  
  Phi_minus_1 <- colSums(Pi_1)/(n_alt-1); Phi_minus_2 <- colSums(Pi_2)/(n_alt-1)
  Phi_minus_3 <- colSums(Pi_3)/(n_alt-1); Phi_minus_4 <- colSums(Pi_4)/(n_alt-1)

  # Defuzyfikacja końcowa przepływów
  def_Phi_plus  <- (Phi_plus_1 + Phi_plus_2 + Phi_plus_3 + Phi_plus_4) / 4
  def_Phi_minus <- (Phi_minus_1 + Phi_minus_2 + Phi_minus_3 + Phi_minus_4) / 4
  Phi_net <- def_Phi_plus - def_Phi_minus

  data.frame(Alternatywa = 1:n_alt, Phi_Plus = def_Phi_plus, Phi_Minus = def_Phi_minus, Phi_Net = Phi_net, Wynik = Phi_net, Ranking = rank(-Phi_net, ties.method = "first"))
}