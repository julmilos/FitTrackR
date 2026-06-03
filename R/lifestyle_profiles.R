#' @title Profile przykładowych użytkowników urządzeń wearable
#' @description Predefiniowane profile użytkowników wspierające dobór wag kryteriów
#' w analizie MCDA dla urządzeń wearable (smartwatchy oraz opasek fitness).
#' @keywords internal

.get_lifestyle_profile <- function(profile) {

  profiles <- list(

    beginner = list(
      weights = c(
        Dokladnosc     = 0.05,
        Bateria        = 0.20,
        Funkcjonalnosc = 0.25,
        Kompatybilnosc = 0.05,
        Komfort        = 0.05,
        Odpornosc      = 0.10,
        Cena           = 0.30
      )
    ),

    athlete = list(
      weights = c(
        Dokladnosc     = 0.45,
        Bateria        = 0.05,
        Funkcjonalnosc = 0.15,
        Kompatybilnosc = 0.05,
        Komfort        = 0.05,
        Odpornosc      = 0.15,
        Cena           = 0.10
      )
    ),

    casual = list(
      weights = c(
        Dokladnosc     = 0.15,
        Bateria        = 0.15,
        Funkcjonalnosc = 0.20,
        Kompatybilnosc = 0.20,
        Komfort        = 0.15,
        Odpornosc      = 0.05,
        Cena           = 0.10
      )
    )
  )

  if (!profile %in% names(profiles)) {
    stop(paste("Profil nieznany. Dostepne:", paste(names(profiles), collapse = ", ")))
  }

  return(profiles[[profile]])
}
