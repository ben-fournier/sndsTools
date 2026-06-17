library(sndsTools)
library(dplyr)

# - initialize_connection
# - extract_consultations_mcofcstc

# Recherche toutes les consultations hospitalières pour les spécialités de médecine générale (01, 22, 23).

start_date <- as.Date("2019-01-01")
end_date <- as.Date("2022-12-31")
spe_codes <- c("01", "22", "23")

consultations <- extract_consultations_mcofcstc(
  start_date = start_date,
  end_date = end_date,
  spe_codes = spe_codes,
)
consultations |> head()
