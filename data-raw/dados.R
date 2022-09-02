data_path <- system.file(
    "extdata",
    "telepsi_ana.xlsx",
    package = "TelePsi"
)

raw_data <- readxl::read_xlsx(data_path, sheet = "Completa")

dados <- raw_data |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(where(is.character),
                                \(x) x |>
                                    stringr::str_trim() |>
                                    stringr::str_squish()))

dados

usethis::use_data(dados, overwrite = TRUE)
