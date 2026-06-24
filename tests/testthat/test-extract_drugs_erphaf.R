require(dplyr)
require(tibble)

fake_patients_ids <- tibble::tribble(
  ~BEN_IDT_ANO, ~BEN_NIR_PSA,
  1,           11,
  2,           12,
  3,           13
)

fake_dcir_join_keys <- tibble::tribble(
  ~DCT_ORD_NUM, ~FLX_DIS_DTD, ~FLX_TRT_DTD,
             1, "2019-02-10", "2019-01-10",
             2, "2019-02-02", "2019-01-02",
             3, "2019-02-03", "2019-01-03",
             4, "2020-02-05", "2020-01-05",
             5, "2019-02-04", "2019-01-04"
) |>
  dplyr::mutate(
    FLX_DIS_DTD = as.Date(FLX_DIS_DTD),
    FLX_TRT_DTD = as.Date(FLX_TRT_DTD),
    FLX_EMT_ORD = 1,
    FLX_EMT_NUM = 1,
    FLX_EMT_TYP = 1,
    ORG_CLE_NUM = 1,
    PRS_ORD_NUM = 1,
    REM_TYP_AFF = 1
  )

fake_er_prs_f <- tibble::tribble(
  ~BEN_NIR_PSA, ~BEN_RNG_GEM,
  ~EXE_SOI_DTD,  ~PSP_SPE_COD, ~DPN_QLF, ~CPL_MAJ_TOP,
  11, 1,   "2019-01-10", "01",        0,            0,
  12, 1,   "2019-01-02", "22",        0,            0,
  13, 1,   "2019-01-03", "32",        0,            0,
  15, 1,   "2020-01-05", "34",        0,            1,
  13, 1,   "2019-01-04", "01",       71,            2
) |>
  dplyr::mutate(EXE_SOI_DTD = as.Date(EXE_SOI_DTD)) |>
  dplyr::bind_cols(fake_dcir_join_keys)

fake_er_pha_f <- tibble::tribble(
  ~PHA_PRS_C13,       ~PHA_ACT_QSN,
  "3400932026555",    1,
  "3400932725847",    1,
  "3400930219874",    1,
  "3400930219874",    1,
  "3400936267343",    1
) |>
  dplyr::bind_cols(fake_dcir_join_keys)

fake_ir_pha_r <- tibble::tribble(
  ~PHA_CIP_C13,       ~PHA_ATC_CLA,
  "3400932026555",    "N04BC01",
  "3400932725847",    "N05AC01",
  "3400930219874",    "J05AG05",
  "3400930219874",    "J05AG05",
  "3400936267343",    "J01MA06"
)

fake_er_ete_f <- tibble::tribble(
  ~ETE_NUM, ~ETE_IND_TAA,
  11,          10,
  12,          10,
  13,          10
) |>
  dplyr::bind_cols(fake_dcir_join_keys |> head(3))

create_mock_er_pha_f <- function(
    conn,
    fake_er_pha_f,
    fake_ir_pha_r,
    fake_er_prs_f,
    fake_er_ete_f
) {
  DBI::dbWriteTable(conn, "ER_PHA_F", fake_er_pha_f, overwrite = TRUE)
  DBI::dbWriteTable(conn, "IR_PHA_R", fake_ir_pha_r, overwrite = TRUE)
  DBI::dbWriteTable(conn, "ER_PRS_F", fake_er_prs_f, overwrite = TRUE)
  DBI::dbWriteTable(conn, "ER_ETE_F", fake_er_ete_f, overwrite = TRUE)
}


test_that("extract_drugs_er_pha_f works for ATC", {
  conn <- connect_synthetic_snds()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  create_mock_er_pha_f(
    conn = conn,
    fake_er_pha_f = fake_er_pha_f,
    fake_ir_pha_r = fake_ir_pha_r,
    fake_er_prs_f = fake_er_prs_f,
    fake_er_ete_f = fake_er_ete_f
  )

  start_date <- as.Date("2019-01-01")
  end_date <- as.Date("2019-12-31")

  drug_dispenses <- extract_drugs_er_pha_f(
    start_date = start_date,
    end_date = end_date,
    atc_cod_starts_with_filter = c("J05"),
    patients_ids_filter = fake_patients_ids,
    conn = conn
  )

  expected <- result <- tibble::tribble(
    ~BEN_IDT_ANO, ~EXE_SOI_DTD,
    ~PHA_ACT_QSN, ~PHA_ATC_CLA, ~PHA_PRS_C13, ~PSP_SPE_COD,
    3, "2019-01-03",   1, "J05AG05", "3400930219874", "32"
  ) |>
    dplyr::mutate(EXE_SOI_DTD = as.Date(EXE_SOI_DTD))

  expect_equal(
    drug_dispenses |> dplyr::arrange(BEN_IDT_ANO, EXE_SOI_DTD),
    expected
  )
})

test_that("extract_drugs_er_pha_f works for CIP13", {
  conn <- connect_synthetic_snds()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  create_mock_er_pha_f(
    conn = conn,
    fake_er_pha_f = fake_er_pha_f,
    fake_ir_pha_r = fake_ir_pha_r,
    fake_er_prs_f = fake_er_prs_f,
    fake_er_ete_f = fake_er_ete_f
  )

  start_date <- as.Date("2019-01-01")
  end_date <- as.Date("2019-12-31")

  drug_dispenses <- extract_drugs_erphaf(
    start_date = start_date,
    end_date = end_date,
    atc_cod_starts_with_filter = c("J05"),
    cip13_cod_filter = c("3400932725847"),
    patients_ids_filter = fake_patients_ids,
    conn = conn
  )

  expected <- tibble::tribble(
    ~BEN_IDT_ANO, ~EXE_SOI_DTD,
    ~PHA_ACT_QSN, ~PHA_ATC_CLA, ~PHA_PRS_C13, ~PSP_SPE_COD,
    2, "2019-01-02",   1, "N05AC01", "3400932725847", "22",
    3, "2019-01-03",   1, "J05AG05", "3400930219874", "32"
  ) |>
    dplyr::mutate(EXE_SOI_DTD = as.Date(EXE_SOI_DTD))

  expect_equal(
    drug_dispenses |> dplyr::arrange(BEN_IDT_ANO, EXE_SOI_DTD),
    expected
  )
})
