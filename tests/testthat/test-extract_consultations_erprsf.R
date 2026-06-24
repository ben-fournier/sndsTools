require(dplyr)

fake_patients_ids <- tibble::tribble(
  ~BEN_IDT_ANO, ~BEN_NIR_PSA, ~BEN_RNG_GEM,
             1,            11,           1,
             2,            12,           1,
             3,            13,           1
)

fake_erprsf <- tibble::tribble(
  ~BEN_NIR_PSA, ~BEN_RNG_GEM,
  ~EXE_SOI_DTD, ~FLX_DIS_DTD,
  ~PSE_SPE_COD, ~PFS_EXE_NUM, ~PRS_NAT_REF,
  11, 1,   "2019-01-10", "2019-02-10",   "01", 1, "C",
  12, 1,   "2019-01-02", "2019-02-02",   "22", 2, "CS",
  13, 1,   "2019-01-03", "2019-02-03",   "99", 3, "C",
  13, 1,   "2020-01-05", "2020-02-05",   "01", 4, "C",
  15, 1,   "2019-01-04", "2019-02-04",   "01", 5, "C"
) |>
  dplyr::mutate(
    EXE_SOI_DTD = as.Date(EXE_SOI_DTD),
    FLX_DIS_DTD = as.Date(FLX_DIS_DTD),
    PRS_ACT_QTE = 1,
    DPN_QLF     = "0",
    PRS_DPN_QLP = "0",
    CPL_MAJ_TOP = 0,
    CPL_AFF_COD = 0,
    PSE_STJ_COD = 51
  )

create_mock_erprsf <- function(conn, fake_erprsf) {
  DBI::dbWriteTable(conn, "ER_PRS_F", fake_erprsf, overwrite = TRUE)
}

test_that("extract_consultations_erprsf_works ", {
  conn <- connect_synthetic_snds()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)
  create_mock_erprsf(
    conn = conn,
    fake_erprsf = fake_erprsf
  )

  start_date <- as.Date("2019-01-01")
  end_date <- as.Date("2019-12-31")
  pse_spe_filter <- c("01", "22", "32", "34")
  prestation_filter <- c("C", "CS")

  consultations <- extract_consultations_erprsf(
    start_date = start_date,
    end_date = end_date,
    pse_spe_filter = pse_spe_filter,
    prestation_filter = prestation_filter,
    patients_ids_filter = fake_patients_ids,
    conn = conn
  )

  expected <- tibble::tribble(
    ~BEN_IDT_ANO, ~EXE_SOI_DTD, ~PSE_SPE_COD, ~PFS_EXE_NUM,
    ~PRS_NAT_REF, ~PRS_ACT_QTE, ~BEN_RNG_GEM,
    1, "2019-01-10", "01", 1,   "C" , 1, 1,
    2, "2019-01-02", "22", 2,   "CS", 1, 1
  ) |>
    dplyr::mutate(EXE_SOI_DTD = as.Date(EXE_SOI_DTD))

  expect_equal(
    consultations |> arrange(BEN_IDT_ANO, EXE_SOI_DTD) |> collect(),
    expected
  )
})
