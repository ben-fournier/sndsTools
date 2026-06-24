require(dplyr)
require(tibble)

fake_dcir_join_keys <- tibble::tribble(
  ~DCT_ORD_NUM, ~FLX_DIS_DTD, ~FLX_TRT_DTD,
  ~FLX_EMT_ORD, ~FLX_EMT_NUM, ~FLX_EMT_TYP,
  ~ORG_CLE_NUM, ~PRS_ORD_NUM, ~REM_TYP_AFF,
  1, "2019-02-10", "2019-02-10",   1, 1, 1,   1, 1, 1,
  2, "2019-02-02", "2019-02-02",   1, 1, 1,   1, 1, 1,
  3, "2019-02-03", "2019-02-03",   1, 1, 1,   1, 1, 1,
  4, "2019-02-04", "2019-02-04",   1, 1, 1,   1, 1, 1,
  5, "2019-03-01", "2019-03-01",   1, 1, 1,   1, 1, 1,
  6, "2019-03-02", "2019-03-02",   2, 2, 2,   2, 2, 2
) |>
  dplyr::mutate(
    FLX_DIS_DTD = as.Date(FLX_DIS_DTD),
    FLX_TRT_DTD = as.Date(FLX_TRT_DTD)
  )

fake_er_prs_f <- tibble::tribble(
  ~BEN_NIR_PSA, ~EXE_SOI_DTD, ~EXE_SOI_DTF, ~PSP_SPE_COD,
  ~DPN_QLF,     ~CPL_MAJ_TOP, ~BEN_CDI_NIR, ~PRS_NAT_REF,
  11, "2019-01-10", "2019-01-10", "01",   0, 0, "00", "3336",
  12, "2019-01-02", "2019-01-02", "02",   0, 0, "03", "3336",
  13, "2019-01-03", "2019-01-03", "03",   0, 0, "04", "3317",
  15, "2019-01-04", "2019-01-04", "04",   0, 1, "00", "3336",
  13, "2019-02-01", "2019-02-01", "05",   0, 2, "03", "3351",
  16, "2019-02-02", "2019-02-02", "06",   0, 0, "00", "3317"
) |>
  dplyr::mutate(
    EXE_SOI_DTD = as.Date(EXE_SOI_DTD),
    EXE_SOI_DTF = as.Date(EXE_SOI_DTF),
  ) |>
  dplyr::bind_cols(fake_dcir_join_keys)

fake_er_ucd_f <- tibble::tribble(
  ~UCD_TOP_UCD, ~UCD_UCD_COD, ~UCD_DLV_NBR,
  0, "9231824",              1,
  1, "9231825",              1,
  9, "9231824",              1,
  2, "9231824",              1,
  3, "9231827",              1,
  4, "9231827",              1
) |>
  dplyr::bind_cols(fake_dcir_join_keys)

fake_er_ete_f <- tibble::tribble(
  ~ETE_NUM, ~ETE_IND_TAA,
        11,           10,
        12,           10,
        13,           10,
        14,            1,
        15,            1,
        15,           10
) |>
  dplyr::bind_cols(fake_dcir_join_keys)

create_mock_erphaf <- function(
    conn,
    fake_er_prs_f,
    fake_er_ucd_f,
    fake_er_ete_f
) {
  DBI::dbWriteTable(conn, "ER_PRS_F", fake_er_prs_f, overwrite = TRUE)
  DBI::dbWriteTable(conn, "ER_UCD_F", fake_er_ucd_f, overwrite = TRUE)
  DBI::dbWriteTable(conn, "ER_ETE_F", fake_er_ete_f, overwrite = TRUE)
}

test_that("extract_drugs_erucdf respects UCD filter", {
  conn <- connect_synthetic_snds()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  create_mock_erphaf(
    conn = conn,
    fake_er_prs_f = fake_er_prs_f,
    fake_er_ucd_f = fake_er_ucd_f,
    fake_er_ete_f = fake_er_ete_f
  )

  # Test with specific UCD filter (only J05 codes)
  start_date <- as.Date("2019-01-01")
  end_date <- as.Date("2019-04-01")

  # mode lazy table
  result <- extract_drugs_erucdf(
    start_date = start_date,
    end_date = end_date,
    ucd_codes_filter = c("9231824"),
    output_table_name = "RESULT_WITH_FILTER",
    dis_dtd_lag_months = 1,
    conn = conn
  )

  result_table <- dplyr::tbl(conn, "RESULT_WITH_FILTER")
  # result_data <- result_table |> dplyr::collect()

  # test structure of the result
  expected_colnames <- c(
    "BEN_NIR_PSA",
    "EXE_SOI_DTD",
    "EXE_SOI_DTF",
    "PRS_NAT_REF",
    "UCD_TOP_UCD",
    "UCD_UCD_COD",
    "UCD_DLV_NBR"
  )

  expect_setequal(
    colnames(result_table),
    expected_colnames
  )

  # test that only two rows are present
  n_rows <- result_table |> dplyr::count() |> dplyr::pull()
  expect_equal(n_rows, 2)


  # mode data collected
  filter_ucd_codes <- c("9231824", "9231825")
  result_data <- extract_drugs_erucdf(
    start_date = start_date,
    end_date = end_date,
    ucd_codes_filter = filter_ucd_codes,
    # output_table_name = NULL
    dis_dtd_lag_months = 1,
    conn = conn
  )

  result_ucd_codes <- result_data |> dplyr::distinct(UCD_UCD_COD) |> dplyr::pull()

  expect_setequal(result_ucd_codes, filter_ucd_codes)
})

