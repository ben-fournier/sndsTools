require(dplyr)
require(tibble)

fake_patients_ids <- tibble::tribble(
  ~BEN_IDT_ANO, ~BEN_NIR_PSA,
             1,           11,
             2,           12,
             3,           13
)

fake_ir_imb_r <- tibble::tribble(
  ~BEN_NIR_PSA, ~IMB_ALD_DTD, ~IMB_ALD_DTF,
  ~IMB_ALD_NUM, ~MED_MTF_COD, ~IMB_ETM_NAT,
  11, "2019-01-10", "2019-02-10",   2, "I50", "01",
  15, "2019-01-02", "2019-02-02",   1, "I65", "01",
  12, "2019-01-03", "2019-02-03",   1, "I65", "01",
  13, "2019-01-05", "2019-02-05",   1, "I60", "01",
  13, "2019-01-04", "2019-02-04",   1, "I60", "11"
) |>
  dplyr::mutate(
    IMB_ALD_DTD = as.Date(IMB_ALD_DTD),
    IMB_ALD_DTF = as.Date(IMB_ALD_DTF)
  )

create_mock_ir_imb_r <- function(
    conn,
    fake_ir_imb_r
) {
  DBI::dbWriteTable(conn, "IR_IMB_R", fake_ir_imb_r, overwrite = TRUE)
}

test_that("extract_longtermdiseases_irimbr works", {
  conn <- connect_synthetic_snds()
  on.exit(DBI::dbDisconnect(conn, shutdown = TRUE), add = TRUE)

  create_mock_ir_imb_r(
    conn = conn,
    fake_ir_imb_r = fake_ir_imb_r
  )

  start_date <- as.Date("2019-01-01")
  end_date <- as.Date("2019-12-31")

  ald <- extract_longtermdiseases_irimbr(
    start_date = start_date,
    end_date = end_date,
    icd_cod_starts_with = c("I6"),
    patients_ids = fake_patients_ids,
    conn = conn
  )

  expected <- tibble::tribble(
    ~BEN_IDT_ANO, ~IMB_ALD_NUM,
    ~IMB_ALD_DTD, ~IMB_ALD_DTF, ~IMB_ETM_NAT, ~MED_MTF_COD,
    2, 1,   "2019-01-03", "2019-02-03", "01", "I65",
    3, 1,   "2019-01-05", "2019-02-05", "01", "I60"
  ) |>
    dplyr::mutate(
      IMB_ALD_DTD = as.Date(IMB_ALD_DTD),
      IMB_ALD_DTF = as.Date(IMB_ALD_DTF)
    )

  expect_equal(
    ald |> dplyr::arrange(BEN_IDT_ANO, IMB_ALD_DTD),
    expected
  )
})
