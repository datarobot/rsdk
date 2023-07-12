# Copyright 2023 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

describe("UploadData", {
  it("produces a form_file without a name in curl<5.0.1", {
    skip_if_too_new_installed("curl", "5.0.0")
    upload <- UploadData(iris)
    expect_true(is(upload, "form_file"))
    expect_true(is.null(upload$name))
    expect_false("name" %in% names(upload))
  })

  it("produces a form_file with a non-null name from a data frame", {
    skip_if_not_installed("curl", "5.0.1")
    upload <- UploadData(iris)
    expect_true(is(upload, "form_file"))
    expect_false(is.null(upload$name))
    expect_equal(upload$name, basename(upload$path))
  })

  it("produces a form_file with a non-null name from a data file", {
    skip_if_not_installed("curl", "5.0.1")
    iriscsv <- withr::with_tempfile("iriscsv", {
      write.csv(iris, iriscsv)
      upload <- UploadData(iriscsv)
      expect_true(is(upload, "form_file"))
      expect_false(is.null(upload$name))
      expect_equal(upload$name, basename(iriscsv))
      expect_equal(upload$name, basename(upload$path))
    })
  })

  it("produces a form_file with a specified name", {
    skip_if_not_installed("curl", "5.0.1")
    fileName <- "squak_parrot.csv"
    upload <- UploadData(iris, fileName = fileName)
    expect_true(is(upload, "form_file"))

    expect_false(is.null(upload$name))
    expect_equal(upload$name, fileName)
    expect_false(upload$name == basename(upload$path))
  })

  it("expects fileName arg to be a character vector", {
    expect_error(UploadData(iris, fileName = 1))
    expect_error(UploadData(iris, fileName = 5.0))
  })
})
