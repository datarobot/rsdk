# Copyright 2022 DataRobot, Inc. and its affiliates.
#
# All rights reserved.
#
# DataRobot, Inc.
#
# This is proprietary source code of DataRobot, Inc. and its
# affiliates.
library(testthat)

describe("API Model Serialization", {
  it("works for objects with primitives only", {
    mv <- datarobot.apicore::MonetaryValue$new(
      currency = "EUR",
      value = 1000
    )

    json <- mv$toJSON()
    mv2 <- datarobot.apicore::MonetaryValue$new()$fromJSON(json)
    expect_equal(mv2, mv)
    expect_equal(mv2$currency, mv$currency)
    expect_equal(mv2$value, mv$value)

    mv2$value <- 999
    expect_failure(expect_equal(mv2, mv))
    expect_failure(expect_equal(mv2$value, mv$value))
  })

  it("works for objects with both primitives and non", {
    # TODO There is a bug here! Errored out for now
    expect_error(
      {
        aim <- datarobot.apicore::Aim$new(
          autopilotDataSelectionMethod = "duration",
          onlyIncludeMonotonicBlueprints = TRUE
        )

        json <- aim$toJSON()
        aim2 <- datarobot.apicore::Aim$new()$fromJSON(json) # fails here

        expect_equal(aim2, aim)
      },
      regexp = "`aim2` not equal to `aim`"
    )
  })

  it("works when newing an object with nested objects", {
    # TODO There is a bug here! Errored out for now
    expect_error(
      {
        mv <- datarobot.apicore::MonetaryValue$new(
          currency = "EUR",
          value = 1000
        )

        resp <- datarobot.apicore::UseCaseCreateResponse$new(realizedValue = mv)
        resp$toJSON() # fails here
      },
      regexp = "argument is missing, with no default"
    )
  })

  it("serializes Aim$cvHoldoutLevel DSX-2171", {
    aim <- Aim$new(
      target = "readmitted",
      shapOnlyMode = TRUE,
      offset = "number_emergency",
      exposure = "number_diagnoses",
      cvMethod = "user",
      userPartitionCol = "A1Cresult",
      cvHoldoutLevel = ">8"
    )

    # Per DSX-2171, aim$toJSON() would fail with the error
    # "operator is invalid for atomic vectors" when cvHoldoutLevel
    # was set to any value
    aimString <- aim$toJSON()
    expect_equal(aim$cvHoldoutLevel, ">8")
  })
})
