
# '@export
whatif <- function (curr.p, curr.share, if.p, if.share, exp,
                    result = c("return", "equity", "average",
                               "percent", "cost", "pre.post",
                               "all")) {

  total.share = curr.share+if.share
  curr.percent = curr.share/total.share
  if.percent = if.share/total.share

  cost =if.share*if.p

  average = curr.p*curr.percent + if.p*if.percent

  equity = average*total.share

  diff = exp - average
  return = total.share*diff

  percent = return/equity*100

  prediff = exp - curr.p
  prereturn = (exp - curr.p)*curr.share

  prepost = return - prereturn

  if (result == "average"){
    return(round(average, 2))
  } else if (result == "cost") {
    return(round(cost, 2))
  } else if (result == "return") {
    return(round(return, 2))
  } else if (result == "percent") {
    return (round(percent, 2))
  } else if (result == "equity") {
    return(round(equity, 2))
  } else if (result == "pre.post") {
    return(round(prepost, 2))
  }else if (result == "all") {

    all <- list("avg cost" = round(average, 2),
                "if cost" = round(cost, 2),
                "equity" = round(equity, 2),
                "return" = round(return, 2),
                "percent" = round (percent, 2),
                "pre/post" = round(prepost, 2))

    output <- matrix(unlist(all), ncol = 6, byrow = FALSE)

    # all <- matrix(data = c(round(average, 2),
    #                        round(cost, 2),
    #                        round(equity, 2),
    #                        round(return, 2),
    #                        round (percent, 2),
    #                        round(prepost, 2)),
    #                  ncol = 6,
    #                  nrow = 1)

    colnames(output) <- c("avg cost",
                       "if cost",
                       "equity",
                        "return",
                        "percent",
                       "pre/post")

    rownames(output) <-if.p

    return(output)
  }

}

