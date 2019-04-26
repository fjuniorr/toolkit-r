


#' @export
xmr <- function(x, sampling = NULL, trend = FALSE, lock = FALSE) {

  if(is.null(sampling)) {
    sampling <- list(seq_along(x))
  } else {
    stopifnot(identical(seq_along(x), unlist(sampling))) # checks if sampling period is correctly specified
    }

  if(length(trend) > 1) {
    stopifnot(identical(length(sampling), length(trend))) # checks that each sample has a trend value
  }

  if(length(trend) == 1 & !is.null(sampling)) { # if sampling is NULL I don?t need to replicate the trend vector because xmr_data will ba called only once
    trend <- rep(trend, times = length(sampling)) # this make it easier to call xmr_data with approppriate trend argument
  }


  samples <- lapply(seq_along(sampling), function(i, x, sampling, trend) {
    ls <- xmr_data(x[sampling[[i]]], trend[i])
    ls$data$sampling <- paste0("sample", i)
    ls
  }, x = x, sampling = sampling, trend = trend)


  dt <- data.table::as.data.table(do.call("rbind", purrr::map(samples, "data")))

  dt[, trend := seq_len(nrow(dt))]
  dt[, locked := FALSE]

  if(lock == TRUE) {
    stopifnot(length(sampling) > 1) # in order to lock the limits we need at least two samples
    locked_obs <- sampling[[length(sampling)]]


    pre_locked_obs_index <- length(sampling) - 1
    pre_locked_obs <- sampling[[pre_locked_obs_index]]

    dt[locked_obs, mrBar := dt[pre_locked_obs, unique(mrBar)]]
    dt[locked_obs, URL := dt[pre_locked_obs, unique(URL)]]

    if(trend[pre_locked_obs_index]) {
      trend_locked_obs <- (locked_obs - head(pre_locked_obs, 1)) + 1
      df <- data.frame(trend = trend_locked_obs)
    } else {
      df <- data.frame(trend = rep(TRUE, length(locked_obs)))
    }


    # suppress warning prediction from a rank-deficient fit may be misleading
    dt[locked_obs, xBar := suppressWarnings(predict(samples[[pre_locked_obs_index]]$fit, newdata = df))]

    dt[head(locked_obs, 1), mr := abs(dt[head(locked_obs, 1), x] - dt[last(pre_locked_obs, 1), x])]
    dt[locked_obs, locked := TRUE]
  }

  dt[, UNPL := xBar + (2.66 * mrBar)]
  dt[, LNPL := xBar - (2.66 * mrBar)]
  dt[, mrCritical := (mr > URL)]
  dt[, xCritical := (x > UNPL | x < LNPL)]


  dt[, .(trend, x, mr, mrBar, URL, xBar, UNPL, LNPL, mrCritical, xCritical, sampling, locked)]
}

#' XmR Charts
#'
#' Graficos de Controle
#' @import ggplot2
#' @import data.table
#' @export
xmr_plot <- function(data) {

  p1 <- ggplot(data = data, aes(x = trend, y = x)) +
    geom_point(aes(color = xCritical), size = 3) +
    geom_line() +
    geom_line(aes(x = trend, y = xBar)) +
    geom_line(aes(x = trend, y = UNPL), color = "red") +
    geom_line(aes(x = trend, y = LNPL), color = "red") +
    theme_bw() +
    scale_color_manual(values = c("black", "red"), guide = FALSE) +
    ylab("Individual Values (X)")


  p2 <- ggplot(data = data, aes(trend, mr)) +
    geom_point(aes(color = mrCritical), size = 3, na.rm = TRUE) +
    geom_line(na.rm = TRUE) +
    geom_line(aes(x = trend, y = mrBar)) +
    geom_line(aes(x = trend, y = URL), color = "red") +
    ylab("Moving Ranges (mR)") +
    theme_bw() +
    scale_color_manual(values = c("black", "red"), guide = FALSE)

  gridExtra::grid.arrange(p1, p2, nrow = 2)

}

#' @export
xmr_data <- function(x, trend) {

  stopifnot(is.atomic(x), is.numeric(x), is.logical(trend))

  dt <- data.table::data.table(x)

  if(trend) {
    dt[, trend := seq_along(x)]
  } else {
    dt[, trend := rep(TRUE, length(x))] # we can use a dummy regressor to estimate xBar
  }

  # individual (X) data
  fit <- lm(x ~ trend,data = dt)
  dt[, xBar := suppressWarnings(predict(fit, newdata = data.frame(trend)))]


  # moving range (mR) data
  dt[, mr := abs((x - data.table::shift(x, type = "lag")))]
  dt[, mrBar := mean(mr, na.rm = TRUE)]
  dt[, URL := 3.267 * mrBar]


  list(data = dt[, .(x, xBar, mr, mrBar, URL)], fit = fit)
}


#' Multiple plot function
#'
#' ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
#' - cols:   Number of columns in layout
#' - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#'
#' If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
#' then plot 1 will go in the upper left, 2 will go in the upper right, and
#' 3 will go all the way across the bottom.
#' @export
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#=================================================================
# interactive script

# library(ggplot2)
#
# # monthly rcl from jan/2014 to dec/2017 (48 obs)
# rcl <- c(4952785236, 3870532712, 3704057113, 3601189526, 3463474590,
#          4292643883, 3562734178, 3696401761, 3678873314, 3887113544, 3979902304,
#          4954527274, 4623461356, 3861725808, 3781705612, 3732497428, 3705515860,
#          3675942229, 3692998854, 3752313586, 4594087664, 2799347866, 3835202033,
#          9588437471, 4840712017, 4128363042, 4124698808, 4149893579, 4200996211,
#          4440818662, 3948792705, 3922664212, 4162646209, 3979901907, 4717547570,
#          7114434209, 5200100937, 4531123085, 4486035904, 4212081728, 4628473360,
#          4476152019, 4167795426, 4561687157, 4264453298, 4639999235, 4565035765,
#          5370721469)
#
# xmr_plot(rcl)
# xmr_plot(rcl, trend = TRUE)
