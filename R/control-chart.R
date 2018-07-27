#' XmR Charts
#'
#' Graficos de Controle
#' @import ggplot2
#' @import data.table
#' @export
xmr_plot <- function(x, trend = FALSE) {

  dt <- xmr_data(x, trend)

  p1 <- ggplot(data = dt, aes(x = trend, y = x)) + geom_point(aes(color = xCritical), size = 3) + geom_line() +
    geom_abline(intercept = intercept(dt$xBar), slope = slope(dt$xBar)) +
    geom_abline(intercept = intercept(dt$UNPL), slope = slope(dt$UNPL), color = "red") +
    geom_abline(intercept = intercept(dt$LNPL), slope = slope(dt$LNPL), color = "red") +
    theme_bw() +
    scale_color_manual(values = c("black", "red"), guide = FALSE) +
    ylab("Individual Values (X)")


  p2 <- ggplot(data = dt, aes(trend, mr)) + geom_point(na.rm = TRUE) + geom_line(na.rm = TRUE) +
    geom_hline(yintercept = dt$mrBar) +
    geom_hline(yintercept = dt$URL, color = "red") +
    ylab("Moving Ranges (mR)") +
    theme_bw()

  multiplot(p1, p2, cols = 1)

  invisible(dt)

}

xmr_data <- function(x, trend = FALSE, labels = NULL) {

  if(is.data.frame(x)) {
    stopifnot(length(x) == 1)
    x <- as.numeric(x[, 1])
  }

  stopifnot(is.atomic(x), is.numeric(x), is.logical(trend))

  names(x) <- "x"

  dt <- data.table::data.table(trend = 1:length(x))
  dt[, x := x]
  if(trend) {
    dt[, xBar := fitted(lm(x ~ trend))]
  } else {
    dt[, xBar := mean(x, na.rm = TRUE)]
  }
  dt[, mr := abs((x - data.table::shift(x, type = "lag")))]
  dt[, mrBar := mean(mr, na.rm = TRUE)]
  dt[, URL := 3.267 * mrBar]
  dt[, UNPL := xBar + (2.66 * mean(mr, na.rm = TRUE))]
  dt[, LNPL := xBar - (2.66 * mean(mr, na.rm = TRUE))]
  dt[, xCritical := (x > UNPL | x < LNPL)]
  dt[, mrCritical := (mr > URL)]

  dt
}


slope <- function(x) {
  stopifnot(is.atomic(x), is.numeric(x))
  trend <- 1:length(x)
  fit <- lm(x ~ trend)
  slope <- unname(coef(fit)["trend"])
  slope
}

intercept <- function(x) {
  stopifnot(is.atomic(x), is.numeric(x))
  trend <- 1:length(x)
  fit <- lm(x ~ trend)
  slope <- unname(coef(fit)["(Intercept)"])
  slope
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
