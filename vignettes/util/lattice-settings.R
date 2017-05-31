fontsize.settings <- trellis.par.get("fontsize")
fontsize.settings$text <- 14 # 10
trellis.par.set("fontsize", fontsize.settings)

lineColors <- list(
  pal2 = c(
    "#ee4247", # red
    "#007bbd"  # blue
  ),
  pal3 = c(
    "#62bc60", # green
    "#007bbd", # blue
    "#ee4247"  # red
  ))

atLim <- function(lim, by=1) {
  at <- seq(from = lim[1], to = lim[2], by = by)
  return(at)
}

add.lattice.xsubticks <- function (lim, ..., n = 2) {
    ans    <- lattice::xscale.components.default(lim = lim, ..., n = n)
    ticks  <- ans$bottom$ticks$at
    ticks2 <- head(ticks, -1) + diff(head(ticks, 2)) / 2
    ans$bottom$ticks$at <- c(ticks, ticks2)
    ans$bottom$ticks$tck <- c(rep(1, length(ticks)), rep(0.5, length(ticks2)))
    ans$bottom$labels$at <- ans$bottom$ticks$at
    ans$bottom$labels$labels <- c(ans$bottom$labels$labels, rep(' ', length(ticks2)))
    ans$bottom$labels$check.overlap <- FALSE
    ans
}

## xyplot functions
panelEO <- function(hgrid=-1, ...) {
  lims <- current.panel.limits()
  panel.grid(h=hgrid, v = 0)
  panel.xyplot(...)
  panel.abline(h=lims$ylim[1]) # bottom
  panel.abline(h=0) # zero
}

axisEOleft <- function(side, line.col = "black", ...) {
  ## Only draw axes on the left and bottom
  if(side %in% c("left","bottom")) {
    ## Call default axis drawing function
    ## axis.default(side = side, line.col = "black",
    ##              ## ticks = "no",
    ##              ...)
    axis.default(side = "bottom", line.col = "black", ticks = "yes", ...)
    axis.default(side = "left", line.col = "black", ticks = "no", ...)
  }
}

axisEOright <- function(side, line.col = "black", ...) {
  ## Only draw axes on the left and bottom
  if(side %in% c("right","bottom")) {
    ## Call default axis drawing function
    ## axis.default(side = side, line.col = "black", ...)
    ## axis.default(side = side, line.col = "black",
    ##              ## ticks = "no",
    ##              labels = "yes",
    ##              ...)
    axis.default(side = "bottom", line.col = "black", ticks = "yes", ...)
    axis.default(side = "right", line.col = "black", ticks = "no", labels = "yes", ...)
  }
}

ylabGrid <- function(label=stop("'label' must be specified"),
                     hjust=0) {
  grid::textGrob(label = label, rot = 0,
                 vjust = -14, hjust = hjust)
}

par.settingsEO <- function(pal=stop("'pal' must be specified"),
                           pos="left") {
  par.settings <-
    list(
      axis.line = list(col = "transparent"),
      superpose.line = list(
        col = lineColors[[pal]],
        lwd = 4), # 2
      layout.heights = list(
        bottom.padding = -2
      )
    )

  ## str(trellis.par.get("layout.widths"))
  if (pos %in% c("left", "both")) {
    par.settings <-
      c(par.settings,
        list(layout.widths = list(
               ## makes label disappear
               right.padding = -2,
               ## try to fill gap from missing ticks
               ylab.axis.padding = -1
             ))
        )
  }

  if (pos=="right") {
    par.settings <-
      c(par.settings,
        list(layout.widths = list(
               left.padding = -6
             ))
        )
  }

  return(par.settings)
}
