
#------------------------------------------------
# define basic package imports

#' @import graphics
#' @useDynLib bobfunctions2, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#------------------------------------------------
#' @title Dummy function
#'
#' @description Simple test function that demonstrates some of the features of
#'   the bellsandwhistles package.
#'
#' @details Takes a vector of values, returns the square.
#'
#' @param x vector of values
#'
#' @export
#' @examples
#' # Find square of first 100 values
#' dummy1(1:100)

dummy1 <- function(x = 1:5) {
  
  # print message to console
  message("running R dummy1 function")
  
  # get arguments in list form
  args <- list(x = x)
  
  # run C++ function with these arguments
  output_raw <- dummy1_cpp(args)
  
  # some optional processing of output
  message("processing output")
  ret <- output_raw$x_squared
  
  # return
  return(ret)
}

#------------------------------------------------
#' @title Produce script header text
#'
#' @description Produce header text to paste into new script (automatically 
#'   copied to the clipboard). Different header types are available for
#'   different types of script (see below).
#'
#' @param type switches between the following text blocks:
#'   \itemize{
#'     \item{type = 1: basic header including name, author, date and purpose}
#'     \item{type = 2: as above, plus list of useful commands and shortcuts when
#'     working with packages}
#'   }
#'
#' @export

header <- function(type = 1) {
  
  # check inputs
  assert_single_int(type)
  assert_in(type, 1:2)
  
  # make basic text list
  s <- list()
  s <- c(s, "")
  s <- c(s, "# .R")
  s <- c(s, "#")
  s <- c(s, "# Author: Bob Verity")
  s <- c(s, paste0("# Date: ", Sys.Date()))
  s <- c(s, "#")
  s <- c(s, "# Purpose:")
  s <- c(s, "# (this is an example header)")
  s <- c(s, "#")
  
  # type2 text
  if (type == 2) {
    s <- c(s, "")
    s <- c(s, "# RStudio shortcuts:")
    s <- c(s, "#    cmd+shift+L     : load package from local version")
    s <- c(s, "#    cmd+shift+D     : document (NB, option must be activated in Build Tools)")
    s <- c(s, "#    cmd+shift+E     : check")
    s <- c(s, "#    cmd+shift+T     : test")
    s <- c(s, "")
    s <- c(s, "# Useful commands:")
    s <- c(s, "# devtools::install()  # install package")
    s <- c(s, "# pkgdown::build_site() # build all pages of pkgdown website")
    s <- c(s, "# pkgdown::build_article('my-article')  # build single vignette")
    s <- c(s, "# check('.', args = '--no-examples')  # run checks without examples")
    s <- c(s, "# covr::report()    # interactive coverage report")
  }
  
  # further shared text
  s <- c(s, "# ------------------------------------------------------------------")
  s <- c(s, "")
  s <- c(s, "# load bobfunctions2 package. If not already installed this can be obtained from")
  s <- c(s, "# Github via the command devtools::install_github('bobverity/bobfunctions2')")
  s <- c(s, "library(bobfunctions2)")
  s <- c(s, "")
  
  # combine text
  ret <- paste(unlist(s), collapse = "\n")
  
  # copy to clipboard
  clip <- pipe("pbcopy", "w")                       
  write(ret, file = clip)                               
  close(clip)
  
  # print text to console
  cat(ret)
}

#------------------------------------------------
#' @title Produce function header text
#'
#' @description Produce header text for a single function to paste into script
#'   (automatically copied to the clipboard). Header text is written in roxygen
#'   format.
#'
#' @param type switches between the following text blocks:
#'   \enumerate{
#'     \item basic function header
#'     \item complete function header with all the bells and whistles
#'     \item function not exported
#'     \item S3 method, e.g. \code{print.my_class()}
#'   }
#'
#' @export

func_header <- function(type = 1) {
  
  # check inputs
  assert_single_int(type)
  assert_in(type, 1:4)
  
  # make type1 text list
  s <- list()
  if (type == 1) {
    s <- c(s, "#------------------------------------------------")
    s <- c(s, "#' @title TODO - title")
    s <- c(s, "#'")
    s <- c(s, "#' @description TODO - description")
    s <- c(s, "#'")
    s <- c(s, "#' @details TODO - details")
    s <- c(s, "#'")
    s <- c(s, "#' @param x TODO - parameter description.")
    s <- c(s, "#'")
    s <- c(s, "#' @import ggplot2")
    s <- c(s, "#' @importFrom stats prcomp")
    s <- c(s, "#' @export")
  }
  
  # make type2 text list
  if (type == 2) {
    s <- c(s, "#------------------------------------------------")
    s <- c(s, "#' @title TODO - title")
    s <- c(s, "#'")
    s <- c(s, "#' @description TODO - description")
    s <- c(s, "#'")
    s <- c(s, "#' @details TODO - details.")
    s <- c(s, "#'")
    s <- c(s, "#' Character formatting:")
    s <- c(s, "#' \\emph{italics}, \\strong{bold}, \\code{function(argument = value)}, \\pkg{package_name}")
    s <- c(s, "#'")
    s <- c(s, "#' Mathematics:")
    s <- c(s, "#' \\eqn{a + b}: inline eqution, \\deqn{a + b}: display (block) equation")
    s <- c(s, "#'")
    s <- c(s, "#' Numbered list:")
    s <- c(s, "#'   \\enumerate{")
    s <- c(s, "#'     \\item First item")
    s <- c(s, "#'     \\item Second item")
    s <- c(s, "#'   }")
    s <- c(s, "#'")
    s <- c(s, "#' Bulleted list:")
    s <- c(s, "#'   \\itemize{")
    s <- c(s, "#'     \\item First item")
    s <- c(s, "#'     \\item Second item")
    s <- c(s, "#'   }")
    s <- c(s, "#'")
    s <- c(s, "#' Named list:")
    s <- c(s, "#'   \\describe{")
    s <- c(s, "#'     \\item{One}{First item}")
    s <- c(s, "#'     \\item{Two}{Second item}")
    s <- c(s, "#'   }")
    s <- c(s, "#'")
    s <- c(s, "#' Links to other documentation:")
    s <- c(s, "#' \\code{\\link{function}}: link to function in this package. \\code{\\link[MASS]{abbey}}: link to function in another package. \\link[=dest]{name}: link to dest, but show name. \\code{\\link[MASS:abbey]{name}}: link to function in another package, but show name. \\linkS4class{abc}: link to an S4 class.")
    s <- c(s, "#'")
    s <- c(s, "#' Links to the web:")
    s <- c(s, "#' \\url{http://rstudio.com}. \\href{http://rstudio.com}{Rstudio}. \\email{hadley@@rstudio.com} (note the doubled @)")
    s <- c(s, "#'")
    s <- c(s, "#' @param x TODO - parameter description.")
    s <- c(s, "#' @param y TODO - parameter description.")
    s <- c(s, "#'   \\itemize{")
    s <- c(s, "#'     \\item First item")
    s <- c(s, "#'     \\item Second item")
    s <- c(s, "#'   }")
    s <- c(s, "#'")
    s <- c(s, "#' @references TODO - references")
    s <- c(s, "#'")
    s <- c(s, "#' @import ggplot2")
    s <- c(s, "#' @importFrom stats prcomp")
    s <- c(s, "#' @export")
    s <- c(s, "#' @examples")
    s <- c(s, "#' # TODO - example")
  }
  
  if (type == 3) {
    s <- c(s, "#------------------------------------------------")
    s <- c(s, "# TODO - simple description (for code purposes only)")
    s <- c(s, "#' @noRd")
  }
  
  if (type == 4) {
    s <- c(s, "#------------------------------------------------")
    s <- c(s, "# TODO - simple description (for code purposes only)")
    s <- c(s, "#' @method r_function clustom_class")
    s <- c(s, "#' @export")
  }
  
  # combine text
  ret <- paste(unlist(s), collapse = "\n")
  
  # copy to clipboard
  clip <- pipe("pbcopy", "w")                       
  write(ret, file = clip)                               
  close(clip)
  
  # print text to console
  cat(ret)
}

#------------------------------------------------
#' @title Bin values in two dimensions
#'
#' @description Bin values in two dimensions based on a vector of breaks in each
#'   dimension. Values are included in a bin if they are >= the left break and <
#'   the right break. Default breaks are chosen to encompass all the data.
#'
#' @param x first dimension of values to bin
#' @param y second dimension of values to bin
#' @param x_breaks set of breaks in x-dimension
#' @param y_breaks set of breaks in y-dimension
#'
#' @export

bin_2d <- function(x, y, x_breaks = NULL, y_breaks = NULL) {
  
  # check inputs
  assert_numeric(x)
  assert_vector(x)
  assert_numeric(y)
  assert_vector(y)
  assert_same_length(x, y)
  if (is.null(x_breaks)) {
    x_range <- buffer_range(min(x, na.rm = TRUE), max(x, na.rm = TRUE), buffer = 1.1)
    x_breaks <- seq(x_range[1], x_range[2], l = 101)
  }
  if (is.null(y_breaks)) {
    y_range <- buffer_range(min(y, na.rm = TRUE), max(y, na.rm = TRUE), buffer = 1.1)
    y_breaks <- seq(y_range[1], y_range[2], l = 101)
  }
  assert_numeric(x_breaks)
  assert_vector(x_breaks)
  assert_numeric(y_breaks)
  assert_vector(y_breaks)
  
  # get number of breaks in each dimension
  nx <- length(x_breaks)
  ny <- length(y_breaks)
  
  # create table of binned values
  tb <- table(findInterval(x, x_breaks), findInterval(y, y_breaks))
  
  # convert to dataframe and force numeric
  df <- as.data.frame(tb, stringsAsFactors = FALSE)
  names(df) <- c("x", "y", "count")
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)
  
  # subset to within breaks range
  df <- subset(df, x > 0 & x < nx & y > 0 & y < ny)
  
  # fill in matrix
  z <- matrix(0, ny-1, nx-1)
  z[cbind(df$y, df$x)] <- df$count
  
  # return output as list
  ret <- list(x_mids = midpoints(x_breaks),
              y_mids = midpoints(y_breaks),
              z = z)
  return(ret)
}

#------------------------------------------------
#' @title Find midpoints of a vector of breaks
#'
#' @description Find midpoints of a vector of breaks. The resulting vector will
#'   have one less element than the input vector.
#'
#' @param x a vector of breaks
#'
#' @export

midpoints <- function(x) {
  return((x[-1] + x[-length(x)])/2)
}

#------------------------------------------------
#' @title Expand range to include buffer
#'
#' @description Expand range to include buffer. The buffer distance is defined
#'   as a proportion of the range.
#'
#' @param x_min minimum of starting range
#' @param x_max maximum of starting range
#' @param buffer the proportional increase in the starting range
#'
#' @export

buffer_range <- function(x_min, x_max, buffer = 1.1) {
  
  # check inputs
  assert_single_numeric(x_min)
  assert_single_numeric(x_max)
  assert_gr(x_max, x_min)
  assert_single_pos(buffer, zero_allowed = FALSE)
  
  # create buffer range
  x_range <- (x_max - x_min)
  x_mid <- (x_max + x_min)/2
  ret <- c(x_mid - x_range/2*buffer, x_mid + x_range/2*buffer)
  
  return(ret)
}

#------------------------------------------------
#' @title Calculate pairwise great circle distance between points
#'
#' @description Analogue of the \code{dist()} function, but calculating great
#'   circle distances. Points should be input as a two-column matrix or
#'   dataframe with longitude in the first column and latitude in the second.
#'
#' @param x a two-column matrix or dataframe with longitude in the first column
#'   and latitude in the second
#'
#' @export

dist_gc <- function(x) {
  
  # check inputs
  assert_ncol(x, 2)
  
  # calculate distance matrix
  ret <- apply(x, 1, function(y) {lonlat_to_bearing(x[,1], x[,2], y[1], y[2])$gc_dist})
  diag(ret) <- 0
  
  return(ret)
}

#------------------------------------------------
#' @title Calculate great circle distance and bearing between coordinates
#'
#' @description Calculate great circle distance and bearing between spatial
#'   coordinates.
#'
#' @param origin_lon the origin longitude
#' @param origin_lat the origin latitude
#' @param dest_lon the destination longitude
#' @param dest_lat the destination latitude
#' @param earth_rad the assumed radius of the earth (km)
#'
#' @export

lonlat_to_bearing <- function(origin_lon, origin_lat, dest_lon, dest_lat, earth_rad = 6371) {
  
  # check inputs
  assert_numeric(origin_lon)
  assert_vector(origin_lon)
  assert_numeric(origin_lat)
  assert_vector(origin_lat)
  assert_same_length(origin_lon, origin_lat)
  assert_numeric(dest_lon)
  assert_vector(dest_lon)
  assert_numeric(dest_lat)
  assert_vector(dest_lat)
  assert_same_length(dest_lon, dest_lat)
  assert_single_pos(earth_rad, zero_allowed = FALSE)
  
  # convert input arguments to radians
  origin_lon <- origin_lon*2*pi/360
  origin_lat <- origin_lat*2*pi/360
  dest_lon <- dest_lon*2*pi/360
  dest_lat <- dest_lat*2*pi/360
  
  delta_lon <- dest_lon - origin_lon
  
  # calculate bearing
  bearing <- atan2(sin(delta_lon)*cos(dest_lat), cos(origin_lat)*sin(dest_lat)-sin(origin_lat)*cos(dest_lat)*cos(delta_lon))
  
  # calculate great circle angle. Use temporary variable to avoid acos(>1) or 
  # acos(<0), which can happen due to underflow issues
  tmp <- sin(origin_lat)*sin(dest_lat) + cos(origin_lat)*cos(dest_lat)*cos(delta_lon)
  tmp <- ifelse(tmp > 1, 1, tmp)
  tmp <- ifelse(tmp < 0, 0, tmp)
  gc_angle <- acos(tmp)
  
  # convert bearing from radians to degrees measured clockwise from due north,
  # and convert gc_angle to great circle distance via radius of earth (km)
  bearing <- bearing*360/(2*pi)
  bearing <- (bearing+360)%%360
  gc_dist <- earth_rad*gc_angle
  
  # return list
  ret <-list(bearing = bearing,
             gc_dist = gc_dist)
  return(ret)
}

#------------------------------------------------
#' @title Create layout matrix automatically from plot number
#'
#' @description Create a layout matrix that can be used with gridExtra to 
#'   arrange ggplot objects. The dimensions of the matrix are chosen 
#'   automatically from the number of plots to create a rectangular arrangement
#'   (portrait) of sufficient size.
#'
#' @param n number of plots in final layout
#'
#' @export

layout_mat <- function(n) {
  c <- ceiling(sqrt(n))
  if (c*(c-1) >= n) {
    return(matrix(1:(c*(c-1)), nrow = c, byrow = TRUE))
  } else {
    return(matrix(1:c^2, nrow = c, byrow = TRUE))
  }
}

#------------------------------------------------
#' @title Get projection matrix
#'
#' @description Get projection matrix from \code{persp()} function without
#'   producing plot. The angles of projection are given by \code{theta}
#'   (rotation) and \code{phi} (elevation), and the strength of perspective
#'   transformation is given by \code{d}.
#'
#' @param x_lim x-limits of \code{persp()} plot.
#' @param y_lim y-limits of \code{persp()} plot.
#' @param z_lim z-limits of \code{persp()} plot.
#' @param theta angle of rotation (degrees).
#' @param phi angle of elevation (degrees).
#' @param d strength of perspective transformation.
#'
#' @importFrom grDevices dev.off jpeg
#' @export

get_projection <- function(x_lim, y_lim, z_lim, theta, phi, d = 2) {
  
  # check inputs
  assert_limit(x_lim)
  assert_limit(y_lim)
  assert_limit(z_lim)
  assert_single_numeric(theta)
  assert_single_numeric(phi)
  assert_single_pos(d, zero_allowed = FALSE)
  
  # write perspective plot to temp file
  jpeg(tempfile())
  ret <- suppressWarnings(persp(matrix(0,2,2), xlim = x_lim, ylim = y_lim, zlim = z_lim, theta = theta, phi = phi, d = d))
  dev.off()
  
  return(ret)
}

#------------------------------------------------
#' @title Project 3D coordinates to 2D
#'
#' @description Use a projection matrix to transform 3D world coordinates to 2D
#'   coordinates - for example reprenting a camera viewing a 3D scene. See
#'   \code{get_projection()} for how to obtain a projection mtarix. This
#'   function is equivalent to the \code{trans3d()} function, but also stores
#'   depth information relative to the camera.
#'
#' @param x x- world coordinates.
#' @param y y- world coordinates.
#' @param z z- world coordinates.
#' @param proj_mat 4*4 projection matrix, as returned from
#'   \code{get_projection()}.
#'
#' @export

project_2d <- function(x, y, z, proj_mat) {
  tmp <- cbind(x,y,z,1) %*% proj_mat
  tmp <- sweep(tmp, 1, tmp[,4], "/")[, -4, drop = FALSE]
  ret <- as.data.frame(tmp)
  names(ret) <- c("x", "y", "depth")
  return(ret)
}

#------------------------------------------------
#' @title Project 3D line coordinates to 2D
#'
#' @description Use \code{project2D} to project start and end points from 3D
#'   world coordinates to 2D screen coordinates.
#'
#' @param x0 x-coordinates of start.
#' @param y0 y-coordinates of start.
#' @param z0 z-coordinates of start.
#' @param x1 x-coordinates of end.
#' @param y1 y-coordinates of end.
#' @param z1 z-coordinates of end.
#' @param proj_mat 4*4 projection matrix, as returned from
#'   \code{get_projection()}.
#'
#' @export

project_2d_line <- function(x0, y0, z0, x1, y1, z1, proj_mat) {
  
  # project start and end points
  point0 <- project_2d(x0, y0, z0, proj_mat)
  point1 <- project_2d(x1, y1, z1, proj_mat)
  
  # make dataframe
  ret <- data.frame(x0 = point0[,"x"], y0 = point0[,"y"], x1 = point1[,"x"], y1 = point1[,"y"])
  
  return(ret)
}

#------------------------------------------------
#' @title Add grid lines to ggplot perspective plot
#'
#' @description Add grid lines to a ggplot perspective plot.
#'
#' @details The vectors \code{x}, \code{y}, and \code{z} together define the
#'   locations and orientations of the lines. One of these vectors must be a
#'   series of breaks, one must be a limit (i.e. a vector of two values) and one
#'   must be a single value. For example, if \code{x = 1:5, y = c(-1,1), z = 3}
#'   then lines will be drawn parallel to the y-axis at x-values 1:5, spanning a
#'   range -1 to 1, and in the z-plane at position z = 3. If there are only two
#'   breaks then it becomes impossible to determine which axis represents breaks
#'   and which represents limits, hence the argument \code{break_axis} must be
#'   specified (otherwise this is chosen automatically).
#'
#' @param myplot an object of class \code{ggplot}.
#' @param x sequence of breaks \emph{or} a pair of start-end values \emph{or} a single value.
#' @param y (see \code{x} parameter).
#' @param z (see \code{x} parameter).
#' @param proj_mat 4*4 projection matrix, as returned from
#'   \code{get_projection()}.
#' @param col line colour.
#' @param size line size.
#' @param break_axis which axis to apply breaks over. If \code{NULL} then chosen
#'   automatically from other inputs, otherwise must be one of "x", "y" or "z".
#'
#' @export

gg3d_add_grid_lines <- function(myplot, x = seq(0,1,0.1), y = c(0,1), z = 0, proj_mat, col = grey(0.8), size = 0.5, break_axis = NULL) {
  
  # check inputs
  assert_custom_class(myplot, "ggplot")
  assert_vector(x)
  assert_increasing(x)
  assert_vector(y)
  assert_increasing(y)
  assert_vector(z)
  assert_increasing(z)
  assert_matrix(proj_mat)
  assert_numeric(proj_mat)
  assert_dim(proj_mat, c(4,4))
  
  # attempt to set break_axis from other inputs
  arg_lengths <- mapply(length, list(x,y,z))
  if (is.null(break_axis)) {
    if (!any(arg_lengths > 2)) {
      stop("unable to set break_axis automatically due to ambiguous inputs. Need to set this argument manually to 'x', 'y' or 'z'")
    }
    break_axis <- c("x", "y", "z")[arg_lengths > 2]
  }
  assert_in(break_axis, c("x", "y", "z"))
  
  # work out which axis is planar, and which is orthogonal to the break axis
  planar_axis <- setdiff(c("x", "y", "z")[arg_lengths == 1], break_axis)
  orthog_axis <- setdiff(c("x", "y", "z"), c(break_axis, planar_axis))
  
  # check x,y,z lengths
  switch (planar_axis,
    "x" = assert_single_numeric(x),
    "y" = assert_single_numeric(y),
    "z" = assert_single_numeric(z)
  )
  switch (orthog_axis,
    "x" = assert_limit(x),
    "y" = assert_limit(y),
    "z" = assert_limit(z)
  )
  
  # calculate start and end points of lines in 2D projection
  switch (orthog_axis,
    "x" = plot_df <- project_2d_line(x[1], y, z, x[2], y, z, proj_mat),
    "y" = plot_df <- project_2d_line(x, y[1], z, x, y[2], z, proj_mat),
    "z" = plot_df <- project_2d_line(x, y, z[1], x, y, z[2], proj_mat)
  )  
  
  # add to ggplot object
  myplot <- myplot + geom_segment(aes_(x = ~x0, y = ~y0, xend = ~x1, yend = ~y1), col = col, size = size, data = plot_df)
  
  # return invisibly
  invisible(myplot)
}

#------------------------------------------------
#' @title Produce a 3D scatterplot in ggplot format
#'
#' @description Produce a 3D scatterplot in ggplot format.
#'
#' @param x vector of values in the x-dimension.
#' @param y vector of values in the y-dimension.
#' @param z vector of values in the z-dimension.
#' @param colour vector specifying point colours. Can be continuous or discrete.
#' @param size size of data points.
#' @param theta angle of rotation (degrees).
#' @param phi angle of elevation (degrees).
#' @param d strength of perspective transformation.
#' @param x_lim plotting limits in the x-dimension.
#' @param y_lim plotting limits in the y-dimension.
#' @param z_lim plotting limits in the z-dimension.
#' @param x_grid sequence of x-breaks defining grid.
#' @param y_grid sequence of y-breaks defining grid.
#' @param z_grid sequence of z-breaks defining grid.
#' @param z_type switch between horizontal grid in the z-dimension (\code{z_type
#'   = 1}) and both horizontal and vertical grid (\code{z_type = 2}).
#' @param flip_grid_x if \code{TRUE} then the vertical grid in the x-axis is
#'   moved to the other side of the plot.
#' @param flip_grid_y if \code{TRUE} then the vertical grid in the y-axis is
#'   moved to the other side of the plot.
#' @param grid_col the colour of grid lines.
#' @param grid_size the size of grid lines.
#' @param axis_on whether to draw axis lines.
#' @param axis_col the colour of axis lines.
#' @param axis_size the size of axis lines.
#' @param zero_line_on whether to draw lines at zero in every dimension.
#' @param zero_line_col the colour of zero lines.
#' @param zero_line_size the size of zero lines.
#' @param x_lab the x-axis label.
#' @param y_lab the y-axis label.
#' @param z_lab the z-axis label.
#' @param tick_length the absolute length of axis ticks.
#' @param axis_lab_size the size of axis labels.
#' @param axis_lab_dist the absolute distance of axis labels from the edge of
#'   the plotting region.
#'
#' @importFrom grDevices grey
#' @import ggplot2
#' @export

gg3d_scatterplot <- function(x, y, z, colour = 1, size = 0.5, theta = 135, phi = 30, d = 2, x_lim = NULL, y_lim = NULL, z_lim = NULL, x_grid = NULL, y_grid = NULL, z_grid = NULL, z_type = 2, flip_grid_x = FALSE, flip_grid_y = FALSE, grid_col = grey(0.8), grid_size = 0.25, zero_line_on = TRUE, zero_line_col = grey(0.8), zero_line_size = 0.6, axis_on = FALSE, axis_col = "black", axis_size = 0.5, x_lab = "x", y_lab = "y", z_lab = "z", tick_length = 0.2, axis_lab_size = 3, axis_lab_dist = 2) {
  
  # check inputs
  assert_vector(x)
  assert_numeric(x)
  assert_vector(y)
  assert_numeric(y)
  assert_vector(z)
  assert_numeric(z)
  assert_single_pos(size)
  assert_single_numeric(theta)
  assert_single_numeric(phi)
  assert_single_pos(d)
  if (!is.null(x_lim)) {
    assert_limit(x_lim)
  }
  if (!is.null(y_lim)) {
    assert_limit(y_lim)
  }
  if (!is.null(z_lim)) {
    assert_limit(z_lim)
  }
  if (!is.null(x_grid)) {
    assert_vector(x_grid)
    assert_numeric(x_grid)
  }
  if (!is.null(y_grid)) {
    assert_vector(y_grid)
    assert_numeric(y_grid)
  }
  if (!is.null(z_grid)) {
    assert_vector(z_grid)
    assert_numeric(z_grid)
  }
  assert_in(z_type, 1:2)
  assert_single_logical(flip_grid_x)
  assert_single_logical(flip_grid_y)
  assert_single_pos(grid_size)
  assert_single_logical(axis_on)
  assert_single_pos(axis_size)
  assert_single_logical(zero_line_on)
  assert_single_pos(zero_line_size)
  assert_single_pos(tick_length)
  assert_single_pos(axis_lab_size)
  assert_single_pos(axis_lab_dist)
  
  # get pretty breaks in each dimension
  x_pretty <- pretty(x)
  y_pretty <- pretty(y)
  z_pretty <- pretty(z)
  
  # set default limits
  x_lim <- define_default(x_lim, range(x_pretty))
  y_lim <- define_default(y_lim, range(y_pretty))
  z_lim <- define_default(z_lim, range(z_pretty))
  
  # set default grid lines
  if (is.null(x_grid)) {
    delta_x <- x_pretty[2]-x_pretty[1]
    x_grid <- seq(floor(x_lim[1]/delta_x)*delta_x, ceiling(x_lim[2]/delta_x)*delta_x, delta_x)
    x_grid <- x_grid[x_grid >= x_lim[1] & x_grid <= x_lim[2]]
  }
  if (is.null(y_grid)) {
    delta_y <- y_pretty[2]-y_pretty[1]
    y_grid <- seq(floor(y_lim[1]/delta_y)*delta_y, ceiling(y_lim[2]/delta_y)*delta_y, delta_y)
    y_grid <- y_grid[y_grid >= y_lim[1] & y_grid <= y_lim[2]]
  }
  if (is.null(z_grid)) {
    delta_z <- z_pretty[2]-z_pretty[1]
    z_grid <- seq(floor(z_lim[1]/delta_z)*delta_z, ceiling(z_lim[2]/delta_z)*delta_z, delta_z)
    z_grid <- z_grid[z_grid >= z_lim[1] & z_grid <= z_lim[2]]
  }
  
  # get scaling factor in each dimension
  max_scale <- max(diff(x_lim), diff(y_lim), diff(z_lim))
  scale_factor <- c(diff(x_lim), diff(y_lim), diff(z_lim))/max_scale
  
  # get projection matrix
  proj_mat <- get_projection(x_lim = x_lim/scale_factor[1], y_lim = y_lim/scale_factor[2], z_lim = z_lim/scale_factor[3], theta = theta, phi = phi, d = d)
  
  # produce basic plot
  plot1 <- ggplot() + theme(panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank(),
                            panel.background = element_blank(),
                            axis.title.x = element_blank(),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.title.y = element_blank(),
                            axis.text.y = element_blank(),
                            axis.ticks.y = element_blank())
  
  # choose where to put vertical planes
  xv <- x_lim
  if (flip_grid_x) {
    xv <- rev(x_lim)
  }
  yv <- y_lim
  if (flip_grid_y) {
    yv <- rev(y_lim)
  }
  
  # z-plane gridlines
  plot1 <- gg3d_add_grid_lines(plot1, x = x_grid, y = y_lim, z = z_lim[1], proj_mat = proj_mat,
                               col = grid_col, size = grid_size, break_axis = "x")
  plot1 <- gg3d_add_grid_lines(plot1, x = x_lim, y = y_grid, z = z_lim[1], proj_mat = proj_mat,
                               col = grid_col, size = grid_size, break_axis = "y")
  
  # x-plane gridlines
  plot1 <- gg3d_add_grid_lines(plot1, x = x_lim, y = yv[1], z = z_grid, proj_mat = proj_mat,
                               col = grid_col, size = grid_size, break_axis = "z")
  if (z_type == 2) {
    plot1 <- gg3d_add_grid_lines(plot1, x = x_grid, y = yv[1], z = z_lim, proj_mat = proj_mat,
                                 col = grid_col, size = grid_size, break_axis = "x")
  }
  
  # y-plane gridlines
  plot1 <- gg3d_add_grid_lines(plot1, x = xv[1], y = y_lim, z = z_grid, proj_mat = proj_mat,
                               col = grid_col, size = grid_size, break_axis = "z")
  if (z_type == 2) {
    plot1 <- gg3d_add_grid_lines(plot1, x = xv[1], y = y_grid, z = z_lim, proj_mat = proj_mat,
                                 col = grid_col, size = grid_size, break_axis = "y")
  }
  
  # add zero lines
  if (zero_line_on) {
    plot1 <- gg3d_add_grid_lines(plot1, x = 0, y = y_lim, z = z_lim[1], proj_mat = proj_mat,
                                 col = zero_line_col, size = zero_line_size, break_axis = "x")
    plot1 <- gg3d_add_grid_lines(plot1, x = 0, y = yv[1], z = z_lim, proj_mat = proj_mat,
                                 col = zero_line_col, size = zero_line_size, break_axis = "y")
    plot1 <- gg3d_add_grid_lines(plot1, x = x_lim, y = 0, z = z_lim[1], proj_mat = proj_mat,
                                 col = zero_line_col, size = zero_line_size, break_axis = "y")
    plot1 <- gg3d_add_grid_lines(plot1, x = xv[1], y = 0, z = z_lim, proj_mat = proj_mat,
                                 col = zero_line_col, size = zero_line_size, break_axis = "x")
    plot1 <- gg3d_add_grid_lines(plot1, x = xv[1], y = y_lim, z = 0, proj_mat = proj_mat,
                                 col = zero_line_col, size = zero_line_size, break_axis = "z")
    plot1 <- gg3d_add_grid_lines(plot1, x = x_lim, y = yv[1], z = 0, proj_mat = proj_mat,
                                 col = zero_line_col, size = zero_line_size, break_axis = "z")
  }
  
  # add axis lines
  if (axis_on) {
    plot1 <- gg3d_add_grid_lines(plot1, x = xv[1], y = y_lim, z = z_lim[1], proj_mat = proj_mat,
                                 col = axis_col, size = axis_size, break_axis = "x")
    plot1 <- gg3d_add_grid_lines(plot1, x = x_lim, y = yv[1], z = z_lim[1], proj_mat = proj_mat,
                                 col = axis_col, size = axis_size, break_axis = "y")
    plot1 <- gg3d_add_grid_lines(plot1, x = xv[1], y = yv[1], z = z_lim, proj_mat = proj_mat,
                                 col = axis_col, size = axis_size, break_axis = "x")
  }
  
  # calculate tick lengths
  delta_x <- tick_length
  delta_y <- tick_length
  delta_z <- -tick_length
  
  # choose where to put ticks
  if (flip_grid_x) {
    delta_x <- -delta_x
  }
  if (flip_grid_y) {
    delta_y <- -delta_y
  }
  
  # calculate tick lines
  tick_x <- project_2d_line(x_grid, yv[2], z_lim[1], x_grid, yv[2] + delta_y, z_lim[1] + delta_z, proj_mat)
  tick_y <- project_2d_line(xv[2], y_grid, z_lim[1], xv[2] + delta_x, y_grid, z_lim[1] + delta_z, proj_mat)
  tick_z <- project_2d_line(xv[1], yv[2], z_grid, xv[1] - delta_x, yv[2] + delta_y, z_grid, proj_mat)
  
  # add ticks
  tick_all <- rbind(tick_x, tick_y, tick_z)
  plot1 <- plot1 + geom_segment(aes_(x = ~x0, y = ~y0, xend = ~x1, yend = ~y1), col = grey(0.8), size = 0.25, data = tick_all)
  
  # calculate tick text positions
  tick_text_x <- project_2d(x_grid, yv[2] + delta_y*1.5, z_lim[1] + delta_z*1.5, proj_mat)
  tick_text_x$label <- x_grid
  tick_text_y <- project_2d(xv[2] + delta_x*1.5, y_grid, z_lim[1] + delta_z*1.5, proj_mat)
  tick_text_y$label <- y_grid
  tick_text_z <- project_2d(xv[1] - delta_x*1.5, yv[2] + delta_y*1.5, z_grid, proj_mat)
  tick_text_z$label <- z_grid
  
  # add tick text
  plot1 <- plot1 + geom_text(aes_(x = ~x, y = ~y, label = ~label), size = 2, data = tick_text_x)
  plot1 <- plot1 + geom_text(aes_(x = ~x, y = ~y, label = ~label), size = 2, data = tick_text_y)
  plot1 <- plot1 + geom_text(aes_(x = ~x, y = ~y, label = ~label), size = 2, data = tick_text_z)
  
  # calculate axis label positions
  axis_text_x <- project_2d(mean(x_lim), yv[2] + axis_lab_dist, z_lim[1], proj_mat)
  axis_text_x$label <- x_lab
  axis_text_y <- project_2d(xv[2] + axis_lab_dist, mean(y_lim), z_lim[1], proj_mat)
  axis_text_y$label <- y_lab
  axis_text_z <- project_2d(xv[1], yv[2] + axis_lab_dist, mean(z_lim), proj_mat)
  axis_text_z$label <- z_lab
  
  # add axis labels
  plot1 <- plot1 + geom_text(aes_(x = ~x, y = ~y, label = ~label), size = axis_lab_size, data = axis_text_x)
  plot1 <- plot1 + geom_text(aes_(x = ~x, y = ~y, label = ~label), size = axis_lab_size, data = axis_text_y)
  plot1 <- plot1 + geom_text(aes_(x = ~x, y = ~y, label = ~label), size = axis_lab_size, data = axis_text_z)
  
  # convert data coordinates
  data_df <- project_2d(x, y, z, proj_mat)
  data_df$col <- colour
  data_df <- data_df[order(data_df$depth, decreasing = FALSE),]
  
  # add data
  plot1 <- plot1 + geom_point(aes_(x = ~x, y = ~y, col = ~col), size = size, data = data_df)
  
  # return plotting object
  return(plot1)
}

#------------------------------------------------
#' @title Add inset plot to faceted ggplot
#'
#' @description Ordinarily ggplot is not setup to add inset plots (annotations) to faceted plots, because it wants to add the same inset to every facet. This custom annotation function makes it possible to add an inset to a single facet. Credit goes to \href{https://stackoverflow.com/users/471093/baptiste}{baptiste} for \href{https://stackoverflow.com/questions/37867758/insetting-on-facet-grided-and-grid-arrangeed-plot}{this} solution.
#'
#' @param grob a ggplot object.
#' @param xmin inset starts at this coordinate. Set to \code{-Inf} to always
#'   start from far left edge.
#' @param xmax inset ends at this coordinate. Set to \code{Inf} to always start
#'   from far right edge.
#' @param ymin inset starts at this coordinate. Set to \code{-Inf} to always
#'   start from far bottom edge.
#' @param ymax inset ends at this coordinate. Set to \code{Inf} to always start
#'   from far top edge.
#' @param data dataframe, should specify the value of the variable with which
#'   the original plot is facetted (see examples).
#'
#' @import ggplot2
#' @export
#' @examples
#' # produce main plot and inset plot
#' plot_df <- data.frame(x = rnorm(1e3), y = rnorm(1e3), group = 1:2)
#' plot_main <- ggplot2::ggplot() +
#'              ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = plot_df) +
#'              ggplot2::facet_wrap(~group)
#' plot_inset <- ggplot2::qplot(rnorm(1e3))
#' 
#' # use gg_inset to annotate first panel only
#' plot_combined <- plot_main + gg_inset(ggplot2::ggplotGrob(plot_inset), data = data.frame(group = 1),
#'                                       xmin = -Inf, xmax = 1.5, ymin = 0.6, ymax = Inf)
#' 
#' plot_combined

gg_inset <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2::GeomCustomAnn,
        inherit.aes = TRUE, params = list(grob = grob, 
                                          xmin = xmin, xmax = xmax, 
                                          ymin = ymin, ymax = ymax))
}

#------------------------------------------------
#' @title Convert DNA sequence to amino-acid
#'
#' @description Convert three-letter sequence of ATGC to corrseponding
#'   translated amino-adid sequence. Amino-acids can be reported in full name or
#'   single-letter code. Function is most efficient when using a vector of
#'   sequences.
#'
#' @param x three-letter character sequence of A, T, G and C.
#' @param output_format 1 for full name, 2 for single-letter code.
#'
#' @export

dna_to_aa <- function(x, output_format = 1) {
  
  v1 <- c("TTT", "TTC", "TTA", "TTG", "TCT", "TCC", "TCA", "TCG", "TAT", 
          "TAC", "TAA", "TAG", "TGT", "TGC", "TGA", "TGG", "CTT", "CTC", 
          "CTA", "CTG", "CCT", "CCC", "CCA", "CCG", "CAT", "CAC", "CAA", 
          "CAG", "CGT", "CGC", "CGA", "CGG", "ATT", "ATC", "ATA", "ATG", 
          "ACT", "ACC", "ACA", "ACG", "AAT", "AAC", "AAA", "AAG", "AGT", 
          "AGC", "AGA", "AGG", "GTT", "GTC", "GTA", "GTG", "GCT", "GCC", 
          "GCA", "GCG", "GAT", "GAC", "GAA", "GAG", "GGT", "GGC", "GGA", 
          "GGG")
  
  if (output_format == 1) {
    v2 <- c("phe", "phe", "leu", "leu",
            "ser", "ser", "ser", "ser",
            "tyr", "tyr", "STOP", "STOP",
            "cys", "cys", "STOP", "trp",
            "leu", "leu", "leu", "leu",
            "pro", "pro", "pro", "pro",
            "his", "his", "gln", "gln",
            "arg", "arg", "arg", "arg",
            "ile", "ile", "ile", "met",
            "thr", "thr", "thr", "thr",
            "asn", "asn", "lys", "lys",
            "ser", "ser", "arg", "arg",
            "val", "val", "val", "val",
            "ala", "ala", "ala", "ala",
            "asp", "asp", "glu", "glu",
            "gly", "gly", "gly", "gly")
  } else {
    v2 <- c("F", "F", "L", "L",
            "S", "S", "S", "S",
            "Y", "Y", "*", "*",
            "C", "C", "*", "W",
            "L", "L", "L", "L",
            "P", "P", "P", "P",
            "H", "H", "Q", "Q",
            "R", "R", "R", "R",
            "I", "I", "I", "M",
            "T", "T", "T", "T",
            "N", "N", "K", "K",
            "S", "S", "R", "R",
            "V", "V", "V", "V",
            "A", "A", "A", "A",
            "D", "D", "E", "E",
            "G", "G", "G", "G")
  }
  
  # get return sequence
  ret <- v2[match(x, v1)]
  
  return(ret)
}

#------------------------------------------------
#' @title Simulate from simple Wright-Fisher model
#'
#' @description Simulate Wright-Fisher evolution. The model used here is
#'   currently very basic and makes a number of simplifying assumptions, but may
#'   be extended in future to add flexibility.
#'
#' @details Currently assumes haploid population and independent loci (no
#'   linkage disequilibrium). Initialises from symmetric Dirichlet(1) allele
#'   frequencies at every locus. Due to the way migration is currently
#'   implemented, \code{N} must be the same for all demes.
#'
#' @param N number of individuals per deme - currently must be the same for all
#'   demes.
#' @param L number of loci.
#' @param alleles number of alleles. Can be single number for all loci, or
#'   vector of length \code{L}.
#' @param mu mutation rate. Assumes finite-alleles model, with equal chance of
#'   mutating from any allele to any other.
#' @param m_matrix migration matrix specifying the per-generation probability of
#'   an individual migrating from any deme (in rows) to any other deme (in
#'   columns).
#' @param t_out vector of times at which results will be output.
#' @param output_format choose the output format. 1 = counts, 2 = list of
#'   genotypes over demes, 3 = matrix of genotypes over demes.
#'
#' @importFrom utils txtProgressBar
#' @export

sim_wrightfisher <- function(N, L, alleles, mu, m_matrix, t_out, output_format = 3) {
  
  # check inputs
  assert_vector(N)
  assert_pos_int(N, zero_allowed = FALSE)
  if (any(N != N[1])) {
    stop("due to the way migration is implemented, the model is currently limited to using the same number of individuals in every deme")
  }
  assert_single_pos_int(L, zero_allowed = FALSE)
  assert_vector(alleles)
  assert_pos_int(alleles, zero_allowed = FALSE)
  assert_gr(alleles, 1)
  assert_single_bounded(mu)
  assert_symmetric_matrix(m_matrix)
  assert_dim(m_matrix, rep(length(N),2))
  assert_bounded(m_matrix)
  if (!all(rowSums(m_matrix) == 1)) {
    stop("every row of m_matrix must sum to 1")
  }
  assert_vector(t_out)
  assert_pos_int(t_out, zero_allowed = FALSE)
  assert_in(output_format, 1:3)
  
  # process some inputs
  if (length(alleles) == 1) {
    alleles <- rep(alleles, L)
  }
  assert_length(alleles, L)
  K <- length(N)
  
  # make argument list
  args <- list(N = N,
               L = L,
               alleles = alleles,
               mu = mu,
               m_matrix = matrix_to_rcpp(m_matrix),
               t_out = t_out)
  
  # create progress bars
  pb <- txtProgressBar(min = 0, max = max(t_out)-1, initial = NA, style = 3)
  args_progress <- list(pb = pb)
  
  # functions to pass to C++
  args_functions <- list(update_progress = update_progress)
  
  # run efficient C++ function
  output_raw <- sim_wrightfisher_cpp(args, args_functions, args_progress)
  
  # process results
  output_processed <- mapply(function(t) {
    ret <- mapply(function(i, t) {
      ret <- t(mapply(function(x) x[[i]], output_raw$pop[[t]]))
      colnames(ret) <- paste0("a", 1:ncol(ret))
      rownames(ret) <- paste0("deme", 1:nrow(ret))
      return(ret)
    }, 1:L, t = t, SIMPLIFY = FALSE)
    names(ret) <- paste0("locus", 1:length(ret))
    return(ret)
  }, 1:length(t_out), SIMPLIFY = FALSE)
  names(output_processed) <- paste0("t", t_out)
  
  # format 2
  if (output_format %in% c(2,3)) {
    
    output_processed <- mapply(function(t) {
      ret <- mapply(function(k,t) {
        ret <- mapply(function(x,k) {
          sample(rep(1:ncol(x), times = x[k,]))
        }, output_processed[[t]], k = k)
        rownames(ret) <- paste0("ind", 1:nrow(ret))
        return(ret)
      }, 1:K, t = t, SIMPLIFY = FALSE)
      names(ret) <- paste0("deme", 1:K)
      return(ret)
    }, 1:length(t_out), SIMPLIFY = FALSE)
    names(output_processed) <- paste0("time", t_out)
    
  }
  
  # format 3
  if (output_format == 3) {
    
    output_processed <- mapply(function(x) {
      ret <- do.call(rbind, x)
      ret <- cbind(rep(1:length(x), times = mapply(nrow, x)), ret)
      colnames(ret)[1] <- "deme"
      return(ret)
    }, output_processed, SIMPLIFY = FALSE)
    names(output_processed) <- paste0("time", t_out)
    
  }
  
  return(output_processed)
}

#------------------------------------------------
#' @title Return complementary DNA or RNA sequence
#'
#' @description Return complementary DNA or RNA sequence.
#'
#' @param x input DNA sequence as character string.
#' @param format_rna logical. If \code{TRUE} then A complements to U, rather than T.
#'
#' @export

dna_complement <- function(x, format_rna = FALSE) {
  ret <- gsub("G", "X", toupper(x))
  ret <- gsub("C", "G", ret)
  ret <- gsub("X", "C", ret)
  
  ret <- gsub("A", "X", ret)
  ret <- gsub("T", "A", ret)
  if (format_rna) {
    ret <- gsub("X", "U", ret)
  } else {
    ret <- gsub("X", "T", ret)
  }
  return(ret)
}

#------------------------------------------------
#' @title rbind a list of matrices into a single matrix
#'
#' @description rbind a list of matrices into a single matrix. All matrices must
#'   have the same number of columns.
#'
#' @param l list of matrices.
#'
#' @export

list_to_matrix <- function(l) {
  
  # check inputs
  assert_list(l)
  
  # return if single element
  if (length(l) == 1) {
    return(l)
  }
  
  # check same ncol of all elements
  l_col <- mapply(ncol, l)
  if (any(l_col != l_col[1])) {
    stop("all matrices must have the same number of columns")
  }
  
  # rbind all matrices
  ret <- do.call(rbind, l)
  
  return(ret)
}

#------------------------------------------------
#' @title Print summary of differences and intersections of two sets
#'
#' @description Given two sets s1 and s2, print four values:
#'   \enumerate{
#'     \item the number of s1 not in s2
#'     \item the number of s1 in s2
#'     \item the number of s2 in s1
#'     \item the number of s2 not in s1
#'   }
#' 
#' The second and third values will be the same when there are no duplicates,
#' but otherwise could be different.
#'
#' @param s1 first set.
#' @param s2 second set.
#'
#' @export

set_compare <- function(s1, s2) {
  
  # check inputs
  assert_vector(s1)
  assert_vector(s2)
  
  # set operations
  v2 <- sum(s1 %in% s2)
  v1 <- length(s1) - v2
  v3 <- sum(s2 %in% s1)
  v4 <- length(s2) - v3
  ret <- c(set1_only = v1,
           set1_in_set2 = v2,
           set2_in_set1 = v3,
           set2_only = v4)
  
  return(ret)
}

#------------------------------------------------
#' @title Convert vector to matrix
#'
#' @description Given two input vectors, expands these vectors into matrices
#'   based on the dimensions of the other vector. Returns either the matrix in
#'   x, or the matrix in y.
#'
#' @param x,y two vectors of any type.
#' @param dim which dimension of the output matrix to return.
#'
#' @export

vec2mat <- function(x, y, dim) {
  
  if (dim == 1) {
    output <- matrix(rep(x, each = length(y)), length(y))
  } else {
    output <- matrix(rep(y, length(x)), length(y))
  }
  return(output)
}

#------------------------------------------------
#' @title Produce smooth colours from values
#'
#' @description Read in continuous values between xmin and xmax and return
#'   colours associated with these values, taken from a smoothly varying scale.
#'
#' @param x values from which to obtain colours.
#' @param xmin,xmax minimum and maximum values of x.
#' @param n_levels number of colours in palette.
#' @param raw_cols colours that make up the palette.
#'
#' @export

smooth_cols <- function(x,
                        xmin = min(x, na.rm = T),
                        xmax = max(x, na.rm = T),
                        n_levels = 1e3,
                        raw_cols = col_tim()) {
  
  # get x between 0 and 1
  x <- (x - xmin)/(xmax - xmin)
  
  # make smooth colours
  my_pal <- colorRampPalette(raw_cols)
  cols <- my_pal(n_levels+1)[floor(x*n_levels) + 1]
  
  return(cols)
}

#------------------------------------------------
#' @title Generate Perlin noise
#'
#' @description Generates 2D Perlin noise of any scale, in a matrix of any size.
#'   Credit to
#'   \href{https://stackoverflow.com/users/1129973/vincent-zoonekynd}{Vincent
#'   Zoonekynd} for
#'   \href{https://stackoverflow.com/questions/15387328/realistic-simulated-elevation-data-in-r-perlin-noise}{this
#'   answer}.
#'
#' @param out_rows,out_cols rows and columnds in output matrix.
#' @param levels_x,levels_y bumpyness in x and y dimension.
#'
#' @export

perlin_noise = function(out_rows = 128,
                        out_cols = 128,
                        levels_x = 10,
                        levels_y = 10) {
  
  # check inputs
  assert_single_pos_int(out_rows, zero_allowed = FALSE)
  assert_single_pos_int(out_cols, zero_allowed = FALSE)
  assert_single_pos_int(levels_x, zero_allowed = FALSE)
  assert_greq(levels_x, 2)
  assert_single_pos_int(levels_y, zero_allowed = FALSE)
  assert_greq(levels_y, 2)
  
  # convert to more convenient names
  M = out_rows
  N = out_cols
  n = levels_x
  m = levels_y
  
  # for each point on this n*m grid, choose a unit 1 vector
  vector_field <- apply(array(rnorm(2*n*m), dim = c(2,n,m)), 2:3, function(u) u/sqrt(sum(u^2)))
  
  f <- function(x,y) {
    
    # find the grid cell in which the point (x,y) lies
    i <- floor(x)
    j <- floor(y)
    stopifnot( i >= 1 || j >= 1 || i < n || j < m )
    
    # the 4 vectors, from the vector field, at the vertices of the square
    v1 <- vector_field[,i,j]
    v2 <- vector_field[,i+1,j]
    v3 <- vector_field[,i,j+1]
    v4 <- vector_field[,i+1,j+1]
    
    # vectors from the point to the vertices
    u1 <- c(x,y) - c(i,j)
    u2 <- c(x,y) - c(i+1,j)
    u3 <- c(x,y) - c(i,j+1)
    u4 <- c(x,y) - c(i+1,j+1)
    
    # scalar products
    a1 <- sum( v1 * u1 )
    a2 <- sum( v2 * u2 )
    a3 <- sum( v3 * u3 )
    a4 <- sum( v4 * u4 )
    
    # weighted average of the scalar products
    s <- function(p) 3*p^2 - 2*p^3
    p <- s( x - i )
    q <- s( y - j )
    b1 <- (1-p)*a1 + p*a2
    b2 <- (1-p)*a3 + p*a4
    (1-q) * b1 + q * b2
  }
  
  xs <- seq(from = 1, to = n, length = N+1)[-(N+1)]
  ys <- seq(from = 1, to = m, length = M+1)[-(M+1)]
  outer( xs, ys, Vectorize(f) )
}

#------------------------------------------------
#' @title Generate fractal noise
#'
#' @description Generates 2D fractal noise by layering Perlin noise at different levels.
#'
#' @param out_rows,out_cols rows and columnds in output matrix.
#' @param a scale parameter. Values near zero place more weight on large
#'   features, a value of 1 places equal weight on all levels, and values > 1
#'   place more weight on small features.
#' @param n_levels number of levels of Perlin noise to layer.
#'
#' @importFrom stats rnorm
#' @export

fractal_noise = function(out_rows = 128,
                        out_cols = 128,
                        a = 0.6,
                        n_levels = 7) {
  
  # check inputs
  assert_single_pos_int(out_rows, zero_allowed = FALSE)
  assert_single_pos_int(out_cols, zero_allowed = FALSE)
  assert_single_pos(a, zero_allowed = FALSE)
  assert_single_pos_int(n_levels, zero_allowed = FALSE)
  
  # layer Perlin noise
  ret <- 0
  for (i in 1:n_levels) {
    ret <- ret + a^i*perlin_noise(out_rows, out_cols, 2^i, 2^i)
  }
  
  return(ret)
}

#------------------------------------------------
#' @title Apply box blur to a matrix
#'
#' @description Smooths a matrix of values by applying a box blur, in which each
#'   pixel of a matrix is replaced with the average value of pixels in a box
#'   around it.
#'
#' @param m a matrix of values to be blurred.
#' @param d the distance either side of each pixel that is searched when
#'   blurring
#'
#' @export

box_blur = function(m, d = 5) {
  
  # check inputs
  assert_matrix(m)
  assert_numeric(m)
  assert_single_pos_int(d, zero_allowed = FALSE)
  
  # run efficient C++ function
  ret <- box_blur_cpp(m, d)
  
  return(ret)
}
