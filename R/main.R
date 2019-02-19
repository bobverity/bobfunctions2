
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

get_projection <- function(x_lim, y_lim, z_lim, theta, phi, d) {
  
  # check inputs
  assert_limit(x_lim)
  assert_limit(y_lim)
  assert_limit(z_lim)
  assert_single_numeric(theta)
  assert_single_numeric(phi)
  assert_single_pos(d, zero_allowed = FALSE)
  
  # write perspective plot to temp file
  jpeg(tempfile())
  ret <- persp(matrix(0,2,2), xlim = x_lim, ylim = y_lim, zlim = z_lim, theta = theta, phi = phi, d = d)
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
  ret <- as.data.frame(sweep(tmp, 1, tmp[,4], "/")[,-4])
  names(ret) <- c("x", "y", "depth")
  return(ret)
}
