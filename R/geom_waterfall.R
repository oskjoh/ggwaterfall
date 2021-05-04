# TODO
# Add segments between rectangles
# Take control over x-variable classes


# ** StatBump ------------------------------------------------------------------
StatWaterfall <- ggplot2::ggproto("StatWaterfall", ggplot2::Stat,
                                  extra_params = c("grand_total", "na.rm", "width"),
                                  setup_data = function(data, params) {

                                    if(params$grand_total) {
                                      data <- data %>%
                                        bind_rows(tibble(y = 0, x = max(data$x) + 1)) %>%
                                        fill(PANEL, group)
                                    }

                                    data %>%
                                      mutate(delta_y = y - lag(y, default = 0),
                                             xmin = x - width*.5,
                                             xmax = x + width*.5,
                                             ymin = lag(y, default = 0),
                                             ymax = lag(y, default = 0) + delta_y)

                                  },
                                  compute_group = function(data, scales) {
                                    data
                                  },
                                  required_aes = c("x", "y")
)

# ** geom_waterfall -----------------------------------------------------------------
#' @title geom_waterfall
#'
#' Creates a ggplot that makes a smooth rank over time. To change the `smooth`
#' argument you need to put it outside of the `aes` of the geom. Uses the x and y aestethics.
#' Usually you want to compare multiple lines and if so, use the `color` aestethic.
#' To change the direction of the curve to 'vertical' set `direction = "y`
#'
#' @param mapping provide you own mapping. both x and y need to be numeric.
#' @param data provide you own data
#' @param geom change geom
#' @param position change position
#' @param na.rm remove missing values
#' @param show.legend show legend in plot
#' @param smooth how much smooth should the curve have? More means steeper curve.
#' @param direction the character x or y depending of smoothing direction
#' @param inherit.aes should the geom inherits aestethics
#' @param ... other arguments to be passed to the geom
#'
#' @return ggplot layer
#'
#' @examples
#' library(ggplot2)
#' library(ggbump)
#' df <- data.frame(country = c(
#'   "India", "India", "India",
#'   "Sweden", "Sweden", "Sweden",
#'   "Germany", "Germany", "Germany",
#'   "Finland", "Finland", "Finland"),
#' year = c(2011, 2012, 2013,
#' 2011, 2012, 2013,
#' 2011, 2012, 2013,
#' 2011, 2012, 2013),
#' month = c("January", "July", "November",
#'           "January", "July", "November",
#'           "January", "July", "November",
#'           "January", "July", "November"),
#' rank = c(4, 2, 2, 3, 1, 4, 2, 3, 1, 1, 4, 3))
#'
#' # Contingous x axis
#' ggplot(df, aes(year, rank, color = country)) +
#'   geom_point(size = 10) +
#'   geom_bump(size = 2)
#'
#' # Discrete x axis
#' ggplot(df, aes(month, rank, color = country)) +
#'   geom_bump(size = 2)
#'
#' @export
geom_waterfall <- function(mapping = NULL, data = NULL, geom = "rect",
                           position = "identity", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, grand_total = TRUE,
                           width = .9, ...) {
  ggplot2::layer(
    stat = StatWaterfall, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, grand_total = grand_total, width = width, ...)
  )
}



# df %>%
# ggplot() +
#   geom_rect(aes(xmin = time - 0.45, xmax = time + 0.45, ymin = balance, ymax = balance + change)) +
#   geom_text(aes(x = time, y = pmin(balance, balance + change) - 50, label = scales::dollar(change)), hjust = 0.5, vjust = 1, size = 3)


