gauge <- function(value, min, max, sectors = gaugeSectors(),
									symbol = NULL, label = NULL,
									abbreviate = TRUE, abbreviateDecimals = 1,
									href = NULL) {
	
	if (!inherits(sectors, "gaugeSectors")) {
		stop("sectors must be a gaugeSectors() object")
	}
	
	# make sure at least one sector range is populated
	if (is.null(sectors$warning) && is.null(sectors$danger)) {
		sectors$success <- sectors$success %||% c(min, max)
	}
	
	x <- list(
		value = value,
		min = min,
		max = max,
		customSectors = sectors,
		symbol = symbol,
		label = label,
		humanFriendly = abbreviate,
		humanFriendlyDecimal = abbreviateDecimals,
		href = href
	)
	
	# create widget
	htmlwidgets::createWidget(
		name = 'gauge', x,
		package = 'flexdashboard',
		dependencies = rmarkdown::html_dependency_jquery(),
		preRenderHook = function(widget) {
			# bs_current_theme() will tell us if bslib is relevant, but we also need to
			# resolve accent colors in the non-bslib case
			theme <-  bslib::bs_current_theme() %||% getOption("flexdashboard.theme", "cosmo")
			
			# create the customSectors justgage payload
			sectors <- widget$x$customSectors
			colors <- sectors$colors
			ranges <- Map(
				sectors[c("success", "warning", "danger")], colors,
				f = function(sector, color) {
					if (is.null(sector)) return(NULL)
					if (!is.numeric(sector) || length(sector) != 2)
						stop("gaugeSector() ranges must be a numeric vector of length 2", call. = FALSE)
					list(lo = min(sector), hi = max(sector), color = color)
				}, USE.NAMES = FALSE
			)
			
			widget$x$customSectors <- list(
				percents = FALSE,
				ranges = dropNulls(ranges)
			)
			
			# Do no more if bslib isn't relevant
			if (!bslib::is_bs_theme(theme)) {
				return(widget)
			}
			
			# Supply smarter defaults for grayscale colors and fonts
			vars <- bslib::bs_get_variables(theme, c("bg", "fg", "font-family-base"))
			gray_pal <- scales::colour_ramp(
				htmltools::parseCssColors(vars[c("bg", "fg")])
			)
			defaults <- list(
				gaugeColor = "blue",
				valueFontColor = gray_pal(0.9),
				labelFontColor = gray_pal(0.65),
				valueFontFamily = vars[["font-family-base"]],
				labelFontFamily = vars[["font-family-base"]]
			)
			widget$x <- utils::modifyList(widget$x, defaults)
			widget
		}
	)
}

#' @export
#' @rdname gauge
gaugeSectors <- function(success = NULL, warning = NULL, danger = NULL,
												 colors = c("success", "warning", "danger")) {
	structure(
		list(
			success = success, warning = warning, danger = danger,
			colors = rep_len(colors %||% c("success", "warning", "danger"), 3)
		),
		class = "gaugeSectors"
	)
}

resolveAccentColors <- function(colors, theme) {
	if (!length(colors)) return(colors)
	
	idx <- vapply(colors, is_accent_color, logical(1))
	if (bslib::is_bs_theme(theme)) {
		accentMap <- getSassAccentColors(theme, unique(colors[idx]))
		colors[idx] <- accentMap[colors[idx]]
	} else if (is.character(theme)) {
		colors[idx] <- themeColors[[theme]][colors[idx]]
	}
	as.character(colors)
}

getSassAccentColors <- function(theme, accents = accent_colors()) {
	if ("3" %in% bslib::theme_version(theme)) {
		accents <- paste0("brand-", accents)
	}
	vals <- bslib::bs_get_variables(theme, accents)
	names(vals) <- sub("^brand-", "", accents)
	vals
}
