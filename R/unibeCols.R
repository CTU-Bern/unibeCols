#' University of Bern corporate design colours
#' Colours taken from pages 18 to 20 of https://intern.unibe.ch/unibe/uniintern/content/e1883/e683686/e695596/e1075264/e1075265/03_UniBE_ManualGestaltungselemente_202112_ger.pdf
#' @export
#' @rdname unibecols
unibeRed <- function() "#e4003c"
#' 50% UnibeRed
#' @export
#' @rdname unibecols
unibeRed50 <- function() "#f39b95"
#' UnibeRed colour scale (2 colours)
#' @export
#' @rdname unibecols
unibeRedS <- function() c(unibeRed(), unibeRed50())

#' Black
#' @export
#' @rdname unibecols
unibeBlack <- function() "#000000"
#' Scale from black to grey  (6 colours)
#' @export
#' @rdname unibecols
unibeBlackS <- function() c(unibeBlack(), "#575756", "#878787", "#b3b2b2", "#dadada", "#ededed")
#' UNIBE grey
#' @export
#' @rdname unibecols
unibeGrey <- function() "#7a7c81"

#' UNIBE green
#' @export
#' @rdname unibecols
unibeGreen <- function() "#466553"
#' UNIBE green scale (5 colours)
#' @export
#' @rdname unibecols
unibeGreenS <- function() c(unibeGreen(), "#668271", "#8aa092", "#afbfb5", "#d6ded9")

#' UNIBE saphire (similar-ish to sky blue)
#' @export
#' @rdname unibecols
unibeSaphire <- function() "#007ea2"
#' UNIBE saphire scale (5 colours)
#' @export
#' @rdname unibecols
unibeSaphireS <- function() c(unibeSaphire(), "#5294b4", "#85adc6", "#b0c7d9", "#d8e2ec")

#' UNIBE ocean (dark blue)
#' @export
#' @rdname unibecols
unibeOcean <- function() "#203a5d"
#' UNIBE ocean scale (5 colours)
#' @export
#' @rdname unibecols
unibeOceanS <- function() c(unibeOcean(), "#4a5575", "#757792", "#a1a0b4", "#d0ced9")

#' UNIBE violet (a sort of burgundy colour)
#' @export
#' @rdname unibecols
unibeViolet <- function() "#8a1e22"
#' UNIBE violet scale (5 colours)
#' @export
#' @rdname unibecols
unibeVioletS <- function() c(unibeViolet(), "#a14540", "#b86f65", "#d19d93", "#e8cdc6")

#' UNIBE brown
#' @export
#' @rdname unibecols
unibeBrown <- function() "#5a3217"
#' UNIBE brown scale (5 colours)
#' @export
#' @rdname unibecols
unibeBrownS <- function() c(unibeBrown(), "#754e31", "#927157", "#b49b87", "#d7cac0")

#' UNIBE mint (cyan-ish)
#' @export
#' @rdname unibecols
unibeMint <- function() "#36b5b6"
#' UNIBE mint scale (5 colours)
#' @export
#' @rdname unibecols
unibeMintS <- function() c(unibeMint(), "#75c4c5", "#a0d3d4", "#c4e3e3", "#e2f1f2")

#' UNIBE paster (pink)
#' @export
#' @rdname unibecols
unibePastel <- function() "#ec627d"
#' UNIBE paster scale (5 colours)
#' @export
#' @rdname unibecols
unibePastelS <- function() c(unibePastel(), "#f08797", "#f4a9b1", "#f8c8cc", "#fce4e7")

#' UNIBE ice (purplish blue)
#' @export
#' @rdname unibecols
unibeIce <- function() "#4767af"
#' UNIBE ice scale (5 colours)
#' @export
#' @rdname unibecols
unibeIceS <- function() c(unibeIce(), "#6e82c0", "#949fd1", "#b9bee1", "#dcdef1")

#' UNIBE mustard
#' @export
#' @rdname unibecols
unibeMustard <- function() "#c2b600"
#' UNIBE mustard scale (5 colours)
#' @export
#' @rdname unibecols
unibeMustardS <- function() c(unibeMustard(), "#cfc43c", "#dcd274", "#e8e1a4", "#f4f0d3")

#' UNIBE apricot
#' @export
#' @rdname unibecols
unibeApricot <- function() "#ee7402"
#' UNIBE apricot scale (5 colours)
#' @export
#' @rdname unibecols
unibeApricotS <- function() c(unibeApricot(), "#f3923e", "#f7af70", "#fbcba1", "#fde6d1")

#' all UNIBE scales
#' @export
#' @rdname unibecols
#' @param plot plot the colours. if FALSE, returns a dataframe of the colours
#' @importFrom ggplot2 ggplot geom_tile scale_fill_identity theme_bw theme aes element_blank
#' @importFrom forcats fct_rev
#' @examples
#' unibeRed()
#' unibeRedS()
#' unibeGreenS()
#' unibePalettes()
unibePalettes <- function(plot = TRUE){
  x <- palette <- NULL

  cols <- c("unibeRedS", "unibeBlackS", "unibeGreenS", "unibeSaphireS", "unibeOceanS",
    "unibeVioletS", "unibeBrownS", "unibeMintS", "unibePastelS", "unibeIceS",
    "unibeMustardS", "unibeApricotS")

  dat <- do.call(rbind, lapply(cols,
    function(name){
      fn <- get(name)
      # print(name)
      values <- fn()
      data.frame(palette = name,
                 x = 1:length(values),
                 cols = values)
    }))

  dat$palette <- factor(dat$palette, cols)

  if(plot){
    ggplot(dat, aes(x = x, y = fct_rev(palette), fill = cols)) +
      geom_tile() +
      scale_fill_identity() +
      theme_bw() +
      theme(axis.title = element_blank())
  } else {
    dat
  }

}

#' list of all UNIBE scales
#' @export
#' @rdname unibecols
unibeScales <- function() c("unibeRedS", "unibeBlackS", "unibeGreenS", "unibeSaphireS", "unibeOceanS",
                            "unibeVioletS", "unibeBrownS", "unibeMintS", "unibePastelS", "unibeIceS",
                            "unibeMustardS", "unibeApricotS")

