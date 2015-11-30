# based on example found at http://www.r-bloggers.com/r-good-practice-–-adding-footnotes-to-graphics/

makeFootnote <- function(footnoteText= format(Sys.time(), "%d %b %Y"),
                         base.size = 8,
                         color= grey(.5))
{
  require(grid)
  pushViewport(viewport())
  grid.text(label = footnoteText,
            x = unit(1, "npc") - unit(2, "mm"),
            y = unit(2, "mm"),
            just = c("right", "bottom"),
            gp = gpar(fontsize = base.size*0.7,
                      col = color))
  popViewport()
}