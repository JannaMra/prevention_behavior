################################################################################
# Project: Anticipatory saccades for prevention actions
# - do the layout of the plots
# - PLEASE NOTE THAT IN THE VERSIONS WE ARE USING, THE SEGOE UI FONT GENERATES
# - SOME BUGS. YOU MIGHT NOT BE ABLE TO REPRODUCE THE FONT POSITIONS EXACTLY!
# #> In grid.Call(C_textBounds, as.graphicsAnnot(x$label),  ... :
# #> font family 'Segoe UI' not found in PostScript font database
# - (the actual statistical visualization is unaffected, of course)


#---------------------------------------------------------
# SET ENVIRONMENT
rm(list=ls()) 
options(digits = 7)
library(tidyverse)
# extrafont::font_import()   # <- necessary only once
extrafont::loadfonts(device = "win")


#---------------------------------------------------------
# SELECT THE EXPERIMENT TO ANALYZE HERE  <-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-<-
exps = c("exp_1", "exp_2")
exp = exps[2]

if (exp == "exp_1"){
  load("e1_panel_A.RData")
  load("e1_panel_B.RData")
  png_name <- "e1_figure_main_text.png"
} else if (exp == "exp_2") {
  load("e2_panel_A.RData")
  load("e2_panel_B.RData")
  png_name <- "e2_figure_main_text.png"
}



#---------------------------------------------------------
# DO THE GENERAL LAYOUT
panel_A <- panel_A + theme(text = element_text(family = "Segoe UI"))
panel_B <- panel_B + theme(text = element_text(family = "Segoe UI"))

legend <- cowplot::get_legend(panel_A)

panel_A <- panel_A + theme(legend.position = "none")
panel_B <- panel_B + theme(legend.position = "none")


void_plot <- ggplot() + theme_void()

merged_plot <- cowplot::plot_grid(
  void_plot, void_plot, void_plot,
  panel_A, void_plot, panel_B,
  nrow = 2, ncol = 3,
  rel_widths = c(1, 0.075, 1), 
  rel_heights = c(0.075, 1),
  labels = c("(A)", "", "(B)", "", "", ""),
  label_fontfamily = "Segoe UI",
  label_size = 20,
  hjust = 0,
  vjust = 1.25
)



#---------------------------------------------------------
# SAVE TO HARD DRIVE
png(
  png_name,
  width = 2600,
  height = 1300,
  res = 300
)
cowplot::ggdraw(merged_plot) + cowplot::draw_plot(legend, x = 0.67, y = 0.835, width = 0.5, height = 0.1)
dev.off()
