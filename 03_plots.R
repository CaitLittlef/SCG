display.brewer.pal(8, "Dark2")
palette <- brewer.pal(8, "Dark2")



# Gather data, remove mtC and removal vals.
colnames(scg_summ)
boo <- scg_summ %>%
  dplyr::select(-scg_ann_all_mtC, -scg_ann_ag_bg_mtC,
                -scg_10yr_rem_mtC,
                -scg_ann_all_mtC_rem, -scg_ann_ag_bg_mtC_rem,
                -scg_10yr_rem_mtCO2e) %>%
  gather(key = "cat", value = "mtCO2e", -year)
# Make sure factors are in proper order
boo$cat <- factor(boo$cat,
                  levels = c("scg_ann_all_mtCO2e",
                             "scg_ann_all_mtCO2e_rem",
                             "scg_ann_ag_bg_mtCO2e",
                             "scg_ann_ag_bg_mtCO2e_rem"))
levels(boo$cat)



## Plot 
g <- ggplot() +
  geom_line(data = boo, #boo[boo$year<2050,],
            aes(x = year, y = mtCO2e, color = cat, linetype = cat),
            cex = 1.5) +
  labs(title = "Second College Grant metric tonnes CO2e",
       x = NULL,
       y = "mt CO2e") + 
  scale_x_continuous(breaks = seq(2015, 2120, 5)) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE))+
                     # breaks = seq(0, 6100000, 2500)) +
  scale_color_manual(values = palette[c(1,5,7,6)],
                     labels = c("All pools",
                                "All pools incl. 3000 cord removal",
                                "AG & BG live & dead trees",
                                "AG & BG live & dead trees\nincl. 3000 cord removal")) +
  scale_linetype_manual(values=c("solid", "dashed", "solid", "dashed"), guide = "none") +
  # guides(color = guide_legend(reverse = TRUE), linetype = "none") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5, size = 12), # centers title
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(size = 0.25),
        axis.text.x=element_text(angle=45, hjust= 1),
        legend.justification=c(1,0), 
        legend.position = c(0.99, 0.01),
        legend.title = element_blank(),
        legend.box = "horizontal",
        legend.text=element_text(size=10), #angle = 90),
        legend.background = element_rect(color = "transparent", fill = "white")) #+
# annotate("text", x = 2018, y = 200000, label = "imports not\nincluded", size = 4)
g

dev.off()


## Save as pdf by version
# v <- 1
pdf(paste0(out.dir, "SCG_mtCO2e_2018-2108_v",v,"_", currentDate, ".pdf"),
    width = 6, height = 4) ; v <- v+1
g
dev.off()

