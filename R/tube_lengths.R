# Here I'll take a look at Lian's lap6c tube lengths

library(tidyverse)
library(broom) # For the multi-group t-test

###### Manipulating the data ######

# Importing data
tubes <- read.table(file = './data/larp6c_tube_lengths.tsv', header = T, sep = '\t')

# Making some factors
tubes$rep_number <- as.factor(tubes$rep_number)
tubes$timepoint <- as.factor(tubes$timepoint)
tubes$trial_letter <- as.factor(tubes$trial_letter)

# Converting tube length to µm. This is based on the field scope at 4x, 
# using the micrometer slide (2000 µm was 1142.567 pixels), so 0.571284 pixels/µm.
tubes$tube_length <- tubes$tube_length / 0.571284

# Grouping by rep then letter, then gettting the averages
tubes_summary <- tubes %>% group_by(rep_number, trial_letter) %>%
                           summarize(mean_tube_length = mean(tube_length))

# Adding genotype info
tubes_summary$genotype <- factor(c("WT", "larp6c", "WT", "larp6c", "WT", "larp6c",
                                   "larp6c", "WT", "larp6c", "WT", "larp6c", "WT",
                                   "larp6c", "WT", "larp6c", "WT", "larp6c", "WT",
                                   "WT", "larp6c", "WT", "larp6c", "WT", "larp6c"))

tubes_summary$genotype <- factor(tubes_summary$genotype, levels = c("WT", "larp6c"))

# Making a new df for the final plot
plot_tubes <- tubes
plot_tubes$full_name <- paste(plot_tubes$rep_number, plot_tubes$trial_letter, sep = '-')
genotype_key <- tubes_summary[ , c(1, 2, 4)]
genotype_key$full_name <- paste(genotype_key$rep_number, genotype_key$trial_letter, sep = '-')
genotype_key <- genotype_key[ , c(3, 4)]
plot_tubes <- full_join(plot_tubes, genotype_key)

###### Tube length boxplot ######
color_vector <- c("#2558A8", "#019E73")

# Export 1300x1500
ggplot(tubes_summary, aes(x = genotype, y = mean_tube_length, fill = genotype)) +
  geom_boxplot(size = 2) +
  scale_fill_manual(values = color_vector,
                    breaks = c("WT", "larp6c"),
                    labels = c("WT", "larp6c")) +
  scale_x_discrete(labels = c("WT", "larp6c")) +
  labs(title = "Mean tube length", y = "Tube length (µm)") +
  theme_bw() +
  theme(axis.title = element_text(size = 52, face = "bold"),
        axis.text = element_text(size = 56, face = "bold", color = "black"),
        axis.text.x = element_text(margin = margin(5, 0, 0, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        plot.title = element_text(size = 72, face = "bold", margin = margin(0, 0, 40, 0), hjust = 0.5), 
        axis.line = element_line(size = 2, color = "black"),
        axis.ticks = element_line(size = 2, color = "black"),
        axis.ticks.length = unit(10, "pt"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")


# Facet-wrapped by rep (export 1500x1500)
ggplot(tubes_summary, aes(x = genotype, y = mean_tube_length, fill = genotype)) +
  geom_boxplot(size = 2) +
  scale_fill_manual(values = color_vector,
                    breaks = c("WT", "larp6c"),
                    labels = c("WT", "larp6c")) +
  scale_x_discrete(labels = c("WT", "larp6c")) +
  labs(title = "Mean tube length", y = "Tube length (µm)") +
  theme_bw() +
  theme(axis.title = element_text(size = 52, face = "bold"),
        axis.text = element_text(size = 56, face = "bold", color = "black"),
        axis.text.x = element_text(margin = margin(5, 0, 0, 0)),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
        plot.title = element_text(size = 72, face = "bold", margin = margin(0, 0, 40, 0), hjust = 0.5), 
        axis.line = element_line(size = 2, color = "black"),
        axis.ticks = element_line(size = 2, color = "black"),
        axis.ticks.length = unit(10, "pt"),
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 56, face = "bold"),
        panel.spacing = unit(4, "lines")) +
  facet_wrap(~rep_number)


###### New paper plot ######
plot_tubes$genotype <- factor(plot_tubes$genotype, levels = c("WT", "larp6c"))
ggplot(plot_tubes, aes(x = genotype, y = tube_length, fill = genotype)) +
  geom_boxplot(size = 1, outlier.size = 1, position = position_dodge(0.85), color = 'black') +
  scale_fill_manual(values = c('white', 'white')) +
  scale_x_discrete(labels = c("WT", "larp6c")) +
  labs(title = "larp6c tube lengths", 
       y = "Pollen tube length (µm)") +
  scale_y_continuous(breaks = seq(0, 320, by = 40),
                     labels = seq(0, 320, by = 40),
                     limits = c(0, 320),
                     expand = c(0, 0)) +
  theme_bw() +
  theme(axis.title = element_text(size = 26, face = 'bold'),
        axis.text = element_text(size = 22, face = 'bold', color = 'black'),
        axis.text.x = element_text(size = 27, face = 'bold', color = 'black', 
                                   margin = margin(10, 0, 0, 0)),
        # plot.title = element_text(size = 32, face = 'bold', margin = margin(0, 0, 10, 0)),
        plot.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(0, 10, 0, 0)),
        panel.border = element_blank(),
        axis.line = element_line(size = 1, color = 'black'),
        axis.ticks = element_line(size = 1, color = 'black'),
        axis.ticks.length = unit(8, 'pt'),
        plot.margin = margin(0.5, 0.5, 0.5, 0.5, 'cm'),
        panel.grid = element_blank(),
        legend.position = 'none')

ggsave(filename = './plots/tube_lengths.png',
       device = 'png',
       width = 4,
       height = 7.5,
       dpi = 400,
       units = 'in')


###### Stats ######
### Getting the percent reduction for the mean of everything
total_tubes <- tubes_summary %>% ungroup() %>%
                                 group_by(genotype) %>%
                                 summarize(mean_tube_length = mean(mean_tube_length))

# Percent reduction = 31%
1 - (total_tubes$mean_tube_length[total_tubes$genotype == "larp6c"] / total_tubes$mean_tube_length[total_tubes$genotype == "WT"])

### Getting the percent for the individual reps
rep_percent <- tubes_summary %>% 
               ungroup %>%
               group_by(rep_number, genotype) %>%
               summarize(rep_mean = mean(mean_tube_length)) %>%
               ungroup %>%
               group_by(rep_number) %>%
               summarize(percent = 100 * (1 - (rep_mean[genotype == "larp6c"] / rep_mean[genotype == "WT"])))

### t-test for everything
# Checking for equal variance (p-value = 0.1175, fail to reject null hypothesis that the variances are the same)
var.test(tubes_summary$mean_tube_length[tubes_summary$genotype == "larp6c"],
         tubes_summary$mean_tube_length[tubes_summary$genotype == "WT"])

# Running the t-test (different lengths, p-value = 3.697e-06)
t.test(tubes_summary$mean_tube_length ~ tubes_summary$genotype)

### t-test for each rep
t.test(plot_tubes[plot_tubes$rep_number == 1, ]$tube_length ~ plot_tubes[plot_tubes$rep_number == 1, ]$genotype)
t.test(plot_tubes[plot_tubes$rep_number == 2, ]$tube_length ~ plot_tubes[plot_tubes$rep_number == 2, ]$genotype)
t.test(plot_tubes[plot_tubes$rep_number == 3, ]$tube_length ~ plot_tubes[plot_tubes$rep_number == 3, ]$genotype)
t.test(plot_tubes[plot_tubes$rep_number == 4, ]$tube_length ~ plot_tubes[plot_tubes$rep_number == 4, ]$genotype)
