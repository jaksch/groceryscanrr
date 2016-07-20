
setwd('C:\\R\\github\\groceryscanrr')
load('.\\db_data\\grocery_data.RData')
head(grocery_data)
nrow(grocery_data)

library('dplyr')
library('ggplot2')
library('scales')

data <- grocery_data %>% 
  select(-c(id, timestamp))

# tops <- count(plot_data2, parent, wt = value) %>%
#   arrange(desc(n))

tops <- data %>% 
  count(categorie, wt = amount)

crp.rg <- colorRampPalette(c("red","yellow","cyan","blue","red"))
parent_cols <- crp.rg(nrow(tops))
names(parent_cols) <- tops$categorie

# plot_data2 <- arrange(ungroup(mutate(group_by(plot_data2, parent), rank=rank(value))), -rank)

plot_data <- data %>% 
  group_by(categorie) %>% 
  mutate(rank = rank(amount)) %>% 
  ungroup() %>% 
  arrange(desc(rank)) %>% 
  as.data.frame()

# plot_data2 <- mutate(group_by(plot_data2, parent),
#                      color=alpha(parent_cols[parent[1]], seq(1, 0.3, length.out=n())))

plot_data2 <- plot_data %>% 
  group_by(categorie) %>% 
  mutate(color = alpha(parent_cols[categorie[1]], seq(1, 0.3, length.out = n())))

plot_data2$categorie <- factor(plot_data2$categorie, levels = arrange(tops, n)$categorie)

# top_f <- slice(group_by(plot_data2, parent), 1) %>%
#   ungroup() %>%
#   mutate(color = ifelse(parent %in% c('Nyheter', 'Sport', 'Rampelys', 'E24'),
#                         '#2b2b2b', NA))

top_f <- plot_data2 %>% 
  group_by(categorie) %>% 
  slice(1) %>% 
  mutate(color = '#2b2b2b')

gg <- ggplot()
gg <- gg + geom_bar(data = plot_data2, stat="identity",
                    aes(x = categorie, y = amount, fill = color, order = rank),
                    color = "white", size = 0.15, width = 0.65)
gg <- gg + geom_text(data = tops,
                     aes(x = categorie, y = n, label = n),
                     hjust = -0.2, size = 3)
gg <- gg + geom_text(data = top_f,
                     aes(x = categorie, y = amount / 2, label = categorie2, color = color),
                     hjust = 0.5, size = 3)
gg <- gg + scale_x_discrete(expand = c(0,0))
gg <- gg + scale_y_continuous(expand = c(0,0), limits = c(0, 40))
gg <- gg + scale_color_identity()
gg <- gg + scale_fill_identity()
gg <- gg + coord_flip()
gg <- gg + labs(title = "TBD")

gg <- gg + theme_bw() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks.y = element_blank()) +
  geom_vline(xintercept = 0, size = 0.4, color = "black")

gg