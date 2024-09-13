library(tidyverse)

code_tally = read_table("data/code_tally.txt", col_names = F) %>% 
  rename(fun = X1, n = X2) %>% 
  group_by(fun) %>% 
  reframe(n = sum(n)) %>% 
  arrange(-n)

write_csv(code_tally %>% filter(n >= 10) %>% mutate("What does this function do?" = NA), file = "data/code_tally_list.csv")

code_tally_plot = code_tally %>% 
  filter(n >= 5) %>% 
  ggplot(aes(x = n, y = reorder(fun, n))) + 
  geom_bar(stat = "identity", width = 0.1) +
  geom_point() +
  labs(y = "Function", 
       x = "# times used in the book",
       subtitle = "R functions used more than 5 times in Statistical Rethinking") +
  theme_bw()

ggsave(code_tally_plot, file = "plots/code_tally_plot.jpg", width = 6.5, height= 5)
