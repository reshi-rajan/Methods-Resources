library(dagitty)
library(ggdag)
library(tidyverse)


ggdag_classic(gnarly_dag, size = 2.50) +  
  theme_dag() 

newdag <- dagify(m ~ x + y,
            n ~ x + y,
            z ~ x,
            w ~ x,
            q ~ x + w,
            t ~ x + w)

newdag %>%
  tidy_dagitty() %>%
  dag_label(labels = c(
    "x" = "Old Fashioned Racism",
    'y' = "Obama's Election",
    "n" = "Democratic Support",
    "m" = "Republican Support", 
    'z' = 'Oppose Obama',
    'w' = 'Assessment of Obama',
    'q' = 'Party ID',
    't' = 'Midterm Vote')) %>%
  mutate(linetype = ifelse(name == 'y', "dashed", "solid")) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_linetype = linetype), show.legend = FALSE) +
  geom_dag_point() + 
  geom_dag_text(color = 'black') +
  geom_dag_label_repel(aes(label = label, fill = label),
                       col = "white", show.legend = F)+ 
  theme_dag()


