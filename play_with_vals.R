srhi <- scored[scored$scale_name=="SRHI" & !is.na(scored$score),] %>% select(-n_items) %>% spread("scored_scale","score")

ggplot(srhi,aes(healthy,unhealthy))+geom_point()

View(srhi)