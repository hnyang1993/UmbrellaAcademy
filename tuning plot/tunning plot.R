#rsvm = rbind(fit_svmRadial1$results, fit_svmRadial2$results, fit_svmRadial3$results)[, 1:4]
#rsvm = rsvm[rsvm$C %in% seq(10,90,20),]
load("/Users/apple/BIOS735/UmbrellaAcademy/rf_gridsearch2.Rdata")
rf = rf_gridsearch$results
tune.best = rf[order(rf$Kappa, decreasing = T)[1],]
library(ggrepel)
# Kappa plot
ggplot(rf, aes(mtry, Kappa)) + geom_point() + geom_line() + 
  geom_point(data = tune.best, col="black", size=3.5, stroke = 0.8, shape=21) + 
  scale_x_continuous(breaks = c(1,2,4,8,16,32,64)) +
  geom_label_repel(data = tune.best, label = "mtry = 16", color = 'white', 
                   fill = hue_pal()(5)[5], segment.color="black", nudge_x = 3, size = 4)
# accuracy plot
ggplot(rf, aes(mtry, Accuracy)) + geom_point() + geom_line() + 
  geom_point(data = tune.best, col="black", size=3.5, stroke = 0.8, shape=21) + 
  scale_x_continuous(breaks = c(1,2,4,8,16,32,64)) + 
  geom_label_repel(data = tune.best, label = "mtry=16", color = 'white', 
                   fill = hue_pal()(5)[5], segment.color="black", nudge_x = 3, size = 4)

