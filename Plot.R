p1 <- ggplot(df1 , aes(fitted,predict,  color = dummy))+
  geom_point(shape = 21, fill = "lightgray", size = 3.5)+
  geom_smooth(method = "lm", formula = y~x, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  annotate(geom = "text", x = 15, y = 100, label = "R² = 0.80")+
  #geom_text()+
  theme(panel.grid.major = element_line(colour = "black"),
        panel.border = element_rect(linetype = 1, fill = NA),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.95, .25),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        text = element_text(size = 12)
        #axis.text.x=element_blank(),
        #axis.title.x=element_blank()
        )+
  scale_fill_manual(values = c('#757474'))+
  scale_colour_manual(values = c('#757474'))+
  expand_limits(x=c(10,50),y=c(10,50))+
  scale_x_continuous(breaks = seq(0,120,30), limits = c(0,120))+
  scale_y_continuous(breaks = seq(0,120,30), limits = c(0,120))+
  xlab("Effective productivity predicted , m³/h")+
  ylab("Effective productivity observed, m³/h")+
  theme(legend.position = "none")

p1

p6 <- ggplot(db_ , aes(fitted, observed,  color = dummy, label = id))+
  geom_point(shape = 21, fill = "lightgray", size = 3.5)+
  geom_smooth(method = "lm", formula = y~x, se = F)+
  geom_abline(intercept = 0, slope = 1)+
  #geom_text()+
  theme(panel.grid.major = element_line(colour = "black"),
        panel.border = element_rect(linetype = 1, fill = NA),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.95, .25),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        text = element_text(size = 12))+
  scale_fill_manual(values = c('#757474'))+
  scale_colour_manual(values = c('#757474'))+
  annotate(geom = "text", x = 15, y = 100, label = "R² = 0.84")+
  expand_limits(x=c(10,50),y=c(10,50))+
  scale_x_continuous(breaks = seq(0,120,30), limits = c(0,120))+
  scale_y_continuous(breaks = seq(0,120,30), limits = c(0,120))+
  xlab("Effective productivity predicted , m³/h")+
  ylab("Effective productivity observed, m³/h")+
  theme(legend.position = "none")


p6

p12 <- ggarrange(p1,p6,
                 labels = c("A", "B"),
                 ncol = 2)
p12
#############################################################
df1 <- data.frame(exp(fitted(modmm)), data$PEF, data$ID)
df1$res <- df1[,1] - df1[,2] 

df1$dummy <- "dummy"
#res <- residuals(modmm, type  = 'response')

p2 <- ggplot(df1, aes(res))+
  xlab("Residuals")+
  ylab("Frecuency of residuals")+
  theme(panel.grid.major = element_line(colour = "black"),
        panel.border = element_rect(linetype = 1, fill = NA),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.95, .25),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        text = element_text(size = 12))+
  scale_fill_manual(values = c('#757474'))+
  scale_colour_manual(values = c('#757474'))+
  scale_x_continuous(breaks = seq(-30,30,10), limits = c(-30,30))+
  scale_y_continuous(breaks = seq(0,80,20), limits = c(0,80))+
  theme(legend.position = "none")+
  geom_histogram(bins = 25  )
p2

db_$residuals <- db_$observed - db_$fitted 

p4 <- ggplot(db_, aes(residuals))+
  xlab("Residuals")+
  ylab("Frecuency of residuals")+
  theme(panel.grid.major = element_line(colour = "black"),
        panel.border = element_rect(linetype = 1, fill = NA),
        panel.background = element_rect(fill = "white"),
        legend.position = c(.95, .25),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        text = element_text(size = 12))+
  scale_fill_manual(values = c('#757474'))+
  scale_colour_manual(values = c('#757474'))+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = seq(-30,30,10), limits = c(-30,30))+
  scale_y_continuous(breaks = seq(0,80,20), limits = c(0,80))+
  geom_histogram(bins = 30)

p4

p24 <- ggarrange(p2,p4,
                 labels = c("A", "B"),
                 ncol = 2)
p24

