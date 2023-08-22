library(ggplot2); library(dplyr); library(Rmisc)

theme_set(theme_minimal(base_size = 18))
# One continuous IV, one categorical IV, continuous DV
# How long are the odontoblasts (cells responsible for tooth growth) of guinea pigs after receiving
# different doses of vitamin C / orange juice?
ToothGrowth <- ToothGrowth
test <- rbind(ToothGrowth, c(NA, NA, 1.5))

ggplot(test, aes(x = as.factor(dose), y = len, fill = supp)) + 
  geom_dotplot(binaxis = "y", stackdir = "center", position=position_dodge(0.8), color = "none") + 
  scale_fill_manual(values = c("#ffa754", "#545dff")) +
  labs(x = "Dose (mg/day)", y = "Odontoblast Length", fill = "Supplement") +
  theme(legend.position = "bottom") 


summary_TG <- summarySE(data = ToothGrowth, measurevar = "len", groupvars = c("dose", "supp"))
ggplot(summary_TG, aes(dose, len, fill = supp)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) + 
  scale_fill_manual(values = c("#ffa754", "#545dff")) + 
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.45)) + 
  labs(x = "Dose (mg/day)", y = "Odontoblast Length", fill = "Supplement") +
  theme(legend.position = "bottom")

# Two continuous IVs and one continuous DV
# How likely is a batter to miss a ball depending on the speed of the ball and the spin of the ball?
spinrates <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/spinrates.csv",
                      stringsAsFactors = FALSE)
spinrates <- spinrates[,2:4]
ggplot(spinrates, aes(x=velocity, y = spinrate)) +
  geom_tile(aes(fill = swing_miss)) +
  labs(title = "Likelihood of swinging and missing on a fastball",
       x = "Pitch velocity (mph)", y = "Spin rate (rpm)", fill = "Likelihood of swing and miss") + 
  theme(legend.position = "bottom") 


