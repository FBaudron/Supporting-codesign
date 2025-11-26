
# GENERATING QR CODES FOR FARMS/TRIALS AND PLOTS/TREATMENTS---------------------

rm(list = ls())

if (!require('baRcodeR')) install.packages("baRcodeR")

library(baRcodeR)


set.seed(123)

given = c("Tendai","Fungai","Tawanda","Ropafadzo","Tafadzwa","Nyasha","Chiedza", "Anesu","Munyaradzi","Chenai","Kudzai","Tariro","Simba","Rutendo", "Kudzanai","Vimbai","Tanaka","Blessing","Shamiso","Rudo","Kuda", "Tinashe","Chipo","Prosper","Sekai","Loveness","Godfrey","Mercy", "Brenna","Nokutenda","Sibusiso","Vusumuzi","Eliazar","Palesa", "Tafara","Mandla","Zvikomborero","Tanyaradzwa")

surname = c("Mupfumi","Chikwepa","Mazarura","Nyamadzawo","Chikora","Makarau", "Gwekwerere","Mandizha","Sibanda","Gonye","Mtsvera","Ndlovu", "Moyo","Ncube","Mapfumo","Chidavaenzi","Mutsvangwa","Dube", "Mashingaidze","Nyamapfene","Gunda","Mungano","Chikowore", "Matambanadzo","Zimuto","Mabhunu","Chinyoka","Chimanikire", "Chiriseri","Gwanyanya","Madziva","Mapiravana","Gwatidzo", "Chikore","Mashingairi","Chirwa","Chinyamurindi")

n = 25

fake_names = paste(sample(given, n, replace = TRUE), sample(surname, n, replace = TRUE))

fake_names


## Generating QR codes for farmers/trials

create_PDF(Labels = fake_names, numcol = 2, numrow = 4, Fsz = 8, name = "farms_qrcodes")


## Generating QR codes for plots/treatments

l = expand.grid(FR = fake_names, TR = c("CONV", "CA", "PPULL"))
l = paste(l$FR, l$TR, sep = "-")

create_PDF(Labels = l, numcol = 2, numrow = 4, Fsz = 8, name = "plots_qrcodes")


# INTEGRATION QR CODES IN KOBOTOOLBOX SURVEYS-----------------------------------

## Data from KoboToolBox surveys that include QR codes

rm(list = ls())

if (!require('openxlsx')) install.packages("openxlsx")
if (!require('tidyr')) install.packages("tidyr")
if (!require('lmerTest')) install.packages("lmerTest")
if (!require('see')) install.packages("see")
if (!require('performance')) install.packages("performance")
if (!require('ggeffects')) install.packages("ggeffects")
if (!require('sjPlot')) install.packages("sjPlot")

library(openxlsx)
library(tidyr)
library(lmerTest)
library(see)
library(performance)
library(ggeffects)
library(sjPlot)

data = read.xlsx("EXPERIMENT SURVEY DATA.xlsx", sheet = 1)

sc1 = read.xlsx("EXPERIMENT SURVEY DATA.xlsx", sheet = 2)
sc2 = read.xlsx("EXPERIMENT SURVEY DATA.xlsx", sheet = 3)
sc3 = read.xlsx("EXPERIMENT SURVEY DATA.xlsx", sheet = 4)


data = separate(data = data, col = "Scan.the.QR.code.of.the.1st.plot",
                into = c("Farmer", "1st plot"), sep = "-")

data = data[, c(3, 17, 5:7)]

data = separate(data = data, col = "Scan.the.QR.code.of.the.2nd.plot",
                into = c("Farmer", "2nd plot"), sep = "-")

data = data[, c(1:3, 5:6)]

data = separate(data = data, col = "Scan.the.QR.code.of.the.3rd.plot",
                into = c("Farmer", "3rd plot"), sep = "-")

data = data[, c(1:4, 6)]


data1 = data[, c(1:3)]
data2 = data[, c(1:2, 4)]
data3 = data[, c(1:2, 5)]

sc1 = sc1[, c(11, 1:8)]
sc2 = sc2[, c(11, 1:8)]
sc3 = sc3[, c(11, 1:8)]


data1 = merge(data1, sc1, by.x = "_index", by.y = "_parent_index", all.y = TRUE)
data2 = merge(data2, sc2, by.x = "_index", by.y = "_parent_index", all.y = TRUE)
data3 = merge(data3, sc3, by.x = "_index", by.y = "_parent_index", all.y = TRUE)

names(data1)[3] = "Treatment"
names(data2)[3] = "Treatment"
names(data3)[3] = "Treatment"

data = rbind(data1, data2, data3)

data = data[, -c(1)]

names(data)[c(1, 4:10)] = c("Farmer", "armmoured_cricket_leaf_damage", "armmoured_cricket_damage_score", "fall_armyworm_leaf_damage", "fall_armyworm_frass_whorl", "fall_armyworm_damage_score", "stalk_borer_leaf_damage", "stalk_borer_damage_score")


## Running basic statistical models on the cleaned data

data$Treatment = factor(data$Treatment, levels = c("CONV", "CA", "PPULL"))

mod_faw_severity = glmer(fall_armyworm_damage_score ~ Treatment + Vstage + (1|Farmer), data = data, family = poisson())

check_plot = check_model(mod_faw_severity)

print(check_plot)


anova(mod_faw_severity)
summary(mod_faw_severity)


predict_response(mod_faw_severity, terms = "Treatment")


plot_model(mod_faw_severity, type = "pred", pred.type = 'fe', terms = "Treatment")


