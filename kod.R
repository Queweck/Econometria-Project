################################################################################
#                               0. PAKIETY                                     #
################################################################################

# Instalacja wymaganych pakietów (jednorazowo)
install.packages("readstata13")
install.packages("dplyr")
install.packages("tseries")
install.packages("lmtest")
install.packages("corrplot")
install.packages("car")
install.packages("strucchange")
install.packages("stargazer")
install.packages("rstudioapi")
install.packages("flextable")
install.packages("skedastic")

# Ładowanie bibliotek
library(skedastic)
library(stargazer)
library(rstudioapi)
library(strucchange)
library(car)
library(corrplot)
library(ggplot2)
library(lmtest)
library(sandwich)
library(readstata13)
library(dplyr)
library(tseries)
library(flextable)
library(modelsummary)


################################################################################
#                           1. PRZYGOTOWANIE DANYCH                            #
################################################################################

# Wczytanie danych z plików .dta
wave1_PL <- readstata13::read.dta13("GGS_Poland.dta")
earnings <- readstata13::read.dta13("Earnings.dta")

# Wybór zmiennych z pierwszej fali badania
wave1dane_PL <- wave1_PL %>%
  select("arid", "asex", "abyear","aage", "aeduc","a149","a150AgeR", 
         "aactstat","aparstat", "amarstat","ankids", "aplace",
         "a203a","a308","a301AgeR","a302bAgeR","numdivorce",
         "a401a_a","a5113","a5116AgeR",
         "a550", "a701","a845","a839","a855_a","a871AgeR","a1102mnth",
         "a1107_a","a1107_b","a1107_c","a1107_i",
         "a1113_a","a1113_f",
         "a720_a","a720_f","a720_d","a720_c","a1113_b",
         "a1109_1","a1109_2","a1109_3",
         "a253y_1","ageyoungest","coreschild")

# Korekta identyfikatora (dopasowanie do zbioru earnings)
wave1dane_PL$id <- wave1dane_PL$arid + 0.26

# Łączenie zbiorów
DANE_PL_ER <- inner_join(wave1dane_PL, earnings, by = c("id" = "id"))

# Usunięcie braków w zmiennej dochodu
DANE_PL <- DANE_PL_ER %>%
  filter(!is.na(`_4_log_earnings_imputed`))


################################################################################
#                   2. WYBÓR I PRZEKSZTAŁCENIE ZMIENNYCH                       #
################################################################################

DANE_PL <- DANE_PL %>%
  select("arid", "asex", "abyear","aage", "aeduc","a149","a150AgeR",
         "aactstat","aparstat", "amarstat","ankids", "aplace",
         "a203a","a308","a301AgeR","a302bAgeR","numdivorce",
         "a401a_a","a5113","a5116AgeR",
         "a550", "a701","a845","a839","a855_a","a871AgeR","a1102mnth",
         "a1107_a","a1107_b","a1107_c","a1107_i",
         "a1113_a","a1113_f",
         "a720_a","a720_f","a720_d","a720_c","a1113_b",
         "a1109_1","a1109_2","a1109_3",
         "a253y_1","ageyoungest","coreschild",
         "_4_log_earnings_imputed","earnings_local")

# Zmiana nazw zmiennych
colnames(DANE_PL) <- c("id","sex","byear","age","educ","studies","ageeduc",
                       "actstat","paractstat","marstat","nkids","place",
                       "kidshelp","pareduc","parlivage","ageatmar","ndiv",
                       "housework","fatheduc","ageliveown",
                       "pardiv","health","jobstat","jobsat","jobtired",
                       "ageexper","rel",
                       "vmarout","vlivwmar","vmarfor","vkidlivown",
                       "vmanold","vkidwomwork",
                       "peoplehelp","peopleclose","poplefriends","peoplemiss",
                       "vworkwom","jobimp1","jobimp2","jobimp3",
                       "ybfchild","ageyoung","childlive",
                       "logear","ear")

# Ograniczenie próby do wieku 35–50 lat
DANE_PL_3550 <- DANE_PL %>%
  filter(DANE_PL$age >= 35)

DANE_PL_3550 <- DANE_PL_3550 %>%
  filter(DANE_PL_3550$age <= 50)

# Tworzenie zmiennych pomocniczych
DANE_PL_3550$expear <- exp(DANE_PL_3550$logear)

DANE_PL_3550$educ_high <- ifelse(
  DANE_PL_3550$educ %in% c("isced 6 - second stage of teritiary",
                           "isced 5 - first stage of tertiary"), 1, 0)

DANE_PL_3550$educ_low <- ifelse(
  DANE_PL_3550$educ %in% c("isced 4 - post secondary non-tertiary",
                           "isced 3 - upper secondary level",
                           "isced 2 - lower secondary level"), 1, 0)

DANE_PL_3550$fatheduc <- ifelse(
  DANE_PL_3550$fatheduc %in% c("isced 6 - second stage of teritiary",
                               "isced 5 - first stage of tertiary"), 1, 0)

DANE_PL_3550$ifkids <- ifelse(DANE_PL_3550$nkids == 0, 0, 1)

DANE_PL_3550$agebfchild <- DANE_PL_3550$ybfchild - DANE_PL_3550$byear
DANE_PL_3550$marstat <- ifelse(DANE_PL_3550$marstat == "married", 1, 0)
DANE_PL_3550$paractstat <- ifelse(DANE_PL_3550$paractstat == "co-resident partner", 1, 0)
DANE_PL_3550$female <- ifelse(DANE_PL_3550$sex == "female", 1, 0)

# Usunięcie osób po rozwodzie
DANE_PL_3550 <- DANE_PL_3550 %>%
  filter(ndiv == 0)


################################################################################
#                   3. OSTATECZNY ZBIÓR ANALITYCZNY                            #
################################################################################

DANE_3550 <- DANE_PL_3550 %>%
  select("id","female","educ_high","educ_low","ageeduc",
         "paractstat","marstat","ifkids","place","parlivage","ageatmar",
         "ndiv","fatheduc","ageliveown",
         "agebfchild","logear","expear")

# Rekodowanie miejsca zamieszkania
DANE_3550 <- DANE_3550 %>%
  mutate(place = case_when(
    place == "city with 500 thousands inhabitants and more" ~ "6_city (500<)",
    place == "city with 200 to 499 thousands inhabitants" ~ "5_city (200-499)",
    place == "city with 100 - 199 thousands inhabitants and more" ~ "4_city (100-199)",
    place == "city with 20 - 99 thousands inhabitants and more" ~ "3_city (20-99)",
    place == "city with less than 20 thousands inhabitants" ~ "2_city (<20)",
    place == "village" ~ "1_village (not city)"
  ))

# Czyszczenie braków logicznych
DANE_3550$parlivage[DANE_3550$paractstat == 0 & is.na(DANE_3550$parlivage)] <- 0
DANE_3550$ageatmar[DANE_3550$marstat == 0 & is.na(DANE_3550$ageatmar)] <- 0
DANE_3550$ageatmar[DANE_3550$paractstat == 0 & is.na(DANE_3550$ageatmar)] <- 0
DANE_3550$agebfchild[DANE_3550$ifkids == 0 & is.na(DANE_3550$agebfchild)] <- 0

DANE_3550 <- na.omit(DANE_3550)

colSums(is.na(DANE_3550))

# Konstrukcja dodatkowych zmiennych
DANE_3550$cohab_gap <- DANE_3550$ageatmar - DANE_3550$parlivage
DANE_3550$cohab_gap[DANE_3550$ageatmar == 0 & is.na(DANE_3550$cohab_gap)] <- 0
DANE_3550$cohab_gap[DANE_3550$parlivage == 0 & is.na(DANE_3550$cohab_gap)] <- 0

DANE_3550$educ_grup <- 2*DANE_3550$educ_high + DANE_3550$educ_low

################################################################################
#                           5. ANALIZA WSTĘPNA                                 #
################################################################################

# Boxplot: wykształcenie a log(dochód)
boxplot(logear ~ educ_grup, data = DANE_3550,
        xlab = "Wykształcenie (2-wyższe, 1-średnie, 0-podstawowe i brak)",
        ylab = "log(dochód)",
        main = "Wykształcenie, a log(dochód)")


################################################################################
#                    6. TWORZENIE PODZBIORÓW DO WYKRESÓW                      #
################################################################################

DANE_3550_0 <- DANE_3550 %>%
  filter(ageatmar != 0)

DANE_3550_kids <- DANE_3550 %>%
  filter(ifkids != 0)

DANE_3550_0_0 <- DANE_3550_0 %>%
  filter(parlivage != 0)

DANE_3550_0_0_0 <- DANE_3550_0_0 %>%
  filter(ifkids != 0)

DANE_3550_0_0_0_0 <- DANE_3550_0_0_0 %>%
  filter(ageliveown != 0)


################################################################################
#                    7. ANALIZA ZMIENNEJ OBJAŚNIANEJ                           #
################################################################################

# Test normalności Jarque-Bera
jarque.bera.test(DANE_3550$logear)

par(mfrow = c(1,2))

# Histogram wynagrodzenia
hist(DANE_3550$expear,
     main="Histogram miesięcznego wynagodzenia",
     xlab="Miesięczne wynagrodzenie",
     ylab="Częstość",
     probability = TRUE)

x <- seq(min(DANE_3550$expear), max(DANE_3550$expear), length.out = 1000)
lines(x,
      dnorm(x,
            mean = mean(DANE_3550$expear),
            sd = sd(DANE_3550$expear)),
      col = "blue",
      lwd = 2)

# Histogram log(dochodu)
hist(DANE_3550$logear,
     main="Histogram log(miesięcznego wynagodzenia)",
     xlab="log(miesięczne wynagrodzenie)",
     ylab="Częstość",
     probability=TRUE)

x_log <- seq(min(DANE_3550$logear), max(DANE_3550$logear), length.out = 1000)
lines(x_log,
      dnorm(x_log,
            mean = mean(DANE_3550$logear),
            sd = sd(DANE_3550$logear)),
      col = "blue",
      lwd = 2)

par(mfrow = c(1,1))


################################################################################
#                       8. MACIERZ KORELACJI                                   #
################################################################################

vars <- DANE_3550_0_0_0[,c("logear","ageatmar","ageliveown",
                           "agebfchild","cohab_gap","ageeduc","parlivage")]

cor_mat <- cor(vars, use = "complete.obs", method = "pearson")

corrplot(cor_mat,
         method = "color",
         type = "upper",
         order = "hclust",
         addCoef.col = "black",
         tl.col = "black",
         tl.srt = 45,
         col = colorRampPalette(c("blue", "white", "red"))(200),
         number.cex = 0.8)


################################################################################
#                           9. WYKRESY ZALEŻNOŚCI                              #
################################################################################

par(mfrow = c(1, 2))

ggplot(DANE_3550_0, aes(x=ageatmar, y=logear)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm",
              formula = y ~ x + I(x^2),
              se = FALSE,
              linewidth = 1.2,
              color = "blue") +
  labs(
    title = "Zależność między wiekiem ślubu, a logarytmem dochodu",
    x = "wiek ślubu",
    y = "log(dochód)"
  )

hist(DANE_3550_0$ageatmar)

par(mfrow = c(1,1))


################################################################################
#                           10. BOXPLOTY                                       #
################################################################################

boxplot(logear ~ place, data = DANE_3550,
        xlab = "miejsce zamieszkania",
        ylab = "log(dochód)",
        main = "Miejsce zamieszkania, a log(dochód)")

boxplot(logear ~ female, data = DANE_3550,
        xlab = "Płeć (1-kobieta)",
        ylab = "log(dochód)",
        main = "Płeć, a log(dochód)")

boxplot(logear ~ marstat, data = DANE_3550,
        xlab = "Status związku (1-małżeństwo)",
        ylab = "log(dochód)",
        main = "Małżeństwo, a log(dochód)")

boxplot(logear ~ fatheduc, data = DANE_3550,
        xlab = "Edukacja ojca (1-wyższe wykształcenie)",
        ylab = "log(dochód)",
        main = "Wyksztłcenie ojca, a log(dochód)")

boxplot(logear ~ educ_grup, data = DANE_3550,
        xlab = "Wykształcenie (2-wyższe, 1-średnie, 0-podstawowe i brak)",
        ylab = "log(dochód)",
        main = "Wykształcenie, a log(dochód)")


################################################################################
#                      11. MODELE EKONOMETRYCZNE                               #
################################################################################

MODEL1 <- lm(logear ~ female + place + educ_high + educ_low + ageeduc +
             fatheduc + ageliveown + ageatmar + agebfchild +
             marstat + cohab_gap,
             data = DANE_3550)
summary(MODEL1)

MODEL2 <- lm(logear ~ female + place + educ_high + educ_low +
             ageeduc + I(ageeduc^2) + fatheduc + ageliveown +
             ageatmar + I(ageatmar^2) +
             female:ageatmar + female:I(ageatmar^2) +
             agebfchild + female:agebfchild +
             I(agebfchild^2) + female:I(agebfchild^2) +
             marstat + cohab_gap,
             data = DANE_3550)
summary(MODEL2)

MODEL3 <- lm(logear ~ female + place + educ_high +
             ageeduc + I(ageeduc^2) + fatheduc + ageliveown +
             agebfchild + female:agebfchild +
             I(agebfchild^2) + female:I(agebfchild^2),
             data = DANE_3550)
summary(MODEL3)


################################################################################
#                            12. TESTY DIAGNOSTYCZNE                           #
################################################################################

resettest(MODEL3, power = 2:3, type = "fitted")
vif(MODEL3)
bptest(MODEL3)
white(MODEL3)


################################################################################
#                            13. TEST CHOWA                                    #
################################################################################

DANE_3550_m <- DANE_3550 %>% filter(female == 0)
DANE_3550_w <- DANE_3550 %>% filter(female == 1)

MODELm <- lm(logear ~ female + place + educ_high +
             ageeduc + I(ageeduc^2) + fatheduc + ageliveown +
             agebfchild + female:agebfchild +
             I(agebfchild^2) + female:I(agebfchild^2),
             data = DANE_3550_m)

MODELw <- lm(logear ~ female + place + educ_high +
             ageeduc + I(ageeduc^2) + fatheduc + ageliveown +
             agebfchild + female:agebfchild +
             I(agebfchild^2) + female:I(agebfchild^2),
             data = DANE_3550_w)


################################################################################
#                        14. OBSERWACJE ODSTAJĄCE                              #
################################################################################

cooks_d <- cooks.distance(MODEL3)
cook_threshold <- 4/nrow(DANE_3550)

plot(MODEL3, which=4, cook.level=cook_threshold)
abline(h=cook_threshold, lty=2, col="red")

plot(MODEL3, which=5)


################################################################################
#                           15. SUMNK I WMNK                                   #
################################################################################

residuals_sq <- residuals(MODEL3)^2

model_var <- lm(log(residuals_sq) ~ educ_high + ageeduc + I(ageeduc^2) +
                ageliveown + agebfchild,
                data = DANE_3550)

wagi_fgls <- 1 / exp(fitted(model_var))

MODEL3_SUMNK <- lm(logear ~ female + place + educ_high +
                   ageeduc + I(ageeduc^2) + fatheduc + ageliveown +
                   agebfchild + female:agebfchild +
                   I(agebfchild^2) + female:I(agebfchild^2),
                   data = DANE_3550,
                   weights = 1/I(wagi_fgls^2))

MODEL3_WMNK <- lm(logear ~ female + place + educ_high +
                  ageeduc + I(ageeduc^2) + fatheduc + ageliveown +
                  agebfchild + female:agebfchild +
                  I(agebfchild^2) + female:I(agebfchild^2),
                  data = DANE_3550,
                  weights = 1 / (ageeduc^2))

summary(MODEL3_WMNK)

bptest(MODEL3_WMNK)
bptest(MODEL3_SUMNK)

################################################################################
#        Tabela porównawcza estymatorów (OLS, WMNK, SUMNK, HC3)                #
################################################################################

ft_2 <- modelsummary(
  list(
    "MODEL 3" = MODEL3,
    "MODEL 3 (WMNK)" = MODEL3_WMNK,
    "MODEL 3 (SUMNK)" = MODEL3_SUMNK,
    "MODEL 3 (macierz odporna)" = MODEL3_res
  ),
  coef_map = var_names,
  stars = TRUE,
  statistic = "std.error",
  gof_map = c("nobs", "r.squared", "adj.r.squared"),
  output = "flextable"
)

ft_2 <- ft_2 %>%
  fontsize(size = 9) %>%
  padding(padding.top = 1, padding.bottom = 1) %>%
  line_spacing(space = 0.9) %>%
  autofit()

ft_2
