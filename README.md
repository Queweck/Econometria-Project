# Wpływ wieku pierwszego ślubu na dochód w późniejszym życiu na podstawie danych z GGS z 2004 roku w Polsce

## Opis projektu

Projekt analizuje determinanty dochodu (logarytmu miesięcznych zarobków) w Polsce 
na podstawie danych z badania:
- Generations and Gender Survey (GGS) z 2004 roku

Celem badania jest sprawdzenie, czy:
- Wiek zawarcia związku małżeńskiego
+ przy kontroli istotnych zmiennych dotyczących cech charakteru

wpływają na poziom dochodu osób w ich późniejszym życiu.

---

## Metodologia

W analizie zastosowano:

- klasyczny model regresji liniowej (OLS)

Elementy weryfikacji poprawności modelu:
- test White’a i Breuscha–Pagana (heteroskedastyczność)
- test RESET (specyfikacja modelu)
- współczynnik VIF (współliniowość)
- estymację z macierzą wariancji odpornej (HC3)

Zmienna zależna:
- `logear` – logarytm miesięcznego dochodu

## Omówienie wyników

Najważniejsze wnioski:
- wiek ślubu nie ma wpływu na dochód jednostki w późniejszym życiu
- istotny wpływ na dochód jednostki ma wiek narodzin pierwszego dziecka
