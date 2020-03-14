# Warsztaty Badawcze 2

To jest oficjalne repo dla przedmiotu "Warsztaty Badawcze 2".

# Nota na temat datasetu i zadania

Nasz dataset to breast cancer. Zawiera szereg zmiennych dyskretnych, głównie kategorycznych.

Zadanie polega na przewidzeniu wystąpienia raka piersi.

## TODO

Przetestować:
- gbm
- bart

Porównać z modelem drzewiastym. -bj

Porównać z modelem trenowanym metodą boxa-coxa.

Zwizualizować dane i spróbować dodać nowe zmienne tj. zbudować parametryczny model predykcyjny (uogólniony liniowy). 
Być może można wspomóc się bibliotekami jak np. Dalex. 

## Done

Czyszczenie danych - bj. Poprawiłem dane na kilka sposobów. Głównie przekształciłem dane do postaci niekategorycznej gdzie się dało, co znacząco poprawiło jakosć predykcji dla random forest (z ok. 70 do 92). UPDATE teraz wyniki się trochę zmieniły.

Dopasowany został model:
  - Random Forest - acc 0.920 old
  - lm            - acc 0.7545126 old
  - svm z jądrem gausowskim - bj
