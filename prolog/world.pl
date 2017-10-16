:- ensure_loaded(borders).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(rivers).

country(C) :-
  country(C, _, _, _, _, _, _, _).

larger(Country1, Country2) :-
  country(Country1, _, _, _, Area1, _, _, _),
  country(Country2, _, _, _, Area2, _, _, _),
  Area1>Area2.

river_country(River, Country) :-
  country(Country),
  river(River, Cs),
  member(Country, Cs).

country_region(Country, Region) :-
  country(Country, Region, _, _, _, _, _, _).