% Regola per assegnare 30 km/h alle strade di tipo track con maxspeed nullo.
assign_track_speed :-
    retract(strada(Id, 'track', Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, 'track', Name, Oneway, 30, Lanes, Length)),
    fail.
assign_track_speed.

% Predicati per memorizzare la distribuzione di probabilità con pesi
:- dynamic speed_distribution/3.

% Calcolo della distribuzione per ciascuna categoria di strada con pesi di lunghezza
calculate_speed_distribution :-
    findall(Highway, strada(_, Highway, _, _, _, _, _), Highways),
    list_to_set(Highways, UniqueHighways),
    forall(member(Hwy, UniqueHighways), calculate_distribution_for(Hwy)).

calculate_distribution_for(Highway) :-
    findall(MaxSpeed-Length, strada(_, Highway, _, _, MaxSpeed, _, Length), SpeedsLengths),
    exclude(=(null-_), SpeedsLengths, ValidSpeedsLengths),
    maplist(round_length, ValidSpeedsLengths, RoundedSpeedsLengths),
    calculate_weighted_distribution(RoundedSpeedsLengths, Distribution, Total),
    retractall(speed_distribution(Highway, _, _)),
    assertz(speed_distribution(Highway, Distribution, Total)).

round_length(MaxSpeed-Length, MaxSpeed-RoundedLength) :-
    RoundedLength is round(Length).

calculate_weighted_distribution(SpeedsLengths, Distribution, Total) :-
    findall(Speed, member(Speed-_, SpeedsLengths), Speeds),
    msort(Speeds, SortedSpeeds),
    findall([Speed, WeightSum], (
        member(Speed, SortedSpeeds),
        findall(Weight, member(Speed-Weight, SpeedsLengths), Weights),
        sumlist(Weights, WeightSum)
    ), Distribution),
    sum_weights(SpeedsLengths, Total).

sum_weights(List, Sum) :-
    sum_weights(List, 0, Sum).

sum_weights([], Sum, Sum).
sum_weights([_-Weight|Rest], Acc, Sum) :-
    NewAcc is Acc + Weight,
    sum_weights(Rest, NewAcc, Sum).

% Regola per assegnare i valori mancanti basandosi sulla distribuzione di probabilità
assign_missing_speeds :-
    strada(Id, Highway, Name, Oneway, null, Lanes, Length),
    speed_distribution(Highway, Distribution, Total),
    random_between(1, Total, Rand),
    select_speed(Distribution, Rand, SelectedSpeed),
    retract(strada(Id, Highway, Name, Oneway, null, Lanes, Length)),
    assertz(strada(Id, Highway, Name, Oneway, SelectedSpeed, Lanes, Length)),
    fail.
assign_missing_speeds.

select_speed([[Speed, Weight]|_], Rand, Speed) :-
    Rand =< Weight.
select_speed([[_, Weight]|Rest], Rand, Speed) :-
    NewRand is Rand - Weight,
    select_speed(Rest, NewRand, Speed).

% Regola che chiama le regole di assegnazione della velocità con open-world assumption
assign_speeds :-
    assign_track_speed,
    calculate_speed_distribution,
    assign_missing_speeds.