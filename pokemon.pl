% sabri bayrakdar
% 2016400378
% compiling:  yes
% complete:  yes

:- [pokemon_data].                                                                                  %Include pokemon_data.pl file.
%%%%%%%%%%%%%%%%%%%%%%Case 1%%%%%%%%%%%%%%%%%%%%%%%%
evolution_path(Level, Pokemon, EvolvedPokemon):-                                                    %This function finds all available evolution pokemons for given pokemon with recursive.
    pokemon_evolution(Pokemon,EvolvedPokemon,MinRequiredLevel), MinRequiredLevel=<Level.            
evolution_path(Level, Pokemon, EvolvedPokemon):-                                                    %This function finds all available evolution pokemons for given pokemon with recursive.
    pokemon_evolution(Pokemon,DiffEvolvedPokemon,MinRequiredLevel), MinRequiredLevel=<Level,
    evolution_path(Level, DiffEvolvedPokemon, EvolvedPokemon).

find_pokemon_evolution(Level, Pokemon, EvolvedPokemon):-                                            %This function calls evolution_path function with three parameters and lists its return values.
    findall(EvolvedPokemon,evolution_path(Level, Pokemon, EvolvedPokemon),Path),
    append([Pokemon],Path,List),                                                                    %After listing return value of evolution_path function it appends pokemon itself, because in case of it has no adequate level for evolution, to show it remains as same pokemon. 
    last(List,EvolvedPokemon).                                                                      %Finally get the last element of the list to show that the pokemon can evolve as long as its level let itself to evolve.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 2%%%%%%%%%%%%%%%%%%%%%%%%%
pokemon_level_stats(PokemonLevel, Pokemon, PokemonHp, PokemonAttack, PokemonDefense):-              %This function gets the base values of pokemon (health, attack and defense) and calculate its actual values according to pokemon level.
    pokemon_stats(Pokemon, _, HealthPoint, Attack, Defense),                                        %Get the values of pokemon from pokemon_data.pl file
    PokemonHp is HealthPoint+PokemonLevel*2,                                                        %Calculate health
    PokemonAttack is Attack+PokemonLevel,                                                           %Calculate attack
    PokemonDefense is Defense+PokemonLevel.                                                         %Calculate defense
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 3%%%%%%%%%%%%%%%%%%%%%%%%%
element_at(X,[X|_],0).                                                                              %I defined element_at function as helper function and used it in single_type_multiplier function.
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.                                           %Element_at function is to search an element in a list.It search an element and if it finds the given element then returns index of the element.

single_type_multiplier(AttackerType, DefenderType, Multiplier):-                                    %This function finds the multiplier of AttackerType for DefenderType.
    pokemon_types(TypeNameList),                                                                    %Get the type names as list from the data
    type_chart_attack(AttackerType, AttackerMultiplierList),                                        %Get the multiplier list of AttackerType.
    nth0(Index, TypeNameList, DefenderType),                                                        %Find the index of DefenderType in TypeNameList (name list of types).
    element_at(Multiplier,AttackerMultiplierList,Index).                                            %Find the element with index that i found in previous instruction (Index).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 4%%%%%%%%%%%%%%%%%%%%%%%%%
type_multiplier(_,[], 1).
type_multiplier(AttackerType,[H|T], Sum) :-
   type_multiplier(AttackerType,T, Rest),
   single_type_multiplier(AttackerType,H,Multiplier),
   Sum is Multiplier * Rest.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 5%%%%%%%%%%%%%%%%%%%%%%%%%
type_multiplier_complex([],_, 0).                               %This function takes AttackerPokemon list and for each of them calls type_multiplier function.It compares their multipliers choosing max multiplier.
type_multiplier_complex([H|T],DefenderTypes, Sum) :-
   type_multiplier_complex(T,DefenderTypes,Rest),
   type_multiplier(H,DefenderTypes,Multiplier),
   Sum is max(Multiplier , Rest).

pokemon_type_multiplier(AttackerPokemon,DefenderPokemon,Multiplier):-
    pokemon_stats(AttackerPokemon, AttackerTypes, _, _, _),                         %Find the type/types of attacker pokemon
    pokemon_stats(DefenderPokemon, DefenderTypes, _, _, _),                         %Find the type/types of defender pokemon
    type_multiplier_complex(AttackerTypes,DefenderTypes,Multiplier).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 6%%%%%%%%%%%%%%%%%%%%%%%%%                                %%%%%%%%%% DefenderPokemonLevel is not used !!!!!!!
pokemon_attack(AttackerPokemon, AttackerPokemonLevel, DefenderPokemon, DefenderPokemonLevel, Damage):-
    pokemon_level_stats(AttackerPokemonLevel,AttackerPokemon,_,AttackerPokemonAttack,_),
    pokemon_level_stats(DefenderPokemonLevel,DefenderPokemon,_,_,DefenderPokemonDefense),
    pokemon_type_multiplier(AttackerPokemon,DefenderPokemon,TypeMultiplier),
    Damage is (0.5 * AttackerPokemonLevel * (AttackerPokemonAttack / DefenderPokemonDefense) * TypeMultiplier + 1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 7%%%%%%%%%%%%%%%%%%%%%%%%%
pokemon_fight(Pokemon1, Pokemon1Level, Pokemon2, Pokemon2Level, Pokemon1Hp, Pokemon2Hp, Rounds):-
    pokemon_level_stats(Pokemon1Level,Pokemon1,Pokemon1HpInit,_,_),
    pokemon_level_stats(Pokemon2Level,Pokemon2,Pokemon2HpInit,_,_),
    pokemon_attack(Pokemon1,Pokemon1Level,Pokemon2,Pokemon2Level,Damage1),
    pokemon_attack(Pokemon2,Pokemon2Level,Pokemon1,Pokemon1Level,Damage2),
    finish_fight(Pokemon1HpInit,Pokemon2HpInit,Damage1,Damage2,Pokemon1Hp,Pokemon2Hp,Rounds).

finish_fight(Pokemon1Hp,Pokemon2Hp,Damage1,Damage2,FinalHp1,FinalHp2,MinRound):-
    Round1 is (Pokemon1Hp / Damage2),
    Round2 is (Pokemon2Hp / Damage1),
    MinRound is ceiling(min(Round1,Round2)),
    FinalHp1 is (Pokemon1Hp - MinRound * Damage2),
    FinalHp2 is (Pokemon2Hp - MinRound * Damage1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%Case 8%%%%%%%%%%%%%%%%%%%%%%%%%%
brute_force([Pokemon], [Level], [EvolvedPokemon]):-
    find_pokemon_evolution(Level, Pokemon, EvolvedPokemon).
brute_force([Pokemon|Pokemons], [Level|Levels], [EvolvedPokemon|T]):-
    brute_force(Pokemons, Levels, T),
    find_pokemon_evolution(Level, Pokemon, EvolvedPokemon).

match([Team1],[Level1],PokemonTrainer1,[Team2],[Level2],PokemonTrainer2,[Result]):-
    pokemon_fight(Team1,Level1,Team2,Level2,Hp1,Hp2,_),
    (Hp1>Hp2->Result = PokemonTrainer1;Result = PokemonTrainer2).
match([Team1|Team1s],[Level1|Level1s],PokemonTrainer1,[Team2|Team2s],[Level2|Level2s],PokemonTrainer2,[Result|Results]):-
    pokemon_fight(Team1,Level1,Team2,Level2,Hp1,Hp2,_),
    (Hp1>Hp2->Result = PokemonTrainer1;Result = PokemonTrainer2),
    match(Team1s,Level1s,PokemonTrainer1,Team2s,Level2s,PokemonTrainer2,Results).

pokemon_tournament(PokemonTrainer1, PokemonTrainer2, WinnerTrainerList):-
    pokemon_trainer(PokemonTrainer1, PokemonTeam1, PokemonLevels1),
    pokemon_trainer(PokemonTrainer2, PokemonTeam2, PokemonLevels2),
    brute_force(PokemonTeam1,PokemonLevels1,EvolvedPokemonTeam1),
    brute_force(PokemonTeam2,PokemonLevels2,EvolvedPokemonTeam2),
    match(EvolvedPokemonTeam1,PokemonLevels1,PokemonTrainer1,EvolvedPokemonTeam2,PokemonLevels2,PokemonTrainer2,WinnerTrainerList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 9%%%%%%%%%%%%%%%%%%%%%%%%
best_pokemon(EnemyPokemon, LevelCap, RemainingHP, BestPokemon):-
    findall(RemainingHP,pokemon_fight(EnemyPokemon,LevelCap,_,LevelCap,_,RemainingHP,_),HPList),
    list_max(HPList,RemainingHP),
    pokemon_fight(EnemyPokemon,LevelCap,BestPokemon,LevelCap,_,RemainingHP,_).

list_max([], R, R). %end
list_max([X|Xs], WK, R):- X >  WK, list_max(Xs, X, R). %WK is Carry about
list_max([X|Xs], WK, R):- X =< WK, list_max(Xs, WK, R).
list_max([X|Xs], R):- list_max(Xs, X, R). %start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 10%%%%%%%%%%%%%%%%%%%%%%%
best_pokemon_team(OpponentTrainer, PokemonTeam):-
    pokemon_trainer(OpponentTrainer, EnemyPokemonTeam, PokemonLevels),
    best_pokemon_recursion(EnemyPokemonTeam,PokemonTeam,PokemonLevels).

best_pokemon_recursion([E],[B],[PL]):-
    best_pokemon(E,PL,_,B).
best_pokemon_recursion([E|EnemyPokemonTeam],[B|BestPokemons],[PL|PokemonLevels]):-
    best_pokemon(E,PL,_,B),
    best_pokemon_recursion(EnemyPokemonTeam,BestPokemons,PokemonLevels).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 11%%%%%%%%%%%%%%%%%%%%%%%
pokemon_types(TypeList,InitialPokemonList,PokemonList):-
    pokemon_type(TypeList,InitialPokemonList,PokeList),
    destroy_inner_list(PokeList,PokemonList).

destroy_inner_list([],[]).
destroy_inner_list([H|Tail],List):-
    append(H,Ys0,List),
    destroy_inner_list(Tail,Ys0).

pokemon_type([T], InitialPokemonList, [PokemonList]):-
    findall(Pokemon, ((pokemon_stats(Pokemon,PokemonTypes, _, _, _),(member(T,PokemonTypes))),member(Pokemon,InitialPokemonList)),PokemonList).
pokemon_type([T|TypeList], InitialPokemonList, [P|PokemonList]):-
    findall(Pokemon, ((pokemon_stats(Pokemon,PokemonTypes, _, _, _),(member(T,PokemonTypes))),member(Pokemon,InitialPokemonList)),P),
    pokemon_type(TypeList,InitialPokemonList,PokemonList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%Case 12%%%%%%%%%%%%%%%%%%%%%%%
generate_pokemon_team(LikedTypes,DislikedTypes,Criterion,Count,PokemonTeam):-
    like(LikedTypes,Likedd),
    destroy_inner_list(Likedd,Liked),
    dislike(DislikedTypes,Dislikedd),
    destroy_inner_list(Dislikedd,Disliked),
    remove_list(Liked,Disliked,Removed),
    switch(Criterion, [
        h : predsort(health_criteria,Removed, Sorted),
        a : predsort(attack_criteria,Removed, Sorted),
        d : predsort(defense_criteria,Removed, Sorted)
    ]),
    split(Sorted,Count,PokemonTeam,_).

remove_list([], _, []).
remove_list([X|Tail], L2, Result):- member(X, L2), !, remove_list(Tail, L2, Result). 
remove_list([X|Tail], L2, [X|Result]):- remove_list(Tail, L2, Result).

like([L],[P]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(L,Types)),P).
like([L|LikedTypes],[P|PokemonTeam]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(L,Types)),P),
    like(LikedTypes,PokemonTeam).

dislike([D],[P]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(D,Types)),P).
dislike([D|DislikedTypes],[P|PokemonTeam]):-
    findall([Pokemon,HealthPoint,Attack,Defense], (pokemon_stats(Pokemon, Types, HealthPoint, Attack, Defense),member(D,Types)),P),
    dislike(DislikedTypes,PokemonTeam).

health_criteria(R,[_,N1,_,_],[_,N2,_,_]) :- 
    N1=\=N2, !,
    compare(R,N2,N1).
health_criteria(R,E1,E2) :-
    compare(R,E1,E2).

attack_criteria(R,[_,_,N1,_],[_,_,N2,_]) :- 
    N1=\=N2, !,
    compare(R,N2,N1).
attack_criteria(R,E1,E2) :-
    compare(R,E1,E2).

defense_criteria(R,[_,_,_,N1],[_,_,_,N2]) :- 
    N1=\=N2, !,
    compare(R,N2,N1).
defense_criteria(R,E1,E2) :-
    compare(R,E1,E2).

switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

split(L,0,[],L).
split([X|Xs],N,[X|Ys],Zs) :- N > 0, N1 is N - 1, split(Xs,N1,Ys,Zs).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%