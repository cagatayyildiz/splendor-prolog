:- module(cagatay, []).
/*
:- dynamic cards/1.
:- dynamic tickets/1.
:- dynamic round/1.

cards([]).
tickets([]).
*/

initialize(PlayerName, PlayerCount) :-
	show(1, 'I am ~w of a ~w player game.~n', [PlayerName, PlayerCount]),
	assert(round(0)).

getGems(_,_).
buyCard(_).
reserveCard(_).
reserveCardFromDeck(_).


possible_moves([[0,0,1,1,1,0],[0,1,0,1,1,0],[0,1,1,0,1,0],[0,1,1,1,0,0],
[1,0,0,1,1,0],[1,0,1,0,1,0],[1,0,1,1,0,0],[1,1,0,0,1,0],[1,1,0,1,0,0],[1,1,1,0,0,0],
[2,0,0,0,0,0],[0,2,0,0,0,0],[0,0,2,0,0,0],[0,0,0,2,0,0],[0,0,0,0,2,0],
[1,1,0,0,0,0],[1,0,1,0,0,0],[1,0,0,1,0,0],[1,0,0,0,1,0],
[0,1,1,0,0,0],[0,1,0,1,0,0],[0,1,0,0,1,0],[0,0,1,1,0,0],[0,0,1,0,1,0],[0,0,0,1,1,0],
[1,0,0,0,0,0],[0,1,0,0,0,0],[0,0,1,0,0,0],[0,0,0,1,0,0],[0,0,0,0,1,0],[0,0,0,0,0,0]]).


updateRound(NewR):-
	round(R),
	write('\n\tThis is round '), write(R), nl,
	retractall(round(_)),
	NewR is R+1,
	assert(round(NewR)).


decideAction(Player, Oponents, StateProxy, Action) :-
	updateRound(Round),
	call(StateProxy, Player, gems, Gems),
	call(StateProxy, Player, score, Score),
	call(StateProxy, Player, bonuses, Bonuses),
	call(StateProxy, Player, reserves, Reserves),
	call(StateProxy, game, cards, Cards),
	call(StateProxy, game, tokens, Tokens),
	write('\t'),write('Gems: '),write(Gems),nl,
	write('\t'),write('Bonuses: '),write(Bonuses),nl,
	write('\t'),write('Reserves: '),write(Reserves),nl,
	write('\t'),write('Cards: '),write(Cards),nl,
	write('\t'),write('Tokens: '),write(Tokens),nl,
	get_all_possible_token_moves(PossMoves,Gems,Tokens),
	% getGems([0,0,1,1,1,0],[0,0,0,0,0,0]).
	write('\t'),write('Possible Token Actions: '),write(PossMoves),nl,
	(
		length(Reserves, ReservesLength),
		ReservesLength<3,
		% FIRST, TRY TO RESERVE
		( 
			(
				Round < 50,
				length(Cards, CardsLength),
				CardsLength1 is CardsLength+1,
				random(1,CardsLength1, ReserveId1),
				random(1,10,ReserveId2),
				splendor:min(ReserveId1,ReserveId2,ReserveId),
				nth1(ReserveId, Cards, ReservedCard),
				Action = reserveCard(ReservedCard,[0,0,0,0,0,0]),
				!
			)
			;
			(
				Round >= 50,
				length(Cards, CardsLength),
				CardsLength1 is CardsLength+1,
				random(1,CardsLength1, ReserveId1),
				random(8,13,ReserveId2),
				splendor:max(ReserveId1,ReserveId2,ReserveId),
				(
					(
						ReserveId =< CardsLength,
						nth1(ReserveId, Cards, ReservedCard),
						Action = reserveCard(ReservedCard,[0,0,0,0,0,0]),
						!
					)
					;
					(
						ReserveId > CardsLength,
						nth1(ReserveId1, Cards, ReservedCard),
						Action = reserveCard(ReservedCard,[0,0,0,0,0,0]),
						!
					)
				)
			)
		)
		;
		% THEN, TRY TO BUY CARD. RESERVED HAVE PRIORITY
		( 
			(
				canBuyCards(Gems, Bonuses, Reserves, CanBuyCards),
				nth1(1, CanBuyCards, CardId),
				Action = buyCard(CardId),
				!
			)
			;
			(
				canBuyCards(Gems, Bonuses, Cards, CanBuyCards),
				nth1(1, CanBuyCards, CardId),
				Action = buyCard(CardId),
				!
			)
		)
		;
		% THEN, TRY TO TAKE TOKENS THAT GETS YOU BUY A RESERVED CARD
		pick_token_just_before_card(PossMoves,Gems,Bonuses,Reserves,TokensToTake),
		length(TokensToTake,L),
		L > 1,
		write('\tFOUND TOKENS TO BUY A RESERVED CARD! \n'),
		nth1(1,TokensToTake,Token),
		Action = getGems(Token, [0,0,0,0,0,0]),
		!
		;
		% THEN, TRY TO TAKE TOKENS THAT GETS YOU BUY ANY CARD WITH PROBABILITY 0.33333
		pick_token_just_before_card(PossMoves,Gems,Bonuses,Cards,TokensToTake),
		length(TokensToTake,L),
		L > 1,
		write('\tFOUND TOKENS TO BUY A CARD! \n'),
		nth1(1,TokensToTake,Token),
		Action = getGems(Token, [0,0,0,0,0,0]),
		!
		;
		% IF NOTHING WORKS, RANDOMLY GET SOME TOKEN. SORRY BRO
		randomGetGems(Gems, Tokens, RandGems, BackGems),
		Action = getGems(RandGems, BackGems),
		!
	)
	,member(Oponent, Oponents)
	,call(StateProxy, Oponent, score, _)
	.

pick_token_just_before_card([],_,_,_,[]):-!.
pick_token_just_before_card([TokensToTake|RestOfTokensAllowedToTake],TokensAtHand,Bonuses,TargetedCards,AllTokensToTake):-
	pick_token_just_before_card(RestOfTokensAllowedToTake,TokensAtHand,Bonuses,TargetedCards,RestTokensToTake),
	sum_lists(TokensToTake,TokensAtHand,SumTokens),
	canBuyCards(SumTokens,Bonuses,TargetedCards,CardsAllowedToBuy),
	(
		(
			length(CardsAllowedToBuy,Len),
			Len > 1,
			append(TokensToTake,RestTokensToTake,AllTokensToTake),!
		)
		;
		(
			AllTokensToTake = RestTokensToTake
		)
	).



% sum(List1+List2)
% sum_lists(G,[],G):-!.
% sum_lists([],G,G):-!.
% sum_lists([H1|T1],[H2|T2],[H3|T3]):-
%	sum_lists(T1,T2,T3),
%	H3 is H1 + H2.



sum(List1+List2)
sum_lists([0],[Y],[Y]):- Y =< 1, !.
sum_lists([0],[Y],[1]):- Y > 1, !.
sum_lists([H1|T1],[H2|T2],[H3|T3]):-
	sum_lists(T1,T2,T3),
	H3 is H1 + H2.



selectNoble([H|_],H).

canBuyCards(_, _, [], []).

canBuyCards(Gems, Bonuses, [H|T], A) :-
	(
		canBuyCard(Gems, Bonuses, H),
		A = [H|X2]
		;
		A=X2
	),
	canBuyCards(Gems, Bonuses, T, X2)
	.


randomTest(A) :-
	A=3,
	display('hi there'),
	repeat,
	random(1, 10000000, X),
	X<5,
	!.


% Check if the player currently having Curr gems, can take X gems provided that C gems are available
% all are lists of length 6
isMoveValid(Curr,X,C):-
	isGetGemValid(Curr, X, [0,0,0,0,0,0], C).

get_all_possible_token_moves(Poss_Moves,TokensAtHand,TokensAvailable):-
	possible_moves(Moves),
	get_all_possible_token_moves_sub(Moves,Poss_Moves,TokensAtHand,TokensAvailable).

get_all_possible_token_moves_sub([],[],_,_).

get_all_possible_token_moves_sub([H|T],[H|X],TokensAtHand,TokensAvailable):-
	isMoveValid(TokensAtHand,H,TokensAvailable),
	get_all_possible_token_moves_sub(T,X,TokensAtHand,TokensAvailable).
	
get_all_possible_token_moves_sub([H|T],X,TokensAtHand,TokensAvailable):-
	\+ isMoveValid(TokensAtHand,H,TokensAvailable),
	get_all_possible_token_moves_sub(T,X,TokensAtHand,TokensAvailable).
	