% Programmer: Etamar Cohen
% File Name: Apocalypse.pl
% Description: The program simulates a game call Apocalypse. It's a chess veriant.
% Input: wanted level, and each turn the coordinates in format of [current_coordinates | new_coordinates].
% Output: The board, and further instructions as needed.
% Synopsys: choose a level, and play the game turn by turn. follow the instructions.

%write the rules, and call choose_level.
apocalypse() :-
	writeln("Welcome to Apocalypse. Apocalypse is a chess variant invented by C. S. Elliott\nin 1976."),
	writeln("The rules:"),
	writeln("\t1. Each player starts with five pawns and two knights."),
	writeln("\t2. Both pieces moves the same as chess, but the pawns doesn't\n\thave a double-step option on their first move."),
	writeln("\t3. For each turn, each player secretly writes down their move, then the\n\tplayers simultaneously declare them."),
	writeln("\t4. If they moved to the same square, a horseman captures a footman.\n\tSame-type pieces are both removed from the board."),
	writeln("\t5. If a capture was declared using a footman, but the piece to be\n\tcaptured move from it's square, the footman move still stands."),
	writeln("\t6. Moving your piece to a square containing one of your piece would be\n\tconsidered an illegal move."),
	writeln("\t7. A pawn promotes to horseman when reaching the last rank."),
	writeln("\t8. A player wins by being first to eliminate all of the opponent's\n\tfootmen. A stalemate is a draw."),
	writeln("\t9. A stalemate would be if there's only one pawn for both sides."),
	writeln("Each turn, enter the current coordinates of the piece you want to move and the\nnew coordinates, seperated by | [e.g. B5 | A3]."),
	writeln("At each point, you can enter 'quit' to close the game, or 'restart' for a new\ngame."),
	writeln("The Program will ignore all input that isn't requested by the program."),
	writeln("The computer play's black."),
	
	choose_level().
	
%choose level and call play_turn (starts the game). happens until the user enters legal level.
choose_level() :-
	writeln("Choose level:"),
	writeln("\t1. Easy."),
	writeln("\t2. Intermidate."),
	writeln("\t3. Hard."),	
	writeln("FP"), %signal that the printing is finished
	
	read_line_to_string(current_input, Level), string_to_list(Level, LevelChar),
	
	%check input. if err then get input again, else start turn
	((level_in_ascii_to_nums(LevelChar, LevelNum), check_level_input(LevelNum)) -> true; (writeln("illegal input."), choose_level(), !)),
	writeln([['BK', 'BP', 'BP', 'BP', 'BK'], ['BP', '_', '_', '_', 'BP'], ['_', '_', '_', '_', '_'], ['WP', '_', '_', '_', 'WP'], ['WK', 'WP', 'WP', 'WP', 'WK']]),
	Level1 is LevelNum,
	play_turn([['BK', 'BP', 'BP', 'BP', 'BK'], ['BP', '_', '_', '_', 'BP'], ['_', '_', '_', '_', '_'], ['WP', '_', '_', '_', 'WP'], ['WK', 'WP', 'WP', 'WP', 'WK']], Level1).

%checks that the input level really represents a level in the game.
check_level_input(Level) :- Level >= 1, Level =< 3, !.
check_level_input(_) :- fail.

%get turn from the user, calculate computer turn by using AB algorithm, and call check_input.
play_turn(Board, Level) :-
	writeln("Enter Turn and wait for the computer:"),
	writeln("FP"), %signal that the printing is finished
	alphabeta(Level, Board, -1000, 1000, GoodPos, _),
	read_line_to_string(current_input, PLTurn),
	check_input(Board, Level, PLTurn, GoodPos).
	
%the user entered wrong coordinates, no need calculating computer turn again. get coordinates from the user.
play_turn(Board, Level, CMPTurn) :-
	writeln("Enter Turn:"),
	writeln("FP"), %signal that the printing is finished
	read_line_to_string(current_input, PLTurn),
	check_input(Board, Level, PLTurn, CMPTurn).
	
%check if the input entered is legal, and continue the game.
check_input(Board, Level, PLTurn, CMPTurn) :-
	string_to_list(PLTurn, PLTurnList),
	((check_coordinates(PLTurnList, ASCCoordinates)) -> true;
		(writeln("Those aren't coordinates. Enter Turn in format of [current_coordinates | new_coordinates]:"), play_turn(Board, Level, CMPTurn) ,!)),
	
	%get info of the coordinates to check legality. check legality and then calculate turn logic (what happened and changing the board accordingly)
	coordinates_in_ascii_to_nums(ASCCoordinates, NUMCooridantes),
	
	get_piece_in_coordinates(Board, NUMCooridantes, PLOldPiece, PLNewPiece),
	string_chars(PLOldPiece, PLOldPieceChars),
	string_chars(PLNewPiece, PLNewPieceChars),
	
	get_piece_in_coordinates(Board, CMPTurn, CMPOldPiece, _),
	string_chars(CMPOldPiece, CMPOldPieceChars),

	%check move and play both turns, check if win, and continue the game.
	((check_move(PLOldPieceChars, PLNewPieceChars, NUMCooridantes)) -> check_turn_logic(Board, Level, NUMCooridantes, PLOldPiece, PLOldPieceChars, CMPTurn, CMPOldPiece, CMPOldPieceChars);
	writeln("The move isn't legal"), play_turn(Board, Level, CMPTurn)).	

%check what happened in the turn by the roles.
%if they move to the same square and the new square is empty- a horseman captures a footman. Same-type pieces are both removed from the board.
check_turn_logic(Board, Level, [X1, Y1, X3, Y3], PLOldPiece, PLOldPieceChars, [X2, Y2, X3, Y3], CMPOldPiece, CMPOldPieceChars) :-
	get_role(PLOldPieceChars, PLOldPiecerole), get_role(CMPOldPieceChars, CMPOldPiecerole), %get info
	((PLOldPiecerole = CMPOldPiecerole) -> change_piece_in_coordinates(Board, ['_', '_', '_'], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard); %same type pieces- remove both pieces from board
		((PLOldPiecerole = 'P', CMPOldPiecerole = 'K') -> change_piece_in_coordinates(Board, ['_', '_', CMPOldPiece], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard); %computer wins player
			change_piece_in_coordinates(Board, ['_', '_', PLOldPiece], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard))), %player wins computer
	check_piece_upgrade(NewBoard, [[X3, Y3]], NewBoard2), %check if the piece is a pawn who got to last rank and upgrade to knight
	writeln(NewBoard2),
	check_win(NewBoard2, Level), !. %check if the game ended

%player move to a square the computer running from.
check_turn_logic(Board, Level, [X1, Y1, X2, Y2], PLOldPiece, _, [X2, Y2, X3, Y3], CMPOldPiece, _) :-
	change_piece_in_coordinates(Board, ['_', PLOldPiece, CMPOldPiece], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard),
	check_piece_upgrade(NewBoard, [[X2, Y2], [X3, Y3]], NewBoard2),
	writeln(NewBoard2),
	check_win(NewBoard2, Level), !.

%computer move to a square the player running from.
check_turn_logic(Board, Level, [X1, Y1, X2, Y2], PLOldPiece, _, [X3, Y3, X1, Y1], CMPOldPiece, _) :-
	change_piece_in_coordinates(Board, ['_', PLOldPiece, CMPOldPiece], [[X3, Y3], [X2, Y2], [X1, Y1]], NewBoard),
	check_piece_upgrade(NewBoard, [[X2, Y2], [X1, Y1]], NewBoard2),
	writeln(NewBoard2),
	check_win(NewBoard2, Level), !.
	
%no overlapping coordinates.
check_turn_logic(Board, Level, [X1, Y1, X2, Y2], PLOldPiece, _, [X3, Y3, X4, Y4], CMPOldPiece, _) :-
	change_piece_in_coordinates(Board, ['_', '_', PLOldPiece, CMPOldPiece], [[X1, Y1], [X3, Y3], [X2, Y2], [X4, Y4]], NewBoard),
	check_piece_upgrade(NewBoard, [[X2, Y2], [X4, Y4]], NewBoard2),
	writeln(NewBoard2),
	check_win(NewBoard2, Level).

%check if a pawn got to last rank and upgrade it to a knight
check_piece_upgrade(Board, [], Board) :- !.
check_piece_upgrade(Board, [X | Zs], NewBoard) :-
	get_piece_in_coordinates(Board, X, Piece), string_chars(Piece, PieceChars), get_side(PieceChars, PieceSide), get_role(PieceChars, Piecerole),
	check_piece_upgrade_1(Board, X, PieceSide, Piecerole, NewBoard1),
	check_piece_upgrade(NewBoard1, Zs, NewBoard), !.

%white pawn who go to last rank
check_piece_upgrade_1(Board, [X, 1], 'W', 'P', NewBoard) :-
	change_piece_in_coordinates(Board, ['WK'], [[X, 1]], NewBoard), !.

%black pawn who got to last rank
check_piece_upgrade_1(Board, [X, 5], 'B', 'P', NewBoard) :-
	change_piece_in_coordinates(Board, ['BK'], [[X, 5]], NewBoard), !.

%not an upgrade
check_piece_upgrade_1(Board, _, _, _, Board).

%after a turn, check if we have a winner. if not, continue the game.
check_win(Board, Level) :-	
	%check if there's no more pawn for either side
	count_piece(Board, 'WP', PlayerPawnsCount), count_piece(Board, 'BP', ComputerPawnsCount),
	check_win_1(Board, Level, PlayerPawnsCount, ComputerPawnsCount).

%tie
check_win_1(_, _, 0, 0) :-
	writeln("It's a tie!."), writeln("FP"), !.

%Player Won
check_win_1(_, _, _, 0) :-
	writeln("You Have Won!."), writeln("FP"), !.
	
%Computer Won
check_win_1(_, _, 0, _) :-
	writeln("The Computer Has Won!."), writeln("FP"), !.
	
%stalemate
check_win_1(_, _, 1, 1) :-
	writeln("It's a stalemate!."), writeln("FP"), !.
	
%no winner
check_win_1(Board, Level, _, _) :-
	play_turn(Board, Level).
	

%check that the input really represent coordinates.
check_coordinates([A, B, SP, C, SP, D, E | Y], [A, B, D, E]) :-
	A >= 65, A =< 69, B >= 49, B =< 53, C = 124, D >= 65, D =< 69, E >= 49, E =< 53, SP = 32, Y = [], !.
	
%convert ascii to numeric value. E.g. 'A' = 1, 'B' = 2, '1' = 1, ETC.
coordinates_in_ascii_to_nums([A, B, C, D], [A1, B1, C1, D1]) :-
	A1 is A - 64,
	B1 is B - 48,
	C1 is C - 64,
	D1 is D - 48, !.
	
coordinates_in_ascii_to_nums([A, B], [A1, B1]) :-
	A1 is A - 64,
	B1 is B - 48.
	
level_in_ascii_to_nums([A], A1) :-
	A1 is A - 48, !.
		
%send back what a specific coordinates contains
get_piece_in_coordinates(Board, [A, B, C, D], OldPiece, NewPiece) :-
	nth1(B, Board, Line1), nth1(A, Line1, OldPiece),
	nth1(D, Board, Line2), nth1(C, Line2, NewPiece).
	
%send back what a specific coordinate contains
get_piece_in_coordinates(Board, [X, Y], Piece) :-
	nth1(Y, Board, Line1), nth1(X, Line1, Piece).
	
%check if piece is the player's (white), that the new piece isn't his and if the piece can move so.
check_move([PLOldPieceSide | PLOldPieceRole], [PLNewPieceSide | _], NUMCooridantes) :-
	PLOldPieceSide = 'W', PLNewPieceSide \= 'W', check_move_legality(PLOldPieceRole, PLNewPieceSide, NUMCooridantes).

%check if a pawn can move so.
check_move_legality(['P'], _, [X1, Y1, X2, Y2]) :-
	Y2 - Y1 =:= -1, X2 = X1, !.

check_move_legality(['P'], PLNewPieceRole, [X1, Y1, X2, Y2]) :-	
	Y2 - Y1 =:= -1, (X2 - X1 =:= 1, !; X2 - X1 =:= -1), PLNewPieceRole \= '_', !.

%check if a knight can move so.
check_move_legality(['K'], _, [X1, Y1, X2, Y2]) :-
	(Y2 - Y1 =:= -2, !; Y2 - Y1 =:= 2), (X2 - X1 =:= 1, !; X2 - X1 =:= -1), !.

%check if a knight can move so.
check_move_legality(['K'], _, [X1, Y1, X2, Y2]) :-
	(Y2 - Y1 =:= -1, !; Y2 - Y1 =:= 1), (X2 - X1 =:= 2, !; X2 - X1 =:= -2), !.

%returns role of given piece
get_role(['_'], '_') :- !.
get_role([_, Role], Role).

%returns side of given piece
get_side(['_'], '_') :- !.
get_side([Side, _], Side).
	
%change the piece in given coordinate to new piece 
change_piece_in_coordinates(Board, [], [], Board) :- !.
change_piece_in_coordinates(Board, [PieceChars | PCs], [X | Ys], NewBoard) :-
	change_line_in_board(Board, X, PieceChars, NewBoard1),
	change_piece_in_coordinates(NewBoard1, PCs, Ys, NewBoard).
	

change_line_in_board([L1, L2, L3, L4, L5], [X, 1], NewPiece, [NewLine, L2, L3, L4, L5]) :- change_piece_in_line(L1, X, NewPiece, NewLine), !.
change_line_in_board([L1, L2, L3, L4, L5], [X, 2], NewPiece, [L1, NewLine, L3, L4, L5]) :- change_piece_in_line(L2, X, NewPiece, NewLine), !.
change_line_in_board([L1, L2, L3, L4, L5], [X, 3], NewPiece, [L1, L2, NewLine, L4, L5]) :- change_piece_in_line(L3, X, NewPiece, NewLine), !.
change_line_in_board([L1, L2, L3, L4, L5], [X, 4], NewPiece, [L1, L2, L3, NewLine, L5]) :- change_piece_in_line(L4, X, NewPiece, NewLine), !.
change_line_in_board([L1, L2, L3, L4, L5], [X, 5], NewPiece, [L1, L2, L3, L4, NewLine]) :- change_piece_in_line(L5, X, NewPiece, NewLine).

change_piece_in_line([_, B, C, D, E], 1, NewPiece, [NewPiece, B, C, D, E]).
change_piece_in_line([A, _, C, D, E], 2, NewPiece, [A, NewPiece, C, D, E]).
change_piece_in_line([A, B, _, D, E], 3, NewPiece, [A, B, NewPiece, D, E]).
change_piece_in_line([A, B, C, _, E], 4, NewPiece, [A, B, C, NewPiece, E]).
change_piece_in_line([A, B, C, D, _], 5, NewPiece, [A, B, C, D, NewPiece]).


%count how many of a piece a Side has
count_piece([], _, 0) :- !.
count_piece([Piece | Ys], Piece, Count) :- count_piece(Ys, Piece, Count1), Count is 1+Count1, !.
count_piece([[A, B, C, D, E] | Ys], Piece, Count) :- count_piece([A, B, C, D, E], Piece, Count1), count_piece(Ys, Piece, Count2), Count is Count1+Count2, !.
count_piece([NotPiece | Ys], Piece, Count) :- NotPiece \= Piece, count_piece(Ys, Piece, Count), !.

%count how many of pieces each side have  BKCount, BPCount, WKCount, WPCount
count_pieces([], 0, 0, 0, 0) :- !.
count_pieces(['BK' | Ys], BKCount, BPCount, WKCount, WPCount) :-
	count_pieces(Ys, BKCount1, BPCount, WKCount, WPCount),
	BKCount is 1+BKCount1.
count_pieces(['BP' | Ys], BKCount, BPCount, WKCount, WPCount) :-
	count_pieces(Ys, BKCount, BPCount1, WKCount, WPCount),
	BPCount is 1+BPCount1.
count_pieces(['WK' | Ys], BKCount, BPCount, WKCount, WPCount) :-
	count_pieces(Ys, BKCount, BPCount, WKCount1, WPCount),
	WKCount is 1+WKCount1.
count_pieces(['WP' | Ys], BKCount, BPCount, WKCount, WPCount) :-
	count_pieces(Ys, BKCount, BPCount, WKCount, WPCount1),
	WPCount is 1+WPCount1.
count_pieces(['_' | Ys], BKCount, BPCount, WKCount, WPCount) :-
	count_pieces(Ys, BKCount, BPCount, WKCount, WPCount).
count_pieces([[A, B, C, D, E] | Ys], BKCount, BPCount, WKCount, WPCount) :-
	count_pieces([A, B, C, D, E], BKCount1, BPCount1, WKCount1, WPCount1), 
	count_pieces(Ys, BKCount2, BPCount2, WKCount2, WPCount2),
	BKCount is BKCount1+BKCount2,
	BPCount is BPCount1+BPCount2,
	WKCount is WKCount1+WKCount2,
	WPCount is WPCount1+WPCount2, !.

%alphabeta algorithm changed to work with simultaneous turns and depth limitation. See PsuedoCode in doc for more info
%let Alpha be the best min value we found so far.
%let Beta be the min value in current branch
alphabeta(Depth, Pos, Alpha, Beta, GoodPos, Val) :-
	moves(Pos, PossibleMovesBlack, PossibleMovesWhite), Depth > 0, !, %if didn't meet the depth limitation.
	boundedbest(Depth, Pos, PossibleMovesBlack, PossibleMovesWhite, PossibleMovesWhite, Alpha, Beta, _, GoodPos, Val);
	staticval(Pos, Val).

%finished, return the best pos found
boundedbest(_, _, [], _, _, Alpha, _, TempPos, TempPos, Alpha) :- !.
%finished all senerios of a certain black move. check if new alpha found and save the pos as temp.
boundedbest(Depth, Pos, [BlackMove | Xs], [], PossibleMovesWhite, Alpha, Beta, TempPos, GoodPos, GoodVal) :-
	((Beta > Alpha) -> boundedbest(Depth, Pos, Xs, PossibleMovesWhite, PossibleMovesWhite, Beta, 1000, BlackMove, GoodPos, GoodVal);
		boundedbest(Depth, Pos, Xs, PossibleMovesWhite, PossibleMovesWhite, Alpha, 1000, TempPos, GoodPos, GoodVal)), !.
		
%for each Black move X white move, get the staticval by calling alphabeta with depth-1 for limitation, and then check if it the worse val of white move with the specific black move.
%if so, change the beta, but if it already worse than alpha, prune the black move.
boundedbest(Depth, Pos, [BlackMove | Xs], [WhiteMove | Ys], PossibleMovesWhite, Alpha, Beta, TempPos, GoodPos, GoodVal) :-
	Depth1 is Depth-1,
	get_new_pos(Pos, BlackMove, WhiteMove, NewPos),
	alphabeta(Depth1, NewPos, Alpha, 1000, _, Val),
	((Val < Beta) -> 
		((Val =< Alpha, TempPos \= BlackMove) -> 
			boundedbest(Depth, Pos, Xs, PossibleMovesWhite, PossibleMovesWhite, Alpha, 1000, TempPos, GoodPos, GoodVal);
			boundedbest(Depth, Pos, [BlackMove | Xs], Ys, PossibleMovesWhite, Alpha, Val, TempPos, GoodPos, GoodVal));
		boundedbest(Depth, Pos, [BlackMove | Xs], Ys, PossibleMovesWhite, Alpha, Beta, TempPos,GoodPos, GoodVal)), !.

%return the value of the hueristic function for a given board
staticval(Pos, Val) :-
	count_pieces(Pos, _, _, _, WPCount), WPCount = 0, Val is 100, !;  %computer won
	count_pieces(Pos, _, BPCount, _, WPCount), WPCount = 1, WPCount = BPCount, Val is 0, !; %stalemate
	count_pieces(Pos, BKCount, BPCount, WKCount, WPCount), Val is (BPCount*2 + BKCount) - (WPCount*2 + WKCount).

%get all posible moves of both sides
moves(Pos, PossibleMovesBlack, PossibleMovesWhite) :- ((ab_check_win(Pos)) -> fail;
	moves_1(Pos, PossibleMovesBlack, PossibleMovesWhite, [1, 1], [], [])).

moves_1(_, PossibleMovesBlack, PossibleMovesWhite, [1, 6], PossibleMovesBlack, PossibleMovesWhite) :- !.
moves_1(Pos, PossibleMovesBlack2, PossibleMovesWhite2, [6, X], PossibleMovesBlack, PossibleMovesWhite) :- 
	X1 is X+1, moves_1(Pos, PossibleMovesBlack2, PossibleMovesWhite2, [1, X1], PossibleMovesBlack, PossibleMovesWhite), !.
moves_1(Pos, PossibleMovesBlack2, PossibleMovesWhite2, [X, Y], PossibleMovesBlack, PossibleMovesWhite) :-
	Y < 6,
	get_piece_in_coordinates(Pos, [X, Y], Piece),
	moves_2(Pos, [X, Y], Piece, NewMovesToAdd), X1 is X+1,
	((string_chars(Piece, PieceChars), get_side(PieceChars, Side), Side = 'B') -> 
		append(NewMovesToAdd, PossibleMovesBlack, PossibleMovesBlack1), moves_1(Pos, PossibleMovesBlack2, PossibleMovesWhite2, [X1, Y], PossibleMovesBlack1, PossibleMovesWhite);
		append(NewMovesToAdd, PossibleMovesWhite, PossibleMovesWhite1), moves_1(Pos, PossibleMovesBlack2, PossibleMovesWhite2, [X1, Y], PossibleMovesBlack, PossibleMovesWhite1)), !.

moves_2(_, _, '_', []) :- !.
moves_2(Pos, [X, Y], 'BK', NewMovesToAdd) :-
	X1 is X+2, Y1 is Y+1,
	X2 is X+2, Y2 is Y-1,
	X3 is X-2, Y3 is Y+1,
	X4 is X-2, Y4 is Y-1,
	X5 is X+1, Y5 is Y+2,
	X6 is X+1, Y6 is Y-2,
	X7 is X-1, Y7 is Y+2,
	X8 is X-1, Y8 is Y-2,
	ab_check_move_legality(Pos, ['B', 'K'], [X, Y], [[X1, Y1], [X2, Y2], [X3, Y3], [X4, Y4], [X5, Y5], [X6, Y6], [X7, Y7], [X8, Y8]], NewMovesToAdd), !.

moves_2(Pos, [X, Y], 'BP', NewMovesToAdd) :-
	X1 is X, Y1 is Y+1,
	X2 is X+1, Y2 is Y+1,
	X3 is X-1, Y3 is Y+1,
	ab_check_move_legality(Pos, ['B', 'P'], [X, Y], [[X1, Y1], [X2, Y2], [X3, Y3]], NewMovesToAdd), !.
	
moves_2(Pos, [X, Y], 'WK', NewMovesToAdd) :-
	X1 is X+2, Y1 is Y+1,
	X2 is X+2, Y2 is Y-1,
	X3 is X-2, Y3 is Y+1,
	X4 is X-2, Y4 is Y-1,
	X5 is X+1, Y5 is Y+2,
	X6 is X+1, Y6 is Y-2,
	X7 is X-1, Y7 is Y+2,
	X8 is X-1, Y8 is Y-2,
	ab_check_move_legality(Pos, ['W', 'K'], [X, Y], [[X1, Y1], [X2, Y2], [X3, Y3], [X4, Y4], [X5, Y5], [X6, Y6], [X7, Y7], [X8, Y8]], NewMovesToAdd), !.
	
moves_2(Pos, [X, Y], 'WP', NewMovesToAdd) :-
	X1 is X, Y1 is Y-1,
	X2 is X+1, Y2 is Y-1,
	X3 is X-1, Y3 is Y-1,
	ab_check_move_legality(Pos, ['W', 'P'], [X, Y], [[X1, Y1], [X2, Y2], [X3, Y3]], NewMovesToAdd), !.

ab_check_move_legality(_, _, _, [], []) :- !.
ab_check_move_legality(Board, [Side, 'K'], [X, Y], [[X1, Y1] | Ys], NewMovesToAdd) :- 
	((X1 >= 1, X1 =< 5, Y1 >= 1, Y1 =< 5, get_piece_in_coordinates(Board, [X1, Y1], Piece), string_chars(Piece, PieceChars), get_side(PieceChars, Side1), Side \= Side1) -> 
		ab_check_move_legality(Board, [Side, 'K'], [X, Y], Ys, NewMovesToAdd1), append([[X, Y, X1, Y1]], NewMovesToAdd1, NewMovesToAdd);
		ab_check_move_legality(Board, [Side, 'K'], [X, Y], Ys, NewMovesToAdd)), !.
ab_check_move_legality(Board, [Side, 'P'], [X, Y], [[X1, Y1] | Ys], NewMovesToAdd) :-
	((X1 >= 1, X1 =< 5, Y1 >= 1, Y1 =< 5, get_piece_in_coordinates(Board, [X1, Y1], Piece), string_chars(Piece, PieceChars), get_side(PieceChars, Side1), Side \= Side1) ->
		((Side1 = '_') ->
			((X \= X1) -> ab_check_move_legality(Board, [Side, 'P'], [X, Y], Ys, NewMovesToAdd);
				ab_check_move_legality(Board, [Side, 'P'], [X, Y], Ys, NewMovesToAdd1), append([[X, Y, X1, Y1]], NewMovesToAdd1, NewMovesToAdd));
			((X \= X1) -> ab_check_move_legality(Board, [Side, 'P'], [X, Y], Ys, NewMovesToAdd1), append([[X, Y, X1, Y1]], NewMovesToAdd1, NewMovesToAdd);
				ab_check_move_legality(Board, [Side, 'P'], [X, Y], Ys, NewMovesToAdd)));
		ab_check_move_legality(Board, [Side, 'P'], [X, Y], Ys, NewMovesToAdd)).

%check what happened in the turn by the roles.
%if they move to the same square and the new square is empty- a horseman captures a footman. Same-type pieces are both removed from the board.
ab_check_turn_logic(Board, [X1, Y1, X3, Y3], PLOldPiece, PLOldPieceChars, _, [X2, Y2, X3, Y3], CMPOldPiece, CMPOldPieceChars, _, NewBoard2) :-
	get_role(PLOldPieceChars, PLOldPiecerole), get_role(CMPOldPieceChars, CMPOldPiecerole),
	((PLOldPiecerole = CMPOldPiecerole) -> change_piece_in_coordinates(Board, ['_', '_', '_'], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard);
		((PLOldPiecerole = 'P', CMPOldPiecerole = 'K') -> change_piece_in_coordinates(Board, ['_', '_', CMPOldPiece], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard);
			change_piece_in_coordinates(Board, ['_', '_', PLOldPiece], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard))),
	ab_check_piece_upgrade(NewBoard, [[X3, Y3]], NewBoard2).

%player move to a square the computer running from.
ab_check_turn_logic(Board, [X1, Y1, X2, Y2], PLOldPiece, _, _, [X2, Y2, X3, Y3], CMPOldPiece, _, _, NewBoard2) :-
	change_piece_in_coordinates(Board, ['_', PLOldPiece, CMPOldPiece], [[X1, Y1], [X2, Y2], [X3, Y3]], NewBoard),
	ab_check_piece_upgrade(NewBoard, [[X2, Y2], [X3, Y3]], NewBoard2).

%computer move to a square the player running from.
ab_check_turn_logic(Board, [X1, Y1, X2, Y2], PLOldPiece, _, _, [X3, Y3, X1, Y1], CMPOldPiece, _, _, NewBoard2) :-
	change_piece_in_coordinates(Board, ['_', PLOldPiece, CMPOldPiece], [[X3, Y3], [X2, Y2], [X1, Y1]], NewBoard),
	ab_check_piece_upgrade(NewBoard, [[X2, Y2], [X1, Y1]], NewBoard2).
	
%no overlapping coordinates.
ab_check_turn_logic(Board, [X1, Y1, X2, Y2], PLOldPiece, _, _, [X3, Y3, X4, Y4], CMPOldPiece, _, _, NewBoard2) :-
	change_piece_in_coordinates(Board, ['_', '_', PLOldPiece, CMPOldPiece], [[X1, Y1], [X3, Y3], [X2, Y2], [X4, Y4]], NewBoard),
	ab_check_piece_upgrade(NewBoard, [[X2, Y2], [X4, Y4]], NewBoard2).

ab_check_piece_upgrade(Board, [], Board) :- !.
ab_check_piece_upgrade(Board, [X | Zs], NewBoard) :-
	get_piece_in_coordinates(Board, X, Piece), string_chars(Piece, PieceChars), get_side(PieceChars, PieceSide), get_role(PieceChars, Piecerole),
	check_piece_upgrade_1(Board, X, PieceSide, Piecerole, NewBoard1),
	check_piece_upgrade(NewBoard1, Zs, NewBoard), !.
	
ab_check_win(Board) :-	
	%check if there's no more pawn for either side
	count_piece(Board, 'WP', PlayerPawnsCount), count_piece(Board, 'BP', ComputerPawnsCount),
	ab_check_win_1(PlayerPawnsCount, ComputerPawnsCount).

%tie
ab_check_win_1(0, 0).

%Player Won
ab_check_win_1(_, 0).
	
%Computer Won
ab_check_win_1(0, _).
	
%stalemate
ab_check_win_1(1, 1).

%return new board for given a move of both sides.
get_new_pos(Pos, [X, Y, X1, Y1], [X2, Y2, X3, Y3], NewBoard) :-
	get_piece_in_coordinates(Pos, [X2, Y2, X3, Y3], PLOldPiece, PLNewPiece),
	get_piece_in_coordinates(Pos, [X, Y, X1, Y1], CMPOldPiece, CMPNewPiece),
	string_chars(PLOldPiece, PLOldPieceChars),
	string_chars(PLNewPiece, PLNewPieceChars),
	string_chars(CMPOldPiece, CMPOldPieceChars),
	string_chars(CMPNewPiece, CMPNewPieceChars),
	
	ab_check_turn_logic(Pos, [X2, Y2, X3, Y3], PLOldPiece, PLOldPieceChars, PLNewPieceChars, [X, Y, X1, Y1], CMPOldPiece, CMPOldPieceChars, CMPNewPieceChars, NewBoard).

	