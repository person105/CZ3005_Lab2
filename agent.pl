/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  CZ3005 Lab Assignment 2

- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- dynamic([
    %  where X,Y are integers, returns true if the Agent has visited relative position (X,Y).
    % Otherwise it returns false.
    visited/2,

    % where X,Y are integers, returns true if the Agent knows (or reasons) that the Wumpus
    % may inhabit relative position (X,Y). Otherwise it returns false. Notice that there may be more than
    % one positions where true is returned. It is also possible that all positions return false, either because
    % there is no Wumpus or no relevant information exists.
    wumpus/2,

    % where X, Y are integers, returns true if the Agent knows (or reasons) that a Confundus
    % Portal may inhabit relative position (X,Y). Otherwise false is returned.
    confundus/2,

    % where X, Y are integers, returns true if the Agent knows (or reasons that) it will experience
    % a Tingle indicator beging on at the relative position (X,Y). Otherwise, false is returned.
    tingle/2,

    % where X, Y are integers, returns true if the Agent knows (or reasons that) it will experience
    % Glitter indicator being on at the relative position (X,Y). Otherwise false is returned. Notice that
    % this knowledge changes, once the Agent picks up the coin.
    glitter/2,

    % where X,Y are integers, returns true if the Agent knows it will experience Stench indicator
    % being on at the relative position (X,Y). Otherwise, false is returned. Notice that this knowledge
    % changes, once the Wumpus is killed.
    stench/2,

    % where X,Y are integers, returns true if the Agent knows or can reason that the relative
    % position (X,Y) contains neither a Wumpus nor a Confundus Portal.
    safe/2,
    wall/2,
    gold/2,
    current/3,
    coinsObtained/1
]).


% 
% Main Code
% 
% 

% Goal: The goal of the Agent is to find all coins that are accessible and return to the initial position.

maxNumberOfMoves(40).
maxNumberOfAsPlanned(20).


%Removes all objects from world
clearWorld :-
    retractall(wall(_,_)),
    retractall(gold(_,_)),
    retractall(coinsObtained(_)),
    retractall(current(_,_,_)),
    retractall(wumpus(_,_)),
    retractall(confundus(_,_)),
    retractall(visited(_,_)),
    retractall(tingle(_,_)),
    retractall(glitter(_,_)),
    retractall(stench(_,_)).

%Builds 7x6 outer walls to limit world
buildWalls :-
    asserta(wall(0,0)),
    asserta(wall(0,1)),
    asserta(wall(0,2)),
    asserta(wall(0,3)),
    asserta(wall(0,4)),
    asserta(wall(0,5)),
    asserta(wall(0,6)),
    
    asserta(wall(1,6)),
    asserta(wall(2,6)),
    asserta(wall(3,6)),
    asserta(wall(4,6)),
    asserta(wall(5,6)),
    asserta(wall(6,6)),
    asserta(wall(7,6)),

    asserta(wall(7,5)),
    asserta(wall(7,4)),
    asserta(wall(7,3)),
    asserta(wall(7,2)),
    asserta(wall(7,1)),

    asserta(wall(7,0)),
    asserta(wall(6,0)),
    asserta(wall(5,0)),
    asserta(wall(4,0)),
    asserta(wall(3,0)),
    asserta(wall(2,0)),
    asserta(wall(1,0)).


% implement Agent’s reset due to arriving into a cell inhabited by the Wumpus.
reborn :-
    clearWorld,
    buildWalls,
    asserta(gold(2,3)),
    asserta(wumpus(1,3)),
    asserta(confundus(3,1)),
    asserta(confundus(3,3)),
    asserta(confundus(4,4)),
    asserta(coinsObtained(0)),
    setCurrent(1,1,rnorth),
    setSafe(1,1),
    format('~n~n~n   Playing on world ~d ~n~n~n', 1).
    


agent(current(X,Y,D),do(A,L)) :-
    current(X0,Y0,D0),  %get agent's last position
    (
        (A = turnleft, X = X0, Y = Y0, ((D0 = rnorth, D = rwest)
                                        ;
                                        (D0 = reast, D = rnorth)
                                        ;
                                        (D0 = rsouth, D = reast)
                                        ;
                                        (D0 = rwest, D = rsouth)))
                                        ; %turn left
        
        (A = turnright, X = X0, Y = Y0, ((D0 = rnorth, D = reast)
                                        ;
                                        (D0 = reast, D = rsouth)
                                        ;
                                        (D0 = rsouth, D = rwest)
                                        ;
                                        (D0 = rwest, D = rnorth)))
                                        ; %turn right

        (A = moveforward, D = D0, getForwardRoom((X0,Y0),D0,(XN,YN)), 
                                        !, %if successful in getting forward room, break.
                                        (wall(XN,YN) -> setCurrent(X0,Y0,D0) %if hit wall- bump
                                        ; confundus(XN,YN) -> reposition([on,off,off,off,off,off]) 
                                        ; wumpus(XN,YN) -> endGame
                                        ; setCurrent(XN,YN,D0) %else, go forward
                                        )) 
                                        ;
        (A = pickup, X = X0, Y = Y0, getGold)
        %                                 ;
        % (A = shoot)
    ) ;
    (
        %position doesnt change
        agent(Current,Movement), %position of agent now same as before
        \+A = turnleft,
        \+A = turnright,
        \+A = moveforward
    
    ).


% returns true if the Agent has the arrow, and begins to return false after shoot action has been
% executed by the Agent.
% hasarrow :-.
    

%  implement Agent’s reasoning response to executing action A and receiving sensory input L.
%  A constants are {shoot,moveforward,turnleft,turnright,pickup}
%  Sensory input is a list of six indicators, one for each percept: Confounded, Stench, Tingle, Glitter,
%  Bump, Scream. In that order. Each indicator can have one of the two values {on, off}.
move(A,L) :-
    maplist(=, L, [Confounded, Stench, Tingle, Glitter, Bump, Scream]), 
    (   A = moveforward -> (Bump = off, agent(_,do(moveforward,L))) %Does Forward
    ;   A = turnleft -> (agent(current(X,Y,D),do(turnleft,L)), setCurrent(X,Y,D)) %Does Left
    ;   A = turnright -> (agent(current(X,Y,D),do(turnright,L)), setCurrent(X,Y,D)) %Does Right
    ;   A = pickup -> (Glitter = on, agent(current(X,Y,D),do(pickup,L)), retract(gold(X,Y))) %Does Grab
    % ;   A = shoot -> () %Does Shoot
    ).


% implements Agent’s reset due to game start or arrival to a cell inhabited by a Confundus
% Portal. The argument includes the initial sensory information. The format of L is the same as with the
% move/2 function. Confundus indicator should be on.
% The Agent is randomly relocated to a
% safe location in the Wumpus World. The relative position is reset to (0,0) and relative orientation is
% reset to become the Relative North. All memory of previous steps and sensory readings is lost, except
% the fact of existence of uncollected coins, whether the Wumpus is alive and whether the Agent has the
% arrow.
reposition(L) :- 
    format('Agent got confounded!'),nl,
    format('repositioning...'),nl,
    maplist(=, L, [Confounded, Stench, Tingle, Glitter, Bump, Scream]), 
    retractall(visited(_,_)),
    retractall(tingle(_,_)),
    retractall(glitter(_,_)),
    retractall(stench(_,_)),
    setCurrent(1,1,rnorth).



% is true if the list L contains a sequence of actions that leads the Agent to inhabit a safe and
% non-visited location.
% if there are no more safely accessible un-visited portions of the map left and there are no Coins on the
% explored portion of the map, the explore(L) should return true on an action sequence that returns the
% Agent to the relative Origin.
explore(L) :- 
    current(X,Y,D), %get start position
    (\+L = [] -> checkActions(L,X,Y,D)
    ; L = s0 -> generateActions(X,Y,D,L)
    ).



endGame :- 
    format('Agent died!'),nl,
    clearWorld.


% 
%   Helper Methods
%    
% 

%Returns room in front of another in a certain direction
getForwardRoom((X0,Y0),D0,(XN,YN)) :-
    (D0 = rnorth, XN is X0, YN is Y0+1)
    ;
    (D0 = reast, XN is X0+1, YN is Y0)
    ;
    (D0 = rsouth, XN is X0, YN is Y0-1)
    ;
    (D0 = rwest, XN is X0-1, YN is Y0).


setCurrent(X,Y,D) :-
    retractall(current(_,_,_)),
    asserta(current(X,Y,D)).


setVisited(X,Y) :-
    asserta(visited(X,Y)).


setSafe(X,Y) :-
    asserta(safe(X,Y)).


getGold :-
    format('Agent picked up a gold coin.'),nl,
    coinsObtained(X),
    XN is X+1,
    asserta(coinsObtained(XN)).


checkActions([],X,Y,D).
checkActions([H|T],X,Y,D) :-
    (H = moveforward -> (getForwardRoom((X,Y),D,(XN,YN)), \+wumpus(XN,YN), \+confundus(XN,YN), checkActions(T,XN,YN,D))
    
    ; H = turnleft -> (((D = rnorth, DN = rwest)
                        ;
                        (D = reast, DN = rnorth)
                        ;
                        (D = rsouth, DN = reast)
                        ;
                        (D = rwest, DN = rsouth)), checkActions(T,X,Y,DN))

    ; H = turnright -> (((D = rnorth, DN = reast)
                        ;
                        (D = reast, DN = rsouth)
                        ;
                        (D = rsouth, DN = rwest)
                        ;
                        (D = rwest, DN = rnorth)), checkActions(T,X,Y,DN))
    ).


generateActions(X,Y,D,L) :- .
    % explore([moveforward]),




% %Returns list of all adjacent rooms
% getAdjacentRooms((X,Y),L) :-
%     XL is X-1,
%     XR is X+1,
%     YD is Y-1,
%     YU is Y+1,
%     append([(XL,Y), (XR,Y), (X,YU), (X,YD)],[],L).

