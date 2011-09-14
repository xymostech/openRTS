-module(pos).

-compile({no_auto_import, [length/1]}).

-include("include/pos.hrl").

-export([add/2, length/1, unit/1, mult/2]).

add(#pos{x=X1, y=Y1}, #pos{x=X2, y=Y2}) ->
  #pos{x=(X1+X2), y=(Y1+Y2)}.

length(#pos{x=X, y=Y}) ->
  math:sqrt(X*X+Y*Y).

unit(#pos{x=X, y=Y}=Pos) ->
  Len = length(Pos),
  #pos{x=(X/Len), y=(Y/Len)}.

mult(#pos{x=X, y=Y}, M) ->
  #pos{x=(X*M), y=(Y*M)}.
