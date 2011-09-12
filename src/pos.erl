-module(pos).

-include("include/pos.hrl").

-export([add/2]).

add(#pos{x=X1, y=Y1}, #pos{x=X2, y=Y2}) ->
  #pos{x=(X1+X2), y=(Y1+Y2)}.
