-ifndef(unit_h).
-define(unit_h, ok).

-include("include/pos.hrl").

-record(unit, {id, pos=#pos{}, owner}).

-endif.
