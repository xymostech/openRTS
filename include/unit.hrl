-ifndef(unit_h).
-define(unit_h, ok).

-include("include/pos.hrl").

-record(unit, {id, type_id, pos=#pos{}, health=0, owner}).

-endif.
