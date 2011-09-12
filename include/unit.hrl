-include("include/pos.hrl").

-record(unit, {id, pos=#pos{}, owner}).
-record(command, {id, dir=#pos{}, steps}).
