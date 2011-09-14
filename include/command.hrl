-ifndef(command_h).
-define(command_h, ok).

-include("include/pos.hrl").

-record(move_command, {unit_id, pos}).

-record(command, {id, command}).

-endif.
