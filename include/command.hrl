-ifndef(command_h).
-define(command_h, ok).

-include("include/pos.hrl").

-record(move_command, {pos}).
-record(spawn_command, {spawn_type, turns}).
-record(attack_command, {att_id, turns}).

-record(command, {id, unit_id, command}).

-endif.
