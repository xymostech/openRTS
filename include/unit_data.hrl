-ifndef(unit_data_h).
-define(unit_data_h, ok).

-record(attack, {range, damage, turns}).
-record(spawn, {id, turns}).
-record(u_data, {id, speed=0, spawns=[], attack=#attack{}}).

-endif.
