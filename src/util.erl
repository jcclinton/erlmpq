-module(util).

-export([map_size/0, hash_table_size/0, header_size/0, header_ex_size/0, block_size/0, block_ex_size/0]).
-export([add_32bit/1]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").



%% add numbers in list, keep limited to 32 bits
add_32bit(L) ->
	lists:foldl(fun(N, Sum) ->
		(N + Sum) band 16#FFFFFFFF
	end, 0, L).



map_size() ->
	4 + 4.

hash_table_size() ->
	4 + 4 + 2 + 2 + 4.

header_size() ->
	4 + 4 + 4 + 2 + 2 + 4 + 4 + 4 + 4.

header_ex_size() ->
	8 + 2 + 2.

block_size() ->
	4 + 4 + 4 + 4.

block_ex_size() ->
	2.
