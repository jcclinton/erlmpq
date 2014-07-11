-module(util).

-export([get_block_flags/2]).
-export([map_size/0, hash_table_size/0, header_size/0, header_ex_size/0, block_size/0, block_ex_size/0]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


%% gets flag for block at given offset of blocks blob
get_block_flags(Offset, Blocks) ->
	Size = block_size() * 8,
	SizeSansFlags = Size - 32,
	<<_:Offset/binary, _:SizeSansFlags/integer, Flags?L, _/binary>> = Blocks,
	Flags.



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
