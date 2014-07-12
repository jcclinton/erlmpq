-module(util).

-export([get_block_at_offset/2, get_hash_table_at_offset/2, get_map_at_offset/2]).
-export([map_size/0, hash_table_size/0, header_size/0, header_ex_size/0, block_size/0, block_ex_size/0]).
-export([add_32bit/1]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


get_map_at_offset(Offset, Map) ->
	<<_:Offset/binary, BlockTableIndices?L, BlockTableDiff?L, _/binary>> = Map,
	#map{block_table_indices=BlockTableIndices, block_table_diff=BlockTableDiff}.


get_hash_table_at_offset(Offset, Hashes) ->
	<<_:Offset/binary, HashA?L, HashB?L, Locale?W, Platform?W, BlockTableIndex?L, _/binary>> = Hashes,
	#hash{hash_a=HashA, hash_b=HashB, locale=Locale, platform=Platform, block_table_index=BlockTableIndex}.


%% gets flag for block at given offset of blocks blob
get_block_at_offset(Offset, Blocks) ->
	<<_:Offset/binary, BlockOffset?L, PackedSize?L, UnpackedSize?L, Flags?L, _/binary>> = Blocks,
	#block{offset=BlockOffset, packed_size=PackedSize, unpacked_size=UnpackedSize, flags=Flags}.



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
