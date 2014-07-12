-module(archive).

-export([get_map_at_offset/2, get_hash_table_at_offset/2, get_block_at_offset/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


get_map_at_offset(Map, OffsetIn) ->
	Offset = OffsetIn * util:map_size(),
	<<_:Offset/binary, BlockTableIndices?L, BlockTableDiff?L, _/binary>> = Map,
	#map{block_table_indices=BlockTableIndices, block_table_diff=BlockTableDiff}.


get_hash_table_at_offset(Hashes, OffsetIn) ->
	Offset = OffsetIn * util:hash_table_size(),
	<<_:Offset/binary, HashA?L, HashB?L, Locale?W, Platform?W, BlockTableIndex?L, _/binary>> = Hashes,
	#hash{hash_a=HashA, hash_b=HashB, locale=Locale, platform=Platform, block_table_index=BlockTableIndex}.


%% gets flag for block at given offset of blocks blob
get_block_at_offset(Blocks, OffsetIn) ->
	Offset = OffsetIn * util:block_size(),
	<<_:Offset/binary, BlockOffset?L, PackedSize?L, UnpackedSize?L, Flags?L, _/binary>> = Blocks,
	#block{offset=BlockOffset, packed_size=PackedSize, unpacked_size=UnpackedSize, flags=Flags}.
