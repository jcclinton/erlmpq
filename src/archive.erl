-module(archive).

-export([get_map_at_offset/2, get_hash_table_at_offset/2, get_block_at_offset/2, get_block_ex_at_offset/2, get_file_at_offset/2, get_file_packed_offset_at_offset/2]).
-export([update_block_at_offset/3, update_file_at_offset/3]).
-export([open/2, close/1]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


open(Filename, Offset) ->
	{ArchiveOffset, HeaderSearch} = if Offset == -1 -> {0, true};
		true -> {Offset, false}
	end,
	{ok, Fd} = case file:open(Filename, [read, binary]) of
		{error, Error} -> throw(Error);
		Res -> Res
	end,
	InitialArchive = #archive{fd=Fd},
	Archive = archive_builder:add_header_to_archive(InitialArchive, ArchiveOffset, HeaderSearch),

	BuildFuns = [
		fun archive_builder:add_header_ex_to_archive/1,
		fun archive_builder:add_hash_table_to_archive/1,
		fun archive_builder:add_block_to_archive/1,
		fun archive_builder:add_block_ex_to_archive/1,
		fun archive_builder:add_map_to_archive/1,
		fun archive_builder:add_file_to_archive/1
	],
	ArchiveOut = lists:foldl(fun(Fun, Arch) ->
		Fun(Arch)
	end, Archive, BuildFuns),
	{ok, ArchiveOut}.


close(Archive) ->
	case file:close(Archive#archive.fd) of
		ok -> ok;
		{error, Error} -> throw(Error)
	end.


update_block_at_offset(BlocksBin, Block, OffsetIn) ->
	Offset = OffsetIn * util:block_size(),
	BlockOffset = Block#block.offset,
	PackedSize = Block#block.packed_size,
	UnpackedSize = Block#block.unpacked_size,
	Flags = Block#block.flags,
	<<Head:Offset/binary, _?L, _?L, _?L, _?L, Rest/binary>> = BlocksBin,
	<<Head/binary, BlockOffset?L, PackedSize?L, UnpackedSize?L, Flags?L, Rest/binary>>.


update_file_at_offset(Files, File, OffsetIn) ->
	%Offset = OffsetIn * util:file_size(),
	%Seed = File#file.seed,
	%PackedOffset = File#file.packed_offset,
	%OpenCount = File#file.open_count,
	%<<Head:Offset/binary, _OldSeed?L, _OldPackedOffset?L, _OldOpenCount?L, Rest/binary>> = FilesBin,
	%<<Head/binary, Seed?L, PackedOffset?L, OpenCount?L, Rest/binary>>.
	{Head, [_|Tail]} = lists:split(OffsetIn, Files),
	Head ++ [File|Tail].

get_file_packed_offset_at_offset(PackedOffset, OffsetIn) ->
	Offset = OffsetIn * util:file_packed_offset_size(),
	Size = byte_size(PackedOffset),
	if Offset >= Size -> 0;
		true ->
			<<_:Offset/binary, Val?L, _/binary>> = PackedOffset,
			Val
	end.

get_file_at_offset(File, OffsetIn) ->
	%Offset = OffsetIn * util:file_size(),
	%<<_:Offset/binary, Seed?L, PackedOffset?L, OpenCount?L, _/binary>> = File,
	%#file{seed=Seed, packed_offset=PackedOffset, open_count=OpenCount}.
	lists:nth(OffsetIn + 1, File).

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

get_block_ex_at_offset(BlockExs, OffsetIn) ->
	OffsetOut = if BlockExs == undefined -> 0;
		true ->
			Offset = OffsetIn * util:block_ex_size(),
			<<_:Offset/binary, OffsetHigh?W, _/binary>> = BlockExs,
			OffsetHigh
	end,
	#block_ex{offset_high=OffsetOut}.
