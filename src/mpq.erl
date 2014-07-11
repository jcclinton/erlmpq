-module(mpq).

-export([archive_open/1, archive_open/2, archive_close/1]).
-export([file_number/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").




archive_close(Archive) ->
	file:close(Archive#archive.fd).


archive_open(Filename) -> archive_open(Filename, -1).
archive_open(Filename, Offset) ->
	{ArchiveOffset, HeaderSearch} = if Offset == -1 -> {0, true};
		true -> {Offset, false}
	end,
	{ok, Fd} = file:open(Filename, [read, binary]),
	InitialArchive = #archive{fd=Fd},
	Archive = archive_builder:add_header_to_archive(InitialArchive, ArchiveOffset, HeaderSearch),

	BuildFuns = [
		fun archive_builder:add_header_ex_to_archive/1,
		fun archive_builder:add_hash_table_to_archive/1,
		fun archive_builder:add_block_to_archive/1,
		fun archive_builder:add_block_ex_to_archive/1,
		fun archive_builder:add_map_to_archive/1
	],
	ArchiveOut = lists:foldl(fun(Fun, Arch) ->
		Fun(Arch)
	end, Archive, BuildFuns),
	{ok, ArchiveOut}.


file_number(Archive, Filename) ->
	HTCount = Archive#archive.header#header.hash_table_count,
	Hash1 = crypto:hash_string(Filename, 16#0) band (HTCount - 1),
	Hash2 = crypto:hash_string(Filename, 16#100),
	Hash3 = crypto:hash_string(Filename, 16#200),
	Number = loop_hash(Archive, Hash1, Hash1, Hash2, Hash3, HTCount),
	io:format("file number: ~p~n", [Number]),
	Number.


loop_hash(Archive, I, Hash1, Hash2, Hash3, HTCount) ->
	Hashes = Archive#archive.hash,
	Hash = util:get_hash_table_at_offset(I, Hashes),
	HashA = Hash#hash.hash_a,
	HashB = Hash#hash.hash_b,
	BlockTableIndex = Hash#hash.block_table_index,
	%io:format("I: ~p~nblocktableindex: ~p~n~n", [I, BlockTableIndex]),
	HasCycled = ((I + 1) band (HTCount - 1)) == Hash1,
	if BlockTableIndex == ?HASH_FREE orelse HasCycled -> 0;
		true ->
			if HashA == Hash2 andalso HashB == Hash3 ->
					Maps = Archive#archive.map,
					Map = util:get_map_at_offset(BlockTableIndex, Maps),
					Diff = Map#map.block_table_diff,
					BlockTableIndex - Diff;
				true ->
					Next = (I + 1) band (HTCount -1),
					loop_hash(Archive, Next, Hash1, Hash2, Hash3, HTCount)
			end
	end.
