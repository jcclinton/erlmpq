-module(mpq).

-export([archive_open/2, archive_close/1]).
-export([test/0]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").






test() ->
	%Files = ["dbc", "terrain", "patch", "patch-2"],
	File = "dbc",
	Dir = "/Users/jclinton/Downloads/torrents/world_of_warcraft_classic/Data/",
	Suffix = ".MPQ",
	Filename = Dir ++ File ++ Suffix,
	{ok, Archive} = archive_open(Filename, -1),
	archive_close(Archive),
	ok.



archive_close(Archive) ->
	file:close(Archive#archive.fd).


archive_open(Filename, Offset) ->
	{ArchiveOffset, HeaderSearch} = if Offset == -1 -> {0, true};
		true -> {Offset, false}
	end,
	{ok, Fd} = file:open(Filename, [read, raw, binary]),
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
