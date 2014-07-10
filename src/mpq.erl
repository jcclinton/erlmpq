-module(mpq).

-export([]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").






test() ->
	%Files = ["dbc", "terrain", "patch", "patch-2"],
	File = "dbc",
	Dir = "/Users/jclinton/Downloads/torrents/world_of_warcraft_classic/Data/",
	Suffix = ".MPQ",
	Filename = Dir ++ File ++ Suffix,
	case archive_open(Filename, -1) of
		{error, Error} -> {error, Error};
		{ok, Archive} -> {ok, Archive}
	end.


archive_open(Filename, Offset) ->
	{ArchiveOffset, HeaderSearch} = if Offset == -1 -> {0, true};
		true -> {Offset, false}
	end,
	{ok, Fd} = file:open(Filename, [read, raw, binary]),
	InitialArchive = #archive{fd=Fd},
	Archive = handle(InitialArchive),

	file:close(Fd),
	{ok, Archive}.


handle(ArchiveIn) ->
	ArchiveIn.
