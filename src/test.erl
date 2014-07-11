-module(test).

-export([]).
-compile([export_all]).



run() ->
	%Files = ["dbc", "terrain", "patch", "patch-2"],
	File = "dbc",
	Dir = "/Users/jclinton/Downloads/torrents/world_of_warcraft_classic/Data/",
	Suffix = ".MPQ",
	Filename = Dir ++ File ++ Suffix,
	{ok, Archive} = mpq:archive_open(Filename),
	Files = get_file_list_to(Archive),
	mpq:archive_close(Archive),
	ok.


get_file_list_to(Archive) ->
	FileNumber = mpq:file_number(Archive, "(listfile)"),
	ok.

extract_file() ->
	ok.
