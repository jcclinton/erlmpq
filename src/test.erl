-module(test).

-export([]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


get_output_dir() ->
	"/Users/jclinton/projects/erlang/erlmpq/data".

get_data_dir() ->
	"/Users/jclinton/Downloads/torrents/world_of_warcraft_classic/Data/".

run_all() ->
	Files = ["dbc", "terrain", "patch", "patch-2"],
	DataDir = get_data_dir(),
	OutputDir = get_output_dir(),
	tools:extract_dbc_files(Files, OutputDir, DataDir).


run() ->
	File = "dbc",
	DataDir = get_data_dir(),
	OutputDir = get_output_dir(),
	tools:extract_dbc_file(File, OutputDir, DataDir).


archive() ->
	File = "dbc",
	Dir = get_data_dir(),
	Suffix = ".MPQ",
	Filename = Dir ++ File ++ Suffix,
	{ok, Archive} = mpq:archive_open(Filename),
	Archive.

get_filelist(Archive) ->
	{_ArchiveOut, FileList} = tools:get_file_list_to(Archive, <<".dbc">>),
	FileList.




output_archive(Archive) ->
	lists:foreach(fun(I) ->
		Hashes = Archive#archive.hash,
		_Hash = archive:get_hash_table_at_offset(Hashes, I),
		%io:format("locale: ~p~n", [Hash#hash.locale]),
		ok
	end, lists:seq(0, Archive#archive.header#header.hash_table_count-1)),
		io:format("~n"),
	lists:foreach(fun(I) ->
		Blocks = Archive#archive.block,
		Block = archive:get_block_at_offset(Blocks, I),
		io:format("offset: ~p~n", [Block#block.offset]),
		io:format("packed size: ~p~n", [Block#block.packed_size]),
		io:format("unpacked size: ~p~n", [Block#block.unpacked_size]),
		io:format("flag: ~p~n~n", [Block#block.flags]),
		ok
	%end, lists:seq(0, Archive#archive.header#header.block_table_count-1)),
	end, lists:seq(0, 0)),
		%io:format("blocks: ~p~n", [Archive#archive.block]),
		ok.
