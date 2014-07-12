-module(test).

-export([]).
-compile([export_all]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


encrypt() ->
	HashBin = <<100?L, 200?L, 99?L, 1000?L>>,
	Seed = crypto:hash_string("(hash table)", 16#300),
	Size = byte_size(HashBin),
	io:format("input: ~p~n", [HashBin]),
	Encrypted = crypto:encrypt_block(HashBin, Size, Seed),
	io:format("encrypted: ~p~n", [Encrypted]),
	HashBin = crypto:decrypt_block(Encrypted, Size, Seed),
	io:format("decrypted: ~p~n", [HashBin]),
	ok.

	
decrypt() ->
	HashBin = <<100?L, 200?L, 99?L, 1000?L>>,
	Seed = crypto:hash_string("(hash table)", 16#300),
	Size = byte_size(HashBin),
	io:format("input: ~p~nSeed: ~p~nsize: ~p~n", [HashBin, Seed, Size]),
	<<DecryptedHashes?L, DH2?L, DH3?L, DH4?L>> = crypto:decrypt_block(HashBin, Size, Seed),
	io:format("decrypt: ~p~n", [DecryptedHashes]),
	io:format("decrypt2: ~p~n", [DH2]),
	io:format("decrypt3: ~p~n", [DH3]),
	io:format("decrypt4: ~p~n", [DH4]),
	ok.


run() ->
	%Files = ["dbc", "terrain", "patch", "patch-2"],
	File = "dbc",
	Dir = "/Users/jclinton/Downloads/torrents/world_of_warcraft_classic/Data/",
	Suffix = ".MPQ",
	Filename = Dir ++ File ++ Suffix,
	{ok, Archive} = mpq:archive_open(Filename),
	%output_archive(Archive),
	Files = get_file_list_to(Archive),
	mpq:archive_close(Archive),
	ok.

output_archive(Archive) ->
	lists:foreach(fun(I) ->
		Hashes = Archive#archive.hash,
		Hash = archive:get_hash_table_at_offset(Hashes, I),
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



get_file_list_to(Archive) ->
	FileNumber = mpq:file_number(Archive, "(listfile)"),
	ok.

extract_file() ->
	ok.
