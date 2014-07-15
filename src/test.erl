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


run_all() ->
	Files = ["dbc", "terrain", "patch", "patch-2"],
	lists:foreach(fun(File) ->
		io:format("running ~p~n", [File]),
		run_internal(File)
	end, Files).


run() ->
	File = "terrain",
	run_internal(File).

run_internal(File) ->
	Dir = "/Users/jclinton/Downloads/torrents/world_of_warcraft_classic/Data/",
	OutDir = "/Users/jclinton/projects/erlang/erlmpq/data",
	Suffix = ".MPQ",
	Filename = Dir ++ File ++ Suffix,
	{ok, Archive} = mpq:archive_open(Filename),
	%output_archive(Archive),
	{ArchiveOut, FileList} = get_file_list_to(Archive, <<".dbc">>),
	extract_dbc_files(ArchiveOut, FileList, OutDir),
	mpq:archive_close(ArchiveOut),
	ok.

archive() ->
	File = "dbc",
	Dir = "/Users/jclinton/Downloads/torrents/world_of_warcraft_classic/Data/",
	Suffix = ".MPQ",
	Filename = Dir ++ File ++ Suffix,
	{ok, Archive} = mpq:archive_open(Filename),
	Archive.

create_files(Archive) ->
	OutDir = "/Users/jclinton/projects/erlang/erlmpq/data",
	{ArchiveOut, FileList} = get_file_list_to(Archive, <<".dbc">>),
	extract_dbc_files(ArchiveOut, FileList, OutDir),
	ok.


extract_dbc(Archive) ->
	FileList = [<<"DBFilesClient\\AnimationData.dbc">>],
	OutDir = "/Users/jclinton/projects/erlang/erlmpq/data",
	_ = extract_dbc_files(Archive, FileList, OutDir),
	ok.


extract_dbc_files(Archive, FileList, Dir) ->
	lists:foldl(fun(FilenameIn, ArchiveIn) ->
		FilenameBin = binary:replace(FilenameIn, <<"\\">>, <<"/">>, [global]),
		Filename = binary_to_list(FilenameBin),
		Name = Dir ++ "/" ++ Filename,
		%io:format("filename: ~p~n", [Name]),
		util:make_dir(Dir, Filename),
		{ok, Fd} = file:open(Name, [write, binary]),
		FileNumber = mpq:file_number(ArchiveIn, binary_to_list(FilenameIn)),
		%UnpackedSize = mpq:file_unpacked_size(Archive, FileNumber),
		%io:format("filenumber: ~p~n", [FileNumber]),
		%io:format("unpacked size: ~p~n", [UnpackedSize]),
		{ArchiveOut, Buffer} = mpq:file_read(ArchiveIn, FileNumber),
		%io:format("buffer: ~p~n", [Buffer]),
		io:format("buffer size: ~p~n", [byte_size(Buffer)]),

		ok = file:write(Fd, Buffer),
		file:close(Fd),
		ArchiveOut
	end, Archive, FileList).



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



get_file_list_to(Archive, Ext) ->
	FileNumber = mpq:file_number(Archive, "(listfile)"),
	%io:format("file number: ~p~n", [FileNumber]),
	{ArchiveOut, Buffer} = mpq:file_read(Archive, FileNumber),
	FileList1 = binary:split(Buffer, <<"\r\n">>, [global]),
	FileList = lists:filter(fun(Name) ->
		if Name == <<"">> -> false;
			Name /= <<"">> ->
				% check if it has the correct extension
				NameExt = binary:part(Name, {byte_size(Name), byte_size(Ext) * -1}),
				if NameExt == Ext -> true;
					NameExt /= Ext -> false
				end
		end
	end, FileList1),
	%io:format("buffer: ~p~n", [Buffer]),
	%io:format("List: ~p~n", [FileList]),
	{ArchiveOut, FileList}.

extract_file() ->
	ok.
