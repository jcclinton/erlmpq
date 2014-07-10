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
	end,
	ok.


archive_open(Filename, Offset) ->
	{ArchiveOffset, HeaderSearch} = if Offset == -1 -> {0, true};
		true -> {Offset, false}
	end,
	{ok, Fd} = file:open(Filename, [read, raw, binary]),
	InitialArchive = #archive{fd=Fd},
	Archive = add_header_to_archive(InitialArchive, ArchiveOffset, HeaderSearch),

	Archive2 = if Archive#archive.header#header.version == ?LIBMPQ_ARCHIVE_VERSION_TWO ->
			add_header_ex_to_archive(Archive);
		true -> Archive
	end,

	Archive3 = add_hash_table_to_archive(Archive2),
			

	file:close(Fd),
	{ok, Archive}.


add_hash_table_to_archive(Archive) ->
	ArchiveOffset = Archive#archive.archive_offset,
	HashTableOffset = Archive#archive.header#header.hash_table_offset,
	HashTableOffsetHigh = Archive#archive.header_ex#header_ex.hash_table_offset_high bsl 32,
	Offset = ArchiveOffset + HashTableOffset + HashTableOffsetHigh,

	TableCount = Archive#archive.header#header.hash_table_count,
	Size = hash_table_size() * TableCount,

	{ok, HashTables} = file:pread(Archive#archive.fd, Offset, Size),
	%allHashes is a list of hash records
	AllHashes = get_all_hashes(HashTables),
	Archive#archive{hash=AllHashes}.


get_all_hashes(HashTables) ->
	get_all_hashes(HashTables, []).

get_all_hashes(<<>>, Acc) -> Acc;
get_all_hashes(Bin, Acc) ->
	<<HashA?L, HashB?L, Locale?W, Platform?W, BlockTableIndex?L, Rest/binary>> = Bin,
	HashRecord = #hash{
		hash_a = HashA,
		hash_b = HashB,
		locale = Locale,
		platform = Platform,
		block_table_index = BlockTableIndex
	},
	get_all_hashes(Rest, [HashRecord|Acc]).



add_header_ex_to_archive(Archive) ->
		ExOffset = header_size() + Archive#archive.archive_offset,
		ExSize = header_ex_size(),
		{ok, HeaderEx} = file:pread(Archive#archive.fd, ExOffset, ExSize),
		<<ExtendedOffset?Q, HashTableOffsetHigh?W, BlockTableOffsetHigh?W>> = HeaderEx,
		HeaderExRecord = #header_ex{
			extended_offset = ExtendedOffset,
			hash_table_offset_high = HashTableOffsetHigh,
			block_table_offset_high = BlockTableOffsetHigh
		},
		Archive#archive{header_ex=HeaderExRecord}.


add_header_to_archive(ArchiveIn, ArchiveOffset, HeaderSearch) ->
	case extract_header(ArchiveIn, ArchiveOffset) of
		{ok, Header} ->
			BlockSize = ?BLOCK_SIZE bsl Header#header.block_size,
			ArchiveIn#archive{header=Header, block_size=BlockSize, archive_offset=ArchiveOffset};
		{error, not_found} ->
			if HeaderSearch -> {error, mpq_error_format};
				true -> add_header_to_archive(ArchiveIn, ArchiveOffset + ?BLOCK_SIZE, HeaderSearch)
			end
	end.


extract_header(ArchiveIn, ArchiveOffset) ->
	HeaderSize = header_size(),
	{ok, Header} = file:pread(ArchiveIn#archive.fd, ArchiveOffset, HeaderSize),
	<<MpqMagic?L, OldHeaderSize?L, ArchiveVersion?L, Version?W, BlockSize?W, HashTableOffset?L, BlockTableOffset?L, HashTableCount?L, BlockTableCount?L>> = Header,
	ExpectedMagic = ?LIBMPQ_HEADER,
	if ExpectedMagic == MpqMagic ->
			RealHeaderSize = if Version == ?LIBMPQ_ARCHIVE_VERSION_ONE -> HeaderSize;
				Version == ?LIBMPQ_ARCHIVE_VERSION_TWO -> HeaderSize + header_ex_size();
				true -> OldHeaderSize
			end,
			io:format("block size: ~p~n", [BlockSize]),
			io:format("version: ~p~n", [Version]),
			io:format("block table count: ~p~n", [BlockTableCount]),
			io:format("hash table count: ~p~n", [HashTableCount]),
			{ok, #header{
				mpq_magic = MpqMagic,
				header_size = RealHeaderSize,
				archive_version = ArchiveVersion,
				version = Version,
				block_size = BlockSize,
				hash_table_offset = HashTableOffset,
				block_table_offset = BlockTableOffset,
				hash_table_count = HashTableCount,
				block_table_count = BlockTableCount
			}};
		true ->
			{error, not_found}
	end.



hash_table_size() ->
	4 + 4 + 2 + 2 + 4.


header_size() ->
	4 + 4 + 4 + 2 + 2 + 4 + 4 + 4 + 4.

header_ex_size() ->
	8 + 2 + 2.
