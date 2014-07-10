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
	Archive = get_header(InitialArchive, ArchiveOffset, HeaderSearch),

	file:close(Fd),
	{ok, Archive}.


get_header(ArchiveIn, ArchiveOffset, HeaderSearch) ->
	case extract_header(ArchiveIn, ArchiveOffset) of
		{ok, Header} -> ArchiveIn#archive{header=Header};
		{error, not_found} ->
			if HeaderSearch -> {error, mpq_error_format};
				true -> get_header(ArchiveIn, ArchiveOffset + 512, HeaderSearch)
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



header_size() ->
	4 + 4 + 4 + 2 + 2 + 4 + 4 + 4 + 4.

header_ex_size() ->
	8 + 2 + 2.
