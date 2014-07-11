-module(mpq).

-export([archive_open/2]).
-compile([export_all]).

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
	Archive = add_header_to_archive(InitialArchive, ArchiveOffset, HeaderSearch),

	Archive2 = if Archive#archive.header#header.version == ?LIBMPQ_ARCHIVE_VERSION_TWO ->
			add_header_ex_to_archive(Archive);
		true -> Archive
	end,

	io:format("adding hash table to archive~n"),
	Archive3 = add_hash_table_to_archive(Archive2),
	io:format("adding block table to archive~n"),
	Archive4 = add_block_to_archive(Archive3),

	io:format("adding block ex table to archive~n"),
	Archive5 = if Archive#archive.header_ex#header_ex.extended_offset > 0 ->
			add_block_ex_to_archive(Archive4);
		true ->
			Archive4
		end,

	io:format("adding map table to archive~n"),
	Archive6 = add_map_to_archive(Archive5),
			
	{ok, Archive6}.


add_map_to_archive(Archive) ->
	TableCount = Archive#archive.header#header.block_table_count,
	EmptyMap = binary:copy(<<0?Q>>, TableCount),
	Size = map_size(),

	{Map, Count} = lists:foldl(fun(I, {Map, Count}) ->
		Diff = I - Count,
		Offset = I * Size,
		<<Head:Offset/binary, Indices?L, _OldDiff?L, Tail/binary>> = Map,
		Map1 = <<Head/binary, Indices?L, Diff?L, Tail/binary>>,
		Flags = get_block_flags(I, Archive#archive.block),
		NotExists = Flags band ?FLAG_EXISTS == 0,
		if NotExists -> {Map1, Count};
			not NotExists ->
				<<Head2:Count/binary, _OldIndices?L, Diff2?L, Tail2/binary>> = Map1,
				Map2 = <<Head2/binary, I?L, Diff2?L, Tail2/binary>>,
				{Map2, Count+1}
		end
	end, {EmptyMap, 0}, lists:seq(0, TableCount-1)),
	Archive#archive{map=Map, files=Count}.


add_block_ex_to_archive(Archive) ->
	Offset = Archive#archive.header_ex#header_ex.extended_offset + Archive#archive.archive_offset,
	Size = block_ex_size() * Archive#archive.header#header.block_table_count,

	{ok, BlocksExBin} = file:pread(Archive#archive.fd, Offset, Size),
	Archive#archive{block_ex=BlocksExBin}.


get_block_flags(Offset, Blocks) ->
	Size = block_size() * 8,
	SizeSansFlags = Size - 32,
	<<_:Offset/binary, _:SizeSansFlags/integer, Flags?L, _/binary>> = Blocks,
	Flags.


add_block_to_archive(Archive) ->
	BlockTableOffsetHigh = Archive#archive.header_ex#header_ex.block_table_offset_high bsl 32,
	Offset = Archive#archive.header#header.block_table_offset + Archive#archive.archive_offset + BlockTableOffsetHigh,
	Size = block_size() * Archive#archive.header#header.block_table_count,

	{ok, BlocksBin} = file:pread(Archive#archive.fd, Offset, Size),
	Seed = hash_string("(block table)", 16#300),
	DecryptedBlocks = decrypt_block(BlocksBin, Size, Seed),
	Archive#archive{block=DecryptedBlocks}.


add_hash_table_to_archive(Archive) ->
	ArchiveOffset = Archive#archive.archive_offset,
	HashTableOffset = Archive#archive.header#header.hash_table_offset,
	HashTableOffsetHigh = Archive#archive.header_ex#header_ex.hash_table_offset_high bsl 32,
	Offset = ArchiveOffset + HashTableOffset + HashTableOffsetHigh,

	TableCount = Archive#archive.header#header.hash_table_count,
	Size = hash_table_size() * TableCount,

	{ok, HashBin} = file:pread(Archive#archive.fd, Offset, Size),
	%allHashes is a list of hash records
	%AllHashes = get_all_hashes(HashBin),
	Seed = hash_string("(hash table)", 16#300),
	DecryptedHashes = decrypt_block(HashBin, Size, Seed),
	Archive#archive{hash=DecryptedHashes}.


decrypt_block(Buffer, Size, Seed) ->
	Seed2 = 16#EEEEEEEE,
	decrypt_block(Buffer, Size, Seed, Seed2, <<>>).

decrypt_block(_, Size, _, _, Acc) when Size < 4 -> Acc;
decrypt_block(<<Buff?L, Rest/binary>>, Size, Seed, Seed2, Acc) ->
	if Size rem 100000 == 0 ->
		io:format("decrypting block: ~p~n", [Size]);
		true -> ok
	end,
	CryptOffset = 16#400 + (Seed band 16#FF),
	Seed2Out = Seed2 + crypt_buffer:get_offset(CryptOffset),
	Char = Buff bxor (Seed + Seed2Out),
	SeedOut = ((bnot Seed bsl 16#15) + 16#11111111) bor (Seed bsr 16#0B),
	Seed2Out2 = Char + Seed2Out + (Seed2Out bsl 5) + 3,
	decrypt_block(Rest, Size-4, SeedOut, Seed2Out2, <<Acc/binary, Char?L>>).



hash_string(String, Offset) ->
	Seed1In = 16#7FED7FED,
	Seed2In = 16#EEEEEEEE,
	StringUpper = string:to_upper(String),
	{Seed1Out, _} = lists:foldl(fun(Char, {Seed1Acc,Seed2Acc}) ->
		S1a = crypt_buffer:get_offset(Offset + Char),
		S1b = Seed1Acc + Seed2Acc,
		Seed1 = S1a bxor S1b,
		Seed2 = Char + Seed1 + Seed2Acc + (Seed2Acc bsl 5) + 3,
		{Seed1, Seed2}
	end, {Seed1In, Seed2In}, StringUpper),
	Seed1Out.


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



map_size() ->
	4 + 4.

hash_table_size() ->
	4 + 4 + 2 + 2 + 4.

header_size() ->
	4 + 4 + 4 + 2 + 2 + 4 + 4 + 4 + 4.

header_ex_size() ->
	8 + 2 + 2.

block_size() ->
	4 + 4 + 4 + 4.

block_ex_size() ->
	2.
