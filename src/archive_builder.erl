-module(archive_builder).

-export([add_header_to_archive/3]).
-export([add_map_to_archive/1, add_block_ex_to_archive/1, add_block_to_archive/1, add_hash_table_to_archive/1, add_header_ex_to_archive/1, add_file_to_archive/1]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


add_file_to_archive(Archive) ->
	BlockCount = Archive#archive.header#header.block_table_count,
	Size = util:file_size() * BlockCount,
	%File = binary:copy(<<0>>, Size),
	File = lists:foldl(fun(_, L) ->
		[#file{}|L]
	end, [], lists:seq(1, Size)),
	Archive#archive{file=File}.



add_map_to_archive(Archive) ->
	TableCount = Archive#archive.header#header.block_table_count,
	EmptyMap = binary:copy(<<0?Q>>, TableCount),
	Size = util:map_size(),

	{Map, Count} = lists:foldl(fun(I, {Map, Count}) ->
		Diff = I - Count,
		Offset = I * Size,
		<<Head:Offset/binary, Indices?L, _OldDiff?L, Tail/binary>> = Map,
		Map1 = <<Head/binary, Indices?L, Diff?L, Tail/binary>>,
		Block = archive:get_block_at_offset(Archive#archive.block, I),
		Flags = Block#block.flags,
		Exists = util:has_flag(Flags, ?FLAG_EXISTS),
		if not Exists -> {Map1, Count};
			Exists ->
				<<Head2:Count/binary, _OldIndices?L, Diff2?L, Tail2/binary>> = Map1,
				Map2 = <<Head2/binary, I?L, Diff2?L, Tail2/binary>>,
				{Map2, Count+1}
		end
	end, {EmptyMap, 0}, lists:seq(0, TableCount-1)),
	Archive#archive{map=Map, files=Count}.


add_block_ex_to_archive(Archive) ->
	if Archive#archive.header_ex#header_ex.extended_offset > 0 ->
		Offset = Archive#archive.header_ex#header_ex.extended_offset + Archive#archive.archive_offset,
		Size = util:block_ex_size() * Archive#archive.header#header.block_table_count,

		{ok, BlocksExBin} = file:pread(Archive#archive.fd, Offset, Size),
		Archive#archive{block_ex=BlocksExBin};
	true -> Archive
	end.


add_block_to_archive(Archive) ->
	BlockTableOffsetHigh = Archive#archive.header_ex#header_ex.block_table_offset_high bsl 32,
	Offset = Archive#archive.header#header.block_table_offset + Archive#archive.archive_offset + BlockTableOffsetHigh,
	Size = util:block_size() * Archive#archive.header#header.block_table_count,

	{ok, BlocksBin} = file:pread(Archive#archive.fd, Offset, Size),
	Seed = crypto:hash_string("(block table)", 16#300),
	DecryptedBlocks = crypto:decrypt_block(BlocksBin, Size, Seed),
	Archive#archive{block=DecryptedBlocks}.


add_hash_table_to_archive(Archive) ->
	ArchiveOffset = Archive#archive.archive_offset,
	HashTableOffset = Archive#archive.header#header.hash_table_offset,
	HashTableOffsetHigh = Archive#archive.header_ex#header_ex.hash_table_offset_high bsl 32,
	Offset = ArchiveOffset + HashTableOffset + HashTableOffsetHigh,

	TableCount = Archive#archive.header#header.hash_table_count,
	Size = util:hash_table_size() * TableCount,

	{ok, HashBin} = file:pread(Archive#archive.fd, Offset, Size),
	Seed = crypto:hash_string("(hash table)", 16#300),
	DecryptedHashes = crypto:decrypt_block(HashBin, Size, Seed),
	Archive#archive{hash=DecryptedHashes}.




add_header_ex_to_archive(Archive) ->
	if Archive#archive.header#header.version == ?LIBMPQ_ARCHIVE_VERSION_TWO ->
		ExOffset = util:header_size() + Archive#archive.archive_offset,
		ExSize = util:header_ex_size(),
		{ok, HeaderEx} = file:pread(Archive#archive.fd, ExOffset, ExSize),
		<<ExtendedOffset?Q, HashTableOffsetHigh?W, BlockTableOffsetHigh?W>> = HeaderEx,
		HeaderExRecord = #header_ex{
			extended_offset = ExtendedOffset,
			hash_table_offset_high = HashTableOffsetHigh,
			block_table_offset_high = BlockTableOffsetHigh
		},
		Archive#archive{header_ex=HeaderExRecord};
	true -> Archive
	end.


add_header_to_archive(ArchiveIn, ArchiveOffset, HeaderSearch) ->
	case extract_header(ArchiveIn, ArchiveOffset) of
		{ok, Header} ->
			BlockSize = ?BLOCK_SIZE bsl Header#header.block_size,
			ArchiveIn#archive{header=Header, block_size=BlockSize, archive_offset=ArchiveOffset};
		{error, not_found} ->
			if not HeaderSearch -> {error, mpq_error_format};
				HeaderSearch -> add_header_to_archive(ArchiveIn, ArchiveOffset + ?BLOCK_SIZE, HeaderSearch)
			end
	end.



%% private

extract_header(ArchiveIn, ArchiveOffset) ->
	HeaderSize = util:header_size(),
	{ok, Header} = file:pread(ArchiveIn#archive.fd, ArchiveOffset, HeaderSize),
	<<MpqMagic?L, OldHeaderSize?L, ArchiveVersion?L, Version?W, BlockSize?W, HashTableOffset?L, BlockTableOffset?L, HashTableCount?L, BlockTableCount?L>> = Header,
	ExpectedMagic = ?LIBMPQ_HEADER,
	if ExpectedMagic == MpqMagic ->
			RealHeaderSize = if Version == ?LIBMPQ_ARCHIVE_VERSION_ONE -> HeaderSize;
				Version == ?LIBMPQ_ARCHIVE_VERSION_TWO -> HeaderSize + util:header_ex_size();
				true -> OldHeaderSize
			end,
			io:format("block size: ~p~n", [BlockSize]),
			io:format("version: ~p~n", [Version]),
			io:format("block table count: ~p~n", [BlockTableCount]),
			io:format("hash table count: ~p~n", [HashTableCount]),

			io:format("header size: ~p~n", [RealHeaderSize]),
			io:format("archive version: ~p~n", [ArchiveVersion]),
			io:format("hash table offset: ~p~n", [HashTableOffset]),
			io:format("block table offset: ~p~n", [BlockTableOffset]),
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
