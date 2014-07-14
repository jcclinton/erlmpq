-module(mpq).

-export([archive_open/1, archive_open/2, archive_close/1]).
-export([file_number/2, file_read/2, file_unpacked_size/2, file_offset/2]).
-export([block_close_offset/2, block_open_offset/2, block_unpacked_size/3, block_read/4]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").




archive_close(Archive) ->
	file:close(Archive#archive.fd).


archive_open(Filename) -> archive_open(Filename, -1).
archive_open(Filename, Offset) ->
	{ArchiveOffset, HeaderSearch} = if Offset == -1 -> {0, true};
		true -> {Offset, false}
	end,
	{ok, Fd} = file:open(Filename, [read, binary]),
	InitialArchive = #archive{fd=Fd},
	Archive = archive_builder:add_header_to_archive(InitialArchive, ArchiveOffset, HeaderSearch),

	BuildFuns = [
		fun archive_builder:add_header_ex_to_archive/1,
		fun archive_builder:add_hash_table_to_archive/1,
		fun archive_builder:add_block_to_archive/1,
		fun archive_builder:add_block_ex_to_archive/1,
		fun archive_builder:add_map_to_archive/1,
		fun archive_builder:add_file_to_archive/1
	],
	ArchiveOut = lists:foldl(fun(Fun, Arch) ->
		Fun(Arch)
	end, Archive, BuildFuns),
	{ok, ArchiveOut}.


file_read(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	%FileOffset = file_offset(Archive, FileNumber),
	Blocks = file_blocks(Archive, FileNumber),
	%io:format("block: ~p~n", [Blocks]),
	Archive2 = block_open_offset(Archive, FileNumber),

	Buffer = lists:foldl(fun(I, Acc) ->
		UnpackedSize = block_unpacked_size(Archive2, FileNumber, I),
		%io:format("Unpacked size: ~p~n", [UnpackedSize]),
		OutBuf = block_read(Archive2, FileNumber, I, UnpackedSize),
		<<Acc/binary, OutBuf/binary>>
	end, <<>>, lists:seq(0, Blocks-1)),

	ArchiveOut = block_close_offset(Archive2, FileNumber),
	{ArchiveOut, Buffer}.


block_unpacked_size(Archive, FileNumber, BlockNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	%_ValidBlockNumber = util:check_block_num(Archive, BlockNumber),
	Map = archive:get_map_at_offset(Archive#archive.map, FileNumber),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	Flags = Block#block.flags,
	HasFlag = util:has_flag(Flags, ?FLAG_SINGLE),
	UnpackedSize = Block#block.unpacked_size,
	BlockSize = Archive#archive.block_size,
	if HasFlag -> UnpackedSize;
		true ->
			TotalSize = ((UnpackedSize + BlockSize - 1) div BlockSize) - 1,
			if BlockNumber < TotalSize -> BlockSize;
				true ->
					UnpackedSize - (BlockSize * BlockNumber)
			end
	end.




block_read(Archive, FileNumber, BlockNumber, OutSize) ->
	Map = archive:get_map_at_offset(Archive#archive.map, FileNumber),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	Offset = Block#block.offset,
	BlockEx = archive:get_block_ex_at_offset(Archive#archive.block_ex, I),
	OffsetHigh = BlockEx#block_ex.offset_high bsl 32,
	File = archive:get_file_at_offset(Archive#archive.file, FileNumber),
	PackedOffset = archive:get_file_packed_offset_at_offset(File#file.packed_offset, BlockNumber),
	BlockOffset = Offset + OffsetHigh + PackedOffset + Archive#archive.archive_offset,
	PackedOffset2 = archive:get_file_packed_offset_at_offset(File#file.packed_offset, BlockNumber + 1),
	InSize = if PackedOffset2 == 0 -> PackedOffset;
		true -> PackedOffset2 - PackedOffset
	end,
	{ok, BufferIn} = file:pread(Archive#archive.fd, BlockOffset, InSize),
	IsEncrypted = archive_file:is_encrypted(Archive, FileNumber),
	Buffer1 = if IsEncrypted ->
			Seed = crypto:block_seed(Archive, FileNumber, BlockNumber),
			crypto:decrypt_block(BufferIn, InSize, Seed);
		true ->
			BufferIn
	end,

	IsCompressed = archive_file:is_compressed(Archive, FileNumber),
	Buffer2 = if IsCompressed ->
			block:decompress_block(Buffer1, InSize, OutSize, ?FLAG_COMPRESS_MULTI);
		true -> Buffer1
	end,

	IsImploded = archive_file:is_imploded(Archive, FileNumber),
	Buffer3 = if IsImploded ->
			block:decompress_block(Buffer2, InSize, OutSize, ?FLAG_COMPRESS_PKZIP);
		true -> Buffer2
	end,

	Buffer4 = if not IsCompressed andalso not IsImploded ->
			block:decompress_block(Buffer3, InSize, OutSize, ?FLAG_COMPRESS_NONE);
		true -> Buffer3
	end,
	Buffer4.

block_open_offset(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Files = Archive#archive.file,
	File = archive:get_file_at_offset(Files, FileNumber),
	Blocks = Archive#archive.block,
	Block = archive:get_block_at_offset(Blocks, FileNumber),
	Flags = Block#block.flags,
	{NewFile, NewFlags} = if File#file.open_count > 0 ->
			{File#file{open_count=File#file.open_count + 1}, Flags};
		true ->
			block:open_offset(Archive, FileNumber)
	end,
	%io:format("new file: ~p~n", [NewFile]),
	NewBlocks = if Flags == NewFlags -> Blocks;
		true ->
			NewBlock = Block#block{flags=NewFlags},
			archive:update_block_at_offset(Blocks, NewBlock, FileNumber)
	end,
	NewFiles = archive:update_file_at_offset(Files, NewFile, FileNumber),
	Archive#archive{file=NewFiles, block=NewBlocks}.

block_close_offset(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Files = Archive#archive.file,
	File = archive:get_file_at_offset(Files, FileNumber),
	if File == 0 -> Archive;
		File > 0 ->
			NewFileCount = File#file.open_count - 1,
			NewFile = if NewFileCount == 0 ->
					File#file{open_count=NewFileCount, packed_offset=0, seed=0};
				NewFileCount > 0 ->
					File#file{open_count=NewFileCount}
			end,
			NewFiles = archive:update_file_at_offset(Files, NewFile, FileNumber),
			Archive#archive{file=NewFiles}
	end.


file_blocks(Archive, FileNumber) ->
	_Valid = util:check_file_num(Archive, FileNumber),
	Map = archive:get_map_at_offset(Archive#archive.map, FileNumber),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	Flags = Block#block.flags,
	HasFlag = util:has_flag(Flags, ?FLAG_SINGLE),
	if HasFlag -> 1;
		not HasFlag ->
			UnpackedSize = Block#block.unpacked_size,
			BlockSize = Archive#archive.block_size,
			Numer = (UnpackedSize + BlockSize - 1),
			Numer div BlockSize
	end.


file_offset(Archive, Number) ->
	_Valid = util:check_file_num(Archive, Number),
	Map = archive:get_map_at_offset(Archive#archive.map, Number),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	BlockEx = archive:get_block_ex_at_offset(Archive#archive.block_ex, I),
	Offset = Block#block.offset,
	OffsetHigh = BlockEx#block_ex.offset_high bsl 32,
	Offset + OffsetHigh.




file_unpacked_size(Archive, Number) ->
	_Valid = util:check_file_num(Archive, Number),
	Map = archive:get_map_at_offset(Archive#archive.map, Number),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	Block#block.unpacked_size.
		


file_number(Archive, Filename) ->
	HTCount = Archive#archive.header#header.hash_table_count,
	Hash1 = crypto:hash_string(Filename, 16#0) band (HTCount - 1),
	Hash2 = crypto:hash_string(Filename, 16#100),
	Hash3 = crypto:hash_string(Filename, 16#200),
	Number = loop_hash(Archive, Hash1, Hash1, Hash2, Hash3, HTCount),
	Number.


loop_hash(Archive, I, Hash1, Hash2, Hash3, HTCount) ->
	Hashes = Archive#archive.hash,
	Hash = archive:get_hash_table_at_offset(Hashes, I),
	HashA = Hash#hash.hash_a,
	HashB = Hash#hash.hash_b,
	BlockTableIndex = Hash#hash.block_table_index,
	%io:format("I: ~p~nblocktableindex: ~p~n~n", [I, BlockTableIndex]),
	HasCycled = ((I + 1) band (HTCount - 1)) == Hash1,
	if BlockTableIndex == ?HASH_FREE orelse HasCycled -> 0;
		true ->
			if HashA == Hash2 andalso HashB == Hash3 ->
					Maps = Archive#archive.map,
					Map = archive:get_map_at_offset(Maps, BlockTableIndex),
					Diff = Map#map.block_table_diff,
					BlockTableIndex - Diff;
				true ->
					Next = (I + 1) band (HTCount -1),
					loop_hash(Archive, Next, Hash1, Hash2, Hash3, HTCount)
			end
	end.
