-module(block).

-export([decompress_block/4]).
-export([get_flags_at_file_number/2]).
-export([close_offset/2, open_offset/2, unpacked_size/3, read/4]).


-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


get_flags_at_file_number(Archive, FileNumber) ->
	Map = archive:get_map_at_offset(Archive#archive.map, FileNumber),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	Block#block.flags.


decompress_block(Buffer, InSize, OutSize, Flag) ->
	if Flag == ?FLAG_COMPRESS_NONE -> Buffer;
		true ->
			% check if block is really compressed
			% some blocks have set the compression flag
			% but are really not compressed
					%io:format("insize: ~p~noutsize: ~p~n", [InSize, OutSize]),
			if InSize >= OutSize -> Buffer;
				true ->
					if Flag == ?FLAG_COMPRESS_MULTI ->
							extract:decompress_multi(Buffer);
						Flag == ?FLAG_COMPRESS_PKZIP ->
							extract:decompress_pkzip(Buffer);
						true ->
							io:format("trying to decompress using unimplemented flag: ~p~n", [Flag]),
							Buffer
					end
			end
	end.




unpacked_size(Archive, FileNumber, BlockNumber) ->
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




read(Archive, FileNumber, BlockNumber, OutSize) ->
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


open_offset(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Files = Archive#archive.file,
	File = archive:get_file_at_offset(Files, FileNumber),
	Blocks = Archive#archive.block,
	Block = archive:get_block_at_offset(Blocks, FileNumber),
	Flags = Block#block.flags,
	{NewFile, NewFlags} = if File#file.open_count > 0 ->
			{File#file{open_count=File#file.open_count + 1}, Flags};
		true ->
			open_offset_internal(Archive, FileNumber)
	end,
	%io:format("new file: ~p~n", [NewFile]),
	NewBlocks = if Flags == NewFlags -> Blocks;
		true ->
			NewBlock = Block#block{flags=NewFlags},
			archive:update_block_at_offset(Blocks, NewBlock, FileNumber)
	end,
	NewFiles = archive:update_file_at_offset(Files, NewFile, FileNumber),
	Archive#archive{file=NewFiles, block=NewBlocks}.

% opens previously unopened block
open_offset_internal(Archive, FileNumber) ->
	Maps = Archive#archive.map,
	Map = archive:get_map_at_offset(Maps, FileNumber),
	I = Map#map.block_table_indices,
	Blocks = Archive#archive.block,
	Block = archive:get_block_at_offset(Blocks, I),
	Flags = Block#block.flags,
	IsSingle = util:has_flag(Flags, ?FLAG_SINGLE),
	BlockSize = Archive#archive.block_size,
	PackedSize1 = if IsSingle ->
			% 8 bytes
			4 * 2;
		true ->
			4 * (((Block#block.unpacked_size + BlockSize - 1) div BlockSize) + 1)
	end,
	HasExtra = util:has_flag(Flags, ?FLAG_EXTRA),
	PackedSize = if HasExtra -> PackedSize1 + 4;
		true -> PackedSize1
	end,
	OpenCount = 1,
	IsCompressed = util:has_flag(Flags, ?FLAG_COMPRESSED),
	if IsCompressed andalso not IsSingle ->
			BlockExs = Archive#archive.block_ex,
			BlockEx = archive:get_block_ex_at_offset(BlockExs, I),
			Offset = Block#block.offset + (BlockEx#block_ex.offset_high bsl 32) + Archive#archive.archive_offset,
			{ok, PackedOffset} = file:pread(Archive#archive.fd, Offset, PackedSize),
			<<FirstWord?L, _/binary>> = PackedOffset,
			NewFlags = if FirstWord /= PackedSize andalso FirstWord /= PackedSize + 4 ->
					Flags bor ?FLAG_ENCRYPTED;
				true -> Flags
			end,
			IsEncrypted = util:has_flag(NewFlags, ?FLAG_ENCRYPTED),
			File = if IsEncrypted ->
					Seed = crypto:decrypt_key(PackedOffset, PackedSize, BlockSize),
					DecryptedPackedOffset = crypto:decrypt_block(PackedOffset, PackedSize, Seed - 1),
					#file{seed=Seed, packed_offset=DecryptedPackedOffset, open_count=OpenCount};
				true ->
					#file{seed=0, packed_offset=PackedOffset, open_count=OpenCount}
			end,
			{File, NewFlags};
		true ->
			if IsSingle ->
					Size = Block#block.packed_size,
					PackedOffset = <<0?L, Size?L>>,
					{#file{seed=0, packed_offset=PackedOffset, open_count=OpenCount}, Flags};
				true ->
					PackedOffset = lists:foldl(fun(Num, Acc) ->
						Size = (Block#block.unpacked_size + BlockSize - 1) div BlockSize,
						Word = if Size == Num -> Block#block.unpacked_size;
							true -> Num * BlockSize
						end,
						<<Acc/binary, Word?L>>
					end, <<>>, lists:seq(0, PackedSize1-1)),
					{#file{seed=0, packed_offset=PackedOffset, open_count=OpenCount}, Flags}
			end
	end.



close_offset(Archive, FileNumber) ->
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
