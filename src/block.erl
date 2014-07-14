-module(block).

-export([open_offset/2]).
-export([decompress_block/4]).
-export([get_flags_at_file_number/2]).


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
			if InSize < OutSize -> Buffer;
				true ->
					% not implemented
					io:format("trying to decompress using unimplemented flag: ~p~n", [Flag]),
					Buffer
			end
	end.



% opens previously unopened block
open_offset(Archive, FileNumber) ->
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
			4 * ((Block#block.unpacked_size + BlockSize - 1) div (BlockSize + 1))
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
