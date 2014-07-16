-module(archive_file).

-export([is_encrypted/2, is_compressed/2, is_imploded/2]).
-export([number/2, read/2]).
-export([unpacked_size/2, offset/2]).


-include("include/binary.hrl").
-include("include/mpq_internal.hrl").

read(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	%FileOffset = archive_file:offset(Archive, FileNumber),
	Blocks = blocks(Archive, FileNumber),
	%io:format("block: ~p~n", [Blocks]),
	Archive2 = block:open_offset(Archive, FileNumber),

	Buffer = lists:foldl(fun(I, Acc) ->
		UnpackedSize = block:unpacked_size(Archive2, FileNumber, I),
		%io:format("Unpacked size: ~p~n", [UnpackedSize]),
		OutBuf = block:read(Archive2, FileNumber, I, UnpackedSize),
		<<Acc/bitstring, OutBuf/bitstring>>
	end, <<>>, lists:seq(0, Blocks-1)),

	ArchiveOut = block:close_offset(Archive2, FileNumber),
	{ArchiveOut, Buffer}.




blocks(Archive, FileNumber) ->
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


offset(Archive, Number) ->
	_Valid = util:check_file_num(Archive, Number),
	Map = archive:get_map_at_offset(Archive#archive.map, Number),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	BlockEx = archive:get_block_ex_at_offset(Archive#archive.block_ex, I),
	Offset = Block#block.offset,
	OffsetHigh = BlockEx#block_ex.offset_high bsl 32,
	Offset + OffsetHigh.




unpacked_size(Archive, FileNumber) ->
	_Valid = util:check_file_num(Archive, FileNumber),
	Map = archive:get_map_at_offset(Archive#archive.map, FileNumber),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	Block#block.unpacked_size.
		


number(Archive, Filename) ->
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


is_encrypted(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Flags = block:get_flags_at_file_number(Archive, FileNumber),
	util:has_flag(Flags, ?FLAG_ENCRYPTED).

is_compressed(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Flags = block:get_flags_at_file_number(Archive, FileNumber),
	util:has_flag(Flags, ?FLAG_COMPRESS_MULTI).

is_imploded(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Flags = block:get_flags_at_file_number(Archive, FileNumber),
	util:has_flag(Flags, ?FLAG_COMPRESS_PKZIP).
