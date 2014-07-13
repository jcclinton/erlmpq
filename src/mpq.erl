-module(mpq).

-export([archive_open/1, archive_open/2, archive_close/1]).
-export([file_number/2, file_read/2]).

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
		fun archive_builder:add_map_to_archive/1
	],
	ArchiveOut = lists:foldl(fun(Fun, Arch) ->
		Fun(Arch)
	end, Archive, BuildFuns),
	{ok, ArchiveOut}.


file_read(Archive, Number) ->
	_Valid = util:check_file_num(Archive, FileNumber),
	UnpackedSize = file_unpacked_size(Archive, FileNumber),
	FileOffset = file_offset(Archive, FileNumber),
	Blocks = file_blocks(Archive, FileNumber),
	block_open_offset(Archive, FileNumber),

	ok.


block_unpacked_size(Archive, FileNumber, BlockNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	_ValidBlockNumber = util:check_block_num(Archive, BlockNumber),
	Map = util:get_map_at_offset(Archive#archive.map, FileNumber),
	I = Map#map.block_table_indices,
	Block = util:get_block_at_offset(Archive#archive.block, I),
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


block_close_offset(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	NewOpenCount = Archive#archive
	ok.

block_read(Archive, Number, I) ->
	ok.

block_open_offset(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	ok.



file_blocks(Archive, Number) ->
	_Valid = util:check_file_num(Archive, Number),
	Map = archive:get_map_at_offset(Archive#archive.map, Number),
	I = Map#map.block_table_indices,
	Block = archive:get_block_at_offset(Archive#archive.block, I),
	Flags = Block#block.flags,
	HasFlag = util:has_flag(Flags, ?FLAG_SINGLE),
	if HasFlag -> 1;
		not HasFlag ->
			UnpackedSize = Block#block.unpacked_size,
			BlockSize = Archive#archive.block_size,
			(UnpackedSize + BlockSize - 1) div BlockSize
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
