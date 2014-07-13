-module(util).

-export([map_size/0, hash_table_size/0, header_size/0, header_ex_size/0, block_size/0, block_ex_size/0]).
-export([add_32bit/1, has_flag/2, check_file_num/2, check_block_num/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").



%% add numbers in list, keep limited to 32 bits
add_32bit(L) ->
	lists:foldl(fun(N, Sum) ->
		(N + Sum) band 16#FFFFFFFF
	end, 0, L).


check_file_num(Archive, Num) ->
	Num > Archive#archive.files - 1 orelse Num < 0.

check_block_num(Archive, Num) ->
	if Num < 0 -> false;
		true ->
			Map = archive:get_map_at_offset(Archive, Num),
			I = Map#map.block_table_indices,
			Block = archive:get_block_at_offset(Archive, I),
			Flags = Block#block.flags,
			HasFlag = util:has_flag(Flags, ?FLAG_SINGLE),
			Val = if HasFlag /= 0 -> 1;
				true ->
					UnpackedSize = Block#block.unpacked_size,
					BlockSize = Archive#archive.block_size,
					(UnpackedSize + BlockSize - 1) / BlockSize
			end,
			if Num >= Val -> false;
				true -> true
			end
	end.



has_flag(Flags, Flag) ->
	Flags band Flag /= 0.



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
