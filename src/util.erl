-module(util).

-export([map_size/0, hash_table_size/0, header_size/0, header_ex_size/0, block_size/0, block_ex_size/0, file_size/0, file_packed_offset_size/0]).
-export([add_32bit/1, sub_32bit/1, has_flag/2, check_file_num/2, check_block_num/2]).
-export([make_dir/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").



% takes a filename, fully qualified with directory
% and strips out the filename and creates the directories
make_dir(BaseDir, Filename) ->
	Sep = "/",
	Tokens = string:tokens(Filename, Sep),
	[_|RevDirs] = lists:reverse(Tokens),
	Dirs = lists:reverse(RevDirs),
	%Sep ++ string:join(Dirs, Sep) ++ Sep.
	lists:foldl(fun(DirName, Path) ->
		NewPath = Path ++ Sep ++ DirName,
		file:make_dir(NewPath),
		NewPath
	end, BaseDir, Dirs).



%% add numbers in list
%% convert from signed to unsigned and keep limited to 32 bits
add_32bit(L) ->
	lists:foldl(fun(N, Acc) ->
		Num = (N + Acc) band 16#FFFFFFFF,
		Bin = <<Num?SL>>,
		<<Out?L>> = Bin,
		Out
	end, 0, L).


%% sub numbers in list, starting with first element
%% convert from signed to unsigned and keep limited to 32 bits
sub_32bit([El|Rest]) ->
	lists:foldl(fun(N, Acc) ->
		Num = (Acc - N) band 16#FFFFFFFF,
		Bin = <<Num?SL>>,
		<<Out?L>> = Bin,
		Out
	end, El, Rest).


check_file_num(Archive, Num) ->
	if Num > Archive#archive.files - 1 orelse Num < 0 -> throw(error_exist);
		true -> ok
	end.

check_block_num(Archive, Num) ->
	if Num < 0 -> throw(error_exist);
		true ->
			Map = archive:get_map_at_offset(Archive#archive.map, Num),
			I = Map#map.block_table_indices,
			Block = archive:get_block_at_offset(Archive, I),
			Flags = Block#block.flags,
			HasFlag = util:has_flag(Flags, ?FLAG_SINGLE),
			Val = if HasFlag /= 0 -> 1;
				true ->
					UnpackedSize = Block#block.unpacked_size,
					BlockSize = Archive#archive.block_size,
					(UnpackedSize + BlockSize - 1) div BlockSize
			end,
			if Num >= Val -> throw(error_exist);
				true -> ok
			end
	end.



has_flag(Flags, Flag) ->
	Flags band Flag /= 0.


file_packed_offset_size() ->
	4.

file_size() ->
	4 + 4 + 4.

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
