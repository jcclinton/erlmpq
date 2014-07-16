-module(util).

-export([map_size/0, hash_table_size/0, header_size/0, header_ex_size/0, block_size/0, block_ex_size/0, file_size/0, file_packed_offset_size/0]).
-export([add_32bit/1, sub_32bit/1, has_flag/2, check_file_num/2, check_block_num/2]).
-export([make_dir/2]).
-export([file_open/2, file_close/1, file_pread/3]).

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


% checks if a given filenumber is valid
check_file_num(Archive, FileNumber) ->
	if FileNumber > Archive#archive.files - 1 orelse FileNumber < 0 -> throw(error_exist);
		true -> ok
	end.

% checks if a given block number is valid
check_block_num(Archive, BlockNumber) ->
	if BlockNumber < 0 -> throw(error_exist);
		true ->
			Map = archive:get_map_at_offset(Archive#archive.map, BlockNumber),
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
			if BlockNumber >= Val -> throw(error_exist);
				true -> ok
			end
	end.


% wrapper around file:open to simplify code
file_open(Filename, Options) ->
	case file:open(Filename, Options) of
		{error, Error} -> throw(Error);
		{ok, Fd} -> Fd
	end.

% wrapper around file:close to simplify code
file_close(Fd) ->
	case file:close(Fd) of
		{error, Error} -> throw(Error);
		ok -> ok
	end.

% wrapper around file:pread to simplify code
file_pread(Fd, Offset, Size) ->
	case file:pread(Fd, Offset, Size) of
		{error, Error} -> throw(Error);
		{ok, Result} -> Result
	end.



% check if a given flag is contained in some integer
has_flag(Flags, Flag) ->
	Flags band Flag /= 0.


%% byte sizes for all common binary data structures

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
