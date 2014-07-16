-module(crypto).

-export([decrypt_block/3, encrypt_block/3]).
-export([hash_string/2]).
-export([decrypt_key/3]).
-export([block_seed/3]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


block_seed(Archive, FileNumber, BlockNumber) ->
	ok = util:check_file_num(Archive, FileNumber),
	ok = util:check_block_num(Archive, BlockNumber),
	File = archive:get_file_at_offset(Archive#archive.file, FileNumber),
	File#file.seed + BlockNumber.


% encrypt given buffer Buffer of size Size given seed Seed
encrypt_block(Buffer, Size, Seed) ->
	Seed2 = 16#EEEEEEEE,
	encrypt_block(Buffer, Size, Seed, Seed2, <<>>).

encrypt_block(_, Size, _, _, Acc) when Size < 4 -> Acc;
encrypt_block(<<Buff?L, Rest/binary>>, Size, Seed, Seed2, Acc) ->
	if Size rem 100000 == 0 ->
		io:format("encrypting block: ~p~n", [Size]);
		true -> ok
	end,
	CryptOffset = util:add_32bit([16#400, Seed band 16#FF]),
	Seed2Out = util:add_32bit([Seed2, crypt_buffer:get_buffer_by_offset(CryptOffset)]),
	Char = Buff bxor util:add_32bit([Seed, Seed2Out]),
	SeedOut = util:add_32bit([bnot Seed bsl 16#15, 16#11111111]) bor (Seed bsr 16#0B),
	Seed2Out2 = util:add_32bit([Buff, Seed2Out, Seed2Out bsl 5, 3]),
	encrypt_block(Rest, Size-4, SeedOut, Seed2Out2, <<Acc/binary, Char?L>>).




% decrypt given buffer Buffer of size Size given seed Seed
decrypt_block(Buffer, Size, Seed) ->
	Seed2 = 16#EEEEEEEE,
	decrypt_block(Buffer, Size, Seed, Seed2, <<>>).

% the buffer is decrypted 4 bytes at a time, so quit when there are less than 4 left
decrypt_block(_, Size, _, _, Acc) when Size < 4 -> Acc;
decrypt_block(<<Buff?L, Rest/binary>>, Size, Seed, Seed2, Acc) ->
	if Size rem 100000 == 0 ->
		io:format("decrypting block: ~p~n", [Size]);
		true -> ok
	end,
	CryptOffset = util:add_32bit([16#400, Seed band 16#FF]),
	Seed2Out = util:add_32bit([Seed2, crypt_buffer:get_buffer_by_offset(CryptOffset)]),
	Char = Buff bxor util:add_32bit([Seed, Seed2Out]),
	SeedOut = util:add_32bit([bnot Seed bsl 16#15, 16#11111111]) bor (Seed bsr 16#0B),
	Seed2Out2 = util:add_32bit([Char, Seed2Out, Seed2Out bsl 5, 3]),
	%io:format("inbuf: ~p~n", [Char]),
	decrypt_block(Rest, Size-4, SeedOut, Seed2Out2, <<Acc/binary, Char?L>>).


% decrypt key
decrypt_key(Buffer, InSize, BlockSize) ->
	Seed2In = 16#EEEEEEEE,
	Offset = 16#400,
	<<Buf0?L, Buf1?L, _/binary>> = Buffer,
	Temp = util:sub_32bit([Buf0 bxor InSize, Seed2In]),
	lists:foldl(fun(I, Key) ->
		CryptBuf = crypt_buffer:get_buffer_by_offset(Offset + I),
		Seed1 = util:sub_32bit([Temp, CryptBuf]),
		Seed1Lower = Seed1 band 16#FF,
		CryptOffset = util:add_32bit([Seed1Lower, Offset]),
		CryptBuf2 = crypt_buffer:get_buffer_by_offset(CryptOffset),
		Seed2_2 = util:add_32bit([Seed2In, CryptBuf2]),
		Ch = Buf0 bxor util:add_32bit([Seed1, Seed2_2]),
		if Ch /= InSize -> Key;
			true ->
				SaveSeed = util:add_32bit([Seed1, 1]),
				Seed1_2 = util:add_32bit([not Seed1 bsl 16#15, 16#11111111]) bor (Seed1 bsr 16#0B),
				Seed2_3 = util:add_32bit([Ch, Seed2_2, Seed2_2 bsl 5, 3]),
				CryptOffset2 = util:add_32bit([Offset, Seed1_2 band 16#FF]),
				CryptBuf3 = crypt_buffer:get_buffer_by_offset(CryptOffset2),
				Seed2_4 = util:add_32bit([Seed2_3, CryptBuf3]),
				Ch2 = Buf1 bxor util:add([Seed1_2, Seed2_4]),
				LessThan = util:sub([Ch2, Ch]) =< BlockSize,
				if LessThan -> SaveSeed;
					true -> Key
				end
			end
	end, 0, lists:seq(0, 16#100-1)).


% hash a string
% offset is used to get a value from the crypt buffer
hash_string(String, Offset) ->
	Seed1In = 16#7FED7FED,
	Seed2In = 16#EEEEEEEE,
	{Seed1Out, _} = lists:foldl(fun(Char, {Seed1Acc,Seed2Acc}) ->
		Index = util:add_32bit([Offset, Char]),
		Seed1 = crypt_buffer:get_buffer_by_offset(Index) bxor util:add_32bit([Seed1Acc, Seed2Acc]),
		Seed2 = util:add_32bit([Char, Seed1, Seed2Acc, Seed2Acc bsl 5, 3]),
		{Seed1, Seed2}
	end, {Seed1In, Seed2In}, string:to_upper(String)),
	Seed1Out.

