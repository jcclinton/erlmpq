-module(crypto).

-export([decrypt_block/3, encrypt_block/3]).
-export([hash_string/2]).
-export([decrypt_key/3]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


encrypt_block(Buffer, Size, Seed) ->
	Seed2 = 16#EEEEEEEE,
	%Buffer.
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




decrypt_block(Buffer, Size, Seed) ->
	Seed2 = 16#EEEEEEEE,
	%Buffer.
	decrypt_block(Buffer, Size, Seed, Seed2, <<>>).

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


decrypt_key(Buffer, InSize, BlockSize) ->
	Key = 1,
	Key.


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

