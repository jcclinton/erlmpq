-module(crypto).

-export([decrypt_block/3, hash_string/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


decrypt_block(Buffer, Size, Seed) ->
	Seed2 = 16#EEEEEEEE,
	Buffer.
	%decrypt_block(Buffer, Size, Seed, Seed2, <<>>).

decrypt_block(_, Size, _, _, Acc) when Size < 4 -> Acc;
decrypt_block(<<Buff?L, Rest/binary>>, Size, Seed, Seed2, Acc) ->
	if Size rem 100000 == 0 ->
		io:format("decrypting block: ~p~n", [Size]);
		true -> ok
	end,
	CryptOffset = 16#400 + (Seed band 16#FF),
	Seed2Out = Seed2 + crypt_buffer:get_offset(CryptOffset),
	Char = Buff bxor (Seed + Seed2Out),
	SeedOut = ((bnot Seed bsl 16#15) + 16#11111111) bor (Seed bsr 16#0B),
	Seed2Out2 = Char + Seed2Out + (Seed2Out bsl 5) + 3,
	decrypt_block(Rest, Size-4, SeedOut, Seed2Out2, <<Acc/binary, Char?L>>).



hash_string(String, Offset) ->
	Seed1In = 16#7FED7FED,
	Seed2In = 16#EEEEEEEE,
	StringUpper = string:to_upper(String),
	{Seed1Out, _} = lists:foldl(fun(Char, {Seed1Acc,Seed2Acc}) ->
		S1a = crypt_buffer:get_offset(Offset + Char),
		S1b = Seed1Acc + Seed2Acc,
		Seed1 = S1a bxor S1b,
		Seed2 = Char + Seed1 + Seed2Acc + (Seed2Acc bsl 5) + 3,
		{Seed1, Seed2}
	end, {Seed1In, Seed2In}, StringUpper),
	Seed1Out.
