-module(extract).

-export([decompress_multi/1]).
-export([decompress_wave_stereo/1]).
-export([decompress_wave_mono/1]).
-export([decompress_huffman/1]).
-export([decompress_bzip2/1]).
-export([decompress_pkzip/1]).
-export([decompress_zlib/1]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").



% buffer may have been decompressed using multiple compression algorithms
% loop through each algorithm, check if its flag exists in the first byte of the buffer
decompress_multi(<<DecompressFlag?B, BufferIn/binary>>) ->
	% apply all requisite decompression functions based on flag
	lists:foldl(fun({Flag, Fun}, Buffer) ->
		HasFlag = util:has_flag(DecompressFlag, Flag),
		if HasFlag -> Fun(Buffer);
			true -> Buffer
		end
	end, BufferIn, get_decompression_table()).

decompress_wave_stereo(Buffer) ->
	io:format("attemping to decompress buffer using unimplemented decompression type: ~p~n", ["wave stereo"]),
	Buffer.

decompress_wave_mono(Buffer) ->
	io:format("attemping to decompress buffer using unimplemented decompression type: ~p~n", ["wave mono"]),
	Buffer.

decompress_huffman(Buffer) ->
	io:format("attemping to decompress buffer using unimplemented decompression type: ~p~n", ["huffman"]),
	Buffer.

decompress_bzip2(Buffer) ->
	io:format("attemping to decompress buffer using unimplemented decompression type: ~p~n", ["bzip2"]),
	Buffer.

decompress_pkzip(Buffer) ->
	io:format("attemping to decompress buffer using unimplemented decompression type: ~p~n", ["pkzip"]),
	Buffer.

decompress_zlib(Buffer) ->
	Z = zlib:open(),
	ok = zlib:inflateInit(Z),
	BuffOut = zlib:inflate(Z, Buffer),
	ok = zlib:close(Z),
	if is_list(BuffOut) ->
			list_to_binary(BuffOut);
		true -> BuffOut
	end.



get_decompression_table() ->
	[
		{?COMPRESSION_HUFFMAN, fun ?MODULE:decompress_huffman/1},
		{?COMPRESSION_ZLIB, fun ?MODULE:decompress_zlib/1},
		{?COMPRESSION_PKZIP, fun ?MODULE:decompress_pkzip/1},
		{?COMPRESSION_BZIP2, fun ?MODULE:decompress_bzip2/1},
		{?COMPRESSION_WAVE_MONO, fun ?MODULE:decompress_wave_mono/1},
		{?COMPRESSION_WAVE_STEREO, fun ?MODULE:decompress_wave_stereo/1}
	].
