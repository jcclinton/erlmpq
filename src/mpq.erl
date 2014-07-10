-module(mpq).

-export([]).
-compile([export_all]).

-include("include/binary.hrl").


archive_open(FileName, Offset) ->
	{ArchiveOffset, HeaderSearch} = if Offset == -1 -> {0, true};
		true -> {Offset, false}
	end,
	ok.
