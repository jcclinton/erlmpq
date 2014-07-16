-module(mpq).

-export([archive_open/1, archive_open/2, archive_close/1]).
-export([file_number/2, file_read/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").



% interface to open an archive
archive_open(Filename) -> archive_open(Filename, -1).
archive_open(Filename, Offset) ->
	wrap_output(archive, open, [Filename, Offset]).


% interface to close an archive
archive_close(Archive) ->
	wrap_output(archive, close, [Archive]).


% interface to read a file
file_read(Archive, FileNumber) ->
	wrap_output(archive_file, read, [Archive, FileNumber]).

% interface to get a filenumber
file_number(Archive, Filename) ->
	wrap_output(archive_file, number, [Archive, Filename]).



% catch any exceptions that have bubbled up
% otherwise pass result straight through
wrap_output(M, F, A) ->
	try apply(M, F, A) of
		Res -> Res
	catch
		Err -> {error, Err}
	end.



