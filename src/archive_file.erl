-module(archive_file).

-export([is_encrypted/2, is_compressed/2, is_imploded/2]).


is_encrypted(Archive, FileNumber) ->
	true.

is_compressed(Archive, FileNumber) ->
	true.

is_imploded(Archive, FileNumber) ->
	true.
