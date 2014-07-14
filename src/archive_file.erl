-module(archive_file).

-export([is_encrypted/2, is_compressed/2, is_imploded/2]).


-include("include/binary.hrl").
-include("include/mpq_internal.hrl").

is_encrypted(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Flags = block:get_flags_at_file_number(Archive, FileNumber),
	util:has_flag(Flags, ?FLAG_ENCRYPTED).

is_compressed(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Flags = block:get_flags_at_file_number(Archive, FileNumber),
	util:has_flag(Flags, ?FLAG_COMPRESS_MULTI).

is_imploded(Archive, FileNumber) ->
	_ValidFileNumber = util:check_file_num(Archive, FileNumber),
	Flags = block:get_flags_at_file_number(Archive, FileNumber),
	util:has_flag(Flags, ?FLAG_COMPRESS_PKZIP).
