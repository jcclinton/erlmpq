-module(tools).

-export([extract_dbc_files/3, extract_dbc_file/3, get_file_list_to/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


extract_dbc_files(MpqFileList, OutputDir, DataDir) ->
	lists:foreach(fun(File) ->
		extract_dbc_file(File, OutputDir, DataDir)
	end, MpqFileList),
	ok.

extract_dbc_file(File, OutputDir, DataDir) ->
	Filename = DataDir ++ File ++ ".MPQ",
	{ok, ArchiveInitial} = mpq:archive_open(Filename),
	{Archive, FileList} = get_file_list_to(ArchiveInitial, <<".dbc">>),

	ArchiveOut = lists:foldl(fun(FilenameIn, ArchiveIn) ->
		FilenameBin = binary:replace(FilenameIn, <<"\\">>, <<"/">>, [global]),
		Filename = binary_to_list(FilenameBin),
		Name = OutputDir ++ "/" ++ Filename,
		util:make_dir(OutputDir, Filename),
		{ok, Fd} = file:open(Name, [write, binary]),
		FileNumber = mpq:file_number(ArchiveIn, binary_to_list(FilenameIn)),
		{ArchiveOut, Buffer} = mpq:file_read(ArchiveIn, FileNumber),

		ok = file:write(Fd, Buffer),
		file:close(Fd),
		ArchiveOut
	end, Archive, FileList),
	mpq:archive_close(ArchiveOut),
	ok.



%extract file list from an archive for a given file extension
get_file_list_to(Archive, Ext) ->
	FileNumber = mpq:file_number(Archive, "(listfile)"),
	{ArchiveOut, Buffer} = mpq:file_read(Archive, FileNumber),
	FileList1 = binary:split(Buffer, <<"\r\n">>, [global]),
	FileList = lists:filter(fun(Name) ->
		if Name == <<"">> -> false;
			Name /= <<"">> ->
				% check if it has the correct extension
				NameExt = binary:part(Name, {byte_size(Name), byte_size(Ext) * -1}),
				if NameExt == Ext -> true;
					NameExt /= Ext -> false
				end
		end
	end, FileList1),
	{ArchiveOut, FileList}.
