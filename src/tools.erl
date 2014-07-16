-module(tools).

-export([extract_dbc_files/3, extract_dbc_file/3, get_file_list_to/2]).

-include("include/binary.hrl").
-include("include/mpq_internal.hrl").


% extract dbc files from a list of mpq files
extract_dbc_files(MpqFileList, OutputDir, DataDir) ->
	lists:foreach(fun(File) ->
		extract_dbc_file(File, OutputDir, DataDir)
	end, MpqFileList),
	ok.

% extracts all dbc files from a given mpq file
% expects OutputDir to not have a trailing slash
% expects DataDir to have a trailing slash
extract_dbc_file(MpqFilename, OutputDir, DataDir) ->
	MpqFilePath = DataDir ++ MpqFilename ++ ".MPQ",
	{ok, ArchiveInitial} = mpq:archive_open(MpqFilePath),
	{Archive, FileList} = get_file_list_to(ArchiveInitial, <<".dbc">>),

	ArchiveOut = lists:foldl(fun(FilenameIn, ArchiveIn) ->
		% filenamess are in the form dbcclient//filename.dbc
		% replace // with a \
		% // is either a namespace or it is a dir separator for windows
		FilenameBin = binary:replace(FilenameIn, <<"\\">>, <<"/">>, [global]),
		Filename = binary_to_list(FilenameBin),
		Name = OutputDir ++ "/" ++ Filename,
		% recursively create any directories that need to be created
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
	%io:format("filenumber: ~p~n", [FileNumber]),
	{ArchiveOut, Buffer} = mpq:file_read(Archive, FileNumber),
	FileList1 = binary:split(Buffer, <<"\r\n">>, [global]),
	%io:format("filelist: ~p~n", [FileList1]),
	FileList = lists:filter(fun(Name) ->
		if Name == <<"">> -> false;
			Name /= <<"">> ->
				% check if it has the correct extension
				NameExt = binary:part(Name, {byte_size(Name), byte_size(Ext) * -1}),
				%io:format("ext: ~p~n", [NameExt]),
				if NameExt == Ext -> true;
					NameExt /= Ext -> false
				end
		end
	end, FileList1),
	{ArchiveOut, FileList}.
