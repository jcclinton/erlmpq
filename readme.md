## ErlMPQ
This is a library to handle reading MPQ files. This library was inspired by the C library libmpq. MPQ files are encrypted and compressed hash tables used to store data for online games.


#### API
The API is implemented in the `mpq` module. API functions include:

* `mpq:archive_open(Filename)` and `mpq:archive_open(Filename, HeaderOffset)` where `Filename` is the name, including path, of the MPQ file. Returns an archive object.
* `mpq:archive_close(Archive)` where `Archive` is the archive returned by `archive_open`
* `mpq:file_number(Archive, Filename)` where filename is the desired dbc filename. Returns FileNumber is its internal archive identifier
* `mpq:file_read(Archive, FileNumber)` returns binary data of dbc file identified by FileNumber

Any of these functions can also return `{error, Error}`. Any functions from other modules can be used, but exceptions will be uncaught.
