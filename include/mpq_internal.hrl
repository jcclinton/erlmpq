-define(LIBMPQ_HEADER, 16#1A51504D).
-define(LIBMPQ_ARCHIVE_VERSION_ONE, 0).
-define(LIBMPQ_ARCHIVE_VERSION_TWO, 1).

-define(HASH_FREE, 16#FFFFFFFF).
-define(BLOCK_SIZE, 512).

-define(FLAG_EXISTS, 16#80000000).

-define(FLAG_ENCRYPTED, 16#00010000).

-define(FLAG_COMPRESSED, 16#0000FF00).
-define(FLAG_COMPRESS_PKZIP, 16#00000100).
-define(FLAG_COMPRESS_MULTI, 16#00000200).
-define(FLAG_COMPRESS_NONE, 16#00000300).

-define(FLAG_SINGLE, 16#01000000).
-define(FLAG_EXTRA, 16#04000000).


-record(header, {
	mpq_magic = 0,
	header_size,
	archive_version,
	version,
	block_size,
	hash_table_offset,
	block_table_offset,
	hash_table_count,
	block_table_count
}).

-record(header_ex, {
	extended_offset = 0,
	hash_table_offset_high = 0,
	block_table_offset_high = 0
}).

-record(hash, {
	hash_a,
	hash_b,
	locale,
	platform,
	block_table_index
}).

-record(block, {
	offset,
	packed_size,
	unpacked_size,
	flags
}).

-record(block_ex, {
	offset_high
}).

-record(file, {
	seed,
	packed_offset,
	open_count = 0
}).

-record(map, {
	block_table_indices,
	block_table_diff
}).

-record(archive, {
	fd,
	block_size,
	archive_offset,
	header,
	header_ex = #header_ex{},
	hash,
	block,
	block_ex,
	file,
	map,
	files = 0
}).
