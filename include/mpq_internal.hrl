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
	extended_offset,
	hash_table_offset_high,
	block_table_offset_high
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
	open_count
}).

-record(map, {
	block_table_indices,
	block_table_diff
}).

-record(archive, {
	fd,
	block_size,
	archive_offset,
	header = #header{},
	header_ex = #header_ex{},
	hash = #hash{},
	block = #block{},
	block_ex = #block_ex{},
	file = #file{},
	map = #map{},
	files = 0
}).
