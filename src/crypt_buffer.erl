-module(crypt_buffer).

-export([get_offset/1]).


get_offset(Offset) ->
	% add one to offset because the buffer starts at indices zero
	lists:nth(Offset+1, get_buffer()).


get_buffer() -> [
    16#55c636e2, 16#02be0170, 16#584b71d4, 16#2984f00e, 16#b682c809, 16#91cf876b,
    16#775a9c24, 16#597d5ca5, 16#5a1afeb2, 16#d3e9ce0d, 16#32cdcdf8, 16#b18201cd,
    16#3cce05ce, 16#a55d13be, 16#bb0afe71, 16#9376ab33, 16#848f645e, 16#87e45a45,
    16#45b86017, 16#5e656ca8, 16#1b851a95, 16#2542dbd7, 16#ab4df9e4, 16#5976ae9b,
    16#6c317e7d, 16#cddd2f94, 16#3c3c13e5, 16#335b1371, 16#31a592ca, 16#51e4fc4c,
    16#f7db5b2f, 16#8abdbe41, 16#8beaa674, 16#20d6b319, 16#de6c9a9d, 16#c5ac84e5,
    16#445a5feb, 16#94958cb0, 16#1e7d3847, 16#f35d29b0, 16#ca5cceda, 16#b732c8b5,
    16#fdcc41dd, 16#0edcec16, 16#9d01feae, 16#1165d38e, 16#9ee193c8, 16#bf33b13c,
    16#61bc0dfc, 16#ef3e7be9, 16#f8d4d4c5, 16#c79b7694, 16#5a255943, 16#0b3dd20a,
    16#9d1ab5a3, 16#cfa8ba57, 16#5e6d7069, 16#cb89b731, 16#3dc0d15b, 16#0d4d7e7e,
    16#97e37f2b, 16#fefc2bb1, 16#f95b16b5, 16#27a55b93, 16#45f22729, 16#4c986630,
    16#7c666862, 16#5fa40847, 16#a3f16205, 16#791b7764, 16#386b36d6, 16#6e6c3fef,
    16#c75855db, 16#4abc7dc7, 16#4a328f9b, 16#cef20c0f, 16#60b88f07, 16#f7bb4b8f,
    16#830b5192, 16#94f711ec, 16#20250752, 16#399d21a3, 16#e5c0840d, 16#e76cffa5,
    16#624fab29, 16#5df133e6, 16#83e0b9b8, 16#c5796bfb, 16#4a7ab2d0, 16#ba59a821,
    16#03a81e4c, 16#cd3adfdb, 16#32b26b8c, 16#8e35c533, 16#9e6300e9, 16#8cf92ac5,
    16#880d18eb, 16#131a53b3, 16#2ed2dc64, 16#b23257c1, 16#a06450c1, 16#1b92cb8e,
    16#72ed730e, 16#19a685f0, 16#82836483, 16#42d94e8a, 16#ee9bd6f6, 16#556d0b6a,
    16#ba65589a, 16#de24cce4, 16#53329f6c, 16#c754fe8b, 16#503d2dc7, 16#10027ba4,
    16#d3b60a8b, 16#68e68d83, 16#0a9128a9, 16#595fa35f, 16#0b03b5be, 16#150a45c4,
    16#b1629cce, 16#e5f7497b, 16#8a7098a4, 16#b8233e69, 16#8ea0f978, 16#5b579970,
    16#eab14318, 16#4b28b263, 16#b6766cef, 16#06782877, 16#155c6dd0, 16#c711333c,
    16#f819cedf, 16#00eb1d68, 16#d6fffa6e, 16#439e5962, 16#d765d6db, 16#cb0bcee9,
    16#6d3c5647, 16#965466f3, 16#0ca983c9, 16#74ecc1ce, 16#fc0563b6, 16#42b08fee,
    16#c5b38853, 16#fe502ceb, 16#7b432faf, 16#c309e610, 16#2c3997d8, 16#43774654,
    16#15bd9d2c, 16#ed6a420d, 16#c7ff520c, 16#b8a97fd1, 16#5e4d60cc, 16#b9738d11,
    16#da2181ff, 16#73ac2597, 16#3a8eec8d, 16#ac85e779, 16#f3f975d6, 16#b9fe7b91,
    16#0f155d1e, 16#2860b6dd, 16#835977cb, 16#b0607436, 16#9cab7f6b, 16#8ab91186,
    16#c12b51e9, 16#20084e8b, 16#44ba8ead, 16#a542b130, 16#82bcd5c4, 16#cc747f4e,
    16#0f1909d8, 16#da242e1c, 16#6f7d1aa0, 16#d2626486, 16#88d0781e, 16#ab695ccd,
    16#fa569145, 16#b4feb55c, 16#be47e896, 16#e70a7a88, 16#d56185a2, 16#acf4c871,
    16#09282332, 16#1ddeeaa8, 16#590c7adb, 16#f4a97667, 16#bfd85705, 16#0ea77ccc,
    16#a9f85364, 16#83195869, 16#8bfb041a, 16#db842f5c, 16#d6f0f315, 16#a7756ea7,
    16#0a51b439, 16#a9edf8a3, 16#d9084e2f, 16#827407f8, 16#d4ac8284, 16#09739d0d,
    16#b3bb6cfc, 16#d539c77d, 16#6bbc9ac0, 16#35c641aa, 16#934c96b0, 16#d17af317,
    16#29c6baef, 16#b275cdac, 16#d72662de, 16#9f5c2544, 16#c1a98f75, 16#d98e8f9a,
    16#47bd5c86, 16#70c610a6, 16#b5482ed4, 16#23b9c68c, 16#3c1bae66, 16#69556e7f,
    16#d902f5e0, 16#653d195b, 16#de6541fb, 16#07bcc6ac, 16#c6ee7788, 16#801534d4,
    16#2c1f35c0, 16#d9de614d, 16#bdccac85, 16#b4d4a0da, 16#242d549b, 16#9d964796,
    16#b9ceb982, 16#59fa99a9, 16#d8986cc1, 16#9e90c1a1, 16#01bbd82f, 16#d7f1c5fd,
    16#dd847eba, 16#883d305d, 16#25f13152, 16#4a92694d, 16#77f1e601, 16#8024e6e7,
    16#02a5f53d, 16#9c3ef4d9, 16#af403ccc, 16#e2ad03c0, 16#46edf6ec, 16#6f9bd3e6,
    16#cc24ad7a, 16#47afab12, 16#82298df7, 16#708c9eec, 16#76f8c1b1, 16#b39459d2,
    16#3f1e26d9, 16#e1811be7, 16#56ed1c4d, 16#c9d18af8, 16#e828060e, 16#91cada2e,
    16#5ccbf9b7, 16#f1a552d4, 16#3c9d4343, 16#e1008785, 16#2adfeebf, 16#f90240a0,
    16#3d08cce7, 16#426e6fb0, 16#573c984f, 16#13a843ae, 16#406b7439, 16#636085d9,
    16#5000ba9a, 16#ad4a47ab, 16#af001d8d, 16#419907ae, 16#185c8f96, 16#e5e9ed4d,
    16#61764133, 16#d3703d97, 16#ac98f0c6, 16#dbc3a37c, 16#85f010c4, 16#90491e32,
    16#f12e18bf, 16#c88c96e1, 16#d3fbd6d9, 16#e3c28b08, 16#d5bf08cc, 16#b1e78859,
    16#2546ddcf, 16#b030b200, 16#aafd2811, 16#55b22d21, 16#d38bf567, 16#469c7a2b,
    16#5ad05792, 16#a1a5981e, 16#7dfb8384, 16#34d1ca0a, 16#7eb0dbe0, 16#d61ce0f6,
    16#398068b7, 16#e6406d1f, 16#95ae6b47, 16#e4281230, 16#b0843061, 16#a70a3a68,
    16#e340f625, 16#72dcbffd, 16#8eb8afcd, 16#18b6661f, 16#17ef5a5c, 16#000c5b22,
    16#6ba13836, 16#6165e383, 16#74481c5b, 16#e56f0711, 16#a26f5024, 16#5ff22e60,
    16#31a5e829, 16#a1094bf0, 16#c680ec6c, 16#8cf327d7, 16#ebf1348a, 16#6a227d2f,
    16#74065184, 16#8df65112, 16#2bbd05ee, 16#e4d00ed6, 16#2980ee1a, 16#6ae1da73,
    16#e84614da, 16#6c9906ab, 16#cf8e02db, 16#d3723e97, 16#92f66caf, 16#ac8491c7,
    16#aec65696, 16#b98997cf, 16#fa16c762, 16#6d73c65f, 16#205d22a6, 16#4dd3aaa5,
    16#2deb6bc0, 16#9f37686c, 16#71a5282b, 16#376bb9e0, 16#7fff2a1b, 16#de67982f,
    16#9cbf33ce, 16#2e6dab37, 16#6e3424b9, 16#0ee143bc, 16#832a60d9, 16#bb6329e1,
    16#13f6befd, 16#5965fb84, 16#f60b233c, 16#3d695183, 16#433224a1, 16#b5d9cae5,
    16#82459bab, 16#9f21b311, 16#af6c5247, 16#b447b13a, 16#7b2676c3, 16#c38979cd,
    16#8526ae25, 16#c550ad5b, 16#685099a7, 16#65e9c2bd, 16#e5c6dc36, 16#e10b37a9,
    16#88016878, 16#ce81d4e4, 16#24d6fc80, 16#4106152d, 16#6d4f5f90, 16#c4dc74be,
    16#db48676c, 16#6cb569b7, 16#f3bf598f, 16#042b08d9, 16#02ccb2de, 16#b1056f65,
    16#47994af4, 16#fa141ba4, 16#9376ab2e, 16#07a76737, 16#75e7e6fc, 16#449d80a1,
    16#03b7259d, 16#f6df358a, 16#5a75d5b9, 16#47286923, 16#3b1a30ef, 16#eebe3d6a,
    16#9db1aa00, 16#007a90d9, 16#24667071, 16#019c73cf, 16#69039bcd, 16#95900744,
    16#6518b1eb, 16#6905f202, 16#ee3951b2, 16#e141fca9, 16#797fa832, 16#5a95e55b,
    16#d6263b15, 16#5b61f394, 16#897acb1c, 16#005f83a9, 16#22420f71, 16#f495176e,
    16#7e138f3d, 16#1392e384, 16#373bf7aa, 16#8e512816, 16#a960b3ca, 16#0474d74c,
    16#ffacd6d7, 16#2ef5ed9e, 16#60992aaa, 16#7e690e99, 16#23c0749d, 16#d8e29105,
    16#555d5909, 16#15631bfe, 16#a69c5a1c, 16#501017ca, 16#99438048, 16#38733ac7,
    16#e682e2c8, 16#d4655fd6, 16#956e4c04, 16#347df643, 16#2f4b177b, 16#93ed3aa4,
    16#a77e1dd5, 16#7ae55702, 16#d2a52fd9, 16#ef8ba18c, 16#b7d3c1ee, 16#8078ba8d,
    16#ab5aaadb, 16#752be08f, 16#068b31c1, 16#078aae3c, 16#aa5a8343, 16#123d9268,
    16#2ceaee43, 16#8ebdb239, 16#650251f3, 16#04883648, 16#8c62e12e, 16#12b32167,
    16#e5112e9a, 16#10002548, 16#3e7a818d, 16#077e5327, 16#f140cc21, 16#6ce7d75d,
    16#9b99f9a5, 16#3215741c, 16#b6aadbae, 16#738768dc, 16#82a3742f, 16#76517020,
    16#dd872ad8, 16#9d0902b2, 16#7d1a6b04, 16#49381592, 16#63a652a5, 16#0c15e626,
    16#e22f70d6, 16#01e84385, 16#b29de134, 16#20c5000e, 16#e961f443, 16#2d31662e,
    16#3ce6bc28, 16#34f9dd94, 16#fa45de53, 16#497588bd, 16#9468215b, 16#0777fa5c,
    16#6f7114c0, 16#e0e82694, 16#e4371986, 16#57112de2, 16#e0cac289, 16#f2a3cee0,
    16#6a41e1b9, 16#bfcea77d, 16#f927fd52, 16#69747d98, 16#bea76cdb, 16#8dd39557,
    16#04db5ece, 16#2a0885c8, 16#3be4e8ee, 16#21d785dc, 16#09de7c0e, 16#3258ea33,
    16#51922982, 16#ee8dd024, 16#3df6965d, 16#30c1237b, 16#f7f6686a, 16#9faca186,
    16#7c400076, 16#85acef8a, 16#f4b6d220, 16#ddc3481c, 16#439eaec4, 16#717bbe63,
    16#8259faa7, 16#d682bd68, 16#932a8610, 16#38bf0a7f, 16#6212e2c7, 16#88ee3168,
    16#b3c27047, 16#6133cb1e, 16#15295506, 16#5ae66246, 16#1d208ddd, 16#a91d3dba,
    16#c315968d, 16#6aa2664b, 16#716d0cca, 16#891f4956, 16#80866bff, 16#bd56c847,
    16#9093425a, 16#28dd9e87, 16#84ef3e08, 16#690a49d6, 16#6a7eff82, 16#abcfe400,
    16#3d3be5ca, 16#381b650c, 16#4b7c8622, 16#3e0246f3, 16#a3561654, 16#9488865c,
    16#3aef1bf2, 16#5e5d68a2, 16#d32f1ddc, 16#51972bf0, 16#177a213b, 16#469375c2,
    16#37640bd0, 16#fc3324c8, 16#07091a09, 16#2d63d3fb, 16#2153f023, 16#48223875,
    16#61a55826, 16#8c136538, 16#49f71d98, 16#84c7d51e, 16#85551a73, 16#13d604c5,
    16#d701a626, 16#87b844ca, 16#741eb29d, 16#2a2c977c, 16#c797ca03, 16#6c4085d7,
    16#2dacf79b, 16#734fa2eb, 16#cc290557, 16#fa1e75e4, 16#06b29a27, 16#bece2a7a,
    16#70a4554b, 16#c935942e, 16#a764bbc1, 16#1fe391d6, 16#7807f0c2, 16#40606ed9,
    16#e5153086, 16#e91d7dd2, 16#ed5d3ba9, 16#aa14b64a, 16#83b24dd9, 16#ec1ff5cd,
    16#ba33ead3, 16#e4ef735c, 16#bc062438, 16#d8bfd523, 16#473d1e04, 16#2007f8a7,
    16#b02903ed, 16#86ea8ada, 16#95ab69cf, 16#fd1f9809, 16#9cb3d8bb, 16#51f45958,
    16#9cdd4276, 16#c245865e, 16#8f0c836b, 16#4ee7dc07, 16#f6368d9d, 16#ef2c1dc1,
    16#ee56b54b, 16#bd62ce2f, 16#f4916aad, 16#c81cb594, 16#41729f49, 16#24bef0a4,
    16#def487a9, 16#222e05b8, 16#8d3bf5c6, 16#11b55009, 16#ad09d2b3, 16#19db9fd1,
    16#d7427085, 16#33dbfc8b, 16#526b9378, 16#790e1bc8, 16#b2998a00, 16#a5641703,
    16#0676d249, 16#6b9185cc, 16#30e4348f, 16#82c52f65, 16#57c7dc24, 16#489c1ecd,
    16#9fcab02a, 16#56d61117, 16#fe869cac, 16#55fc5140, 16#7fbbb382, 16#9e5afc79,
    16#10047c99, 16#fc9f5984, 16#56587e2d, 16#b98193f0, 16#98fe5e8e, 16#29b15b6b,
    16#9561f055, 16#bb0caa25, 16#1e4ecc15, 16#23f5393b, 16#0845b458, 16#ceff67ca,
    16#b099900c, 16#00b1564f, 16#39eef3d1, 16#fcc1bf84, 16#ac8893b5, 16#6484bf0e,
    16#91c02ab3, 16#8c0c0c70, 16#686fa8c6, 16#e171bed6, 16#dfae37df, 16#d5a1a4e7,
    16#e3eb49a1, 16#5e6014e0, 16#205b21ac, 16#fd58b3da, 16#2e7c07cd, 16#ef2cc85a,
    16#d7587b46, 16#f417847d, 16#8a30cec1, 16#70984f6c, 16#f0b63388, 16#c220c98d,
    16#ede62936, 16#92c0a7b3, 16#1ef371e8, 16#2005f7af, 16#91a47265, 16#b0cf5504,
    16#d500aba8, 16#cb5c4bd3, 16#9b3bcbc3, 16#cf6644b5, 16#ce9488ef, 16#003fc96e,
    16#aa42222f, 16#4844f3d0, 16#4db89d77, 16#08681aae, 16#662f3a28, 16#761552db,
    16#1df7a17a, 16#93feed9a, 16#cc496a4f, 16#a217cfcd, 16#3ba3c930, 16#268f7e77,
    16#0797b4a1, 16#8bebfc51, 16#068930c4, 16#16c874e2, 16#c242da24, 16#fb229f76,
    16#a0795b02, 16#689fc036, 16#17a73732, 16#d21aec00, 16#ac00a692, 16#5b217f18,
    16#ae421624, 16#2bc05cc0, 16#48c1db7a, 16#4f4e63b4, 16#1667f04e, 16#34020f94,
    16#972b2555, 16#9a07355b, 16#01665970, 16#7db60c6f, 16#3ad7103b, 16#5c3d09c0,
    16#eea3dada, 16#88c21c10, 16#102436d7, 16#6a3b3400, 16#eb523c4c, 16#fb97d896,
    16#964cb86b, 16#dd878038, 16#0529da4d, 16#0b1468a5, 16#18739ac8, 16#f7f26668,
    16#f64f4471, 16#5c14f5c3, 16#44a081fb, 16#39ac7e37, 16#8a17c26b, 16#868f5e67,
    16#3931978d, 16#6edf7817, 16#4951cc67, 16#943407f3, 16#cc5e748f, 16#2b7ee729,
    16#cbb320f0, 16#11fec8e7, 16#fccfc658, 16#03454354, 16#373aa1ec, 16#1d58fe9a,
    16#064710ae, 16#a88aa0ba, 16#d183a23e, 16#40d150a3, 16#f531b8d1, 16#a7d99f85,
    16#11838cd5, 16#b19e64b3, 16#3d67a5e9, 16#b02c5ac6, 16#99b9b9e8, 16#4c202b7a,
    16#15f261d3, 16#a84c2d0d, 16#50f185a6, 16#33ba41d5, 16#39791013, 16#4baff44e,
    16#eeeeaa1c, 16#e0488314, 16#559ccd2b, 16#a104f445, 16#636f37c4, 16#264d5e3b,
    16#75c17f35, 16#75424131, 16#bb115739, 16#74fe755a, 16#7d3a7aa6, 16#2d8be784,
    16#83ed154a, 16#fc2673d8, 16#44dd4a7f, 16#79056cc8, 16#82cc8831, 16#9d3c1b7c,
    16#e9453bfa, 16#24315694, 16#661f3253, 16#75549f5c, 16#bb2b63ed, 16#67e00d96,
    16#f48966c7, 16#0d7bea56, 16#c25f92ef, 16#a947a79d, 16#de4adf6f, 16#ac0f0342,
    16#d3eb246b, 16#a4aa118e, 16#3c3e6a46, 16#457f4441, 16#a50a406f, 16#6c508d9f,
    16#e9ac18e7, 16#1ecdb4ba, 16#39ac7e3a, 16#7fb304fa, 16#6f38f8e8, 16#4aecea6d,
    16#61035e73, 16#81708907, 16#ebc07205, 16#90fd7614, 16#b52d217f, 16#6c4de195,
    16#1dd49084, 16#64ee482c, 16#94c7a521, 16#540c09d8, 16#75df8dd5, 16#414131f7,
    16#3698fd76, 16#f784db4f, 16#f8c97a03, 16#048f39b9, 16#3bf4f0bd, 16#8cb50992,
    16#9b58d9ee, 16#e5ab79cc, 16#9a5f6052, 16#bd9591b0, 16#fad2232b, 16#5a632254,
    16#0286e618, 16#8ad3c8f7, 16#e4060176, 16#754c4617, 16#5c10490b, 16#6f7d6fff,
    16#2187b42a, 16#5775095b, 16#02f4c663, 16#5a5dca06, 16#fe4ad4c7, 16#53e19f7d,
    16#59ff46b5, 16#bcc42ba5, 16#fd2f4a97, 16#bed6d905, 16#95629b6b, 16#21a1c0db,
    16#aa10b45d, 16#e6ef6d58, 16#2892cf4d, 16#9fed6c10, 16#1e386bf7, 16#9be0c6e8,
    16#2b2f15ef, 16#19f5ac7b, 16#7aff0e72, 16#31da576f, 16#30252cb4, 16#577960ac,
    16#166e9e5a, 16#a9374a61, 16#71369c96, 16#7ff826ae, 16#e8175326, 16#cabbfd33,
    16#0191190e, 16#699d3c3e, 16#36b40b22, 16#b3950513, 16#9b889bfa, 16#a52a5007,
    16#ac290fed, 16#3b4e4a4f, 16#b753d8d6, 16#3c531f22, 16#582f6427, 16#a9cd93a9,
    16#546e39ae, 16#242faad2, 16#d2e0f747, 16#09f6325d, 16#59d48719, 16#ad7eb66e,
    16#d5512878, 16#56debf9d, 16#5107e5a5, 16#f1c00aa4, 16#814ccca8, 16#600d90f0,
    16#9be97619, 16#915fa5f2, 16#2b5628dd, 16#a33d5f5a, 16#595df7c1, 16#6966215d,
    16#50ec8337, 16#f1d21372, 16#0ee2eefb, 16#ad9e70b7, 16#ab0d2fe4, 16#cf277b5d,
    16#62585a2c, 16#835a7844, 16#74b1fa6b, 16#49baffd5, 16#2ea9c864, 16#129311a8,
    16#bdfa1867, 16#83ca5997, 16#9d1db719, 16#84bb79e6, 16#9e3f99f2, 16#313f6101,
    16#1b99245b, 16#d15d8fb2, 16#cef90f81, 16#2945268d, 16#dbbcf573, 16#b1021886,
    16#9ee7ec1d, 16#1cf824f7, 16#7eaa2e32, 16#69c0a2b5, 16#7494419c, 16#e253d7d3,
    16#48da3d12, 16#45b8b571, 16#db4d147a, 16#d82d8dde, 16#265d10a2, 16#b0a6eb9a,
    16#7e1c93a6, 16#36fe2f46, 16#dcad6b00, 16#05439191, 16#b0ce5484, 16#61d1c309,
    16#8da62a03, 16#06d0fe2f, 16#bac6dd3c, 16#ca2006f3, 16#8321b1af, 16#0411a6f3,
    16#e8918eac, 16#21a2c152, 16#91c0d54f, 16#6aaa14fa, 16#dd22a440, 16#88cb2075,
    16#7a4eb813, 16#67afa071, 16#d8d98c9c, 16#31f10d47, 16#6ff1a8a8, 16#2faaf0a1,
    16#48a221bb, 16#3be6948b, 16#aa79e79b, 16#0ea7278c, 16#7a3857ef, 16#49b7fe55,
    16#d51cb931, 16#041c018d, 16#00b90501, 16#45ea7881, 16#8fc1dbcf, 16#b80b32a9,
    16#abacd2e9, 16#677bdc40, 16#ecace542, 16#6d6514eb, 16#31c09ff7, 16#5e6c1abd,
    16#1c391d0f, 16#0e9d77f1, 16#7119392d, 16#6be9b0ba, 16#6194fa77, 16#45e62148,
    16#42234af2, 16#c3239d66, 16#939cbdbc, 16#56200d9c, 16#6b275208, 16#001a61f3,
    16#ccc2a546, 16#4b722be0, 16#ee25f2b7, 16#6d86cf9e, 16#aa6be0cd, 16#4dcda7b6,
    16#78d4aa13, 16#36ea7ad9, 16#3f29d700, 16#deea2d84, 16#6a6af5bd, 16#18afb81c,
    16#d8e4e73c, 16#8aa708ba, 16#658b94d9, 16#a676478c, 16#cfa10c22, 16#25593c74,
    16#8d962235, 16#5f980270, 16#3df6ebc0, 16#8e7d92fa, 16#c3ee55e1, 16#d5f72447,
    16#02b0fa95, 16#52b0b520, 16#70d2c11f, 16#3a6fdd6c, 16#193aa698, 16#5496f7d5,
    16#4208931b, 16#7a4106ec, 16#83e86840, 16#f49b6f8c, 16#ba3d9a51, 16#55f54ddd,
    16#2de51372, 16#9afb571b, 16#3ab35406, 16#ad64ff1f, 16#c77764fe, 16#7f864466,
    16#416d9cd4, 16#a2489278, 16#e30b86e4, 16#0b5231b6, 16#ba67aed6, 16#e5ab2467,
    16#60028b90, 16#1d9e20c6, 16#2a7c692a, 16#6b691cdb, 16#9e51f817, 16#9b763dec,
    16#3d29323f, 16#cfe12b68, 16#754b459b, 16#a2238047, 16#d9c55514, 16#6bdcffc1,
    16#693e6340, 16#82383fe7, 16#1916ea5f, 16#ec7bcd59, 16#72de165a, 16#e79a1617,
    16#8ec86234, 16#a8f0d284, 16#20c90226, 16#7bf98884, 16#28a58331, 16#3ec3fa6e,
    16#4ce0895b, 16#c353b4d0, 16#33ef064f, 16#21e5e210, 16#c8bb589d, 16#e85dcab2,
    16#ac65829f, 16#a7bf92d0, 16#05a6174d, 16#25a50c2e, 16#e5c78777, 16#3d75021f,
    16#4baa9c98, 16#23bdc884, 16#9653bbd7, 16#badce7f5, 16#c283a484, 16#c040df2e,
    16#9370a841, 16#2f316022, 16#36eed231, 16#ac2cbc0c, 16#13c0a49b, 16#cdd12997,
    16#07fe91b2, 16#cd7eabcd, 16#2c01271d, 16#18432df8, 16#599c6bc7, 16#75e93d5a,
    16#b67a6ee2, 16#8e738e16, 16#ff9073fd, 16#af77026a, 16#f86ea2fc, 16#91509ea3,
    16#33a78dc6, 16#4f79234a, 16#3a7535bc, 16#3539fcb1, 16#3103ee52, 16#4f6f1e69,
    16#6bb3ebbc, 16#4cb77555, 16#8dd1e999, 16#2ade439d, 16#11521fae, 16#b94d2545,
    16#8dde9abd, 16#1909393f, 16#b792a23d, 16#749c455b, 16#b5b60f2c, 16#380459ce,
    16#0dad5820, 16#b130845b, 16#291cbd52, 16#de9a5bb7, 16#51def961, 16#515b6408,
    16#ca6e823e, 16#382e6e74, 16#eebe3d71, 16#4c8f0c6a, 16#e676dcea, 16#14e1dc7c,
    16#6f7fc634, 16#cf85a943, 16#d39ea96e, 16#136e7c93, 16#7164b304, 16#f32f1333,
    16#35c34034, 16#de39d721, 16#91a87439, 16#c410111f, 16#29f17aac, 16#1316a6ff,
    16#12f194ee, 16#420b9499, 16#f72db0dc, 16#690b9f93, 16#17d14bb2, 16#8f931ab8,
    16#217500bc, 16#875413f8, 16#98b2e43d, 16#c51f9571, 16#54cebdca, 16#0719cc79,
    16#f3c7080d, 16#e4286771, 16#a3eab3cd, 16#4a6b00e0, 16#11cf0759, 16#7e897379,
    16#5b32876c, 16#5e8cd4f6, 16#0cedfa64, 16#919ac2c7, 16#b214f3b3, 16#0e89c38c,
    16#f0c43a39, 16#eae10522, 16#835bce06, 16#9eec43c2, 16#ea26a9d6, 16#69531821,
    16#6725b24a, 16#da81b0e2, 16#d5b4ae33, 16#080f99fb, 16#15a83daf, 16#29dfc720,
    16#91e1900f, 16#28163d58, 16#83d107a2, 16#4eac149a, 16#9f71da18, 16#61d5c4fa,
    16#e3ab2a5f, 16#c7b0d63f, 16#b3cc752a, 16#61ebcfb6, 16#26ffb52a, 16#ed789e3f,
    16#aa3bc958, 16#455a8788, 16#c9c082a9, 16#0a1bef0e, 16#c29a5a7e, 16#150d4735,
    16#943809e0, 16#69215510, 16#ef0b0da9, 16#3b4e9fb3, 16#d8b5d04c, 16#c7a023a8,
    16#b0d50288, 16#64821375, 16#c260e8cf, 16#8496bd2c, 16#ff4f5435, 16#0fb5560c,
    16#7cd74a52, 16#93589c80, 16#88975c47, 16#83bda89d, 16#8bcc4296, 16#01b82c21,
    16#fd821dbf, 16#26520b47, 16#04983e19, 16#d3e1ca27, 16#782c580f, 16#326ff573,
    16#c157bcc7, 16#4f5e6b84, 16#44ebfbfb, 16#da26d9d8, 16#6cd9d08e, 16#1719f1d8,
    16#715c0487, 16#2c2d3c92, 16#53faaba9, 16#bc836146, 16#510c92d6, 16#e089f82a,
    16#4680171f, 16#369f00de, 16#70ec2331, 16#0e253d55, 16#dafb9717, 16#e5dd922d,
    16#95915d21, 16#a0202f96, 16#a161cc47, 16#eacfa6f1, 16#ed5e9189, 16#dab87684,
    16#a4b76d4a, 16#fa704897, 16#631f10ba, 16#d39da8f9, 16#5db4c0e4, 16#16fde42a,
    16#2dff7580, 16#b56fec7e, 16#c3ffb370, 16#8e6f36bc, 16#6097d459, 16#514d5d36,
    16#a5a737e2, 16#3977b9b3, 16#fd31a0ca, 16#903368db, 16#e8370d61, 16#98109520,
    16#ade23cac, 16#99f82e04, 16#41de7ea3, 16#84a1c295, 16#09191be0, 16#30930d02,
    16#1c9fa44a, 16#c406b6d7, 16#eedca152, 16#6149809c, 16#b0099ef4, 16#c5f653a5,
    16#4c10790d, 16#7303286c
].
