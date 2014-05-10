#-------------------------------------------------------------------------
# NSF Memory Specification
#-------------------------------------------------------------------------

#*************************************************************************
MEMORY {
#*************************************************************************

#-------------------------------------------------------------------------
# Program (5K)
#-------------------------------------------------------------------------
	HEADER:	start = $7F80, size = $80, fill = yes;		# NSF header
	PROG:	start = $8000, size = $1000, fill = yes;	# main prog
	PROGH:	start = $9000, size = $c00, fill = yes;		# vt region
	
#-------------------------------------------------------------------------
# Memory
#-------------------------------------------------------------------------
	
	ZMEM:		start = $0000, size = 128;		# 128 bytes data
	ZCODE:		start = $0080, size = 128;		# 128 bytes code
	RAMCODE:	start = $0200, size = 512;		# 512 bytes code
	MEM:		start = $0400, size = 512;		# 512 bytes data
}

#*************************************************************************
SEGMENTS {
#*************************************************************************

#-------------------------------------------------------------------------
# standard segments
#-------------------------------------------------------------------------
	HEADER:					# NSF header
		load = HEADER, 		#
		type = ro;			#
		
	CODE:					# 4k main program
		load = PROG, 		#
		type = ro, 			#
		define = yes, 		#
		align=$100;			#
		
	ZPCODE:					# 128b zpage code
		load = PROG, 		#
		run = ZCODE, 		#
		type = ro, 			#
		define = yes;		#
	
	RAMCODE:				# 512b ram code
		load = PROG,		#
		run = RAMCODE,		#
		type = ro,			#
		define = yes;		#
		
	TABLES:					# 3k table region
		load = PROGH, 		#
		type = ro, 			#
		define = yes;		#
	
	ZEROPAGE:				# 128b zpage data
		load = ZMEM,		#
		type = zp;			#
		
	BSS:					# 512b ram data
		load = MEM,			#
		type = bss;			#
	
}
