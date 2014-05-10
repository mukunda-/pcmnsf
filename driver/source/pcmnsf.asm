;------------------------------------------------------------------------------
; PCMNSF (C) 2010 Mukunda Johnson, Andrew Richards
;------------------------------------------------------------------------------

	.import __CODE_SIZE__
	.import __CODE_LOAD__
	.import __ZPCODE_LOAD__
	.import __ZPCODE_RUN__
	.import __ZPCODE_SIZE__
	.import __RAMCODE_LOAD__
	.import __RAMCODE_RUN__
	.import __RAMCODE_SIZE__

;=================================================================================
	.segment "HEADER"
;=================================================================================
  .byte		"NESM", 1Ah			; marker			//
  .byte		01h				; version			//
  .byte		01h				; number of songs		//
  .byte		01h				; starting song			//
  .word		__CODE_LOAD__			; load address			//
  .word		__init				; init address			//
  .word		__play				; play address			//
  .byte		"hurf durf"			; song name			//
  .res		32-9, 0				;				//
  .res		32, 0				; artist			//
  .res		32, 0				; copyright			//
  .word		0411Ah				; NTSC speed (60hz)		//
  .byte		0, 1, 0, 0, 0, 0, 0, 0		; bankswitch init values	//
  .word		0411Ah				; PAL speed (60hz !)		//
  .byte		0				; PAL/NTSC bits			//
  .byte		0				; EXT chip support		//
  .byte		0, 0, 0, 0			; expansion bytes		//
;=================================================================================

;____________________________________
;// memory bank access:             /
;//----------------------------------.
;// [8xxx] Program Code		     /
;// [9xxx] Volume Mul. Table	     /
;// [Axxx] Channel 1 Sample Window   /	
;// [Bxxx] Channel 2 Sample Window   /
;// [Cxxx] Channel 3 Sample Window   /
;// [Dxxx] Channel 4 Sample Window   /
;// [Exxx] Unused		     /
;// [Fxxx] Sequencer Data Window     /
;//----------------------------------'


;------------------------------------------------------------------------------
; registers used
;------------------------------------------------------------------------------
APU_CTRL	= 4015h
DAC		= 4011h

;------------------------------------------------------------------------------
; total cycle measurements
;------------------------------------------------------------------------------
.define CYCLES_FOR_MIXING 126	; adjust for sampling rate/load (sr=1789773/cps)
.define CYCLES_FOR_UPDATE 40	; cps = both measurements added

;------------------------------------------------------------------------------
; macro for returning from update routines
;
; jumps to proper return address depending on how many cycles were used
;------------------------------------------------------------------------------
.macro uret vector, cycles
	lda	#<vector		;2
	.if (cycles & 1) = 0		;
		sta	update_vector	;+3 make odd
		
		.if (cycles+2+3+3) <= CYCLES_FOR_UPDATE
			jmp	mixing_complete-((CYCLES_FOR_UPDATE-(cycles+2+3+3))/2)
		.else
			jmp	mixing_complete
		.endif
	
	.else
		; if cycles exceed CFU then just cheat!
		;
		sta	update_vector		;+3 make even
		.if (cycles+2+4+3) <= CYCLES_FOR_UPDATE
			jmp	mixing_complete-((CYCLES_FOR_UPDATE-(cycles+2+4+3))/2)
		.else
			jmp	mixing_complete
		.endif
	
	.endif
.endmacro

.define nom nop ; nom nom nom

;------------------------------------------------------------------------------
; debug probe
;------------------------------------------------------------------------------
.macro probe
	.local @PROBE
@PROBE: jmp @PROBE
.endmacro
	
;==============================================================================
	.zeropage
;==============================================================================

; Data located in code:
;   sample read addresses
;   volume map addresses
;   channel rates
;

;------------------------------------------------------------------------------
; channel variables
;------------------------------------------------------------------------------
.define ch_volmap_l(i) __mixing_start__ + 4 + i*6	; volume table address (L)
.define ch_volmap_h(i) __mixing_start__ + 5 + i*6	; volume table address (H)
.define ch_read_l(i) __mixing_start__ + 1 + i*6		; sample read position (L)
.define ch_read_h(i) __mixing_start__ + 2 + i*6		; sample read position (H)
.define ch_frac(i) __ch_frac+i

;------------------------------------------------------------------------------
; resampling rates
;------------------------------------------------------------------------------
.define ch_rate_l(i) __resampling_start__ + 3 + i*((__resampling_end__-__resampling_start__)/4)
.define ch_rate_h(i) __resampling_start__ + 9 + i*((__resampling_end__-__resampling_start__)/4)

;------------------------------------------------------------------------------
; other channel vars
;------------------------------------------------------------------------------
.define ch_read_b(i) __ch_read_b + i			; source bank numbers
.define ch_rate_cache_l(__i__) __ch_rate_cache_l+__i__	; rate cache (for temp storage)
.define ch_rate_cache_h(__i__) __ch_rate_cache_h+__i__	;
.define ch_sample(__i) __ch_sample+__i			; sample index

;------------------------------------------------------------------------------
; channel data reservation
;------------------------------------------------------------------------------
__ch_rate_cache_l: .res 4
__ch_rate_cache_h: .res 4
__ch_sample:	   .res 4
__ch_read_b:	   .res 4
__ch_frac:	   .res 4

;------------------------------------------------------------------------------
; other variables
;------------------------------------------------------------------------------
cbits:		.res 1 ; sequencer data bits
data1:		.res 1 ; general purpose storage
data2:		.res 1
data3:		.res 1
data4:		.res 1
data5:		.res 1
data6:		.res 1
pattern:	.res 3 ; sequencer data read address
pattern_L:	.res 1 ; lower index (pattern[0] is always 0)

;==============================================================================
	.segment "TABLES"
;==============================================================================

;------------------------------------------------------------------------------
; 64x64 multiplication table
;------------------------------------------------------------------------------
volume_table_start:
.incbin "python/volume_table.bin"

;------------------------------------------------------------------------------
; output saturation table
;------------------------------------------------------------------------------
saturation_table:
.incbin "python/saturation_table.bin"

;==============================================================================
	.segment "ZPCODE"
;==============================================================================

; (quick-to-modify 'zeropage' code)

;------------------------------------------------------------------------------
mix_next_sample:
;------------------------------------------------------------------------------

;-----------------------------------------------------------------------------
__mixing_start__:
;-----------------------------------------------------------------------------
	
				;  | mix channel samples together
	ldx	$aabb		; 4|<- ch_read_l/h(0)
	lda	$aabb, x	; 4|<- ch_volmap_l/h(0)
	ldx	$aabb		; 4|1..
	adc	$aabb, x	; 4|
	ldx	$aabb		; 4|2...
	adc	$aabb, x	; 4|
	ldx	$aabb		; 4|3....
	adc	$aabb, x	; 4| (32 cycles)
;-----------------------------------------------------------------------------
	tax				; 2| saturate result
	lda	saturation_table, x 	; 4|
;-----------------------------------------------------------------------------
	sta	DAC			; 4| write to DAC
;-----------------------------------------------------------------------------
__update_jump:
	jmp	(update_table)	; 5| *main update vector (PATCH HERE!)
				; (47 cycles total)
				
update_vector := (__update_jump+1)
	
;==============================================================================
	.segment "RAMCODE"
;==============================================================================

;------------------------------------------------------------------------------
; nop instructions to waste extra cycles
;------------------------------------------------------------------------------
.repeat 30
	clc
.endrep
	
;------------------------------------------------------------------------------
mixing_complete:
;------------------------------------------------------------------------------
	
; first #$aa = ch_rate_l
; second #$aa = ch_rate_h
; third #0 is actually zero ;)
	
;------------------------------------------------------------------------------
.macro do_resample idx, over_function, next	
;------------------------------------------------------------------------------
	lda	<ch_frac(idx)		;  3| read += rate
	adc	#$aa			;  2|
	sta	<ch_frac(idx)		;  3|
	lda	<ch_read_l(idx)		;  3| add upper bytes
	adc	#$aa			;  2|
	sta	<ch_read_l(idx)		;  3|
	bcc	next			;3/2| quit if no page-cross (19 cycles)
	lda	<ch_read_h(idx)		;  3| increment page (these cycles not counted for)
	adc	#0			;  2|
	sta	<ch_read_h(idx)		;  3|
	cmp	#$b0+idx*$10		;  2| catch bank overflow
	beq	over_function		;3/2|
.endmacro				;   |
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
.macro rs_overfunc index, return	; handle bank overflow
	lda	#$a0 + index*$10	; cycles not counted (rare occurance)
	sta	<ch_read_h(index)
	inc	<ch_read_b(index)
	lda	<ch_read_b(index)
	sta	$5FFA + index
	jmp	return
.endmacro

;------------------------------------------------------------------------------
__resampling_start__:				; add rate to each channel (resampling)
	do_resample 0, rs_over1_s, rs_return1	; +19
rs_return1:					;
	do_resample 1, rs_over2_s, rs_return2	; +19
rs_return2:					; 
	do_resample 2, rs_over3_s, rs_return3	; +19
rs_return3:					;
	do_resample 3, rs_over4_s, rs_return4	; +19
rs_return4:					; (76 cycles)
;------------------------------------------------------------------------------
__resampling_end__:
	jmp	mix_next_sample 		;+3 cycles
	
;------------------------------------------------------------------------------
rs_over1_s: jmp	rs_over1
rs_over2_s: jmp	rs_over2
rs_over3_s: jmp	rs_over3
rs_over4_s: jmp	rs_over4
;------------------------------------------------------------------------------

;==============================================================================
	.code
;==============================================================================

;------------------------------------------------------------------------------
; music data header
;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
sample_address_l:		; sample addresses (l/h/b)
	.res	64, $aa		;
sample_address_h:		;
	.res	64, $bb		;
sample_address_bank:		;
	.res	64, $cc		;
;------------------------------------------------------------------------------
sample_end_h:			; sample end addresses (h/b)
	.res	64, $dd		;
sample_end_b:			;
	.res	64, $ff		;
;------------------------------------------------------------------------------
sample_loop_l:			; sample loop lengths (l/h/b)
	.res	64, $aa		;
sample_loop_h:			;
	.res	64, $bb		;
sample_loop_b:			;
	.res	64, $cc		;
;------------------------------------------------------------------------------
pattern_reset:			; pattern reset address
	.byte	$aa, $bb, $cc	;
;------------------------------------------------------------------------------

zerovol:
	.res	16, $20


; macro to get next channel vector
.define getchv(vec) ((<vec)+idx*18)

;============================================================================================
; per-channel sequence-update functions
;============================================================================================
.macro CH_UPDATE_FUNCTIONS prefix, idx, final
.scope prefix
	
;============================================================================================
test_read_source_reset:
;============================================================================================
	lda	#%0001<<((idx&1)*4)	;  2| test source cbit
	bit	cbits			;  3| (7/8 cycles)
	beq	@no_source		;3/2|
;--------------------------------------------------------------------------------------------
	lda	(pattern),y		;  5|
	iny				;  2|
	sta	ch_sample(idx)		;  3|
	lda	#0			;  2|
	sta	ch_rate_l(idx)		;  4|
	sta	ch_rate_h(idx)		;  4| (20 cycles)
	
	uret	getchv(UV_CH1_SETSOURCE), 28
;--------------------------------------------------------------------------------------------
@no_source:
	uret	getchv(UV_CH1_VOLUME), 8
	
;============================================================================================
set_source:
;============================================================================================
	lda	ch_sample(idx)		;  3| copy source address
	tax				;  2|
	lda	sample_address_l, x	;  4|
	sta	ch_read_l(idx)		;  2|
	lda	sample_address_h, x	;  4|
	ora	#$a0+idx*$10		;  2|
	sta	ch_read_h(idx)		;  2|
	lda	sample_address_bank, x	;  4|
	sta	ch_read_b(idx)		;  2|
	sta	$5ffa+idx		;  4| (29 cycles)
	uret	getchv(UV_CH1_TESTOFFSET), 29
	
;============================================================================================
test_apply_offset:
;============================================================================================
	lda	#%0010<<((idx&1)*4)	;  2| test offset cbit
	bit	cbits			;  3|
	beq	@no_offset		;3/2| (7/8 cycles)
;--------------------------------------------------------------------------------------------
	lda	(pattern),y		;  5| copy offset setting to data1
	iny				;  2| data2 = offset.h
	sta	data1			;  3|
	lsr				;  2|
	lsr				;  2|
	lsr				;  2|
	lsr				;  2|
	sta	data2			;  3| (21 cycles)
	uret	getchv(UV_CH1_OFFSET2), 28
;--------------------------------------------------------------------------------------------
@no_offset:				;
	uret	getchv(UV_CH1_VOLUME), 8
	
;============================================================================================
apply_offset_part2:
;============================================================================================
	lda	data1			;  3| add bank and read_h offset
	and	#$0f			;  2|
	ora	#$f0-($a0+idx*$10)	;  2|
	clc				;  2|
	adc	ch_read_h(idx)		;  3|
	and	#$f			;  2|
	ora	#$a0+idx*$10		;  2|
	sta	ch_read_h(idx)		;  3|
	lda	ch_read_b(idx)		;  3|
	adc	data2			;  3|
	sta	ch_read_b(idx)		;  3|
	sta	$5ffa+idx		;  4|
	uret	getchv(UV_CH1_VOLUME), 32
	
;============================================================================================
apply_volume:
;============================================================================================
	lda	#%0100<<((idx&1)*4)	;  2| test volume flag
	bit	cbits			;  3| 
	beq	@no_volume		;3/2| (7/8 cycles)
;--------------------------------------------------------------------------------------------
	lda	(pattern),y		;  5| set volume map entry
	iny				;  2|
	sta	ch_volmap_h(idx)	;  3|
	and	#%11000000		;  2|
	sta	ch_volmap_l(idx)	;  3|
	eor	ch_volmap_h(idx)	;  3|
	ora	#$90			;  2|
	sta	ch_volmap_h(idx)	;  3| (23 cycles)
;--------------------------------------------------------------------------------------------
	uret	getchv(UV_CH1_RATE1), 30
;--------------------------------------------------------------------------------------------
@no_volume:				;
	uret	getchv(UV_CH1_RATE1), 8
	
;============================================================================================
apply_rate1:
;============================================================================================
	lda	#%1000<<((idx&1)*4)	;  2| test rate cbit
	bit	cbits			;  3|
	beq	@restore_rate		;3/2| (7/8 cycles)
;--------------------------------------------------------------------------------------------
	lda	(pattern),y		;  5| read rate entry
	iny				;  2|
	cmp	#$80			;  2| asr#1
	ror				;  2|
	bcs	@rate_relative		;3/2|
	and	#$7f			;  2|
	sta	ch_rate_cache_h(idx)	;  3| 18
	uret	getchv(UV_CH1_RATECOPYH), 25
;--------------------------------------------------------------------------------------------
@restore_rate:				;
	uret	getchv(UV_CH1_COPYRATE), 8
;--------------------------------------------------------------------------------------------	
@rate_relative:				;   |
	sta	data1			;  3|
	uret	getchv(UV_CH1_SLIDERATE), 24
;--------------------------------------------------------------------------------------------
	
;============================================================================================
apply_rate_copyh:
;============================================================================================
	lda	(pattern),y		;  5|
	iny				;  2|
	sta	ch_rate_cache_l(idx)	;  3|
	sta	ch_rate_l(idx)		;  4|
	lda	ch_rate_cache_h(idx)	;  3|
	sta	ch_rate_h(idx)		;  4|
	uret	final, 21
	
;============================================================================================
slide_rate:
;============================================================================================
	bit	data1			;  3|
	clc				;  2|
	bmi	@minus			;3/2| rate += x
	lda	ch_rate_cache_l(idx)	;  3|
	adc	data1			;  3|
	sta	ch_rate_cache_l(idx)	;  3|
	lda	ch_rate_cache_h(idx)	;  3|
	adc	#0			;  2|
	sta	ch_rate_cache_h(idx)	;  3|
	uret	getchv(UV_CH1_COPYRATE), 24
;--------------------------------------------------------------------------------------------
@minus:					;  
	lda	ch_rate_cache_l(idx)	;  3| rate -= x
	adc	data1			;  3|
	sta	ch_rate_cache_l(idx)	;  3|
	lda	ch_rate_cache_h(idx)	;  3|
	adc	#255			;  2|
	sta	ch_rate_cache_h(idx)	;  3|
	uret	getchv(UV_CH1_COPYRATE), 25
	
;============================================================================================
copy_rate:
;============================================================================================
	lda	ch_rate_cache_h(idx)	;  3|
	sta	ch_rate_h(idx)		;  4|
	lda	ch_rate_cache_l(idx)	;  3|
	sta	ch_rate_l(idx)		;  4|
	uret	final, 14
	
.endscope
.endmacro

;--------------------------------------------------------------------------------------------
__ch4_functions_start:
CH_UPDATE_FUNCTIONS ch4, 3, UV_UPDATEEND
__ch4_functions_end:
;--------------------------------------------------------------------------------------------
__ch3_functions_start:
CH_UPDATE_FUNCTIONS ch3, 2, UV_CH4_TESTREADSRC
;--------------------------------------------------------------------------------------------
__ch2_functions_start:
CH_UPDATE_FUNCTIONS ch2, 1, UV_MIDUPDATE
;--------------------------------------------------------------------------------------------
__ch1_functions_start:
CH_UPDATE_FUNCTIONS ch1, 0, UV_CH2_TESTREADSRC
;--------------------------------------------------------------------------------------------

;============================================================================================
read_cbits_12:
;============================================================================================
	lda	(pattern),y		;5  | read cbits
	iny				;2  |
	sta	cbits			;3  |
	uret	UV_CH1_TESTREADSRC, 10	;
	
;============================================================================================
read_cbits_34:
;============================================================================================
	lda	(pattern),y		;5  | read cbits
	iny				;2  |
	sta	cbits			;3  |
	uret	UV_CH3_TESTREADSRC, 10

;============================================================================================
update_end:
;============================================================================================
	lda	(pattern),y		;  5| read duration byte
	iny				;  2|
	sty	pattern_L		;  3| save pattern position
	lsr				;  2| shift out restart bit
	tay				;  2|
	bcs	@restart		;3/2| 16/17->
	uret	UV_CH1LP_TESTEND, 16
;--------------------------------------------------------------------------------------------
@restart:				; restart pattern reader
	lda	pattern_reset		;  4|
	sta	pattern_L		;  3|
	lda	pattern_reset+1		;  4|
	ora	#0f0h			;  2|
	sta	pattern+1		;  3|
	ldx	pattern_reset+2		;  4|
	stx	pattern+2		;  3|
	stx	$5fff			;  4| 27
	uret	UV_CH1LP_TESTEND, 44	; (pood)
	
	
;---------------------------------------------------------------------------------------------
rs_over1:
	rs_overfunc 0, rs_return1
rs_over2:
	rs_overfunc 1, rs_return2
rs_over3:
	rs_overfunc 2, rs_return3
rs_over4:
	rs_overfunc 3, rs_return4
;---------------------------------------------------------------------------------------------

.macro loop_functions prefix, idx, final
.scope prefix
;==============================================================================
testend:
;==============================================================================
	ldx	ch_sample(idx)		;  3| check for loop
	lda	ch_read_h(idx)		;  3|
	and	#0fh			;  2|
	cmp	sample_end_h, x		;  4|
	bne	@notend1		;3/2|
	lda	ch_read_b(idx)		;  3|
	cmp	sample_end_b, x		;  4|
	bne	@notend2		;3/2|
	uret	UV_CH1LP_LOOPSOUND+idx*4, 23
@notend1:
	nop				; +2 waste cycles
	nop				; +2
	nop				; +2
	lda	<99h			; +3
@notend2:
	uret	final, 24
;==============================================================================
loopsound:
;==============================================================================
	
	sec				;2 |
	ldx	ch_sample(idx)		;3 |
	lda	ch_read_l(idx)		;3 |
	sbc	sample_loop_l,x		;4 |
	sta	ch_read_l(idx)		;3 |
	lda	ch_read_h(idx)		;3 |
	and	#0fh			;2 |
	sbc	sample_loop_h,x		;4 |
	and	#0fh			;2 |
	ora	#0a0h+idx*10h		;2 |
	sta	ch_read_h(idx)		;3 |
	lda	ch_read_b(idx)		;3 |
	sbc	sample_loop_b,x		;4 |
	sta	ch_read_b(idx)		;3 |
	sta	$5ffa+idx		;4
	uret	final, 44			; MAXIMUM CYCLE USAGE! 44 (52)

.endscope
.endmacro

;loop_functions ch4lp, 3, UV_CH1LP_TESTEND
loop_functions ch4lp, 3, UV_CHECKUPDATE
loop_functions ch3lp, 2, UV_CH4LP_TESTEND
loop_functions ch2lp, 1, UV_CH3LP_TESTEND
loop_functions ch1lp, 0, UV_CH2LP_TESTEND

;==============================================================================
check_update:
;==============================================================================
	dey				;  2|
	bne	@update_delay		;3/2| 4/5
;------------------------------------------------------------------------------
@start_update:
;------------------------------------------------------------------------------
	lda	pattern_L		;  3|
	cmp	#240			;  2| check if we need to increment block/bank
	bcc	:+			;3/2| ->8
	sbc	#240			;  2|
	inc	pattern+1		;  5|
	bne	:++			;3/2| ->17
;------------------------------------------------------------------------------
	ldy	#$f0			;  2|
	sty	pattern+1		;  3|
	inc	pattern+2		;  5|
	ldy	pattern+2		;  5|
	sty	$5fff			;  4|
	tay				;  2| 
	uret	UV_READ_CBITS12, 41
;------------------------------------------------------------------------------
:	ldy	99h			;align cycles to 23
	nop				;
	nop				;
	nop				;
:	tay				;
;------------------------------------------------------------------------------
	uret	UV_READ_CBITS12, 23
	
@update_delay:
	uret	UV_CH1LP_TESTEND, 5
	
;==============================================================================
mid_update_checkpoint:
;==============================================================================
	cpy	#240			;  2| test for sequence page overflow chance
	bcc	@quit5			;3/2|
;------------------------------------------------------------------------------
	tya				;  2| lower offset -= 240
	sbc	#240			;  2|
	tay				;  2|
;------------------------------------------------------------------------------
	inc	pattern+1		;  5| increment page, catch overflow
	bne	@quit18			;3/2|
;------------------------------------------------------------------------------
	lda	#0f0h			;  2| increment bank index
	sta	pattern+1		;  3|
	inc	pattern+2		;  5|
	lda	pattern+2		;  3|
	sta	$5fff			;  4|
	uret	UV_READ_CBITS34, 34
;------------------------------------------------------------------------------
@quit5:	uret	UV_READ_CBITS34, 5
;------------------------------------------------------------------------------
@quit18: uret	UV_READ_CBITS34, 18

;------------------------------------------------------------------------------

;------------------------------------------------------------------------------
.macro ute label, name ; update table entry macro
name: .word label
.endmacro 
;------------------------------------------------------------------------------

;============================================================================================
; update vector table
;============================================================================================

; align by 256 to avoid page-crossing cycle penalty
.align 256, $ab

update_table:
	ute	ch1lp::testend,		UV_CH1LP_TESTEND
	ute	ch1lp::loopsound,	UV_CH1LP_LOOPSOUND
	
	ute	ch2lp::testend,		UV_CH2LP_TESTEND
	ute	ch2lp::loopsound,	UV_CH2LP_LOOPSOUND
	
	ute	ch3lp::testend,		UV_CH3LP_TESTEND
	ute	ch3lp::loopsound,	UV_CH3LP_LOOPSOUND
	
	ute	ch4lp::testend,		UV_CH4LP_TESTEND
	ute	ch4lp::loopsound,	UV_CH4LP_LOOPSOUND
	
	ute	check_update, 		UV_CHECKUPDATE
	ute	update_end,		UV_UPDATEEND
	ute	mid_update_checkpoint,	UV_MIDUPDATE
	ute	read_cbits_12, 		UV_READ_CBITS12
	ute	read_cbits_34, 		UV_READ_CBITS34
	
	ute	ch1::test_read_source_reset, 	UV_CH1_TESTREADSRC
	ute	ch1::set_source, 		UV_CH1_SETSOURCE
	ute	ch1::test_apply_offset,		UV_CH1_TESTOFFSET
	ute	ch1::apply_offset_part2,	UV_CH1_OFFSET2
	ute	ch1::apply_volume,		UV_CH1_VOLUME
	ute	ch1::apply_rate1,		UV_CH1_RATE1
	ute	ch1::apply_rate_copyh,		UV_CH1_RATECOPYH
	ute	ch1::slide_rate,		UV_CH1_SLIDERATE
	ute	ch1::copy_rate,			UV_CH1_COPYRATE
	
	ute	ch2::test_read_source_reset, 	UV_CH2_TESTREADSRC
	ute	ch2::set_source, 		UV_CH2_SETSOURCE
	ute	ch2::test_apply_offset,		UV_CH2_TESTOFFSET
	ute	ch2::apply_offset_part2,	UV_CH2_OFFSET2
	ute	ch2::apply_volume,		UV_CH2_VOLUME
	ute	ch2::apply_rate1,		UV_CH2_RATE1
	ute	ch2::apply_rate_copyh,		UV_CH2_RATECOPYH
	ute	ch2::slide_rate,		UV_CH2_SLIDERATE
	ute	ch2::copy_rate,			UV_CH2_COPYRATE
	
	ute	ch3::test_read_source_reset, 	UV_CH3_TESTREADSRC
	ute	ch3::set_source, 		UV_CH3_SETSOURCE
	ute	ch3::test_apply_offset,		UV_CH3_TESTOFFSET
	ute	ch3::apply_offset_part2,	UV_CH3_OFFSET2
	ute	ch3::apply_volume,		UV_CH3_VOLUME
	ute	ch3::apply_rate1,		UV_CH3_RATE1
	ute	ch3::apply_rate_copyh,		UV_CH3_RATECOPYH
	ute	ch3::slide_rate,		UV_CH3_SLIDERATE
	ute	ch3::copy_rate,			UV_CH3_COPYRATE
	
	ute	ch4::test_read_source_reset, 	UV_CH4_TESTREADSRC
	ute	ch4::set_source, 		UV_CH4_SETSOURCE
	ute	ch4::test_apply_offset,		UV_CH4_TESTOFFSET
	ute	ch4::apply_offset_part2,	UV_CH4_OFFSET2
	ute	ch4::apply_volume,		UV_CH4_VOLUME
	ute	ch4::apply_rate1,		UV_CH4_RATE1
	ute	ch4::apply_rate_copyh,		UV_CH4_RATECOPYH
	ute	ch4::slide_rate,		UV_CH4_SLIDERATE
	ute	ch4::copy_rate,			UV_CH4_COPYRATE
	
;=============================================================================================
.macro copy_memory_with_banks_ab source, dest, size
;=============================================================================================
	.local @loop1
	lda	#0
	sta	data1
	lda	#((>source) & $f) | $a0
	sta	data2
	
	ldy	#(>source >> 4) - 8
	sty	$5ffa
	iny
	sty	$5ffb
	
	lda	#<dest
	sta	data3
	lda	#>dest
	sta	data4
	ldy	#<source
	ldx	#0
	
	
	lda	#<size
	sta	data5
	lda	#>size
	sta	data6
	
	sec
	
@loop1:
	lda	(data1),y
	sta	(data3,x)
	iny
	bne	:+
	inc	data2
:	inc	data3
	bne	:+
	inc	data4
:	lda	data5
	sbc	#1
	sta	data5
	lda	data6
	sbc	#0
	sta	data6
	ora	data5
	bne	@loop1
.endmacro

;=============================================================================================
__play:
;=============================================================================================
	
	; load code
;---------------------------------------------------------------------------------------------
	
	copy_memory_with_banks_ab __ZPCODE_LOAD__, __ZPCODE_RUN__, __ZPCODE_SIZE__ 
	copy_memory_with_banks_ab __RAMCODE_LOAD__, __RAMCODE_RUN__, __RAMCODE_SIZE__
	
;---------------------------------------------------------------------------------------------
	lda	#<zerovol			; initialize channels
.repeat 4, i					; (point to zero byte, rate=0)
	sta	ch_read_l(i)			;
.endrepeat					;
						;
	lda	#>zerovol			;
.repeat 4, i					;
	sta	ch_read_h(i)			;
.endrepeat					;

	lda	#<volume_table_start
.repeat 4, i
	sta	ch_volmap_l(i)
.endrepeat
	lda	#>volume_table_start
.repeat 4, i
	sta	ch_volmap_h(i)
.endrepeat
						;
	lda	#0				;
.repeat 4, i					;
	sta	ch_rate_l(i)			;
	sta	ch_rate_h(i)			;
.endrepeat					;
;---------------------------------------------------------------------------------------------
	
;---------------------------------------------------------------------------------------------	
	lda	#0				; load pattern address
	sta	pattern				;
	lda	pattern_reset			; init pattern address
	sta	pattern_L			;
	lda	pattern_reset+1			;
	ora	#0f0h				;
	sta	pattern+1			;
	ldx	pattern_reset+2			;
	stx	pattern+2			;
	stx	$5fff				;
;---------------------------------------------------------------------------------------------
	; :::other initialization here::::
	ldy	#1
	jmp	mix_next_sample

;=============================================================================================
__init:
;=============================================================================================
	; hurf durf
	rts	
