#
# DRIVER MAKEFILE
#

.SUFFIXES:

#---------------------------------------------------------------------------------

CC65		:= /c/cc65/bin
AS			:= $(CC65)/ca65
LD			:= $(CC65)/ld65
OD			:= $(CC65)/od65

#---------------------------------------------------------------------------------

BUILD		:=	build
SOURCES		:=	source
INCLUDES	:=	include

#---------------------------------------------------------------------------------

ASFLAGS		:=	--cpu 6502 -s
INCLUDE		:=	$(foreach dir,$(INCLUDES),-I $(CURDIR)/$(dir)) -I $(CURDIR)/$(BUILD)

LDFLAGS		:=	-C $(CURDIR)/config.ld
VPATH		:=	$(foreach dir,$(SOURCES),$(CURDIR)/$(dir)) 
SFILES		:=	$(foreach dir,$(SOURCES),$(notdir $(wildcard $(dir)/*.asm)))
OFILES		:=	$(SFILES:.asm=.o)
 
.PHONY: clean
 
all	: test.nsf

#---------------------------------------------------------------------------------

clean:
	@echo clean ...
	@rm -fr test.nsf pcm.nsf pcm.map test.bin
	
#---------------------------------------------------------------------------------

#test.nsf: test.bin pcm.nsf
#	@echo creating test.nsf....
#	@../pmerge/Debug/pmerge test.bin pcm.nsf test.nsf
	
#---------------------------------------------------------------------------------

test.nsf: pcm.nsf test.mod
	@echo creating test.nsf...
	@../pppp/pppp test.mod pcm.nsf test.nsf
#test.bin: test.mod
#	@echo compiling mod....
#	@../pppp/pppp $< $@
	
#---------------------------------------------------------------------------------

pcm.nsf: $(OFILES)
	@echo LINKING pcm.nsf.....
	@$(LD) $(LDFLAGS) $(OFILES) -o $@ -m $(@:.nsf=.map)
	@echo Okay! :: $(notdir $@)
	$(OD) --dump-exports pcmnsf.o > syms.txt
	
#---------------------------------------------------------------------------------

%.o : %.asm
	@echo Assembling :: $(notdir $<)
	@$(AS) $(ASFLAGS) $(INCLUDE) $< -o $@
