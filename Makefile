#################################################
# Trasm make file
#################################################

#PAS		 = fpc
PAS		 = ppcross386
PASFLAGS	 = -Mtp -O2 -Xd -Xs
#PASFLAGS	 = -Mtp -g
PASFLAGS	+= -Twin32 -XPi386-win32

TGTNAME		 = trasm
BIN_SUFFIX	 = .exe

TGT1		 = src/$(TGTNAME)$(BIN_SUFFIX)
TGT2		 = src/$(TGTNAME)_b$(BIN_SUFFIX)

.PHONY: all clean
all: $(TGT1) $(TGT2)

$(TGT1): src/trasm.pas
	$(PAS) $(PASFLAGS) $^

$(TGT2): src/trasm_b.pas
	$(PAS) $(PASFLAGS) $^

clean:
	$(RM) src/trasm.o src/trasm.exe src/trasm \
	      src/trasm_b.o src/trasm_b.exe src/trasm_b
