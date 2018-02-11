#################################################
# Trasm make file
#################################################

TGTNAME		 = trasm
SRCDIR		 = src/


MV		 = mv
PP		 = fpc
PPFLAGS		 = -Mtp -O2 -Xd -Xs -CX
## debug
ifdef DEBUG
PPFLAGS		 = -Mtp -g
endif

BIN_SUFFIX	 =
ifdef WIN32
PP		 = ppcross386
PPFLAGS		+= -Twin32 -XPi386-win32
BIN_SUFFIX	 = .exe
endif


TGT1		 = $(TGTNAME)$(BIN_SUFFIX)
TGT2		 = $(TGTNAME)_b$(BIN_SUFFIX)

DEPENDS		 = $(SRCDIR)trcommon.pas
DEPENDS		+= $(SRCDIR)trerror.pas
DEPENDS		+= $(SRCDIR)trutils.pas

.PHONY: all clean

all: $(TGT1) $(TGT2)

$(TGT1): $(SRCDIR)$(TGTNAME).pas $(DEPENDS)
	$(PP) -o$@ $(PPFLAGS) $< && \
	$(MV) $(SRCDIR)$@ .

$(TGT2): $(SRCDIR)$(TGTNAME)_b.pas $(DEPENDS)
	$(PP) -o$@ $(PPFLAGS) $< &&\
	$(MV) $(SRCDIR)$@ .

clean:
	$(RM) $(SRCDIR)*.o $(SRCDIR)*.ppu $(SRCDIR)ppas.* \
	      $(TGT1) $(TGT2)

