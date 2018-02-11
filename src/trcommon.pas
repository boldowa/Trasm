unit trcommon;

interface

uses
	trerror,
	trutils;

type
	code_type=array [1..9] of byte;
	
	function parse_line (var l,f1,f2,f3,f4: string): integer;
	function find_startaddress: integer;
	function maxavail : longint;
	function get_symbolmemory (l: integer): integer;
	procedure free_symbolmemory (l: integer);
	function find_addressing_mode (s: string): integer;
	function addressing_immediate (var ml: mltype; var nob: longint; f2,f3: string): integer;
	function addressing_absolute (var ml: mltype; var nob: longint; f2,f3: string): integer;
	function addressing_indirect (var ml: mltype; var nob: longint; f2,f3: string): integer;
	function type00 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type01 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type02 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type03 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type04 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type05 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type06 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type07 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type08 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type09 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type0a (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type0b (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type0c (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type0d (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type0e (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type0f (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type10 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type11 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type12 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function type13 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function pad_file (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function org_change (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function binary_append (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function source_append (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function data_string_byte (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function data_string_word (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function data_string_dword (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function data_buffer_byte (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function data_buffer_word (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function data_buffer_dword (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
	function base_change (f1,f2,f3,f4: string): integer;
	function base_parse (f1,f2,f3,f4: string): integer;
	function assemble_line (f1,f2,f3,f4: string; var ml: mltype; var nob:longint): integer;
	function process_error (err:string; e,m: integer): integer;
	function processpass: integer;
	procedure save_symbol_list;

const
      max_mnemonics=131;
      mnemonics: array [0..max_mnemonics-1] of string [10]=
        ('BRK','CLC','CLD','CLI','CLV','DEX','DEY','INX',
         'INY','NOP','PHA','PHB','PHD','PHK','PHP','PHX',
         'PHY','PLA','PLB','PLD','PLP','PLX','PLY','RTI',
         'RTL','RTS','SEC','SED','SEI','STP','SWA','TAD',
         'TAS','TAX','TAY','TCD','TCS','TDA','TDC','TSA',
         'TSC','TSX','TXA','TXS','TXY','TYA','TYX','WAI',
         'XBA','XCE','ADC','AND','CMP','EOR','LDA','ORA',
         'SBC','STA','STX','STY','ASL','LSR','ROL','ROR',
         'DEC','INC','CPX','CPY','LDX','LDY','JMP','JML',
         'JSR','JSL','BIT','BCC','BCS','BEQ','BMI','BNE',
         'BPL','BRA','BVC','BVS','BRL','MVN','MVP','PEA',
         'PEI','PER','REP','SEP','STZ','TRB','TSB','ORG',
         'INCBIN','BIN','=','EQU','DCB','DCW','DC.B','DC.W',
         'DB' ,'DW' ,'DSB','DSW','DS.B','DS.W','NAM','NAME',
         'TIT','TITLE','COU','COUNTRY','VER','VERSION','PAD','INT',
         'INTERRUPTS','END','DD','DCD','DC.D','DSD','DS.D','BAS',
         'BASE', 'SRC','INCSRC');
      mnemonic_types: array [0..max_mnemonics-1] of byte=
        ($00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$01,$01,$01,$01,$01,$01,
         $01,$01,$02,$02,$03,$03,$03,$03,
         $04,$04,$05,$05,$06,$06,$07,$08,
         $09,$0A,$0B,$0C,$0C,$0C,$0C,$0C,
         $0C,$0C,$0C,$0C,$0D,$0E,$0E,$0F,
         $10,$11,$11,$11,$12,$13,$13,$FF,
         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
         $FF,$FF,$FF);
      mnemonic_opcodes: array [0..max_mnemonics-1] of byte=
        ($00,$18,$D8,$58,$B8,$CA,$88,$E8,
         $C8,$EA,$48,$8B,$0B,$4B,$08,$DA,
         $5A,$68,$AB,$2B,$28,$FA,$7A,$40,
         $6B,$60,$38,$F8,$78,$DB,$EB,$5B,
         $1B,$AA,$A8,$5B,$1B,$7B,$7B,$3B,
         $3B,$BA,$8A,$9A,$9B,$98,$BB,$CB,
         $EB,$FB,$61,$21,$C1,$41,$A1,$01,
         $E1,$81,$86,$84,$06,$46,$26,$66,
         $C6,$E6,$E0,$C0,$A2,$A0,$4C,$DC,
         $20,$22,$24,$90,$B0,$F0,$30,$D0,
         $10,$80,$50,$70,$82,$54,$44,$F4,
         $D4,$62,$C2,$E2,$64,$14,$04,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00,$00,$00,$00,$00,$00,
         $00,$00,$00);

      console_data=$378;
      console_status=$379;
      console_control=$37a;

var
    asmfile,symfile,lstfile: text;
    asmfname,symfname,lstfname: string;
    binfile: file;
    binfname: string;

    src_mode: byte;
    src_file: array [0..7] of string;
    src_line: array [0..7] of longint;
    base_mode: byte;
    base_address, base_start: array [0..7] of longint;
    irq_mode: byte;

    pass: byte;
    show_hex: byte;
    address,address_start,address_end: longint;
    line_count,lines_total,statement_count,statement_total: longint;
    error_count: longint;

    { symbol, Operator, Operand, Comment }
    f1,f2,f3,f4: string;
    operator_index: integer;

    no_byte,no_word,no_long: byte;

    flag_debug,
    flag_display,
    flag_forwardref,
    flag_listing,
    flag_symbol: byte;

    typeff: function (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
    define_interrupts: function (f1,f2,f3,f4: string): integer;


implementation

{ parse_line: <integer> 0=no error, else error
  l: <string> line to be parsed,
  f1: <string> symbol,
  f2: <string> operator,
  f3: <string> operand,
  f4: <string> comment

  Takes a line and extracts the first full instruction line.
  Returns the corresponding fields in f1..4 and the remaining
  line is returned back in l: <string>.

  error types:
      1: missing closing quotation in statement
      2: too many fields in statement
      3: illegal forced type (type must be Byte,Word,Long)
      4: bad operator }
function parse_line (var l,f1,f2,f3,f4: string): integer;
var p: integer;
    l1,l2: string;
    c: char;
    s: string;
    i: integer;
    quote,comment: byte;
label Nomore;
begin
  f1:='';
  f2:='';
  f3:='';
  f4:='';

  operator_index:=-1;

  while (length(l)>0) and (l[1]=' ') do delete (l,1,1);
  while (length(l)>0) and (l[length (l)]=' ') do delete (l,length (l),1);
  if (l='') then
  begin
    parse_line:=0;
    goto Nomore
  end;

  comment:=0;
  quote:=0;
  p:=1;
  repeat
    c:=l[p];
    if (c=';') and (quote=0) then comment:=1;
    if (c in [#39,'"']) then quote:=quote xor ord (c);
    inc (p);
  until (p>length(l)) or ((l[p]=':') and (quote=0) and (comment=0));
  if (l[p]=':') then
  begin
    l2:=l;
    delete (l2,1,p);
    l1:=copy (l,1,p-1);
  end else
  begin
    l1:=l;
    l2:='';
  end;
  l:=l2;
  if (l1=':') then l1:='';

  if (l1='') then goto Nomore;
  while (l1[1]=' ') do delete (l1,1,1);
  while (l1[length (l1)]=' ') do delete (l1,length (l1),1);
  if (l1[1]=';') then
  begin
    f4:=l1;
    l1:='';
    goto Nomore;
  end;
  quote:=0;
  p:=1;
  repeat
    c:=l1[p];
    if (c in [#39,'"']) then quote:=quote xor (ord (c));
    inc (p);
  until (p>length(l1)) or ((l1[p] in [' ',';']) and (quote=0));
  if (p>length(l1)) and (quote<>0) then
  begin
    parse_line:=err_noquote;
    exit;
  end;
  f1:=copy (l1,1,p-1);
  delete (l1,1,p-1);

  if (l1='') then goto Nomore;
  while (l1[1]=' ') do delete (l1,1,1);
  while (l1[length (l1)]=' ') do delete (l1,length (l1),1);
  if (l1[1]=';') then
  begin
    f4:=l1;
    l1:='';
    goto Nomore;
  end;
  quote:=0;
  p:=1;
  repeat
    c:=l1[p];
    if (c in [#39,'"']) then quote:=quote xor (ord (c));
    inc (p);
  until (p>length(l1)) or ((l1[p] in [' ',';']) and (quote=0));
  if (p>length(l1)) and (quote<>0) then
  begin
    parse_line:=err_noquote;
    exit;
  end;
  f2:=copy (l1,1,p-1);
  delete (l1,1,p-1);

  if (l1='') then goto Nomore;
  while (l1[1]=' ') do delete (l1,1,1);
  while (l1[length (l1)]=' ') do delete (l1,length (l1),1);
  if (l1[1]=';') then
  begin
    f4:=l1;
    l1:='';
    goto Nomore;
  end;
  quote:=0;
  p:=1;
  repeat
    c:=l1[p];
    if (c in [#39,'"']) then quote:=quote xor (ord (c));
    inc (p);
  until (p>length(l1)) or ((l1[p] in [' ',';']) and (quote=0));
  if (p>length(l1)) and (quote<>0) then
  begin
    parse_line:=err_noquote;
    exit;
  end;
  f3:=copy (l1,1,p-1);
  delete (l1,1,p-1);

  if (l1='') then goto Nomore;
  while (l1[1]=' ') do delete (l1,1,1);
  while (l1[length (l1)]=' ') do delete (l1,length (l1),1);
  f4:=l1;
  if (f4[1]<>';') then
  begin
    parse_line:=err_toomanyfields;
    exit;
  end;
  l1:='';

Nomore:
  if (length (f1)>0) and (length (f1)<7) then
  begin
    s:=f1;
    if (s[4]='.') then
    begin
      case s[5] of
        'B': ;
        'W': ;
        'L': ;
        else
        begin
          parse_line:=err_illegaltype;
        end;
      end;
      delete (s,4,2);
    end;
    i:=0;
    while (i<max_mnemonics) and not (mnemonics[i]=s) do inc (i);
    if (i<max_mnemonics) and (mnemonics[i]=s) then
    begin
      operator_index:=i;
      if (not (f4[1]=';') and (f4<>'')) or not (f3='') then
      begin
        parse_line:=err_toomanyfields;
        exit;
      end;
      f3:=f2;
      f2:=f1;
      f1:='';
      if (f3[1]=';') then
      begin
        f4:=f3;
        f3:='';
      end;
    end else operator_index:=-1;
  end;
  if (operator_index=-1) and (f2<>'') then
  begin
    s:=f2;
    if (s[4]='.') then
    begin
      case s[5] of
        'B': ;
        'W': ;
        'L': ;
        else
        begin
          parse_line:=err_illegaltype;
        end;
      end;
      delete (s,4,2);
    end;
    i:=0;
    while (i<max_mnemonics) and not (mnemonics[i]=s) do inc (i);
    if (i<max_mnemonics) and (mnemonics[i]=s) then operator_index:=i;
  end;
  if (f2='') and (f3='') then operator_index:=32767;
  if (operator_index=-1) then
  begin
    parse_line:=err_badoperator;
    exit;
  end;

  parse_line:=0;
end;

{ find_startaddress: <integer> 0=no error, else error

  Find the start address equate in the source file.
  error types:
      1: no start address defined
      2: unexplained error in address definition }
function find_startaddress: integer;
var l,nl: string;
    sa: longint;
label Done;
begin
  find_startaddress:=1;
  reset (asmfile);
  while not eof (asmfile) do
  begin
    readln (asmfile, l);
    while (length (l)>0) do
    begin
      l:=toupper(l);
      l:=converttabs(l);
      parse_line (l,f1,f2,f3,f4);
      if (f2='ORG') then
      begin
        if (evaluate_expression (f3,nl,sa)=0) then
        begin
          address_start:=sa;
          find_startaddress:=0;
          goto Done;
        end;
        find_startaddress:=2;
        goto Done;
      end;
    end;
  end;

Done:
  close (asmfile);
end;

{ Don't care about memory (For Free Pascal) }
function maxavail : longint;
begin
  maxavail := High(LongInt);
end;

function get_symbolmemory (l: integer): integer;
var i: integer;
begin
  writeln ('Memory available: ', maxavail);
  i:=0;
  while (maxavail>sizeof (symboltype)) and (i<l) do
  begin
    getmem (symbols [i], sizeof (symboltype));
    inc (i);
  end;
  if (i=l) then get_symbolmemory:=0 else get_symbolmemory:=1;
end;

procedure free_symbolmemory (l: integer);
var i: integer;
begin
  i:=0;
  while (i<l) do
  begin
    freemem (symbols [i], sizeof (symboltype));
    inc (i);
  end;
end;

function find_addressing_mode (s: string): integer;
begin
  find_addressing_mode:=0;

  { Check if direct addressing }
  if (s [1]='#') then
  begin
    find_addressing_mode:=1;
    exit;
  end;

  { Check if indirect addressing }
  if (pos ('(',s)>0) and (pos (')',s)>0) then
  begin
    if (pos (',S),Y', s) > 0) then find_addressing_mode:=23 else
      if (pos (',X)', s) > 0) then find_addressing_mode:=21 else
        if (pos ('),Y', s) > 0) then find_addressing_mode:=22 else
          if (pos (',', s) = 0) then find_addressing_mode:=20;
    exit;
  end;

  { Check if indirect long addressing }
  if (pos ('[', s) > 0) and (pos (']', s) > 0) then
  begin
    if (pos (',S', s) > 0) or (pos (',X', s) > 0) then exit else
      if (pos ('],Y', s) > 0) then find_addressing_mode:=32 else
        if (pos (',Y', s) = 0) then find_addressing_mode:=30;
    exit;
  end;

  { Check if absolute indexed addressing }
  if (pos (',X', s) > 0) then find_addressing_mode:=11 else
    if (pos (',Y', s) > 0) then find_addressing_mode:=12 else
      if (pos (',S', s) > 0) then find_addressing_mode:=13 else
        if (pos (',', s) = 0) then find_addressing_mode:=10;
end;

function addressing_immediate (var ml: mltype; var nob: longint; f2,f3: string): integer;
var l: longint;
    e: integer;
    s: string;
begin
  delete (f3,1,1);
  delete (f2,1,3);
  e:=evaluate_expression (f3,s,l);
  if (e=0) then
  begin
    if ((f2='.L') or (f2='.W') or (f2='.V')) and (l>=0) and (l<=65535) then
    begin
      ml[1]:=lo(word(l));
      ml[2]:=hi(word(l));
      nob:=3;
    end else
    if (l>=0) and (l<=255) then
    begin
      ml[1]:=lo(word(l));
      nob:=2;
    end else
    if (l>=0) and (l<=65535) then
    begin
      ml[1]:=lo(word(l));
      ml[2]:=hi(word(l));
      nob:=3;
    end else e:=err_operandoutofrange;
  end;
  if (e>0) then
    if (f2='.L') or (f2='.W') or (f2='.V') then nob:=3 else nob:=2;
  addressing_immediate:=e;
end;

function addressing_absolute (var ml: mltype; var nob: longint; f2,f3: string): integer;
var l: longint;
    e: integer;
    s: string;
begin
  delete (f2,1,3);
  nob:=3;
  e:=evaluate_expression (f3,s,l);
  if (e=0) then
  begin
    if (f2='.L') then
    begin
      nob:=4;
      if (l>=0) and (l<=16777215) and (no_long=0) then
      begin
        ml[0]:=ml[0]+$02;
        ml[1]:=l and 255;
        ml[2]:=(l shr 8) and 255;
        ml[3]:=(l shr 16) and 255;
      end else e:=err_operandoutofrange;
    end else
    if (f2='.W') then
    begin
      nob:=3;
      if (l>=0) and (l<=65535) then
      begin
        ml[1]:=l and 255;
        ml[2]:=(l shr 8) and 255;
      end else e:=err_operandoutofrange;
    end else
    if (f2='.B') and (no_byte=0) then
    begin
      nob:=2;
      ml[0]:=ml[0]-$08;
      if (l>=0) and (l<=255) then
      begin
        ml[1]:=l and 255;
      end else e:=err_operandoutofrange;
    end else
    if (l>=0) and (l<=255) and (no_byte=0) then
    begin
      nob:=2;
      ml[0]:=ml[0]-$08;
      ml[1]:=l and 255;
    end else
    if (((l>=0) and (l<=65535)) or (l and $ff0000=address and $ff0000)) and (no_word=0) then
    begin
      nob:=3;
      ml[1]:=l and 255;
      ml[2]:=(l shr 8) and 255;
    end else
    if (l>=0) and (l<=16777215) and (no_long=0) then
    begin
      nob:=4;
      ml[0]:=ml[0]+$02;
      ml[1]:=l and 255;
      ml[2]:=(l shr 8) and 255;
      ml[3]:=(l shr 16) and 255;
    end else e:=err_operandoutofrange;
  end;
  if (e>0) then
    if (f2='.L') then nob:=4 else
      if (f2='.W') then nob:=3 else
        if (f2='.B') then nob:=2 else nob:=3;
  addressing_absolute:=e;
end;

function addressing_indirect (var ml: mltype; var nob: longint; f2,f3: string): integer;
var l: longint;
    e: integer;
    s: string;
begin
  nob:=2;
  e:=evaluate_expression (f3,s,l);
  if (e=0) then
  begin
    ml[1]:=lo(word(l));
    ml[2]:=hi(word(l));
    if (l>=0) and (l<=255) and (no_byte=0) then nob:=2 else
    if (l>=0) and (l<=65535) and (no_word=0) then nob:=3;
  end;
  addressing_indirect:=e;
end;

{ immediate }
function type00 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
begin
  ml[0]:=mnemonic_opcodes [operator_index];
  nob:=1;
  if (f3<>'') then type00:=err_unexpectedoperand else type00:=0;
end;

function type01 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      1:
      begin
        ml[0]:=mnemonic_opcodes [operator_index]+$08;
        e:=addressing_immediate (ml,nob,f2,f3);
      end;
      10:
      begin
        ml[0]:=mnemonic_opcodes [operator_index]+$0c;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      11:
      begin
        delete (f3,pos (',X',f3),2);
        ml[0]:=mnemonic_opcodes [operator_index]+$1c;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      12:
      begin
        no_byte:=1;
        no_long:=1;
        delete (f3,pos (',Y',f3),2);
        ml[0]:=mnemonic_opcodes [operator_index]+$18;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      13:
      begin
        no_word:=1;
        no_long:=1;
        delete (f3,pos (',S',f3),2);
        ml[0]:=mnemonic_opcodes [operator_index]+$02;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      20:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (')',f3),1);
        ml[0]:=mnemonic_opcodes [operator_index]+$11;
        e:=addressing_indirect (ml,nob,f2,f3);
      end;
      21:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (',X)',f3),3);
        ml[0]:=mnemonic_opcodes [operator_index]+$00;
        e:=addressing_indirect (ml,nob,f2,f3);
      end;
      22:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos ('),Y',f3),3);
        ml[0]:=mnemonic_opcodes [operator_index]+$10;
        e:=addressing_indirect (ml,nob,f2,f3);
      end;
      23:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (',S),Y',f3),5);
        ml[0]:=mnemonic_opcodes [operator_index]+$12;
        e:=addressing_indirect (ml,nob,f2,f3);
      end;
      30:
      begin
        no_word:=1;
        no_long:=1;
        delete (f3,pos ('[',f3),1);
        delete (f3,pos (']',f3),1);
        ml[0]:=mnemonic_opcodes [operator_index]+$06;
        e:=addressing_indirect (ml,nob,f2,f3);
      end;
      32:
      begin
        no_word:=1;
        no_long:=1;
        delete (f3,pos ('[',f3),1);
        delete (f3,pos ('],Y',f3),3);
        ml[0]:=mnemonic_opcodes [operator_index]+$16;
        e:=addressing_indirect (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type01:=e;
end;

function type02 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$08;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      11:
      begin
        no_word:=1;
        no_long:=1;
        if (pos ('STX',f2)=0) then
        begin
          delete (f3,pos (',X',f3),2);
          ml[0]:=mnemonic_opcodes [operator_index]+$18;
          e:=addressing_absolute (ml,nob,f2,f3);
        end else e:=err_illegaladdressingmode;
      end;
      12:
      begin
        no_word:=1;
        no_long:=1;
        if (pos ('STY',f2)=0) then
        begin
          delete (f3,pos (',Y',f3),2);
          ml[0]:=mnemonic_opcodes [operator_index]+$18;
          e:=addressing_absolute (ml,nob,f2,f3);
        end else e:=err_illegaladdressingmode;
      end else e:=err_illegaladdressingmode;
    end
  end;
  type02:=e;
end;

function type03 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') or (f3='A') then
  begin
    nob:=1;
    ml[0]:=mnemonic_opcodes [operator_index]+$04;
    e:=0;
  end else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$08;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      11:
      begin
        no_long:=1;
        delete (f3,pos (',X',f3),2);
        ml[0]:=mnemonic_opcodes [operator_index]+$18;
        e:=addressing_absolute (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end
  end;
  type03:=e;
end;

function type04 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') or (f3='A') then
  begin
    nob:=1;
    if (f2='DEC') then ml[0]:=$3a;
    if (f2='INC') then ml[0]:=$1a;
    e:=0;
  end else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$08;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      11:
      begin
        no_long:=1;
        delete (f3,pos (',X',f3),2);
        ml[0]:=mnemonic_opcodes [operator_index]+$18;
        e:=addressing_absolute (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end
  end;
  type04:=e;
end;

function type05 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      1:
      begin
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_immediate (ml,nob,f2,f3);
      end;
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$0c;
        e:=addressing_absolute (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end
  end;
  type05:=e;
end;

function type06 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      1:
      begin
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_immediate (ml,nob,f2,f3);
      end;
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$0c;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      11:
      begin
        no_long:=1;
        if (pos ('LDX',f2)=0) then
        begin
          delete (f3,pos (',X',f3),2);
          ml[0]:=mnemonic_opcodes [operator_index]+$1c;
          e:=addressing_absolute (ml,nob,f2,f3);
        end else e:=err_illegaladdressingmode;
      end;
      12:
      begin
        no_long:=1;
        if (pos ('LDY',f2)=0) then
        begin
          delete (f3,pos (',Y',f3),2);
          ml[0]:=mnemonic_opcodes [operator_index]+$1c;
          e:=addressing_absolute (ml,nob,f2,f3);
        end else e:=err_illegaladdressingmode;
      end else e:=err_illegaladdressingmode;
    end
  end;
  type06:=e;
end;

function type07 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_byte:=1;
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_absolute (ml,nob,f2,f3);
        if (nob=4) then ml[0]:=mnemonic_opcodes [operator_index]+$10;
      end;
      20:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (')',f3),1);
        no_byte:=1;
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$20;
        e:=addressing_indirect (ml,nob,f2,f3);
        nob:=3;
      end;
      21:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (',X)',f3),3);
        no_byte:=1;
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$30;
        e:=addressing_indirect (ml,nob,f2,f3);
        nob:=3;
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type07:=e;
end;

function type08 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_byte:=1;
        no_word:=1;
        f2:=f2+'.L';
        ml[0]:=mnemonic_opcodes [operator_index]-$82;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      20:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (')',f3),1);
        no_byte:=1;
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_indirect (ml,nob,f2,f3);
        nob:=3;
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type08:=e;
end;

function type09 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_byte:=1;
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      21:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (',X)',f3),3);
        no_byte:=1;
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$dc;
        e:=addressing_indirect (ml,nob,f2,f3);
        nob:=3;
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type09:=e;
end;

function type0a (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_byte:=1;
        no_word:=1;
        ml[0]:=mnemonic_opcodes [operator_index]-$02;
        e:=addressing_absolute (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type0a:=e;
end;

function type0b (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      1:
      begin
        no_long:=1;
        ml[0]:=$89;
        e:=addressing_immediate (ml,nob,f2,f3);
      end;
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$08;
        e:=addressing_absolute (ml,nob,f2,f3);
      end;
      11:
      begin
        no_long:=1;
        delete (f3,pos (',X',f3),2);
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$18;
        e:=addressing_absolute (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type0b:=e;
end;

function type0c (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    s: string;
    l: longint;
begin
  ml[0]:=mnemonic_opcodes [operator_index];
  ml[1]:=0;
  nob:=2;
  if (f3='') then e:=err_nooperand else
  begin
    e:=evaluate_expression (f3,s,l);
    if (e=0) then
    begin
      l:=l-(address+2);
      if (l<-128) or (l>127) then e:=err_branchoutofrange else ml[1]:=lo (word(l));
    end;
  end;
  type0c:=e;
end;

function type0d (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    s: string;
    l: longint;
begin
  ml[0]:=mnemonic_opcodes [operator_index];
  ml[1]:=0;
  ml[2]:=0;
  nob:=3;
  if (f3='') then e:=err_nooperand else
  begin
    e:=evaluate_expression (f3,s,l);
    if (e=0) then
    begin
      l:=l-(address+2);
      if (l<-32768) or (l>32767) then e:=err_branchoutofrange else
      begin
        ml[1]:=lo (word(l));
        ml[2]:=hi (word(l));
      end;
    end;
  end;
  type0d:=e;
end;

function type0e (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    s: string;
    l1: longint;
    i: integer;
begin
  ml[0]:=mnemonic_opcodes [operator_index];
  ml[1]:=0;
  ml[2]:=0;
  nob:=3;
  if (f3='') then e:=err_nooperand else
  begin
    i:=pos (',',f3);
    if (i=0) then e:=err_illegaladdressingmode else
    begin
      s:=copy (f3,1,i-1);
      e:=evaluate_expression (f3,s,l1);
      s:=copy (f3,i+1,length(s)-i+1);
      e:=evaluate_expression (f3,s,l1);
      if (l1>=0) and (l1<=255) then
      begin
        ml[1]:=lo(word(l1));
      end else e:=err_operandoutofrange;
    end;
  end;
  type0e:=e;
end;

function type0f (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_byte:=1;
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_absolute (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type0f:=e;
end;

function type10 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      20:
      begin
        delete (f3,pos ('(',f3),1);
        delete (f3,pos (')',f3),1);
        no_word:=1;
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_indirect (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type10:=e;
end;

function type11 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      1:
      begin
        no_word:=1;
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index];
        e:=addressing_immediate (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type11:=e;
end;

function type12 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=0;
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$38;
        e:=addressing_absolute (ml,nob,f2,f3);
        if (nob=2) then ml[0]:=mnemonic_opcodes [operator_index];
      end;
      11:
      begin
        delete (f3,pos (',X',f3),2);
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$3a;
        e:=addressing_absolute (ml,nob,f2,f3);
        if (nob=2) then ml[0]:=mnemonic_opcodes [operator_index]+$10;
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type12:=e;
end;

function type13 (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i: integer;
begin
  if (f3='') then e:=err_nooperand else
  begin
    i:=find_addressing_mode (f3);
    case i of
      10:
      begin
        no_long:=1;
        ml[0]:=mnemonic_opcodes [operator_index]+$08;
        e:=addressing_absolute (ml,nob,f2,f3);
      end else e:=err_illegaladdressingmode;
    end;
  end;
  type13:=e;
end;

function pad_file (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    buf: array [0..32767] of byte;
    l: longint;
    s: string;
begin
  e:=0;
  fillchar (buf, sizeof (buf), 0);
  show_hex:=1;
  if (f3='') then
  begin
    l:=((address+$010000) and $ff0000) or $008000;
    nob:=l-address;
    if (pass=3) then
    begin
      l:=$8000-(address and $7fff);
      blockwrite (binfile, buf, l);
    end;
  end else
  begin
    e:=evaluate_expression (f3,s,l);
    if (e>0) then e:=err_badoperand else
    begin
      if (l<32768) or (l>65535) then e:=err_operandoutofrange else
      begin
        l:=l+(address and $ff0000);
        if (l<address) then e:=err_operandoutofrange else
        begin
          nob:=l-address;
          if (pass=3) then blockwrite (binfile, buf, nob);
        end;
      end;
    end;
  end;
  pad_file:=e;
end;

function org_change (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    buf: array [0..32767] of byte;
    l: longint;
    s: string;
begin
  e:=0;
  fillchar (buf, sizeof (buf), 0);
  show_hex:=1;
  if (f3<>'') then
  begin
    e:=evaluate_expression (f3,s,l);
    if (e>0) then e:=err_badoperand else
    begin
      if (l<address) or (l>$ffffff) or (l and $ffff<$8000) then e:=err_operandoutofrange else
      begin
        nob:=l-address;
        l:=nob;
        if (pass=3) then
        begin
          while (l>65535) do
          begin
            blockwrite (binfile, buf, 32768);
            l:=l-65536;
          end;
          blockwrite (binfile, buf, l and $7fff);
        end;
      end;
    end;
  end else e:=err_nooperand;
  org_change:=e;
end;

function binary_append (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    buf: array [0..32767] of byte;
    l: longint;
    w: word;
    f: file;
begin
  e:=0;
  show_hex:=1;
  if (f3[1] in ['"',#39]) then
  begin
    delete (f3,1,1);
    delete (f3,length(f3),1);
  end;
  {$I-}
  assign (f,f3);
  reset (f,1);
  {$I+}
  if (ioresult = 0) then
  begin
    l:=filesize (f);
    l:=(l div 32768)*65536+(l and $7fff);
    if ((l+address and $ffff)<$8000) then l:=l+$8000;
    nob:=l;
    if (pass=3) then
    begin
      repeat
        blockread (f, buf, sizeof (buf), w);
        blockwrite (binfile, buf, w);
      until (w=0);
    end;
    close (f);
  end else e:=err_badbinaryfile;
  binary_append:=e;
end;

function source_append (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    f: file;
begin
  e:=0;
  show_hex:=1;
  if (f3[1] in ['"',#39]) then
  begin
    delete (f3,1,1);
    delete (f3,length(f3),1);
  end;
  {$I-}
  assign (f,f3);
  reset (f,1);
  {$I+}
  if (ioresult = 0) then
  begin
    src_file [src_mode]:=asmfname;
    src_line [src_mode]:=line_count;
    line_count:=0;
    inc (src_mode);
    close (f);
    close (asmfile);
    asmfname:=f3;
    assign (asmfile, asmfname);
    reset (asmfile);
  end else e:=err_badsourcefile;
  source_append:=e;
end;

function data_string_byte (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i1,i2: integer;
    quote: byte;
    s,ns: string;
    l: longint;
begin
  e:=0;
  nob:=0;
  while (length(f3)>0) do
  begin
    while (f3[1]=' ') do delete (f3,1,1);
    i1:=1;
    quote:=0;
    repeat
      if (f3[i1] in ['"',#39]) then quote:=quote xor ord (f3[i1]);
      inc (i1);
    until (i1>length (f3)) or ((quote=0) and (f3[i1]=','));
    s:=copy (f3,1,i1-1);
    while (s[length(s)]=' ') do delete (s,length(s),1);
    if (s[1] in ['"',#39]) then
    begin
      i2:=2;
      while (i2<=length(s)) and (s[i2]<>s[1]) do
      begin
        ml [nob]:=ord (s[i2]);
        inc (nob);
        inc (i2);
      end;
    end else
    begin
      e:=evaluate_expression (s,ns,l);
      if (e>0) then l:=0;
      if (l<0) or (l>255) then e:=err_operandoutofrange;
      ml [nob]:=lo (word(l));
      inc (nob);
    end;
    delete (f3,1,i1);
  end;
  data_string_byte:=e;
end;

function data_string_word (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i1,i2: integer;
    quote: byte;
    s,ns: string;
    l: longint;
begin
  e:=0;
  nob:=0;
  while (length(f3)>0) do
  begin
    while (f3[1]=' ') do delete (f3,1,1);
    i1:=1;
    quote:=0;
    repeat
      if (f3[i1] in ['"',#39]) then quote:=quote xor ord (f3[i1]);
      inc (i1);
    until (i1>length (f3)) or ((quote=0) and (f3[i1]=','));
    s:=copy (f3,1,i1-1);
    while (s[length(s)]=' ') do delete (s,length(s),1);
    if (s[1] in ['"',#39]) then
    begin
      i2:=2;
      while (i2<=length(s)) and (s[i2]<>s[1]) do
      begin
        ml [nob]:=ord (s[i2]);
        ml [nob+1]:=0;
        inc (nob,2);
        inc (i2);
      end;
    end else
    begin
      e:=evaluate_expression (s,ns,l);
      if (e>0) then l:=0;
      if (l<0) or (l>65535) then e:=err_operandoutofrange;
      ml [nob]:=lo (word(l));
      ml [nob+1]:=hi (word(l));
      inc (nob,2);
    end;
    delete (f3,1,i1);
  end;
  data_string_word:=e;
end;

function data_string_dword (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    i1,i2: integer;
    quote: byte;
    s,ns: string;
    l: longint;
begin
  e:=0;
  nob:=0;
  while (length(f3)>0) do
  begin
    while (f3[1]=' ') do delete (f3,1,1);
    i1:=1;
    quote:=0;
    repeat
      if (f3[i1] in ['"',#39]) then quote:=quote xor ord (f3[i1]);
      inc (i1);
    until (i1>length (f3)) or ((quote=0) and (f3[i1]=','));
    s:=copy (f3,1,i1-1);
    while (s[length(s)]=' ') do delete (s,length(s),1);
    if (s[1] in ['"',#39]) then
    begin
      i2:=2;
      while (i2<=length(s)) and (s[i2]<>s[1]) do
      begin
        ml [nob]:=ord (s[i2]);
        ml [nob+1]:=0;
        ml [nob+2]:=0;
        ml [nob+3]:=0;
        inc (nob,4);
        inc (i2);
      end;
    end else
    begin
      e:=evaluate_expression (s,ns,l);
      if (e>0) then l:=0;
{      if (l<0) or (l>$ffffffff) then e:=err_operandoutofrange;}
      ml [nob]:=lo (word(l));
      ml [nob+1]:=hi (word(l));
      ml [nob+2]:=(l shr 16) and 255;
      ml [nob+3]:=(l shr 24) and 255;
      inc (nob,4);
    end;
    delete (f3,1,i1);
  end;
  data_string_dword:=e;
end;

function data_buffer_byte (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    buf: array [0..32767] of byte;
    s: string;
    l: longint;
begin
  e:=0;
  show_hex:=1;
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=evaluate_expression (f3,s,l);
    if (e=0) then
    begin
      if (l<0) or (l>32768) then e:=err_operandoutofrange else
      begin
        fillchar (buf,l,0);
        if (pass=3) then blockwrite (binfile, buf, l);
        inc (nob,l);
      end;
    end;
  end;
  data_buffer_byte:=e;
end;

function data_buffer_word (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    buf: array [0..32767] of byte;
    s: string;
    l: longint;
begin
  e:=0;
  show_hex:=1;
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=evaluate_expression (f3,s,l);
    if (e=0) then
    begin
      if (l<0) or (l>32768) then e:=err_operandoutofrange else
      begin
        fillchar (buf,l,0);
        if (pass=3) then
        begin
          blockwrite (binfile, buf, l);
          blockwrite (binfile, buf, l);
        end;
        inc (nob,l*2);
      end;
    end;
  end;
  data_buffer_word:=e;
end;

function data_buffer_dword (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
    buf: array [0..32767] of byte;
    s: string;
    l: longint;
begin
  e:=0;
  show_hex:=1;
  nob:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=evaluate_expression (f3,s,l);
    if (e=0) then
    begin
      if (l<0) or (l>32768) then e:=err_operandoutofrange else
      begin
        fillchar (buf,l,0);
        if (pass=3) then
        begin
          blockwrite (binfile, buf, l);
          blockwrite (binfile, buf, l);
          blockwrite (binfile, buf, l);
          blockwrite (binfile, buf, l);
        end;
        inc (nob,l*4);
      end;
    end;
  end;
  data_buffer_dword:=e;
end;

function base_change (f1,f2,f3,f4: string): integer;
var e: integer;
    l: longint;
    s: string;
begin
  e:=0;
  if (f3='') then e:=err_nooperand else
  begin
    e:=evaluate_expression (f3,s,l);
    if (e>0) then e:=err_badoperand else
    begin
      if (l<0) or (l>$ffffff) then e:=err_operandoutofrange else
      begin
        if (base_mode<16) then
        begin
          base_start [base_mode]:=l;
          base_address [base_mode]:=address;
          inc (base_mode);
          address:=l;
        end;
      end;
    end;
  end;
  base_change:=e;
end;

function base_parse (f1,f2,f3,f4: string): integer;
var e: integer;
begin
  e:=0;
  if (f2='END') then
  begin
    dec (base_mode);
    address:=base_address [base_mode]+(address-base_start [base_mode]);
    e:=1;
  end;
  base_parse:=e;
end;

{ assemble_line: <integer> 0=no error, else error
  error types:
      100: overflow in symbols
      101: symbol already exists, overwrite
      102: bad operand
      103: bad symbol name }
function assemble_line (f1,f2,f3,f4: string; var ml: mltype; var nob:longint): integer;
var e,i: integer;
    newf3: string;
    operand: longint;
label skip_label_check;
begin
  e:=0;
  nob:=0;
  assemble_line:=0;

  { Check for definitions of temporary symbols }
  if (f1[1]='-') then
  begin
    i:=0;
    while (f1[i+1]='-') and (i<=length(f1)) do inc (i);
    if (i>16) then i:=16;
    temps_minus[i].number:=address;
    goto skip_label_check;
  end;
  if (f1[1]='+') then
  begin
    i:=0;
    while (f1[i+1]='+') and (i<=length(f1)) do inc (i);
    if (i>16) then i:=16;
    temps_plus[i].number:=address;
    goto skip_label_check;
  end;

  { Check if a symbol is being defined and if it is whether the
    symbol has a valid name }
  if (f1<>'') and (scan_symbol (f1)>0) then
  begin
    assemble_line:=err_badsymbolname;
    f1:='';
    if ((f2='') and (f3='')) or (f2='=') or (f3='EQU') then exit;
  end;

  { Save the symbol }
  if (f1<>'') then
  begin

    { Check if symbols exists or not }
    if (find_symbol (f1,i)<>0) then
    begin

      { Check if line only defines a symbol at the current address }
      if (f2='') and (f3='') then
      begin
        f2:='EQU';
        f3:='*';
      end;

      { Check if line is an equate definition }
      if (f2='=') or (f2='EQU') then
      begin
        operator_index:=32767;

        { Check if equate operand is the current address }
        if (f3='*') then str (address,f3);

        { Save symbol }
        if (evaluate_expression (f3,newf3,operand)=0) then
        begin
          if (save_symbol (f1,operand,pass)>0) then e:=err_overflowsymbols;
        end else
        begin
          e:=err_badoperand;
          operand:=0;
          if (save_symbol (f1,operand,pass)>0) then e:=err_overflowsymbols;
        end;
      end else

      { Symbol is defined as the current address }
      if (save_symbol (f1,address,pass)>0) then e:=err_overflowsymbols else e:=0;
    end else

    { Symbol already exists }
    begin
      if (symbols[i]^.pass=pass) then e:=err_symbolexists;

      { Check if equate operand is the current address }
      if (f2='') and (f3='') then
      begin
        f2:='EQU';
        f3:='*';
      end;

      { Check if line is an equate definition }
      if (f2='=') or (f2='EQU') then
      begin
        operator_index:=32767;

        { Check if equate operand is the current address }
        if (f3='*') then str (address,f3);

        { Save symbol }
        if (evaluate_expression (f3,newf3,operand)=0) then
        begin
          symbols[i]^.number:=operand;
        end else
        begin
          assemble_line:=err_badoperand;
          operand:=0;
          if (save_symbol (f1,operand,pass)>0) then e:=err_overflowsymbols else e:=0;
        end;
      end else

      { Symbol is defined as the current address }
      symbols[i]^.number:=address;
    end;
  end;

skip_label_check:
  no_byte:=0;
  no_word:=0;
  no_long:=0;
  if (operator_index>-1) and (operator_index<32767) then
  begin
    case (mnemonic_types [operator_index]) of
      $00:  e:=type00 (ml, nob,f1,f2,f3,f4);
      $01:  e:=type01 (ml, nob,f1,f2,f3,f4);
      $02:  e:=type02 (ml, nob,f1,f2,f3,f4);
      $03:  e:=type03 (ml, nob,f1,f2,f3,f4);
      $04:  e:=type04 (ml, nob,f1,f2,f3,f4);
      $05:  e:=type05 (ml, nob,f1,f2,f3,f4);
      $06:  e:=type06 (ml, nob,f1,f2,f3,f4);
      $07:  e:=type07 (ml, nob,f1,f2,f3,f4);
      $08:  e:=type08 (ml, nob,f1,f2,f3,f4);
      $09:  e:=type09 (ml, nob,f1,f2,f3,f4);
      $0a:  e:=type0a (ml, nob,f1,f2,f3,f4);
      $0b:  e:=type0b (ml, nob,f1,f2,f3,f4);
      $0c:  e:=type0c (ml, nob,f1,f2,f3,f4);
      $0d:  e:=type0d (ml, nob,f1,f2,f3,f4);
      $0e:  e:=type0e (ml, nob,f1,f2,f3,f4);
      $0f:  e:=type0f (ml, nob,f1,f2,f3,f4);
      $11:  e:=type11 (ml, nob,f1,f2,f3,f4);
      $12:  e:=type12 (ml, nob,f1,f2,f3,f4);
      $13:  e:=type13 (ml, nob,f1,f2,f3,f4);
      $ff:  e:=typeff (ml, nob,f1,f2,f3,f4);
    end;

    assemble_line:=e;
  end;
end;

function process_error (err:string; e,m: integer): integer;
begin
  process_error:=0;
  if (m=1) then
  begin
    case e of
      err_noquote:
        save_error (err+'Missing quotation mark');
      err_toomanyfields:
        save_error (err+'Too many fields in statement');
      err_illegaltype:
        save_error (err+'Illegal forced type');
      err_badoperator:
        save_error (err+'Not a valid operator');
      err_overflowsymbols:
      begin
        save_error (err+'Overflow in symbols, maximum symbols='+inttostr(max_symbols));
        process_error:=e;
        exit;
      end else save_error (err+'Unknown error #'+inttostr (e));
    end;
  end else
  begin
    case e of
      err_overflowsymbols:
      begin
        save_error (err+'Overflow in symbols, maximum symbols='+inttostr(max_symbols));
        process_error:=e;
        exit;
      end;
      err_symbolexists:
        save_error (err+'Symbol already exists, overwriting');
      err_badoperand:
        save_error (err+'Bad operand');
      err_badsymbolname:
        save_error (err+'Invalid symbol name');
      err_symbolnotexist:
        if (pass>1) then save_error (err+'Symbol does not exist') else
          if (flag_forwardref=1) then save_error (err+'Symbol does not exist, forward reference?');
      err_unexpectedoperand:
        save_error (err+'No operand needed');
      err_branchoutofrange:
        if (pass>1) then save_error (err+'Branch out of range') else
        save_error (err+'Branch out of range, forward reference?');
      err_nooperand:
        save_error (err+'No operand in statment');
      err_operandoutofrange:
        save_error (err+'Operand out of range');
      err_illegaladdressingmode:
        save_error (err+'Illegal addressing mode');
      err_invalidcharacter:
        save_error (err+'Invalid character in statement');
      err_badirqdefinition:
        save_error (err+'Error in interrupt definition table');
      err_badbinaryfile:
        save_error (err+'Unable to append binary file');
      err_badsourcefile:
        save_error (err+'Unable to append source file');
      else save_error (err+'Unknown error #'+inttostr (e));
    end;
  end;
end;

function processpass: integer;
var l,lst: string;
    e: integer;
    ml: mltype;
    b, nob: longint;
    err: string;
label Done, Last_Source;
begin
  reset (asmfile);

Last_Source:

  processpass:=0;
  while not (eof (asmfile)) do
  begin
    statement_count:=0;
    inc (line_count);
    inc (lines_total);
    readln (asmfile, l);
    if (flag_debug>0) then
    begin
      writeln ('Line #',line_count);
      writeln ('   Raw   :',l);
    end;
    if (l='') and (pass=3) then
    begin
      lst:=' '; {inttostr (statement_total+statement_count)+' ';}
      while (length (lst)<6) do lst:=lst+' ';
      lst:=lst+dectohex (address, 6)+' ';
      if (flag_display=1) then writeln (lst);
      if (flag_listing=1) then writeln (lstfile, lst);
    end;
    if (l<>'') then
    begin
      l:=toupper(l);
      l:=converttabs(l);
    end;
    if (flag_debug>0) then writeln ('   Up/Tab:',l);
    while (l<>'') do
    begin
      nob:=0;
      inc (statement_count);
      e:=parse_line (l,f1,f2,f3,f4);
      if (flag_debug>0) then
      begin
        writeln ('   Statement #',statement_count);
        writeln ('      Fields: [',f1,'] [',f2,'] [',f3,'] [',f4,']');
      end;
      if (f1='') and (f2='') and (f3='') and (f4='') then
      begin
        dec (statement_count);
      end else
      begin
        if (e>0) then
        begin
          err:='Error in '+inttostr(line_count)+'.'+inttostr(statement_count)+'/'+
            inttostr(statement_total+statement_count)+': ';
          if (process_error (err,e,1)>0) then goto Done;
          inc (error_count);
        end else
        begin
          if (irq_mode>0) then e:=define_interrupts (f1,f2,f3,f4) else
          begin
            if (base_mode>0) then
            begin
              e:=base_parse (f1,f2,f3,f4);
              if (e=0) then e:=assemble_line (f1,f2,f3,f4,ml,nob) else e:=0;
            end else e:=assemble_line (f1,f2,f3,f4,ml,nob);
          end;
          if (e>0) then
          begin
            err:='Error in '+inttostr(line_count)+'.'+inttostr(statement_count)+'/'+
              inttostr(statement_total+statement_count)+': ';
            if (process_error (err,e,2)>0) then goto Done;
            inc (error_count);
          end;
        end;

{ line assembled alright }

        if (pass=3) then
        begin
          lst:=inttostr (statement_total+statement_count)+' ';
          while (length (lst)<6) do lst:=lst+' ';
          lst:=lst+dectohex (address, 6)+' ';
          if (show_hex=0) then
          begin
            for b:=1 to nob do
              lst:=lst+dectohex (ml[b-1],2)+' ';
            blockwrite (binfile, ml, nob);
          end;
          while (length (lst)<25) do lst:=lst+' ';
          lst:=lst+f1+' ';
          while (length (lst)<25+12) do lst:=lst+' ';
          lst:=lst+f2+' ';
          while (length (lst)<25+12+2+6+2) do lst:=lst+' ';
          lst:=lst+f3+' ';
          if (f4<>'') then
          begin
            while (length (lst)<25+12+2+6+2+16+2) do lst:=lst+' ';
            lst:=lst+f4;
          end;
          if (flag_display=1) then writeln (lst);
          if (flag_listing=1) then writeln (lstfile, lst);
        end;
        inc (address,nob);
        if (address and $8000=0) and (base_mode=0) then address:=address or $008000;
        show_hex:=0;
      end;
    end;
    inc (statement_total,statement_count);
  end;
  processpass:=0;

Done:
  close (asmfile);
  if (src_mode>0) then
  begin
    save_error ('   '+asmfname+' - Lines: '+inttostr (line_count));
    dec (src_mode);
    asmfname:=src_file [src_mode];
    assign (asmfile, asmfname);
    reset (asmfile);
    for b:=1 to src_line [src_mode] do
      readln (asmfile, l);
    line_count:=src_line [src_mode];
    goto Last_Source;
  end;
end;

procedure save_symbol_list;
var i: integer;
    lst: string;
begin
  assign (symfile, symfname);
  rewrite (symfile);
  writeln (symfile, symbol_count,' Symbols use for ',toupper (asmfname));
  writeln (symfile);
  for i:=0 to symbol_count-1 do
  begin
    lst:=symbols[i]^.name+' ';
    while (length (lst)<symbol_size+1) do lst:=lst+' ';
    lst:=lst+'= $';
    lst:=lst+dectohex(symbols[i]^.number,6)+' ; ';
    lst:=lst+inttostr (symbols[i]^.number);
    writeln (symfile, lst);
  end;
  writeln (symfile);
  close (symfile);
end;

end.

