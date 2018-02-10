{
Outputs bin file only version
To compile this program with Free Pascal, type this command in command prompt. (Set PATH variable first!)
fpc -Mtp trasm_b.pas
}

program _65816_Tricks_Assembler;
{$M 49152,0,655360}
{
65816 Tricks Assembler Version 1.11   (C)opyright 1994
Coded by 1000 Miles [Tricks]
}

type code_type=array [1..9] of byte;

const symbol_size=18;
      max_symbols=8192;
      max_mnemonics=131;
      mnemonics: array [0..max_mnemonics-1] of string [6]=
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
      err_noquote=1;
      err_toomanyfields=2;
      err_illegaltype=3;
      err_badoperator=4;
      err_overflowsymbols=100;
      err_symbolexists=101;
      err_badoperand=102;
      err_badsymbolname=103;
      err_symbolnotexist=104;
      err_unexpectedoperand=105;
      err_branchoutofrange=106;
      err_nooperand=107;
      err_operandoutofrange=108;
      err_illegaladdressingmode=109;
      err_invalidcharacter=150;
      err_badirqdefinition=175;
      err_badbinaryfile=200;
      err_badsourcefile=210;

      init_code: code_type=($d5,$aa,$96,$06,$00,$00,$00,$00,$86);
      run_code: code_type=($d5,$aa,$96,$06,$01,$00,$00,$00,$86);
      play_code: code_type=($d5,$aa,$96,$06,$02,$00,$00,$00,$85);
      dload_code: code_type=($d5,$aa,$96,$00,$00,$80,$00,$20,$21);
      setpage_code: code_type=($d5,$aa,$96,$05,$00,$00,$00,$00,$84);

      console_data=$378;
      console_status=$379;
      console_control=$37a;

type symboltype=record
       name: string [symbol_size];
       number: longint;
       pass: byte;
     end;
     mltype=array [0..255] of byte;

var asmfile,symfile,lstfile: text;
    asmfname,symfname,lstfname: string;
    binfile: file;
    binfname: string;

    cartridge_title: string;
    cartridge_country: byte;
    cartridge_version: byte;
    src_mode: byte;
    src_file: array [0..7] of string;
    src_line: array [0..7] of longint;
    base_mode: byte;
    base_address, base_start: array [0..7] of longint;
    irq_mode: byte;
    irq_nmi,
    irq_res,
    irq_brk,
    irq_abo,
    irq_cop,
    irq_irq: word;

    pass: byte;
    show_hex: byte;
    address,address_start,address_end: longint;
    line_count,lines_total,statement_count,statement_total: longint;
    error_count: longint;
    symbol_count: longint;
    symbols: array [0..max_symbols] of ^symboltype;
    temps_minus, temps_plus: array [1..16] of symboltype;

    { symbol, Operator, Operand, Comment }
    f1,f2,f3,f4: string;
    operator_index: integer;

    force_byte,force_word,force_long: byte;
    no_byte,no_word,no_long: byte;

    flag_debug,
    flag_display,
    flag_forwardref,
    flag_listing,
    flag_symbol: byte;

procedure save_error (s: string);
begin
  writeln (s);
end;

function inttostr (n: longint): string;
var s: string;
begin
  str (n,s);
  inttostr:=s;
end;

function nospace (s: string): string;
var i: integer;
    quote: byte;
begin
  quote:=0;
  for i:=1 to length (s) do
  begin
    if (quote=0) and (s[i]=' ') then delete (s,i,1);
    if (s[i] in ['"',#39]) then quote:=quote xor ord (s[i]);
  end;
  nospace:=s;
end;

function toupper (s: string): string;
var i: integer;
    quote: byte;
begin
  quote:=0;
  for i:=1 to length (s) do
  begin
    if (quote=0) then s[i]:=upcase (s[i]);
    if (s[i] in ['"',#39]) then quote:=quote xor ord (s[i]);
  end;
  toupper:=s;
end;

function converttabs (s: string): string;
begin
  while (pos (#9,s)>0) do
  begin
    insert ('        ',s,pos (#9,s));
    delete (s,pos (#9,s),1);
  end;     
  converttabs:=s;
end;

function dectohex (l: longint; w: byte): string;
const hextable: string=('0123456789ABCDEF');
var s: string;
    b: byte;
begin
  s:='';
  for b:=(w-1) downto 0 do
    s:=s+hextable [1+(l shr (b*4)) and 15];
  dectohex:=s;
end;

{ bintodec: <longint> decimal value
  s: <string> binary number

  Converts a binary number in text format into a decimal value. }
function bintodec (s: string): longint;
const bintable='01';
var l,m: longint;
    i: integer;
begin
  l:=0;
  m:=1;
  for i:=length (s) downto 1 do
  begin
    l:=l+(pos (s[i], bintable)-1)*m;
    m:=m shl 1;
  end;
  bintodec:=l;
end;

{ hextodec: <longint> decimal value
  s: <string> hexadecimal number

  Converts a hexadecimal number in text format into a decimal value. }
function hextodec (s: string): longint;
const hextable='0123456789ABCDEF';
var l,m: longint;
    i: integer;
begin
  l:=0;
  m:=1;
  for i:=length (s) downto 1 do
  begin
    l:=l+(pos (upcase (s[i]), hextable)-1)*m;
    m:=m shl 4;
  end;
  hextodec:=l;
end;

{ save_symbol: <integer> 0=symbol saved,1=overflow of symbols
  l: <string> symbol name,
  n: <longint> value of symbol

  Saves a new symbol and what pass the symbol was saved in. }
function save_symbol(l: string; n: longint): integer;
begin
  if (symbol_count<max_symbols) then
  begin
    symbols[symbol_count]^.name:=copy (l,1,symbol_size);
    symbols[symbol_count]^.number:=n;
    symbols[symbol_count]^.pass:=pass;
    inc (symbol_count);
    save_symbol:=0;
  end else save_symbol:=1;
end;

{ find_symbol: <integer> 0=found symbol,1=not found
  l: <string> name of symbol to search for,
  v: <integer> symbol number

  Seach for a symbol and return the symbol number in v. }
function find_symbol (s: string; var i: integer): integer;
begin
  i:=0;
  while (i<symbol_count) and not (symbols[i]^.name=copy (s,1,symbol_size)) do inc (i);
  if (i<symbol_count) then find_symbol:=0 else find_symbol:=1;
end;

{ scan_symbol: <integer) 0=symbol ok,1=invalid symbol name
  l: <string> name of symbol

  Scans a symbol name for invalid characters }
function scan_symbol (l: string): integer;
var i: integer;
begin
  i:=1;
  while not (i>length (l)) and (l[i] in ['A'..'Z','0'..'9','_']) do inc (i);
  if (i>length(l)) then scan_symbol:=0 else scan_symbol:=1;
end;

{ find_operands: <integer> 0=no error, 1=error
  e: <string> expression containing operands,
  p1: <integer> pointer to the end of the left side operand,
  p2: <integer> pointer to the beginning of the right side operand,
  op1: <string> left operand,
  op2: <string> right operand

  Locates the two operands that surround an operator and returns them. }
function find_operands (e: string; var p1,p2: integer; var op1,op2: string): integer;
var p: integer;
begin
  op1:='';
  op2:='';
  e:='~'+e+'~';
  p:=p1;
  while not (e[p] in ['+','*','/','&','|','.','~']) and (p>0) do dec (p);
  if (p=0) then
  begin
    find_operands:=1;
    op1:='0';
    op2:='0';
    exit;
  end;
  inc (p);
  op1:=copy (e, p, p1-p+2);
  p1:=p-1;

  p:=p2;
  while not (e[p] in ['+','*','/','&','|','.','~']) and not (p>length (e)) do inc (p);
  if (p>length(e)) then
  begin
    find_operands:=1;
    op1:='0';
    op2:='0';
    exit;
  end;
  dec (p);
  op2:=copy (e, p2, p-p2+1);
  p2:=p-1;

  find_operands:=0;
end;

{ eval_operator: <integer> 0=no error, 1=error
  e: <string> expression to be evaluated,
  o: <string> operator to use

  The actual routine that takes two operands and an operator and
  then calculates the end value. }
function eval_operator (var e: string; o: string): integer;
var l, l1,l2: longint;
    op1,op2: string;
    p1,p2: integer;
    n1,n2: longint;
    lo1,lo2,hi1,hi2: byte;
    i: integer;
    s: string;
begin
  eval_operator:=0;
  while (pos (o, e)>0) do
  begin
    p1:=pos (o, e)-1;
    p2:=p1+2+length(o);
    if (find_operands (e,p1,p2,op1,op2)>0) then
    begin
      eval_operator:=1;
      exit;
    end;

    { Check for negative numbers }
    n1:=1;
    n2:=1;
    if (op1[1]='-') then
    begin
      n1:=-1;
      delete (op1,1,1);
    end;
    if (op2[1]='-') then
    begin
      n2:=-1;
      delete (op2,1,1);
    end;

    { Check for forced low or high byte }
    lo1:=0;
    lo2:=0;
    hi1:=0;
    hi2:=0;
    case op1[1] of
      '<':
      begin
        lo1:=1;
        delete (op1,1,1);
      end;
      '>':
      begin
        hi1:=1;
        delete (op1,1,1);
      end;
    end;
    case op2[1] of
      '<':
      begin
        lo2:=1;
        delete (op2,1,1);
      end;
      '>':
      begin
        hi2:=1;
        delete (op2,1,1);
      end;
    end;

    { Check for a symbol }
    if (op1[1] in ['A'..'Z','_']) then
    begin
      if (find_symbol (op1,i)>0) then
      begin
        eval_operator:=err_symbolnotexist;
        l1:=0;
      end else l1:=symbols[i]^.number;
      str (l1,op1);
    end;
    if (op2[1] in ['A'..'Z','_']) then
    begin
      if (find_symbol (op2,i)>0) then
      begin
        eval_operator:=err_symbolnotexist;
        l2:=0;
      end else l2:=symbols[i]^.number;
      str (l2,op2);
    end;

    { Check for a binary number }
    if (op1[1]='%') then
    begin
      delete (op1,1,1);
      l1:=bintodec (op1);
      str(l1,op1);
    end;
    if (op2[1]='%') then
    begin
      delete (op2,1,1);
      l2:=bintodec (op2);
      str(l2,op2);
    end;

    { Check for a hexadecimal number }
    if (op1[1]='$') then
    begin
      delete (op1,1,1);
      l1:=hextodec (op1);
      str(l1,op1);
    end;
    if (op2[1]='$') then
    begin
      delete (op2,1,1);
      l2:=hextodec (op2);
      str(l2,op2);
    end;

    { Check for text }
    if (op1[1] in ['"',#39]) then
    begin
      l1:=ord (op1[2]);
      if (op1[3]<>op1[1]) then l1:=l1+ord (op1[3])*256;
      str(l1,op1);
    end;
    if (op2[1] in ['"',#39]) then
    begin
      l2:=ord (op2[2]);
      if (op2[3]<>op2[1]) then l2:=l2+ord (op2[3])*256;
      str(l2,op2);
    end;

    val (op1, l1, i);
    val (op2, l2, i);
    if (lo1>0) then l1:=l1 and 255;
    if (hi1>0) then l1:=(l1 shr 8) and 255;
    if (lo2>0) then l2:=l2 and 255;
    if (hi2>0) then l2:=(l2 shr 8) and 255;
    l1:=l1*n1;
    l2:=l2*n2;

    if (o='<<') then l:=l1 shl l2;
    if (o='>>') then l:=l1 shr l2;
    if (o='&') then l:=l1 and l2;
    if (o='.MOD.') then l:=l1 mod l2;
    if (o='|') then l:=l1 or l2;
    if (o='.XOR.') then l:=l1 xor l2;
    if (o='/') then
    begin
      if (l2=0) then
      begin
        save_error ('Division by zero');
        l:=0;
      end else l:=l1 div l2;
    end;
    if (o='*') then l:=l1*l2;
    if (o='+') then l:=l1+l2;
    str (l,s);
    delete (e, p1, p2-p1+1);
    insert (s, e, p1);
  end;
end;

{ eval: <integer> 0=no error, 1=error
  e: <string> expression,
  r: <longint> result from expression

  Evaluates the expression in order of operations. }
function eval (e: string; var r: longint): integer;
const operators: array [0..8] of string [5]=
        ('*','/','.MOD.','&','<<','>>','+','|','.XOR.');
var i: integer;
    x: integer;
begin
  eval:=1;
  for i:=0 to 8 do
  begin
    x:=eval_operator (e,operators[i]);
    if (x>0) then
    begin
      eval:=x;
      exit;
    end;
  end;
  eval:=0;
  val (e,r,i);
  case e[1] of
    '<':
    begin
      delete (e,1,1);
      val (e,r,i);
      r:=r and 255;
    end;
    '>':
    begin
      delete (e,1,1);
      val (e,r,i);
      r:=(r shr 8) and 255;
    end;
    '%':
    begin
      delete (e,1,1);
      r:=bintodec (e);
    end;
    '$':
    begin
      delete (e,1,1);
      r:=hextodec (e);
    end;
    'A'..'Z','_':
    begin
      if (find_symbol (e,i)>0) then
      begin
        eval:=err_symbolnotexist;
        r:=0;
      end else r:=symbols[i]^.number;
    end
  end;
end;

{ convertminus: <string> new expression
  e: <string> expression to change minus (-) to plus-minus (+-)

  Converts all subtraction to add the negative (+-). }
function convertminus (e: string): string;
var i: integer;
begin
  for i:=1 to length (e) do
    if (e[i]='-') and not (e[i-1] in ['+','*','/']) then
    begin

      insert ('+', e, i);
      inc (i);
    end;
  convertminus:=e;
end;

{ findmodifiers: <integer> 0=no error, 1=error
  e: <string> expression,
  ne: <string> new expression to add brackets to modifiers

  Converts modifiers into symbol format.

  SHL <<      AND &       OR  |
  SHR >>      MOD .MOD.   XOR .XOR. }
function findmodifiers (e: string): string;
const modifiers: array [0..5] of string [5]=
        (' SHL ',' SHR ',' AND ',' MOD ',' OR ',' XOR ');
      newmodifiers: array [0..5] of string [5]=
        ('<<','>>','&','.MOD.','|','.XOR.');
var p: integer;
    m: integer;
begin
  for m:=0 to 5 do
  begin
    p:=pos (modifiers[m],e);
    if (p>0) then
    begin
      delete (e,p,length(modifiers[m]));
      insert (newmodifiers[m],e,p);
    end;
  end;
  findmodifiers:=e;
end;

{ evaluate_expression: <integer> 0=no error, 1=error
  e: <string> expression to evaluate,
  var ne: <string> new expression,
  var r: <longint> resulting number from expression

  Takes an expression and returns the result. }
function evaluate_expression (e: string; var ne: string; var r: longint): integer;
var p,pbegin,pend: integer;
    pexpression: string;
    result: longint;
    i: integer;
begin
  i:=0;
  while (e[i+1]='-') and (i<=length(e)) do inc (i);
  if (e='-') or (i>1) then
  begin
    if (i>16) then i:=16;
    ne:=e;
    r:=temps_minus[i].number;
    evaluate_expression:=0;
    exit;
  end;
  i:=0;
  while (e[i+1]='+') and (i<=length(e)) do inc (i);
  if (e='+') or (i>1) then
  begin
    if (i>16) then i:=16;
    ne:=e;
    r:=temps_plus[i].number;
    evaluate_expression:=0;
    exit;
  end;

  evaluate_expression:=1;
  r:=0;

  e:=toupper (e);               { Convert all text to uppercase }
  e:=findmodifiers (e);         { Convert modifiers to symbol form }
  e:=nospace (e);               { Remove all spaces in expression }
  e:=convertminus (e);          { Convert all - to +- }

  { Save new expression and change periods (.) back to spaces ( ) }
  ne:=e;
  while (pos ('.', ne)>0) do ne [pos ('.', ne)]:=' ';

  { Find presence of all brackets and evaluate expressions in brackets }
  while (pos (')', e) > 0) do
  begin
    pend:=pos (')', e)+1;
    p:=pend;
    while (p>0) and (e [p]<>'(') do dec (p);
    if (p=0) then exit;
    pbegin:=p;
    pexpression:=copy (e, pbegin+1, pend-pbegin-2);
    if (eval (pexpression, result)=0) then
    begin
      delete (e, pbegin, pend-pbegin);
      str (result, pexpression);
      insert (pexpression, e, pbegin);
    end else exit;
  end;
  if (pos ('(', e)>0) then exit;
  if (e<>'') then evaluate_expression:=eval (e,r);
{
  if (eval (e,r)>0) then exit;
  evaluate_expression:=0;
}
end;

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
    force_byte:=0;
    force_word:=0;
    force_long:=0;
    s:=f1;
    if (s[4]='.') then
    begin
      case s[5] of
        'B': force_byte:=1;
        'W': force_word:=1;
        'L': force_long:=1;
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
        'B': force_byte:=1;
        'W': force_word:=1;
        'L': force_long:=1;
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

function define_name (f1,f2,f3,f4: string): integer;
var e: integer;
begin
  e:=0;
  if (f3[1] in ['"',#39]) then
  begin
    delete (f3,1,1);
    delete (f3,length(f3),1);
    cartridge_title:=copy (f3,1,20);
  end;
  cartridge_title:=copy (f3,1,20);
  define_name:=e;
end;

function define_version (f1,f2,f3,f4: string): integer;
var e: integer;
    s: string;
    l: longint;
begin
  e:=evaluate_expression (f3,s,l);
  if (e=0) then cartridge_version:=l else cartridge_version:=0;
  define_version:=e;
end;

function define_country (f1,f2,f3,f4: string): integer;
var e: integer;
    s: string;
    l: longint;
begin
  e:=evaluate_expression (f3,s,l);
  if (e=0) then cartridge_country:=l else cartridge_country:=1;
  define_country:=e;
end;

function define_interrupts (f1,f2,f3,f4: string): integer;
var s: string;
    e: integer;
    r: longint;
label end_interrupts;
begin
  define_interrupts:=0;

  if (f2='END') then
  begin
    irq_mode:=0;
    goto end_interrupts;
  end;

  if (f2='=') or (f2='EQU') then
  begin
    e:=evaluate_expression (f3,s,r);
    if (e=0) then
    begin
      if (f1='NMI') then irq_nmi:=r and $ffff;
      if (f1='RES') or (f1='RESET') then irq_res:=r and $ffff;
      if (f1='BRK') or (f1='BREAK') then irq_brk:=r and $ffff;
      if (f1='ABO') or (f1='ABORT') then irq_abo:=r and $ffff;
      if (f1='COP') then irq_cop:=r and $ffff;
      if (f1='IRQ') then irq_irq:=r and $ffff;
      if (f1='ALL') then
      begin
        irq_nmi:=r and $ffff;
        irq_res:=r and $ffff;
        irq_brk:=r and $ffff;
        irq_abo:=r and $ffff;
        irq_cop:=r and $ffff;
        irq_irq:=r and $ffff;
      end;
    end else define_interrupts:=0;
  end else define_interrupts:=err_badirqdefinition;

end_interrupts:
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

function typeff (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
var e: integer;
begin
  e:=0;
  nob:=0;

  if (f2='PAD') then e:=pad_file (ml,nob,f1,f2,f3,f4) else
  if (f2='BIN') or (f2='INCBIN') then e:=binary_append (ml,nob,f1,f2,f3,f4) else
  if (f2='SRC') or (f2='INCSRC') then e:=source_append (ml,nob,f1,f2,f3,f4) else
  if (f2='DCB') or (f2='DC.B') or (f2='DB') then e:=data_string_byte (ml,nob,f1,f2,f3,f4) else
  if (f2='DCW') or (f2='DC.W') or (f2='DW') then e:=data_string_word (ml,nob,f1,f2,f3,f4) else
  if (f2='DCD') or (f2='DC.D') or (f2='DD') then e:=data_string_dword (ml,nob,f1,f2,f3,f4) else
  if (f2='DSB') or (f2='DS.B') then e:=data_buffer_byte (ml,nob,f1,f2,f3,f4) else
  if (f2='DSW') or (f2='DS.W') then e:=data_buffer_word (ml,nob,f1,f2,f3,f4) else
  if (f2='DSD') or (f2='DS.D') then e:=data_buffer_dword (ml,nob,f1,f2,f3,f4) else
  if (f2='COU') or (f2='COUNTRY') then e:=define_country (f1,f2,f3,f4) else
  if (f2='NAM') or (f2='NAME') or (f3='TIT') or (f3='TITLE') then e:=define_name (f1,f2,f3,f4) else
  if (f2='VER') or (f2='VERSION') then e:=define_version (f1,f2,f3,f4) else
  if (f2='BAS') or (f2='BASE') then e:=base_change (f1,f2,f3,f4) else
  if (f2='INT') or (f2='INTERRUPTS') then
  begin
    irq_mode:=1
  end else
  if (f2='ORG') then e:=org_change (ml,nob,f1,f2,f3,f4) else
  if (f2='=') or (f2='EQU') then e:=0 else
    e:=err_badoperator;

  typeff:=e;
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
          if (save_symbol (f1,operand)>0) then e:=err_overflowsymbols;
        end else
        begin
          e:=err_badoperand;
          operand:=0;
          if (save_symbol (f1,operand)>0) then e:=err_overflowsymbols;
        end;
      end else

      { Symbol is defined as the current address }
      if (save_symbol (f1,address)>0) then e:=err_overflowsymbols else e:=0;
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
          if (save_symbol (f1,operand)>0) then e:=err_overflowsymbols else e:=0;
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

procedure make_smc_file;
var w: word;
    buf: array [0..32767] of byte;
    header: array [0..511] of byte;
    l: longint;
begin
  fillchar (header, sizeof (header),0);
  assign (binfile, binfname);
  reset (binfile,1);
  l:=filesize (binfile);
  if (l<$8000) then l:=$8000;
  l:=l div 8192;
  header[0]:=lo(word(l));
  header[1]:=hi(word(l));
  header[2]:=0;
  header[8]:=$aa;
  header[9]:=$bb;
  header[10]:=$04;
  repeat
    blockread (binfile, buf, sizeof (buf), w);
  until (w<>sizeof (buf));

  fillchar (buf, sizeof (buf),0);

  fillchar (header, sizeof (header),0);
  header [$3c]:=lo (word(address_start));
  header [$3d]:=hi (word(address_start));
  move (cartridge_title[1], header, length(cartridge_title)); { title name }
  l:=(filesize(binfile)*8) div 1048576;
  if (l<=4) then header [$17]:=$09 else { 4 megabit }
    if (l<=8) then header [$17]:=$0a else { 8 megabit }
      header [$17]:=$0b; { 12 or 16 megabit }
  header [$19]:=lo (word(cartridge_country)); { country code }
  header [$1b]:=lo (word(cartridge_version-1)); { version # }

  { native mode interrupts }

  header [$24]:=lo (irq_cop); { co-processor }
  header [$25]:=hi (irq_cop);
  header [$26]:=lo (irq_brk); { break }
  header [$27]:=hi (irq_brk);
  header [$28]:=lo (irq_abo); { abort }
  header [$29]:=hi (irq_abo);
  header [$2a]:=lo (irq_nmi); { non-maskable }
  header [$2b]:=hi (irq_nmi);

  header [$2e]:=lo (irq_irq); { interrupt }
  header [$2f]:=hi (irq_irq);

  { emulation mode interrupts }
  header [$34]:=lo (irq_cop); { co-processor }
  header [$35]:=hi (irq_cop);
  header [$38]:=lo (irq_abo); { abort }
  header [$39]:=hi (irq_abo);
  header [$3a]:=lo (irq_nmi); { non-maskable }
  header [$3b]:=hi (irq_nmi);
  header [$3c]:=lo (irq_res); { reset, both native and emulation }
  header [$3d]:=hi (irq_res);
  header [$3e]:=lo (irq_brk); { break }
  header [$3f]:=hi (irq_brk);

  close (binfile);
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

function init (s: string): integer;
var i: integer;
begin
  init:=1;
  symfname:=s+'.sym';
  lstfname:=s+'.lst';
  assign (lstfile, lstfname);
  binfname:=s+'.bin';
  assign (binfile, binfname);
  cartridge_title:='(C) 1994 TricksASM';
  cartridge_country:=1;
  cartridge_version:=1;
  address_start:=$008000;
  src_mode:=0;
  base_mode:=0;
  irq_mode:=0;
  irq_nmi:=0;
  irq_res:=0;
  irq_brk:=0;
  irq_abo:=0;
  irq_cop:=0;
  irq_irq:=0;
  symbol_count:=0;
  flag_debug:=0;
  flag_display:=0;
  flag_forwardref:=1;
  flag_listing:=0;
  flag_symbol:=0;
  for i:=1 to paramcount do
  begin
    if (paramstr(i)='-z') then flag_debug:=1;
    if (paramstr(i)='-$') then flag_symbol:=1;
    if (paramstr(i)='-d') then flag_display:=1;
    if (paramstr(i)='-f') then flag_forwardref:=0;
    if (paramstr(i)='-l') then flag_listing:=1;
  end;
  if (get_symbolmemory (max_symbols)>0) then exit;
  init:=0;
end;

var i: integer;
begin
  writeln;
  writeln ('65816 Tricks Assembler Version 1.11   (C)opyright 1994 1000 Miles [Tricks]');
  writeln ('Internet: norman_yen@idream.tfbbs.wimsey.com, IRC: minus');
  writeln ('Bin file only version 2007/02/01 by nanashi');
  writeln;

  if (paramcount=0) then
  begin
    writeln ('     USAGE: TRASM_B [options] source');
    writeln;
    writeln ('     Options: -$ ... Generate symbol table   (default off)');
    writeln ('              -d ... Display assembly        (default off)');
    writeln ('              -f ... Show forward reference  (default on)');
    writeln ('              -l ... Generate source listing (default off)');
    writeln ('              -p ... Ignored                 (default off)');
    writeln ('              -s ... Ignored                 (default off)');
    writeln ('              -z ... Internal debugging      (default off)');
    writeln;
  end else
  begin
    i:=1;
    while (copy (paramstr(i),1,1)='-') or (copy (paramstr (i),1,1)='/') do inc (i);
    asmfname:=paramstr (i);
    if (pos ('.',asmfname)=0) then asmfname:=asmfname+'.asm';

    assign (asmfile, asmfname);
    {$I-}
    reset (asmfile);
    close (asmfile);
    if (ioresult<>0) then
    begin
      writeln ('Invalid source filename.');
      halt(1);
    end;
    {$I+}

    if (init (copy (asmfname, 1, pos ('.', asmfname)-1))>0) then
    begin
      writeln ('Not enough memory available, 400 KB required.');
      halt(1);
    end;

    case find_startaddress of
      0: save_error ('Starting address defined as $'+dectohex (address_start,6));
      1: save_error ('No start address defined, defaulting to $'+dectohex (address_start,6));
      2: save_error ('Error in start address definition, defaulting to $'+dectohex (address_start,6));
    end;
    address:=address_start;

    for pass:=1 to 3 do
    begin
      save_error ('');
      save_error ('Pass '+inttostr(pass));
      if (pass=3) then rewrite (binfile,1);
      if (pass=3) and (flag_listing=1) then rewrite (lstfile);
      line_count:=0;
      lines_total:=0;
      statement_total:=0;
      error_count:=0;
      address:=address_start;
      i:=processpass;
      if (i>0) then
      begin
        save_error ('Fatal error, unable to continue assembling.');
        close (asmfile);
        close (binfile);
        halt (1);
      end;
      if (pass=3) then
      begin
        close (binfile);
        if (flag_listing=1) then close (lstfile);
        make_smc_file;
        save_error (' ');
        save_error ('Source code assembled.');
        if (error_count>0) then
        begin
          save_error ('');
          save_error ('Although the source code was assembled, the program may not run');
          save_error ('properly due to errors detected during the assembling process.');
        end;
      end;
    end;

    writeln;
    if (flag_display=1) then
    begin
      for i:=0 to symbol_count-1 do
        writeln (symbols[i]^.name,'=',dectohex(symbols[i]^.number,6));
    end;
    if (flag_symbol=1) then save_symbol_list;

    address_end:=address;
    save_error ('[$'+dectohex (address_start,6)+'-$'+dectohex (address_end,6)+']');
    save_error ('Lines='+inttostr(lines_total));
    save_error ('Statements='+inttostr(statement_total));
    save_error ('Symbols='+inttostr(symbol_count));
    save_error ('Errors='+inttostr(error_count));
    free_symbolmemory (max_symbols);
  end;
end.
