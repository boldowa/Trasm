{
To compile this program with Free Pascal, type this command in command prompt. (Set PATH variable first!)
fpc -Mtp trasm.pas
}

program _65816_Tricks_Assembler;
{$M 49152,0,655360}
{
65816 Tricks Assembler Version 1.11   (C)opyright 1994
Coded by 1000 Miles [Tricks]
}

uses
	trerror,
	trutils,
	trcommon;

var
    errfile: text;
    errfname: string;
    smcfile: file;
    smcfname: string;

    cartridge_title: string;
    cartridge_country: byte;
    cartridge_version: byte;
    use_irq: byte;
    irq_nmi,
    irq_res,
    irq_brk,
    irq_abo,
    irq_cop,
    irq_irq: word;

    symbol_count: longint;
    symbols: array [0..max_symbols] of ^symboltype;

    flag_pad: byte;

procedure _save_error (s: string);
begin
	writeln (s);
	writeln (errfile,s);
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

function _define_interrupts (f1,f2,f3,f4: string): integer;
var s: string;
    e: integer;
    r: longint;
label end_interrupts;
begin
  _define_interrupts:=0;

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
    end else _define_interrupts:=0;
  end else _define_interrupts:=err_badirqdefinition;

end_interrupts:
end;


function _typeff (var ml: mltype; var nob: longint; f1,f2,f3,f4: string): integer;
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
    use_irq:=1;
    irq_mode:=1
  end else
  if (f2='ORG') then e:=org_change (ml,nob,f1,f2,f3,f4) else
  if (f2='=') or (f2='EQU') then e:=0 else
    e:=err_badoperator;

  _typeff:=e;
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
  assign (smcfile, smcfname);
  rewrite (smcfile,1);
  blockwrite (smcfile, header, 512);
  repeat
    blockread (binfile, buf, sizeof (buf), w);
    blockwrite (smcfile, buf, w);
  until (w<>sizeof (buf));

  fillchar (buf, sizeof (buf),0);

  if (filesize (binfile)<32768) then
    blockwrite (smcfile, buf, 32768-filesize (binfile));

  { pad file to next 32K }
  if ((filesize (smcfile)-512) mod 32768>0) and (flag_pad=1) then
    blockwrite (smcfile, buf, 32768-(filesize (smcfile)-512) mod 32768);

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

  seek (smcfile, $8000+$0200-$40);
  if (use_irq=0) then blockwrite (smcfile, header, 36) else
    blockwrite (smcfile, header, 64);

  seek (smcfile, $7ffc+$0200);
  blockread (smcfile, w, 2);
  if (w=0) and (use_irq=0) then
  begin
    w:=address_start and $ffff;
    seek (smcfile, $7ffc+$0200);
    blockwrite (smcfile, w, 2);
  end;

  close (binfile);
  close (smcfile);
end;

function init (s: string): integer;
var i: integer;
begin
  init:=1;
  symfname:=s+'.sym';
  lstfname:=s+'.lst';
  assign (lstfile, lstfname);
  errfname:=s+'.err';
  assign (errfile, errfname);
  binfname:=s+'.bin';
  assign (binfile, binfname);
  smcfname:=s+'.smc';
  cartridge_title:='(C) 1994 TricksASM';
  cartridge_country:=1;
  cartridge_version:=1;
  address_start:=$008000;
  src_mode:=0;
  base_mode:=0;
  irq_mode:=0;
  use_irq:=0;
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
  flag_pad:=0;
  for i:=1 to paramcount do
  begin
    if (paramstr(i)='-z') then flag_debug:=1;
    if (paramstr(i)='-$') then flag_symbol:=1;
    if (paramstr(i)='-d') then flag_display:=1;
    if (paramstr(i)='-f') then flag_forwardref:=0;
    if (paramstr(i)='-l') then flag_listing:=1;
    if (paramstr(i)='-p') then flag_pad:=1;
  end;
  if (get_symbolmemory (max_symbols)>0) then exit;
  init:=0;
end;

procedure init_proc;
begin
	save_error := _save_error;
	typeff := _typeff;
	define_interrupts := _define_interrupts;
end;

var i: integer;
begin
  init_proc;
  writeln;
  writeln ('65816 Tricks Assembler Version 1.11   (C)opyright 1994 1000 Miles [Tricks]');
  writeln ('Internet: norman_yen@idream.tfbbs.wimsey.com, IRC: minus');
  writeln;
  if (paramcount=0) then
  begin
    writeln ('     USAGE: TRASM [options] source');
    writeln;
    writeln ('     Options: -$ ... Generate symbol table   (default off)');
    writeln ('              -d ... Display assembly        (default off)');
    writeln ('              -f ... Show forward reference  (default on)');
    writeln ('              -l ... Generate source listing (default off)');
    writeln ('              -p ... Pad SMC file to 32K     (default off)');
    writeln ('              -s ... Ignored                 (default off)');
    writeln ('              -z ... Internal debugging      (default off)');
    writeln;
  end else
  begin
    i:=1;
    while (copy (paramstr(i),1,1)='-') do inc (i);
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

    rewrite (errfile);
    writeln (errfile,'Error listing for ',toupper (asmfname));
    writeln (errfile);

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
        close (errfile);
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
    writeln (errfile);
    close (errfile);
    free_symbolmemory (max_symbols);
  end;
end.
