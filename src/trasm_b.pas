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

uses
	trerror,
	trutils,
	trcommon;


procedure _save_error (s: string);
begin
	writeln (s);
end;


function define_name (f1,f2,f3,f4: string): integer;
var e: integer;
begin
  e:=0;
  if (f3[1] in ['"',#39]) then
  begin
    delete (f3,1,1);
    delete (f3,length(f3),1);
  end;
  define_name:=e;
end;

function define_version (f1,f2,f3,f4: string): integer;
var e: integer;
    s: string;
    l: longint;
begin
  e:=evaluate_expression (f3,s,l);
  define_version:=e;
end;

function define_country (f1,f2,f3,f4: string): integer;
var e: integer;
    s: string;
    l: longint;
begin
  e:=evaluate_expression (f3,s,l);
  define_country:=e;
end;

function _define_interrupts (f1,f2,f3,f4: string): integer;
label end_interrupts;
begin
  _define_interrupts:=0;

  if (f2='END') then
  begin
    irq_mode:=0;
    goto end_interrupts;
  end;

  if (f2<>'=') and (f2<>'EQU') then
  _define_interrupts:=err_badirqdefinition;

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
    irq_mode:=1
  end else
  if (f2='ORG') then e:=org_change (ml,nob,f1,f2,f3,f4) else
  if (f2='=') or (f2='EQU') then e:=0 else
    e:=err_badoperator;

  _typeff:=e;
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
  address_start:=$008000;
  src_mode:=0;
  base_mode:=0;
  irq_mode:=0;
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
