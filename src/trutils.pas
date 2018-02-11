{ TRASM utility unit }
unit trutils;

interface

uses
	trerror;

	function inttostr (n: longint): string;
	function nospace (s: string): string;
	function toupper (s: string): string;
	function converttabs (s: string): string;
	function dectohex (l: longint; w: byte): string;
	function bintodec (s: string): longint;
	function hextodec (s: string): longint;
	function save_symbol(l: string; n: longint; pass: byte): integer;
	function find_symbol (s: string; var i: integer): integer;
	function scan_symbol (l: string): integer;
	function find_operands (e: string; var p1,p2: integer; var op1,op2: string): integer;
	function eval_operator (var e: string; o: string): integer;
	function eval (e: string; var r: longint): integer;
	function convertminus (e: string): string;
	function findmodifiers (e: string): string;
	function evaluate_expression (e: string; var ne: string; var r: longint): integer;


const	symbol_size=18;
	max_symbols=8192;


type	symboltype=record
		name: string [symbol_size];
		number: longint;
		pass: byte;
	end;

	mltype=array [0..255] of byte;


var
	symbols: array [0..max_symbols] of ^symboltype;
	symbol_count: longint;
	temps_minus, temps_plus: array [1..16] of symboltype;


implementation



function inttostr (n: longint): string;
var	s: string;
begin
	str (n,s);
	inttostr:=s;
end;


function nospace (s: string): string;
var	i: integer;
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
var	i: integer;
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
var	s: string;
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
var	l,m: longint;
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
var	l,m: longint;
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
  pass: <byte> process pass

  Saves a new symbol and what pass the symbol was saved in. }
function save_symbol(l: string; n: longint; pass: byte): integer;
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
var	l, l1,l2: longint;
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
var	i: integer;
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
var	i: integer;
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

	SHL <<			AND &			 OR	|
	SHR >>			MOD .MOD.	 XOR .XOR. }
function findmodifiers (e: string): string;
const modifiers: array [0..5] of string [5]=
				(' SHL ',' SHR ',' AND ',' MOD ',' OR ',' XOR ');
			newmodifiers: array [0..5] of string [5]=
				('<<','>>','&','.MOD.','|','.XOR.');
var	p: integer;
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
var	p,pbegin,pend: integer;
	pexpression: string;
	result: longint;
	i: integer;
begin
	i:=0;
	while (i<=length(e)) and (e[i+1]='-') do inc (i);
	if (e='-') or (i>1) then
	begin
		if (i>16) then i:=16;
		ne:=e;
		r:=temps_minus[i].number;
		evaluate_expression:=0;
		exit;
	end;
	i:=0;
	while (i<=length(e)) and (e[i+1]='+') do inc (i);
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

	e:=toupper (e);							 { Convert all text to uppercase }
	e:=findmodifiers (e);				 { Convert modifiers to symbol form }
	e:=nospace (e);							 { Remove all spaces in expression }
	e:=convertminus (e);					{ Convert all - to +- }

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


end.

