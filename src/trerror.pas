unit trerror;

interface

const
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

type
	Tsave_error = procedure(s: string);

var
	save_error: Tsave_error;

implementation

end.

