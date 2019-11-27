{$mode objfpc}
{$assertions on}

{$ifdef win32}
  {$define cpux86}
{$endif}

{$ifdef fpc}
  {$ifdef cpu64}
    {$define cpux64}
  {$endif cpu64}
  {$ifdef cpu32}
    {$define cpux86}
  {$endif cpu32}
  {$ifdef darwin}
    {$define macos}
    {$define align_stack}
    {$ifdef cpu32}
      {$define macos32}
    {$endif cpu32}
  {$endif darwin}
{$endif fpc}

{$modeswitch cvar}

unit Python3Core;
interface
uses
{$IFDEF windows}
  Windows,
{$ELSE}
  CTypes,
  Types,
{$ENDIF}
  Classes,
  SysUtils,
  SyncObjs,
  Variants,
  TinyWideStrings
  ;

//#######################################################
//##                                                   ##
//##           PYTHON specific constants               ##
//##                                                   ##
//#######################################################

type
{$IFNDEF UNICODE}
  UnicodeString = WideString;
  TUnicodeStringList = TWideStringList;
{$ELSE}
  TUnicodeStringList = TStringList;
{$ENDIF}

{$IFNDEF FPC}
  {$IF CompilerVersion < 21}
    NativeInt = integer;
    NativeUInt = Cardinal;
  {$IFEND}
  PNativeInt = ^NativeInt;
{$ELSE}
  {$IF DEFINED(FPC_FULLVERSION) and (FPC_FULLVERSION >= 20500)}
  {$ELSE}
    NativeInt = integer;
    NativeUInt = Cardinal;
  {$IFEND}
  PNativeInt = ^NativeInt;
{$ENDIF}

const
  PYT_METHOD_BUFFER_INCREASE = 10;
  PYT_MEMBER_BUFFER_INCREASE = 10;
  PYT_GETSET_BUFFER_INCREASE = 10;

  METH_VARARGS  = $0001;
  METH_KEYWORDS = $0002;

  // Masks for the co_flags field of PyCodeObject
  CO_OPTIMIZED   = $0001;
  CO_NEWLOCALS   = $0002;
  CO_VARARGS     = $0004;
  CO_VARKEYWORDS = $0008;

  // Rich comparison opcodes introduced in version 2.1
  Py_LT = 0;
  Py_LE = 1;
  Py_EQ = 2;
  Py_NE = 3;
  Py_GT = 4;
  Py_GE = 5;
type
  // Delphi equivalent used by TPyObject
  TRichComparisonOpcode = (pyLT, pyLE, pyEQ, pyNE, pyGT, pyGE);
const
{Type flags (tp_flags) introduced in version 2.0

These flags are used to extend the type structure in a backwards-compatible
fashion. Extensions can use the flags to indicate (and test) when a given
type structure contains a new feature. The Python core will use these when
introducing new functionality between major revisions (to avoid mid-version
changes in the PYTHON_API_VERSION).

Arbitration of the flag bit positions will need to be coordinated among
all extension writers who publically release their extensions (this will
be fewer than you might expect!)..

Python 1.5.2 introduced the bf_getcharbuffer slot into PyBufferProcs.

Type definitions should use Py_TPFLAGS_DEFAULT for their tp_flags value.

Code can use PyType_HasFeature(type_ob, flag_value) to test whether the
given type object has a specified feature.
}

// PyBufferProcs contains bf_getcharbuffer
  Py_TPFLAGS_HAVE_GETCHARBUFFER = (1 shl 0);

// PySequenceMethods contains sq_contains
  Py_TPFLAGS_HAVE_SEQUENCE_IN = (1 shl 1);

// Objects which participate in garbage collection (see objimp.h)
  Py_TPFLAGS_GC = (1 shl 2);

// PySequenceMethods and PyNumberMethods contain in-place operators
  Py_TPFLAGS_HAVE_INPLACEOPS = (1 shl 3);

// PyNumberMethods do their own coercion */
  Py_TPFLAGS_CHECKTYPES = (1 shl 4);

  Py_TPFLAGS_HAVE_RICHCOMPARE = (1 shl 5);

// Objects which are weakly referencable if their tp_weaklistoffset is >0
// XXX Should this have the same value as Py_TPFLAGS_HAVE_RICHCOMPARE?
// These both indicate a feature that appeared in the same alpha release.

  Py_TPFLAGS_HAVE_WEAKREFS = (1 shl 6);

// tp_iter is defined
  Py_TPFLAGS_HAVE_ITER = (1 shl 7);

// New members introduced by Python 2.2 exist
  Py_TPFLAGS_HAVE_CLASS = (1 shl 8);

// Set if the type object is dynamically allocated
  Py_TPFLAGS_HEAPTYPE = (1 shl 9);

// Set if the type allows subclassing
  Py_TPFLAGS_BASETYPE = (1 shl 10);

// Set if the type is 'ready' -- fully initialized
  Py_TPFLAGS_READY = (1 shl 12);

// Set while the type is being 'readied', to prevent recursive ready calls
  Py_TPFLAGS_READYING = (1 shl 13);

// Objects support garbage collection (see objimp.h)
  Py_TPFLAGS_HAVE_GC = (1 shl 14);

  Py_TPFLAGS_DEFAULT  =      Py_TPFLAGS_HAVE_GETCHARBUFFER
                             or Py_TPFLAGS_HAVE_SEQUENCE_IN
                             or Py_TPFLAGS_HAVE_INPLACEOPS
                             or Py_TPFLAGS_HAVE_RICHCOMPARE
                             or Py_TPFLAGS_HAVE_WEAKREFS
                             or Py_TPFLAGS_HAVE_ITER
                             or Py_TPFLAGS_HAVE_CLASS
                             or Py_TPFLAGS_BASETYPE
                             ;

// See function PyType_HasFeature below for testing the flags.

// Delphi equivalent used by TPythonType
type
  TPFlag = (tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfGC, tpfHaveInplaceOps,
            tpfCheckTypes, tpfHaveRichCompare, tpfHaveWeakRefs
            ,tpfHaveIter, tpfHaveClass, tpfHeapType, tpfBaseType, tpfReady, tpfReadying, tpfHaveGC
            );
  TPFlags = set of TPFlag;
const
  TPFLAGS_DEFAULT = [tpfHaveGetCharBuffer, tpfHaveSequenceIn, tpfHaveInplaceOps,
                     tpfHaveRichCompare, tpfHaveWeakRefs, tpfHaveIter,
                     tpfHaveClass, tpfBaseType
                    ];
//-------  Python opcodes  ----------//
Const
   single_input                     = 256;
   file_input                       = 257;
   eval_input                       = 258;
   p4d_funcdef                      = 259;
   p4d_parameters                   = 260;
   p4d_varargslist                  = 261;
   p4d_fpdef                        = 262;
   p4d_fplist                       = 263;
   p4d_stmt                         = 264;
   p4d_simple_stmt                  = 265;
   p4d_small_stmt                   = 266;
   p4d_expr_stmt                    = 267;
   p4d_augassign                    = 268;
   p4d_print_stmt                   = 269;
   p4d_del_stmt                     = 270;
   p4d_pass_stmt                    = 271;
   p4d_flow_stmt                    = 272;
   p4d_break_stmt                   = 273;
   p4d_continue_stmt                = 274;
   p4d_return_stmt                  = 275;
   p4d_raise_stmt                   = 276;
   p4d_import_stmt                  = 277;
   p4d_import_as_name               = 278;
   p4d_dotted_as_name               = 279;
   p4d_dotted_name                  = 280;
   p4d_global_stmt                  = 281;
   p4d_exec_stmt                    = 282;
   p4d_assert_stmt                  = 283;
   p4d_compound_stmt                = 284;
   p4d_if_stmt                      = 285;
   p4d_while_stmt                   = 286;
   p4d_for_stmt                     = 287;
   p4d_try_stmt                     = 288;
   p4d_except_clause                = 289;
   p4d_suite                        = 290;
   p4d_test                         = 291;
   p4d_and_test                     = 291;
   p4d_not_test                     = 293;
   p4d_comparison                   = 294;
   p4d_comp_op                      = 295;
   p4d_expr                         = 296;
   p4d_xor_expr                     = 297;
   p4d_and_expr                     = 298;
   p4d_shift_expr                   = 299;
   p4d_arith_expr                   = 300;
   p4d_term                         = 301;
   p4d_factor                       = 302;
   p4d_power                        = 303;
   p4d_atom                         = 304;
   p4d_listmaker                    = 305;
   p4d_lambdef                      = 306;
   p4d_trailer                      = 307;
   p4d_subscriptlist                = 308;
   p4d_subscript                    = 309;
   p4d_sliceop                      = 310;
   p4d_exprlist                     = 311;
   p4d_testlist                     = 312;
   p4d_dictmaker                    = 313;
   p4d_classdef                     = 314;
   p4d_arglist                      = 315;
   p4d_argument                     = 316;
   p4d_list_iter                    = 317;
   p4d_list_for                     = 318;
   p4d_list_if                      = 319;

  // structmember.h
const
//* Types */
  T_SHORT                       = 0;
  T_INT                         = 1;
  T_LONG                        = 2;
  T_FLOAT                       = 3;
  T_DOUBLE                      = 4;
  T_STRING                      = 5;
  T_OBJECT                      = 6;
//* XXX the ordering here is weird for binary compatibility */
  T_CHAR                        = 7;  //* 1-character string */
  T_BYTE                        = 8;  //* 8-bit signed int */
//* unsigned variants: */
  T_UBYTE                       = 9;
  T_USHORT                      = 10;
  T_UINT                        = 11;
  T_ULONG                       = 12;

//* Added by Jack: strings contained in the structure */
  T_STRING_INPLACE= 13;

  T_OBJECT_EX                   = 16;{* Like T_OBJECT, but raises AttributeError
                                        when the value is NULL, instead of
                                        converting to None. *}

//* Flags */
  READONLY                      = 1;
  RO                            = READONLY;   //* Shorthand */
  READ_RESTRICTED               = 2;
  WRITE_RESTRICTED              = 4;
  RESTRICTED                    = (READ_RESTRICTED or WRITE_RESTRICTED);
type
  TPyMemberType = (mtShort, mtInt, mtLong, mtFloat, mtDouble, mtString, mtObject,
                   mtChar, mtByte, mtUByte, mtUShort, mtUInt, mtULong,
                   mtStringInplace, mtObjectEx);
  TPyMemberFlag = (mfDefault, mfReadOnly, mfReadRestricted, mfWriteRestricted, mfRestricted);

//#######################################################
//##                                                   ##
//##           Non-Python specific constants           ##
//##                                                   ##
//#######################################################

const
  ErrInit         = -300;
  CR              = #13;
  LF              = #10;
  TAB             = #09;
  CRLF            = CR+LF;



//#######################################################
//##                                                   ##
//##    Global declarations, nothing Python specific   ##
//##                                                   ##
//#######################################################

type
   TPAnsiChar     = array[0..16000] of PAnsiChar;
   TPWideChar = array[0..16000] of PWideChar;
   PPAnsiChar     = ^TPAnsiChar;
   PPWideChar = ^TPWideChar;
   PInt       = ^Integer;
   PDouble    = ^Double;
   PFloat     = ^Real;
   PLong      = ^LongInt;
   PShort     = ^ShortInt;


//#######################################################
//##                                                   ##
//##            Python specific interface              ##
//##                                                   ##
//#######################################################

type
  PP_frozen     = ^P_frozen;
  P_frozen      = ^_frozen;
  PPyObject     = ^PyObject;
  PPPyObject      = ^PPyObject;
  PPPPyObject     = ^PPPyObject;
  PPyIntObject      = ^PyIntObject;
  PPyTypeObject     = ^PyTypeObject;
  PPySliceObject    = ^PySliceObject;

  AtExitProc        = procedure;
  PyCFunction       = function( self, args:PPyObject): PPyObject; cdecl;
  PyCFunctionWithKW = function( self, args, keywords:PPyObject): PPyObject; cdecl;

  unaryfunc         = function( ob1 : PPyObject): PPyObject; cdecl;
  binaryfunc        = function( ob1,ob2 : PPyObject): PPyObject; cdecl;
  ternaryfunc       = function( ob1,ob2,ob3 : PPyObject): PPyObject; cdecl;
  inquiry           = function( ob1 : PPyObject): integer; cdecl;
  lenfunc           = function( ob1 : PPyObject): NativeInt; cdecl;
  coercion          = function( ob1,ob2 : PPPyObject): integer; cdecl;
  ssizeargfunc      = function( ob1 : PPyObject; i: NativeInt): PPyObject; cdecl;
  ssizessizeargfunc = function( ob1 : PPyObject; i1, i2: NativeInt):
                                PPyObject; cdecl;
  ssizeobjargproc   = function( ob1 : PPyObject; i: NativeInt; ob2 : PPyObject):
                                integer; cdecl;
  ssizessizeobjargproc = function( ob1: PPyObject; i1, i2: NativeInt;
                                ob2: PPyObject): integer; cdecl;
  objobjargproc     = function( ob1,ob2,ob3 : PPyObject): integer; cdecl;

  pydestructor      = procedure(ob: PPyObject); cdecl;
  printfunc         = function( ob: PPyObject; var f: file; i: integer): integer; cdecl;
  getattrfunc       = function( ob1: PPyObject; name: PAnsiChar): PPyObject; cdecl;
  setattrfunc       = function( ob1: PPyObject; name: PAnsiChar; ob2: PPyObject): integer; cdecl;
  cmpfunc           = function( ob1,ob2: PPyObject): integer; cdecl;
  reprfunc          = function( ob: PPyObject): PPyObject; cdecl;
  hashfunc          = function( ob: PPyObject): NativeInt; cdecl; // !! in 2.x it is still a LongInt
  getattrofunc      = function( ob1,ob2: PPyObject): PPyObject; cdecl;
  setattrofunc      = function( ob1,ob2,ob3: PPyObject): integer; cdecl;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from object.h
  getreadbufferproc = function ( ob1: PPyObject; i: NativeInt; ptr: Pointer): NativeInt; cdecl;
  getwritebufferproc= function ( ob1: PPyObject; i: NativeInt; ptr: Pointer): NativeInt; cdecl;
  getsegcountproc   = function ( ob1: PPyObject; i: NativeInt): NativeInt; cdecl;
  getcharbufferproc = function ( ob1: PPyObject; i: NativeInt; const pstr: PAnsiChar): NativeInt; cdecl;
  objobjproc        = function ( ob1, ob2: PPyObject): integer; cdecl;
  visitproc         = function ( ob1: PPyObject; ptr: Pointer): integer; cdecl;
  traverseproc      = function ( ob1: PPyObject; proc: visitproc; ptr: Pointer): integer; cdecl;

  richcmpfunc       = function ( ob1, ob2 : PPyObject; i : Integer) : PPyObject; cdecl;
  getiterfunc       = function ( ob1 : PPyObject) : PPyObject; cdecl;
  iternextfunc      = function ( ob1 : PPyObject) : PPyObject; cdecl;
  descrgetfunc      = function ( ob1, ob2, ob3 : PPyObject) : PPyObject; cdecl;
  descrsetfunc      = function ( ob1, ob2, ob3 : PPyObject) : Integer; cdecl;
  initproc          = function ( self, args, kwds : PPyObject) : Integer; cdecl;
  newfunc           = function ( subtype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl;
  allocfunc         = function ( self: PPyTypeObject; nitems : NativeInt) : PPyObject; cdecl;

  PyNumberMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     nb_add           : binaryfunc;
     nb_substract     : binaryfunc;
     nb_multiply      : binaryfunc;
     nb_divide        : binaryfunc;
     nb_remainder     : binaryfunc;
     nb_divmod        : binaryfunc;
     nb_power         : ternaryfunc;
     nb_negative      : unaryfunc;
     nb_positive      : unaryfunc;
     nb_absolute      : unaryfunc;
     nb_nonzero       : inquiry;
     nb_invert        : unaryfunc;
     nb_lshift        : binaryfunc;
     nb_rshift        : binaryfunc;
     nb_and           : binaryfunc;
     nb_xor           : binaryfunc;
     nb_or            : binaryfunc;
     nb_coerce        : coercion;
     nb_int           : unaryfunc;
     nb_long          : unaryfunc;
     nb_float         : unaryfunc;
     nb_oct           : unaryfunc;
     nb_hex           : unaryfunc;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
     nb_inplace_add       : binaryfunc;
     nb_inplace_subtract  : binaryfunc;
     nb_inplace_multiply  : binaryfunc;
     nb_inplace_divide    : binaryfunc;
     nb_inplace_remainder : binaryfunc;
     nb_inplace_power     : ternaryfunc;
     nb_inplace_lshift    : binaryfunc;
     nb_inplace_rshift    : binaryfunc;
     nb_inplace_and       : binaryfunc;
     nb_inplace_xor       : binaryfunc;
     nb_inplace_or        : binaryfunc;

     // Added in release 2.2
     // The following require the Py_TPFLAGS_HAVE_CLASS flag
     nb_floor_divide         : binaryfunc;
     nb_true_divide          : binaryfunc;
     nb_inplace_floor_divide : binaryfunc;
     nb_inplace_true_divide  : binaryfunc;
  end;
  PPyNumberMethods = ^PyNumberMethods;

  PySequenceMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     sq_length    : lenfunc;
     sq_concat    : binaryfunc;
     sq_repeat    : ssizeargfunc;
     sq_item      : ssizeargfunc;
     sq_slice     : ssizessizeargfunc;
     sq_ass_item  : ssizeobjargproc;
     sq_ass_slice : ssizessizeobjargproc;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
     sq_contains        : objobjproc;
     sq_inplace_concat  : binaryfunc;
     sq_inplace_repeat  : ssizeargfunc;
  end;
  PPySequenceMethods = ^PySequenceMethods;

  PyMappingMethods = {$IFNDEF CPUX64}packed{$ENDIF} record
     mp_length        : lenfunc;
     mp_subscript     : binaryfunc;
     mp_ass_subscript : objobjargproc;
  end;
  PPyMappingMethods = ^PyMappingMethods;

/// jah 29-sep-2000 : updated for python 2.0
///                   added from .h
  PyBufferProcs = {$IFNDEF CPUX64}packed{$ENDIF} record
     bf_getreadbuffer   : getreadbufferproc;
     bf_getwritebuffer  : getwritebufferproc;
     bf_getsegcount     : getsegcountproc;
     bf_getcharbuffer   : getcharbufferproc;
  end;
  PPyBufferProcs = ^PyBufferProcs;

  Py_complex =  {$IFNDEF CPUX64}packed{$ENDIF} record
     real : double;
     imag : double;
  end;

  PyObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt: NativeInt;
    ob_type:   PPyTypeObject;
  end;

  PyIntObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    ob_ival   : LongInt;
  end;

  _frozen = {$IFNDEF CPUX64}packed{$ENDIF} record
     name : PAnsiChar;
     code : PByte;
     size : Integer;
  end;

  PySliceObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt:          NativeInt;
    ob_type:            PPyTypeObject;
    start, stop, step:  PPyObject;
  end;

  PPyMethodDef = ^PyMethodDef;
  PyMethodDef  = {$IFNDEF CPUX64}packed{$ENDIF} record
     ml_name:  PAnsiChar;
     ml_meth:  PyCFunction;
     ml_flags: Integer;
     ml_doc:   PAnsiChar;
  end;

  // structmember.h
  PPyMemberDef = ^PyMemberDef;
  PyMemberDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    _type : integer;
    offset : NativeInt;
    flags : integer;
    doc : PAnsiChar;
  end;

  // descrobject.h

  // Descriptors

  getter = function ( obj : PPyObject; context : Pointer) : PPyObject; cdecl;
  setter = function ( obj, value : PPyObject; context : Pointer) : integer; cdecl;

  PPyGetSetDef = ^PyGetSetDef;
  PyGetSetDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    get : getter;
    _set : setter;
    doc : PAnsiChar;
    closure : Pointer;
  end;

  wrapperfunc = function (self, args: PPyObject; wrapped : Pointer) : PPyObject; cdecl;

  pwrapperbase = ^wrapperbase;
  wrapperbase = {$IFNDEF CPUX64}packed{$ENDIF} record
    name : PAnsiChar;
    wrapper : wrapperfunc;
    doc : PAnsiChar;
  end;

  // Various kinds of descriptor objects

  {#define PyDescr_COMMON \
          PyObject_HEAD \
          PyTypeObject *d_type; \
          PyObject *d_name
  }

  PPyDescrObject = ^PyDescrObject;
  PyDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
  end;

  PPyMethodDescrObject = ^PyMethodDescrObject;
  PyMethodDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_method : PPyMethodDef;
  end;

  PPyMemberDescrObject = ^PyMemberDescrObject;
  PyMemberDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_member : PPyMemberDef;
  end;

  PPyGetSetDescrObject = ^PyGetSetDescrObject;
  PyGetSetDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_getset : PPyGetSetDef;
  end;

  PPyWrapperDescrObject = ^PyWrapperDescrObject;
  PyWrapperDescrObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of PyDescr_COMMON
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    d_type     : PPyTypeObject;
    d_name     : PPyObject;
    // End of PyDescr_COMMON
    d_base : pwrapperbase;
    d_wrapped : Pointer; // This can be any function pointer
  end;

  PPyModuleDef_Base = ^PyModuleDef_Base;
  PyModuleDef_Base = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    m_init     : function( ) : PPyObject; cdecl;
    m_index     : NativeInt;
    m_copy : PPyObject;
  end;

  PPyModuleDef = ^PyModuleDef;
  PyModuleDef = {$IFNDEF CPUX64}packed{$ENDIF} record
    m_base : PyModuleDef_Base;
    m_name : PAnsiChar;
    m_doc : PAnsiChar;
    m_size : NativeInt;
    m_methods : PPyMethodDef;
    m_reload : inquiry;
    m_traverse : traverseproc;
    m_clear : inquiry;
    m_free : inquiry;
  end;


  // object.h
  PyTypeObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt:      NativeInt;
    ob_type:        PPyTypeObject;
    ob_size:        NativeInt; // Number of items in variable part
    tp_name:        PAnsiChar;   // For printing
    tp_basicsize, tp_itemsize: NativeInt; // For allocation

    // Methods to implement standard operations

    tp_dealloc:     pydestructor;
    tp_print:       printfunc;
    tp_getattr:     getattrfunc;
    tp_setattr:     setattrfunc;
    tp_compare:     cmpfunc;
    tp_repr:        reprfunc;

    // Method suites for standard classes

    tp_as_number:   PPyNumberMethods;
    tp_as_sequence: PPySequenceMethods;
    tp_as_mapping:  PPyMappingMethods;

    // More standard operations (here for binary compatibility)

    tp_hash:        hashfunc;
    tp_call:        ternaryfunc;
    tp_str:         reprfunc;
    tp_getattro:    getattrofunc;
    tp_setattro:    setattrofunc;

/// jah 29-sep-2000 : updated for python 2.0

    // Functions to access object as input/output buffer
    tp_as_buffer:   PPyBufferProcs;
    // Flags to define presence of optional/expanded features
    tp_flags:       LongInt;

    tp_doc:         PAnsiChar; // Documentation string

    // call function for all accessible objects
    tp_traverse:    traverseproc;

    // delete references to contained objects
    tp_clear:       inquiry;

    // rich comparisons
    tp_richcompare: richcmpfunc;

    // weak reference enabler
    tp_weaklistoffset: NativeInt;
    // Iterators
    tp_iter : getiterfunc;
    tp_iternext : iternextfunc;

    // Attribute descriptor and subclassing stuff
    tp_methods          : PPyMethodDef;
    tp_members          : PPyMemberDef;
    tp_getset           : PPyGetSetDef;
    tp_base             : PPyTypeObject;
    tp_dict             : PPyObject;
    tp_descr_get        : descrgetfunc;
    tp_descr_set        : descrsetfunc;
    tp_dictoffset       : NativeInt;
    tp_init             : initproc;
    tp_alloc            : allocfunc;
    tp_new              : newfunc;
    tp_free             : pydestructor; // Low-level free-memory routine
    tp_is_gc            : inquiry; // For PyObject_IS_GC
    tp_bases            : PPyObject;
    tp_mro              : PPyObject; // method resolution order
    tp_cache            : PPyObject;
    tp_subclasses       : PPyObject;
    tp_weaklist         : PPyObject;
    tp_del              : PyDestructor;
    tp_version_tag      : NativeUInt;  // Type attribute cache version tag. Added in version 2.6
    tp_finalize         : PyDestructor;
    //More spares
    tp_xxx1             : NativeInt;
    tp_xxx2             : NativeInt;
    tp_xxx3             : NativeInt;
    tp_xxx4             : NativeInt;
    tp_xxx5             : NativeInt;
    tp_xxx6             : NativeInt;
    tp_xxx7             : NativeInt;
    tp_xxx8             : NativeInt;
    tp_xxx9             : NativeInt;
    tp_xxx10            : NativeInt;
  end;

  PPyMethodChain = ^PyMethodChain;
  PyMethodChain = {$IFNDEF CPUX64}packed{$ENDIF} record
    methods: PPyMethodDef;
    link:    PPyMethodChain;
  end;

  PPyClassObject = ^PyClassObject;
  PyClassObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    cl_bases   : PPyObject;       // A tuple of class objects
    cl_dict    : PPyObject;       // A dictionary
    cl_name    : PPyObject;       // A string
    // The following three are functions or NULL
    cl_getattr : PPyObject;
    cl_setattr : PPyObject;
    cl_delattr : PPyObject;
  end;

  PPyInstanceObject = ^PyInstanceObject;
  PyInstanceObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    in_class  : PPyClassObject;      // The class object
    in_dict   : PPyObject;           // A dictionary
  end;

{ Instance method objects are used for two purposes:
   (a) as bound instance methods (returned by instancename.methodname)
   (b) as unbound methods (returned by ClassName.methodname)
   In case (b), im_self is NULL
}

  PPyMethodObject = ^PyMethodObject;
  PyMethodObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    im_func  : PPyObject;      // The function implementing the method
    im_self  : PPyObject;      // The instance it is bound to, or NULL
    im_class : PPyObject;      // The class that defined the method
  end;


  // Bytecode object, compile.h
  PPyCodeObject = ^PyCodeObject;
  PyCodeObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    ob_refcnt      : NativeInt;
    ob_type        : PPyTypeObject;
    co_argcount    : Integer;         // #arguments, except *args
    co_nlocals     : Integer;         // #local variables
    co_stacksize   : Integer;          // #entries needed for evaluation stack
    co_flags       : Integer;         // CO_..., see below
    co_code        : PPyObject;       // instruction opcodes (it hides a PyStringObject)
    co_consts      : PPyObject;       // list (constants used)
    co_names       : PPyObject;       // list of strings (names used)
    co_varnames    : PPyObject;       // tuple of strings (local variable names)
    co_freevars    : PPyObject;       // tuple of strings (free variable names)
    co_cellvars    : PPyObject;       // tuple of strings (cell variable names)
    // The rest doesn't count for hash/cmp
    co_filename    : PPyObject;       // string (where it was loaded from)
    co_name        : PPyObject;       // string (name, for reference)
    co_firstlineno : Integer;         // first source line number
    co_lnotab      : PPyObject;       // string (encoding addr<->lineno mapping)
  end;


  // from pystate.h
  PPyInterpreterState = ^PyInterpreterState;
  PPyThreadState = ^PyThreadState;
  PPyFrameObject = ^PyFrameObject;

  // Interpreter environments
  PyInterpreterState = {$IFNDEF CPUX64}packed{$ENDIF} record
    next           : PPyInterpreterState;
    tstate_head    : PPyThreadState;

//  The strucure has changed between versions beyond this point.
//  Not safe to use members
//    modules        : PPyObject;
//    sysdict        : PPyObject;
//    builtins       : PPyObject;

    //Spares
    is_xxx1             : NativeInt;
    is_xxx2             : NativeInt;
    is_xxx3             : NativeInt;
    is_xxx4             : NativeInt;
    is_xxx5             : NativeInt;
    is_xxx6             : NativeInt;
    is_xxx7             : NativeInt;
    is_xxx8             : NativeInt;
    is_xxx9             : NativeInt;
  end;

  // Thread specific information
  PyThreadState = {$IFNDEF CPUX64}packed{$ENDIF} record
    {prev          : PPyThreadState; introduced in python 3.4}
    next           : PPyThreadState;
    interp         : PPyInterpreterState;
    interp34       : PPyInterpreterState;

//  The strucure has changed between versions beyond this point.
//  Not safe to use members
//
//    frame          : PPyFrameObject;
//    recursion_depth: integer;
//    ticker         : integer;
//    tracing        : integer;
//
//    sys_profilefn  : Pointer;           // c-functions for profile/trace
//    sys_tracefn    : Pointer;
//    sys_profilefunc: PPyObject;
//    sys_tracefunc  : PPyObject;
//
//    curexc_type    : PPyObject;
//    curexc_value   : PPyObject;
//    curexc_traceback: PPyObject;
//
//    exc_type       : PPyObject;
//    exc_value      : PPyObject;
//    exc_traceback  : PPyObject;
//
//    dict           : PPyObject;
//    tick_counter      :Integer;
//    gilstate_counter  :Integer;
//
//    async_exc         :PPyObject; { Asynchronous exception to raise }
//    thread_id         :LongInt;   { Thread id where this tstate was created }
    //Spares
    ts_xxx1             : NativeInt;
    ts_xxx2             : NativeInt;
    ts_xxx3             : NativeInt;
    ts_xxx4             : NativeInt;
    ts_xxx5             : NativeInt;
    ts_xxx6             : NativeInt;
    ts_xxx7             : NativeInt;
    ts_xxx8             : NativeInt;
    ts_xxx9             : NativeInt;
    ts_xxx10            : NativeInt;
    ts_xxx11            : NativeInt;
    ts_xxx12            : NativeInt;
    ts_xxx13            : NativeInt;
    ts_xxx14            : NativeInt;
    ts_xxx15            : NativeInt;
    ts_xxx16            : NativeInt;
    ts_xxx17            : NativeInt;
    ts_xxx18            : NativeInt;
    ts_xxx19            : NativeInt;

    { XXX signal handlers should also be here }
  end;

  // from frameobject.h

  PPyTryBlock = ^PyTryBlock;
  PyTryBlock = {$IFNDEF CPUX64}packed{$ENDIF} record
    b_type    : Integer;       // what kind of block this is
    b_handler : Integer;       // where to jump to find handler
    b_level   : Integer;       // value stack level to pop to
  end;

  CO_MAXBLOCKS  = 0..19;
  PyFrameObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the VAR_HEAD of an object.
    ob_refcnt    : NativeInt;
    ob_type      : PPyTypeObject;
    ob_size      : NativeInt;           // Number of items in variable part
    // End of the Head of an object
    f_back       : PPyFrameObject;    // previous frame, or NULL
    f_code       : PPyCodeObject;     // code segment
    f_builtins   : PPyObject;         // builtin symbol table (PyDictObject)
    f_globals    : PPyObject;         // global symbol table (PyDictObject)
    f_locals     : PPyObject;         // local symbol table (PyDictObject)
    f_valuestack : PPPyObject;        // points after the last local
    (* Next free slot in f_valuestack.  Frame creation sets to f_valuestack.
       Frame evaluation usually NULLs it, but a frame that yields sets it
       to the current stack top. *)
    f_stacktop   : PPPyObject;
    f_trace      : PPyObject;         // Trace function
    f_exc_type, f_exc_value, f_exc_traceback: PPyObject;
    f_tstate     : PPyThreadState;
    f_lasti      : Integer;           // Last instruction if called
    f_lineno     : Integer;           // Current line number
    f_iblock     : Integer;           // index in f_blockstack
    f_blockstack : array[CO_MAXBLOCKS] of PyTryBlock; // for try and loop blocks
    f_localsplus : array[0..0] of PPyObject; // locals+stack, dynamically sized
  end;

  // From traceback.c
  PPyTraceBackObject = ^PyTraceBackObject;
  PyTraceBackObject = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt : NativeInt;
    ob_type   : PPyTypeObject;
    // End of the Head of an object
    tb_next   : PPyTraceBackObject;
    tb_frame  : PPyFrameObject;
    tb_lasti  : Integer;
    tb_lineno : Integer;
  end;

  // Parse tree node interface

  PNode = ^node;
  node = {$IFNDEF CPUX64}packed{$ENDIF} record
    n_type      : smallint;
    n_str       : PAnsiChar;
    n_lineno    : integer;
    n_col_offset: integer;
    n_nchildren : integer;
    n_child     : PNode;
  end;

  PPyCompilerFlags = ^PyCompilerFlags;
  PyCompilerFlags = {$IFNDEF CPUX64}packed{$ENDIF} record
    flags : integer;
  end;

  // From weakrefobject.h

  PPyWeakReference = ^PyWeakReference;
  PyWeakReference = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    wr_object   : PPyObject;
    wr_callback : PPyObject;
    hash        : NativeInt;
    wr_prev     : PPyWeakReference;
    wr_next     : PPyWeakReference;
  end;

  // from datetime.h


{* Fields are packed into successive bytes, each viewed as unsigned and
 * big-endian, unless otherwise noted:
 *
 * byte offset
 *  0     year     2 bytes, 1-9999
 *  2     month    1 byte,  1-12
 *  3     day      1 byte,  1-31
 *  4     hour     1 byte,  0-23
 *  5     minute   1 byte,  0-59
 *  6     second   1 byte,  0-59
 *  7     usecond  3 bytes, 0-999999
 * 10
 *}

const
  { # of bytes for year, month, and day. }
  _PyDateTime_DATE_DATASIZE = 4;

  { # of bytes for hour, minute, second, and usecond. }
  _PyDateTime_TIME_DATASIZE = 6;

  { # of bytes for year, month, day, hour, minute, second, and usecond. }
  _PyDateTime_DATETIME_DATASIZE = 10;
type
  PyDateTime_Delta = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode    : Integer;  // -1 when unknown
    days        : Integer;  // -MAX_DELTA_DAYS <= days <= MAX_DELTA_DAYS
    seconds     : Integer;  // 0 <= seconds < 24*3600 is invariant
    microseconds: Integer;  // 0 <= microseconds < 1000000 is invariant
  end;
  PPyDateTime_Delta = ^PyDateTime_Delta;

  PyDateTime_TZInfo = {$IFNDEF CPUX64}packed{$ENDIF} record // a pure abstract base clase
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
  end;
  PPyDateTime_TZInfo = ^PyDateTime_TZInfo;

{
/* The datetime and time types have hashcodes, and an optional tzinfo member,
 * present if and only if hastzinfo is true.
 */
#define _PyTZINFO_HEAD    \
  PyObject_HEAD   \
  long hashcode;    \
  char hastzinfo;   /* boolean flag */
}

{* No _PyDateTime_BaseTZInfo is allocated; it's just to have something
 * convenient to cast to, when getting at the hastzinfo member of objects
 * starting with _PyTZINFO_HEAD.
 *}
  _PyDateTime_BaseTZInfo = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
  end;
  _PPyDateTime_BaseTZInfo = ^_PyDateTime_BaseTZInfo;

{* All time objects are of PyDateTime_TimeType, but that can be allocated
 * in two ways, with or without a tzinfo member.  Without is the same as
 * tzinfo == None, but consumes less memory.  _PyDateTime_BaseTime is an
 * internal struct used to allocate the right amount of space for the
 * "without" case.
 *}
{#define _PyDateTime_TIMEHEAD \
  _PyTZINFO_HEAD    \
  unsigned char data[_PyDateTime_TIME_DATASIZE];
}

  _PyDateTime_BaseTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo false
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
  end;
  _PPyDateTime_BaseTime = ^_PyDateTime_BaseTime;

  PyDateTime_Time = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo true
    // Start of _PyDateTime_TIMEHEAD
      // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_TIME_DATASIZE)] of Byte;
    // End of _PyDateTime_TIMEHEAD
    tzinfo     : PPyObject;
  end;
  PPyDateTime_Time = ^PyDateTime_Time;



{* All datetime objects are of PyDateTime_DateTimeType, but that can be
 * allocated in two ways too, just like for time objects above.  In addition,
 * the plain date type is a base class for datetime, so it must also have
 * a hastzinfo member (although it's unused there).
 *}
  PyDateTime_Date = {$IFNDEF CPUX64}packed{$ENDIF} record
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATE_DATASIZE)] of Byte;
  end;
  PPyDateTime_Date = ^PyDateTime_Date;

 {
#define _PyDateTime_DATETIMEHEAD  \
  _PyTZINFO_HEAD      \
  unsigned char data[_PyDateTime_DATETIME_DATASIZE];
}

  _PyDateTime_BaseDateTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo false
    // Start of _PyTZINFO_HEAD
    // Start of the Head of an object
    ob_refcnt  : NativeInt;
    ob_type    : PPyTypeObject;
    // End of the Head of an object
    hashcode   : Integer;
    hastzinfo  : Char;  // boolean flag
    // End of _PyTZINFO_HEAD
    data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
  end;
  _PPyDateTime_BaseDateTime = ^_PyDateTime_BaseDateTime;

  PyDateTime_DateTime = {$IFNDEF CPUX64}packed{$ENDIF} record // hastzinfo true
    // Start of _PyDateTime_DATETIMEHEAD
      // Start of _PyTZINFO_HEAD
        // Start of the Head of an object
        ob_refcnt  : NativeInt;
        ob_type    : PPyTypeObject;
        // End of the Head of an object
      hashcode   : Integer;
      hastzinfo  : Char;  // boolean flag
      // End of _PyTZINFO_HEAD
      data       : array[0..Pred(_PyDateTime_DATETIME_DATASIZE)] of Byte;
    // End of _PyDateTime_DATETIMEHEAD
    tzinfo : PPyObject;
  end;
  PPyDateTime_DateTime = ^PyDateTime_DateTime;

//#######################################################
//##                                                   ##
//##         GIL state                                 ##
//##                                                   ##
//#######################################################
  PyGILState_STATE = (PyGILState_LOCKED, PyGILState_UNLOCKED);

var
  Py_DebugFlag:cint; cvar; external;
  Py_VerboseFlag:cint; cvar; external;
  Py_InteractiveFlag:cint; cvar; external;
  Py_OptimizeFlag:cint; cvar; external;
  Py_NoSiteFlag:cint; cvar; external;
  Py_FrozenFlag:cint; cvar; external;
  Py_IgnoreEnvironmentFlag:cint; cvar; external;
  PyImport_FrozenModules:PP_frozen; cvar; external;
  Py_None:PPyObject;
  Py_Ellipsis:PPyObject; cvar; external;
  _Py_TrueStruct:PPyIntObject; cvar; external;
  _Py_NoneStruct:PPyIntObject; cvar; external;
  Py_False:PPyIntObject;
  Py_True:PPyIntObject;
  _Py_FalseStruct:PPyIntObject; cvar; external;
  Py_NotImplemented:PPyObject; cvar; external;
  PyExc_AttributeError:PPPyObject; cvar; external;
  PyExc_EOFError:PPPyObject; cvar; external;
  PyExc_IOError:PPPyObject; cvar; external;
  PyExc_ImportError:PPPyObject; cvar; external;
  PyExc_IndexError:PPPyObject; cvar; external;
  PyExc_KeyError:PPPyObject; cvar; external;
  PyExc_KeyboardInterrupt:PPPyObject; cvar; external;
  PyExc_MemoryError:PPPyObject; cvar; external;
  PyExc_NameError:PPPyObject; cvar; external;
  PyExc_OverflowError:PPPyObject; cvar; external;
  PyExc_RuntimeError:PPPyObject; cvar; external;
  PyExc_SyntaxError:PPPyObject; cvar; external;
  PyExc_SystemError:PPPyObject; cvar; external;
  PyExc_SystemExit:PPPyObject; cvar; external;
  PyExc_TypeError:PPPyObject; cvar; external;
  PyExc_ValueError:PPPyObject; cvar; external;
  PyExc_ZeroDivisionError:PPPyObject; cvar; external;
  PyExc_ArithmeticError:PPPyObject; cvar; external;
  PyExc_Exception:PPPyObject; cvar; external;
  PyExc_FloatingPointError:PPPyObject; cvar; external;
  PyExc_LookupError:PPPyObject; cvar; external;
  PyExc_AssertionError:PPPyObject; cvar; external;
  PyExc_EnvironmentError:PPPyObject; cvar; external;
  PyExc_IndentationError:PPPyObject; cvar; external;
  PyExc_MemoryErrorInst:PPPyObject; cvar; external;
  PyExc_NotImplementedError:PPPyObject; cvar; external;
  PyExc_OSError:PPPyObject; cvar; external;
  PyExc_TabError:PPPyObject; cvar; external;
  PyExc_UnboundLocalError:PPPyObject; cvar; external;
  PyExc_UnicodeError:PPPyObject; cvar; external;
  PyExc_WindowsError:PPPyObject; cvar; external;
  PyExc_Warning:PPPyObject; cvar; external;
  PyExc_DeprecationWarning:PPPyObject; cvar; external;
  PyExc_RuntimeWarning:PPPyObject; cvar; external;
  PyExc_SyntaxWarning:PPPyObject; cvar; external;
  PyExc_UserWarning:PPPyObject; cvar; external;
  PyExc_ReferenceError:PPPyObject; cvar; external;
  PyExc_StopIteration:PPPyObject; cvar; external;
  PyExc_FutureWarning:PPPyObject; cvar; external;
  PyExc_PendingDeprecationWarning:PPPyObject; cvar; external;
  PyExc_UnicodeDecodeError:PPPyObject; cvar; external;
  PyExc_UnicodeEncodeError:PPPyObject; cvar; external;
  PyExc_UnicodeTranslateError:PPPyObject; cvar; external;
  PyType_Type:PPyTypeObject; cvar; external;
  PyCFunction_Type:PPyTypeObject; cvar; external;
  PyCObject_Type:PPyTypeObject; cvar; external;
  PyCode_Type:PPyTypeObject; cvar; external;
  PyComplex_Type:PPyTypeObject; cvar; external;
  PyDict_Type:PPyTypeObject; cvar; external;
  PyFile_Type:PPyTypeObject; cvar; external;
  PyFloat_Type:PPyTypeObject; cvar; external;
  PyFrame_Type:PPyTypeObject; cvar; external;
  PyFunction_Type:PPyTypeObject; cvar; external;
  PyList_Type:PPyTypeObject; cvar; external;
  PyLong_Type:PPyTypeObject; cvar; external;
  PyMethod_Type:PPyTypeObject; cvar; external;
  PyModule_Type:PPyTypeObject; cvar; external;
  PyObject_Type:PPyTypeObject; cvar; external;
  PyRange_Type:PPyTypeObject; cvar; external;
  PySlice_Type:PPyTypeObject; cvar; external;
  PyTuple_Type:PPyTypeObject; cvar; external;
  PyBaseObject_Type:PPyTypeObject; cvar; external;
  PyBuffer_Type:PPyTypeObject; cvar; external;
  PyCallIter_Type:PPyTypeObject; cvar; external;
  PyCell_Type:PPyTypeObject; cvar; external;
  PyClassMethod_Type:PPyTypeObject; cvar; external;
  PyProperty_Type:PPyTypeObject; cvar; external;
  PySeqIter_Type:PPyTypeObject; cvar; external;
  PyStaticMethod_Type:PPyTypeObject; cvar; external;
  PySuper_Type:PPyTypeObject; cvar; external;
  PyTraceBack_Type:PPyTypeObject; cvar; external;
  PyUnicode_Type:PPyTypeObject; cvar; external;
  PyWrapperDescr_Type:PPyTypeObject; cvar; external;
  _PyWeakref_RefType:PPyTypeObject; cvar; external;
  _PyWeakref_ProxyType:PPyTypeObject; cvar; external;
  _PyWeakref_CallableProxyType:PPyTypeObject; cvar; external;
  PyBool_Type:PPyTypeObject; cvar; external;
  PyEnum_Type:PPyTypeObject; cvar; external;
  PyBytes_Type:PPyTypeObject; cvar; external;

  function PyComplex_FromCComplex (c: Py_complex):PPyObject; cdecl; external;
  function PyComplex_FromDoubles (realv,imag : double):PPyObject; cdecl; external;
  function PyComplex_RealAsDouble (op : PPyObject ): double; cdecl; external;
  function PyComplex_ImagAsDouble (op : PPyObject ): double; cdecl; external;
  function PyComplex_AsCComplex (op : PPyObject ): Py_complex; cdecl; external;
  function PyCFunction_GetFunction (ob : PPyObject): Pointer; cdecl; external;
  function PyCFunction_GetSelf (ob : PPyObject): PPyObject; cdecl; external;
  function PyCallable_Check (ob : PPyObject): integer; cdecl; external;
  function PyClass_New (ob1,ob2,ob3 :  PPyObject): PPyObject; cdecl; external;
  //function PyClass_New (ob1,ob2,ob3 :  PPyObject): PPyObject; cdecl; external name 'PyObject_New';
  //function PyClass_IsSubclass (ob1, ob2 : PPyObject): integer; cdecl; external name 'PyObject_IsSubclass';
  function PyModule_Create (moduledef: PPyModuleDef):PPyObject; cdecl; external name '_PyModule_CreateInitialized';
  function PyModule_Create2 (moduledef: PPyModuleDef; Api_Version: Integer):PPyObject; cdecl; external;
  function PyErr_BadArgument : integer; cdecl; external;
  procedure PyErr_BadInternalCall ; cdecl; external;
  function PyErr_CheckSignals : integer; cdecl; external;
  procedure PyErr_Clear ; cdecl; external;
  procedure PyErr_Fetch ( errtype, errvalue, errtraceback: PPPyObject); cdecl; external;
  function PyErr_NoMemory : PPyObject; cdecl; external;
  function PyErr_Occurred : PPyObject; cdecl; external;
  procedure PyErr_Print ; cdecl; external;
  procedure PyErr_Restore (errtype, errvalue, errtraceback: PPyObject); cdecl; external;
  function PyErr_SetFromErrno (ob :  PPyObject):PPyObject; cdecl; external;
  procedure PyErr_SetNone (value: PPyObject); cdecl; external;
  procedure PyErr_SetObject (ob1, ob2  : PPyObject); cdecl; external;
  procedure PyErr_SetString ( ErrorObject: PPyObject; text: PAnsiChar); cdecl; external;
  function PyImport_GetModuleDict : PPyObject; cdecl; external;
  function PyArg_Parse ( args: PPyObject; format: PAnsiChar) :  Integer; cdecl varargs; external;
  function PyArg_ParseTuple ( args: PPyObject; format: PAnsiChar): Integer; cdecl varargs; external;
  function Py_BuildValue ( format: PAnsiChar): PPyObject; cdecl varargs; external;
  procedure Py_Initialize ; cdecl; external;
  procedure Py_Exit ( RetVal: Integer); cdecl; external;
  function PyEval_GetBuiltins : PPyObject; cdecl; external;
  function PyDict_Copy (mp: PPyObject):PPyObject; cdecl; external;
  function PyDict_GetItem (mp, key : PPyObject):PPyObject; cdecl; external;
  function PyDict_SetItem (mp, key, item :PPyObject ):integer; cdecl; external;
  function PyDict_DelItem (mp, key : PPyObject ):integer; cdecl; external;
  procedure PyDict_Clear (mp : PPyObject); cdecl; external;
  function PyDict_Next (mp : PPyObject; pos: PNativeInt; key, value: PPPyObject):integer; cdecl; external;
  function PyDict_Keys (mp: PPyObject):PPyObject; cdecl; external;
  function PyDict_Values (mp: PPyObject):PPyObject; cdecl; external;
  function PyDict_Items (mp: PPyObject):PPyObject; cdecl; external;
  function PyDict_Size (mp: PPyObject):NativeInt; cdecl; external;
  function PyDict_DelItemString (dp : PPyObject;key : PAnsiChar ):integer; cdecl; external;
  function PyDict_New : PPyObject; cdecl; external;
  function PyDict_GetItemString ( dp: PPyObject; key: PAnsiChar): PPyObject; cdecl; external;
  function PyDict_SetItemString ( dp: PPyObject; key: PAnsiChar; item: PPyObject): Integer; cdecl; external;
  function PyDictProxy_New (obj : PPyObject) : PPyObject; cdecl; external;
  function PyModule_GetDict ( module:PPyObject): PPyObject; cdecl; external;
  function PyObject_Str ( v: PPyObject): PPyObject; cdecl; external;
  function PyRun_String ( str: PAnsiChar; start: Integer; globals: PPyObject; locals: PPyObject): PPyObject; cdecl; external;
  function PyRun_SimpleString ( str: PAnsiChar): Integer; cdecl; external;
  function PyBytes_AsString ( ob: PPyObject): PAnsiChar; cdecl; external;
  function PyBytes_AsStringAndSize ( ob: PPyObject; var buffer: PAnsiChar; var size: NativeInt): integer; cdecl; external;
  procedure PySys_SetArgv ( argc: Integer; argv: PPWideChar); cdecl; external;
  function PyCFunction_New (md:PPyMethodDef;ob:PPyObject):PPyObject; cdecl; external;
  function PyCFunction_NewEx (md:PPyMethodDef;self, ob:PPyObject):PPyObject; cdecl; external;
  function PyEval_CallObjectWithKeywords (callable_obj, args, kw:PPyObject):PPyObject; cdecl; external;
  function PyEval_GetFrame :PPyObject; cdecl; external;
  function PyEval_GetGlobals :PPyObject; cdecl; external;
  function PyEval_GetLocals :PPyObject; cdecl; external;
  function PyEval_GetRestricted :integer; cdecl; external;
  procedure PyEval_InitThreads ; cdecl; external;
  procedure PyEval_RestoreThread ( tstate: PPyThreadState); cdecl; external;
  function PyEval_SaveThread :PPyThreadState; cdecl; external;
  function PyFile_FromString (pc1,pc2:PAnsiChar):PPyObject; cdecl; external;
  function PyFile_GetLine (ob:PPyObject;i:integer):PPyObject; cdecl; external;
  function PyFile_Name (ob:PPyObject):PPyObject; cdecl; external;
  procedure PyFile_SetBufSize (ob:PPyObject;i:integer); cdecl; external;
  function PyFile_SoftSpace (ob:PPyObject;i:integer):integer; cdecl; external;
  function PyFile_WriteObject (ob1,ob2:PPyObject;i:integer):integer; cdecl; external;
  procedure PyFile_WriteString (s:PAnsiChar;ob:PPyObject); cdecl; external;
  function PyFloat_AsDouble (ob:PPyObject):DOUBLE; cdecl; external;
  function PyFloat_FromDouble (db:double):PPyObject; cdecl; external;
  function PyFunction_GetCode (ob:PPyObject):PPyObject; cdecl; external;
  function PyFunction_GetGlobals (ob:PPyObject):PPyObject; cdecl; external;
  function PyFunction_New (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyImport_AddModule (name:PAnsiChar):PPyObject; cdecl; external;
  procedure PyImport_Cleanup ; cdecl; external;
  function PyImport_GetMagicNumber :LONGINT; cdecl; external;
  function PyImport_ImportFrozenModule (key:PAnsiChar):integer; cdecl; external;
  function PyImport_ImportModule (name:PAnsiChar):PPyObject; cdecl; external;
  function PyImport_Import (name:PPyObject):PPyObject; cdecl; external;
  function PyImport_ReloadModule (ob:PPyObject):PPyObject; cdecl; external;
  function PyInstance_New (obClass, obArg, obKW:PPyObject):PPyObject; cdecl; external;
  function PyList_Append (ob1,ob2:PPyObject):integer; cdecl; external;
  function PyList_AsTuple (ob:PPyObject):PPyObject; cdecl; external;
  function PyList_GetItem (ob:PPyObject;i:NativeInt):PPyObject; cdecl; external;
  function PyList_GetSlice (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl; external;
  function PyList_Insert (dp:PPyObject;idx:NativeInt;item:PPyObject):integer; cdecl; external;
  function PyList_New (size:NativeInt):PPyObject; cdecl; external;
  function PyList_Reverse (ob:PPyObject):integer; cdecl; external;
  function PyList_SetItem (dp:PPyObject;idx:NativeInt;item:PPyObject):integer; cdecl; external;
  function PyList_SetSlice (ob:PPyObject;i1,i2:NativeInt;ob2:PPyObject):integer; cdecl; external;
  function PyList_Size (ob:PPyObject):NativeInt; cdecl; external;
  function PyList_Sort (ob:PPyObject):integer; cdecl; external;
  function PyLong_AsDouble (ob:PPyObject):DOUBLE; cdecl; external;
  function PyLong_AsLong (ob:PPyObject):LONGINT; cdecl; external;
  function PyLong_FromDouble (db:double):PPyObject; cdecl; external;
  function PyLong_FromLong (l:longint):PPyObject; cdecl; external;
  function PyLong_FromString (pc:PAnsiChar;var ppc:PAnsiChar;i:integer):PPyObject; cdecl; external;
  function PyLong_FromUnsignedLong (val:cardinal) : PPyObject; cdecl; external;
  function PyLong_AsUnsignedLong (ob:PPyObject) : Cardinal; cdecl; external;
  function PyLong_FromUnicode (ob:PPyObject; a, b : integer) : PPyObject; cdecl; external;
  function PyLong_FromLongLong (val:Int64) : PPyObject; cdecl; external;
  function PyLong_AsLongLong (ob:PPyObject) : Int64; cdecl; external;
  function PyMapping_Check (ob:PPyObject):integer; cdecl; external;
  function PyMapping_GetItemString (ob:PPyObject;key:PAnsiChar):PPyObject; cdecl; external;
  function PyMapping_HasKey (ob,key:PPyObject):integer; cdecl; external;
  function PyMapping_HasKeyString (ob:PPyObject;key:PAnsiChar):integer; cdecl; external;
  function PyMapping_Length (ob:PPyObject):NativeInt; cdecl; external;
  function PyMapping_SetItemString (ob:PPyObject; key:PAnsiChar; value:PPyObject):integer; cdecl; external;
  function PyMethod_Class (ob:PPyObject):PPyObject; cdecl; external;
  function PyMethod_Function (ob:PPyObject):PPyObject; cdecl; external;
  function PyMethod_New (ob1,ob2,ob3:PPyObject):PPyObject; cdecl; external;
  function PyMethod_Self (ob:PPyObject):PPyObject; cdecl; external;
  function PyModule_GetName (ob:PPyObject):PAnsiChar; cdecl; external;
  function PyModule_New (key:PAnsiChar):PPyObject; cdecl; external;
  function PyNumber_Absolute (ob:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Add (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_And (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Check (ob:PPyObject):integer; cdecl; external;
  function PyNumber_Coerce (var ob1,ob2:PPyObject):integer; cdecl; external;
  function PyNumber_Divide (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_FloorDivide (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_TrueDivide (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Divmod (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Float (ob:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Int (ob:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Invert (ob:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Long (ob:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Lshift (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Multiply (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Negative (ob:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Or (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Positive (ob:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Power (ob1,ob2,ob3:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Remainder (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Rshift (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Subtract (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyNumber_Xor (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  procedure PyOS_InitInterrupts ; cdecl; external;
  function PyOS_InterruptOccurred :integer; cdecl; external;
  function PyObject_CallObject (ob,args:PPyObject):PPyObject; cdecl; external;
  function PyObject_CallMethod ( obj : PPyObject; method, format : PAnsiChar {...}) : PPyObject; cdecl varargs; external;
  function PyObject_Compare (ob1,ob2:PPyObject):integer; cdecl; external;
  function PyObject_RichCompare (ob1,ob2:PPyObject;opid:integer):PPyObject; cdecl; external;
  function PyObject_RichCompareBool (ob1,ob2:PPyObject;opid:integer):Integer; cdecl; external;
  function PyObject_GetAttr (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PyObject_GetAttrString (ob:PPyObject;c:PAnsiChar):PPyObject; cdecl; external;
  function PyObject_GetItem (ob,key:PPyObject):PPyObject; cdecl; external;
  function PyObject_DelItem (ob,key:PPyObject):PPyObject; cdecl; external;
  function PyObject_HasAttrString (ob:PPyObject;key:PAnsiChar):integer; cdecl; external;
  function PyObject_Hash (ob:PPyObject):NativeInt; cdecl; external;
  function PyObject_IsTrue (ob:PPyObject):integer; cdecl; external;
  function PyObject_Length (ob:PPyObject):NativeInt; cdecl; external;
  function PyObject_Repr (ob:PPyObject):PPyObject; cdecl; external;
  function PyObject_SetAttr (ob1,ob2,ob3:PPyObject):integer; cdecl; external;
  function PyObject_SetAttrString (ob:PPyObject;key:PAnsiChar;value:PPyObject):integer; cdecl; external;
  function PyObject_SetItem (ob1,ob2,ob3:PPyObject):integer; cdecl; external;
  function PyObject_Init (ob:PPyObject; t:PPyTypeObject):PPyObject; cdecl; external;
  function PyObject_InitVar (ob:PPyObject; t:PPyTypeObject; size:NativeInt):PPyObject; cdecl; external;
  function PyObject_New (t:PPyTypeObject):PPyObject; cdecl; external;
  function PyObject_NewVar (t:PPyTypeObject; size:NativeInt):PPyObject; cdecl; external;
  procedure PyObject_Free (ob:PPyObject); cdecl; external;
  function PyObject_GetIter (obj: PPyObject) : PPyObject; cdecl; external;
  function PyIter_Next (obj: PPyObject) : PPyObject; cdecl; external;
  function PyObject_IsInstance (inst, cls:PPyObject):integer; cdecl; external;
  function PyObject_IsSubclass (derived, cls:PPyObject):integer; cdecl; external;
  function PyObject_Call (ob, args, kw:PPyObject):PPyObject; cdecl; external;
  function PyObject_GenericGetAttr (obj, name : PPyObject) : PPyObject; cdecl; external;
  function PyObject_GenericSetAttr (obj, name, value : PPyObject) : Integer; cdecl; external;
  function PyObject_GC_Malloc (size:NativeUInt):PPyObject; cdecl; external;
  function PyObject_GC_New (t:PPyTypeObject):PPyObject; cdecl; external;
  function PyObject_GC_NewVar (t:PPyTypeObject; size:NativeInt):PPyObject; cdecl; external;
  function PyObject_GC_Resize (t:PPyObject; newsize:NativeInt):PPyObject; cdecl; external;
  procedure PyObject_GC_Del (ob:PPyObject); cdecl; external;
  procedure PyObject_GC_Track (ob:PPyObject); cdecl; external;
  procedure PyObject_GC_UnTrack (ob:PPyObject); cdecl; external;
  function PySequence_Check (ob:PPyObject):integer; cdecl; external;
  function PySequence_Concat (ob1,ob2:PPyObject):PPyObject; cdecl; external;
  function PySequence_Count (ob1,ob2:PPyObject):integer; cdecl; external;
  function PySequence_GetItem (ob:PPyObject;i:NativeInt):PPyObject; cdecl; external;
  function PySequence_GetSlice (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl; external;
  function PySequence_In (ob1,ob2:PPyObject):integer; cdecl; external;
  function PySequence_Index (ob1,ob2:PPyObject):NativeInt; cdecl; external;
  function PySequence_Length (ob:PPyObject):NativeInt; cdecl; external;
  function PySequence_Repeat (ob:PPyObject;count:NativeInt):PPyObject; cdecl; external;
  function PySequence_SetItem (ob:PPyObject;i:NativeInt;value:PPyObject):integer; cdecl; external;
  function PySequence_SetSlice (ob:PPyObject;i1,i2:NativeInt;value:PPyObject):integer; cdecl; external;
  function PySequence_DelSlice (ob:PPyObject;i1,i2:NativeInt):integer; cdecl; external;
  function PySequence_Tuple (ob:PPyObject):PPyObject; cdecl; external;
  function PySequence_Contains (ob, value:PPyObject):integer; cdecl; external;
  function PySeqIter_New (obj : PPyObject) : PPyObject; cdecl; external;
  function PySlice_GetIndices (ob:PPySliceObject;length:NativeInt;var start,stop,step:NativeInt):integer; cdecl; external;
  function PySlice_GetIndicesEx (ob:PPySliceObject;length:NativeInt;var start,stop,step,slicelength:NativeInt):integer; cdecl; external;
  function PySlice_New (start,stop,step:PPyObject):PPyObject; cdecl; external;
  procedure PyBytes_Concat (var ob1:PPyObject;ob2:PPyObject); cdecl; external;
  procedure PyBytes_ConcatAndDel (var ob1:PPyObject;ob2:PPyObject); cdecl; external;
  function PyBytes_FromStringAndSize (s:PAnsiChar;i:NativeInt):PPyObject; cdecl; external;
  function PyBytes_Size (ob:PPyObject):NativeInt; cdecl; external;
  function PyBytes_DecodeEscape (s:PAnsiChar; len:NativeInt; errors:PAnsiChar; unicode:NativeInt; recode_encoding:PAnsiChar):PPyObject; cdecl; external;
  function PyBytes_Repr (ob:PPyObject; smartquotes:integer):PPyObject; cdecl; external;
  function PySys_GetObject (s:PAnsiChar):PPyObject; cdecl; external;
  function PySys_SetObject (s:PAnsiChar;ob:PPyObject):integer; cdecl; external;
  procedure PySys_SetPath (path:PAnsiChar); cdecl; external;
  function PyTraceBack_Here (p:pointer):integer; cdecl; external;
  function PyTraceBack_Print (ob1,ob2:PPyObject):integer; cdecl; external;
  function PyTuple_GetItem (ob:PPyObject;i:NativeInt):PPyObject; cdecl; external;
  function PyTuple_GetSlice (ob:PPyObject;i1,i2:NativeInt):PPyObject; cdecl; external;
  function PyTuple_New (size:NativeInt):PPyObject; cdecl; external;
  function PyTuple_SetItem (ob:PPyObject;key:NativeInt;value:PPyObject):integer; cdecl; external;
  function PyTuple_Size (ob:PPyObject):NativeInt; cdecl; external;
  function PyType_IsSubtype (a, b : PPyTypeObject):integer; cdecl; external;
  function PyType_GenericAlloc (atype: PPyTypeObject; nitems:NativeInt) : PPyObject; cdecl; external;
  function PyType_GenericNew (atype: PPyTypeObject; args, kwds : PPyObject) : PPyObject; cdecl; external;
  function PyType_Ready (atype: PPyTypeObject) : integer; cdecl; external;
  function PyUnicode_FromWideChar (const w:PWideChar; size:NativeInt):PPyObject; cdecl; external;
  function PyUnicode_AsWideChar (unicode: PPyObject; w:PWideChar; size:NativeInt):integer; cdecl; external;
  function PyUnicode_Decode (const s:PAnsiChar; size: NativeInt; const encoding : PAnsiChar; const errors: PAnsiChar):PPyObject; cdecl; external;
  function PyUnicode_AsEncodedString (unicode:PPyObject; const encoding:PAnsiChar; const errors:PAnsiChar):PPyObject; cdecl; external;
  function PyUnicode_FromOrdinal (ordinal:integer):PPyObject; cdecl; external;
  function PyUnicode_GetSize (unicode:PPyObject):NativeInt; cdecl; external;
  function PyWeakref_GetObject ( ref : PPyObject) : PPyObject; cdecl; external;
  function PyWeakref_NewProxy ( ob, callback : PPyObject) : PPyObject; cdecl; external;
  function PyWeakref_NewRef ( ob, callback : PPyObject) : PPyObject; cdecl; external;
  function PyWrapper_New ( ob1, ob2 : PPyObject) : PPyObject; cdecl; external;
  function PyBool_FromLong ( ok : Integer) : PPyObject; cdecl; external;
  function PyThreadState_SetAsyncExc (t_id :LongInt; exc :PPyObject) : Integer; cdecl; external;
  function Py_AtExit (proc: AtExitProc):integer; cdecl; external;
  function Py_CompileStringFlags (s1,s2:PAnsiChar;i:integer;flags:PPyCompilerFlags):PPyObject; cdecl; external;
  function Py_CompileStringExFlags (s1,s2:PAnsiChar;i:integer;flags:PPyCompilerFlags;optimize:integer):PPyObject; cdecl; external;
  procedure Py_FatalError (s:PAnsiChar); cdecl; external;
  function Py_FindMethod (md:PPyMethodDef;ob:PPyObject;key:PAnsiChar):PPyObject; cdecl; external;
  function Py_FindMethodInChain (mc:PPyMethodChain;ob:PPyObject;key:PAnsiChar):PPyObject; cdecl; external;
  function _PyObject_New (obt:PPyTypeObject;ob:PPyObject):PPyObject; cdecl; external;
  function _PyString_Resize (var ob:PPyObject;i:NativeInt):integer; cdecl; external;
  procedure Py_Finalize ; cdecl; external;
  function PyErr_ExceptionMatches ( exc : PPyObject) : Integer; cdecl; external;
  function PyErr_GivenExceptionMatches ( raised_exc, exc : PPyObject) : Integer; cdecl; external;
  function PyEval_EvalCode ( co : PPyCodeObject; globals, locals : PPyObject) : PPyObject; cdecl; external;
  function Py_GetVersion : PAnsiChar; cdecl; external;
  function Py_GetCopyright : PAnsiChar; cdecl; external;
  function Py_GetExecPrefix : PAnsiChar; cdecl; external;
  function Py_GetPath : PAnsiChar; cdecl; external;
  procedure Py_SetPythonHome (home : PWideChar); cdecl; external;
  function Py_GetPythonHome : PWideChar; cdecl; external;
  function Py_DecodeLocale (arg: PAnsiChar; size: pinteger): PWideChar; cdecl; external;
  function Py_GetPrefix : PAnsiChar; cdecl; external;
  function Py_GetProgramName : PAnsiChar; cdecl; external;
  function PyParser_SimpleParseStringFlags ( str : PAnsiChar; start, flags : Integer) : PNode; cdecl; external;
  procedure PyNode_Free ( n : PNode ); cdecl; external;
  function PyErr_NewException ( name : PAnsiChar; base, dict : PPyObject ) : PPyObject; cdecl; external;
  function Py_Malloc ( size : NativeInt ) : Pointer; external;
  function PyMem_Malloc ( size : NativeInt ) : Pointer; external;
  procedure Py_SetProgramName ( name: PWideChar); cdecl; external;
  function Py_IsInitialized : integer; cdecl; external;
  function Py_GetProgramFullPath : PAnsiChar; cdecl; external;
  function Py_NewInterpreter : PPyThreadState; cdecl; external;
  procedure Py_EndInterpreter ( tstate: PPyThreadState); cdecl; external;
  procedure PyEval_AcquireLock ; cdecl; external;
  procedure PyEval_ReleaseLock ; cdecl; external;
  procedure PyEval_AcquireThread ( tstate: PPyThreadState); cdecl; external;
  procedure PyEval_ReleaseThread ( tstate: PPyThreadState); cdecl; external;
  function PyInterpreterState_New : PPyInterpreterState; cdecl; external;
  procedure PyInterpreterState_Clear ( interp: PPyInterpreterState); cdecl; external;
  procedure PyInterpreterState_Delete ( interp: PPyInterpreterState); cdecl; external;
  function PyThreadState_New ( interp: PPyInterpreterState): PPyThreadState; cdecl; external;
  procedure PyThreadState_Clear ( tstate: PPyThreadState); cdecl; external;
  procedure PyThreadState_Delete ( tstate: PPyThreadState); cdecl; external;
  function PyThreadState_Get : PPyThreadState; cdecl; external;
  function PyThreadState_Swap ( tstate: PPyThreadState): PPyThreadState; cdecl; external;
  procedure PyErr_SetInterrupt ; cdecl; external;
  function PyGILState_Ensure () : PyGILstate_STATE; cdecl; external;
  procedure PyGILState_Release (gilstate : PyGILState_STATE); cdecl; external;

implementation

end.