{$mode objfpc}
{$assertions on}

unit PythonBridge;
interface
uses
  Python3Core, SysUtils;

type
  TPythonBridgeMethod = record
    name: ansistring;
    callback: PyCFunction;
    help: ansistring;
  end;
  TPythonBridgeMethodArray = array[0..0] of TPythonBridgeMethod;
  PPythonBridgeMethodArray = ^TPythonBridgeMethodArray;

type
  TPythonModule = class
    private
      FModuleName : ansiString;
      FModule : PPyObject;
      FMethodCount : integer;
      FAllocatedMethodCount : integer;
      FMethods : PPyMethodDef;
      FModuleDef : PyModuleDef;

      procedure AllocMethods;
      procedure ReallocMethods;
    public
      constructor Create(name: ansistring);
      destructor Destroy; override;
      procedure Finalize;
      function AddMethod(AMethodName: PAnsiChar;
                         AMethod: PyCFunction;
                         ADocString: PAnsiChar): PPyMethodDef;
      property MethodCount: integer read FMethodCount;
      property MethodsData: PPyMethodDef read FMethods;
      property ModuleDef: PyModuleDef read FModuleDef;
  end;

type
  PythonDataMethodCallback = procedure (data: ansistring) of object; 

procedure PythonInitialize(pythonHome: ansistring; callback: PythonDataMethodCallback);
function PythonAddModule(name: ansistring; methods: PPythonBridgeMethodArray; count: integer): TPythonModule;

function PyString_FromString( str: ansistring): PPyObject;

implementation

type
  TMethodArray = array[ 0 .. 16000 ] of PyMethodDef;
  PMethodArray = ^TMethodArray;
  EPythonError = class(Exception)
    public
      EName : String;
      EValue : String;
  end;
  EPyExecError   = class(EPythonError);

var
  DataMethodCallback: PythonDataMethodCallback = nil;

function PyUnicode_FromWideString(const AString : UnicodeString) : PPyObject;
{$IFDEF unix}
var
  _ucs4Str : UCS4String;
{$ENDIF}
begin
{$IFDEF unix}
  // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
  _ucs4Str := WideStringToUCS4String(AString);
  Result := PyUnicode_FromWideChar( {PWideChar}(@_ucs4Str[0]), Length(_ucs4Str)-1 {trim trailing zero});
{$ELSE}
  Result := PyUnicode_FromWideChar( PWideChar(AString), Length(AString) );
{$ENDIF}
end;

function PyUnicode_AsWideString( obj : PPyObject ) : UnicodeString;
var
  _size : Integer;
{$IFDEF unix}
  _ucs4Str : UCS4String;
{$ENDIF}
begin
    //_size := PySequence_Length(obj); //not fully correct for some Unicode
    _size := PyUnicode_GetSize(obj);
    if _size > 0 then
    begin
{$IFDEF unix}
      // Note that Linux uses UCS4 strings, whereas it declares using UCS2 strings!!!
      SetLength(_ucs4Str, _size+1);
      if PyUnicode_AsWideChar(obj, @_ucs4Str[0], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
      Result := UCS4StringToWideString(_ucs4Str);
      // remove trailing zeros (needed by Kylix1)
      while (Length(Result) > 0) and (Result[Length(Result)] = #0) do
        Delete(Result, Length(Result), 1);
{$ELSE}
      SetLength(Result, _size);
      if PyUnicode_AsWideChar(obj, @Result[1], _size) <> _size then
        raise EPythonError.Create('Could not copy the whole Unicode string into its buffer');
{$ENDIF}
    end
    else
      Result := '';
end;

function PyString_FromString( str: ansistring): PPyObject;
var
  _text : UnicodeString;
begin
  _text := UnicodeString(str);
  result := PyUnicode_FromWideString(_text);
end;

function pyio_write(self, args : PPyObject) : PPyObject; cdecl;
var
  a1 : PPyObject;
begin

  // TODO: still keep this?
  // Forbid printing for any other thread than the main one
  //if GetCurrentThreadId <> MainThreadId then

  if Assigned(args) and (PyTuple_Size(args) > 0) then
    begin
      a1 := PyTuple_GetItem(args, 0);
      if Assigned(a1) then
        DataMethodCallback(PyUnicode_AsWideString(a1));
      Result := Py_None;
    end
  else
    begin
      PyErr_BadArgument;
      Result := nil;
    end;
end;

procedure CheckError(ACatchStopEx : Boolean = False);
begin
  if PyErr_Occurred <> nil then
    begin
      if ACatchStopEx and (PyErr_GivenExceptionMatches(PyErr_Occurred(), PyExc_StopIteration^) <> 0) then
        begin
          PyErr_Clear;
          raise EPythonError.Create('Stop iteration');
        end
      else
        begin
          PyErr_Print;
          //Traceback.Refresh;
          //RaiseError;
          raise EPythonError.Create('Error');
        end;
    end;
end;

function ExecString(command : ansistring; mode : Integer; locals : PPyObject = nil; globals : PPyObject = nil) : PPyObject;

  function CleanString(const s : ansistring) : ansistring;
  var
    i : Integer;
  begin
    result := s;
    if s = '' then
      Exit;
    i := Pos(CR,s);
    while i > 0 do
      begin
        Delete( result, i, 1 );
        i := Pos(CR,result);
      end;
    if result[length(result)] <> LF then
      Insert( LF, result, length(result)+1 );
  end;

var
  m : PPyObject;
  _locals, _globals : PPyObject;
begin
  Result := nil;

  m := PyImport_AddModule(PAnsiChar('__main__'));
  if m = nil then
    raise EPythonError.Create('can''t create __main__');

  if Assigned(locals) then
    _locals  := locals
  else
    _locals  := PyModule_GetDict(m);

  if Assigned(globals) then
    _globals := globals
  else
    _globals := PyModule_GetDict(m);

  try
    Result := PyRun_String(PAnsiChar(CleanString(command)), mode, _globals, _locals);
    if Result = nil then
      CheckError(False);
  except
    if PyErr_Occurred <> nil then
      CheckError(False)
    else
      raise EPythonError.Create('PyRun_String');
  end;
end;

function TPythonModule.AddMethod( AMethodName  : PAnsiChar;
                                  AMethod  : PyCFunction;
                                  ADocString : PAnsiChar ) : PPyMethodDef;
begin
  Assert(FMethods <> nil);
  if FMethodCount = FAllocatedMethodCount then
    ReallocMethods;
  Result := @(PMethodArray(FMethods)^[MethodCount]);
  Result^.ml_name  := AMethodName;
  Result^.ml_meth  := AMethod;
  Result^.ml_flags := METH_VARARGS;
  Result^.ml_doc   := ADocString;
  Inc(FMethodCount);
end;

procedure TPythonModule.ReallocMethods;
var
  MethodPtr : PPyMethodDef;
begin
  Inc(FAllocatedMethodCount, PYT_METHOD_BUFFER_INCREASE);
  ReAllocMem(FMethods, SizeOf(PyMethodDef)*(FAllocatedMethodCount+1));
  MethodPtr :=@(PMethodArray(FMethods)^[MethodCount+1]);
  FillChar(MethodPtr^,SizeOf(PyMethodDef)*PYT_METHOD_BUFFER_INCREASE,0);
end;

procedure TPythonModule.AllocMethods;
begin
  Assert(FMethods = nil);
  FAllocatedMethodCount := PYT_METHOD_BUFFER_INCREASE;
  FMethodCount := 0;
  FMethods := PPyMethodDef(AllocMem(SizeOf(PyMethodDef)*(FAllocatedMethodCount+1)));
end;

procedure TPythonModule.Finalize; 
var
  modules: PPyObject;
begin
  FModuleDef.m_base.ob_refcnt := 1;
  FModuleDef.m_name := PAnsiChar(FModuleName);
  FModuleDef.m_methods := MethodsData;
  FModuleDef.m_size := -1;

  // https://docs.python.org/3.1/c-api/module.html
  FModule:= PyModule_Create2(@ModuleDef, 1013);
  //Result:= PyModule_Create(@md);

  if not Assigned(FModule) then
    CheckError;
  modules := PyImport_GetModuleDict();
  if PyDict_SetItemString(modules, ModuleDef.m_name, FModule) <> 0 then
    CheckError;
end;

constructor TPythonModule.Create(name: ansistring);
begin
  FModuleName := name;
  FillChar(FModuleDef, SizeOf(FModuleDef), 0);
  AllocMethods;
end;

destructor TPythonModule.Destroy;
begin
  FreeMem(FMethods);
  FMethods := nil;
  FMethodCount := -1;
end;

var
  pyio_module: TPythonModule = nil;

function PythonAddModule(name: ansistring; methods: PPythonBridgeMethodArray; count: integer): TPythonModule;
var
  i: integer;
begin
  result := TPythonModule.Create(name);
  for i := 0 to count - 1 do
    result.AddMethod(PAnsiChar(methods^[i].name), methods^[i].callback, PAnsiChar(methods^[i].help));
  result.Finalize;
end;

procedure RedirectIO;
var
  code: ansistring = 'import sys'+LF+
         'class DebugOutput:'+LF+
         '  pyio = __import__("pyio")'+LF+
         '  softspace=0'+LF+
         '  encoding=None'+LF+
         '  def write(self,message):'+LF+
         '     self.pyio.write(message)'+LF+
         '  def flush(self):' + LF +
         '     pass' + LF +
         'sys.old_stdin=sys.stdin'+LF+
         'sys.old_stdout=sys.stdout'+LF+
         'sys.old_stderr=sys.stderr'+LF+
         #0;
begin
  pyio_module := TPythonModule.Create('pyio');
  pyio_module.AddMethod('write', @pyio_write, 'write(String) -> None');
  pyio_module.Finalize;
  PyRun_SimpleString(PAnsiChar(code));
end;

procedure PythonInitialize(pythonHome: ansistring; callback: PythonDataMethodCallback);
begin
  DataMethodCallback := callback;

  Py_SetPythonHome(Py_DecodeLocale(PAnsiChar(pythonHome), nil));
  Py_Initialize;
  // TODO: do we need this?
  //PyEval_InitThreads;
  RedirectIO;
end;

end.