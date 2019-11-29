{$mode objfpc}

{$ifdef darwin}
{$linklib libpython3.7m.a}
{$endif}

{$ifdef linux}
{$linklib c}
{$linklib m}
{$linklib pthread}
{$linklib util}
{$linklib dl}
{$linklib libpython3.7m.a}
{$endif}

// https://www.python.org/downloads/release/python-374/

// build (darwin and linux):
// ./configure --disable-shared
// make

program Main;
uses
	Python3Core, PythonBridge, SysUtils;

function PyVERSION(Self, Args : PPyObject): PPyObject; cdecl;
begin
  result:= PyString_FromString('0.0.1b');
end;

type
  TForm = class
    procedure GotPythonData(data: ansistring); 
  end;

procedure TForm.GotPythonData(data: ansistring); 
begin
  writeln(data);
end;

var
  methods: array[0..0] of TPythonBridgeMethod = ( (name: 'version'; callback: @PyVERSION; help: 'doc')
                                                );

var
  test_file: ansistring = 'import gl'+LF+
                          'import sys'+LF+
                          'print(gl.version())'+LF+
                          'print(sys.path)'+LF+
                          #0;

var
  home: ansistring;
  form: TForm;
begin
  form := TForm.Create;

  home := ExtractFileDir(ParamStr(0))+'/python37';

  {$ifdef PYTHON_DYNAMIC}
  LoadLibrary(home+'/bin/libpython3.7.dylib');
  {$endif}

  PythonInitialize(home, @form.GotPythonData);
  PythonAddModule('gl', @methods, length(methods));

  PyRun_SimpleString(PAnsiChar(test_file));
end.