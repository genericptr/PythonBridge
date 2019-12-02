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
    procedure GotPythonData(data: UnicodeString); 
  end;

procedure TForm.GotPythonData(data: UnicodeString); 
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
  home, root: ansistring;
  form: TForm;
begin
  form := TForm.Create;
  root := ExtractFileDir(ParamStr(0));

  {$ifdef unix}
    home := root+'/python37';
    {$ifdef PYTHON_DYNAMIC}
    LoadLibrary(home+'/darwin/libpython3.7.dylib');
    {$endif}
  {$endif}

  {$ifdef windows}
    home := root+'\windows\python37';
    {$ifdef PYTHON_DYNAMIC}
    LoadLibrary(root+'\windows\python37.dll');
    {$endif}
  {$endif}


  PythonInitialize(home, @form.GotPythonData);
  PythonAddModule('gl', @methods, length(methods));

  PyRun_SimpleString(PAnsiChar(test_file));
end.