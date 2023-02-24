program hradla;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umainform, ulogger,
  umapcomponentinput, umapcomponentbase, umapcomponentpinned,
  umapcomponentoutput, umapcomponentio, umapcomponentgate, umapcomponentwire,
  umapcomponentmap, umcdef;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfMCDef, fMCDef);
  Application.Run;
end.

