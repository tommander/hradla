unit umcdef;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  SynEdit, SynHighlighterAny, SynHighlighterIni, ComCtrls;

type

  { TfMCDef }

  TfMCDef = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    dlgOpen: TOpenDialog;
    Panel1: TPanel;
    dlgSave: TSaveDialog;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
  private

  public
  end;

var
  fMCDef: TfMCDef;

function AppDir(AFile: string = ''): string;

implementation

uses umainform;

{$R *.lfm}

{ TfMCDef }

function AppDir(AFile: string = ''): string;
begin
  result := ExtractFilePath(ParamStr(0));
  if AFile <> '' then
  begin
    result := IncludeTrailingPathDelimiter(result)+AFile;
  end;
end;

procedure TfMCDef.BitBtn2Click(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    SynEdit1.Lines.LoadFromFile(dlgOpen.FileName);
    showmessage('ok');
  end;
end;

procedure TfMCDef.BitBtn1Click(Sender: TObject);
begin
  SynEdit1.Lines.Clear;
end;

procedure TfMCDef.BitBtn3Click(Sender: TObject);
begin
  if dlgSave.Execute then
  begin
    SynEdit1.Lines.SaveToFile(dlgSave.FileName);
    showmessage('ok');
  end;
end;

procedure TfMCDef.BitBtn4Click(Sender: TObject);
begin
  Close;
end;

procedure TfMCDef.BitBtn5Click(Sender: TObject);
var strName,strFile: string;
begin
  strName := '';
  if not InputQuery('New custom map', 'Name?', strName) then
  begin
    showmessage('Cancelled');
    Exit;
  end;

  strFile := AppDir('custmaps/'+strName+'.mcdef');

  if FileExists(strFile) then
  begin
    showmessagefmt('Custom map "%s" already exists', [strName]);
    Exit;
  end;

  SynEdit1.Lines.SaveToFile(strFile);
  showmessagefmt('Custom map "%s" was successfully saved.', [strName]);
  fMainForm.ListCustmaps();
end;

end.

