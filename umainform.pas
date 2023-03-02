unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, Spin, Math, umcdef, umapcomponentmap, umapcomponentbase,
  umapcomponentwire, umapcomponentinput, umapcomponentoutput, umapcomponentgate,
  umapcomponentio;

const
  intMapWidth: word = 20;
  intMapHeight: word = 10;

type
  TMapAction = (maSelect,maDelete,maAdd);

  { TfMainForm }

  TfMainForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    btnInP0: TBitBtn;
    btnWireRT: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    btnWireLB: TBitBtn;
    BitBtn2: TBitBtn;
    btnInP3: TBitBtn;
    btnWireRB: TBitBtn;
    btnInP2: TBitBtn;
    btnWireH: TBitBtn;
    btnWireV: TBitBtn;
    btnInP1: TBitBtn;
    btnOutP0: TBitBtn;
    btnOutP3: TBitBtn;
    btnOutP2: TBitBtn;
    btnOutP1: TBitBtn;
    btnWireLT: TBitBtn;
    btnGateP10: TBitBtn;
    btnGateP13: TBitBtn;
    btnGateP12: TBitBtn;
    btnGateP11: TBitBtn;
    btnGateP20: TBitBtn;
    btnGateP23: TBitBtn;
    btnGateP22: TBitBtn;
    btnGateP21: TBitBtn;
    btnGateP30: TBitBtn;
    btnGateP33: TBitBtn;
    btnGateP32: TBitBtn;
    btnGateP31: TBitBtn;
    BitBtn42: TBitBtn;
    BitBtn43: TBitBtn;
    BitBtn44: TBitBtn;
    BitBtn45: TBitBtn;
    BitBtn46: TBitBtn;
    BitBtn47: TBitBtn;
    BitBtn48: TBitBtn;
    BitBtn49: TBitBtn;
    BitBtn50: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    il32: TImageList;
    il16: TImageList;
    il24: TImageList;
    imgMain: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label3: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    ListView2: TListView;
    dlgOpen: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel2: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pnlR: TPanel;
    pnlT: TPanel;
    pnlL: TPanel;
    pnlB: TPanel;
    ScrollBox1: TScrollBox;
    ScrollBox2: TScrollBox;
    ScrollBox3: TScrollBox;
    ScrollBox4: TScrollBox;
    ScrollBox5: TScrollBox;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    splR: TSplitter;
    splL: TSplitter;
    splT: TSplitter;
    splB: TSplitter;
    Timer2: TTimer;
    tsWire: TTabSheet;
    tsInput: TTabSheet;
    tsOutput: TTabSheet;
    tsGate: TTabSheet;
    tsMap: TTabSheet;
    Timer1: TTimer;
    (* EVENTS *)
    procedure BitBtn10Click(Sender: TObject);
    procedure BitBtn11Click(Sender: TObject);
    procedure BitBtn12Click(Sender: TObject);
    procedure BitBtn13Click(Sender: TObject);
    procedure btnInP0Click(Sender: TObject);
    procedure btnWireRTClick(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure btnWireLBClick(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure btnInP3Click(Sender: TObject);
    procedure btnWireRBClick(Sender: TObject);
    procedure btnInP2Click(Sender: TObject);
    procedure btnWireHClick(Sender: TObject);
    procedure btnWireVClick(Sender: TObject);
    procedure btnInP1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure btnGateP10Click(Sender: TObject);
    procedure btnGateP13Click(Sender: TObject);
    procedure btnGateP12Click(Sender: TObject);
    procedure btnGateP11Click(Sender: TObject);
    procedure btnGateP20Click(Sender: TObject);
    procedure btnGateP23Click(Sender: TObject);
    procedure btnGateP22Click(Sender: TObject);
    procedure btnGateP21Click(Sender: TObject);
    procedure btnGateP30Click(Sender: TObject);
    procedure btnGateP33Click(Sender: TObject);
    procedure btnWireLTClick(Sender: TObject);
    procedure btnGateP32Click(Sender: TObject);
    procedure btnGateP31Click(Sender: TObject);
    procedure BitBtn42Click(Sender: TObject);
    procedure BitBtn43Click(Sender: TObject);
    procedure BitBtn44Click(Sender: TObject);
    procedure BitBtn45Click(Sender: TObject);
    procedure BitBtn46Click(Sender: TObject);
    procedure BitBtn47Click(Sender: TObject);
    procedure BitBtn48Click(Sender: TObject);
    procedure BitBtn49Click(Sender: TObject);
    procedure BitBtn4Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure BitBtn7Click(Sender: TObject);
    procedure BitBtn8Click(Sender: TObject);
    procedure BitBtn9Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgMainMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure imgMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure imgMainResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    (* MINE *)
    procedure DrawMap(AStyle: TMCDrawMapStyle);
    procedure ToggleInput();
    procedure UpdateSelection();
    procedure ListCustmaps();
    procedure ToggleWireConn(APinA,APinB: integer);
    procedure SetIOPin(APin: integer);
    procedure SetGatePin(APin: integer; AType: byte);
    procedure SetGateType(AType: TMCGate);
  private
    var map: TMapComponentMap;
    var act: TMapAction;
    var cmp: TMapComponentBase;
  public

  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.lfm}

{ TfMainForm }

(* MINE *)

procedure TfMainForm.DrawMap(AStyle: TMCDrawMapStyle);
begin
  map.Draw(imgMain.Canvas, TRect.Create(0, 0, imgMain.Width, imgMain.Height), AStyle, mdsNormal);
end;

procedure TfMainForm.ToggleInput();
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentInput)) then
  begin
    Exit;
  end;

  TMapComponentInput(cmp).Value := not TMapComponentInput(cmp).Value;
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.UpdateSelection();
var i: integer;
    mcg: TMCGate;
begin
  Image1.Picture.Clear;
  Label2.Caption := '';
  Label6.Caption := '';
  Label4.Caption := Format('%dx%d', [map.CursorPos.x, map.CursorPos.y]);
  Label10.Caption := Format('%dx%d', [map.CursorSize.w, map.CursorSize.h]);
  PageControl1.Hide;

  if not Assigned(cmp) then
  begin
    cmp := map;
  end;

  Label6.Caption := cmp.Name;
  Label4.Caption := Format('%dx%d', [cmp.Pos.x, cmp.Pos.y]);
  Label10.Caption := Format('%dx%d', [cmp.Size.w, cmp.Size.h]);
  PageControl1.Show;

  if cmp is TMapComponentWire then
  begin
    il32.GetBitmap(0, Image1.Picture.Bitmap);
    Label2.Caption := 'Wire';
    PageControl1.ActivePageIndex := tsWire.PageIndex;
    Label11.Caption := '';
    for i := TMapComponentWire(cmp).PinLow() to TMapComponentWire(cmp).PinHigh() do
    begin
      if Label11.Caption <> '' then
      begin
        Label11.Caption := Label11.Caption + #13#10;
      end;
      Label11.Caption := Label11.Caption + Format('#%d'#9'%s'#9'%s'#9'%s', [i, BoolToStr(TMapComponentWire(cmp).PinActive[i], 'active', 'inactive'), BoolToStr(TMapComponentWire(cmp).PinValue[i], 'true', 'false'), BoolToStr(TMapComponentWire(cmp).HasConnection(i), 'yes', 'no')]);
    end;
  end
  else if cmp is TMapComponentInput then
  begin
    il32.GetBitmap(1, Image1.Picture.Bitmap);
    Label2.Caption := 'Input';
    PageControl1.ActivePageIndex := tsInput.PageIndex;
    Label16.Caption := BoolToStr(TMapComponentInput(cmp).Value, 'true', 'false');
    Label12.Caption := '';
    for i := TMapComponentInput(cmp).PinLow() to TMapComponentInput(cmp).PinHigh() do
    begin
      if Label12.Caption <> '' then
      begin
        Label12.Caption := Label12.Caption + #13#10;
      end;
      Label12.Caption := Label12.Caption + Format('#%d'#9'%s'#9'%s', [i, BoolToStr(TMapComponentInput(cmp).PinActive[i], 'active', 'inactive'), BoolToStr(TMapComponentInput(cmp).PinValue[i], 'true', 'false')]);
    end;
  end
  else if cmp is TMapComponentOutput then
  begin
    il32.GetBitmap(2, Image1.Picture.Bitmap);
    Label2.Caption := 'Output';
    PageControl1.ActivePageIndex := tsOutput.PageIndex;
    Label18.Caption := BoolToStr(TMapComponentOutput(cmp).Value, 'true', 'false');
    Label13.Caption := '';
    for i := TMapComponentOutput(cmp).PinLow() to TMapComponentOutput(cmp).PinHigh() do
    begin
      if Label13.Caption <> '' then
      begin
        Label13.Caption := Label13.Caption + #13#10;
      end;
      Label13.Caption := Label13.Caption + Format('#%d'#9'%s'#9'%s', [i, BoolToStr(TMapComponentOutput(cmp).PinActive[i], 'active', 'inactive'), BoolToStr(TMapComponentOutput(cmp).PinValue[i], 'true', 'false')]);
    end;
  end
  else if cmp is TMapComponentGate then
  begin
    il32.GetBitmap(3, Image1.Picture.Bitmap);
    Label2.Caption := 'Gate';
    PageControl1.ActivePageIndex := tsGate.PageIndex;
    Label14.Caption := '';
    for i := TMapComponentGate(cmp).PinLow() to TMapComponentGate(cmp).PinHigh() do
    begin
      if Label14.Caption <> '' then
      begin
        Label14.Caption := Label14.Caption + #13#10;
      end;
      Label14.Caption := Label14.Caption + Format('#%d'#9'%s'#9'%s', [i, BoolToStr(TMapComponentGate(cmp).PinActive[i], 'active', 'inactive'), BoolToStr(TMapComponentGate(cmp).PinValue[i], 'true', 'false')]);
    end;
  end
  else if cmp is TMapComponentMap then
  begin
    il32.GetBitmap(4, Image1.Picture.Bitmap);
    Label2.Caption := 'Map';
    PageControl1.ActivePageIndex := tsMap.PageIndex;
    Label21.Caption := IntToStr(TMapComponentMap(cmp).SubcomponentCount());
    Label22.Caption := '';
    for i := 0 to TMapComponentMap(cmp).SubcomponentCount()-1 do
    begin
      if Label22.Caption <> '' then
      begin
        Label22.Caption := Label22.Caption + #13#10;
      end;
      if not Assigned(TMapComponentMap(cmp).Subcomponent(i)) then
      begin
        Label22.Caption := Label22.Caption + Format('#%d'#9'nil', [i]);
      end
      else
      begin
        Label22.Caption := Label22.Caption + Format('#%d'#9'%s'#9'%s'#9'[%d;%d][%d;%d]', [i, TMapComponentMap(cmp).Subcomponent(i).Name, MCTypeIntToStr(MCTypeInt(TMapComponentMap(cmp).Subcomponent(i))), TMapComponentMap(cmp).Subcomponent(i).Pos.x, TMapComponentMap(cmp).Subcomponent(i).Pos.y, TMapComponentMap(cmp).Subcomponent(i).Size.w, TMapComponentMap(cmp).Subcomponent(i).Size.h]);
      end;
    end;
  end;
end;

procedure TfMainForm.ListCustmaps();
var sr: TSearchRec;
    li: TListItem;
begin
  ListView2.Items.Clear;

  if not DirectoryExists(AppDir('custmaps')) then
  begin
    try
      mkdir(AppDir('custmaps'));
    except
      showmessage('Please create the subdirectory "custmaps" first.');
    end;
    Exit;
  end;

  if FindFirst(AppDir('custmaps/*.mcdef'), faReadOnly, sr) <> -1 then
  begin
    try
      repeat
        li := ListView2.Items.Add;
        li.Caption := ChangeFileExt(sr.Name, '');
        li.SubItems.Add(AppDir('custmaps/'+sr.Name));
      until
        FindNext(sr) <> 0;
    finally
      FindClose(sr);
    end;
  end;
end;

procedure TfMainForm.ToggleWireConn(APinA,APinB: integer);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentWire)) then
  begin
    Exit;
  end;

  if TMapComponentWire(cmp).Connected(APinA, APinB) > -1 then
  begin
    TMapComponentWire(cmp).Disconnect(APinA, APinB);
  end
  else
  begin
    TMapComponentWire(cmp).Connect(APinA, APinB);
  end;

  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.SetIOPin(APin: integer);
var i: integer;
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentIO)) or
     (APin < TMapComponentIO(cmp).PinLow()) or (APin > TMapComponentIO(cmp).PinHigh()) then
  begin
    Exit;
  end;

  for i := TMapComponentIO(cmp).PinLow() to TMapComponentIO(cmp).PinHigh() do
  begin
    TMapComponentIO(cmp).PinActive[i] := false;
  end;
  TMapComponentIO(cmp).PinActive[APin] := true;

  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.SetGatePin(APin: integer; AType: byte);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentGate)) then
  begin
    Exit;
  end;

  case AType of
    0: TMapComponentGate(cmp).PinI1 := APin;
    1: TMapComponentGate(cmp).PinI2 := APin;
    2: TMapComponentGate(cmp).PinO1 := APin;
  end;

  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.SetGateType(AType: TMCGate);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentGate)) then
  begin
    Exit;
  end;

  TMapComponentGate(cmp).Gate := AType;
  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

(* EVENTS *)

procedure TfMainForm.imgMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sl: TStringList;
    s{,ss}: string;
//    mcg: TMCGate;
//    intCmp: integer;
//    theCmp: TMapComponentBase;
begin
  cmp := map.Subcomponent(map.SubcomponentByField(map.CursorPos));

  if act = maSelect then
  begin
    if ssDouble in Shift then
    begin
      ToggleInput();
    end;
    UpdateSelection();
    Exit;
  end;

  if act = maDelete then
  begin
    map.RemoveSubcomponent(map.SubcomponentByField(map.CursorPos));
    cmp := nil;
    map.RedrawField := map.CursorPos;
    DrawMap(mdmsRedraw);
    UpdateSelection();
    Exit;
  end;

  if act = maAdd then
  begin
    if (not Assigned(ListView2.Selected)) or (ListView2.Selected.SubItems.Count = 0) or (not FileExists(ListView2.Selected.SubItems[0])) then
    begin
      Exit;
    end;

    sl := TStringList.Create;
    try
      sl.LoadFromFile(ListView2.Selected.SubItems[0]);
      if (Length(sl.Text) >= 6) then
      begin
        s := copy(sl.Text, 1, 6);
        if s = 'STD/WR' then
        begin
          map.AddWire(map.CursorPos);
        end
        else if s = 'STD/IN' then
        begin
          map.AddInput(map.CursorPos);
        end
        else if s = 'STD/OU' then
        begin
          map.AddOutput(map.CursorPos);
        end
        else if s = 'STD/GT' then
        begin
          map.AddGate(mcgNone, map.CursorPos);
        end
        else
        begin
          map.SetMCDef(0, sl.Text, map.CursorPos);
        end;
      end;
    finally
      sl.Free;
    end;
    map.RedrawField := map.CursorPos;
    DrawMap(mdmsRedraw);
    Exit;
  end;
end;

procedure TfMainForm.imgMainResize(Sender: TObject);
begin
  Timer1.Enabled := false;
  Timer1.Enabled := true;
end;

procedure TfMainForm.Timer1Timer(Sender: TObject);
begin
  imgMain.Picture.Bitmap.SetSize(imgMain.Width,imgMain.Height);
  DrawMap(mdmsComplete);
  Timer1.Enabled := false;
end;

procedure TfMainForm.Timer2Timer(Sender: TObject);
var intIL: integer;
    strCmd: string;
begin
  intIL := map.InputLength();
  strCmd := '';
  if Length(Edit1.Text) > intIL then
  begin
    strCmd := copy(Edit1.Text, Length(Edit1.Text)-intIL+1, intIL);
    Edit1.Text := copy(Edit1.Text, 1, Length(Edit1.Text)-intIL);
  end
  else
  begin
    strCmd := Edit1.Text;
    Edit1.Text := '';
    Timer2.Enabled := false;
  end;

  if not map.InputString(strCmd) then
  begin
    Timer2.Enabled := false;
    showmessage('InputString failed');
    Exit;
  end;
  BitBtn1.Click;
  if Edit2.Text <> '' then
  begin
    Edit2.Text := Edit2.Text + ';';
  end;
  Edit2.Text := Edit2.Text + map.OutputString();
end;

procedure TfMainForm.FormCreate(Sender: TObject);
begin
  map := TMapComponentMap.Create('map', MCSize(20,10), MCPos(0,0), nil);
  act := maSelect;
  cmp := map;
  Timer1Timer(nil);
  ListCustmaps();
  UpdateSelection();
end;

procedure TfMainForm.BitBtn2Click(Sender: TObject);
begin
  act := maSelect;
  Label1.Font.Color := clBlack;
  Label1.Caption := 'SELECT';
  imgMain.Cursor := crHandPoint;
end;

procedure TfMainForm.btnGateP10Click(Sender: TObject);
begin
  SetGatePin(0, 0);
end;

procedure TfMainForm.btnGateP13Click(Sender: TObject);
begin
  SetGatePin(3, 0);
end;

procedure TfMainForm.btnGateP12Click(Sender: TObject);
begin
  SetGatePin(2, 0);
end;

procedure TfMainForm.btnGateP11Click(Sender: TObject);
begin
  SetGatePin(1, 0);
end;

procedure TfMainForm.btnGateP20Click(Sender: TObject);
begin
  SetGatePin(0, 1);
end;

procedure TfMainForm.btnGateP23Click(Sender: TObject);
begin
  SetGatePin(3, 1);
end;

procedure TfMainForm.btnGateP22Click(Sender: TObject);
begin
  SetGatePin(2, 1);
end;

procedure TfMainForm.btnGateP21Click(Sender: TObject);
begin
  SetGatePin(1, 1);
end;

procedure TfMainForm.btnGateP30Click(Sender: TObject);
begin
  SetGatePin(0, 2);
end;

procedure TfMainForm.btnGateP33Click(Sender: TObject);
begin
  SetGatePin(3, 2);
end;

procedure TfMainForm.btnWireLTClick(Sender: TObject);
begin
  ToggleWireConn(0,3);
end;

procedure TfMainForm.btnGateP32Click(Sender: TObject);
begin
  SetGatePin(2, 2);
end;

procedure TfMainForm.btnGateP31Click(Sender: TObject);
begin
  SetGatePin(1, 2);
end;

procedure TfMainForm.BitBtn42Click(Sender: TObject);
begin
  SetGateType(mcgBuf);
end;

procedure TfMainForm.BitBtn43Click(Sender: TObject);
begin
  SetGateType(mcgNand);
end;

procedure TfMainForm.BitBtn44Click(Sender: TObject);
begin
  SetGateType(mcgAnd);
end;

procedure TfMainForm.BitBtn45Click(Sender: TObject);
begin
  SetGateType(mcgInv);
end;

procedure TfMainForm.BitBtn46Click(Sender: TObject);
begin
  SetGateType(mcgOr);
end;

procedure TfMainForm.BitBtn47Click(Sender: TObject);
begin
  SetGateType(mcgXnor);
end;

procedure TfMainForm.BitBtn48Click(Sender: TObject);
begin
  SetGateType(mcgXor);
end;

procedure TfMainForm.BitBtn49Click(Sender: TObject);
begin
  SetGateType(mcgNor);
end;

procedure TfMainForm.BitBtn1Click(Sender: TObject);
begin
  map.Tick();
  DrawMap(mdmsComplete);
end;

procedure TfMainForm.btnInP3Click(Sender: TObject);
begin
  SetIOPin(3);
end;

procedure TfMainForm.btnWireRBClick(Sender: TObject);
begin
  ToggleWireConn(1,2);
end;

procedure TfMainForm.btnInP2Click(Sender: TObject);
begin
  SetIOPin(2);
end;

procedure TfMainForm.btnWireHClick(Sender: TObject);
begin
  ToggleWireConn(0,2);
end;

procedure TfMainForm.btnWireVClick(Sender: TObject);
begin
  ToggleWireConn(1,3);
end;

procedure TfMainForm.btnInP1Click(Sender: TObject);
begin
  SetIOPin(1);
end;

procedure TfMainForm.BitBtn11Click(Sender: TObject);
begin
  map.SetMCDef(0,Format('size="%d,%d"', [SpinEdit1.Value, SpinEdit2.Value]),MCPos());
  DrawMap(mdmsComplete);
end;

procedure TfMainForm.BitBtn10Click(Sender: TObject);
begin
  if not Timer2.Enabled then
  begin
    Edit2.Text := '';
  end;
  Timer2.Enabled := not Timer2.Enabled;
end;

procedure TfMainForm.BitBtn12Click(Sender: TObject);
begin
  act := maDelete;
  Label1.Font.Color := clRed;
  Label1.Caption := 'DELETE';
  imgMain.Cursor := crNo;
end;

procedure TfMainForm.BitBtn13Click(Sender: TObject);
begin
  act := maAdd;
  Label1.Font.Color := clGreen;
  Label1.Caption := 'ADD';
  imgMain.Cursor := crDrag;
end;

procedure TfMainForm.btnInP0Click(Sender: TObject);
begin
  SetIOPin(0);
end;

procedure TfMainForm.btnWireRTClick(Sender: TObject);
begin
  ToggleWireConn(2,3);
end;

procedure TfMainForm.BitBtn16Click(Sender: TObject);
begin
  map.Clear(true);
  DrawMap(mdmsComplete);
end;

procedure TfMainForm.BitBtn17Click(Sender: TObject);
var strName: string;
begin
  if not Assigned(cmp) then
  begin
    Exit;
  end;

  strName := cmp.Name;
  if InputQuery('Component name', 'New name', strName) then
  begin
    cmp.Name := strName;
  end;
end;

procedure TfMainForm.BitBtn18Click(Sender: TObject);
var sl: TStringList;
begin
  if dlgOpen.Execute then
  begin
    BitBtn16.Click;
    sl := TStringList.Create;
    try
      sl.LoadFromFile(dlgOpen.FileName);
      map.SetMCDef(0, sl.Text,MCPos());
      DrawMap(mdmsComplete);
    finally
      sl.Free;
    end;
  end;
end;

procedure TfMainForm.btnWireLBClick(Sender: TObject);
begin
  ToggleWireConn(0,1);
end;

procedure TfMainForm.BitBtn4Click(Sender: TObject);
begin
end;

procedure TfMainForm.BitBtn5Click(Sender: TObject);
begin
end;

procedure TfMainForm.BitBtn6Click(Sender: TObject);
begin
end;

procedure TfMainForm.BitBtn7Click(Sender: TObject);
begin
end;

procedure TfMainForm.BitBtn8Click(Sender: TObject);
begin
  map.Zero();
  DrawMap(mdmsComplete);
end;

procedure TfMainForm.BitBtn9Click(Sender: TObject);
begin
  fMCDef.SynEdit1.Lines.Text := map.GetMCDef(0);
  fMCDef.Show;
end;

procedure TfMainForm.FormDestroy(Sender: TObject);
begin
  if Assigned(map) then
  begin
    map.Free;
  end;
end;

procedure TfMainForm.imgMainMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  map.CursorPos := MCPos(Math.Floor(X / (imgMain.Width/map.Size.w)), Math.Floor(Y / (imgMain.Height/map.Size.h)));
  map.CursorSize := MCSize(1,1);
  DrawMap(mdmsSelected);
end;

end.

