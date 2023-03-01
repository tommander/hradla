unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, Spin, Math, umcdef, umapcomponentmap, umapcomponentbase,
  umapcomponentwire, umapcomponentinput, umapcomponentoutput, umapcomponentgate;

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
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    ComboBox8: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
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
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
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
    Label7: TLabel;
    Label8: TLabel;
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
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
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
    procedure BitBtn14Click(Sender: TObject);
    procedure BitBtn15Click(Sender: TObject);
    procedure BitBtn16Click(Sender: TObject);
    procedure BitBtn17Click(Sender: TObject);
    procedure BitBtn18Click(Sender: TObject);
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
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
  Label8.Caption := '';
  Label4.Caption := Format('%dx%d', [map.CursorPos.x, map.CursorPos.y]);
  Label10.Caption := Format('%dx%d', [map.CursorSize.w, map.CursorSize.h]);
  PageControl1.Hide;

  if not Assigned(cmp) then
  begin
    cmp := map;
  end;

  Label6.Caption := cmp.Name;
  Label8.Caption := cmp.FullName;
  Label4.Caption := Format('%dx%d', [cmp.Pos.x, cmp.Pos.y]);
  Label10.Caption := Format('%dx%d', [cmp.Size.w, cmp.Size.h]);
  PageControl1.Show;

  if cmp is TMapComponentWire then
  begin
    ImageList1.GetBitmap(0, Image1.Picture.Bitmap);
    Label2.Caption := 'Wire';
    PageControl1.ActivePageIndex := tsWire.PageIndex;
    Label11.Caption := '';
    ComboBox1.Items.Clear;
    ComboBox2.Items.Clear;
    for i := TMapComponentWire(cmp).PinLow() to TMapComponentWire(cmp).PinHigh() do
    begin
      ComboBox1.Items.Add('Pin %d', [i]);
      ComboBox2.Items.Add('Pin %d', [i]);
      if Label11.Caption <> '' then
      begin
        Label11.Caption := Label11.Caption + #13#10;
      end;
      Label11.Caption := Label11.Caption + Format('#%d'#9'%s'#9'%s'#9'%s', [i, BoolToStr(TMapComponentWire(cmp).PinActive[i], 'active', 'inactive'), BoolToStr(TMapComponentWire(cmp).PinValue[i], 'true', 'false'), BoolToStr(TMapComponentWire(cmp).HasConnection(i), 'yes', 'no')]);
    end;
  end
  else if cmp is TMapComponentInput then
  begin
    ImageList1.GetBitmap(1, Image1.Picture.Bitmap);
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
    ImageList1.GetBitmap(2, Image1.Picture.Bitmap);
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
    ImageList1.GetBitmap(3, Image1.Picture.Bitmap);
    Label2.Caption := 'Gate';
    PageControl1.ActivePageIndex := tsGate.PageIndex;
    ComboBox3.Items.Clear;
    for mcg in TMCGate do
    begin
      ComboBox3.Items.Add(MCGateToStr(mcg));
    end;
    ComboBox3.ItemIndex := Integer(TMapComponentGate(cmp).Gate);
    Label14.Caption := '';
    ComboBox4.Items.Clear;
    ComboBox5.Items.Clear;
    ComboBox6.Items.Clear;
    for i := TMapComponentGate(cmp).PinLow() to TMapComponentGate(cmp).PinHigh() do
    begin
      ComboBox4.Items.Add('Pin %d', [i]);
      ComboBox5.Items.Add('Pin %d', [i]);
      ComboBox6.Items.Add('Pin %d', [i]);
      if Label14.Caption <> '' then
      begin
        Label14.Caption := Label14.Caption + #13#10;
      end;
      Label14.Caption := Label14.Caption + Format('#%d'#9'%s'#9'%s', [i, BoolToStr(TMapComponentGate(cmp).PinActive[i], 'active', 'inactive'), BoolToStr(TMapComponentGate(cmp).PinValue[i], 'true', 'false')]);
    end;
    ComboBox4.ItemIndex := TMapComponentGate(cmp).PinI1;
    ComboBox5.ItemIndex := TMapComponentGate(cmp).PinI2;
    ComboBox6.ItemIndex := TMapComponentGate(cmp).PinO1;
  end
  else if cmp is TMapComponentMap then
  begin
    ImageList1.GetBitmap(4, Image1.Picture.Bitmap);
    Label2.Caption := 'Map';
    PageControl1.ActivePageIndex := tsMap.PageIndex;
    Label21.Caption := IntToStr(TMapComponentMap(cmp).SubcomponentCount());
    Label22.Caption := '';
    Label29.Caption := IntToStr(TMapComponentMap(cmp).IOConnectionCount());
    Label30.Caption := '';
    ComboBox7.Items.Clear;
    for i := TMapComponentMap(cmp).PinLow() to TMapComponentMap(cmp).PinHigh() do
    begin
      ComboBox7.Items.Add('Pin %d', [i]);
    end;
    ComboBox8.Items.Clear;
    for i := 0 to TMapComponentMap(cmp).SubcomponentCount()-1 do
    begin
      if Label22.Caption <> '' then
      begin
        Label22.Caption := Label22.Caption + #13#10;
      end;
      if not Assigned(TMapComponentMap(cmp).Subcomponent(i)) then
      begin
        Label22.Caption := Label22.Caption + Format('#%d'#9'nil', [i]);
        ComboBox8.Items.Add('nil', []);
      end
      else
      begin
        Label22.Caption := Label22.Caption + Format('#%d'#9'%s'#9'%s'#9'[%d;%d][%d;%d]', [i, TMapComponentMap(cmp).Subcomponent(i).Name, MCTypeIntToStr(MCTypeInt(TMapComponentMap(cmp).Subcomponent(i))), TMapComponentMap(cmp).Subcomponent(i).Pos.x, TMapComponentMap(cmp).Subcomponent(i).Pos.y, TMapComponentMap(cmp).Subcomponent(i).Size.w, TMapComponentMap(cmp).Subcomponent(i).Size.h]);
        ComboBox8.Items.Add('%s (%s)', [TMapComponentMap(cmp).Subcomponent(i).Name, MCTypeIntToStr(MCTypeInt(TMapComponentMap(cmp).Subcomponent(i)))]);
      end;
    end;
    for i := 0 to TMapComponentMap(cmp).IOConnectionCount()-1 do
    begin
      if Label30.Caption <> '' then
      begin
        Label30.Caption := Label30.Caption + #13#10;
      end;
      Label30.Caption := Label30.Caption + Format('#%d'#9'C %d'#9'P %d', [i, TMapComponentMap(cmp).IOConnection(i).intCmp, TMapComponentMap(cmp).IOConnection(i).intPin]);
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

(* EVENTS *)

procedure TfMainForm.imgMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var sl: TStringList;
    s,ss: string;
    mcg: TMCGate;
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
          mcg := mcgNone;
          ss := '';
          if Length(sl.Text) >= 9 then
          begin
            ss := copy(sl.Text, 7, 3);
            if ss = '/BF' then
            begin
              mcg := mcgBuf;
            end
            else if ss = '/IN' then
            begin
              mcg := mcgInv;
            end
            else if ss = '/AD' then
            begin
              mcg := mcgAnd;
            end
            else if ss = '/ND' then
            begin
              mcg := mcgNand;
            end
            else if ss = '/OR' then
            begin
              mcg := mcgOr;
            end
            else if ss = '/NR' then
            begin
              mcg := mcgNor;
            end
            else if ss = '/XR' then
            begin
              mcg := mcgXor;
            end
            else if ss = '/XN' then
            begin
              mcg := mcgXnor;
            end
          end;
          map.AddGate(mcg, map.CursorPos);
        end
        else
        begin
          map.AddMap(MCSize(0,0), map.CursorPos, sl.Text);
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
  cmp := nil;
  Timer1Timer(nil);
  ListCustmaps();
end;

procedure TfMainForm.BitBtn2Click(Sender: TObject);
begin
  act := maSelect;
  Label1.Font.Color := clBlack;
  Label1.Caption := 'SELECT';
  imgMain.Cursor := crHandPoint;
end;

procedure TfMainForm.BitBtn1Click(Sender: TObject);
begin
  map.Tick();
  DrawMap(mdmsComplete);
end;

procedure TfMainForm.BitBtn11Click(Sender: TObject);
begin
  map.SetMCDef(0,Format('size="%d,%d"', [SpinEdit1.Value, SpinEdit2.Value]));
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

procedure TfMainForm.BitBtn14Click(Sender: TObject);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentMap)) then
  begin
    Exit;
  end;

  TMapComponentMap(cmp).AddIOConnection(ComboBox7.ItemIndex, ComboBox8.ItemIndex);
  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.BitBtn15Click(Sender: TObject);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentMap)) then
  begin
    Exit;
  end;

  TMapComponentMap(cmp).RemoveIOConnection(ComboBox7.ItemIndex, ComboBox8.ItemIndex);
  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
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
      map.SetMCDef(0, sl.Text);
      DrawMap(mdmsComplete);
    finally
      sl.Free;
    end;
  end;
end;

procedure TfMainForm.BitBtn4Click(Sender: TObject);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentWire)) then
  begin
    Exit;
  end;

  TMapComponentWire(cmp).Connect(ComboBox1.ItemIndex, ComboBox2.ItemIndex);
  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.BitBtn5Click(Sender: TObject);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentWire)) then
  begin
    Exit;
  end;

  TMapComponentWire(cmp).Disconnect(ComboBox1.ItemIndex, ComboBox2.ItemIndex);
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.BitBtn6Click(Sender: TObject);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentGate)) then
  begin
    Exit;
  end;

  TMapComponentGate(cmp).Gate := TMCGate(ComboBox3.ItemIndex);
  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
end;

procedure TfMainForm.BitBtn7Click(Sender: TObject);
begin
  if (not Assigned(cmp)) or (not (cmp is TMapComponentGate)) then
  begin
    Exit;
  end;

  TMapComponentGate(cmp).PinI1 := ComboBox4.ItemIndex;
  TMapComponentGate(cmp).PinI2 := ComboBox5.ItemIndex;
  TMapComponentGate(cmp).PinO1 := ComboBox6.ItemIndex;
  UpdateSelection();
  map.RedrawField := cmp.Pos;
  DrawMap(mdmsRedraw);
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

