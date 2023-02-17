unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, Math, umapcomponent;

const
  intMapWidth: word = 20;
  intMapHeight: word = 10;

type
{  TMapField = record
    FComponent: integer;
  end;}

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Image1: TImage;
    ListView1: TListView;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    rbSelect: TRadioButton;
    rbAdd: TRadioButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    Timer1: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Image1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    //procedure DrawMap(const boolComplete: boolean = false);
    //procedure DrawField(const fX,fY: integer; const boolSelected: boolean);
    //procedure XYToField(const X,Y: integer; var fX,fY: integer);
//    function FieldBounds(const fX,fY: integer): TRect;
//    function ComponentBounds(const ptrCmp: PMapComponent; const fX,fY: integer): TRect;
    procedure _(const strText: string; const boolTime: boolean = true);
    procedure _(const strText: string; arrParams: array of const; const boolTime: boolean = true);
//    function InitMapField(): TMapField;
//    function GetMapField(const fX,fY: integer): TMapField;
    //function GetMapComponent(const fX,fY: integer): PMapComponent;
//    function ComponentFromString(const strDef: string): integer;
//    function PlaceComponent(ptrCmp: PMapComponent;intX,intY: integer): boolean;
    procedure ComponentList(var lv: TListView);
    procedure UpdateSelectedComponent();
  public

  end;

var
  Form1: TForm1;
  cmpRoot: TMapComponent;
  //arrMap: array of array of PMapComponent;
  //arrComponents: array of TMapComponent;
  //sglFieldWidth: single;
  //sglFieldHeight: single;
  intFieldLMMX: integer; //Last MouseMove
  intFieldLMMY: integer;
  intFieldMMX: integer; //Current MouseMove
  intFieldMMY: integer;
  intFieldLCmp: integer; //Last selected component from ListView
  intFieldLCmpX: integer; //Width of ^
  intFieldLCmpY: integer; //Height of ^
  ptrSelCmp: PMapComponent; //Selected component on the map

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ComponentList(var lv: TListView);
var li: TListItem;
    mct: TMapComponentType;
begin
  lv.Items.Clear;

  for mct in TMapComponentType do
  begin
    li := lv.Items.Add;
    li.Caption := TMapComponent.MCTToStr(mct);
    li.ImageIndex := -1;
  end;
end;

procedure TForm1._(const strText: string; const boolTime: boolean = true);
begin
  if boolTime then
  begin
    Memo1.Lines.Add('[%s] %s', [TimeToStr(now), strText]);
    Exit;
  end;
  Memo1.Lines.Add(strText);
end;

procedure TForm1._(const strText: string; arrParams: array of const; const boolTime: boolean = true);
begin
  _(Format(strText, arrParams), boolTime);
end;



{function TForm1.InitMapField(): TMapField;
begin
  result.FComponent := -1;
end;}

{function TForm1.GetMapField(const fX,fY: integer): TMapField;}
{function TForm1.GetMapComponent(const fX,fY: integer): PMapComponent;
begin
  result := nil;
  if (fX < Low(arrMap)) or (fX > High(arrMap)) then
  begin
    Exit;
  end;

  if (fY < Low(arrMap[fX])) or (fY > High(arrMap[fX])) then
  begin
    Exit;
  end;

  result := arrMap[fX][fY];
end;}

{function TForm1.GetMapComponent(const fX,fY: integer): PMapComponent;
var fld: TMapField;
begin
  result := nil;
  fld := GetMapField(fX,fY);
  if (fld.FComponent < Low(arrComponents)) or (fld.FComponent > High(arrComponents)) then
  begin
    Exit;
  end;
  if not arrComponents[fld.FComponent].Active then
  begin
    Exit;
  end;

  result := @arrComponents[fld.FComponent];
end;}

{function TForm1.FieldBounds(const fX,fY: integer): TRect;
begin
  result.Create(Round(fX*sglFieldWidth),Round(fY*sglFieldHeight),Round((fX+1)*sglFieldWidth),Round((fY+1)*sglFieldHeight));
end;

function TForm1.ComponentBounds(const ptrCmp: PMapComponent; const fX,fY: integer): TRect;
var x,y: integer;
    p: PMapComponent;
begin
  if ptrCmp = nil then
  begin
    result := FieldBounds(fX,fY);
    Exit;
  end;
  x := fX;
  y := fY;
  repeat
    Dec(x);
    p := GetMapComponent(x,y);
  until
    (x < Low(arrMap)) or (p <> ptrCmp);
  Inc(x);
  repeat
    Dec(y);
    p := GetMapComponent(x,y);
  until
    (y < Low(arrMap[x])) or (p <> ptrCmp);
  Inc(y);

  result.Create(Round(x*sglFieldWidth),Round(y*sglFieldHeight),Round((x+ptrCmp^.Width)*sglFieldWidth),Round((y+ptrCmp^.Height)*sglFieldHeight));
end; }


procedure TForm1.FormCreate(Sender: TObject);
var x,y: integer;
begin
  cmpRoot := TMapComponent.Create(mctMap, 0, 0);
  cmpRoot.SetMap(Format('w=%d;h=%d', [intMapWidth,intMapHeight]));
  //SetLength(arrComponents, 0);
  //SetLength(arrMap, intMapWidth);
  //for x := Low(arrMap) to High(arrMap) do
  //begin
  //  SetLength(arrMap[x], intMapHeight);
  //  for y := Low(arrMap[x]) to High(arrMap[x]) do
  //  begin
  //    arrMap[x][y] := nil;
  //  end;
  //end;
  //sglFieldWidth := 0;
  //sglFieldHeight := 0;
  intFieldLMMX := -1;
  intFieldLMMY := -1;
  intFieldMMX := -1;
  intFieldMMY := -1;
  intFieldLCmp := -1;
  intFieldLCmpX := -1;
  intFieldLCmpY := -1;
  ptrSelCmp := nil;
  ComponentList(ListView1);
  cmpRoot.Draw(Image1.Canvas, TRect.Empty, [mcdfComplete]);
  //DrawMap(true);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  cmpRoot.Free;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
  if ptrSelCmp = nil then
  begin
    Exit;
  end;

  ptrSelCmp^.Active := false;
  cmpRoot.Draw(Image1.Canvas, TRect.Empty, [mcdfComplete]);
end;

procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  cmpRoot.XYToField(X,Y,intFieldMMX,intFieldMMY);
  //Label1.Caption := Format('[%d;%d]', [intFieldMMX, intFieldMMY]);
  cmpRoot.FieldMouseOver := TPoint.Create(intFieldMMX,intFieldMMY);
  cmpRoot.Draw(Image1.Canvas, TRect.Empty, []);
end;

{function TForm1.ComponentFromString(const strDef: string): integer;
var intWidth,intHeight,intColor: integer;
    strName: string;
begin
  result := -1;
  if (Length(strDef) < 17) then
  begin
    Exit;
  end;

  intWidth := StrToIntDef(copy(strDef,1,3), 0);
  if (intWidth > 255) then
  begin
    Exit;
  end;
  intHeight := StrToIntDef(copy(strDef,4,3), 0);
  if (intHeight > 255) then
  begin
    Exit;
  end;
  intColor := StrToIntDef(copy(strDef,7,10), 0);
  strName := copy(strDef,17,Length(strDef)-16);

  SetLength(arrComponents, Length(arrComponents)+1);
  result := High(arrComponents);
  arrComponents[result] := TMapComponent.Create(strName, intWidth, intHeight, intColor);
end;}

(*function TForm1.PlaceComponent(ptrCmp: PMapComponent;intX,intY: integer): boolean;
var x,y: integer;
    b: boolean;
begin
  result := false;

  b := true;
  for x := 0 to ptrCmp^.Width-1 do
  begin
    for y := 0 to ptrCmp^.Height-1 do
    begin
      if ((intX+x) < Low(arrMap)) or ((intX+x) > High(arrMap)) or ((intY+y) < Low(arrMap[intX+x])) or ((intY+y) > High(arrMap[intX+x])) then
      begin
        b := false;
        break;
      end;
      if not Assigned(arrMap[intX+x][intY+y]) then
      begin
        continue;
      end;
      if not arrMap[intX+x][intY+y]^.Active then
      begin
        continue;
      end;
      b := false;
      break;
    end;
  end;

  if not b then
  begin
    Exit;
  end;

  for x := 0 to ptrCmp^.Width-1 do
  begin
    for y := 0 to ptrCmp^.Height-1 do
    begin
      arrMap[intX+x][intY+y] := ptrCmp;
    end;
  end;

  result := true;
end;  *)

procedure TForm1.UpdateSelectedComponent();
begin
  Memo1.Lines.Clear;
  if (ptrSelCmp = nil) or (not ptrSelCmp^.Active) then
  begin
    Panel5.Enabled:=false;
    Memo1.Lines.Add('No component');
    Exit;
  end;
  Panel5.Enabled:=true;
  Memo1.Lines.Add('Component');
  Memo1.Lines.Add('Type "%s"', [TMapComponent.MCTToStr(ptrSelCmp^.MCType)]);
  Memo1.Lines.Add('Size %dx%d', [ptrSelCmp^.Width, ptrSelCmp^.Height]);
end;

procedure TForm1.Image1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var fX,fY: integer;
    intCmp: integer;
begin
  if rbSelect.Checked then
  begin
    cmpRoot.XYToField(X,Y,fX,fY);
    ptrSelCmp := cmpRoot.GetSubcomponentPtr(fX,fY);
    UpdateSelectedComponent();
    Exit;
  end;

  if rbAdd.Checked then
  begin
    if not Assigned(ListView1.Selected) then
    begin
      ShowMessage('No component selected or missing definition');
      Exit;
    end;

    cmpRoot.XYToField(X,Y,fX,fY);
    cmpRoot.CreateSubcomponent(TMapComponentType(ListView1.Selected.Index), fX, fY);
//    SetLength(arrComponents, Length(arrComponents)+1);
//    intCmp := ;
//    arrComponents[High(arrComponents)] := TMapComponent.Create();
//    if arrComponents[High(arrComponents)] = nil then
//    begin
//      ShowMessage('Could not create component instance');
//      Exit;
//    end;

    //if not PlaceComponent(@arrComponents[High(arrComponents)],fX,fY) then
    //begin
    //  ShowMessage('Could not place component on map');
    //  Exit;
    //end;

    cmpRoot.Draw(Image1.Canvas, TRect.Empty, [mcdfComplete]);
    Exit;
  end;
end;

procedure TForm1.Image1Resize(Sender: TObject);
begin
  Timer1.Enabled := false;
  Timer1.Enabled := true;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Image1.Picture.Bitmap.SetSize(Image1.Width, Image1.Height);
  cmpRoot.Draw(Image1.Canvas, TRect.Empty, [mcdfComplete]);
  Timer1.Enabled := false;
end;

end.

