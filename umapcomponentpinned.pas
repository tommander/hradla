unit umapcomponentpinned;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, RegExpr, umapcomponentbase, ulogger;

type
  TMapComponentPin = record
    Active: boolean;
    Value: boolean;
  end;
  TMCPinType = (mptNone,mptLeft,mptBottom,mptRight,mptTop);
  TMapComponentPinned = class(TMapComponentBase)

    private

      var FPins: array of TMapComponentPin;

      function GetPinType(AIndex: integer): TMCPinType;
      function ReversePinType(APinType: TMCPinType): TMCPinType;
      function GetPinValue(AIndex: integer): boolean;
      procedure SetPinValue(AIndex: integer; AValue: boolean);
      function GetPinActive(AIndex: integer): boolean;
      procedure SetPinActive(AIndex: integer; AValue: boolean);

    protected


    public

      property PinValue[AIndex: integer]: boolean read GetPinValue write SetPinValue;
      property PinActive[AIndex: integer]: boolean read GetPinActive write SetPinActive;

      constructor Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Null();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); override;
      procedure DrawPinsOnly(ACanvas: TCanvas; ARect: TRect);
      procedure Tick();
      procedure Rotate(AClockwise: boolean); override;
      function GetMCDef(ALevel: byte; AIgnorePos: boolean = false): string; override;
      procedure SetMCDef(ALevel: byte; const AValue: string); override;
      function PinLow(): integer;
      function PinHigh(): integer;
      function PinCount(): integer;

      function GetPinIndexFromNeighbourField(APos: TMCPos): integer;
      function GetPinRect(AIndex: integer; ARect: TRect): TRect;
      function GetPinField(AIndex: integer): TMCPos;
      function GetPinNeighbourField(AIndex: integer): TMCPos;

  end;


implementation

(* PRIVATE *)

function TMapComponentPinned.GetPinType(AIndex: integer): TMCPinType;
const METHOD: string = 'TMapComponentPinned.GetPinType';
begin
  FLogger._s(METHOD);

  result := mptNone;
  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) then
  begin
    FLogger._e();
    Exit;
  end;

  if (AIndex < Size.h) then
  begin
    result := mptLeft;
    FLogger._e();
    Exit;
  end;
  if (AIndex < (Size.h+Size.w)) then
  begin
    result := mptBottom;
    FLogger._e();
    Exit;
  end;
  if (AIndex < (Size.h+Size.w+Size.h)) then
  begin
    result := mptRight;
    FLogger._e();
    Exit;
  end;
  result := mptTop;
  FLogger._e();
end;

function TMapComponentPinned.ReversePinType(APinType: TMCPinType): TMCPinType;
const METHOD: string = 'TMapComponentPinned.ReversePinType';
begin
  FLogger._s(METHOD);

  result := mptNone;
  case APinType of
    mptLeft: result := mptRight;
    mptBottom: result := mptTop;
    mptRight: result := mptLeft;
    mptTop: result := mptBottom;
  end;

  FLogger._e();
end;

function TMapComponentPinned.GetPinValue(AIndex: integer): boolean;
const METHOD: string = 'TMapComponentPinned.GetPinValue';
begin
  FLogger._s(METHOD);

  result := false;
  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) or (not FPins[AIndex].Active) then
  begin
    FLogger._e(result);
    Exit;
  end;
  result := FPins[AIndex].Value;

  FLogger._e(result);
end;

procedure TMapComponentPinned.SetPinValue(AIndex: integer; AValue: boolean);
const METHOD: string = 'TMapComponentPinned.SetPinValue';
begin
  FLogger._s(METHOD);

  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) or (not FPins[AIndex].Active) then
  begin
    FLogger._e();
    Exit;
  end;
  FPins[AIndex].Value := AValue;

  FLogger._e();
end;

function TMapComponentPinned.GetPinActive(AIndex: integer): boolean;
const METHOD: string = 'TMapComponentPinned.GetPinActive';
begin
  FLogger._s(METHOD);

  result := false;
  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) then
  begin
    FLogger._e(result);
    Exit;
  end;
  result := FPins[AIndex].Active;

  FLogger._e(result);
end;

procedure TMapComponentPinned.SetPinActive(AIndex: integer; AValue: boolean);
const METHOD: string = 'TMapComponentPinned.SetPinActive';
begin
  FLogger._s(METHOD);

  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) then
  begin
    Exit;
  end;
  FPins[AIndex].Active := AValue;

  FLogger._e();
end;

(* PROTECTED *)

function TMapComponentPinned.GetMCDef(ALevel: byte; AIgnorePos: boolean = false): string;
const METHOD: string = 'TMapComponentPinned.GetMCDef';
var i: integer;
begin
  FLogger._s(METHOD);

  result := inherited GetMCDef(ALevel, AIgnorePos);
//  result := result + Format(#13#10'pins="%d"', [Length(FPins)]);
  for i := Low(FPins) to High(FPins) do
  begin
    if FPins[i].Active then
    begin
      result := result + Format(#13#10'%spin="%d,%d,%d"', [LevelStr(ALevel), i, Integer(FPins[i].Active), Integer(FPins[i].Value)]);
    end;
  end;

  FLogger._e();
end;

procedure TMapComponentPinned.SetMCDef(ALevel: byte; const AValue: string);
const METHOD: string = 'TMapComponentPinned.SetMCDef';
var re: TRegExpr;
    i,ii: integer;
    iW,iH: integer;
begin
  FLogger._s(METHOD);

  iW := Size.w;
  iH := Size.h;

  inherited SetMCDef(ALevel, AValue);

  if (iW <> Size.w) or (iH <> Size.h) then
  begin
    SetLength(FPins, 2*(Size.w+Size.h));
    for i := Low(FPins) to High(FPins) do
    begin
      FPins[i].Active := false;
      FPins[i].Value := false;
    end;
  end;

  re := TRegExpr.Create();
  re.ModifierM := true;
  try
    re.Expression := LevelPrefix(ALevel)+'pin="([^,]+),([^,]+),([^"]+)"';
    if re.Exec(AValue) then
    begin
      repeat
        i := StrToIntDef(re.Match[1], 0);
        if (i >= Low(FPins)) and (i <= High(FPins)) then
        begin
          FPins[i].Active := re.Match[2] = '1';
          if FPins[i].Active then
          begin
            FPins[i].Value := re.Match[3] = '1';
          end;
        end;
      until
        not re.ExecNext;
    end;
  finally
    re.Free;
  end;

  FLogger._e();
end;

(* PUBLIC *)

constructor TMapComponentPinned.Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
var i: integer;
begin
  inherited Create(AName, ASize, APos, AParent);
  Clear;
  SetLength(FPins, 2 * (ASize.w + ASize.h));
  for i := Low(FPins) to High(FPins) do
  begin
    FPins[i].Active := false;
    FPins[i].Value := false;
  end;
end;

destructor TMapComponentPinned.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentPinned.Clear();
const METHOD: string = 'TMapComponentPinned.Clear';
begin
  FLogger._s(METHOD);

  SetLength(FPins, 0);

  FLogger._e();
end;

procedure TMapComponentPinned.Null();
const METHOD: string = 'TMapComponentPinned.Null';
var i: integer;
begin
  FLogger._s(METHOD);

  for i := Low(FPins) to High(FPins) do
  begin
    FPins[i].Value := false;
  end;

  FLogger._e();
end;

procedure TMapComponentPinned.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentPinned.Draw';
var i: integer;
begin
  FLogger._s(METHOD);

  inherited Draw(ACanvas, ARect, AStyle);
  DrawPinsOnly(ACanvas, ARect);

  FLogger._e();
end;

procedure TMapComponentPinned.DrawPinsOnly(ACanvas: TCanvas; ARect: TRect);
const METHOD: string = 'TMapComponentPinned.DrawPinsOnly';
var i: integer;
begin
  FLogger._s(METHOD);

  for i := Low(FPins) to High(FPins) do
  begin
    if FPins[i].Active then
    begin
      SetPen(ACanvas);
      if FPins[i].Value then
      begin
        SetBrush(ACanvas, clRed);
      end
      else
      begin
        SetBrush(ACanvas, clMaroon);
      end;
      ACanvas.Ellipse(GetPinRect(i, ARect));
    end;
  end;

  FLogger._e();
end;
procedure TMapComponentPinned.Tick();
const METHOD: string = 'TMapComponentPinned.Tick';
begin
  FLogger._s(METHOD);

  //

  FLogger._e();
end;

procedure TMapComponentPinned.Rotate(AClockwise: boolean);
const METHOD: string = 'TMapComponentPinned.Rotate';
var i,ii: integer;
    tmp: TMapComponentPin;
begin
  FLogger._s(METHOD);

  inherited Rotate(AClockwise);
  i := 0;
  repeat
    FLogger._(ltDebug, 'Moving from %d', [i]);
    tmp := FPins[i];
    ii := i;
    Dec(ii, Size.w);
    if ii < 0 then
    begin
      Inc(ii, Length(FPins));
    end;
    FPins[i] := FPins[ii];
    i := ii;
    FLogger._(ltDebug, 'Moving to %d', [i]);
  until
    i = 0;

  FLogger._e();
end;

function TMapComponentPinned.PinLow(): integer;
const METHOD: string = 'TMapComponentPinned.PinLow';
begin
  FLogger._s(METHOD);

  result := Low(FPins);

  FLogger._e(result);
end;

function TMapComponentPinned.PinHigh(): integer;
const METHOD: string = 'TMapComponentPinned.PinHigh';
begin
  FLogger._s(METHOD);

  result := High(FPins);

  FLogger._e(result);
end;

function TMapComponentPinned.PinCount(): integer;
const METHOD: string = 'TMapComponentPinned.PinCount';
begin
  FLogger._s(METHOD);

  result := Length(FPins);

  FLogger._e();
end;

function TMapComponentPinned.GetPinIndexFromNeighbourField(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentPinned.GetPinIndexFromNeighbourField';
var i,intIdx: integer;
begin
  FLogger._s(METHOD);

  result := -1;

  //Not-a-neighbour
  if (APos.x < (Pos.x-1)) or (APos.x > (Pos.x+Size.w)) or (APos.y < (Pos.y-1)) or (APos.y > (Pos.y+Size.h)) then
  begin
    FLogger._(ltWarning, 'Not a neighbour');
    FLogger._e(result);
    Exit;
  end;

  //Left
  if (APos.x = (Pos.x-1)) and (APos.y >= Pos.y) and (APos.y < (Pos.y+Size.h)) then
  begin
    result := APos.y-Pos.y;
  end;

  //Bottom
  if (APos.y = (Pos.y+Size.h)) and (APos.x >= Pos.x) and (APos.y < (Pos.x+Size.w)) then
  begin
    result := Size.h+(APos.x-Pos.x);
  end;

  //Right
  if (APos.x = (Pos.x+Size.w)) and (APos.y >= Pos.y) and (APos.y < (Pos.y+Size.h)) then
  begin
    result := (Size.h+Size.w)+(Pos.y+Size.h-1-APos.y);
  end;

  //Top
  if (APos.y = (Pos.y-1)) and (APos.x >= Pos.x) and (APos.y < (Pos.x+Size.w)) then
  begin
    result := (Size.h+Size.w+Size.h)+(Pos.x+Size.w-1-APos.x);
  end;

  FLogger._e(result);
end;

function TMapComponentPinned.GetPinRect(AIndex: integer; ARect: TRect): TRect;
const METHOD: string = 'TMapComponentPinned.GetPinRect';
const PIN_SIZE: word = 8;
var sglMyFieldW,sglMyFieldH: single;
    intLeft,intTop: integer;
    mptType: TMCPinType;
begin
  FLogger._s(METHOD);

  sglMyFieldW := ARect.Width / Size.w;
  sglMyFieldH := ARect.Height / Size.h;

  result := TRect.Empty;
  mptType := GetPinType(AIndex);
  intLeft := 0;
  intTop := 0;
  case mptType of
    mptLeft:
    begin
      intLeft := ARect.Left;
      intTop := ARect.Top+Round((sglMyFieldH/2)+(sglMyFieldH*AIndex))-(PIN_SIZE div 2);
    end;
    mptBottom:
    begin
      intLeft := ARect.Left+Round((sglMyFieldW/2)+(sglMyFieldW*(AIndex-Size.h)))-(PIN_SIZE div 2);
      intTop := ARect.Bottom-PIN_SIZE;
    end;
    mptRight:
    begin
      intLeft := ARect.Right-PIN_SIZE;
      intTop := ARect.Bottom-Round((sglMyFieldH/2)+(sglMyFieldH*(AIndex-Size.h-Size.w)))-(PIN_SIZE div 2);
    end;
    mptTop:
    begin
      intLeft := ARect.Right-Round((sglMyFieldW/2)+(sglMyFieldW*(AIndex-Size.h-Size.w-Size.h)))-(PIN_SIZE div 2);
      intTop := ARect.Top;
    end;
  end;

  result.Create(intLeft, intTop, intLeft+PIN_SIZE, intTop+PIN_SIZE);

  FLogger._e();
end;

function TMapComponentPinned.GetPinField(AIndex: integer): TMCPos;
const METHOD: string = 'TMapComponentPinned.GetPinField';
var mptType: TMCPinType;
begin
  FLogger._s(METHOD);

  result := MCPos();
  mptType := GetPinType(AIndex);
  case mptType of
    mptLeft: result := MCPos(Pos.x, Pos.y+AIndex);
    mptBottom: result := MCPos(Pos.x+AIndex-Size.h, Pos.y+Size.h-1);
    mptRight: result := MCPos(Pos.x+Size.w-1, Pos.y+(Size.h+Size.w+Size.h-1-AIndex));
    mptTop: result := MCPos(Pos.x+(Size.h+Size.w+Size.h+Size.w-1-AIndex),Pos.y);
  end;

  FLogger._e();
end;

function TMapComponentPinned.GetPinNeighbourField(AIndex: integer): TMCPos;
const METHOD: string = 'TMapComponentPinned.GetPinNeighbourField';
var mptType: TMCPinType;
begin
  FLogger._s(METHOD);

  result := MCPos();
  mptType := GetPinType(AIndex);
  case mptType of
    mptLeft: result := MCPos(Pos.x-1, Pos.y+AIndex);
    mptBottom: result := MCPos(Pos.x+AIndex-Size.h, Pos.y+Size.h);
    mptRight: result := MCPos(Pos.x+Size.w, Pos.y+(Size.h+Size.w+Size.h-1-AIndex));
    mptTop: result := MCPos(Pos.x+(Size.h+Size.w+Size.h+Size.w-1-AIndex),Pos.y-1);
  end;

  FLogger._e();
end;

end.

