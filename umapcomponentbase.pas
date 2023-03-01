unit umapcomponentbase;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, RegExpr, ulogger;

type
  TMCSize = record
    w: word;
    h: word;
  end;
  TMCPos = record
    x: smallint;
    y: smallint;
  end;
  TMCDrawStyle = (mdsNormal,mdsSelected,mdsInactive);

//  IMapComponentBase = interface
//  end;
  TMapComponentBase = class//(TInterfacedObject, IMapComponentBase)

    private

      var FName: string; //Component name
      var FSize: TMCSize; //Component size [Fields x Fields]
      var FPos: TMCPos; //Component position on parent's map [Fields x Fields]
      var FParent: TMapComponentBase; //Component's parent

      function GetName(): string;
      procedure SetName(AValue: string);
      function GetFullName(): string;
      function GetSize(): TMCSize;
      procedure SetSize(AValue: TMCSize);
      function GetPos(): TMCPos;
      procedure SetPos(AValue: TMCPos);
      function GetParent(): TMapComponentBase;
      procedure SetParent(AValue: TMapComponentBase);

    protected



    public

      property Name: string read GetName write SetName;
      property FullName: string read GetFullName;
      property Size: TMCSize read GetSize write SetSize;
      property Pos: TMCPos read GetPos write SetPos;
      property Parent: TMapComponentBase read GetParent write SetParent;
//      property MCDef: string read GetMCDef write SetMCDef;

      constructor Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); virtual;
      procedure Tick();
      procedure RequestTick(AFrom,ATo: TMCPos); virtual;
      procedure Rotate(AClockwise: boolean); virtual;
      function Contains(APos: TMCPos; ASize: TMCSize): boolean;
      function GetMCDef(ALevel: byte{; AIgnorePos: boolean = false}): string; virtual;
      procedure SetMCDef(ALevel: byte; const AValue: string); virtual;


      class procedure DrawEmpty(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);

  end;

function LevelStr(ALevel: byte): string;
function LevelPrefix(ALevel: byte): string;
function MCSize(AWidth: word = 0; AHeight: word = 0): TMCSize;
function MCPos(AX: smallint = -1; AY: smallint = -1): TMCPos;
procedure SetPen(ACanvas: TCanvas; AColor: TColor = clBlack; AWidth: integer = 1; AStyle: TPenStyle = psSolid);
procedure SetBrush(ACanvas: TCanvas; AColor: TColor = clBtnFace; AStyle: TBrushStyle = bsSolid);
procedure ClearPen(ACanvas: TCanvas);
procedure ClearBrush(ACanvas: TCanvas);
function FieldWithin(AField: TMCPos; APos: TMCPos; ASize: TMCSize): boolean;

operator = (s1,s2: TMCSize) b: boolean;
operator + (s1,s2: TMCSize) s: TMCSize;
operator - (s1,s2: TMCSize) s: TMCSize;
operator = (p1,p2: TMCPos) b: boolean;
operator + (p1,p2: TMCPos) p: TMCPos;
operator - (p1,p2: TMCPos) p: TMCPos;

implementation

(* GLOBAL *)

operator = (s1,s2: TMCSize) b: boolean;
begin
  result := (s1.w = s2.w) and (s1.h = s2.h);
end;

operator + (s1,s2: TMCSize) s: TMCSize;
begin
  result := MCSize(s1.w+s2.w, s1.h+s2.h);
end;

operator - (s1,s2: TMCSize) s: TMCSize;
begin
  result := MCSize(s1.w-s2.w, s1.h-s2.h);
end;

operator = (p1,p2: TMCPos) b: boolean;
begin
  result := (p1.x = p2.x) and (p1.y = p2.y);
end;

operator + (p1,p2: TMCPos) p: TMCPos;
begin
  result := MCPos(p1.x+p2.x, p1.y+p2.y);
end;

operator - (p1,p2: TMCPos) p: TMCPos;
begin
  result := MCPos(p1.x-p2.x, p1.y-p2.y);
end;

function LevelStr(ALevel: byte): string;
var i: integer;
begin
  result := '';
  if ALevel = 0 then
  begin
    Exit;
  end;
  for i := 1 to ALevel do
  begin
    result := result + ' ';
  end;
end;

function LevelPrefix(ALevel: byte): string;
begin
  result := '^';
  if ALevel > 0 then
  begin
    result := result + Format('\s{%d}', [ALevel]);
  end;
end;

function MCSize(AWidth: word = 0; AHeight: word = 0): TMCSize;
begin
  result.w := AWidth;
  result.h := AHeight;
end;

function MCPos(AX: smallint = -1; AY: smallint = -1): TMCPos;
begin
  result.x := AX;
  result.y := AY;
end;

procedure SetPen(ACanvas: TCanvas; AColor: TColor = clBlack; AWidth: integer = 1; AStyle: TPenStyle = psSolid);
begin
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := AStyle;
end;

procedure SetBrush(ACanvas: TCanvas; AColor: TColor = clBtnFace; AStyle: TBrushStyle = bsSolid);
begin
  ACanvas.Brush.Color := AColor;
  ACanvas.Brush.Style := AStyle;
end;

procedure ClearPen(ACanvas: TCanvas);
begin
  SetPen(ACanvas, clNone, 0, psClear);
end;

procedure ClearBrush(ACanvas: TCanvas);
begin
  SetBrush(ACanvas, clNone, bsClear);
end;

function FieldWithin(AField: TMCPos; APos: TMCPos; ASize: TMCSize): boolean;
begin
  result := (AField.x >= APos.x) and (AField.x < (APos.x+ASize.w)) and
            (AField.y >= APos.y) and (AField.y < (APos.y+ASize.h));
end;

(* PRIVATE *)

function TMapComponentBase.GetName(): string;
const METHOD: string = 'TMapComponentBase.GetName';
begin
  FLogger._s(METHOD);
  try

    result := FName;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentBase.SetName(AValue: string);
const METHOD: string = 'TMapComponentBase.SetName';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FName := AValue;

  finally
    FLogger._e();
  end;
end;

function TMapComponentBase.GetFullName(): string;
const METHOD: string = 'TMapComponentBase.GetFullName';
begin
  FLogger._s(METHOD);
  try

    result := FName;
    if Assigned(FParent) then
    begin
      result := FParent.FullName+'.'+result;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentBase.GetSize(): TMCSize;
const METHOD: string = 'TMapComponentBase.GetSize';
begin
  FLogger._s(METHOD);
  try

    result := FSize;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentBase.SetSize(AValue: TMCSize);
const METHOD: string = 'TMapComponentBase.SetSize';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FSize := AValue;

  finally
    FLogger._e();
  end;
end;

function TMapComponentBase.GetPos(): TMCPos;
const METHOD: string = 'TMapComponentBase.GetPos';
begin
  FLogger._s(METHOD);
  try

    result := FPos;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentBase.SetPos(AValue: TMCPos);
const METHOD: string = 'TMapComponentBase.SetPos';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FPos := AValue;

  finally
    FLogger._e();
  end;
end;

function TMapComponentBase.GetParent(): TMapComponentBase;
const METHOD: string = 'TMapComponentBase.GetParent';
begin
  FLogger._s(METHOD);
  try

    result := FParent;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentBase.SetParent(AValue: TMapComponentBase);
const METHOD: string = 'TMapComponentBase.SetParent';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FParent := AValue;

  finally
    FLogger._e();
  end;
end;

(* PROTECTED *)

function TMapComponentBase.GetMCDef(ALevel: byte{; AIgnorePos: boolean = false}): string;
const METHOD: string = 'TMapComponentBase.GetMCDef';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
//    FLogger._pe('AIgnorePos', TypeInfo(AIgnorePos), @AIgnorePos);
    FLogger._se();

    result := Format('%0:sname="%1:d:%2:s"'#13#10'%0:ssize="%3:d,%4:d"', [LevelStr(ALevel), Length(FName), FName, FSize.w, FSize.h]);
    if Assigned(FParent) then
//    if not AIgnorePos then
    begin
      result := result + Format(#13#10'%spos="%d,%d"', [LevelStr(ALevel), FPos.x, FPos.y]);
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentBase.SetMCDef(ALevel: byte; const AValue: string);
const METHOD: string = 'TMapComponentBase.SetMCDef';
var re: TRegExpr;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    if Length(AValue) = 0 then
    begin
      FLogger._(ltInfo, 'AValue is empty, exiting.');
      Exit;
    end;
    re := TRegExpr.Create();
    re.ModifierM:=true;
    try
      re.Expression := LevelPrefix(ALevel)+'name="(\d+):';
      if re.Exec(AValue) then
      begin
        FLogger._(ltInfo, 'Old name is "%s"' ,[FName]);
        FName := copy(AValue, re.MatchPos[0]+re.MatchLen[0], StrToIntDef(re.Match[1], 0));
        FLogger._(ltInfo, 'New name is "%s"' ,[FName]);
      end;

      re.Expression := LevelPrefix(ALevel)+'size="([^,]+),([^"]+)"';
      if re.Exec(AValue) then
      begin
        FLogger._(ltInfo, 'Old size is [%d;%d]' ,[FSize.w,FSize.h]);
        FSize := MCSize(StrToIntDef(re.Match[1], 0), StrToIntDef(re.Match[2], 0));
        FLogger._(ltInfo, 'New size is [%d;%d]' ,[FSize.w,FSize.h]);
      end;

      if Assigned(FParent) then
      begin
        re.Expression := LevelPrefix(ALevel)+'pos="([^,]+),([^"]+)"';
        if re.Exec(AValue) then
        begin
          FLogger._(ltInfo, 'Old pos is [%d;%d]' ,[FPos.x,FPos.y]);
          FPos := MCPos(StrToIntDef(re.Match[1], -1), StrToIntDef(re.Match[2], -1));
          FLogger._(ltInfo, 'New pos is [%d;%d]' ,[FPos.x,FPos.y]);
        end;
      end;
    finally
      re.Free();
    end;

  finally
    FLogger._e();
  end;
end;

(* PUBLIC *)

constructor TMapComponentBase.Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create;
  Clear;
  FName := AName;
  FSize := ASize;
  FPos := APos;
  FParent := AParent;
end;

destructor TMapComponentBase.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentBase.Clear();
const METHOD: string = 'TMapComponentBase.Clear';
begin
  FLogger._s(METHOD);
  try

    FName := '';
    FSize := MCSize();
    FPos := MCPos();
    FParent := nil;

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentBase.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentBase.Draw';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ACanvas', TypeInfo(ACanvas), @ACanvas);
    FLogger._pe('ARect', TypeInfo(ARect), @ARect);
    FLogger._pe('AStyle', TypeInfo(AStyle), @AStyle);
    FLogger._se();

    SetBrush(ACanvas, clWhite);
    case AStyle of
      mdsNormal: SetPen(ACanvas);
      mdsInactive: SetPen(ACanvas, clGray);
      mdsSelected:
      begin
        SetBrush(ACanvas, clRed);
        SetPen(ACanvas, clMaroon);
      end;
    end;
    ACanvas.Rectangle(ARect);

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentBase.Tick();
const METHOD: string = 'TMapComponentBase.Tick';
begin
  FLogger._s(METHOD);
  try

    //

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentBase.RequestTick(AFrom,ATo: TMCPos);
const METHOD: string = 'TMapComponentBase.RequestTick';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AFrom', TypeInfo(AFrom), @AFrom);
    FLogger._pe('ATo', TypeInfo(ATo), @ATo);
    FLogger._se();

    //

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentBase.Rotate(AClockwise: boolean);
const METHOD: string = 'TMapComponentBase.Rotate';
var tmp: word;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AClockwise', TypeInfo(AClockwise), @AClockwise);
    FLogger._se();

    tmp := FSize.w;
    FSize.w := FSize.h;
    FSize.h := tmp;

  finally
    FLogger._e();
  end;
end;

function TMapComponentBase.Contains(APos: TMCPos; ASize: TMCSize): boolean;
const METHOD: string = 'TMapComponentBase.Contains';
var rctA,rctB: TRect;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._pe('ASize', TypeInfo(ASize), @ASize);
    FLogger._se();

    result := false;

    if (APos = MCPos()) or (ASize = MCSize()) then
    begin
      FLogger._(ltWarning, 'Cannot check this');
      Exit;
    end;

    rctA := TRect.Create(TPoint.Create(FPos.x,FPos.y), FSize.w-1, FSize.h-1);
    rctB := TRect.Create(TPoint.Create(APos.x,APos.y), ASize.w-1, ASize.h-1);
    result := (rctA.Left <= rctB.Right) and (rctA.Right >= rctB.Left) and
              (rctA.Top <= rctB.Bottom) and (rctA.Bottom >= rctB.Top);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

class procedure TMapComponentBase.DrawEmpty(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
begin
  SetBrush(ACanvas);
  case AStyle of
    mdsNormal: SetPen(ACanvas);
    mdsInactive: SetPen(ACanvas, clGray);
    mdsSelected:
    begin
      SetBrush(ACanvas, clRed);
      SetPen(ACanvas, clMaroon);
    end;
  end;
  ACanvas.Rectangle(ARect);
end;

end.

