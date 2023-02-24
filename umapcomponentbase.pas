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
      function GetFullName(): string;
      function GetSize(): TMCSize;
      function GetPos(): TMCPos;
      function GetParent(): TMapComponentBase;

    protected

      var FLogger: TLogger;

      function GetMCDef(): string; virtual;
      procedure SetMCDef(const AValue: string); virtual;

    public

      property Name: string read GetName;
      property FullName: string read GetFullName;
      property Size: TMCSize read GetSize;
      property Pos: TMCPos read GetPos;
      property Parent: TMapComponentBase read GetParent;
      property MCDef: string read GetMCDef write SetMCDef;

      constructor Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); virtual;
      procedure Tick();
      procedure RequestTick(AFrom,ATo: TMCPos); virtual;
      procedure Rotate(AClockwise: boolean); virtual;
      function Contains(APos: TMCPos; ASize: TMCSize): boolean;

      class procedure DrawEmpty(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);

  end;

function MCSize(AWidth: word = 0; AHeight: word = 0): TMCSize;
function MCPos(AX: smallint = -1; AY: smallint = -1): TMCPos;
procedure SetPen(ACanvas: TCanvas; AColor: TColor = clBlack; AWidth: integer = 1; AStyle: TPenStyle = psSolid);
procedure SetBrush(ACanvas: TCanvas; AColor: TColor = clBtnFace; AStyle: TBrushStyle = bsSolid);
procedure ClearPen(ACanvas: TCanvas);
procedure ClearBrush(ACanvas: TCanvas);
function FieldWithin(AField: TMCPos; APos: TMCPos; ASize: TMCSize): boolean;

operator = (s1,s2: TMCSize) b: boolean;
operator = (p1,p2: TMCPos) b: boolean;

implementation

(* GLOBAL *)

operator = (s1,s2: TMCSize) b: boolean;
begin
  result := (s1.w = s2.w) and (s1.h = s2.h);
end;

operator = (p1,p2: TMCPos) b: boolean;
begin
  result := (p1.x = p2.x) and (p1.y = p2.y);
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

  result := FName;

  FLogger._e(result);
end;

function TMapComponentBase.GetFullName(): string;
const METHOD: string = 'TMapComponentBase.GetFullName';
begin
  FLogger._s(METHOD);

  result := FName;
  if Assigned(FParent) then
  begin
    result := FParent.FullName+'.'+result;
  end;

  FLogger._e(result);
end;

function TMapComponentBase.GetSize(): TMCSize;
const METHOD: string = 'TMapComponentBase.GetSize';
begin
  FLogger._s(METHOD);

  result := FSize;

  FLogger._e();
end;

function TMapComponentBase.GetPos(): TMCPos;
const METHOD: string = 'TMapComponentBase.GetPos';
begin
  FLogger._s(METHOD);

  result := FPos;

  FLogger._e();
end;

function TMapComponentBase.GetParent(): TMapComponentBase;
const METHOD: string = 'TMapComponentBase.GetParent';
begin
  FLogger._s(METHOD);

  result := FParent;

  FLogger._e();
end;

(* PROTECTED *)

function TMapComponentBase.GetMCDef(): string;
const METHOD: string = 'TMapComponentBase.GetMCDef';
begin
  FLogger._s(METHOD);

  result := Format('name="%d:%s";size="%d,%d";pos="%d,%d"', [Length(FName), FName, FSize.w, FSize.h, FPos.x, FPos.y]);

  FLogger._e();
end;

procedure TMapComponentBase.SetMCDef(const AValue: string);
const METHOD: string = 'TMapComponentBase.SetMCDef';
var re: TRegExpr;
begin
  FLogger._s(METHOD);

  if Length(AValue) = 0 then
  begin
    FLogger._e();
    Exit;
  end;
  re := TRegExpr.Create();
  try
    re.Expression := 'name="(\d+):';
    if re.Exec(AValue) then
    begin
      FName := copy(AValue, re.MatchPos[1]+re.MatchLen[1], StrToIntDef(re.Match[1], 0));
    end;

    re.Expression := 'size="(\d+),(\d+)"';
    if re.Exec(AValue) then
    begin
      FSize := MCSize(StrToIntDef(re.Match[1], 0), StrToIntDef(re.Match[2], 0));
    end;

    re.Expression := 'pos="(\d+),(\d+)"';
    if re.Exec(AValue) then
    begin
      FPos := MCPos(StrToIntDef(re.Match[1], -1), StrToIntDef(re.Match[2], -1));
    end;
  finally
    re.Free();
  end;

  FLogger._e();
end;

(* PUBLIC *)

constructor TMapComponentBase.Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create;
  FLogger := TLogger.Create('/home/tommander/Programming.Pascal/hradla/log.txt', ltDebug);
  FLogger._(ltInfo, 'Start of log');
  Clear;
  FName := AName;
  FSize := ASize;
  FPos := APos;
  FParent := AParent;
end;

destructor TMapComponentBase.Destroy();
begin
  Clear;
  FLogger._(ltInfo, 'End of log');
  if Assigned(FLogger) then
  begin
    FLogger.Free;
  end;
  inherited Destroy;
end;

procedure TMapComponentBase.Clear();
const METHOD: string = 'TMapComponentBase.Clear';
begin
  FLogger._s(METHOD);

  FName := '';
  FSize := MCSize();
  FPos := MCPos();
  FParent := nil;

  FLogger._e();
end;

procedure TMapComponentBase.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentBase.Draw';
begin
  FLogger._s(METHOD);

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

  FLogger._e();
end;

procedure TMapComponentBase.Tick();
const METHOD: string = 'TMapComponentBase.Tick';
begin
  FLogger._s(METHOD);
  //
  FLogger._e();
end;

procedure TMapComponentBase.RequestTick(AFrom,ATo: TMCPos);
const METHOD: string = 'TMapComponentBase.RequestTick';
begin
  FLogger._s(METHOD);
  //
  FLogger._e();
end;

procedure TMapComponentBase.Rotate(AClockwise: boolean);
const METHOD: string = 'TMapComponentBase.Rotate';
var tmp: word;
begin
  FLogger._s(METHOD);
  FLogger._p('AClockwise', AClockwise);

  tmp := FSize.w;
  FSize.w := FSize.h;
  FSize.h := tmp;

  FLogger._e();
end;

function TMapComponentBase.Contains(APos: TMCPos; ASize: TMCSize): boolean;
const METHOD: string = 'TMapComponentBase.Contains';
var rctA,rctB: TRect;
begin
  FLogger._s(METHOD);

  rctA := TRect.Create(TPoint.Create(FPos.x,FPos.y), FSize.w-1, FSize.h-1);
  rctB := TRect.Create(TPoint.Create(APos.x,APos.y), ASize.w-1, ASize.h-1);
  result := (rctA.Left <= rctB.Right) and (rctA.Right >= rctB.Left) and
            (rctA.Top <= rctB.Bottom) and (rctA.Bottom >= rctB.Top);

  FLogger._e(result);
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

