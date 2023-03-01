unit umapcomponentgate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, RegExpr, ulogger, umapcomponentpinned, umapcomponentbase;

type
  TMCGate = (mcgNone, mcgBuf, mcgInv, mcgAnd, mcgNand, mcgOr, mcgNor, mcgXor, mcgXnor);
  TMapComponentGate = class(TMapComponentPinned)

    private

      var FGate: TMCGate;
      var FPinI1: integer;
      var FPinI2: integer;
      var FPinO1: integer;

      function GetGate(): TMCGate;
      procedure SetGate(AValue: TMCGate);
      function GetPinI1(): integer;
      procedure SetPinI1(AValue: integer);
      function GetPinI2(): integer;
      procedure SetPinI2(AValue: integer);
      function GetPinO1(): integer;
      procedure SetPinO1(AValue: integer);

      procedure UpdatePinActive();

    protected


    public

      property Gate: TMCGate read GetGate write SetGate;
      property PinI1: integer read GetPinI1 write SetPinI1;
      property PinI2: integer read GetPinI2 write SetPinI2;
      property PinO1: integer read GetPinO1 write SetPinO1;

      constructor Create(AGate: TMCGate; const AName: string; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); override;
      procedure Tick();
      function GetMCDef(ALevel: byte{; AIgnorePos: boolean = false}): string; override;
      procedure SetMCDef(ALevel: byte; const AValue: string); override;
//      procedure SetPinTypes(APinI1,APinI2,APinO1: integer);

  end;

function MCGateToStr(AMCG: TMCGate): string;

implementation

(* GLOBAL *)

function MCGateToStr(AMCG: TMCGate): string;
begin
  result := 'none';
  case AMCG of
    mcgBuf: result := 'Buffer';
    mcgInv: result := 'Inverter';
    mcgAnd: result := 'And';
    mcgNand: result := 'Nand';
    mcgOr: result := 'Or';
    mcgNor: result := 'Nor';
    mcgXor: result := 'Xor';
    mcgXnor: result := 'Xnor';
  end;
end;

(* PRIVATE *)

function TMapComponentGate.GetGate(): TMCGate;
const METHOD: string = 'TMapComponentGate.GetGate';
begin
  FLogger._s(METHOD);
  try

    result := FGate;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentGate.SetGate(AValue: TMCGate);
const METHOD: string = 'TMapComponentGate.SetGate';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FGate := AValue;

  finally
    FLogger._e();
  end;
end;

function TMapComponentGate.GetPinI1(): integer;
const METHOD: string = 'TMapComponentGate.GetPinI1';
begin
  FLogger._s(METHOD);
  try

    result := FPinI1;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentGate.SetPinI1(AValue: integer);
const METHOD: string = 'TMapComponentGate.SetPinI1';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FPinI1 := AValue;
    UpdatePinActive();

  finally
    FLogger._e();
  end;
end;

function TMapComponentGate.GetPinI2(): integer;
const METHOD: string = 'TMapComponentGate.GetPinI2';
begin
  FLogger._s(METHOD);
  try

    result := FPinI2;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentGate.SetPinI2(AValue: integer);
const METHOD: string = 'TMapComponentGate.SetPinI2';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FPinI2 := AValue;
    UpdatePinActive();

  finally
    FLogger._e();
  end;
end;

function TMapComponentGate.GetPinO1(): integer;
const METHOD: string = 'TMapComponentGate.GetPinO1';
begin
  FLogger._s(METHOD);
  try

    result := FPinO1;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentGate.SetPinO1(AValue: integer);
const METHOD: string = 'TMapComponentGate.SetPinO1';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FPinO1 := AValue;
    UpdatePinActive();

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentGate.UpdatePinActive();
const METHOD: string = 'TMapComponentGate.Clear';
var i: integer;
begin
  FLogger._s(METHOD);
  try

    for i := PinLow() to PinHigh() do
    begin
      PinActive[i] := false;
    end;
    PinActive[FPinI1] := true;
    PinActive[FPinI2] := true;
    PinActive[FPinO1] := true;

  finally
    FLogger._e();
  end;
end;

(* PROTECTED *)

function TMapComponentGate.GetMCDef(ALevel: byte{; AIgnorePos: boolean = false}): string;
const METHOD: string = 'TMapComponentGate.GetMCDef';
var i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
//    FLogger._pe('AIgnorePos', TypeInfo(AIgnorePos), @AIgnorePos);
    FLogger._se();

    result := inherited GetMCDef(ALevel{, AIgnorePos});
    result := result + Format(#13#10'%0:sgate="%1:d"'#13#10'%0:spini1="%2:d"'#13#10'%0:spini2="%3:d"'#13#10'%0:spino1="%4:d"', [LevelStr(ALevel),Integer(FGate), FPinI1, FPinI2, FPinO1]);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentGate.SetMCDef(ALevel: byte; const AValue: string);
const METHOD: string = 'TMapComponentGate.SetMCDef';
var re: TRegExpr;
    i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    inherited SetMCDef(ALevel, AValue);

    re := TRegExpr.Create();
    re.ModifierM := true;
    try
      re.Expression := LevelPrefix(ALevel)+'gate="([^"]+)"';
      if re.Exec(AValue) then
      begin
        i := StrToIntDef(re.Match[1], -1);
        if (i >= Integer(Low(TMCGate))) and (i <= Integer(High(TMCGate))) then
        begin
          FGate := TMCGate(i);
        end;
      end;

      re.Expression := LevelPrefix(ALevel)+'pini1="([^"]+)"';
      if re.Exec(AValue) then
      begin
        i := StrToIntDef(re.Match[1], -1);
        if (i >= PinLow()) and (i <= PinHigh()) then
        begin
          FPinI1 := i;
        end;
      end;

      re.Expression := LevelPrefix(ALevel)+'pini2="([^"]+)"';
      if re.Exec(AValue) then
      begin
        i := StrToIntDef(re.Match[1], -1);
        if (i >= PinLow()) and (i <= PinHigh()) then
        begin
          FPinI2 := i;
        end;
      end;

      re.Expression := LevelPrefix(ALevel)+'pino1="([^"]+)"';
      if re.Exec(AValue) then
      begin
        i := StrToIntDef(re.Match[1], -1);
        if (i >= PinLow()) and (i <= PinHigh()) then
        begin
          FPinO1 := i;
        end;
      end;
    finally
      re.Free;
    end;

  finally
    FLogger._e();
  end;
end;

(* PUBLIC *)

constructor TMapComponentGate.Create(AGate: TMCGate; const AName: string; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create(AName, MCSize(1,1), APos, AParent);
  Clear;
  FGate := AGate;
end;

destructor TMapComponentGate.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentGate.Clear();
const METHOD: string = 'TMapComponentGate.Clear';
begin
  FLogger._s(METHOD);
  try

    FGate := mcgNone;
    FPinI1 := 0;
    FPinI2 := 1;
    FPinO1 := 2;

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentGate.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);

  procedure CenterText(const AText: string);
  begin
    SetBrush(ACanvas);
    SetPen(ACanvas);
    ACanvas.TextOut(
      ARect.Left+((ARect.Width-ACanvas.TextWidth(AText)) div 2),
      ARect.Top+((ARect.Height-ACanvas.TextHeight(AText)) div 2),
      AText
    );
  end;

const METHOD: string = 'TMapComponentGate.Draw';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ACanvas', TypeInfo(ACanvas), @ACanvas);
    FLogger._pe('ARect', TypeInfo(ARect), @ARect);
    FLogger._pe('AStyle', TypeInfo(AStyle), @AStyle);
    FLogger._se();

    inherited Draw(ACanvas, ARect, AStyle);
    CenterText(MCGateToStr(FGate));
{  case FGate of
    mcgNone:
    begin
    end;
    mcgBuf:
    begin
    end;
    mcgInv:
    begin
    end;
    mcgAnd:
    begin
    end;
    mcgNand:
    begin
    end;
    mcgOr:
    begin
    end;
    mcgNor:
    begin
    end;
    mcgXor:
    begin
    end;
    mcgXnor:
    begin
    end;
  end;}

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentGate.Tick();
const METHOD: string = 'TMapComponentGate.Tick';
var i: integer;
begin
  FLogger._s(METHOD);
  try

    if FGate = mcgNone then
    begin
      FLogger._(ltInfo, 'Tick skipped, gate type is not defined');
      Exit;
    end;

    case FGate of
      mcgBuf:
      begin
        PinValue[FPinO1] := PinValue[FPinI1];
      end;
      mcgInv:
      begin
        PinValue[FPinO1] := not PinValue[FPinI1];
      end;
      mcgAnd:
      begin
        PinValue[FPinO1] := PinValue[FPinI1] and PinValue[FPinI2];
      end;
      mcgNand:
      begin
        PinValue[FPinO1] := not (PinValue[FPinI1] and PinValue[FPinI2]);
      end;
      mcgOr:
      begin
        PinValue[FPinO1] := PinValue[FPinI1] or PinValue[FPinI2];
      end;
      mcgNor:
      begin
        PinValue[FPinO1] := not (PinValue[FPinI1] or PinValue[FPinI2]);
      end;
      mcgXor:
      begin
        PinValue[FPinO1] := PinValue[FPinI1] xor PinValue[FPinI2];
      end;
      mcgXnor:
      begin
        PinValue[FPinO1] := not (PinValue[FPinI1] xor PinValue[FPinI2]);
      end;
    end;
    Parent.RequestTick(GetPinField(FPinO1),GetPinNeighbourField(FPinO1));

  finally
    FLogger._e();
  end;
end;

{procedure TMapComponentGate.SetPinTypes(APinI1,APinI2,APinO1: integer);
const METHOD: string = 'TMapComponentGate.SetPinTypes';
var i: integer;
begin
  FLogger._s(METHOD);

  if FGate = mcgNone then
  begin
    FLogger._e();
    Exit;
  end;

  if (APinI1 < PinLow()) or (APinI1 > PinHigh()) or
     (APinO1 < PinLow()) or (APinO1 > PinHigh()) or
     ((FGate <> mcgBuf) and (FGate <> mcgInv) and ((APinI2 < PinLow()) or (APinI2 > PinHigh()))) or
     (APinI1 = APinI2) or (APinI1 = APinO1) or (APinI2 = APinO1) then
    begin
      FLogger._e();
      Exit;
    end;

  for i := PinLow() to PinHigh() do
  begin
    PinActive[i] := false;
  end;

  FPinI1 := APinI1;
  PinActive[FPinI1] := true;

  if (FGate = mcgBuf) or (FGate = mcgInv) then
  begin
    FPinI2 := -1;
  end
  else
  begin
    FPinI2 := APinI2;
    PinActive[FPinI2] := true;
  end;
  FPinO1 := APinO1;
  PinActive[FPinO1] := true;

  FLogger._e();
end; }

end.

