unit umapcomponentgate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, umapcomponentpinned, umapcomponentbase;

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

      function GetMCDef(): string; override;
      procedure SetMCDef(const AValue: string); override;

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

  result := FGate;

  FLogger._e();
end;

procedure TMapComponentGate.SetGate(AValue: TMCGate);
const METHOD: string = 'TMapComponentGate.SetGate';
begin
  FLogger._s(METHOD);

  FGate := AValue;

  FLogger._e();
end;

function TMapComponentGate.GetPinI1(): integer;
const METHOD: string = 'TMapComponentGate.GetPinI1';
begin
  FLogger._s(METHOD);

  result := FPinI1;

  FLogger._e();
end;

procedure TMapComponentGate.SetPinI1(AValue: integer);
const METHOD: string = 'TMapComponentGate.SetPinI1';
begin
  FLogger._s(METHOD);

  FPinI1 := AValue;
  UpdatePinActive();

  FLogger._e();
end;

function TMapComponentGate.GetPinI2(): integer;
const METHOD: string = 'TMapComponentGate.GetPinI2';
begin
  FLogger._s(METHOD);

  result := FPinI2;

  FLogger._e();
end;

procedure TMapComponentGate.SetPinI2(AValue: integer);
const METHOD: string = 'TMapComponentGate.SetPinI2';
begin
  FLogger._s(METHOD);

  FPinI2 := AValue;
  UpdatePinActive();

  FLogger._e();
end;

function TMapComponentGate.GetPinO1(): integer;
const METHOD: string = 'TMapComponentGate.GetPinO1';
begin
  FLogger._s(METHOD);

  result := FPinO1;

  FLogger._e();
end;

procedure TMapComponentGate.SetPinO1(AValue: integer);
const METHOD: string = 'TMapComponentGate.SetPinO1';
begin
  FLogger._s(METHOD);

  FPinO1 := AValue;
  UpdatePinActive();

  FLogger._e();
end;

procedure TMapComponentGate.UpdatePinActive();
const METHOD: string = 'TMapComponentGate.Clear';
var i: integer;
begin
  FLogger._s(METHOD);

  for i := PinLow() to PinHigh() do
  begin
    PinActive[i] := false;
  end;
  PinActive[FPinI1] := true;
  PinActive[FPinI2] := true;
  PinActive[FPinO1] := true;

  FLogger._e();
end;

(* PROTECTED *)

function TMapComponentGate.GetMCDef(): string;
const METHOD: string = 'TMapComponentGate.GetMCDef';
var i: integer;
begin
  FLogger._s(METHOD);

  result := inherited GetMCDef;
  result := result + Format(';gate="%d";pini1="%d";pini2="%d";pino1="%d"', [Integer(FGate), FPinI1, FPinI2, FPinO1]);

  FLogger._e();
end;

procedure TMapComponentGate.SetMCDef(const AValue: string);
const METHOD: string = 'TMapComponentGate.SetMCDef';
begin
  FLogger._s(METHOD);

  inherited SetMCDef(AValue);

  FLogger._e();
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

  FGate := mcgNone;
  FPinI1 := 0;
  FPinI2 := 1;
  FPinO1 := 2;

  FLogger._e();
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
  FLogger._s(METHOD);

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

  FLogger._e();
end;

procedure TMapComponentGate.Tick();
const METHOD: string = 'TMapComponentGate.Tick';
var i: integer;
begin
  FLogger._s(METHOD);

  if FGate = mcgNone then
  begin
    FLogger._e();
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

  FLogger._e();
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

