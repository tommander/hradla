unit umapcomponentwire;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, RegExpr, ulogger, umapcomponentpinned, umapcomponentbase;

type
  TMCPinConnection = record
    PinA: integer;
    PinB: integer;
  end;

  TMapComponentWire = class(TMapComponentPinned)

    private

      var FPinConnections: array of TMCPinConnection;

    protected


    public

      constructor Create(const AName: string; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); override;
      procedure Tick(APin: integer);
      function GetMCDef(ALevel: byte): string; override;
      procedure SetMCDef(ALevel: byte; const AValue: string; AAddToPos: TMCPos); override;
      procedure Connect(APinA, APinB: integer);
      procedure Disconnect(APinA, APinB: integer);
      function Connected(APinA, APinB: integer): integer;
      function HasConnection(APin: integer): boolean;

  end;

implementation

(* PROTECTED *)

function TMapComponentWire.GetMCDef(ALevel: byte): string;
const METHOD: string = 'TMapComponentWire.GetMCDef';
var i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
    FLogger._se();

    result := inherited GetMCDef(ALevel);
    for i := Low(FPinConnections) to High(FPinConnections) do
    begin
      result := result + Format(#13#10'%spinconn="%d,%d,%d"', [LevelStr(ALevel),i, FPinConnections[i].PinA, FPinConnections[i].PinB]);
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentWire.SetMCDef(ALevel: byte; const AValue: string; AAddToPos: TMCPos);
const METHOD: string = 'TMapComponentWire.SetMCDef';
var re: TRegExpr;
    intA,intB: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    inherited SetMCDef(ALevel, AValue, AAddToPos);

    re := TRegExpr.Create();
    re.ModifierM := true;
    try
      re.Expression := LevelPrefix(ALevel)+'pinconn="([^,]+),([^,]+),([^"]+)"';
      if re.Exec(AValue) then
      begin
        repeat
          intA := StrToIntDef(re.Match[2], 0);
          intB := StrToIntDef(re.Match[3], 0);
          Connect(intA,intB);
        until
          not re.ExecNext;
      end;
    finally
      re.Free;
    end;

  finally
    FLogger._e();
  end;
end;

(* PUBLIC *)

constructor TMapComponentWire.Create(const AName: string; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create(AName, MCSize(1,1), APos, AParent);
  Clear;
end;

destructor TMapComponentWire.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentWire.Clear();
const METHOD: string = 'TMapComponentWire.Clear';
begin
  FLogger._s(METHOD);
  try

    SetLength(FPinConnections, 0);

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentWire.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentWire.Draw';
var i: integer;
    rctA,rctB: TRect;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ACanvas', TypeInfo(ACanvas), @ACanvas);
    FLogger._pe('ARect', TypeInfo(ARect), @ARect);
    FLogger._pe('AStyle', TypeInfo(AStyle), @AStyle);
    FLogger._se();

    inherited Draw(ACanvas, ARect, AStyle);
    SetPen(ACanvas);
    if Length(FPinConnections) = 0 then
    begin
      ACanvas.MoveTo(ARect.Left, ARect.Top);
      ACanvas.LineTo(ARect.Right, ARect.Bottom);
      ACanvas.MoveTo(ARect.Right, ARect.Top);
      ACanvas.LineTo(ARect.Left, ARect.Bottom);
    end
    else
    begin
      for i := Low(FPinConnections) to High(FPinConnections) do
      begin
        rctA := GetPinRect(FPinConnections[i].PinA, ARect);
        rctB := GetPinRect(FPinConnections[i].PinB, ARect);
        ACanvas.MoveTo(rctA.Left+(rctA.Width div 2), rctA.Top+(rctA.Height div 2));
        ACanvas.LineTo(rctB.Left+(rctB.Width div 2), rctB.Top+(rctB.Height div 2));
      end;
    end;

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentWire.Tick(APin: integer);
const METHOD: string = 'TMapComponentWire.Tick';
var ii: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APin', TypeInfo(APin), @APin);
    FLogger._se();

    for ii := Low(FPinConnections) to High(FPinConnections) do
    begin
      if FPinConnections[ii].PinA = APin then
      begin
        PinValue[FPinConnections[ii].PinB] := PinValue[APin];
        Parent.RequestTick(GetPinField(FPinConnections[ii].PinB),GetPinNeighbourField(FPinConnections[ii].PinB));
      end
      else if FPinConnections[ii].PinB = APin then
      begin
        PinValue[FPinConnections[ii].PinA] := PinValue[APin];
        Parent.RequestTick(GetPinField(FPinConnections[ii].PinA),GetPinNeighbourField(FPinConnections[ii].PinA));
      end;
    end;

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentWire.Connect(APinA, APinB: integer);
const METHOD: string = 'TMapComponentWire.Connect';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APinA', TypeInfo(APinA), @APinA);
    FLogger._pe('APinB', TypeInfo(APinB), @APinB);
    FLogger._se();

    if Connected(APinA,APinB) > -1 then
    begin
      FLogger._(ltInfo, 'Pins are already connected.');
      Exit;
    end;
    SetLength(FPinConnections, Length(FPinConnections)+1);
    FPinConnections[High(FPinConnections)].PinA := APinA;
    FPinConnections[High(FPinConnections)].PinB := APinB;
    PinActive[APinA] := true;
    PinActive[APinB] := true;

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentWire.Disconnect(APinA, APinB: integer);
const METHOD: string = 'TMapComponentWire.Disconnect';
var i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APinA', TypeInfo(APinA), @APinA);
    FLogger._pe('APinB', TypeInfo(APinB), @APinB);
    FLogger._se();

    repeat
      i := Connected(APinA, APinB);
      if (i >= Low(FPinConnections)) and (i <= High(FPinConnections)) then
      begin
        FPinConnections[i].PinA := -1;
        FPinConnections[i].PinB := -1;
      end;
    until
      i = -1;

    PinActive[APinA] := HasConnection(APinA);
    PinActive[APinB] := HasConnection(APinB);

  finally
    FLogger._e();
  end;
end;

function TMapComponentWire.Connected(APinA, APinB: integer): integer;
const METHOD: string = 'TMapComponentWire.Connected';
var i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APinA', TypeInfo(APinA), @APinA);
    FLogger._pe('APinB', TypeInfo(APinB), @APinB);
    FLogger._se();

    result := -1;
    for i := Low(FPinConnections) to High(FPinConnections) do
    begin
      if ((FPinConnections[i].PinA = APinA) and (FPinConnections[i].PinB = APinB)) or
         ((FPinConnections[i].PinA = APinB) and (FPinConnections[i].PinB = APinA)) then
      begin
        result := i;
        break;
      end;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentWire.HasConnection(APin: integer): boolean;
const METHOD: string = 'TMapComponentWire.HasConnection';
var i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APin', TypeInfo(APin), @APin);
    FLogger._se();

    result := false;
    for i := Low(FPinConnections) to High(FPinConnections) do
    begin
      if FPinConnections[i].PinA = APin then
      begin
        result := true;
        break;
      end;
      if FPinConnections[i].PinB = APin then
      begin
        result := true;
        break;
      end;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

end.

