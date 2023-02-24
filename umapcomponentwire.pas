unit umapcomponentwire;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, umapcomponentpinned, umapcomponentbase;

type
  TMCPinConnection = record
    PinA: integer;
    PinB: integer;
  end;

  TMapComponentWire = class(TMapComponentPinned)

    private

      var FPinConnections: array of TMCPinConnection;

    protected

      function GetMCDef(): string; override;
      procedure SetMCDef(const AValue: string); override;

    public

      constructor Create(const AName: string; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); override;
      procedure Tick(ANeighbourField: TMCPos);
      procedure Connect(APinA, APinB: integer);
      procedure Disconnect(APinA, APinB: integer);
      function Connected(APinA, APinB: integer): integer;
      function ConnectsTo(APin: integer): integer;

  end;

implementation

(* PROTECTED *)

function TMapComponentWire.GetMCDef(): string;
const METHOD: string = 'TMapComponentWire.GetMCDef';
var i: integer;
begin
  FLogger._s(METHOD);

  result := inherited GetMCDef;
  result := result + Format(';pinconns="%d"', [Length(FPinConnections)]);
  for i := Low(FPinConnections) to High(FPinConnections) do
  begin
    result := result + Format(';pinconn="%d;%d;%d"', [i, FPinConnections[i].PinA, FPinConnections[i].PinB]);
  end;

  FLogger._e();
end;

procedure TMapComponentWire.SetMCDef(const AValue: string);
const METHOD: string = 'TMapComponentWire.SetMCDef';
begin
  FLogger._s(METHOD);

  inherited SetMCDef(AValue);

  FLogger._e();
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

  SetLength(FPinConnections, 0);

  FLogger._e();
end;

procedure TMapComponentWire.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentWire.Draw';
var i: integer;
    rctA,rctB: TRect;
begin
  FLogger._s(METHOD);

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

  FLogger._e();
end;

procedure TMapComponentWire.Tick(ANeighbourField: TMCPos);
const METHOD: string = 'TMapComponentWire.Tick';
var i,ii: integer;
begin
  FLogger._s(METHOD);

  i := GetPinIndexFromNeighbourField(ANeighbourField);
  ii := ConnectsTo(i);
  if ii > -1 then
  begin
    PinValue[ii] := PinValue[i];
    Parent.RequestTick(GetPinField(ii),GetPinNeighbourField(ii));
  end;

  FLogger._e();
end;

procedure TMapComponentWire.Connect(APinA, APinB: integer);
const METHOD: string = 'TMapComponentWire.Connect';
begin
  FLogger._s(METHOD);

  if Connected(APinA,APinB) > -1 then
  begin
    FLogger._e();
    Exit;
  end;
  SetLength(FPinConnections, Length(FPinConnections)+1);
  FPinConnections[High(FPinConnections)].PinA := APinA;
  FPinConnections[High(FPinConnections)].PinB := APinB;
  PinActive[APinA] := true;
  PinActive[APinB] := true;

  FLogger._e();
end;

procedure TMapComponentWire.Disconnect(APinA, APinB: integer);
const METHOD: string = 'TMapComponentWire.Disconnect';
var i: integer;
begin
  FLogger._s(METHOD);

  repeat
    i := Connected(APinA, APinB);
    if (i >= Low(FPinConnections)) and (i <= High(FPinConnections)) then
    begin
      FPinConnections[i].PinA := -1;
      FPinConnections[i].PinB := -1;
    end;
  until
    i = -1;
  PinActive[APinA] := (ConnectsTo(APinA) > -1);
  PinActive[APinB] := (ConnectsTo(APinB) > -1);

  FLogger._e();
end;

function TMapComponentWire.Connected(APinA, APinB: integer): integer;
const METHOD: string = 'TMapComponentWire.Connected';
var i: integer;
begin
  FLogger._s(METHOD);

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

  FLogger._e(result);
end;

function TMapComponentWire.ConnectsTo(APin: integer): integer;
const METHOD: string = 'TMapComponentWire.ConnectsTo';
var i: integer;
begin
  FLogger._s(METHOD);

  result := -1;
  for i := Low(FPinConnections) to High(FPinConnections) do
  begin
    if FPinConnections[i].PinA = APin then
    begin
      result := FPinConnections[i].PinB;
      break;
    end;
    if FPinConnections[i].PinB = APin then
    begin
      result := FPinConnections[i].PinA;
      break;
    end;
  end;

  FLogger._e(result);
end;

end.

