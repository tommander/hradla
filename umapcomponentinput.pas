unit umapcomponentinput;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, umapcomponentio, umapcomponentbase;

type
  TMapComponentInput = class(TMapComponentIO)

    public

      constructor Create(const AName: string; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); override;
      procedure Tick();

  end;

implementation

(* PUBLIC *)

constructor TMapComponentInput.Create(const AName: string; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create(AName, MCSize(1,1), APos, AParent);
  Clear;
  PinActive[2] := true;
end;

destructor TMapComponentInput.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentInput.Clear();
const METHOD: string = 'TMapComponentInput.Clear';
begin
  FLogger._s(METHOD);

  //

  FLogger._e();
end;

procedure TMapComponentInput.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentInput.Draw';
begin
  FLogger._s(METHOD);

  inherited Draw(ACanvas, ARect, AStyle);
  SetPen(ACanvas);
  if Value then
  begin
    SetBrush(ACanvas, clLime);
  end
  else
  begin
    SetBrush(ACanvas, clGreen);
  end;
  ACanvas.Ellipse(
    ARect.Left + (ARect.Width div 3),
    ARect.Top + (ARect.Height div 3),
    ARect.Left + ((ARect.Width div 3)*2),
    ARect.Top + ((ARect.Height div 3)*2)
  );

  FLogger._e();
end;

procedure TMapComponentInput.Tick();
const METHOD: string = 'TMapComponentInput.Tick';
var i: integer;
begin
  FLogger._s(METHOD);

  for i := PinLow() to PinHigh() do
  begin
    if PinActive[i] then
    begin
      PinValue[i] := Value;
      Parent.RequestTick(GetPinField(i),GetPinNeighbourField(i));
      break;
    end;
  end;

  FLogger._e();
end;

end.

