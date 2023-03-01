unit umapcomponentoutput;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, ulogger, umapcomponentio, umapcomponentbase;

type
  TMapComponentOutput = class(TMapComponentIO)

    public

      constructor Create(const AName: string; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle); override;
      procedure Tick();

  end;

implementation

(* PUBLIC *)

constructor TMapComponentOutput.Create(const AName: string; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create(AName, MCSize(1,1), APos, AParent);
  Clear;
  PinActive[0] := true;
end;

destructor TMapComponentOutput.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentOutput.Clear();
const METHOD: string = 'TMapComponentOutput.Clear';
begin
  FLogger._s(METHOD);
  try

    //

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentOutput.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentOutput.Draw';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ACanvas', TypeInfo(ACanvas), @ACanvas);
    FLogger._pe('ARect', TypeInfo(ARect), @ARect);
    FLogger._pe('AStyle', TypeInfo(AStyle), @AStyle);
    FLogger._se();

    inherited Draw(ACanvas, ARect, AStyle);
    SetPen(ACanvas);
    if Value then
    begin
      SetBrush(ACanvas, clAqua);
    end
    else
    begin
      SetBrush(ACanvas, clTeal);
    end;
    ACanvas.Ellipse(
      ARect.Left + (ARect.Width div 3),
      ARect.Top + (ARect.Height div 3),
      ARect.Left + ((ARect.Width div 3)*2),
      ARect.Top + ((ARect.Height div 3)*2)
    );

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentOutput.Tick();
const METHOD: string = 'TMapComponentOutput.Tick';
var i: integer;
begin
  FLogger._s(METHOD);
  try

    for i := PinLow() to PinHigh() do
    begin
      if PinActive[i] then
      begin
        Value := PinValue[i];
        break;
      end;
    end;

  finally
    FLogger._e();
  end;
end;

end.

