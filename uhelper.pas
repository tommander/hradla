unit uhelper;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics;

procedure SetPen(ACanvas: TCanvas; AColor: TColor = clNone; AWidth: integer = 0; AStyle: TPenStyle = psClear);
procedure SetBrush(ACanvas: TCanvas; AColor: TColor = clNone; AStyle: TBrushStyle = bsClear);

implementation

procedure SetPen(ACanvas: TCanvas; AColor: TColor = clNone; AWidth: integer = 0; AStyle: TPenStyle = psClear);
begin
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := AWidth;
  ACanvas.Pen.Style := AStyle;
end;

procedure SetBrush(ACanvas: TCanvas; AColor: TColor = clNone; AStyle: TBrushStyle = bsClear);
begin
  ACanvas.Brush.Color := AColor;
  ACanvas.Brush.Style := AStyle;
end;

end.

