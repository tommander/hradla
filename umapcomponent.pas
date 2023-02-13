unit umapcomponent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uhelper;

type
  TMapComponentType = (mctNone,mctLR,mctTB,mctTR,mctTL,mctBR,mctBL,mctLTR,mctLBR,mctTLB,mctTRB,mctLRTB);
  TMapComponent = class
    private

      var FName: string;
      var FWidth: byte;
      var FHeight: byte;
//      var FColor: TColor;
      var FActive: boolean;
      var FType: TMapComponentType;

      function GetName(): string;
      function GetWidth(): byte;
      function GetHeight(): byte;
      function GetType(): TMapComponentType;
      procedure SetType(AValue: TMapComponentType);
//      function GetColor(): TColor;
      procedure SetActive(AValue: boolean);
      function GetActive(): boolean;

    public
      property Name: string read GetName;
      property Width: byte read GetWidth;
      property Height: byte read GetHeight;
      property MCType: TMapComponentType read GetType write SetType;
//      property Color: TColor read GetColor;
      property Active: boolean read GetActive write SetActive;

      constructor Create(const AType: TMapComponentType);
      destructor Destroy; override;
      procedure Draw(const ACanvas: TCanvas; const ARect:TRect; const boolSelected: boolean);
      procedure Tick();

      class function MCTToStr(AMCT: TMapComponentType): string;
      class function MCTToSize(AMCT: TMapComponentType): TPoint;
      class procedure DrawEmpty(const ACanvas: TCanvas; const ARect:TRect; const boolSelected: boolean);
  end;
  PMapComponent = ^TMapComponent;


implementation

constructor TMapComponent.Create(const AType: TMapComponentType);
begin
  inherited Create();
  SetType(AType);
end;

destructor TMapComponent.Destroy;
begin
  inherited Destroy();
end;

function TMapComponent.GetName(): string;
begin
  result := FName;
end;

function TMapComponent.GetWidth(): byte;
begin
  result := FWidth;
end;

function TMapComponent.GetHeight(): byte;
begin
  result := FHeight;
end;

function TMapComponent.GetType(): TMapComponentType;
begin
  result := FType;
end;

procedure TMapComponent.SetType(AValue: TMapComponentType);
var pt: TPoint;
begin
  FType := AValue;
  FName := MCTToStr(FType);
  pt := MCTToSize(FType);
  FWidth := pt.X;
  FHeight := pt.Y;
  FActive := (FType <> mctNone);
end;

//function TMapComponent.GetColor(): TColor;
//begin
//  result := FColor;
//end;

procedure TMapComponent.SetActive(AValue: boolean);
begin
  FActive := AValue;
end;

function TMapComponent.GetActive(): boolean;
begin
  result := FActive;
end;

class procedure TMapComponent.DrawEmpty(const ACanvas: TCanvas; const ARect:TRect; const boolSelected: boolean);
begin
  if boolSelected then
  begin
    SetPen(ACanvas, clMaroon, 1, psSolid);
    SetBrush(ACanvas, clRed, bsSolid);
    ACanvas.Rectangle(ARect);
    SetPen(ACanvas);
  end
  else
  begin
    SetPen(ACanvas, clBlack, 1, psSolid);
    SetBrush(ACanvas, clBtnFace, bsSolid);
    ACanvas.Rectangle(ARect);
    SetPen(ACanvas);
  end;
end;

procedure TMapComponent.Draw(const ACanvas: TCanvas; const ARect:TRect; const boolSelected: boolean);
var //intTW,intTH: integer;
    //strT: string;
    intTemp1,intTemp2: integer;
begin
  DrawEmpty(ACanvas, ARect, boolSelected);

  intTemp1 := ARect.Top+((ARect.Bottom-ARect.Top) div 2);
  intTemp2 := ARect.Left+((ARect.Right-ARect.Left) div 2);
  {
  LEFT:   ACanvas.MoveTo(ARect.Left, intTemp1);   ACanvas.LineTo(ARect.Left, intTemp1);
  RIGHT:  ACanvas.MoveTo(ARect.Right, intTemp1);  ACanvas.LineTo(ARect.Right, intTemp1);
  TOP:    ACanvas.MoveTo(intTemp2, ARect.Top);    ACanvas.LineTo(intTemp2, ARect.Top);
  BOTTOM: ACanvas.MoveTo(intTemp2, ARect.Bottom); ACanvas.LineTo(intTemp2, ARect.Bottom);
  }
  case FType of
    mctLR:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(ARect.Left, intTemp1);
      ACanvas.LineTo(ARect.Right, intTemp1);
      SetPen(ACanvas);
    end;
    mctTB:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp2, ARect.Top);
      ACanvas.LineTo(intTemp2, ARect.Bottom);
      SetPen(ACanvas);
    end;
    mctTR:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp2, ARect.Top);
      ACanvas.LineTo(ARect.Right, intTemp1);
      SetPen(ACanvas);
    end;
    mctTL:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp2, ARect.Top);
      ACanvas.LineTo(ARect.Left, intTemp1);
      SetPen(ACanvas);
    end;
    mctBR:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp2, ARect.Bottom);
      ACanvas.LineTo(ARect.Right, intTemp1);
      SetPen(ACanvas);
    end;
    mctBL:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp2, ARect.Bottom);
      ACanvas.LineTo(ARect.Left, intTemp1);
      SetPen(ACanvas);
    end;
    mctLTR:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(ARect.Left, intTemp1);
      ACanvas.LineTo(ARect.Right, intTemp1);
      ACanvas.LineTo(intTemp2, ARect.Top);
      ACanvas.LineTo(ARect.Left, intTemp1);
      SetPen(ACanvas);
    end;
    mctLBR:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(ARect.Left, intTemp1);
      ACanvas.LineTo(ARect.Right, intTemp1);
      ACanvas.LineTo(intTemp2, ARect.Bottom);
      ACanvas.LineTo(ARect.Left, intTemp1);
      SetPen(ACanvas);
    end;
    mctTLB:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp2, ARect.Top);
      ACanvas.LineTo(intTemp2, ARect.Bottom);
      ACanvas.LineTo(ARect.Left, intTemp1);
      ACanvas.LineTo(intTemp2, ARect.Top);
      SetPen(ACanvas);
    end;
    mctTRB:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp2, ARect.Top);
      ACanvas.LineTo(intTemp2, ARect.Bottom);
      ACanvas.LineTo(ARect.Right, intTemp1);
      ACanvas.LineTo(intTemp2, ARect.Top);
      SetPen(ACanvas);
    end;
    mctLRTB:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(ARect.Left, intTemp1);
      ACanvas.LineTo(ARect.Right, intTemp1);
      ACanvas.MoveTo(intTemp2, ARect.Top);
      ACanvas.LineTo(intTemp2, ARect.Bottom);
      SetPen(ACanvas);
    end;
  end;
end;

procedure TMapComponent.Tick();
begin
  //
end;

class function TMapComponent.MCTToStr(AMCT: TMapComponentType): string;
begin
  result := '';
  case AMCT of
    mctNone: result := 'None';
    mctLR: result := 'Left-Right';
    mctTB: result := 'Top-Bottom';
    mctTR: result := 'Top-Right';
    mctTL: result := 'Top-Left';
    mctBR: result := 'Bottom-Right';
    mctBL: result := 'Bottom-Left';
    mctLTR: result := 'Left-Top-Right';
    mctLBR: result := 'Left-Bottom-Right';
    mctTLB: result := 'Top-Left-Bottom';
    mctTRB: result := 'Top-Right-Bottom';
    mctLRTB: result := 'Left-Right-Top-Bottom';
  end;
end;

class function TMapComponent.MCTToSize(AMCT: TMapComponentType): TPoint;
begin
  result.Create(-1,-1);
  case AMCT of
    mctLR: result.Create(1,1);
    mctTB: result.Create(1,1);
    mctTR: result.Create(1,1);
    mctTL: result.Create(1,1);
    mctBR: result.Create(1,1);
    mctBL: result.Create(1,1);
    mctLTR: result.Create(1,1);
    mctLBR: result.Create(1,1);
    mctTLB: result.Create(1,1);
    mctTRB: result.Create(1,1);
    mctLRTB: result.Create(1,1);
  end;
end;

end.

