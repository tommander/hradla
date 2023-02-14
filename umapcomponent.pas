unit umapcomponent;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, uhelper;

const
  PIN_IN: byte = 0;
  PIN_OUT: byte = 1;
  PIN_BOTH: byte = 2;

type
  TMapComponentType = (mctNone,mctMap,mctLR,mctTB,mctTR,mctTL,mctBR,mctBL,mctLTR,mctLBR,mctTLB,mctTRB,mctLRTB,mctIn,mctOut,mctAnd);
  TMapComponentPin = record
    FActive: boolean;
    FType: byte; //0 = in, 1 = out, 2 = both
    FValue: boolean;
    FWrite: boolean; //whether the value was set on the pin
  end;

  TMapComponent = class;
  PMapComponent = ^TMapComponent;

  TMapComponent = class
    private

      var FName: string;
      var FLeft: byte;
      var FTop: byte;
      var FWidth: byte;
      var FHeight: byte;
//      var FColor: TColor;
      var FActive: boolean;
      var FType: TMapComponentType;
      var FPins: array of TMapComponentPin;
      var FMap: array of array of PMapComponent;

      function GetName(): string;
      function GetLeft(): byte;
      function GetTop(): byte;
      function GetWidth(): byte;
      function GetHeight(): byte;
      function GetType(): TMapComponentType;
      procedure SetType(AValue: TMapComponentType);
//      function GetColor(): TColor;
      procedure SetActive(AValue: boolean);
      function GetActive(): boolean;
      function GetPinValue(AIndex: integer): boolean;
      procedure SetPinValue(AIndex: integer; AValue: boolean);

    public
      property Name: string read GetName;
      property Left: byte read GetLeft;
      property Top: byte read GetTop;
      property Width: byte read GetWidth;
      property Height: byte read GetHeight;
      property MCType: TMapComponentType read GetType{ write SetType};
//      property Color: TColor read GetColor;
      property Active: boolean read GetActive write SetActive;
      property PinValue[AIndex: integer]: boolean read GetPinValue write SetPinValue;

      constructor Create(const AType: TMapComponentType; const ALeft,ATop: byte);
      destructor Destroy; override;
      procedure Draw(const ACanvas: TCanvas; const ARect:TRect; const boolSelected: boolean);
      procedure Tick();
      procedure SetMap(const strMapDef: string);
      function GetPinIndex(const fX,fY: integer): integer;
      function GetPinPos(const AIndex: integer): TPoint;

      class function MCTToStr(AMCT: TMapComponentType): string;
      class function MCTToSize(AMCT: TMapComponentType): TPoint;
      class procedure DrawEmpty(const ACanvas: TCanvas; const ARect:TRect; const boolSelected: boolean);
  end;

function InitPin(AActive: boolean; AType: byte; AValue: boolean; AWrite: boolean): TMapComponentPin;

implementation

function InitPin(AActive: boolean; AType: byte; AValue: boolean; AWrite: boolean): TMapComponentPin;
begin
  result.FActive := AActive;
  result.FType := AType;
  result.FValue := AValue;
  result.FWrite := AWrite;
end;

constructor TMapComponent.Create(const AType: TMapComponentType; const ALeft,ATop: byte);
begin
  inherited Create();
  FLeft := ALeft;
  FTop := ATop;
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

function TMapComponent.GetLeft(): byte;
begin
  result := FLeft;
end;

function TMapComponent.GetTop(): byte;
begin
  result := FTop;
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
    i: integer;
begin
  FType := AValue;
  FName := MCTToStr(FType);
  pt := MCTToSize(FType);
  FWidth := pt.X;
  FHeight := pt.Y;
  FActive := (FType <> mctNone);
  SetLength(FMap, 0);
  if FType = mctMap then
  begin
    SetLength(FMap, FWidth);
    for i := Low(FMap) to High(FMap) do
    begin
      SetLength(FMap[i], FHeight);
    end;
  end;
  SetLength(FPins, 0);
  if FType <> mctNone then
  begin
    SetLength(FPins, 2*(FWidth+FHeight));
    for i := Low(FPins) to High(FPins) do
    begin
      FPins[i] := InitPin(false,0,false, false);
    end;
    case FType of
      mctLR:
      begin
        FPins[0] := InitPin(true, PIN_BOTH, false, false);
        FPins[2] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctTB:
      begin
        FPins[1] := InitPin(true, PIN_BOTH, false, false);
        FPins[3] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctTR:
      begin
        FPins[2] := InitPin(true, PIN_BOTH, false, false);
        FPins[3] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctTL:
      begin
        FPins[0] := InitPin(true, PIN_BOTH, false, false);
        FPins[3] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctBR:
      begin
        FPins[1] := InitPin(true, PIN_BOTH, false, false);
        FPins[2] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctBL:
      begin
        FPins[0] := InitPin(true, PIN_BOTH, false, false);
        FPins[1] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctLTR:
      begin
        FPins[0] := InitPin(true, PIN_BOTH, false, false);
        FPins[2] := InitPin(true, PIN_BOTH, false, false);
        FPins[3] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctLBR:
      begin
        FPins[0] := InitPin(true, PIN_BOTH, false, false);
        FPins[1] := InitPin(true, PIN_BOTH, false, false);
        FPins[2] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctTLB:
      begin
        FPins[0] := InitPin(true, PIN_BOTH, false, false);
        FPins[1] := InitPin(true, PIN_BOTH, false, false);
        FPins[3] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctTRB:
      begin
        FPins[1] := InitPin(true, PIN_BOTH, false, false);
        FPins[2] := InitPin(true, PIN_BOTH, false, false);
        FPins[3] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctLRTB:
      begin
        FPins[0] := InitPin(true, PIN_BOTH, false, false);
        FPins[1] := InitPin(true, PIN_BOTH, false, false);
        FPins[2] := InitPin(true, PIN_BOTH, false, false);
        FPins[3] := InitPin(true, PIN_BOTH, false, false);
      end;
      mctIn:
      begin
        FPins[2] := InitPin(true, PIN_IN, false, false);
      end;
      mctOut:
      begin
        FPins[0] := InitPin(true, PIN_OUT, false, false);
      end;
      mctAnd:
      begin
        FPins[0] := InitPin(true, PIN_IN, false, false);
        FPins[1] := InitPin(true, PIN_IN, false, false);
        FPins[4] := InitPin(true, PIN_OUT, false, false);
      end;
    end;
  end;
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

function TMapComponent.GetPinValue(AIndex: integer): boolean;
begin
  result := false;
  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) then
  begin
    Exit;
  end;
  result := FPins[AIndex].FValue;
end;

procedure TMapComponent.SetPinValue(AIndex: integer; AValue: boolean);
begin
  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) then
  begin
    Exit;
  end;
  FPins[AIndex].FValue := AValue;
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
    intTemp1,intTemp2,intTemp3Xa,intTemp3Xb,intTemp3Ya,intTemp3Yb: integer;
    intTemp4Xa,intTemp4Xb,intTemp4Xc,intTemp4Ya,intTemp4Yb,intTemp4Yc: integer;
    i: integer;
    pt: TPoint;
begin
  DrawEmpty(ACanvas, ARect, boolSelected);

  intTemp1 := ARect.Top+((ARect.Bottom-ARect.Top) div 2);
  intTemp2 := ARect.Left+((ARect.Right-ARect.Left) div 2);
  intTemp3Ya := ARect.Top+((ARect.Bottom-ARect.Top) div 3);
  intTemp3Xa := ARect.Left+((ARect.Right-ARect.Left) div 3);
  intTemp3Yb := ARect.Top+(((ARect.Bottom-ARect.Top) div 3)*2);
  intTemp3Xb := ARect.Left+(((ARect.Right-ARect.Left) div 3)*2);
  intTemp4Ya := ARect.Top+((ARect.Bottom-ARect.Top) div 4);
  intTemp4Xa := ARect.Left+((ARect.Right-ARect.Left) div 4);
  intTemp4Yb := ARect.Top+(((ARect.Bottom-ARect.Top) div 4)*2);
  intTemp4Xb := ARect.Left+(((ARect.Right-ARect.Left) div 4)*2);
  intTemp4Yc := ARect.Top+(((ARect.Bottom-ARect.Top) div 4)*3);
  intTemp4Xc := ARect.Left+(((ARect.Right-ARect.Left) div 4)*3);
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
    mctIn:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      SetBrush(ACanvas, clLime, bsSolid);
      ACanvas.Ellipse(intTemp3Xa,intTemp3Ya,intTemp3Xb,intTemp3Yb);
      ACanvas.MoveTo(intTemp3Xb,intTemp1);
      ACanvas.LineTo(ARect.Right,intTemp1);
      SetPen(ACanvas);
    end;
    mctOut:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      SetBrush(ACanvas, clYellow, bsSolid);
      ACanvas.Ellipse(intTemp3Xa,intTemp3Ya,intTemp3Xb,intTemp3Yb);
      ACanvas.MoveTo(intTemp3Xa,intTemp1);
      ACanvas.LineTo(ARect.Left,intTemp1);
      SetPen(ACanvas);
    end;
    mctAnd:
    begin
      SetPen(ACanvas, clBlack, 3, psSolid);
      ACanvas.MoveTo(intTemp4Xa,intTemp3Yb);
      ACanvas.LineTo(intTemp2,intTemp3Ya);
      ACanvas.LineTo(intTemp4Xc,intTemp3Yb);
      SetPen(ACanvas);
    end;
  end;

  for i := Low(FPins) to High(FPins) do
  begin
    if not FPins[i].FActive then
    begin
      continue;
    end;

    pt := GetPinPos(i);
    if (pt.X = -1) or (pt.Y = -1) then
    begin
      continue;
    end;

    SetPen(ACanvas, clBlack, 1, psSolid);
    if FPins[i].FType = PIN_IN then
    begin
    end;
  end;
end;

procedure TMapComponent.Tick();
begin
  //
end;

procedure TMapComponent.SetMap(const strMapDef: string);
begin
  //
end;

function TMapComponent.GetPinIndex(const fX,fY: integer): integer;
var i,intIdx: integer;
begin
  result := -1;

  //Not-a-neighbour
  if (fX < (FLeft-1)) or (fX > (FLeft+FWidth)) or (fY < (FTop-1)) or (fY > (FTop+FHeight)) then
  begin
    Exit;
  end;

  //Left
  if fX = (FLeft-1) then
  begin
    if (fY < FTop) or (fY >= (FTop+FHeight)) then
    begin
      Exit;
    end;
    result := fY-FTop;
  end;

  //Bottom
  if fY = (FTop+FHeight) then
  begin
    if (fX < FLeft) or (fY >= (FLeft+FWidth)) then
    begin
      Exit;
    end;
    result := FHeight+(fX-FLeft);
  end;

  //Right
  if fX = (FLeft+FWidth) then
  begin
    if (fY < FTop) or (fY >= (FTop+FHeight)) then
    begin
      Exit;
    end;
    result := (FHeight+FWidth)+(FTop+FHeight-1-fY);
  end;

  //Top
  if fY = (FTop-1) then
  begin
    if (fX < FLeft) or (fY >= (FLeft+FWidth)) then
    begin
      Exit;
    end;
    result := (FHeight+FWidth+FHeight)+(FLeft+FWidth-1-fX);
  end;
  //
{
  Component 3x2

   987
  +---+
 0|   |6
 1|   |5
  +---+
   234

  0: [FLeft-1 ; FTop]
  1: [FLeft-1 ; FTop+1]

  2: [FLeft   ; FTop+2]
  3: [FLeft+1 ; FTop+2]
  4: [FLeft+2 ; FTop+2]

  5: [FLeft+3 ; FTop+1]
  6: [FLeft+3 ; FTop]

  7: [FLeft+2 ; FTop-1]
  8: [FLeft+1 ; FTop-1]
  9: [FLeft   ; FTop-1]
}

end;

function TMapComponent.GetPinPos(const AIndex: integer): TPoint;
begin
  result.Create(-1,-1);
  if (AIndex < Low(FPins)) or (AIndex > High(FPins)) then
  begin
    Exit;
  end;

  if (AIndex < FHeight) then
  begin
    result.Create(FLeft-1, FTop+AIndex);
    Exit;
  end;

  if (AIndex < (FHeight+FWidth)) then
  begin
    result.Create(FLeft+(AIndex-FHeight), FTop+FHeight);
    Exit;
  end;

  if (AIndex < (FHeight+FWidth+FHeight)) then
  begin
    result.Create(FLeft+FWidth, FTop+(FWidth+FHeight+FHeight-1-AIndex));
    Exit;
  end;

  result.Create(FLeft+(FHeight-FWidth-FHeight-FWidth-1-AIndex), FTop-1);
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
    mctIn: result := 'Input';
    mctOut: result := 'Output';
    mctAnd: result := 'Gate AND';
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
    mctIn: result.Create(1,1);
    mctOut: result.Create(1,1);
    mctAnd: result.Create(1,2);
  end;
end;

end.

