unit umapcomponentmap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, umapcomponentwire, umapcomponentgate, umapcomponentinput, umapcomponentoutput, umapcomponentio, umapcomponentpinned, umapcomponentbase;

type
  TMCDrawMapStyle = (mdmsComplete,mdmsSelected,mdmsRedraw);
  TMapComponentMap = class(TMapComponentPinned)

    private

      var FSubcomponents: array of TMapComponentBase;
      var FMap: array of array of integer;
      var FCursorPos: TMCPos; //current position (updated on mouseover)
      var FCursorPosLast: TMCPos; //position since last selection (updated on click)
      var FCursorSize: TMCSize; //
      var FCursorSizeLast: TMCSize; //
      var FRedrawField: TMCPos;

      function ReservePlace(APos: TMCPos; ASize: TMCSize; AExistingCmp: integer = -1): boolean;
      function AddSubcomponent1(const APrefix: string): string;
      function AddSubcomponent2(APos: TMCPos): integer;

      function GetCursorPos(): TMCPos;
      procedure SetCursorPos(AValue: TMCPos);
      function GetCursorSize(): TMCSize;
      procedure SetCursorSize(AValue: TMCSize);
      function GetCursorPosLast(): TMCPos;
      function GetCursorSizeLast(): TMCSize;
      procedure SetRedrawField(AValue: TMCPos);

    protected

      function GetMCDef(): string; override;
      procedure SetMCDef(const AValue: string); override;

    public

      property CursorPos: TMCPos read GetCursorPos write SetCursorPos;
      property CursorPosLast: TMCPos read GetCursorPosLast;
      property CursorSize: TMCSize read GetCursorSize write SetCursorSize;
      property CursorSizeLast: TMCSize read GetCursorSizeLast;
      property RedrawField: TMCPos write SetRedrawField;

      constructor Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      procedure Zero();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawMapStyle); reintroduce;
      procedure Tick();
      procedure RequestTick(AFrom,ATo: TMCPos); override;

      function AddWire(APos: TMCPos): integer;
      function AddGate(AGate: TMCGate; APos: TMCPos): integer;
      function AddInput(APos: TMCPos): integer;
      function AddOutput(APos: TMCPos): integer;
      function AddMap(ASize: TMCSize; APos: TMCPos): integer;
      procedure RemoveSubcomponent(AIndex: integer);

      function SubcomponentByName(const AName: string): integer;
      function SubcomponentByField(APos: TMCPos): integer;
      function Subcomponent(AIndex: integer): TMapComponentBase;

      function InputString(const AString: string): boolean;
      function OutputString(): string;

  end;

implementation

(* PRIVATE *)

function TMapComponentMap.ReservePlace(APos: TMCPos; ASize: TMCSize; AExistingCmp: integer = -1): boolean;
const METHOD: string = 'TMapComponentMap.ReservePlace';
var x,y: integer;
begin
  FLogger._s(METHOD);

  result := false;

  for x := APos.x to APos.x+ASize.w-1 do
  begin
    if (x < Low(FMap)) or (x > High(FMap)) then
    begin
      FLogger._e(result);
      Exit;
    end;
    for y := APos.y to APos.y+ASize.h-1 do
    begin
      if (y < Low(FMap[x])) or (y > High(FMap[x])) then
      begin
        FLogger._e(result);
        Exit;
      end;
      if (FMap[x][y] <> -1) and (FMap[x][y] <> AExistingCmp) then
      begin
        FLogger._e(result);
        Exit;
      end;
    end;
  end;

  for x := APos.x to APos.x+ASize.w-1 do
  begin
    for y := APos.y to APos.y+ASize.h-1 do
    begin
      FMap[x][y] := AExistingCmp;
    end;
  end;

  result := true;
  FLogger._e(result);
end;

function TMapComponentMap.AddSubcomponent1(const APrefix: string): string;
const METHOD: string = 'TMapComponentMap.AddSubcomponent1';
var intName: integer;
begin
  FLogger._s(METHOD);

  result := '';
  SetLength(FSubcomponents, Length(FSubcomponents)+1);
  intName := 0;
  repeat
    result := Format('%s%d', [APrefix, intName]);
    Inc(intName);
  until
    SubcomponentByName(result) = -1;

  FLogger._e(result);
end;

function TMapComponentMap.AddSubcomponent2(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddSubcomponent2';
begin
  FLogger._s(METHOD);

  result := -1;
  if ReservePlace(APos, FSubcomponents[High(FSubcomponents)].Size, High(FSubcomponents)) then
  begin
    result := High(FSubcomponents);
  end
  else
  begin
    FreeAndNil(FSubcomponents[High(FSubcomponents)]);
  end;

  FLogger._e(result);
end;

function TMapComponentMap.GetCursorPos(): TMCPos;
const METHOD: string = 'TMapComponentMap.GetCursorPos';
begin
  FLogger._s(METHOD);

  result := FCursorPos;

  FLogger._e();
end;

procedure TMapComponentMap.SetCursorPos(AValue: TMCPos);
const METHOD: string = 'TMapComponentMap.SetCursorPos';
begin
  FLogger._s(METHOD);

  FCursorPos := AValue;

  FLogger._e();
end;

function TMapComponentMap.GetCursorSize(): TMCSize;
const METHOD: string = 'TMapComponentMap.GetCursorSize';
begin
  FLogger._s(METHOD);

  result := FCursorSize;

  FLogger._e();
end;

procedure TMapComponentMap.SetCursorSize(AValue: TMCSize);
const METHOD: string = 'TMapComponentMap.SetCursorSize';
begin
  FLogger._s(METHOD);

  FCursorSize := AValue;

  FLogger._e();
end;

function TMapComponentMap.GetCursorPosLast(): TMCPos;
const METHOD: string = 'TMapComponentMap.GetCursorPosLast';
begin
  FLogger._s(METHOD);

  result := FCursorPosLast;

  FLogger._e();
end;

function TMapComponentMap.GetCursorSizeLast(): TMCSize;
const METHOD: string = 'TMapComponentMap.GetCursorSizeLast';
begin
  FLogger._s(METHOD);

  result := FCursorSizeLast;

  FLogger._e();
end;

procedure TMapComponentMap.SetRedrawField(AValue: TMCPos);
const METHOD: string = '';
begin
  FLogger._s(METHOD);

  FRedrawField := AValue;

  FLogger._e();
end;

(* PROTECTED *)

function TMapComponentMap.GetMCDef(): string;
const METHOD: string = 'TMapComponentMap.GetMCDef';
var x,y: integer;
    s: string;
begin
  FLogger._s(METHOD);

  result := inherited GetMCDef;
  for x := Low(FMap) to High(FMap) do
  begin
    for y := Low(FMap[x]) to High(FMap[x]) do
    begin
      if FMap[x][y] = -1 then
      begin
        continue;
      end;
      result := result + Format(';map="%d,%d,%d"', [x,y,FMap[x][y]]);
    end;
  end;
  for x := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if not Assigned(FSubcomponents[x]) then
    begin
      continue;
    end;
    s := FSubcomponents[x].MCDef;
    result := result + Format(';sub="%d,%d"', [x,Length(s)]);
    result := result + s;
  end;

  FLogger._e();
end;

procedure TMapComponentMap.SetMCDef(const AValue: string);
const METHOD: string = 'TMapComponentMap.SetMCDef';
begin
  FLogger._s(METHOD);

  inherited SetMCDef(AValue);

  FLogger._e();
end;

(* PUBLIC *)

constructor TMapComponentMap.Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
var x,y: integer;
begin
  inherited Create(AName, ASize, APos, AParent);
  Clear;
  SetLength(FMap, ASize.w);
  for x := Low(FMap) to High(FMap) do
  begin
    SetLength(FMap[x], ASize.h);
    for y := Low(FMap[x]) to High(FMap[x]) do
    begin
      FMap[x][y] := -1;
    end;
  end;
end;

destructor TMapComponentMap.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentMap.Clear();
const METHOD: string = 'TMapComponentMap.Clear';
var i: integer;
begin
  FLogger._s(METHOD);

  if Length(FMap) > 0 then
  begin
    for i := Low(FMap) to High(FMap) do
    begin
      SetLength(FMap[i], 0);
    end;
  end;
  SetLength(FMap, 0);
  if Length(FSubcomponents) > 0 then
  begin
    for i := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if not Assigned(FSubcomponents) then
      begin
        continue;
      end;
      FreeAndNil(FSubcomponents[i]);
    end;
  end;
  SetLength(FSubcomponents, 0);

  FLogger._e();
end;

procedure TMapComponentMap.Zero();
const METHOD: string = 'TMapComponentMap.Zero';
var i: integer;
begin
  FLogger._s(METHOD);

  Null();
  for i := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if not Assigned(FSubcomponents[i]) then
    begin
      continue;
    end;
    if FSubcomponents[i] is TMapComponentMap then
    begin
      TMapComponentMap(FSubcomponents[i]).Zero;
    end
    else if FSubcomponents[i] is TMapComponentPinned then
    begin
      TMapComponentPinned(FSubcomponents[i]).Null;
    end
    else if FSubcomponents[i] is TMapComponentIO then
    begin
      TMapComponentIO(FSubcomponents[i]).Value := false;
    end;
  end;

  FLogger._e();
end;

procedure TMapComponentMap.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawMapStyle);
const METHOD: string = 'TMapComponentMap.Draw';
var sglFldW,sglFldH: single;

  function FieldBounds(APos: TMCPos): TRect;
  begin
    result.Create(Round(APos.x*sglFldW),Round(APos.y*sglFldH),Round((APos.x+1)*sglFldW),Round((APos.y+1)*sglFldH));
  end;

  function ComponentBounds(ACmp: TMapComponentBase): TRect;
  begin
    result.Create(Round(ACmp.Pos.x*sglFldW),Round(ACmp.Pos.y*sglFldH),Round((ACmp.Pos.x+ACmp.Size.w)*sglFldW),Round((ACmp.Pos.y+ACmp.Size.h)*sglFldH));
  end;

var x,y,i: integer;
    c: TMapComponentBase;
begin
  FLogger._s(METHOD);

  if Assigned(Parent) then
  begin
    inherited Draw(ACanvas, ARect, mdsNormal);
  end;

  sglFldW := ARect.Width/Size.w;
  sglFldH := ARect.Height/Size.h;

  if AStyle = mdmsComplete then
  begin
    for x := Low(FMap) to High(FMap) do
    begin
      for y := Low(FMap[x]) to High(FMap[x]) do
      begin
        if FieldWithin(MCPos(x,y), FCursorPos, FCursorSize) or FieldWithin(MCPos(x,y), FCursorPosLast, FCursorSizeLast) then
        begin
          TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(x,y)), mdsSelected);
        end
        else
        begin
          TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(x,y)), mdsNormal);
        end;
      end;
    end;
    for x := Low(FSubcomponents) To High(FSubcomponents) do
    begin
      if Assigned(FSubcomponents[x]) then
      begin
        if FSubcomponents[x].Contains(FCursorPos, FCursorSize) or FSubcomponents[x].Contains(FCursorPosLast, FCursorSizeLast) then
        begin
          FSubcomponents[x].Draw(ACanvas, ComponentBounds(FSubcomponents[x]), mdsSelected);
        end
        else
        begin
          FSubcomponents[x].Draw(ACanvas, ComponentBounds(FSubcomponents[x]), mdsNormal);
        end;
      end;
    end;
    FLogger._e();
    Exit;
  end;

  if (AStyle = mdmsSelected) and ((FCursorPosLast <> FCursorPos) or (FCursorSizeLast <> FCursorSize)) then
  begin
    if (FCursorPosLast <> MCPos()) and (FCursorSizeLast <> MCSize()) then
    begin
      for x := 0 to FCursorSizeLast.w-1 do
      begin
        for y := 0 to FCursorSizeLast.h-1 do
        begin
          i := SubcomponentByField(MCPos(FCursorPosLast.x+x, FCursorPosLast.y+y));
          c := Subcomponent(i);
          if Assigned(c) then
          begin
            c.Draw(ACanvas, ComponentBounds(c), mdsNormal);
          end
          else
          begin
            TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(FCursorPosLast.x+x, FCursorPosLast.y+y)), mdsNormal);
          end;
        end;
      end;
    end;

    FCursorPosLast := FCursorPos;
    FCursorSizeLast := FCursorSize;

    if (FCursorPosLast <> MCPos()) and (FCursorSizeLast <> MCSize()) then
    begin
      for x := 0 to FCursorSizeLast.w-1 do
      begin
        for y := 0 to FCursorSizeLast.h-1 do
        begin
          i := SubcomponentByField(MCPos(FCursorPosLast.x+x, FCursorPosLast.y+y));
          c := Subcomponent(i);
          if Assigned(c) then
          begin
            c.Draw(ACanvas, ComponentBounds(c), mdsSelected);
          end
          else
          begin
            TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(FCursorPosLast.x+x, FCursorPosLast.y+y)), mdsSelected);
          end;
        end;
      end;
    end;

    FLogger._e();
    Exit;
  end;

  if AStyle = mdmsRedraw then
  begin
    i := SubcomponentByField(FRedrawField);
    c := Subcomponent(i);
    if Assigned(c) then
    begin
      c.Draw(ACanvas, ComponentBounds(c), mdsNormal);
    end
    else
    begin
      TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(FRedrawField), mdsNormal);
    end;

    FLogger._e();
    Exit;
  end;

  FLogger._e();
end;

procedure TMapComponentMap.Tick();
const METHOD: string = 'TMapComponentMap.Tick';
var i: integer;
begin
  FLogger._s(METHOD);

  for i := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if (not Assigned(FSubcomponents[i])) or (not (FSubcomponents[i] is TMapComponentInput)) then
    begin
      continue;
    end;
    TMapComponentInput(FSubcomponents[i]).Tick();
  end;

  FLogger._e();
end;

procedure TMapComponentMap.RequestTick(AFrom,ATo: TMCPos);
const METHOD: string = 'TMapComponentMap.RequestTick';
var cmpFrom,cmpTo: TMapComponentBase;
    intFrom,intTo: integer;
begin
  FLogger._s(METHOD);

  cmpFrom := Subcomponent(SubcomponentByField(AFrom));
  cmpTo := Subcomponent(SubcomponentByField(ATo));
  if (not Assigned(cmpFrom)) or (not Assigned(cmpTo)) or (not (cmpFrom is TMapComponentPinned)) or (not (cmpTo is TMapComponentPinned)) then
  begin
    FLogger._e();
    Exit;
  end;
  intFrom := TMapComponentPinned(cmpFrom).GetPinIndexFromNeighbourField(ATo);
  intTo := TMapComponentPinned(cmpTo).GetPinIndexFromNeighbourField(AFrom);
  TMapComponentPinned(cmpTo).PinValue[intTo] := TMapComponentPinned(cmpFrom).PinValue[intFrom];
  if cmpTo is TMapComponentWire then
  begin
    TMapComponentWire(cmpTo).Tick(AFrom);
  end
  else if cmpTo is TMapComponentInput then
  begin
    TMapComponentInput(cmpTo).Tick();
  end
  else if cmpTo is TMapComponentOutput then
  begin
    TMapComponentOutput(cmpTo).Tick();
  end
  else if cmpTo is TMapComponentGate then
  begin
    TMapComponentGate(cmpTo).Tick();
  end
  else if cmpTo is TMapComponentMap then
  begin
    TMapComponentMap(cmpTo).Tick();
  end;

  FLogger._e();
end;

function TMapComponentMap.AddWire(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddWire';
var strName: string;
begin
  FLogger._s(METHOD);

  result := -1;

  strName := AddSubcomponent1('w');
  FSubcomponents[High(FSubcomponents)] := TMapComponentWire.Create(strName, APos, self);
  result := AddSubcomponent2(APos);

  FLogger._e(result);
end;

function TMapComponentMap.AddGate(AGate: TMCGate; APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddGate';
var strName: string;
begin
  FLogger._s(METHOD);
  result := -1;

  strName := AddSubcomponent1('g');
  FSubcomponents[High(FSubcomponents)] := TMapComponentGate.Create(AGate, strName, APos, self);
  result := AddSubcomponent2(APos);

  FLogger._e(result);
end;

function TMapComponentMap.AddInput(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddInput';
var strName: string;
begin
  FLogger._s(METHOD);
  result := -1;

  strName := AddSubcomponent1('i');
  FSubcomponents[High(FSubcomponents)] := TMapComponentInput.Create(strName, APos, self);
  result := AddSubcomponent2(APos);

  FLogger._e(result);
end;

function TMapComponentMap.AddOutput(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddOutput';
var strName: string;
begin
  FLogger._s(METHOD);
  result := -1;

  strName := AddSubcomponent1('o');
  FSubcomponents[High(FSubcomponents)] := TMapComponentOutput.Create(strName, APos, self);
  result := AddSubcomponent2(APos);

  FLogger._e(result);
end;

function TMapComponentMap.AddMap(ASize: TMCSize; APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddMap';
var strName: string;
begin
  FLogger._s(METHOD);
  result := -1;

  strName := AddSubcomponent1('m');
  FSubcomponents[High(FSubcomponents)] := TMapComponentMap.Create(strName, ASize, APos, self);
  result := AddSubcomponent2(APos);

  FLogger._e(result);
end;

procedure TMapComponentMap.RemoveSubcomponent(AIndex: integer);
const METHOD: string = 'TMapComponentMap.RemoveSubcomponent';
var x,y: integer;
begin
  FLogger._s(METHOD);

  if (AIndex >= Low(FSubcomponents)) and (AIndex <= High(FSubcomponents)) and Assigned(FSubcomponents[AIndex]) then
  begin
    FreeAndNil(FSubcomponents[AIndex]);
  end;
  for x := Low(FMap) to High(FMap) do
  begin
    for y := Low(FMap[x]) to High(FMap[x]) do
    begin
      if FMap[x][y] = AIndex then
      begin
        FMap[x][y] := -1;
      end;
    end;
  end;

  FLogger._e();
end;

function TMapComponentMap.SubcomponentByName(const AName: string): integer;
const METHOD: string = 'TMapComponentMap.SubcomponentByName';
var i: integer;
begin
  FLogger._s(METHOD);

  result := -1;
  for i := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if Assigned(FSubcomponents[i]) and (FSubcomponents[i].Name = AName) then
    begin
      result := i;
      break;
    end;
  end;
  FLogger._e(result);
end;

function TMapComponentMap.SubcomponentByField(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.SubcomponentByField';
begin
  FLogger._s(METHOD);

  result := -1;
  if (APos.x >= Low(FMap)) and (APos.x <= High(FMap)) and (APos.y >= Low(FMap[APos.x])) and (APos.y <= High(FMap[APos.x])) then
  begin
    result := FMap[APos.x][APos.y];
  end;

  FLogger._e(result);
end;

function TMapComponentMap.Subcomponent(AIndex: integer): TMapComponentBase;
const METHOD: string = 'TMapComponentMap.Subcomponent';
begin
  FLogger._s(METHOD);

  result := nil;
  if (AIndex >= Low(FSubcomponents)) and (AIndex <= High(FSubcomponents)) then
  begin
    result := FSubcomponents[AIndex];
  end;

  FLogger._e();
end;

function TMapComponentMap.InputString(const AString: string): boolean;
const METHOD: string = 'TMapComponentMap.InputString';
var i: integer;
    w: word;
begin
  FLogger._s(METHOD);

  result := false;

  w := 0;
  for i := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if Assigned(FSubcomponents[i]) and (FSubcomponents[i] is TMapComponentInput) then
    begin
      Inc(w);
    end;
  end;

  if Length(AString) <> w then
  begin
    FLogger._e(result);
    Exit;
  end;

  w := 1;
  for i := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if Assigned(FSubcomponents[i]) and (FSubcomponents[i] is TMapComponentInput) then
    begin
      TMapComponentInput(FSubcomponents[i]).Value := (AString[w] = '1');
      Inc(w);
    end;
  end;

  result := true;

  FLogger._e(result);
end;

function TMapComponentMap.OutputString(): string;
const METHOD: string = 'TMapComponentMap.OutputString';
var i: integer;
begin
  FLogger._s(METHOD);

  result := '';
  for i := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if Assigned(FSubcomponents[i]) and (FSubcomponents[i] is TMapComponentOutput) then
    begin
      result := result + BoolToStr(TMapComponentOutput(FSubcomponents[i]).Value, '1', '0');
    end;
  end;

  FLogger._e(result);
end;

end.

