unit umapcomponentmap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, RegExpr, ulogger, umapcomponentwire,
  umapcomponentgate, umapcomponentinput, umapcomponentoutput, umapcomponentio,
  umapcomponentpinned, umapcomponentbase;

type
  TMCType = (mctMap, mctInput, mctOutput, mctIO, mctGate, mctWire, mctPinned, mctBase);
  TMCTypes = set of TMCType;
  TMCDrawMapStyle = (mdmsComplete,mdmsSelected,mdmsRedraw);
  TMCIOConnection = record
    intPin: integer;
    intCmp: integer;
  end;
  TMapComponentMap = class(TMapComponentPinned)

    private

      var FSubcomponents: array of TMapComponentBase;
      var FCursorPos: TMCPos; //current position (updated on mouseover)
      var FCursorPosLast: TMCPos; //position since last selection (updated on click)
      var FCursorSize: TMCSize; //
      var FCursorSizeLast: TMCSize; //
      var FRedrawField: TMCPos;

      function ReservePlace(APos: TMCPos; ASize: TMCSize; AExistingCmp: integer = -1; AMapEdit: boolean = true): boolean;
      function AddSubcomponent1(const APrefix: string): string;
      function AddSubcomponent2(APos: TMCPos): integer;

      function GetCursorPos(): TMCPos;
      procedure SetCursorPos(AValue: TMCPos);
      function GetCursorSize(): TMCSize;
      procedure SetCursorSize(AValue: TMCSize);
      function GetCursorPosLast(): TMCPos;
      function GetCursorSizeLast(): TMCSize;
      procedure SetRedrawField(AValue: TMCPos);

    public

      property CursorPos: TMCPos read GetCursorPos write SetCursorPos;
      property CursorPosLast: TMCPos read GetCursorPosLast;
      property CursorSize: TMCSize read GetCursorSize write SetCursorSize;
      property CursorSizeLast: TMCSize read GetCursorSizeLast;
      property RedrawField: TMCPos write SetRedrawField;

      constructor Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear(ABuildMap: boolean);
      procedure Zero();
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawMapStyle; AFieldStyle: TMCDrawStyle); reintroduce;
      procedure Tick();
      procedure RequestTick(AFrom,ATo: TMCPos); override;
      function GetMCDef(ALevel: byte): string; override;
      procedure SetMCDef(ALevel: byte; const AValue: string; AAddToPos: TMCPos); override;

      function AddWire(APos: TMCPos): integer;
      function AddGate(AGate: TMCGate; APos: TMCPos): integer;
      function AddInput(APos: TMCPos): integer;
      function AddOutput(APos: TMCPos): integer;
      function AddMap(ASize: TMCSize; APos: TMCPos; const AMCDef: string): integer;
      procedure RemoveSubcomponent(AIndex: integer);

      function SubcomponentByName(const AName: string): integer;
      function SubcomponentByField(APos: TMCPos): integer;
      function Subcomponent(AIndex: integer): TMapComponentBase;
      function SubcomponentCount(): integer;

      function InputString(const AString: string): boolean;
      function InputLength(): integer;
      function OutputString(): string;

  end;

function MCTypeInt(ACmp: TMapComponentBase): integer;
function MCTypeIntToStr(AInt: integer): string;
function MCTypeSet(ACmp: TMapComponentBase): TMCTypes;

implementation

(* GLOBAL *)

function MCTypeIntToStr(AInt: integer): string;
begin
  result := '';
  if AInt = Integer(mctMap) then
  begin
    result := 'Map';
    Exit;
  end;
  if AInt = Integer(mctInput) then
  begin
    result := 'Input';
    Exit;
  end;
  if AInt = Integer(mctOutput) then
  begin
    result := 'Output';
    Exit;
  end;
  if AInt = Integer(mctIO) then
  begin
    result := 'IO';
    Exit;
  end;
  if AInt = Integer(mctGate) then
  begin
    result := 'Gate';
    Exit;
  end;
  if AInt = Integer(mctWire) then
  begin
    result := 'Wire';
    Exit;
  end;
  if AInt = Integer(mctPinned) then
  begin
    result := 'Pinned';
    Exit;
  end;
  if AInt = Integer(mctBase) then
  begin
    result := 'Base';
    Exit;
  end;
end;

function MCTypeInt(ACmp: TMapComponentBase): integer;
var mct: TMCTypes;
begin
  result := -1;
  mct := MCTypeSet(ACmp);

  if mctMap in mct then
  begin
    result := Integer(mctMap);
    Exit;
  end;
  if mctGate in mct then
  begin
    result := Integer(mctGate);
    Exit;
  end;
  if mctWire in mct then
  begin
    result := Integer(mctWire);
    Exit;
  end;
  if mctInput in mct then
  begin
    result := Integer(mctInput);
    Exit;
  end;
  if mctOutput in mct then
  begin
    result := Integer(mctOutput);
    Exit;
  end;
  if mctIO in mct then
  begin
    result := Integer(mctIO);
    Exit;
  end;
  if mctPinned in mct then
  begin
    result := Integer(mctPinned);
    Exit;
  end;
  if mctBase in mct then
  begin
    result := Integer(mctBase);
    Exit;
  end;
end;

function MCTypeSet(ACmp: TMapComponentBase): TMCTypes;
begin
  result := [];
  if ACmp is TMapComponentMap then
  begin
    result := result + [mctMap];
  end;
  if ACmp is TMapComponentGate then
  begin
    result := result + [mctGate];
  end;
  if ACmp is TMapComponentWire then
  begin
    result := result + [mctWire];
  end;
  if ACmp is TMapComponentInput then
  begin
    result := result + [mctInput];
  end;
  if ACmp is TMapComponentOutput then
  begin
    result := result + [mctOutput];
  end;
  if ACmp is TMapComponentIO then
  begin
    result := result + [mctIO];
  end;
  if ACmp is TMapComponentPinned then
  begin
    result := result + [mctPinned];
  end;
  if ACmp is TMapComponentBase then
  begin
    result := result + [mctBase];
  end;
end;

(* PRIVATE *)

function TMapComponentMap.ReservePlace(APos: TMCPos; ASize: TMCSize; AExistingCmp: integer = -1; AMapEdit: boolean = true): boolean;
const METHOD: string = 'TMapComponentMap.ReservePlace';
var x,y,i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._pe('ASize', TypeInfo(ASize), @ASize);
    FLogger._pe('AExistingCmp', TypeInfo(AExistingCmp), @AExistingCmp);
    FLogger._pe('AMapEdit', TypeInfo(AMapEdit), @AMapEdit);
    FLogger._se();

    result := false;

    for i := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if (not Assigned(FSubcomponents[i])) or (i = AExistingCmp) then
      begin
        continue;
      end;
      if FSubcomponents[i].Contains(APos, ASize) then
      begin
        Exit;
      end;
    end;

    result := true;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.AddSubcomponent1(const APrefix: string): string;
const METHOD: string = 'TMapComponentMap.AddSubcomponent1';
var intName: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APrefix', TypeInfo(APrefix), @APrefix);
    FLogger._se();

    result := '';
    SetLength(FSubcomponents, Length(FSubcomponents)+1);
    intName := 0;
    repeat
      result := Format('%s%d', [APrefix, intName]);
      Inc(intName);
    until
      SubcomponentByName(result) = -1;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.AddSubcomponent2(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddSubcomponent2';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._se();

    result := -1;
    if ReservePlace(APos, FSubcomponents[High(FSubcomponents)].Size, High(FSubcomponents)) then
    begin
      result := High(FSubcomponents);
    end
    else
    begin
      FreeAndNil(FSubcomponents[High(FSubcomponents)]);
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.GetCursorPos(): TMCPos;
const METHOD: string = 'TMapComponentMap.GetCursorPos';
begin
  FLogger._s(METHOD);
  try

    result := FCursorPos;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentMap.SetCursorPos(AValue: TMCPos);
const METHOD: string = 'TMapComponentMap.SetCursorPos';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FCursorPos := AValue;

  finally
    FLogger._e();
  end;
end;

function TMapComponentMap.GetCursorSize(): TMCSize;
const METHOD: string = 'TMapComponentMap.GetCursorSize';
begin
  FLogger._s(METHOD);
  try

    result := FCursorSize;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentMap.SetCursorSize(AValue: TMCSize);
const METHOD: string = 'TMapComponentMap.SetCursorSize';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FCursorSize := AValue;

  finally
    FLogger._e();
  end;
end;

function TMapComponentMap.GetCursorPosLast(): TMCPos;
const METHOD: string = 'TMapComponentMap.GetCursorPosLast';
begin
  FLogger._s(METHOD);
  try

    result := FCursorPosLast;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.GetCursorSizeLast(): TMCSize;
const METHOD: string = 'TMapComponentMap.GetCursorSizeLast';
begin
  FLogger._s(METHOD);
  try

    result := FCursorSizeLast;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentMap.SetRedrawField(AValue: TMCPos);
const METHOD: string = '';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FRedrawField := AValue;

  finally
    FLogger._e();
  end;
end;

(* PROTECTED *)

function TMapComponentMap.GetMCDef(ALevel: byte{; AIgnorePos: boolean = false}): string;
const METHOD: string = 'TMapComponentMap.GetMCDef';
var x,y: integer;
    s: string;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
    FLogger._se();

    result := inherited GetMCDef(ALevel{, AIgnorePos});

    for x := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if not Assigned(FSubcomponents[x]) then
      begin
        continue;
      end;
      s := #13#10+FSubcomponents[x].GetMCDef(ALevel+1);
      result := result + Format(#13#10'%ssub="%d,%d"', [LevelStr(ALevel),Length(s),MCTypeInt(FSubcomponents[x])]);
      result := result + s;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentMap.SetMCDef(ALevel: byte; const AValue: string; AAddToPos: TMCPos);
const METHOD: string = 'TMapComponentMap.SetMCDef';
var strDef: string;
    re: TRegExpr;
    intIdx,intLen,intType,i,x,y,iW,iH,intSubOffset: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._pe('AAddToPos', TypeInfo(AAddToPos), @AAddToPos);
    FLogger._se();

    iW := Size.w;
    iH := Size.h;

    if AAddToPos = MCPos() then
    begin
      inherited SetMCDef(ALevel, AValue, AAddToPos);
    end;

    re := TRegExpr.Create;
    re.ModifierM := true;
    try
      if AAddToPos = MCPos() then
      begin
        SetLength(FSubcomponents, 0);
      end;

      re.Expression := LevelPrefix(ALevel)+'sub="([^,]+),([^"]+)"';
      if re.Exec(AValue) then
      begin
        repeat
          intLen := StrToIntDef(re.Match[1], 0);
          intType := StrToIntDef(re.Match[2], -1);

          if (intLen >= 1) and (intType >= 0) then
          begin
            strDef := copy(AValue, re.MatchPos[0]+re.MatchLen[0], intLen);
            case intType of
              Integer(mctWire):
              begin
                SetLength(FSubcomponents, Length(FSubcomponents)+1);
                FSubcomponents[High(FSubcomponents)] := TMapComponentWire.Create('', MCPos(), self);
                TMapComponentWire(FSubcomponents[High(FSubcomponents)]).SetMCDef(ALevel+1,strDef,AAddToPos);
              end;
              Integer(mctInput):
              begin
                SetLength(FSubcomponents, Length(FSubcomponents)+1);
                FSubcomponents[High(FSubcomponents)] := TMapComponentInput.Create('', MCPos(), self);
                TMapComponentInput(FSubcomponents[High(FSubcomponents)]).SetMCDef(ALevel+1,strDef,AAddToPos);
              end;
              Integer(mctOutput):
              begin
                SetLength(FSubcomponents, Length(FSubcomponents)+1);
                FSubcomponents[High(FSubcomponents)] := TMapComponentOutput.Create('', MCPos(), self);
                TMapComponentOutput(FSubcomponents[High(FSubcomponents)]).SetMCDef(ALevel+1,strDef,AAddToPos);
              end;
              Integer(mctGate):
              begin
                SetLength(FSubcomponents, Length(FSubcomponents)+1);
                FSubcomponents[High(FSubcomponents)] := TMapComponentGate.Create(mcgNone, '', MCPos(), self);
                TMapComponentGate(FSubcomponents[High(FSubcomponents)]).SetMCDef(ALevel+1,strDef,AAddToPos);
              end;
              else
              begin
                FLogger._(ltWarning, 'Unsupported component type');
              end;
            end;
          end;
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

constructor TMapComponentMap.Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create(AName, ASize, APos, AParent);
  Clear(true);
end;

destructor TMapComponentMap.Destroy();
begin
  Clear(false);
  inherited Destroy;
end;

procedure TMapComponentMap.Clear(ABuildMap: boolean);
const METHOD: string = 'TMapComponentMap.Clear';
var i,x,y: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ABuildMap', TypeInfo(ABuildMap), @ABuildMap);
    FLogger._se();

    if Length(FSubcomponents) > 0 then
    begin
      for i := Low(FSubcomponents) to High(FSubcomponents) do
      begin
        if not Assigned(FSubcomponents[i]) then
        begin
          continue;
        end;
        FreeAndNil(FSubcomponents[i]);
      end;
    end;
    SetLength(FSubcomponents, 0);
    FCursorPos := MCPos();
    FCursorPosLast := MCPos();
    FCursorSize := MCSize();
    FCursorSizeLast := MCSize();
    FRedrawField := MCPos();

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentMap.Zero();
const METHOD: string = 'TMapComponentMap.Zero';
var i: integer;
begin
  FLogger._s(METHOD);
  try

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

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentMap.Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawMapStyle; AFieldStyle: TMCDrawStyle);
const METHOD: string = 'TMapComponentMap.Draw';
var sglFldW,sglFldH: single;

  function FieldBounds(APos: TMCPos; ASize: TMCSize): TRect;
  begin
    result.Create(ARect.Left+Round((APos.x)*sglFldW),ARect.Top+Round((APos.y)*sglFldH),ARect.Left+Round((APos.x+ASize.w)*sglFldW),ARect.Top+Round((APos.y+ASize.h)*sglFldH));
  end;

  function ComponentBounds(ACmp: TMapComponentBase): TRect;
  begin
    result := TRect.Empty;
    if not Assigned(ACmp) then
    begin
      Exit;
    end;
    result := FieldBounds(ACmp.Pos, ACmp.Size);
  end;

  procedure DrawComponent(ACmp: TMapComponentBase; ADCStyle: TMCDrawStyle);
  begin
    if not Assigned(ACmp) then
    begin
      FLogger._(ltInfo, 'Cannot draw undefined component.');
      Exit;
    end;

    if ACmp is TMapComponentMap then
    begin
      TMapComponentMap(ACmp).Draw(ACanvas, ComponentBounds(ACmp), mdmsComplete, ADCStyle);
      Exit;
    end;
    if ACmp is TMapComponentInput then
    begin
      TMapComponentInput(ACmp).Draw(ACanvas, ComponentBounds(ACmp), ADCStyle);
      Exit;
    end;
    if ACmp is TMapComponentOutput then
    begin
      TMapComponentOutput(ACmp).Draw(ACanvas, ComponentBounds(ACmp), ADCStyle);
      Exit;
    end;
    if ACmp is TMapComponentIO then
    begin
      TMapComponentIO(ACmp).Draw(ACanvas, ComponentBounds(ACmp), ADCStyle);
      Exit;
    end;
    if ACmp is TMapComponentGate then
    begin
      TMapComponentGate(ACmp).Draw(ACanvas, ComponentBounds(ACmp), ADCStyle);
      Exit;
    end;
    if ACmp is TMapComponentWire then
    begin
      TMapComponentWire(ACmp).Draw(ACanvas, ComponentBounds(ACmp), ADCStyle);
      Exit;
    end;
    if ACmp is TMapComponentPinned then
    begin
      TMapComponentPinned(ACmp).Draw(ACanvas, ComponentBounds(ACmp), ADCStyle);
      Exit;
    end;
    if ACmp is TMapComponentBase then
    begin
      ACmp.Draw(ACanvas, ComponentBounds(ACmp), ADCStyle);
      Exit;
    end;
    FLogger._(ltWarning, 'Unknown TMapComponent* class '+ACmp.ClassName);
  end;

var x,y,i: integer;
    c: TMapComponentBase;
    rctPin,rctCmp: TRect;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ACanvas', TypeInfo(ACanvas), @ACanvas);
    FLogger._pe('ARect', TypeInfo(ARect), @ARect);
    FLogger._pe('AStyle', TypeInfo(AStyle), @AStyle);
    FLogger._pe('AFieldStyle', TypeInfo(AFieldStyle), @AFieldStyle);
    FLogger._se();

    sglFldW := ARect.Width/Size.w;
    sglFldH := ARect.Height/Size.h;

    if AStyle = mdmsComplete then
    begin
      //Repaint all fields as empty fields
      for x := 0 to Size.w-1 do
      begin
        for y := 0 to Size.h-1 do
        begin
          if Assigned(Parent) then
          begin
            TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(x,y), MCSize(1,1)), AFieldStyle);
          end
          else if FieldWithin(MCPos(x,y), FCursorPos, FCursorSize) or FieldWithin(MCPos(x,y), FCursorPosLast, FCursorSizeLast) then
          begin
            TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(x,y), MCSize(1,1)), mdsSelected);
          end
          else
          begin
            TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(x,y), MCSize(1,1)), mdsNormal);
          end;
        end;
      end;
      //Paint subcomponents
      for x := Low(FSubcomponents) To High(FSubcomponents) do
      begin
        if Assigned(FSubcomponents[x]) then
        begin
          if Assigned(Parent) then
          begin
            DrawComponent(FSubcomponents[x], AFieldStyle);
          end
          else if FSubcomponents[x].Contains(FCursorPos, FCursorSize) or FSubcomponents[x].Contains(FCursorPosLast, FCursorSizeLast) then
          begin
            DrawComponent(FSubcomponents[x], mdsSelected);
          end
          else
          begin
            DrawComponent(FSubcomponents[x], mdsNormal);
          end;
        end;
      end;

      DrawPinsOnly(ACanvas, ARect);

      Exit;
    end;

  if AStyle = mdmsSelected then
  begin
    if (FCursorPosLast = FCursorPos) and (FCursorSizeLast = FCursorSize) then
    begin
      Exit;
    end;

    //Repaint last mouseover position
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
            DrawComponent(c, mdsNormal);
          end
          else
          begin
            TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(FCursorPosLast.x+x, FCursorPosLast.y+y), MCSize(1,1)), mdsNormal);
          end;
        end;
      end;
    end;

    //Set mouseover position to a new one
    FCursorPosLast := FCursorPos;
    FCursorSizeLast := FCursorSize;

    //Repaint new mouseover position
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
            DrawComponent(c, mdsSelected);
          end
          else
          begin
            TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(MCPos(FCursorPosLast.x+x, FCursorPosLast.y+y), MCSize(1,1)), mdsSelected);
          end;
        end;
      end;
    end;

    Exit;
  end;

  if AStyle = mdmsRedraw then
  begin
    i := SubcomponentByField(FRedrawField);
    c := Subcomponent(i);
    if Assigned(c) then
    begin
      DrawComponent(c, mdsNormal);
    end
    else
    begin
      TMapComponentBase.DrawEmpty(ACanvas, FieldBounds(FRedrawField, MCSize(1,1)), mdsNormal);
    end;

    Exit;
  end;

  FLogger._(ltWarning, 'Unknown map draw style');

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentMap.Tick();
const METHOD: string = 'TMapComponentMap.Tick';
var i: integer;
begin
  FLogger._s(METHOD);
  try

    for i := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if (not Assigned(FSubcomponents[i])) or (not (FSubcomponents[i] is TMapComponentInput)) then
      begin
        continue;
      end;
      TMapComponentInput(FSubcomponents[i]).Tick();
    end;

  finally
    FLogger._e();
  end;
end;

procedure TMapComponentMap.RequestTick(AFrom,ATo: TMCPos);
const METHOD: string = 'TMapComponentMap.RequestTick';
var cmpFrom,cmpTo: TMapComponentBase;
    intFrom,intTo,intTmp: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AFrom', TypeInfo(AFrom), @AFrom);
    FLogger._pe('ATo', TypeInfo(ATo), @ATo);
    FLogger._se();

    cmpFrom := Subcomponent(SubcomponentByField(AFrom));
    cmpTo := Subcomponent(SubcomponentByField(ATo));
    if (not Assigned(cmpFrom)) or (not Assigned(cmpTo)) then
    begin
      FLogger._(ltInfo, 'Source/target component not assigned');
      Exit;
    end;

    if (not (cmpFrom is TMapComponentPinned)) or (not (cmpTo is TMapComponentPinned)) then
    begin
      FLogger._(ltInfo, 'Source/target component not a descendant of TMapComponentPinned');
      Exit;
    end;

    intFrom := TMapComponentPinned(cmpFrom).GetPinIndexFromNeighbourField(ATo);
    intTo := TMapComponentPinned(cmpTo).GetPinIndexFromNeighbourField(AFrom);
    TMapComponentPinned(cmpTo).PinValue[intTo] := TMapComponentPinned(cmpFrom).PinValue[intFrom];
    if cmpTo is TMapComponentWire then
    begin
      TMapComponentWire(cmpTo).Tick(intTo);
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

  finally
    FLogger._e();
  end;
end;

function TMapComponentMap.AddWire(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddWire';
var strName: string;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._se();

    result := -1;

    strName := AddSubcomponent1('w');
    FSubcomponents[High(FSubcomponents)] := TMapComponentWire.Create(strName, APos, self);
    result := AddSubcomponent2(APos);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.AddGate(AGate: TMCGate; APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddGate';
var strName: string;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AGate', TypeInfo(AGate), @AGate);
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._se();

    result := -1;

    strName := AddSubcomponent1('g');
    FSubcomponents[High(FSubcomponents)] := TMapComponentGate.Create(AGate, strName, APos, self);
    result := AddSubcomponent2(APos);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.AddInput(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddInput';
var strName: string;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._se();

    result := -1;

    strName := AddSubcomponent1('i');
    FSubcomponents[High(FSubcomponents)] := TMapComponentInput.Create(strName, APos, self);
    result := AddSubcomponent2(APos);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.AddOutput(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.AddOutput';
var strName: string;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._se();

    result := -1;

    strName := AddSubcomponent1('o');
    FSubcomponents[High(FSubcomponents)] := TMapComponentOutput.Create(strName, APos, self);
    result := AddSubcomponent2(APos);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.AddMap(ASize: TMCSize; APos: TMCPos; const AMCDef: string): integer;
const METHOD: string = 'TMapComponentMap.AddMap';
var strName: string;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ASize', TypeInfo(ASize), @ASize);
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._pe('AMCDef', TypeInfo(AMCDef), @AMCDef);
    FLogger._se();

    result := -1;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentMap.RemoveSubcomponent(AIndex: integer);
const METHOD: string = 'TMapComponentMap.RemoveSubcomponent';
var x,y: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AIndex', TypeInfo(AIndex), @AIndex);
    FLogger._se();

    if (AIndex >= Low(FSubcomponents)) and (AIndex <= High(FSubcomponents)) and Assigned(FSubcomponents[AIndex]) then
    begin
      FreeAndNil(FSubcomponents[AIndex]);
    end;

  finally
    FLogger._e();
  end;
end;

function TMapComponentMap.SubcomponentByName(const AName: string): integer;
const METHOD: string = 'TMapComponentMap.SubcomponentByName';
var i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AName', TypeInfo(AName), @AName);
    FLogger._se();

    result := -1;
    for i := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if Assigned(FSubcomponents[i]) and (FSubcomponents[i].Name = AName) then
      begin
        result := i;
        break;
      end;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.SubcomponentByField(APos: TMCPos): integer;
const METHOD: string = 'TMapComponentMap.SubcomponentByField';
var i: integer;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('APos', TypeInfo(APos), @APos);
    FLogger._se();

    result := -1;
    for i := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if not Assigned(FSubcomponents[i]) then
      begin
        continue;
      end;
      if FSubcomponents[i].Contains(APos, MCSize(1,1)) then
      begin
        result := i;
        break;
      end;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.Subcomponent(AIndex: integer): TMapComponentBase;
const METHOD: string = 'TMapComponentMap.Subcomponent';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AIndex', TypeInfo(AIndex), @AIndex);
    FLogger._se();

    result := nil;
    if (AIndex >= Low(FSubcomponents)) and (AIndex <= High(FSubcomponents)) then
    begin
      result := FSubcomponents[AIndex];
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.SubcomponentCount(): integer;
const METHOD: string = 'TMapComponentMap.InputString';
begin
  FLogger._s(METHOD);
  try

    result := Length(FSubcomponents);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.InputString(const AString: string): boolean;
const METHOD: string = 'TMapComponentMap.InputString';
var i: integer;
    b: boolean;
    c: TMapComponentBase;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AString', TypeInfo(AString), @AString);
    FLogger._se();

    result := false;

    if Length(AString) <> InputLength() then
    begin
      FLogger._(ltInfo, 'Parameter must be %d chars long, is %d chars long', [InputLength(), Length(AString)]);
      Exit;
    end;

    b := true;
    for i := 0 to Length(AString)-1 do
    begin
      c := Subcomponent(SubcomponentByName(Format('i%d', [i])));
      if (not Assigned(c)) or (not (c is TMapComponentInput)) then
      begin
        b := false;
        break;
      end;
    end;

    if not b then
    begin
      FLogger._(ltInfo, 'Expecting input components i0..i%d, but some are missing or have a wrong type.', [InputLength()-1]);
      Exit;
    end;

    result := true;
    for i := Length(AString) downto 1 do
    begin
      c := Subcomponent(SubcomponentByName(Format('i%d', [Length(AString)-i])));
      if (not Assigned(c)) or (not (c is TMapComponentInput)) then
      begin
        result := false;
        break;
      end;
      TMapComponentInput(c).Value := (AString[i] = '1');
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.InputLength(): integer;
const METHOD: string = 'TMapComponentMap.InputLength';
var i: integer;
begin
  FLogger._s(METHOD);
  try

    result := 0;
    for i := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if Assigned(FSubcomponents[i]) and (FSubcomponents[i] is TMapComponentInput) then
      begin
        Inc(result);
      end;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

function TMapComponentMap.OutputString(): string;
const METHOD: string = 'TMapComponentMap.OutputString';
var i: integer;
begin
  FLogger._s(METHOD);
  try

    result := '';
    for i := Low(FSubcomponents) to High(FSubcomponents) do
    begin
      if Assigned(FSubcomponents[i]) and (FSubcomponents[i] is TMapComponentOutput) then
      begin
        result := BoolToStr(TMapComponentOutput(FSubcomponents[i]).Value, '1', '0') + result;
      end;
    end;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

end.

