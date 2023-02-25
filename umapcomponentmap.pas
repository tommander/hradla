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
      var FMap: array of array of integer;
      var FIOConnections: array of TMCIOConnection;
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

    protected


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
      procedure Draw(ACanvas: TCanvas; ARect: TRect; AStyle: TMCDrawMapStyle); reintroduce;
      procedure Tick();
      procedure RequestTick(AFrom,ATo: TMCPos); override;
      function GetMCDef(ALevel: byte; AIgnorePos: boolean = false): string; override;
      procedure SetMCDef(ALevel: byte; const AValue: string); override;

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
      function OutputString(): string;

      procedure AddIOConnection(APin,ACmp: integer);
      procedure RemoveIOConnection(APin,ACmp: integer);

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

  if AMapEdit then
  begin
    for x := APos.x to APos.x+ASize.w-1 do
    begin
      for y := APos.y to APos.y+ASize.h-1 do
      begin
        FMap[x][y] := AExistingCmp;
      end;
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

function TMapComponentMap.GetMCDef(ALevel: byte; AIgnorePos: boolean = false): string;
const METHOD: string = 'TMapComponentMap.GetMCDef';
var x,y: integer;
    s: string;
begin
  FLogger._s(METHOD);

  result := inherited GetMCDef(ALevel, AIgnorePos);

//  result := result + Format(#13#10'maps="%d,%d"', [Size.w, Size.h]);
  for x := Low(FMap) to High(FMap) do
  begin
    for y := Low(FMap[x]) to High(FMap[x]) do
    begin
      if FMap[x][y] = -1 then
      begin
        continue;
      end;
      result := result + Format(#13#10'%smap="%d,%d,%d"', [LevelStr(ALevel),x,y,FMap[x][y]]);
    end;
  end;

  result := result + Format(#13#10'%ssubs="%d"', [LevelStr(ALevel),Length(FSubcomponents)]);
  for x := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if not Assigned(FSubcomponents[x]) then
    begin
      continue;
    end;
    s := #13#10+FSubcomponents[x].GetMCDef(ALevel+1);
    result := result + Format(#13#10'%ssub="%d,%d,%d"', [LevelStr(ALevel),x,Length(s),MCTypeInt(FSubcomponents[x])]);
    result := result + s;
  end;

  result := result + Format(#13#10'%sioconns="%d"', [LevelStr(ALevel),Length(FIOConnections)]);
  for x := Low(FIOConnections) to High(FIOConnections) do
  begin
    if (FIOConnections[x].intCmp < Low(FSubcomponents)) or (FIOConnections[x].intCmp > High(FSubcomponents)) or
       (FIOConnections[x].intPin < PinLow()) or (FIOConnections[x].intPin > PinHigh()) then
    begin
      continue;
    end;
    result := result + Format(#13#10'%sioconn="%d,%d,%d"', [LevelStr(ALevel),x,FIOConnections[x].intPin,FIOConnections[x].intCmp]);
  end;


  FLogger._e();
end;

procedure TMapComponentMap.SetMCDef(ALevel: byte; const AValue: string);
const METHOD: string = 'TMapComponentMap.SetMCDef';
var strDef: string;
    re: TRegExpr;
    intIdx,intLen,intType,i,x,y,iW,iH: integer;
begin
  FLogger._s(METHOD);

  iW := Size.w;
  iH := Size.h;

  inherited SetMCDef(ALevel, AValue);

  if (iw <> Size.w) or (iH <> Size.h) then
  begin
    SetLength(FMap, Size.w);
    for x := Low(FMap) to High(FMap) do
    begin
      SetLength(FMap[x], Size.h);
      for y := Low(FMap[x]) to High(FMap[x]) do
      begin
        FMap[x][y] := -1;
      end;
    end;
  end;

  re := TRegExpr.Create;
  re.ModifierM := true;
  try
    re.Expression := LevelPrefix(ALevel)+'subs="([^"]+)"';
    if re.Exec(AValue) then
    begin
      intLen := StrToIntDef(re.Match[1], 0);
      SetLength(FSubcomponents, intLen);
      for i := Low(FSubcomponents) to High(FSubcomponents) do
      begin
        FSubcomponents[i] := nil;
      end;
    end;

    re.Expression := LevelPrefix(ALevel)+'sub="([^,]+),([^,]+),([^"]+)"';
    if re.Exec(AValue) then
    begin
      repeat
        intIdx := StrToIntDef(re.Match[1], -1);
        intLen := StrToIntDef(re.Match[2], 0);
        intType := StrToIntDef(re.Match[3], -1);

        if (intIdx >= Low(FSubcomponents)) and (intIdx <= High(FSubcomponents)) and (not Assigned(FSubcomponents[intIdx])) and (intLen >= 1) and (intType >= 0) then
        begin
          strDef := copy(AValue, re.MatchPos[0]+re.MatchLen[0], intLen);
          case intType of
            Integer(mctWire):
            begin
              FSubcomponents[intIdx] := TMapComponentWire.Create('', MCPos(), self);
              TMapComponentWire(FSubcomponents[intIdx]).SetMCDef(ALevel+1,strDef);
            end;
            Integer(mctInput):
            begin
              FSubcomponents[intIdx] := TMapComponentInput.Create('', MCPos(), self);
              TMapComponentInput(FSubcomponents[intIdx]).SetMCDef(ALevel+1,strDef);
            end;
            Integer(mctOutput):
            begin
              FSubcomponents[intIdx] := TMapComponentOutput.Create('', MCPos(), self);
              TMapComponentOutput(FSubcomponents[intIdx]).SetMCDef(ALevel+1,strDef);
            end;
            Integer(mctGate):
            begin
              FSubcomponents[intIdx] := TMapComponentGate.Create(mcgNone, '', MCPos(), self);
              TMapComponentGate(FSubcomponents[intIdx]).SetMCDef(ALevel+1,strDef);
            end;
            Integer(mctMap):
            begin
              FSubcomponents[intIdx] := TMapComponentMap.Create('', MCSize(), MCPos(), self);
              TMapComponentMap(FSubcomponents[intIdx]).SetMCDef(ALevel+1,strDef);
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

    re.Expression := LevelPrefix(ALevel)+'map="([^,]+),([^,]+),([^"]+)"';
    if re.Exec(AValue) then
    begin
      repeat
        x := StrToIntDef(re.Match[1], -1);
        y := StrToIntDef(re.Match[2], -1);
        i := StrToIntDef(re.Match[3], -1);

        if (x >= Low(FMap)) and (x <= High(FMap)) then
        begin
          if (y >= Low(FMap[x])) and (y <= High(FMap[x])) then
          begin
            if (i >= Low(FSubcomponents)) and (i <= High(FSubcomponents)) then
            begin
              FMap[x][y] := i;
            end;
          end;
        end;
      until
        not re.ExecNext;
    end;

    re.Expression := LevelPrefix(ALevel)+'ioconns="([^"]+)"';
    if re.Exec(AValue) then
    begin
      i := StrToIntDef(re.Match[1], -1);
      if i >= 0 then
      begin
        SetLength(FIOConnections, i);
        for x := Low(FIOConnections) to High(FIOConnections) do
        begin
          FIOConnections[x].intPin := -1;
          FIOConnections[x].intCmp := -1;
        end;
      end;
    end;

    re.Expression := LevelPrefix(ALevel)+'ioconn="([^,]+),([^,]+),([^"]+)"';
    if re.Exec(AValue) then
    begin
      repeat
        i := StrToIntDef(re.Match[1], -1);
        x := StrToIntDef(re.Match[2], -1);
        y := StrToIntDef(re.Match[3], -1);

        if (i >= Low(FIOConnections)) and (i <= High(FIOConnections)) and
           (x >= PinLow()) and (x <= PinHigh()) and
           (y >= Low(FSubcomponents)) and (y <= High(FSubcomponents))then
        begin
          FIOConnections[i].intPin := x;
          FIOConnections[i].intCmp := y;
        end;
      until
        not re.ExecNext;
    end;

  finally
    re.Free;
  end;

  FLogger._e();
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
  FLogger._s(METHOD);

  if Length(FMap) > 0 then
  begin
    for i := Low(FMap) to High(FMap) do
    begin
      SetLength(FMap[i], 0);
    end;
  end;
  SetLength(FMap, 0);

  if ABuildMap then
  begin
    SetLength(FMap, Size.w);
    for x := Low(FMap) to High(FMap) do
    begin
      SetLength(FMap[x], Size.h);
      for y := Low(FMap[x]) to High(FMap[x]) do
      begin
        FMap[x][y] := -1;
      end;
    end;
  end;

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

  function FieldBounds(APos: TMCPos; ASize: TMCSize): TRect;
  begin
    result.Create(ARect.Left+Round((APos.x)*sglFldW),ARect.Top+Round((APos.y)*sglFldH),ARect.Left+Round((APos.x+ASize.w)*sglFldW),ARect.Top+Round((APos.y+ASize.h)*sglFldH));
  end;

  function ComponentBounds(ACmp: TMapComponentBase): TRect;
  begin
    result := FieldBounds(ACmp.Pos, ACmp.Size);
  end;

  procedure DrawComponent(ACmp: TMapComponentBase; ADCStyle: TMCDrawStyle);
  begin
    if not Assigned(ACmp) then
    begin
      Exit;
    end;

    if ACmp is TMapComponentMap then
    begin
      TMapComponentMap(ACmp).Draw(ACanvas, ComponentBounds(ACmp), mdmsComplete);
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
  end;

var x,y,i: integer;
    c: TMapComponentBase;
    rctPin,rctCmp: TRect;
begin
  FLogger._s(METHOD);

  sglFldW := ARect.Width/Size.w;
  sglFldH := ARect.Height/Size.h;

  if AStyle = mdmsComplete then
  begin
    //Repaint all fields as empty fields
    for x := Low(FMap) to High(FMap) do
    begin
      for y := Low(FMap[x]) to High(FMap[x]) do
      begin
        if FieldWithin(MCPos(x,y), FCursorPos, FCursorSize) or FieldWithin(MCPos(x,y), FCursorPosLast, FCursorSizeLast) then
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
        if FSubcomponents[x].Contains(FCursorPos, FCursorSize) or FSubcomponents[x].Contains(FCursorPosLast, FCursorSizeLast) then
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

    //Paint IO connections
    SetPen(ACanvas, clRed, 2);
    for x := Low(FIOConnections) to High(FIOConnections) do
    begin
      if (FIOConnections[x].intPin < PinLow()) or (FIOConnections[x].intPin > PinHigh()) or
         (FIOConnections[x].intCmp < Low(FSubcomponents)) or (FIOConnections[x].intCmp > High(FSubcomponents)) then
      begin
        continue;
      end;
      rctPin := GetPinRect(FIOConnections[x].intPin, ARect);
      rctCmp := ComponentBounds(FSubcomponents[FIOConnections[x].intCmp]);
      ACanvas.MoveTo(rctPin.Left+(rctPin.Width div 2), rctPin.Top+(rctPin.Height div 2));
      ACanvas.LineTo(rctCmp.Left+(rctCmp.Width div 2), rctCmp.Top+(rctCmp.Height div 2));
    end;
    FLogger._e();
    Exit;
  end;

  if (AStyle = mdmsSelected) and ((FCursorPosLast <> FCursorPos) or (FCursorSizeLast <> FCursorSize)) then
  begin
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

    FLogger._e();
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

  if Assigned(Parent) then
  begin
    for i := Low(FIOConnections) to High(FIOConnections) do
    begin
      if (FIOConnections[i].intPin < PinLow()) or (FIOConnections[i].intPin > PinHigh()) or
         (FIOConnections[i].intCmp < Low(FSubcomponents)) or (FIOConnections[i].intCmp > High(FSubcomponents)) or
         (not (FSubcomponents[FIOConnections[i].intCmp] is TMapComponentInput)) then
      begin
        continue;
      end;
      TMapComponentInput(FSubcomponents[FIOConnections[i].intCmp]).Value := PinValue[FIOConnections[i].intPin];
    end;
  end;

  for i := Low(FSubcomponents) to High(FSubcomponents) do
  begin
    if (not Assigned(FSubcomponents[i])) or (not (FSubcomponents[i] is TMapComponentInput)) then
    begin
      continue;
    end;
    TMapComponentInput(FSubcomponents[i]).Tick();
  end;

  if Assigned(Parent) then
  begin
    for i := Low(FIOConnections) to High(FIOConnections) do
    begin
      if (FIOConnections[i].intPin < PinLow()) or (FIOConnections[i].intPin > PinHigh()) or
         (FIOConnections[i].intCmp < Low(FSubcomponents)) or (FIOConnections[i].intCmp > High(FSubcomponents)) or
         (not (FSubcomponents[FIOConnections[i].intCmp] is TMapComponentOutput)) then
      begin
        continue;
      end;
      PinValue[FIOConnections[i].intPin] := TMapComponentOutput(FSubcomponents[FIOConnections[i].intCmp]).Value;
      Parent.RequestTick(GetPinField(FIOConnections[i].intPin), GetPinNeighbourField(FIOConnections[i].intPin));
    end;
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

function TMapComponentMap.AddMap(ASize: TMCSize; APos: TMCPos; const AMCDef: string): integer;
const METHOD: string = 'TMapComponentMap.AddMap';
var strName: string;
begin
  FLogger._s(METHOD);
  result := -1;

  strName := AddSubcomponent1('m');
  FSubcomponents[High(FSubcomponents)] := TMapComponentMap.Create(strName, ASize, APos, self);
  TMapComponentMap(FSubcomponents[High(FSubcomponents)]).SetMCDef(0,AMCDef);
  TMapComponentMap(FSubcomponents[High(FSubcomponents)]).Pos := APos;
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

function TMapComponentMap.SubcomponentCount(): integer;
const METHOD: string = 'TMapComponentMap.InputString';
begin
  FLogger._s(METHOD);

  result := Length(FSubcomponents);

  FLogger._e(result);
end;

function TMapComponentMap.InputString(const AString: string): boolean;
const METHOD: string = 'TMapComponentMap.InputString';
var i: integer;
    w: word;
    b: boolean;
    c: TMapComponentBase;
begin
  FLogger._s(METHOD);

  result := false;

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
    FLogger._e(result);
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
    if Assigned(FSubcomponents[i]) and (FSubcomponents[i] is TMapComponentInput) then
    begin
      TMapComponentInput(c).Value := (AString[i] = '1');
    end;
  end;

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

procedure TMapComponentMap.AddIOConnection(APin,ACmp: integer);
const METHOD: string = 'TMapComponentMap.AddIOConnection';
var i: integer;
begin
  FLogger._s(METHOD);

  if (APin < PinLow()) or (APin > PinHigh()) or (ACmp < Low(FSubcomponents)) or (ACmp > High(FSubcomponents)) then
  begin
    FLogger._(ltWarning, 'Pin or component ID out of bounds.');
    FLogger._e();
    Exit;
  end;

  if (not (FSubcomponents[ACmp] is TMapComponentIO)) then
  begin
    FLogger._(ltWarning, 'Component is not an IO.');
    FLogger._e();
    Exit;
  end;

  for i := Low(FIOConnections) to High(FIOConnections) do
  begin
    if (FIOConnections[i].intPin = APin) or (FIOConnections[i].intCmp = ACmp) then
    begin
      FLogger._(ltWarning, 'Pin or component already has an IO connection.');
      FLogger._e();
      Exit;
    end;
  end;

  SetLength(FIOConnections, Length(FIOConnections)+1);
  FIOConnections[High(FIOConnections)].intCmp := ACmp;
  FIOConnections[High(FIOConnections)].intPin := APin;
  PinActive[APin] := true;
  FLogger._e();
end;

procedure TMapComponentMap.RemoveIOConnection(APin,ACmp: integer);
const METHOD: string = 'TMapComponentMap.RemoveIOConnection';
var i: integer;
    b: boolean;
begin
  FLogger._s(METHOD);

  for i := Low(FIOConnections) to High(FIOConnections) do
  begin
    if (FIOConnections[i].intPin = APin) and (FIOConnections[i].intCmp = ACmp) then
    begin
      FIOConnections[i].intPin := -1;
      FIOConnections[i].intCmp := -1;
    end;
  end;

  b := false;
  for i := Low(FIOConnections) to High(FIOConnections) do
  begin
    if (FIOConnections[i].intPin = APin) then
    begin
      b := true;
      break;
    end;
  end;

  if not b then
  begin
    PinActive[APin] := false;
  end;

  FLogger._e();
end;

end.

