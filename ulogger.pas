unit ulogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TypInfo, Variants;

type
  TStackEntry = record
    Method: string;
    Params: string;
  end;
  TLogType = (ltDebug,ltInfo,ltWarning,ltError);
  TLogger = class
    private
      var FLog: string;
      var FLogTypeMin: TLogType;
      var FFileName: string;
      var FStack: array of TStackEntry;
      function Method(): string;
      function Params(): string;
      function Tabs(): string;
      procedure Push(const AMethod: string);
      procedure Pop();
      procedure AddParam(const AName,AValue: string);
      procedure LoadFromFile();
      procedure SaveToFile();
      function ThatToStr(ATypeInfo: pointer; AThat: pointer): string;
    public
      constructor Create(const AFileName: string; ALogTypeMin: TLogType = ltInfo; ADoLoadFile: boolean = false);
      destructor Destroy; override;
      procedure Clear;
      procedure _(ALogType: TLogType; const AText: string);
      procedure _(ALogType: TLogType; const AText: string; AParams: array of const);
      procedure _s(const AMethod: string);
      procedure _ss(const AMethod: string);
      procedure _se();
      procedure _e();
      procedure _e(ATypeInfo: pointer; AThat: pointer);
      procedure _pe(AParam: string; ATypeInfo: pointer; AThat: pointer);
  end;

var
  TheLogger: TLogger;

function FLogger(): TLogger;

implementation

(* GLOBAL *)

function FLogger(): TLogger;
begin
  if not Assigned(TheLogger) then
  begin
    TheLogger := TLogger.Create('/home/tommander/Programming.Pascal/hradla/log.txt', ltInfo);
    TheLogger._(ltInfo, 'Start of log');
  end;
  result := TheLogger;
end;

(* PRIVATE *)

function TLogger.Method(): string;
begin
  result := '';
  if Length(FStack) > 0 then
  begin
    result := FStack[High(FStack)].Method;
  end;
end;

function TLogger.Params(): string;
begin
  result := '';
  if Length(FStack) > 0 then
  begin
    result := FStack[High(FStack)].Params;
  end;
end;

function TLogger.Tabs(): string;
var i: integer;
begin
  result := '';
  if Length(FStack) = 0 then
  begin
    Exit;
  end;
  for i := 1 to Length(FStack) do
  begin
    result := result + '  ';
  end;
end;

procedure TLogger.Push(const AMethod: string);
begin
  SetLength(FStack, Length(FStack)+1);
  FStack[High(FStack)].Method := AMethod;
  FStack[High(FStack)].Params := '';
end;

procedure TLogger.Pop();
begin
  if Length(FStack) > 0 then
  begin
    SetLength(FStack, Length(FStack)-1);
  end;
end;

procedure TLogger.AddParam(const AName,AValue: string);
begin
  if Length(FStack) = 0 then
  begin
    Exit;
  end;
  if Length(FStack[High(FStack)].Params) > 0 then
  begin
    FStack[High(FStack)].Params := FStack[High(FStack)].Params + ',';
  end;
  FStack[High(FStack)].Params := FStack[High(FStack)].Params + Format('%s="%s"', [AName, AValue]);
end;

procedure TLogger.LoadFromFile();
var sl: TStringList;
begin
  if not FileExists(FFileName) then
  begin
    Exit;
  end;

  sl := TStringList.Create;
  try
    sl.LoadFromFile(FFileName);
    FLog := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure TLogger.SaveToFile();
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := FLog;
    sl.SaveToFile(FFileName);
  finally
    sl.Free;
  end;
end;

(* PUBLIC *)

constructor TLogger.Create(const AFileName: string; ALogTypeMin: TLogType = ltInfo; ADoLoadFile: boolean = false);
begin
  inherited Create;
  FFileName := AFileName;
  FLogTypeMin := ALogTypeMin;
  Clear();
  if ADoLoadFile then
  begin
    LoadFromFile();
  end;
end;

destructor TLogger.Destroy;
begin
  SaveToFile();
  inherited Destroy;
end;

procedure TLogger.Clear;
begin
  FLog := '';
  SetLength(FStack, 0);
end;

procedure TLogger._(ALogType: TLogType; const AText: string);
begin
  if ALogType < FLogTypeMin then
  begin
    Exit;
  end;

  FLog := FLog + Format('%s[%s]{%s} %s'#13#10, [Tabs(), TimeToStr(now), Method(), AText]);
end;

procedure TLogger._(ALogType: TLogType; const AText: string; AParams: array of const);
begin
  _(ALogType, Format(AText, AParams));
end;

procedure TLogger._s(const AMethod: string);
begin
  _ss(AMethod);
  _se();
end;

procedure TLogger._ss(const AMethod: string);
begin
  Push(AMethod);
end;

procedure TLogger._se();
begin
  _(ltDebug, 'Started %s(%s)', [Method(),Params()]);
end;

procedure TLogger._e();
begin
  _(ltDebug, 'Finished');
  Pop();
end;

procedure TLogger._e(ATypeInfo: pointer; AThat: pointer);
begin
  _(ltDebug, 'Result "'+ThatToStr(ATypeInfo, AThat)+'"');
  _e();
end;

//_pe(TypeInfo(something), @something);
procedure TLogger._pe(AParam: string; ATypeInfo: pointer; AThat: pointer);
begin
  AddParam(AParam, ThatToStr(ATypeInfo, AThat));
end;

//ThatToStr(TypeInfo(something), @something);
function TLogger.ThatToStr(ATypeInfo: pointer; AThat: pointer): string;

  function CCToStr(ACC: TCallConv): string;
  begin
    result := 'cc';
    case ACC of
      ccReg: result := 'reg';
      ccCdecl: result := 'cdecl';
      ccPascal: result := 'pascal';
      ccStdCall: result := 'stdcall';
      ccSafeCall: result := 'safecall';
      ccCppdecl: result := 'cppdecl';
      ccFar16: result := 'far16';
      ccOldFPCCall: result := 'oldfpccall';
      ccInternProc: result := 'internproc';
      ccSysCall: result := 'syscall';
      ccSoftFloat: result := 'softfloat';
      ccMWPascal: result := 'mwpascal';
    end;
  end;

  function PFToStr(APF: TParamFlag): string;
  begin
    result := 'pf';
    case APF of
      pfVar: result := 'var';
      pfConst: result := 'const';
      pfArray: result := 'array';
      pfAddress: result := 'address';
      pfReference: result := 'reference';
      pfOut: result := 'out';
      pfConstRef: result := 'constref';
      pfHidden: result := 'hidden';
      pfHigh: result := 'high';
      pfSelf: result := 'self';
      pfVmt: result := 'vmt';
      pfResult: result := 'result';
    end;
  end;

  function PFSToStr(APFS: TParamFlags): string;
  var pf: TParamFlag;
  begin
    result := '';
    for pf in TParamFlags do
    begin
      if pf in APFS then
      begin
        if result <> '' then
        begin
          result := result + ' ';
        end;
        result := result + PFToStr(pf);
      end;
    end;
  end;

  function PPToStr(APP: PProcedureParam): string;
  begin
    result := '';
    if not Assigned(APP) then
    begin
      Exit;
    end;
    result := Format('%s %s: %s', [PFSToStr(APP^.ParamFlags), APP^.Name, ThatToStr(APP^.ParamType, nil)]);
  end;


var td: PTypeData;
    strName,strName2: string;
    i: integer;
    mf: PManagedField;
begin
  result := '';

  if not Assigned(ATypeInfo) then
  begin
    Exit;
  end;

  td := GetTypeData(ATypeInfo);
  if not Assigned(td) then
  begin
    Exit;
  end;

  case PTypeInfo(ATypeInfo)^.Kind of
    tkUnknown:
    begin
      //-
      result := '<unknown>';
    end;
    tkInteger:
    begin
      //OrdType,MinValue,MaxValue
      case td^.OrdType of
        otSByte:  if Assigned(AThat) then result := IntToStr(Shortint(AThat^))  else result := '<shortint>';
        otUByte:  if Assigned(AThat) then result := UIntToStr(Byte(AThat^))     else result := '<byte>';
        otSWord:  if Assigned(AThat) then result := IntToStr(Smallint(AThat^))  else result := '<smallint>';
        otUWord:  if Assigned(AThat) then result := UIntToStr(Word(AThat^))     else result := '<word>';
        otSLong:  if Assigned(AThat) then result := IntToStr(Longint(AThat^))   else result := '<longint>';
        otULong:  if Assigned(AThat) then result := UIntToStr(Longword(AThat^)) else result := '<longword>';
        otSQWord: if Assigned(AThat) then result := IntToStr(Int64(AThat^))     else result := '<int64>';
        otUQWord: if Assigned(AThat) then result := UIntToStr(QWord(AThat^))    else result := '<qword>';
      end;
    end;
    tkChar:
    begin
      //OrdType,MinValue,MaxValue
      if Assigned(AThat) then result := AnsiChar(AThat^) else result := '<ansichar>';
    end;
    tkEnumeration:
    begin
      //OrdType,MinValue,MaxValue,BaseType,BaseTypeRef,NameList
      if Assigned(AThat) then result := GetEnumName(ATypeInfo, Integer(AThat^)) else result := '<enum>';
    end;
    tkFloat:
    begin
      //FloatType
      case td^.FloatType of
        ftSingle:   if Assigned(AThat) then result := FloatToStr(Single(AThat^)) else result := '<single>';
        ftDouble:   if Assigned(AThat) then result := FloatToStr(Double(AThat^)) else result := '<double>';
        ftExtended: if Assigned(AThat) then result := FloatToStr(Extended(AThat^)) else result := '<extended>';
        ftComp:     if Assigned(AThat) then result := FloatToStr(Comp(AThat^)) else result := '<comp>';
        ftCurr:     if Assigned(AThat) then result := FloatToStr(Currency(AThat^)) else result := '<curr>';
      end;
    end;
    tkSet:
    begin
      //SetSize,CompTypeRef,OrdType,CompType
      if Assigned(AThat) then result := SetToString(PTypeInfo(ATypeInfo), AThat, true) else result := '<set>';
    end;
    tkMethod:
    begin
      //MethodKind,ParamCount,ParamList
      strName := '?';
      strName2 := '';
      case td^.MethodKind of
        mkProcedure: strName := 'procedure';
        mkFunction: strName := 'function';
        mkConstructor: strName := 'constructor';
        mkDestructor: strName := 'destructor';
        mkClassProcedure: strName := 'class procedure';
        mkClassFunction: strName := 'class function';
        mkClassConstructor: strName := 'class constructor';
        mkClassDestructor: strName := 'class destructor';
        mkOperatorOverload: strName := 'operator';
      end;
      for i := Low(td^.ParamList) to High(td^.ParamList) do
      begin
        if Ord(td^.ParamList[i]) < 32 then
        begin
          strName2 := strName2 + '${'+IntToStr(Ord(td^.ParamList[i]))+'}';
        end
        else
        begin
          strName2 := strName2 + td^.ParamList[i];
        end;
      end;
      result := strName + ' :: "' + strName2 + '"';
    end;
    tkSString:
    begin
      //MaxLength
      if Assigned(AThat) then result := ShortString(AThat^) else result := '<shortstring>';
    end;
    tkLString:
    begin
      //-
      if Assigned(AThat) then result := String(AThat^) else result := '<string>';
    end;
    tkAString:
    begin
      //CodePage
      if Assigned(AThat) then result := AnsiString(AThat^) else result := '<ansistring>';
    end;
    tkWString:
    begin
      //-
      if Assigned(AThat) then result := WideString(AThat^) else result := '<widestring>';
    end;
    tkVariant:
    begin
      //-
      if Assigned(AThat) then result := VarToStr(Variant(AThat^)) else result := '<variant>';
    end;
    tkArray:
    begin
      //ArrayData
      result := Format('array(%d,%d) of %s', [td^.ArrayData.DimCount, td^.ArrayData.ElCount, ThatToStr(td^.ArrayData.ElType, nil)]);
    end;
    tkRecord:
    begin
      //RecInitData,RecInitInfo,RecSize,[False:ManagedFldCount],[True:TotalFieldCount]
      strName := '';
      for i := 1 to td^.TotalFieldCount do
      begin
        mf := Pointer(Pointer(@td^.RecSize)+SizeOf(td^.RecSize)+SizeOf(td^.TotalFieldCount)+((i-1)*SizeOf(TManagedField)));
        if strName <> '' then
        begin
          strName := strName + ',';
        end;
        strName := strName + ThatToStr(mf^.TypeRef, Pointer(AThat+mf^.FldOffset));// mf^.TypeRef^.Name + '(' + IntToStr(mf^.FldOffset) + ')';
      end;
      result := Format('%s(%s)', [PTypeInfo(ATypeInfo)^.Name, strName]);
    end;
    tkInterface:
    begin
      //IntfParent,IntfParentRef,IntfFlags,GUID,IntfUnit
      result := td^.IntfUnit+'.'+PTypeInfo(ATypeInfo)^.Name+GUIDToString(td^.GUID);
    end;
    tkClass:
    begin
      //ParentInfo,ClassType,ParentInfoRef,PropCount,UnitName
      result := td^.UnitName+'.'+td^.ClassType.ClassName;
    end;
    tkObject:
    begin
      //-
      result := PTypeInfo(ATypeInfo)^.Name;
    end;
    tkWChar:
    begin
      //OrdType,MinValue,MaxValue
      if Assigned(AThat) then result := WideChar(AThat^) else result := '<widechar>';
    end;
    tkBool:
    begin
      //OrdType,MinValue,MaxValue
      if Assigned(AThat) then result := BoolToStr(Boolean(AThat^), 'true', 'false') else result := '<boolean>';
    end;
    tkInt64:
    begin
      //OrdType,MinInt64Value,MaxInt64Value
      if Assigned(AThat) then result := IntToStr(Int64(AThat^)) else result := '<int64>';
    end;
    tkQWord:
    begin
      //OrdType,MinQWordValue,MaxQWordValue
      if Assigned(AThat) then result := UIntToStr(QWord(AThat^)) else result := '<qword>';
    end;
    tkDynArray:
    begin
      //ElType2,ElType,elSize,elType2Ref,varType,elTypeRef,DynUnitName
      if Assigned(td^.ElType) then
      begin
        strName := ThatToStr(td^.ElType, nil);
      end
      else
      begin
        strName := ThatToStr(td^.ElType2, nil);
      end;
      result := 'array of '+strName;
    end;
    tkInterfaceRaw:
    begin
      //RawIntfParent,IIDStr,RawIntfParentRef,RawIntfFlags,IID,RawIntfUnit
      result := td^.RawIntfUnit+'.'+PTypeInfo(ATypeInfo)^.Name+td^.IIDStr;
    end;
    tkProcVar:
    begin
      //ProcSig
      strName := '';
      for i := 0 to td^.ProcSig.ParamCount-1 do
      begin
        if strName <> '' then
        begin
          strName := strName + '; ';
        end;
        strName := strName + PPToStr(td^.ProcSig.GetParam(i));
      end;
      strName2 := '';
      if Assigned(td^.ProcSig.ResultType) then
      begin
        strName2 := ':'+ThatToStr(td^.ProcSig.ResultType, nil);
      end;
      result := Format('%s(%s)%s;%s', [BoolToStr(Assigned(td^.ProcSig.ResultType), 'function', 'procedure'), strName, strName2, CCToStr(td^.ProcSig.CC)]);
    end;
    tkUString:
    begin
      //-
      if Assigned(AThat) then result := UnicodeString(AThat^) else result := '<unicodestring>';
    end;
    tkUChar:
    begin
      //-
      if Assigned(AThat) then result := UnicodeChar(AThat^) else result := '<unicodechar>';
    end;
    tkHelper:
    begin
      //HelperParent,ExtendedInfo,HelperParentRef,ExtendedInfoRef,HelperProps,HelperUnit
      result := Format('%s.%s', [td^.HelperUnit, PTypeInfo(ATypeInfo)^.Name]);
    end;
    tkFile:
    begin
      //-
      result := Format('<file>', []);
    end;
    tkClassRef:
    begin
      //InstanceType,InstanceTypeRef
      result := Format('<ref:%s>', [td^.InstanceType^.Name]);
    end;
    tkPointer:
    begin
      //RefType,RefTypeRef
      if Assigned(td^.RefType) then
      begin
        case td^.RefType^.Kind of
          tkChar: if Assigned(AThat) then result := PChar(AThat^) else result := '<pchar>';
          tkWChar: if Assigned(AThat) then result := PWideChar(AThat^) else result := '<pwidechar>';
          tkUChar: if Assigned(AThat) then result := PUnicodeChar(AThat^) else result := '<punicodechar>';
          else
          begin
            if Assigned(AThat) then result := Format('<%s^>', [td^.RefType^.Name]) else result := Format('<%s^=nil>', [td^.RefType^.Name]);
          end;
        end;
      end
      else
      begin
        if Assigned(AThat) then result := '<pointer>' else result := '<nil>';
      end;
    end;
  end;
end;

finalization

if Assigned(TheLogger) then
begin
  TheLogger.Free;
end;

end.

