unit ulogger;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogType = (ltDebug,ltInfo,ltWarning,ltError);
  TLogger = class
    private
      var FLog: string;
      var FLogTypeMin: TLogType;
      var FFileName: string;
      var FStack: array of string;
      function Method(): string;
      procedure Push(const AMethod: string);
      procedure Pop();
      procedure LoadFromFile();
      procedure SaveToFile();
    public
      constructor Create(const AFileName: string; ALogTypeMin: TLogType = ltInfo);
      destructor Destroy; override;
      procedure _(ALogType: TLogType; const AText: string);
      procedure _(ALogType: TLogType; const AText: string; AParams: array of const);
      procedure _s(const AMethod: string);
      procedure _p(const AName,AValue: string);
      procedure _p(const AName: string; AValue: int64);
      procedure _p(const AName: string; AValue: boolean);
      procedure _e();
      procedure _e(const AResult: string);
      procedure _e(AResult: int64);
      procedure _e(AResult: boolean);
  end;

implementation

(* PRIVATE *)

function TLogger.Method(): string;
begin
  result := '';
  if Length(FStack) > 0 then
  begin
    result := FStack[High(FStack)];
  end;
end;

procedure TLogger.Push(const AMethod: string);
begin
  SetLength(FStack, Length(FStack)+1);
  FStack[High(FStack)] := AMethod;
end;

procedure TLogger.Pop();
begin
  if Length(FStack) > 0 then
  begin
    SetLength(FStack, Length(FStack)-1);
  end;
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

constructor TLogger.Create(const AFileName: string; ALogTypeMin: TLogType = ltInfo);
begin
  inherited Create;
  FLog := '';
  FFileName := AFileName;
  FLogTypeMin := ALogTypeMin;
  SetLength(FStack, 0);
//  LoadFromFile();
end;

destructor TLogger.Destroy;
begin
  SaveToFile();
  inherited Destroy;
end;

procedure TLogger._(ALogType: TLogType; const AText: string);
begin
  if ALogType < FLogTypeMin then
  begin
    Exit;
  end;

  FLog := FLog + Format('[%s]{%s} %s'#13#10, [TimeToStr(now), Method(), AText]);
end;

procedure TLogger._(ALogType: TLogType; const AText: string; AParams: array of const);
begin
  _(ALogType, Format(AText, AParams));
end;

procedure TLogger._s(const AMethod: string);
begin
  Push(AMethod);
  _(ltDebug, 'Started');
end;

procedure TLogger._p(const AName,AValue: string);
begin
  _(ltDebug, 'Param "%s"="%s"', [AName,AValue]);
end;

procedure TLogger._p(const AName: string; AValue: int64);
begin
  _(ltDebug, 'Param "%s"="%d"', [AName,AValue]);
end;

procedure TLogger._p(const AName: string; AValue: boolean);
begin
  _(ltDebug, 'Param "%s"="%s"', [AName,BoolToStr(AValue, 'true', 'false')]);
end;

procedure TLogger._e();
begin
  _(ltDebug, 'Finished');
  Pop();
end;

procedure TLogger._e(const AResult: string);
begin
  _(ltDebug, 'Result "%s"', [AResult]);
  _e();
end;

procedure TLogger._e(AResult: int64);
begin
  _(ltDebug, 'Result "%d"', [AResult]);
  _e();
end;

procedure TLogger._e(AResult: boolean);
begin
  _(ltDebug, 'Result "%s"', [BoolToStr(AResult, 'true', 'false')]);
  _e();
end;

end.

