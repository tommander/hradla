unit umapcomponentio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, ulogger, umapcomponentpinned, umapcomponentbase;

type
  TMapComponentIO = class(TMapComponentPinned)

    private

      var FValue: boolean;

      procedure SetValue(AValue: boolean);
      function GetValue(): boolean;

    protected


    public

      property Value: boolean read GetValue write SetValue;

      constructor Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
      function GetMCDef(ALevel: byte{; AIgnorePos: boolean = false}): string; override;
      procedure SetMCDef(ALevel: byte; const AValue: string; AAddToPos: TMCPos); override;
  end;

implementation

(* PRIVATE *)

procedure TMapComponentIO.SetValue(AValue: boolean);
const METHOD: string = 'TMapComponentIO.SetValue';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    FValue := AValue;

  finally
    FLogger._e();
  end;
end;

function TMapComponentIO.GetValue(): boolean;
const METHOD: string = 'TMapComponentIO.GetValue';
begin
  FLogger._s(METHOD);
  try

    result := FValue;

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

(* PROTECTED *)

function TMapComponentIO.GetMCDef(ALevel: byte{; AIgnorePos: boolean = false}): string;
const METHOD: string = 'TMapComponentIO.GetMCDef';
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
//    FLogger._pe('AIgnorePos', TypeInfo(AIgnorePos), @AIgnorePos);
    FLogger._se();

    result := inherited GetMCDef(ALevel{, AIgnorePos});
    result := result + Format(#13#10'%svalue="%d"', [LevelStr(ALevel),Integer(FValue)]);

  finally
    FLogger._e(TypeInfo(result), @result);
  end;
end;

procedure TMapComponentIO.SetMCDef(ALevel: byte; const AValue: string; AAddToPos: TMCPos);
const METHOD: string = 'TMapComponentIO.SetMCDef';
var re: TRegExpr;
begin
  FLogger._ss(METHOD);
  try
    FLogger._pe('ALevel', TypeInfo(ALevel), @ALevel);
    FLogger._pe('AValue', TypeInfo(AValue), @AValue);
    FLogger._se();

    inherited SetMCDef(ALevel, AValue, AAddToPos);

    re := TRegExpr.Create;
    re.ModifierM := true;
    try
      re.Expression := LevelPrefix(ALevel)+'value="([^"]+)"';
      if re.Exec(AValue) then
      begin
        FValue := (re.Match[1] <> '0');
      end;
    finally
      re.Free;
    end;

  finally
    FLogger._e();
  end;
end;

(* PUBLIC *)

constructor TMapComponentIO.Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
begin
  inherited Create(AName, ASize, APos, AParent);
  Clear;
end;

destructor TMapComponentIO.Destroy();
begin
  Clear;
  inherited Destroy;
end;

procedure TMapComponentIO.Clear();
const METHOD: string = 'TMapComponentIO.Clear';
begin
  FLogger._s(METHOD);
  try

    FValue := false;

  finally
    FLogger._e();
  end;
end;

end.

