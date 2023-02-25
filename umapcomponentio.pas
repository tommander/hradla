unit umapcomponentio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr, umapcomponentpinned, umapcomponentbase;

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
      function GetMCDef(ALevel: byte; AIgnorePos: boolean = false): string; override;
      procedure SetMCDef(ALevel: byte; const AValue: string); override;
  end;

implementation

(* PRIVATE *)

procedure TMapComponentIO.SetValue(AValue: boolean);
const METHOD: string = 'TMapComponentIO.SetValue';
begin
  FLogger._s(METHOD);

  FValue := AValue;

  FLogger._e();
end;

function TMapComponentIO.GetValue(): boolean;
const METHOD: string = 'TMapComponentIO.GetValue';
begin
  FLogger._s(METHOD);

  result := FValue;

  FLogger._e(result);
end;

(* PROTECTED *)

function TMapComponentIO.GetMCDef(ALevel: byte; AIgnorePos: boolean = false): string;
const METHOD: string = 'TMapComponentIO.GetMCDef';
begin
  FLogger._s(METHOD);

  result := inherited GetMCDef(ALevel, AIgnorePos);
  result := result + Format(#13#10'%svalue="%d"', [LevelStr(ALevel),Integer(FValue)]);

  FLogger._e(result);
end;

procedure TMapComponentIO.SetMCDef(ALevel: byte; const AValue: string);
const METHOD: string = 'TMapComponentIO.SetMCDef';
var re: TRegExpr;
begin
  FLogger._s(METHOD);

  inherited SetMCDef(ALevel, AValue);

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

  FLogger._e();
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

  FValue := false;

  FLogger._e();
end;

end.

