unit umapcomponentio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, umapcomponentpinned, umapcomponentbase;

type
  TMapComponentIO = class(TMapComponentPinned)

    private

      var FValue: boolean;

      procedure SetValue(AValue: boolean);
      function GetValue(): boolean;

    protected

      function GetMCDef(): string; override;
      procedure SetMCDef(const AValue: string); override;

    public

      property Value: boolean read GetValue write SetValue;

      constructor Create(const AName: string; ASize: TMCSize; APos: TMCPos; AParent: TMapComponentBase);
      destructor Destroy(); override;
      procedure Clear();
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

function TMapComponentIO.GetMCDef(): string;
const METHOD: string = 'TMapComponentIO.GetMCDef';
begin
  FLogger._s(METHOD);

  result := inherited GetMCDef;
  result := result + Format(';value="%d"', [Integer(FValue)]);

  FLogger._e(result);
end;

procedure TMapComponentIO.SetMCDef(const AValue: string);
const METHOD: string = 'TMapComponentIO.SetMCDef';
begin
  FLogger._s(METHOD);

  inherited SetMCDef(AValue);

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

