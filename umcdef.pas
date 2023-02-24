unit umcdef;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  SynEdit, SynHighlighterAny;

type

  { TfMCDef }

  TfMCDef = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    Panel1: TPanel;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
  private

  public

  end;

var
  fMCDef: TfMCDef;

implementation

{$R *.lfm}

end.

