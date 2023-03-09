unit umainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, Buttons, ComCtrls, ugamecommon, ugamegrid, GR32, GR32_Image, GR32_Layers;

type

  { TfMainForm }

  TfMainForm = class(TForm)
    btnMapSize: TBitBtn;
    btnViewSize: TBitBtn;
    imgLayer: TImage;
    lblLayerSubtitle: TLabel;
    lblLayerSize: TLabel;
    lblLayerDataL: TLabel;
    lblLayerData: TLabel;
    lblLayerTitle: TLabel;
    lblLayerPosL: TLabel;
    lblLayerPos: TLabel;
    lblLayerNeighboursL: TLabel;
    lblLayerNeighbours: TLabel;
    lblLayerRectL: TLabel;
    lblLayerRect: TLabel;
    lblLayerSizeL: TLabel;
    lblCursor: TLabel;
    pnlLayerHeader: TPanel;
    pnlLayerTitle: TPanel;
    pnlLayerPos: TPanel;
    pnlLayerNeighbours: TPanel;
    pnlLayerRect: TPanel;
    pnlLayerSize: TPanel;
    pnlLayerData: TPanel;
    pnlTop: TPanel;
    pnlClient: TPanel;
    pnlLeft: TPanel;
    pnlBottom: TPanel;
    pnlRight: TPanel;
    rgAction: TRadioGroup;
    sbH: TScrollBar;
    sbV: TScrollBar;
    seMapW: TSpinEdit;
    seViewH: TSpinEdit;
    seViewW: TSpinEdit;
    seMapH: TSpinEdit;
    splT: TSplitter;
    splB: TSplitter;
    splR: TSplitter;
    splL: TSplitter;
    gg: TGameGrid;
    tmrRepaint: TTimer;
    tvObjects: TTreeView;
    procedure btnMapSizeClick(Sender: TObject);
    procedure btnViewSizeClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    { GAMEGRID }
    procedure GGChangeMapSize(Sender: TObject);
    procedure GGChangeViewport(Sender: TObject);
    procedure GGLog(Sender: TObject);
    procedure GGAfterLayerPaint(ALayer: TCustomLayer);
    procedure GGIMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer; Layer: TCustomLayer);
    procedure GGIMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
    procedure GGIMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure GGIMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure GGIResize(Sender: TObject);
    procedure sbHScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure sbVScroll(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure tmrRepaintTimer(Sender: TObject);
    procedure tvObjectsDeletion(Sender: TObject; Node: TTreeNode);

  private
    var firstActivate: boolean;
    var layerToMove: TGameLayer;
  public

  end;

var
  fMainForm: TfMainForm;

implementation

{$R *.lfm}

{ TfMainForm }

procedure TfMainForm.FormCreate(Sender: TObject);
begin
  layerToMove := nil;
  firstActivate := true;

  gg := TGameGrid.Create(self);
  gg.Parent := pnlClient;
  gg.Align := alClient;
  gg.BitmapAlign := baTopLeft;
  gg.RepaintMode := rmOptimizer;
  gg.ScaleMode := smNormal;

  gg.OnChangeMapSize := @GGChangeMapSize;
  gg.OnChangeViewport := @GGChangeViewport;
  gg.OnLog := @GGLog;
  gg.OnAfterLayerPaint := @GGAfterLayerPaint;

  gg.OnMouseMove := @GGIMouseMove;
  gg.OnMouseUp := @GGIMouseUp;
  gg.OnMouseWheelDown := @GGIMouseWheelDown;
  gg.OnMouseWheelUp := @GGIMouseWheelUp;
  gg.OnResize := @GGIResize;

  gg.DirLayers := 'data/layers';
  gg.DirObjects := 'data/objects';

  gg.LoadGraphics();
  gg.LoadObjects(tvObjects);
  sbH.BorderSpacing.Right := sbV.Width;
end;

procedure TfMainForm.btnMapSizeClick(Sender: TObject);
begin
  gg.MapSize := TMapSize.Make(seMapW.Value, seMapH.Value);
end;

procedure TfMainForm.btnViewSizeClick(Sender: TObject);
var vp: TMapRect;
begin
  vp := gg.Viewport;
  vp.Width := seViewW.Value;
  vp.Height := seViewH.Value;
  gg.Viewport := vp;
end;

procedure TfMainForm.FormActivate(Sender: TObject);
begin
  if not firstActivate then
  begin
    Exit;
  end;
  firstActivate := false;
  gg.MapSize := TMapSize.Make(3,2);
  gg.Viewport := TMapRect.Make(TMapPos.Make(0,0),3,2);
end;

{ GameGrid events }

procedure TfMainForm.GGChangeMapSize(Sender: TObject);
begin
  seMapW.Value := gg.MapSize.Width;
  seMapH.Value := gg.MapSize.Height;
end;

procedure TfMainForm.GGChangeViewport(Sender: TObject);
begin
  seViewW.Value := gg.Viewport.Width;
  seViewH.Value := gg.Viewport.Height;
  sbH.Max := gg.MapSize.Width-gg.Viewport.Width;
  sbV.Max := gg.MapSize.Height-gg.Viewport.Height;
end;

procedure TfMainForm.GGLog(Sender: TObject);
begin
  //
end;

procedure TfMainForm.GGAfterLayerPaint(ALayer: TCustomLayer);

  function RelativeRect(ARect: TFloatRect): TRect;
  begin
    result.Left := Round(ARect.Left - (ALayer as TGameLayer).Location.Left);
    result.Right := Round(ARect.Right - (ALayer as TGameLayer).Location.Left);
    result.Top := Round(ARect.Top - (ALayer as TGameLayer).Location.Top);
    result.Bottom := Round(ARect.Bottom - (ALayer as TGameLayer).Location.Top);
  end;

var b: TBitmap32;
    i: integer;
    rct: TRect;
begin
  if not (ALayer is TGameLayer) then
  begin
    Exit;
  end;

  (ALayer as TGameLayer).Bitmap.BeginUpdate;
  try
      // Left pins
      b := gg.GetBitmap('pinL');
      for i := (ALayer as TGameLayer).Rect.Top to (ALayer as TGameLayer).Rect.Bottom do
      begin
        rct := RelativeRect(gg.FieldBoundsFloat(
          TMapPos.Make((ALayer as TGameLayer).Rect.Left, i),
          TMapSize.Make(1,1)
        ));
        b.DrawTo((ALayer as TGameLayer).Bitmap, rct);
      end;

      // Bottom pins
      b := gg.GetBitmap('pinB');
      for i := (ALayer as TGameLayer).Rect.Left to (ALayer as TGameLayer).Rect.Right do
      begin
        rct := RelativeRect(gg.FieldBoundsFloat(
          TMapPos.Make(i, (ALayer as TGameLayer).Rect.Bottom),
          TMapSize.Make(1,1)
        ));
        b.DrawTo((ALayer as TGameLayer).Bitmap, rct);
      end;

      // Right pins
      b := gg.GetBitmap('pinR');
      for i := (ALayer as TGameLayer).Rect.Bottom downto (ALayer as TGameLayer).Rect.Top do
      begin
        rct := RelativeRect(gg.FieldBoundsFloat(
          TMapPos.Make((ALayer as TGameLayer).Rect.Right, i),
          TMapSize.Make(1,1)
        ));
        b.DrawTo((ALayer as TGameLayer).Bitmap, rct);
      end;

      // Top pins
      b := gg.GetBitmap('pinT');
      for i := (ALayer as TGameLayer).Rect.Right downto (ALayer as TGameLayer).Rect.Left do
      begin
        rct := RelativeRect(gg.FieldBoundsFloat(
          TMapPos.Make(i, (ALayer as TGameLayer).Rect.Top),
          TMapSize.Make(1,1)
        ));
        b.DrawTo((ALayer as TGameLayer).Bitmap, rct);
      end;
  finally
    (ALayer as TGameLayer).Bitmap.EndUpdate;
  end;
end;

procedure TfMainForm.GGIMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer; Layer: TCustomLayer);
var fld: TMapPos;
    lidx: integer;
begin
  fld := gg.FieldFromXY(X,Y);
  lidx := -1;
  if Assigned(Layer) then
  begin
    lidx := Layer.Index;
  end;
  gg.FieldCursor := fld;
  lblCursor.Caption := Format('[%d;%d]=>[%d;%d;%d]', [X,Y,lidx,fld.X,fld.Y]);
end;

procedure TfMainForm.GGIMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; Layer: TCustomLayer);
//const arrObj: array[0..3] of string = ('wire','input','output','gate');

  procedure SetLayerInfo(TheLayer: TCustomLayer);
  var gn: TGameNeighbours;
      i: integer;
      s: string;
  begin
    lblLayerTitle.Caption := '';
    lblLayerSubtitle.Caption := '';
    lblLayerPos.Caption := '';
    lblLayerSize.Caption := '';
    lblLayerRect.Caption := '';
    lblLayerNeighbours.Caption := '';
    lblLayerData.Caption := '';

    if not Assigned(TheLayer) then
    begin
      Exit;
    end;
    lblLayerTitle.Caption := Format('Layer #%d', [TheLayer.Index]);
    lblLayerSubtitle.Caption := TheLayer.ClassName;
    if TheLayer is TGameLayer then
    begin
      lblLayerPos.Caption := (TheLayer as TGameLayer).Pos.ToStr();
      lblLayerSize.Caption := (TheLayer as TGameLayer).Size.ToStr();
      lblLayerRect.Caption := (TheLayer as TGameLayer).Rect.ToStr();
      { #todo : Parse data into some reasonable info }
      lblLayerData.Caption := Format('%p', [(TheLayer as TGameLayer).Data]);
      gn := gg.LayerNeighbours(TheLayer as TGameLayer);
      for i := Low(gn) to High(gn) do
      begin
        s := '<nil>';
        if Assigned(gn[i].Layer) then
        begin
          s := gn[i].Layer.Rect.ToStr();
        end;
        lblLayerNeighbours.Caption := lblLayerNeighbours.Caption +
          Format('%d: %s%d %s'#13#10, [i, MapDirToStr(gn[i].Direction),
            gn[i].Index, s]);
      end;
      Exit;
    end;
  end;

  procedure UpdateFieldSelected();
  begin
    if Assigned(Layer) and (Layer is TGameLayer) then
    begin
      gg.FieldSelected := (Layer as TGameLayer).Pos;
    end
    else
    begin
      gg.FieldSelected := gg.FieldFromXY(X,Y);
    end;
  end;

begin
  UpdateFieldSelected();

  case rgAction.ItemIndex of
    1: //Move
    begin
      if not Assigned(layerToMove) then
      begin
        if Layer is TGameLayer then
        begin
          layerToMove := (Layer as TGameLayer);
        end;
      end
      else
      begin
        gg.MoveObject(layerToMove, gg.FieldFromXY(X,Y));
        layerToMove := nil;
      end;
    end;
    2: //Rotate
    begin
      layerToMove := nil;
      if Layer is TGameLayer then
      begin
        gg.RotateObject(Layer as TGameLayer);
      end;
    end;
    3: //Delete
    begin
      layerToMove := nil;
      if Layer is TGameLayer then
      begin
        gg.RemoveObject(Layer as TGameLayer);
      end;
    end;
    4: //Add
    begin
      layerToMove := nil;
      if Assigned(tvObjects.Selected) and Assigned(tvObjects.Selected.Data) then
      begin
        if gg.AddObject(gg.FieldFromXY(X,Y), TGameTreeNodeData(tvObjects.Selected.Data).FContent) = -1 then
        begin
          showmessage('Cannot');
        end;
      end;
    end;
    else //Select
    begin
      layerToMove := nil;
      SetLayerInfo(Layer);
    end;
  end;
end;

procedure TfMainForm.GGIMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Shift = [ssCtrl] then
  begin
    gg.Zoom(false);
  end
  else if Shift = [ssShift] then
  begin
    gg.ScrollH(1);
    sbH.Position := sbH.Position + 1;
  end
  else if Shift = [] then
  begin
    gg.ScrollV(1);
    sbV.Position := sbV.Position + 1;
  end;
end;

procedure TfMainForm.GGIMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if Shift = [ssCtrl] then
  begin
    gg.Zoom(true);
  end
  else if Shift = [ssShift] then
  begin
    gg.ScrollH(-1);
    sbH.Position := sbH.Position - 1;
  end
  else if Shift = [] then
  begin
    gg.ScrollV(-1);
    sbV.Position := sbV.Position - 1;
  end;
end;

procedure TfMainForm.GGIResize(Sender: TObject);
begin
  tmrRepaint.Enabled := false;
  tmrRepaint.Enabled := true;
end;

procedure TfMainForm.sbHScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  gg.ScrollH(ScrollPos-sbH.Position);
end;

procedure TfMainForm.sbVScroll(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  gg.ScrollV(ScrollPos-sbV.Position);
end;

procedure TfMainForm.tmrRepaintTimer(Sender: TObject);
begin
  gg.UpdateLayers;
  tmrRepaint.Enabled := false;
end;

procedure TfMainForm.tvObjectsDeletion(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node.Data) then
  begin
    TGameTreeNodeData(Node.Data).Free;
    Node.Data := nil;
  end;
end;

end.

