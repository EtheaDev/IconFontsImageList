{*******************************************************}
{                                                       }
{            Delphi Visual Component Library            }
{                                                       }
{ Copyright(c) 1995-2019 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}

unit ImgEdit;

interface

uses
  Windows, Messages, SysUtils, Graphics, Forms, StdCtrls, ExtCtrls, Controls,
  Classes, Dialogs, ComCtrls, ImgList, ExtDlgs;

const
  IMAGELIST_SIZE = 48;

type
  TImageListEditor = class;

  TImageOperation = (ioCrop, ioStretch, ioCenter);

  TImageInfo = class(TObject)
  private
    FOwner: TList;
    FOwnerForm: TImageListEditor;
    FOperation: TImageOperation;
    FGraphic: TGraphic;
    FFillColor: TColor;
    FTransparentColor: TColor;
    FCanChangeTransparent: Boolean;
    FCanChangeFill: Boolean;
    FCanChangeOperation: Boolean;
    procedure SetFillColor(const Value: TColor);
    procedure SetTransparentColor(const Value: TColor);
    procedure SetOperation(const Value: TImageOperation);
    procedure SetCanChangeFill(const Value: Boolean);
    procedure SetCanChangeTransparent(const Value: Boolean);
    procedure SetCanChangeOperation(const Value: Boolean);
    procedure Change;

  public
    constructor Create(AOwner: TList; AOwnerForm: TImageListEditor; AGraphic: TGraphic; OwnerIndex: Integer = -1);
    destructor Destroy; override;
    property Operation: TImageOperation read FOperation write SetOperation;
    property Graphic: TGraphic read FGraphic;
    property TransparentColor: TColor read FTransparentColor write SetTransparentColor;
    property FillColor: TColor read FFillColor write SetFillColor;
    property CanChangeTransparent: Boolean read FCanChangeTransparent write SetCanChangeTransparent;
    property CanChangeFill: Boolean read FCanChangeFill write SetCanChangeFill;
    property CanChangeOperation: Boolean read FCanChangeOperation write SetCanChangeOperation;

  end;

  TImageListEditor = class(TForm)
    OK: TButton;
    Cancel: TButton;
    Apply: TButton;
    Help: TButton;
    OpenDialog: TOpenPictureDialog;
    DragTimer: TTimer;
    SaveDialog: TSavePictureDialog;
    ImageListGroup: TGroupBox;
    ImageView: TListView;
    Add: TButton;
    Delete: TButton;
    Clear: TButton;
    ExportBtn: TButton;
    ReplaceBtn: TButton;
    ImageGroup: TGroupBox;
    TransparentLabel: TLabel;
    FillLabel: TLabel;
    OptionsGroup: TRadioGroup;
    MainPanel: TPanel;
    MainImage: TImage;
    OptionsPanel: TPanel;
    TransparentColor: TColorBox;
    FillColor: TColorBox;
    procedure FormCreate(Sender: TObject);
    procedure ImageViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure DragTimerTimer(Sender: TObject);
    procedure ImageViewEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ImageViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ClearClick(Sender: TObject);
    procedure ApplyClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ImageViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure OptionsGroupClick(Sender: TObject);
    procedure MainImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MainImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TransparentColorChange(Sender: TObject);
    procedure FillColorChange(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure ImageViewCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure FormResize(Sender: TObject);
    procedure HelpClick(Sender: TObject);

  private
    FUpdating: Boolean;
    FPickingColor: Boolean;

    FEditingList: TImageList;
    FScaledImages: TImageList;
    FImages: TImageList;

    FOldMasked: Boolean;
    FOldBkColor: TColor;
    FOldBlendColor: TColor;
    FOldDrawingStyle: TDrawingStyle;
    FOldImageType: TImageType;

    FImageInfo: TList;
    FItemHeight: Integer;

    function DoAdd(Icon: TIcon; Index: Integer): Integer; overload;
    function DoAdd(Graphic: TGraphic; Index: Integer): Integer; overload;
    function DoAdd(Graphic: TGraphic; Index: Integer; DivX, DivY: Integer): Integer; overload;

    procedure Replace(Index: Integer; Graphic: TGraphic; Transparent, Fill: TColor;
      Operation: TImageOperation);

    procedure MoveImage(FromIndex, ToIndex: Integer);
    procedure AddColor(const S: string);
    procedure DeleteSelectedImages;
    procedure ClearAllImages;
    procedure ResizeImageGroup;
    function UpdateUI(Changed: Boolean): Boolean;
    procedure UpdatePickColor(X, Y: Integer);

    procedure SetImageFillColor(Color: TColor);
    procedure SetImageOperation(Operation: TImageOperation);
    procedure SetImageTransparentColor(Color: TColor);
    function GetImageInfo(Index: Integer): TImageInfo;
  public
    property Items[Index: Integer]: TImageInfo read GetImageInfo;
  end;

function EditImageList(AImageList: TImageList): Boolean;

implementation

{$R *.dfm}

uses CommCtrl, TypInfo;

procedure GetImages(ImageList: TImageList; Index: Integer; Image, Mask: TBitmap);
var
  R : TRect;
begin
  R := Rect(0, 0, ImageList.Width, ImageList.Height);
  Image.PixelFormat := pf32bit;
  Image.AlphaFormat := afIgnored;
  Image.SetSize(ImageList.Width, ImageList.Height);
  Mask.PixelFormat := pf1bit;
  Mask.SetSize(ImageList.Width, ImageList.Height);

  Image.Canvas.Brush.Color := clBlack;
  Image.Canvas.FillRect(R);
  ImageList_Draw(ImageList.Handle, Index, Image.Canvas.Handle, 0, 0, ILD_NORMAL);

  Mask.Canvas.Brush.Color := clWhite;
  Mask.Canvas.FillRect(R);
  ImageList_Draw(ImageList.Handle, Index, Mask.Canvas.Handle, 0, 0, ILD_MASK);
end;

const
  MaskBackground: array[Boolean] of TColor = (clWhite, clBlack);
  crColorPick = -100;

procedure StretchReplace(List: TImageList; Index: Integer; Image, Mask: TBitmap;
  MaskColor: TColor = clDefault);
var
  NewImage, NewMask: TBitmap;
begin
  NewImage := TBitmap.Create;
  try
    NewImage.Assign(Image);
    NewImage.SetSize(List.Width, List.Height);
    NewImage.Canvas.Brush.Color := clBlack;
    Image.TransparentColor := clnone;
    NewImage.Canvas.FillRect(Rect(0, 0, List.Width, List.Height));
    NewImage.Canvas.StretchDraw(Rect(0, 0, List.Width, List.Height), Image);
    if MaskColor <> clDefault then
      NewImage.TransparentColor := MaskColor;
    if Mask <> nil then
    begin
      NewMask := TBitmap.Create;
      try
        NewMask.SetSize(List.Width, List.Height);
        NewMask.Canvas.Brush.Color := MaskBackground[Mask = nil];
        NewMask.Canvas.FillRect(Rect(0,0,List.Width,List.Height));
        NewMask.Canvas.StretchDraw(Rect(0, 0, List.Width, List.Height), Mask);
        List.Replace(Index, NewImage, NewMask);
      finally
        NewMask.Free;
      end;
    end
    else if MaskColor <> clDefault then
      List.ReplaceMasked(Index, NewImage, MaskColor)
    else
      List.Replace(Index, NewImage, nil);
  finally
    NewImage.Free;
  end;
end;

procedure StretchImageList(SrcList, DstList: TImageList; Width, Height: Integer);
var
  I: Integer;
  Image, Mask: TBitmap;
begin
  DstList.SetSize(Width, Height);
  DstList.ColorDepth := SrcList.ColorDepth;
  Image := TBitmap.Create;
  Mask := TBitmap.Create;
  try
    for I := 0 to SrcList.Count - 1 do
    begin
      GetImages(SrcList, I, Image, Mask);
      DstList.Add(nil, nil);
      StretchReplace(DstList, I, Image, Mask);
    end;
  finally
    Mask.Free;
    Image.Free;
  end;
end;

var
  SavedBounds: TRect = (Left: 0; Top: 0; Right: 0; Bottom: 0);

function EditImageList(AImageList: TImageList): Boolean;
var
  I: Integer;
  ListItems: TImageListEditor;
begin
  if (AImageList.Width > 256) or (AImageList.Height > 256) then
    raise Exception.Create('SImageListTooBig');
  ListItems := TImageListEditor.Create(Application);
  with ListItems do
    try
      Screen.Cursor := crHourglass;
      try
        FEditinglist := AImageList;
        ResizeImageGroup;
        FImages.ColorDepth := FEditingList.ColorDepth;
        FImages.Assign(FEditingList);
        FOldMasked := FImages.Masked;
        FOldBkColor := FImages.BkColor;
        FOldBlendColor := FImages.BlendColor;
        FOldDrawingStyle := FImages.DrawingStyle;
        FOldImageType := FImages.ImageType;
        FImages.Masked := True;
        FImages.BkColor := clBtnHighlight;
        FImages.BlendColor := clDefault;
        FImages.DrawingStyle := dsTransparent;
        FImages.ImageType := itImage;

        for I := 0 to FImages.Count - 1 do
          TImageInfo.Create(FImageInfo, ListItems, nil);

        FScaledImages.ColorDepth := FEditingList.ColorDepth;
        FScaledImages.DrawingStyle := dsTransparent;
        FScaledImages.BlendColor := clDefault;
        FScaledImages.BkColor := clBtnHighlight;
        FScaledImages.ImageType := itImage;
        StretchImageList(FImages, FScaledImages, IMAGELIST_SIZE, IMAGELIST_SIZE);

        Caption := Format('SImageListEditorCaption', [AImageList.Owner.Name, DotSep,
          AImageList.Name]);
        if SavedBounds.Right - SavedBounds.Left > 0 then
          BoundsRect := SavedBounds;

        Apply.Enabled := False;
        UpdateUI(False);
        if ImageView.Items.Count > 0 then
          ImageView.ItemIndex := 0;
      finally
        Screen.Cursor := crDefault;
      end;
      Result := ShowModal = mrOk;
      SavedBounds := BoundsRect;
      if Result and Apply.Enabled then ApplyClick(nil);
    finally
      Free;
    end;
end;

{ TImageListEditor }

function TImageListEditor.GetImageInfo(Index: Integer): TImageInfo;
begin
  Result := FImageInfo[Index];
end;

procedure TImageListEditor.HelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TImageListEditor.SetImageFillColor(Color: TColor);
var
  Item: TListItem;
begin
  Screen.Cursor := crHourglass;
  try
    Item := ImageView.Selected;
    while Item <> nil do
    begin
      if Items[Item.Index].CanChangeFill then
        Items[Item.Index].FillColor := Color;
      Item := ImageView.GetNextItem(Item, sdAll, [isSelected]);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImageListEditor.SetImageOperation(Operation: TImageOperation);
var
  Item: TListItem;
begin
  Screen.Cursor := crHourglass;
  try
    Item := ImageView.Selected;
    while Item <> nil do
    begin
      if Items[Item.Index].CanChangeOperation then
        Items[Item.Index].Operation := Operation;
      Item := ImageView.GetNextItem(Item, sdAll, [isSelected]);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImageListEditor.SetImageTransparentColor(Color: TColor);
var
  Item: TListItem;
begin
  Screen.Cursor := crHourglass;
  try
    Item := ImageView.Selected;
    while Item <> nil do
    begin
      if Items[Item.Index].CanChangeTransparent then
        Items[Item.Index].TransparentColor := Color;
      Item := ImageView.GetNextItem(Item, sdAll, [isSelected]);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;


procedure TImageListEditor.TransparentColorChange(Sender: TObject);
begin
  if FUpdating then Exit;

    SetImageTransparentColor(TransparentColor.Selected);
    UpdateUI(True);
end;

function TImageListEditor.UpdateUI(Changed: Boolean): Boolean;
var
  Index: Integer;
begin
  FUpdating :=  True;
  try
    Result := FImages.Count > 0;
    Clear.Enabled := Result;
    ExportBtn.Enabled := Result;

    ReplaceBtn.Enabled := ImageView.SelCount > 0;
    Delete.Enabled := ImageView.SelCount > 0;

    if (ImageView.Selected <> nil) then
    begin
      Index := ImageView.Selected.Index;
      TransparentColor.Enabled :=  Items[Index].CanChangeTransparent;
      FillColor.Enabled := Items[Index].CanChangeFill;
      FillColor.Selected := Items[Index].FillColor;
      TransparentColor.Selected := Items[Index].TransparentColor;
      OptionsGroup.Enabled := Items[Index].CanChangeOperation;
      OptionsGroup.ItemIndex := Integer(Items[Index].Operation);
    end
    else
    begin
      TransparentColor.Selected := clDefault;
      FillColor.Selected := clDefault;
      TransparentColor.Enabled := False;
      FillColor.Enabled := False;
      OptionsGroup.Enabled := False;
      OptionsGroup.ItemIndex := 0;
    end;
    if TransparentColor.Enabled then
      MainImage.Cursor := crColorPick
    else
      MainImage.Cursor := crDefault;
      
    ImageView.Items.BeginUpdate;
    try
      for Index := ImageView.Items.Count - 1 downto FImages.Count do
        ImageView.Items.Delete(Index);
      for Index := 0 to ImageView.Items.Count - 1 do
      begin
        ImageView.Items[Index].Caption := IntToStr(Index);
        ImageView.Items[Index].ImageIndex := Index;
      end;
      for Index := ImageView.Items.Count to FImages.Count - 1 do
        with ImageView.Items.Add do
        begin
          Caption := IntToStr(Index);
          ImageIndex := Index;
        end;

    finally
      ImageView.Items.EndUpdate;
    end;
    ImageView.AlphaSort;
    MainImage.Picture.Bitmap.Canvas.Brush.Color :=  MainPanel.Color;
    MainImage.Picture.Bitmap.Canvas.FillRect(Rect(0,0,FImages.Width, FImages.Height));
    if ImageView.SelCount = 1 then
      FImages.GetBitmap(ImageView.Selected.Index, MainImage.Picture.Bitmap);

    MainPanel.Invalidate;

    if Changed then
      Apply.Enabled := True;

    if not Result then
      ActiveControl := Add;
  finally
    FUpdating := False;
  end;
end;

procedure TImageListEditor.ApplyClick(Sender: TObject);
var
  TempImageList: TImageList;
begin
  Screen.Cursor := crHourglass;
  try
    TempImageList := TImageList.Create(Application);
    TempImageList.Assign(FImages);
    with TempImageList do
    begin
      Masked := FOldMasked;
      BkColor := FOldBkColor;
      BlendColor := FOldBlendColor;
      DrawingStyle := FOldDrawingStyle;
      ImageType := FOldImageType;
    end;
    FEditingList.Assign(TempImageList);
    TempImageList.Free;
    Apply.Enabled := False;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImageListEditor.DeleteSelectedImages;
var
  Item: TListItem;
  I: Integer;
  Index: Integer;
  NewItem: TImageInfo;
begin
  if ImageView.ItemFocused <> nil then
    Index := ImageView.ItemFocused.Index
  else if ImageView.Selected <> nil then
    Index := ImageView.Selected.Index
  else
    Index := 0;
  Screen.Cursor := crHourglass;
  try
    ImageView.Items.BeginUpdate;
    try
      while ImageView.SelCount > 0 do
      begin
        Item := ImageView.Selected;
        if Item <> nil then
        begin
          FImageInfo.Delete(Item.Index);
          FImages.Delete(Item.Index);
          FScaledImages.Delete(Item.Index);
          if Item.Index <= Index then
            Dec(Index);
          Item.Free;
        end;
      end;
      //Update ImageView label/images and selected item
      for I := 0 to ImageView.Items.Count - 1 do
      begin
        ImageView.Items[I].Caption := IntToStr(I);
        ImageView.Items[I].ImageIndex := I;
      end;
      if Index+1 < ImageView.Items.Count then
        ImageView.ItemIndex := Index+1
      else
        ImageView.ItemIndex := ImageView.Items.Count -1;
    finally
      ImageView.Items.EndUpdate;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

function TImageListEditor.DoAdd(Icon: TIcon; Index: Integer): Integer;
var
  NewBitmap: TBitmap;
begin
  Result := 0;
  NewBitmap := TBitmap.Create;
  try
    NewBitmap.PixelFormat := pf32bit;

    NewBitmap.Height := Icon.Height;
    NewBitmap.Width := Icon.Width;
    NewBitmap.Canvas.Brush.Color := clWhite;
    NewBitmap.Canvas.FillRect(NewBitmap.Canvas.ClipRect);
    NewBitmap.Canvas.Draw(0, 0, Icon);

    Result := DoAdd(NewBitmap, Index);
    if Result <> 0 then
    begin
      Items[Index].FCanChangeTransparent := False;
      Items[Index].FCanChangeFill := False;
      Items[Index].FCanChangeOperation := False;
      Items[Index].FFillColor := clDefault;
      Items[Index].FTransparentColor := clDefault;
      Items[Index].FOperation := ioCenter;
    end;
  finally
    NewBitmap.Free;
  end;
end;

function TImageListEditor.DoAdd(Graphic: TGraphic; Index: Integer): Integer;
begin
  FImages.Insert(Index, nil, nil);
  FScaledImages.Insert(Index, nil , nil);

  TImageInfo.Create(FImageInfo, Self, Graphic, Index);

  Result := 1;
end;

function TImageListEditor.DoAdd(Graphic: TGraphic; Index, DivX,
  DivY: Integer): Integer;
var
  Bmp: TBitmap;
  Clip: TBitmap;
  X, Y: Integer;
  LWidth, LHeight: Integer;
begin
  Result := 0;
  Bmp := TBitmap.Create;
  Clip := TBitmap.Create;
  try
    Bmp.Assign(Graphic);
    Clip.SetSize(FImages.Width, FImages.Height);
    Clip.PixelFormat :=  Bmp.PixelFormat;
    Clip.AlphaFormat := afIgnored;
    LWidth := FImages.Width;
    LHeight := FImages.Height;

    for Y := 0 to DivY - 1 do
      for X := 0 to DivX - 1 do
      begin
        Clip.Canvas.Brush.Color := clBlack;
        Clip.Canvas.FillRect(Rect(0,0,LWidth, LHeight));
        Clip.Canvas.CopyRect(Rect(0,0,LWidth, LHeight), bmp.Canvas,
          Rect(x*LWidth, y * LHeight, (X+1) * LWidth, (Y+1) * LHeight));
        DoAdd(Clip, Index);
        Inc(Index);
        Inc(Result);
      end;


  finally
    Bmp.Free;
    Clip.Free;
  end;

end;


procedure TImageListEditor.ClearAllImages;
begin
  Screen.Cursor := crHourglass;
  try
    ImageView.Items.BeginUpdate;
    try
      ImageView.Items.Clear;
    finally
      ImageView.Items.EndUpdate;
    end;
    FImages.Clear;
    FScaledImages.Clear;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TImageListEditor.MainImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPickingColor := True;
  UpdatePickColor(X, Y);
end;

type
  THackPanel = class(TPanel);

procedure TImageListEditor.UpdatePickColor(X, Y: Integer);
var
  Item: TListItem;
  P: TPoint;
  Color : TColor;
  Changed: Boolean;
begin
  Changed := False;
  P := MainPanel.ScreenToClient(MainImage.ClientToScreen(Point(X, Y)));
  Color := THackPanel(MainPanel).Canvas.Pixels[P.X, P.Y];
  Item := ImageView.Selected;
  while Item <> nil do
  begin
    if Items[Item.Index].CanChangeTransparent then
    begin
      Items[Item.Index].TransparentColor := Color;
      Changed := True;
    end;
    Item := ImageView.GetNextItem(Item, sdAll, [isSelected]);
  end;
  if Changed then
    UpdateUI(True);
end;

procedure TImageListEditor.MainImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if not FPickingColor then Exit;
  UpdatePickColor(X, Y);
end;

procedure TImageListEditor.MainImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FPickingColor := False;
end;

procedure TImageListEditor.MoveImage(FromIndex, ToIndex: Integer);
begin
  if FromIndex = ToIndex then
    Exit;

  FImageInfo.Move(FromIndex, ToIndex);
  FScaledImages.Move(FromIndex, ToIndex);
  FImages.Move(FromIndex, ToIndex);
end;

procedure TImageListEditor.OptionsGroupClick(Sender: TObject);
begin
  if FUpdating then Exit;

  SetImageOperation(TImageOperation(OptionsGroup.ItemIndex));
  UpdateUI(True);
end;

procedure TImageListEditor.ClearClick(Sender: TObject);
begin
  ClearAllImages;
  UpdateUI(True);
end;

procedure TImageListEditor.ImageViewCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare := StrToIntDef(Item1.Caption, 0) - StrToIntDef(Item2.Caption, 0);
end;

procedure TImageListEditor.ImageViewDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  FromIndex, ToIndex: Integer;
  R: TRect;
begin
  if not Assigned(ImageView.ItemFocused) or not Assigned(ImageView.DropTarget) then
    Exit;

  FromIndex := ImageView.Selected.Index;
  
  ToIndex := ImageView.DropTarget.Index;
  R := ImageView.DropTarget.DisplayRect(drBounds);
  if X > (R.Left + R.Right) /2 then
    ToIndex := ToIndex + 1;

  if ToIndex > FromIndex then
    ToIndex := ToIndex -1;

  if FromIndex <> ToIndex then
  begin
    MoveImage(FromIndex, ToIndex);
    ImageView.Selected := nil;
    UpdateUI(True);
    ImageView.ItemIndex := ToIndex;
  end;
end;

procedure TImageListEditor.ImageViewDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := True;
  { Detect dragging over scroll region.
    Use Tag to store state of scrolling:
      1 = Scroll Left, 2 = Scroll Right
      4 = Scroll Up,   8 = Scroll Down}
  DragTimer.Tag := 0;
  with Sender as TListView do
  begin
    if X <= 20 then
      DragTimer.Tag := 1
    else if X >= ClientWidth - 20 then
      DragTimer.Tag := 2;

    if Y <= 20 then
      DragTimer.Tag := DragTimer.Tag + 4
    else if Y >= ClientHeight - 20 then
      DragTimer.Tag := DragTimer.Tag + 8;
  end;
  if DragTimer.Tag = 0 then
    DragTimer.Enabled := False
  else
    DragTimer.Enabled := True;

end;

procedure TImageListEditor.ImageViewEndDrag(Sender, Target: TObject; X,
  Y: Integer);
begin
  DragTimer.Enabled := False;
end;

procedure TImageListEditor.ImageViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateUI(False);
end;

procedure TImageListEditor.DeleteClick(Sender: TObject);
begin
  DeleteSelectedImages;
  UpdateUI(True);
end;

procedure TImageListEditor.DragTimerTimer(Sender: TObject);
var
  Temp: TDragImageList;
  DX, DY: Integer;
begin

  DX := 0;
  DY := 0;
  if (DragTimer.Tag and 1) = 1 then
    DX := -FScaledImages.Width;
  if (DragTimer.Tag and 2) = 2 then
    DX := FScaledImages.Width;
  if (DragTimer.Tag and 4) = 4 then
    DY := -FScaledImages.Height;
  if (DragTimer.Tag and 8) = 8 then
    DY := FScaledImages.Height;

  ImageView.Scroll(DX, DY);
end;

procedure TImageListEditor.ExportBtnClick(Sender: TObject);
var
  UnusedColor: TColor;
  ImageStrip: TBitmap;
  ImageCount: Integer;
  StripWidth, StripHeight: Integer;

  function FindUnusedColor(ImageList: TCustomImageList): TColor;
{  var
    B: TBitmap;}
  begin
{
    B := TBitmap.Create;
    try
      B.Width := ImageList.Width;
      B.Height := ImageList.Height;
      for I := 0 to ImageList.Count - 1 do
      begin
        if ImageView.Items[I].Selected then
          ImageList.GetBitmap(I, B);
      end;
    finally
      B.Free;
    end;
}
    Result := clFuchsia;
  end;

  procedure CreateImageStrip(var Strip: TBitmap; ImageList: TImageList;
    BkColor: TColor);
  var
    I, J, K: Integer;
  begin
    with Strip do
    begin
      Canvas.Brush.Color := BkColor;
      Canvas.FillRect(Rect(0, 0, Strip.Width, Strip.Height));
      J := 0;
      K := 0;
      for I := 0 to ImageList.Count - 1 do
        if (ImageView.SelCount = 0) or ImageView.Items[I].Selected then
        begin
          ImageList.Draw(Canvas, J * ImageList.Width, K * ImageList.Height, I, dsTransparent, itImage);
          Inc(J);
          if J >= StripWidth then
          begin
            J := 0;
            Inc(K);
          end;
        end;
    end;
  end;

  procedure CalcDimensions(Count: Integer; var AWidth, AHeight: Integer);
  var
    X: Double;
  begin
    X := Sqrt(Count);
    AWidth := Trunc(X);
    if Frac(X) > 0 then
      Inc(AWidth);
    X := Count / AWidth;
    AHeight := Trunc(X);
    if Frac(X) > 0 then
      Inc(AHeight);
  end;

begin
  { Find unused color }
  UnusedColor := FindUnusedColor(FImages);
  { Couldn't find a color unused by the image -- notify the user }
  if UnusedColor = clNone then
    MessageDlg('sNoUnusedColors', mtWarning, [mbOk], 0);

    { Save image strip }
    if SaveDialog.Execute then
    begin
      { Build image strip }
      ImageStrip := TBitmap.Create;
      try
        if ImageView.SelCount = 0 then
          ImageCount := FImages.Count
        else
          ImageCount := ImageView.SelCount;
        CalcDimensions(ImageCount, StripWidth, StripHeight);
        ImageStrip.Width := StripWidth * FImages.Width;
        ImageStrip.Height := StripHeight * FImages.Height;
        CreateImageStrip(ImageStrip, FImages, UnusedColor);

        ImageStrip.SaveToFile(SaveDialog.FileName);
      finally
        ImageStrip.Free;
      end;
    end;
end;

procedure TImageListEditor.FillColorChange(Sender: TObject);
begin
  if FUpdating then Exit;

  SetImageFillColor(FillColor.Selected);
  UpdateUI(True);
end;

procedure TImageListEditor.FormCreate(Sender: TObject);
begin
  FUpdating := False;

  FImages := TImageList.Create(nil);
  FScaledImages := TImageList.CreateSize(IMAGELIST_SIZE,IMAGELIST_SIZE);
  ImageView.LargeImages := FScaledImages;
  ImageView.SmallImages := FScaledImages;

  GetColorValues(AddColor);
  TransparentColor.ItemIndex := -1;
  FillColor.ItemIndex := -1;

  Screen.Cursors[crColorPick] := LoadCursor(HInstance, 'COLORPICK');

  FImageInfo := TList.Create;
end;

procedure TImageListEditor.FormDestroy(Sender: TObject);
var
  AImageInfo: TImageInfo;
begin
  Screen.Cursors[crColorPick] := 0;

  FImages.Free;
  FScaledImages.Free;

  while FImageInfo.Count > 0 do TImageInfo(FImageInfo.Last).Free;
  FImageInfo.Free;
end;

procedure TImageListEditor.FormResize(Sender: TObject);
var
  R: TRect;
  NewArrage: TIconArrangement;
  SelIndexes: TArray<Integer>;
  Item: TListItem;
  I: Integer;
begin
  if ImageView.Items.Count = 0 then Exit;
  if FItemHeight = 0 then
  begin
    R := ImageView.Items[0].DisplayRect(drBounds);
    FItemHeight := 2*(R.Bottom - R.Top) + GetSystemMetrics(SM_CYHSCROLL) + 6;
  end;
  if ImageView.Height > FItemHeight then
    NewArrage := iaTop
  else
    NewArrage := iaLeft;
  if NewArrage <> ImageView.IconOptions.Arrangement then
  begin
    SetLength(SelIndexes, ImageView.SelCount);
    Item := ImageView.Selected;
    I := 0;
    while Item <> nil do
    begin
      SelIndexes[I] := Item.Index;
      Item := ImageView.GetNextItem(item, sdAll, [isSelected]);
      Inc(I);
    end;
    ImageView.IconOptions.Arrangement := NewArrage;
    for I := 0 to Length(SelIndexes) - 1 do
      ImageView.Items[SelIndexes[I]].Selected := True;
  end;
end;

procedure TImageListEditor.AddClick(Sender: TObject);
var
  I: Integer;
  IWidth, IHeight: Integer;
  XDiv, YDiv: Integer;
  InsertIndex: Integer;
  MsgDlgBtns: TMsgDlgButtons;
  DialogResult: TModalResult;
  Picture: TPicture;
begin
  DialogResult := mrNo;
  OpenDialog.Title := 'SAddImagesTitle';
  OpenDialog.DefaultExt := GraphicExtension(TGraphic);
  OpenDialog.Filter := GraphicFilter(TGraphic);
  if OpenDialog.Execute(Handle) then
  begin
    Screen.Cursor := crHourglass;
    try
      Picture := TPicture.Create;
      try
        if Sender = ReplaceBtn then
          DeleteSelectedImages;
        if (ImageView.Selected <> nil) then
        begin
          InsertIndex := ImageView.Selected.Index;
          if Sender = Add then
            Inc(InsertIndex);
        end
        else
          InsertIndex := ImageView.Items.Count;
        ImageView.Selected := nil;
        IWidth := FImages.Width;
        IHeight := FImages.Height;
        MsgDlgBtns := [mbYes, mbNo];
        if OpenDialog.Files.Count > 1 then
          msgDlgBtns := MsgDlgBtns + [mbNoToAll, mbYesToAll];
        for I := 0 to OpenDialog.Files.Count - 1 do
        begin
          Picture.LoadFromFile(OpenDialog.Files[I]);
          if Picture.Graphic is TIcon then
          begin
            Inc(InsertIndex, DoAdd(TIcon(Picture.Graphic), InsertIndex))
          end
          else
          begin
            if (Picture.Width > IWidth) and (Picture.Width mod IWidth = 0) then
              XDiv := Picture.Width div IWidth
            else
              XDiv := 1;
            if (Picture.Height > IHeight) and (Picture.Height mod IHeight = 0) then
              YDiv := Picture.Height div IHeight
            else
              YDiv := 1;
            // Check to see if should split
            if not (DialogResult in [mrNoToAll, mrYesToAll]) then
              DialogResult := mrNo;
            if ((XDiv > 1) or (YDiv > 1)) and
              not (DialogResult in [mrNoToAll, mrYesToAll]) then
              DialogResult := MessageDlg(Format('SImageListDivide',
                [ExtractFileName(OpenDialog.Files[I]), XDiv * YDiv]),
                mtConfirmation, MsgDlgBtns, 0);
            if DialogResult in [mrNo, mrNoToAll] then //Add it as is
              Inc(InsertIndex, DoAdd(Picture.Graphic, InsertIndex))
            else //Split into smaller images
              Inc(InsertIndex, DoAdd(Picture.Graphic, InsertIndex, XDiv, YDiv));
          end;
        end;
        UpdateUI(True);
        ImageView.ItemIndex := InsertIndex - 1;
        ImageView.Selected.MakeVisible(False);
      finally
        Picture.Free;
      end;
    finally
      Screen.Cursor := crDefault;
    end;
  end;

end;

procedure TImageListEditor.AddColor(const S: string);
begin
  TransparentColor.Items.Add(S);
  FillColor.Items.Add(S);
end;

procedure TImageListEditor.Replace(Index: Integer; Graphic: TGraphic;
  Transparent, Fill: TColor; Operation: TImageOperation);
var
  Bmp: TBitmap;
  R: TRect;
begin
  Bmp := TBitmap.Create;
  try
    Bmp.SetSize(FImages.Width, FImages.Height);
    bmp.PixelFormat := pf32bit;
    bmp.alphaFormat := afIgnored;
    if Fill = clDefault then
      Bmp.Canvas.Brush.Color := clBlack
    else
      Bmp.Canvas.Brush.Color := Fill;
    Bmp.Canvas.FillRect(Rect(0,0,FImages.Width, FImages.Height));

    case Operation of
      ioCrop:
        Bmp.Canvas.StretchDraw(Rect(0,0, Graphic.Width, graphic.Height), Graphic);
      ioStretch:
        Bmp.Canvas.StretchDraw(Rect(0,0, FImages.Width, FImages.Height), Graphic);
      ioCenter:
        Bmp.Canvas.StretchDraw(Bounds((FImages.Width - Graphic.Width) div 2,
          (FImages.Height - Graphic.Height) div 2, Graphic.Width, Graphic.Height),
          Graphic);
    end;

    if Transparent = clDefault then
    begin
      FImages.Replace(Index, Bmp, nil);
      StretchReplace(FScaledImages, Index, Bmp, nil);
    end
    else
    begin
      //FImages.Replace(Index, Bmp, nil);
      //StretchReplace(FScaledImages, Index, Bmp, nil);
      FImages.ReplaceMasked(Index, Bmp, Transparent);
      StretchReplace(FScaledImages, Index, Bmp, nil, Transparent);
    end;

  finally
    Bmp.Free;
  end;

end;

procedure TImageListEditor.ResizeImageGroup;
var
  AdjustHeight : Integer;
begin
  // For Wide ImageLists, shift the options panel Right.
  if FEditingList.Width > 64 then
  begin
    MainPanel.Width := 14 + FEditingList.Width;
    OptionsPanel.Left := MainPanel.Left + MainPanel.Width;
    OptionsPanel.Width := ImageGroup.Left + ImageGroup.Width - OptionsPanel.Left - 20;
  end;
  // For large ImageLists, shift the listview portion down.
  if FEditingList.Height > 64 then
  begin
    AdjustHeight := FEditingList.Height - 64;
    MainPanel.Height := 14 + FEditingList.Height;
    if MainPanel.Top + MainPanel.Height + 11 > ImageGroup.Height then
      ImageGroup.Height := MainPanel.Top + MainPanel.Height + 11;
    ImageListGroup.Top := ImageGroup.Top + ImageGroup.Height + 11;
    ImageListGroup.Height := ClientHeight - ImageListGroup.Top - 11;
    // If there's enough room, display OptionsGroup vertically.
    if OptionsPanel.Height > OptionsGroup.Top +  50 + 13 * 3 then
    begin
      AdjustHeight := AdjustHeight - OptionsGroup.Height;
      OptionsGroup.Height := 50 + 13 * 3;
      OptionsGroup.Columns := 1;
      OptionsGroup.Width := OptionsPanel.Width - 17;
    end;
    Constraints.MinHeight := Constraints.MinHeight + AdjustHeight;
  end;
end;

{ TImageInfo }

constructor TImageInfo.Create(AOwner: TList; AOwnerForm: TImageListEditor;
  AGraphic: TGraphic; OwnerIndex: Integer = -1);
var
  LGraphicClass: TGraphicClass;
  LWidth, LHeight: Integer;
begin
  FOwner := AOwner;
  FOwnerForm := AOwnerForm;
  if OwnerIndex > -1 then
  begin
    if OwnerIndex > AOwner.Count then
      OwnerIndex := AOwner.Count;
    AOwner.Insert(OwnerIndex, Self);
  end else
    AOwner.Add(Self);

  if Assigned(AGraphic) then
  begin
    FGraphic := TGraphicClass(AGraphic.ClassType).create;
    FGraphic.Assign(AGraphic);
    LWidth := FOwnerForm.FEditingList.Width;
    LHeight := FOwnerForm.FEditingList.Height;
    if Graphic is TBitmap and (TBitmap(Graphic).AlphaFormat = afIgnored ) then
      FTransparentColor := TBitmap(Graphic).TransparentColor and $FFFFFF
    else
      FTransparentColor := clDefault;

    FCanChangeTransparent := (Graphic is TBitmap and
      (TBitmap(Graphic).AlphaFormat = afIgnored)) or
      (not (Graphic is TBitmap) and not Graphic.Transparent);
    FCanChangeFill := (Graphic.Width < LWidth) or (Graphic.Height < LHeight);

    if FCanChangeFill then
      FFillColor := FTransparentColor
    else
      FFillColor := clDefault;

    FCanChangeOperation := (Graphic.Width <> LWidth) or (Graphic.Height <> LHeight);
    if (Graphic.Width < LWidth) and (Graphic.Height < LHeight) then
      FOperation := ioCenter
    else if (Graphic.Width > LWidth) or (Graphic.Height > LHeight) then
      FOperation := ioStretch
    else
      FOperation := ioCenter;
    FOwnerForm.Replace(OwnerIndex, Graphic, FTransparentColor, FFillColor, FOperation);

  end
  else
  begin
    FOperation := ioCrop;
    FTransparentColor := clDefault;
    FFillColor := clDefault;
    FCanChangeTransparent := False;
    FCanChangeFill := False;
    FCanChangeOperation := False;
  end;
end;

destructor TImageInfo.Destroy;
begin
  if Assigned(FGraphic) then
    FGraphic.Free;
  FOwner.Remove(Self);
  FOwner := nil;

  inherited;
end;

procedure TImageInfo.SetCanChangeFill(const Value: Boolean);
begin
  FCanChangeFill := Value;
end;

procedure TImageInfo.SetCanChangeOperation(const Value: Boolean);
begin
  FCanChangeOperation := Value;
end;

procedure TImageInfo.SetCanChangeTransparent(const Value: Boolean);
begin
  FCanChangeTransparent := Value;
end;

procedure TImageInfo.SetFillColor(const Value: TColor);
begin
  if CanChangeFill and (Value <> FillColor) then
  begin
    FFillColor := Value;
    Change;
  end;
end;

procedure TImageInfo.SetOperation(const Value: TImageOperation);
begin
  if CanChangeOperation and (Value <> Operation) then
  begin
    FOperation := Value;
    Change;
  end;
end;
procedure TImageInfo.SetTransparentColor(const Value: TColor);
begin
  if CanChangeTransparent and (Value <> TransparentColor) then
  begin
    FTransparentColor := Value;
    Change;
  end;
end;

procedure TImageInfo.Change;
begin
  FOwnerForm.Replace(FOwner.IndexOf(Self), FGraphic, TransparentColor, 
    FillColor, Operation);
end;


end.
