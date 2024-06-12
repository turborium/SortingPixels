// .d8888b.  8888888b.  8888888b.       888888b.         d8888  .d8888b.  8888888888 8888888b.
// d88P  Y88b 888  "Y88b 888  "Y88b      888  "88b       d88888 d88P  Y88b 888        888  "Y88b
// 888    888 888    888 888    888      888  .88P      d88P888 Y88b.      888        888    888
// 888        888    888 888    888      8888888K.     d88P 888  "Y888b.   8888888    888    888
// 888  88888 888    888 888    888      888  "Y88b   d88P  888     "Y88b. 888        888    888
// 888    888 888    888 888    888      888    888  d88P   888       "888 888        888    888
// Y88b  d88P 888  .d88P 888  .d88P      888   d88P d8888888888 Y88b  d88P 888        888  .d88P
// "Y8888P88 8888888P"  8888888P"       8888888P" d88P     888  "Y8888P"  8888888888 8888888P"
//
// Turborium(c) 2024-2024

unit UnitMain;

{$mode delphiunicode}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Menus, Buttons, ExtDlgs, Math, LazPNG,
  Generics.Defaults, BitmapPixels, SortingPixelsAlgorithm;

type

  { TSortThread }

  TSortThread = class(TThread)
  private
    FBitmap: TBitmap;
    FThresholdMax: Integer;
    FThresholdMin: Integer;
    FSortingDirection: TSortingDirection;
  protected
    procedure Execute(); override;
  public
    constructor Create();
    destructor Destroy(); override;

    property Bitmap: TBitmap read FBitmap;
    property SortingDirection: TSortingDirection read FSortingDirection write FSortingDirection;
    property ThresholdMin: Integer read FThresholdMin write FThresholdMin;
    property ThresholdBright: Integer read FThresholdMax write FThresholdMax;
  end;

  { TFormMain }

  TFormMain = class(TForm)
    BitBtnDown: TBitBtn;
    BitBtnLeft: TBitBtn;
    BitBtnRight: TBitBtn;
    BitBtnUp: TBitBtn;
    ButtonOpen: TButton;
    ButtonOpenSample: TButton;
    ButtonSave: TButton;
    CheckBoxAutoShowEnabled: TCheckBox;
    GroupBoxAutoShow: TGroupBox;
    GroupBoxImageScale: TGroupBox;
    GroupBoxSortDirection: TGroupBox;
    GroupBoxTrashhold: TGroupBox;
    ImageDisp: TImage;
    ImageListSample: TImageList;
    ImageListSortDirection: TImageList;
    LabelAutoShowMaxRange: TLabel;
    LabelAutoShowMinRange: TLabel;
    LabelThresholdMin: TLabel;
    LabelThresholdMax: TLabel;
    LabelThresholdMaxCaption: TLabel;
    LabelThresholdMinCaption: TLabel;
    LabelScale: TLabel;
    OpenPictureDialog: TOpenPictureDialog;
    PanelAutoShow: TPanel;
    PanelParams: TPanel;
    PanelFile: TPanel;
    PanelSortDirection: TPanel;
    PopupMenuSample: TPopupMenu;
    RadioGroupSaveScale: TRadioGroup;
    RadioGroupViewScale: TRadioGroup;
    SavePictureDialog: TSavePictureDialog;
    ScrollBoxDisp: TScrollBox;
    TimerAutoShow: TTimer;
    TrackBarAutoShowTresholdMin: TTrackBar;
    TrackBarAutoShowTresholdMax: TTrackBar;
    TrackBarThresholdMin: TTrackBar;
    TrackBarThresholdMax: TTrackBar;
    TrackBarImageScale: TTrackBar;
    procedure BitBtnDownClick(Sender: TObject);
    procedure BitBtnLeftClick(Sender: TObject);
    procedure BitBtnRightClick(Sender: TObject);
    procedure BitBtnUpClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonOpenSampleClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure CheckBoxAutoShowEnabledChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PanelParamsResize(Sender: TObject);
    procedure RadioGroupSaveScaleClick(Sender: TObject);
    procedure RadioGroupViewScaleClick(Sender: TObject);
    procedure LoadSampleToBitmap(var Bitmap: TBitmap; Index: Integer);
    procedure TimerAutoShowTimer(Sender: TObject);
    procedure TrackBarAutoShowTresholdMaxChange(Sender: TObject);
    procedure TrackBarAutoShowTresholdMinChange(Sender: TObject);
    procedure TrackBarImageScaleChange(Sender: TObject);
    procedure TrackBarThresholdMaxChange(Sender: TObject);
    procedure TrackBarThresholdMinChange(Sender: TObject);
  private
    FRawSource: TBitmap;
    FSource: TBitmap;
    FDestination: TBitmap;

    FViewScale: Integer;
    FImageScale: Integer;
    FSortingDirection: TSortingDirection;
    FTresholdMax: Integer;
    FTresholdMin: Integer;
    FSaveScale: Integer;

    FSortThread: TSortThread;
    FIsNeedUpdate: Boolean;

    FAutoShowEnabled: Boolean;
    FAutoShowRangeMin: Integer;
    FAutoShowRangeMax: Integer;
    FAutoShowDeltaMin: Double;
    FAutoShowDeltaMax: Double;
    FAutoShowTresholdMax: Double;
    FAutoShowTresholdMin: Double;

    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure LoadSample(Index: Integer);
    procedure PopupSampleClick(Sender: TObject);
    procedure SortThreadTerminate(Sender: TObject);
    procedure UpdateSource();
    procedure UpdateRawSource();
    procedure UpdateUI();
    procedure NeedUpdate();
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

const
  SampleCount = 15;
  MaxSourceSize = 3000;

{ TSortThread }

constructor TSortThread.Create();
begin
  inherited Create(True);

  FBitmap := TBitmap.Create();
end;

destructor TSortThread.Destroy();
begin
  FBitmap.Free();

  inherited Destroy();
end;

procedure TSortThread.Execute();
var
  Data: TBitmapData;
begin
  Data.Map(FBitmap, TAccessMode.ReadWrite, False);
  try
    ImageSortingPixels(Data, FSortingDirection, FThresholdMin, FThresholdMax);
  finally
    Data.Unmap();
  end;
end;

{ TFormMain }

constructor TFormMain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FormatSettings.DecimalSeparator := '.';
  FSource := TBitmap.Create();
  FRawSource := TBitmap.Create();
  FDestination := TBitmap.Create();

  // defaults
  Randomize();
  FTresholdMax := 255;
  FTresholdMin := 0;
  FViewScale := 1;
  FImageScale := 100;
  FSaveScale := 3;
  FSortingDirection := TSortingDirection.Up;
  FAutoShowRangeMin := 0 + 100;
  FAutoShowRangeMax := 255 - 100;
  FAutoShowEnabled := False;
end;

destructor TFormMain.Destroy();
begin
  FSource.Free();
  FRawSource.Free();
  FDestination.Free();

  inherited Destroy();
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.AddOnIdleHandler(Idle, True);

  // load random sample
  LoadSample(Random(SampleCount));
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  Application.RemoveOnIdleHandler(Idle);

  if FSortThread <> nil then
    FSortThread.WaitFor();
end;

procedure TFormMain.PanelParamsResize(Sender: TObject);
begin
  GroupBoxAutoShow.Visible := GroupBoxAutoShow.Left + GroupBoxAutoShow.Width <= PanelParams.ClientWidth;
end;

procedure TFormMain.RadioGroupSaveScaleClick(Sender: TObject);
begin
  FSaveScale := RadioGroupSaveScale.ItemIndex + 1;

  NeedUpdate();
end;

procedure TFormMain.SortThreadTerminate(Sender: TObject);
begin
  // copy pixels
  FDestination.LoadFromRawImage(FSortThread.Bitmap.RawImage, False);

  ImageDisp.Picture.Assign(FDestination);
  ImageDisp.Width := ImageDisp.Picture.Width * FViewScale;
  ImageDisp.Height := ImageDisp.Picture.Height * FViewScale;
  ImageDisp.Stretch := True;
  ImageDisp.Repaint;

  FSortThread := nil;
end;

procedure TFormMain.Idle(Sender: TObject; var Done: Boolean);
begin
  if FIsNeedUpdate then
  begin
    // если потока нет - создаем и сбрасываем флаг необходимости обновления
    if FSortThread = nil then
    begin
      FSortThread := TSortThread.Create();
      FSortThread.FreeOnTerminate := True;
      FSortThread.OnTerminate := SortThreadTerminate;

      // copy pixels
      FSortThread.Bitmap.LoadFromRawImage(FSource.RawImage, False);

      FSortThread.SortingDirection := FSortingDirection;
      FSortThread.ThresholdMin := FTresholdMin;
      FSortThread.ThresholdBright := FTresholdMax;

      FSortThread.Start();

      FIsNeedUpdate := False;
    end;
  end;
end;

procedure TFormMain.RadioGroupViewScaleClick(Sender: TObject);
begin
  FViewScale := RadioGroupViewScale.ItemIndex + 1;

  NeedUpdate();
end;

procedure TFormMain.PopupSampleClick(Sender: TObject);
begin
  LoadSample(TMenuItem(Sender).Tag);
end;

procedure TFormMain.ButtonOpenSampleClick(Sender: TObject);
var
  Item: TMenuItem;
  I: Integer;
  Bitmap: TBitmap;
  PopupPoint: TPoint;
begin
  // delay loading
  if PopupMenuSample.Items.Count = 0 then
  begin
    Screen.BeginWaitCursor();
    try
      // load images
      for I := 0 to SampleCount - 1 do
      begin
        Bitmap := TBitmap.Create();
        try
          LoadSampleToBitmap(Bitmap, I);
          ImageListSample.Add(Bitmap, nil);
        finally
          Bitmap.Free();
        end;
      end;
      // make items
      for I := 0 to SampleCount - 1 do
      begin
        Item := TMenuItem.Create(Self);
        Item.Caption := 'Sample ' + IntToStr(I);
        Item.ImageIndex := I;
        Item.OnClick := PopupSampleClick;
        Item.Tag := I;
        PopupMenuSample.Items.Add(Item);
      end;
    finally
      Screen.EndWaitCursor();
    end;
  end;

  // popup
  PopupPoint := ButtonOpenSample.ClientToScreen(Point(ButtonOpenSample.Width, ButtonOpenSample.Height));
  PopupMenuSample.PopUp(PopupPoint.X, PopupPoint.Y);
end;

procedure TFormMain.ButtonSaveClick(Sender: TObject);
var
  Picture: TPicture;
begin
  if FSortThread <> nil then
    FSortThread.WaitFor();

  if SavePictureDialog.Execute then
  begin
    Picture := TPicture.Create();
    try
      Picture.Bitmap.SetSize(FDestination.Width * FSaveScale, FDestination.Height * FSaveScale);
      Picture.Bitmap.Canvas.AntialiasingMode := amOff;
      Picture.Bitmap.Canvas.StretchDraw(Rect(0, 0, Picture.Bitmap.Width, Picture.Bitmap.Height), FDestination);

      // jpeg quality fix
      if TPicture.FindGraphicClassWithFileExt(ExtractFileExt(SavePictureDialog.FileName)) = TJPEGImage then
        Picture.Jpeg.CompressionQuality := 95;

      Picture.SaveToFile(SavePictureDialog.FileName);
    finally
      Picture.Free();
    end;
  end;

  NeedUpdate();
end;

procedure TFormMain.CheckBoxAutoShowEnabledChange(Sender: TObject);
begin
  FAutoShowEnabled := CheckBoxAutoShowEnabled.Checked;

  NeedUpdate();
end;

procedure TFormMain.BitBtnUpClick(Sender: TObject);
begin
  FSortingDirection := TSortingDirection.Up;

  NeedUpdate();
end;

procedure TFormMain.ButtonOpenClick(Sender: TObject);
var
  Picture: TPicture;
begin
  if OpenPictureDialog.Execute then
  begin
    Picture := TPicture.Create();
    try
      Picture.LoadFromFile(OpenPictureDialog.FileName);
      FRawSource.Assign(Picture.Graphic);
    finally
      Picture.Free();
    end;

    SavePictureDialog.FileName := ChangeFileExt(ExtractFileName(OpenPictureDialog.FileName), '');

    UpdateRawSource();
  end;
end;

procedure TFormMain.BitBtnLeftClick(Sender: TObject);
begin
  FSortingDirection := TSortingDirection.Left;

  NeedUpdate();
end;

procedure TFormMain.BitBtnDownClick(Sender: TObject);
begin
  FSortingDirection := TSortingDirection.Down;

  NeedUpdate();
end;

procedure TFormMain.BitBtnRightClick(Sender: TObject);
begin
  FSortingDirection := TSortingDirection.Right;

  NeedUpdate();
end;

procedure TFormMain.TrackBarImageScaleChange(Sender: TObject);
begin
  FImageScale := TrackBarImageScale.Position;
  UpdateSource();

  NeedUpdate();
end;

procedure TFormMain.TrackBarThresholdMaxChange(Sender: TObject);
begin
  FTresholdMax := TrackBarThresholdMax.Position;
  if FTresholdMin > FTresholdMax then
    FTresholdMin := FTresholdMax;

  NeedUpdate();
end;

procedure TFormMain.TrackBarThresholdMinChange(Sender: TObject);
begin
  FTresholdMin := TrackBarThresholdMin.Position;
  if FTresholdMin > FTresholdMax then
    FTresholdMax := FTresholdMin;

  NeedUpdate();
end;

procedure TFormMain.LoadSampleToBitmap(var Bitmap: TBitmap; Index: Integer);
var
  Png: TPNGImage;
begin
  Png := TPNGImage.Create();
  try
    Png.LoadFromResourceName(HInstance, IntToStr(Index));
    Bitmap.Assign(Png);
  finally
    Png.Free();
  end;
end;

procedure TFormMain.TimerAutoShowTimer(Sender: TObject);
const
  Vals: array of Double = [-1, -0.8, -0.5, -0.3, 0.3, 0.5, 0.8, 1];
begin

  if Random(20) = 0 then
  begin
    FAutoShowDeltaMin := RandomFrom(Vals);
  end;

  if Random(20) = 0 then
  begin
    FAutoShowDeltaMax := RandomFrom(Vals);
  end;

  FAutoShowTresholdMin := FAutoShowTresholdMin + FAutoShowDeltaMin;
  FAutoShowTresholdMax := FAutoShowTresholdMax + FAutoShowDeltaMax;

  if (FAutoShowTresholdMax > 255) or (FAutoShowTresholdMax < FAutoShowRangeMax) then
  begin
    FAutoShowDeltaMax := -FAutoShowDeltaMax;
    if FAutoShowTresholdMax < FAutoShowRangeMax then
      FAutoShowTresholdMax := FAutoShowRangeMax
    else
      FAutoShowTresholdMax := 255;
  end;

  if (FAutoShowTresholdMin > FAutoShowRangeMin) or (FAutoShowTresholdMin < 0) then
  begin
    FAutoShowDeltaMin := -FAutoShowDeltaMin;
    if FAutoShowTresholdMin > FAutoShowRangeMin then
      FAutoShowTresholdMin := FAutoShowRangeMin
    else
      FAutoShowTresholdMin := 0;
  end;

  FTresholdMin := Trunc(FAutoShowTresholdMin);
  FTresholdMax := Trunc(FAutoShowTresholdMax);

  UpdateUI();
end;

procedure TFormMain.TrackBarAutoShowTresholdMaxChange(Sender: TObject);
begin
  FAutoShowRangeMax := TrackBarAutoShowTresholdMax.Position;
  if FAutoShowRangeMin > FAutoShowRangeMax then
    FAutoShowRangeMin := FAutoShowRangeMax;

  NeedUpdate();
end;

procedure TFormMain.TrackBarAutoShowTresholdMinChange(Sender: TObject);
begin
  FAutoShowRangeMin := TrackBarAutoShowTresholdMin.Position;
  if FAutoShowRangeMin > FAutoShowRangeMax then
    FAutoShowRangeMax := FAutoShowRangeMin;

  NeedUpdate();
end;

procedure TFormMain.LoadSample(Index: Integer);
begin
  FTresholdMax := RandomRange(130, 255);
  FTresholdMin := RandomRange(0, 120);
  FSortingDirection := TSortingDirection(Random(4));
  //FRawSource.LoadFromResourceName(HInstance, IntToStr(Index));
  LoadSampleToBitmap(FRawSource, Index);
  UpdateRawSource();
end;

procedure TFormMain.UpdateSource();
var
  Scaled: TBitmap;
  W, H: Integer;
begin
  Scaled := TBitmap.Create();
  try
    W := Max(1, Trunc(FRawSource.Width * FImageScale / 100));
    H := Max(1, Trunc(FRawSource.Height * FImageScale / 100));

    if W > MaxSourceSize then
    begin
      W := MaxSourceSize;
      H := Trunc(W * (FRawSource.Height / FRawSource.Width));
    end;
    if H > MaxSourceSize then
    begin
      H := MaxSourceSize;
      W := Trunc(H * (FRawSource.Width / FRawSource.Height));
    end;

    Scaled.SetSize(W, H);
    Scaled.Canvas.AntialiasingMode := amOn;
    Scaled.Canvas.StretchDraw(Rect(0, 0, Scaled.Width, Scaled.Height), FRawSource);
    FSource.Assign(Scaled);
  finally
    Scaled.Free();
  end;

  NeedUpdate();
end;

procedure TFormMain.UpdateRawSource();
begin
  ScrollBoxDisp.HorzScrollBar.Position := 0;
  ScrollBoxDisp.VertScrollBar.Position := 0;
  FViewScale := 1;
  FImageScale := 100;
  UpdateSource();
end;

procedure TFormMain.UpdateUI();
begin
  // update image scale
  TrackBarImageScale.Position := FImageScale;
  LabelScale.Caption := IntToStr(FImageScale) + '%' + ' [' + IntToStr(FSource.Width) + 'x' + IntToStr(FSource.Height) + ']';

  // update direction buttons
  if FSortingDirection = TSortingDirection.Left then
    BitBtnLeft.Font.Style := [fsBold]
  else
    BitBtnLeft.Font.Style := [];
  if FSortingDirection = TSortingDirection.Up then
    BitBtnUp.Font.Style := [fsBold]
  else
    BitBtnUp.Font.Style := [];
  if FSortingDirection = TSortingDirection.Right then
    BitBtnRight.Font.Style := [fsBold]
  else
    BitBtnRight.Font.Style := [];
  if FSortingDirection = TSortingDirection.Down then
    BitBtnDown.Font.Style := [fsBold]
  else
    BitBtnDown.Font.Style := [];

  // update view scale
  RadioGroupViewScale.ItemIndex := FViewScale - 1;

  // update trashhold
  TrackBarThresholdMin.Position := FTresholdMin;
  TrackBarThresholdMax.Position := FTresholdMax;

  // update save scale
  RadioGroupSaveScale.ItemIndex := FSaveScale - 1;

  // threshold labels
  LabelThresholdMin.Caption := IntToStr(FTresholdMin);
  LabelThresholdMax.Caption := IntToStr(FTresholdMax);

  // auto show
  CheckBoxAutoShowEnabled.Checked := FAutoShowEnabled;
  TrackBarAutoShowTresholdMin.Position := FAutoShowRangeMin;
  TrackBarAutoShowTresholdMax.Position := FAutoShowRangeMax;
  TrackBarThresholdMax.Enabled := not FAutoShowEnabled;
  TrackBarThresholdMin.Enabled := not FAutoShowEnabled;
  RadioGroupSaveScale.Enabled := not FAutoShowEnabled;
  ButtonSave.Enabled := not FAutoShowEnabled;
end;

procedure TFormMain.NeedUpdate();
begin
  FIsNeedUpdate := True;

  TimerAutoShow.Enabled := FAutoShowEnabled;

  UpdateUI();
end;

end.

