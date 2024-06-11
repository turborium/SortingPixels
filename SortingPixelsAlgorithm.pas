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

unit SortingPixelsAlgorithm;

{$MODE DELPHIUNICODE}
{$SCOPEDENUMS ON}
{$DEFINE STABLE}

interface

uses
  Classes, SysUtils, Generics.Defaults, Generics.Collections, BitmapPixels;

type
  TSortingDirection = (Left, Up, Right, Down);

procedure ImageSortingPixels(var Data: TBitmapData; Direction: TSortingDirection; ThresholdMin, ThresholdMax: Integer);

function PixelBrightness(const Pixel: TPixelRec): Byte; inline;

implementation

function PixelBrightness(const Pixel: TPixelRec): Byte;
begin
  Result := (299 * Pixel.R + 587 * Pixel.G + 114 * Pixel.B) div 1000;
end;

function CompareUp({$IFDEF STABLE}constref{$ELSE}const{$ENDIF} Left, Right: TPixelRec): Integer;
begin
  Result := PixelBrightness(Right) - PixelBrightness(Left);
end;

function CompareDown({$IFDEF STABLE}constref{$ELSE}const{$ENDIF} Left, Right: TPixelRec): Integer;
begin
  Result :=  PixelBrightness(Left) - PixelBrightness(Right);
end;

procedure ImageSortingPixels(var Data: TBitmapData; Direction: TSortingDirection; ThresholdMin, ThresholdMax: Integer);
var
  X, Y, Brightness: Integer;
  Span: TArray<TPixelRec>;
  Len, TempX, I, TempY: Integer;
  Comparer: IComparer<TPixelRec>;
begin
  // создаем компаратор для сортировки по убыванию или возрастанию
  if Direction in [TSortingDirection.Left, TSortingDirection.Up] then
  	Comparer := TComparer<TPixelRec>.Construct(CompareUp)
  else
  	Comparer := TComparer<TPixelRec>.Construct(CompareDown);

  if Direction in [TSortingDirection.Left, TSortingDirection.Right] then
  begin
    // проходимся по всем строкам изображения
    for Y := 0 to Data.Height - 1 do
    begin
      if TThread.CheckTerminated() then
        break;

      // обрабатываем строку в цикле
      X := 0;
      while X < Data.Width do
      begin
        // вычисляем длину непрерывного куска строки соответсвующего условиям
        Len := 0;
        TempX := X;
        while TempX < Data.Width do
        begin
          // даже если первый пиксел не соответсвует условиям, мы получим длину куска в единицу
          Len := Len + 1;

          // яркость пиксела
          Brightness := PixelBrightness(Data.Pixels[TempX, Y]);

          // не соответсвует усливию - выходим
          if Brightness < ThresholdMin then
            break;
          if Brightness > ThresholdMax then
            break;

          // следующий пиксель
          TempX := TempX + 1;
        end;

        // сортируем пикселы куска, если это имеет смысл
        if Len > 1 then
        begin
          // заполняем массив из изображения
          SetLength(Span, Len);
          for I := 0 to Len - 1 do
          begin
            Span[I] := Data.Pixels[X + I, Y];
          end;

          // сортируем
          TArrayHelper<TPixelRec>.Sort(Span, Comparer);

          // записываем отсортированные пикселы обратно
          for I := 0 to Len - 1 do
          begin
            Data.Pixels[X + I, Y] := Span[I];
          end;
        end;

        // пропускаем уже отсортированные пикселы
        X := X + Len;
      end;
    end;
  end else
  begin
    // проходимся по всем столбцам изображения
    for X := 0 to Data.Width - 1 do
    begin
      if TThread.CheckTerminated() then
        break;

      // обрабатываем столбец в цикле
      Y := 0;
      while Y < Data.Height do
      begin
        // вычисляем длину непрерывного куска столбца соответсвующего условиям
        Len := 0;
        TempY := Y;
        while TempY < Data.Height do
        begin
          // даже если первый пиксел не соответсвует условиям, мы получим длину куска в единицу
          Len := Len + 1;

          // яркость пиксела
          Brightness := PixelBrightness(Data.Pixels[X, TempY]);

          // не соответсвует усливию - выходим
          if Brightness < ThresholdMin then
            break;
          if Brightness > ThresholdMax then
            break;

          // следующий пиксель
          TempY := TempY + 1;
        end;

        // сортируем пикселы куска, если это имеет смысл
        if Len > 1 then
        begin
          // заполняем массив из изображения
          SetLength(Span, Len);
          for I := 0 to Len - 1 do
          begin
            Span[I] := Data.Pixels[X, Y + I];
          end;

          // сортируем
          TArrayHelper<TPixelRec>.Sort(Span, Comparer);

          // записываем отсортированные пикселы обратно
          for I := 0 to Len - 1 do
          begin
            Data.Pixels[X, Y + I] := Span[I];
          end;
        end;

        // пропускаем уже отсортированные пикселы
        Y := Y + Len;
      end;
    end;
  end;
end;


end.

