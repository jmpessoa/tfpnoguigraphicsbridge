unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, FPNoGUIGraphicsBridge, ViewPort, Forms, Controls,
  Graphics, Dialogs, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    FPNoGUIGraphicsBridge1: TFPNoGUIGraphicsBridge;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ViewPort1: TViewPort;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
    GeometryUtilsBridge, fpcolorbridge;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
   ViewPort1.Height:= PaintBox1.Height;
   ViewPort1.Width:= PaintBox1.Width;

   ViewPort1.SetScaleXY(0 {minx}, 12 {maxx}, 0 {miny}, 12 {maxy});

   FPNoGUIGraphicsBridge1.PathToFontFile:= 'C:\Windows\Fonts\LUCON.TTF';

   //[android]  FPNoGUIGraphicsBridge1.PathToFontFile:= '/system/fonts/Roboto-Regular.ttf';
   //[linux]    FPNoGUIGraphicsBridge1.PathToFontFile:= '/usr/share/fonts/TTF/Arial.ttf';

   FPNoGUIGraphicsBridge1.SetSurfaceSize(PaintBox1.Width,PaintBox1.Height);

   //FPNoGUIGraphicsBridge1.ActiveViewPort:= ViewPort1; //or in design time
   FPNoGUIGraphicsBridge1.PaintViewPort;
   FPNoGUIGraphicsBridge1.PaintGrid(True);

   ViewPort1.PenColor:= colbrRed;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(0,2), ToRealPoint(2,1)]); {left-top, right-bottom}

   ViewPort1.PenColor:= colbrGreen;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(2,4), ToRealPoint(4,3)]); {left-top, right-bottom}

   ViewPort1.PenColor:= colbrBlue;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(4,6), ToRealPoint(6,5)]); {left-top, right-bottom}

   ViewPort1.PenColor:= colbrYellow;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(6,8), ToRealPoint(8,7)]); {left-top, right-bottom}

   ViewPort1.PenColor:= colbrOrange;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(8,10), ToRealPoint(10,9)]); {left-top, right-bottom}

   ViewPort1.PenColor:= colbrLime;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(10,12), ToRealPoint(12,11)]); {left-top, right-bottom}

   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(-0.2,-0.5),'Jan',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(0.8,-0.5),'Fev',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(1.8,-0.5),'Mar',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(2.8,-0.5),'Abr',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(3.8,-0.5),'Mai',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(4.8,-0.5),'Jun',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(5.8,-0.5),'Jul',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(6.8,-0.5),'Ago',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(7.8,-0.5),'Set',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(8.8,-0.5),'Out',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(9.8,-0.5),'Nov',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(10.8,-0.5),'Dez',11);

   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(-0.7,1), 'A1',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(-0.7,3), 'A2',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(-0.7,5), 'A3',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(-0.7,7), 'A4',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(-0.7,9),'A5',11);
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(-0.7,11),'A6',11);

   PaintBox1.Invalidate;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  FPNoGUIGraphicsBridge1.CopyToCanvas(PaintBox1.Canvas);
end;

end.

