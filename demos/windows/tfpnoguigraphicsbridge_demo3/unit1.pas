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

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
   FPNoGUIGraphicsBridge1.CopyToCanvas(PaintBox1.Canvas);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

   ViewPort1.Height:= PaintBox1.Height;
   ViewPort1.Width:= PaintBox1.Width;

   ViewPort1.SetScaleXY(0 {minx}, 12 {maxx}, -5 {miny}, 15 {maxy});

   FPNoGUIGraphicsBridge1.PathToFontFile:= 'C:\Windows\Fonts\LUCON.TTF';

   //[android]  FPNoGUIGraphicsBridge1.PathToFontFile:= '/system/fonts/Roboto-Regular.ttf';
   //[linux]    FPNoGUIGraphicsBridge1.PathToFontFile:= '/usr/share/fonts/TTF/Arial.ttf';

   FPNoGUIGraphicsBridge1.SetSurfaceSize(PaintBox1.Width,PaintBox1.Height);

   //FPNoGUIGraphicsBridge1.ActiveViewPort:= ViewPort1; //or in design time
   FPNoGUIGraphicsBridge1.PaintViewPort;
   FPNoGUIGraphicsBridge1.PaintGrid(True);

   ViewPort1.PenColor:= colbrRed;
   FPNoGUIGraphicsBridge1.DrawPath([ToRealPoint(4,10),
                                    ToRealPoint(5,11),
                                    ToRealPoint(6,11),
                                    ToRealPoint(7,12),
                                    ToRealPoint(8,12),
                                    ToRealPoint(10,13),
                                    ToRealPoint(11,14),
                                    ToRealPoint(12,15)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(4,10),ToRealPoint(4+0.1,10) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(5,11),ToRealPoint(5+0.1,11) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(6,11),ToRealPoint(6+0.1,11) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(7,12),ToRealPoint(7+0.1,12) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(8,12),ToRealPoint(8+0.1,12) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(10,13),ToRealPoint(10+0.13,13)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(11,14),ToRealPoint(11+0.1,14)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(12,15),ToRealPoint(12+0.1,15)]);

   ViewPort1.PenColor:= colbrBlue;
   FPNoGUIGraphicsBridge1.DrawPath([ToRealPoint(4,-5),
                                    ToRealPoint(5,0),
                                    ToRealPoint(6,7),
                                    ToRealPoint(7,10),
                                    ToRealPoint(8,10),
                                    ToRealPoint(10,11),
                                    ToRealPoint(11,12),
                                    ToRealPoint(12,14)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(4,-5),ToRealPoint(4+0.1,-5) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(5,0),ToRealPoint(5+0.1,0) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(6,7),ToRealPoint(6+0.1,7) ]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(7,10),ToRealPoint(7+0.1,10)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(8,10),ToRealPoint(8+0.1,10)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(10,11),ToRealPoint(10+0.13,11)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(11,12),ToRealPoint(11+0.1,12)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(12,14),ToRealPoint(12+0.1,14)]);

   ViewPort1.PenColor:= colbrIndigo;
   FPNoGUIGraphicsBridge1.DrawPath([ToRealPoint(4,0),
                                    ToRealPoint(5,6),
                                    ToRealPoint(6,9),
                                    ToRealPoint(7,11),
                                    ToRealPoint(8,11),
                                    ToRealPoint(10,12),
                                    ToRealPoint(11,13),
                                    ToRealPoint(12,13)]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(4,0){center},ToRealPoint(4+0.1,0) {rX}]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(5,6){center},ToRealPoint(5+0.1,6) {rX}]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(6,9){center},ToRealPoint(6+0.1,9) {rX}]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(7,11){center},ToRealPoint(7+0.1,11) {rX}]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(8,11){center},ToRealPoint(8+0.1,11) {rX}]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(10,12){center},ToRealPoint(10+0.13,12) {rX}]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(11,13){center},ToRealPoint(11+0.1,13) {rX}]);
   FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(12,13){center},ToRealPoint(12+0.1,13) {rX}]);

   //legends
   ViewPort1.PenColor:= colbrRed;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(12,9),ToRealPoint(12.5,8)]); {left-top, right-bottom}
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(12.8,8), 'Jan', 12, colbrRed);

   ViewPort1.PenColor:= colbrBlue;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(12,7),ToRealPoint(12.5,6)]); {left-top, right-bottom}
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(12.8,6), 'Fev', 12, colbrBlue);

   ViewPort1.PenColor:= colbrIndigo;
   FPNoGUIGraphicsBridge1.DrawFillRectangle([ToRealPoint(12,5),ToRealPoint(12.5,4)]); {left-top, right-bottom}
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(12.8,4), 'Mar', 12, colbrIndigo);

   PaintBox1.Invalidate;
end;

end.

