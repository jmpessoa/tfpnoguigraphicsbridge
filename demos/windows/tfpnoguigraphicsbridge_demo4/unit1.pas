unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, FPNoGUIGraphicsBridge, ViewPort, Forms, Controls,
  Graphics, Dialogs, ComCtrls, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    FPNoGUIGraphicsBridge1: TFPNoGUIGraphicsBridge;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    StatusBar1: TStatusBar;
    ViewPort1: TViewPort;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FPNoGUIGraphicsBridge1DrawParameterizedFunction(t: real; out
      x: real; out y: real; out skip: boolean);
    procedure PaintBox1Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Velocity, AngleDegree, Gravity, AngleRadian: real;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  GeometryUtilsBridge, fpcolorbridge;

{ TForm1 }

//http://www.sbmac.org.br/eventos/cnmac/xxxiii_cnmac/pdf/763.pdf
//http://www.icmc.usp.br/CMS/Arquivos/arquivos_enviados/SMA_88_Calculo2Curvas.pdf
//http://www.biocristalografia.df.ibilce.unesp.br/walter/fis_geral/proj/projetil2.html
//http://www.sofisica.com.br/conteudos/Mecanica/Cinematica/movobl.php
//https://www.algosobre.com.br/fisica/balistica-e-lancamento-de-projetil.html

//http://www.convertalot.com/ballistic_trajectory_calculator.html
//http://www.ajdesigner.com/phpprojectilemotion/range_equation.php#ajscroll
//http://www.mrmont.com/teachers/physicsteachershelper-proj.html

procedure TForm1.Button1Click(Sender: TObject);
var

   tmin, tmax: real;
   Ymax, Xmax: real;
   t1, x1, y1, t2, x2, y2: real;
   i: integer;
   rx, dy: real;
   auxText: string;
begin
   //km/h --->> m/s [dividir by 3,6]

   AngleDegree:= StrToFloat(Edit1.Text);
   Velocity:=  StrToFloat(Edit2.Text);   {m/s}

   AngleRadian:= (AngleDegree*3.1416)/180;
   Gravity:= StrToFloat(Edit3.Text); {9.81 m/s2}

   tmin:= 0;
   tmax:= 2*Velocity*sin(AngleRadian)/Gravity;
   Xmax:= Velocity*cos(AngleRadian)*tmax;

   Ymax:= Velocity*Velocity*sin(AngleRadian)*sin(AngleRadian)/(2*Gravity);

   ViewPort1.Height:= PaintBox1.Height;
   ViewPort1.Width:= PaintBox1.Width;
   ViewPort1.SetScaleXY(0 {minx}, XMax {maxx}, 0 {miny}, Ymax {maxy});

   FPNoGUIGraphicsBridge1.PaintViewPort;
   FPNoGUIGraphicsBridge1.PaintGrid(True);

   //hanldle by OnDrawParameterizedFunction
   FPNoGUIGraphicsBridge1.DrawParameterizedFunction(True{clear the surface}, tmin, tmax);

   for i:= 0 to 8 do
   begin
     t1:= (i/8)*tmax;
     x1:=  (Velocity*cos(AngleRadian))*t1;
     y1:=  (Velocity*sin(AngleRadian))*t1 - 1/2*t1*t1*Gravity;
     rx:= Xmax/110; //circle radius
     FPNoGUIGraphicsBridge1.DrawFillCircle([ToRealPoint(x1, y1), ToRealPoint(x1 + rx, y1)]);
   end;

   FPNoGUIGraphicsBridge1.SetPenColor(colbrLightSlateBlue); //Light
   for i:= 0 to 7 do
   begin
     t1:= (i/8)*tmax;
     x1:=  (Velocity*cos(AngleRadian))*t1;
     y1:=  (Velocity*sin(AngleRadian))*t1 - 1/2*t1*t1*Gravity;

     t2:= ((i+1)/8)*tmax;
     x2:=  (Velocity*cos(AngleRadian))*t2;
     y2:=  (Velocity*sin(AngleRadian))*t2 - 1/2*t2*t2*Gravity;

     FPNoGUIGraphicsBridge1.DrawLineArrow(x1, y1, (x1+x2)/2 , (y1+y2)/2, 2);
     FPNoGUIGraphicsBridge1.DrawLineArrow(x1, y1, (x1+x2)/2 , y1, 2);
     FPNoGUIGraphicsBridge1.DrawLineArrow(x1, y1, x1 , (y1+y2)/2, 2);
   end;
   auxText:= '                 Time: ' + FloatToStrF(tmax, ffFixed, 0,2) + 's' +
                           '      Height: ' +  FloatToStrF(Ymax, ffFixed, 0,2) + 'm' +
                           '      Distance: '+ FloatToStrF(Xmax, ffFixed, 0,2) +'m';

   dy:= Ymax/FPNoGUIGraphicsBridge1.ActiveViewPort.GridData.YInterval;
   FPNoGUIGraphicsBridge1.TextOut(ToRealPoint(0.1, -2*dy) , auxText);

   PaintBox1.Invalidate;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   FPNoGUIGraphicsBridge1.PathToFontFile:= 'C:\Windows\Fonts\LUCON.TTF';
   FPNoGUIGraphicsBridge1.SetSurfaceSize(PaintBox1.Width,PaintBox1.Height);
end;

procedure TForm1.FPNoGUIGraphicsBridge1DrawParameterizedFunction(t: real; out
  x: real; out y: real; out skip: boolean);
begin                //hint: do "skip:= True;" to not draw some point!
  x:=  (Velocity*cos(AngleRadian))*t;
  y:=  (Velocity*sin(AngleRadian))*t - 1/2*t*t*Gravity;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  FPNoGUIGraphicsBridge1.CopyToCanvas(PaintBox1.Canvas);
end;

end.

