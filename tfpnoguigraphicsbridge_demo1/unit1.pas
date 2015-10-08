unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, FPNoGUIGraphicsBridge, ViewPort, GeometryUtilsBridge,
  FPDxfWriteBridge, FPColorBridge;

type

  { TForm1 }

  TForm1 = class(TForm)
    FPDxfWriteBridge1: TFPDxfWriteBridge;
    FPNoGUIGraphicsBridge1: TFPNoGUIGraphicsBridge;
    PaintBox1: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    StatusBar1: TStatusBar;
    ViewPort1: TViewPort;
    procedure FormActivate(Sender: TObject);
    procedure FPDxfWriteBridge1ProduceEntity(out entityDXF: string);
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

function GenericFunction1(x: real): real;
begin
   Result:= x*x*x;
end;

function GenericFunction2(x: real): real;
begin
    Result:= 4*x*x*x*x - 5*x*x*x - x*x + x -1;
end;

{ TForm1 }

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  FPNoGUIGraphicsBridge1.Surface.CopyToCanvas(0,0,PaintBox1.Canvas);
end;

procedure TForm1.FPDxfWriteBridge1ProduceEntity(out entityDXF: string);
var
   i: integer;
   strList: TStringList;
begin
   strList:= TStringList.Create;
   entityDXF:='';
   for i:= 0 to FPNoGUIGraphicsBridge1.EntityList.Count - 1 do
   begin
      if FPDxfWriteBridge1.RestrictedLayer <> '' then
      begin
        if Pos(FPDxfWriteBridge1.RestrictedLayer,
               FPNoGUIGraphicsBridge1.GetEntity(i).EntityData.Layer) > 0 then
        begin
           strList.Add(FPNoGUIGraphicsBridge1.GetEntity(i).DataToDXF);
        end;
      end
      else
      begin
        strList.Add(FPNoGUIGraphicsBridge1.GetEntity(i).DataToDXF);
      end;
   end;
   entityDXF:= Trim(strList.Text);
   strList.Free;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
   W, H: integer;
begin
   W:= PaintBox1.Width;
   H:= PaintBox1.Height;

   FPNoGUIGraphicsBridge1.SetSurfaceSize(W, H);
   FPNoGUIGraphicsBridge1.PathToFontFile:= 'C:\Windows\Fonts\LUCON.TTF';
   //hint [android]  FPNoGUIGraphicsBridge1.PathToFontFile:= '/system/fonts/Roboto-Regular.ttf';
   //hint [linux]    FPNoGUIGraphicsBridge1.PathToFontFile:= '/usr/share/fonts/TTF/Arial.ttf';

   ViewPort1.SetSize(W, H);
   ViewPort1.DrawAxis:= True;
   ViewPort1.DrawGrid:= True;
   ViewPort1.SetScaleXY(-1.6 {xmin}, 1.6 {xmax}, -2.0 {ymin}, 6.0 {ymax});

   FPNoGUIGraphicsBridge1.ActiveViewPort:= ViewPort1;

   FPNoGUIGraphicsBridge1.AddFunction(@GenericFunction1,-1.6,1.6);
   FPNoGUIGraphicsBridge1.AddFunction(@GenericFunction2,-1.6,1.6);

   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Circle',[Point(0,1.0){center},
                                                           Point(0+0.5,1.0){radiox}],'This is a Circle!','foo');

   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Line',[Point(0.0,1.5),Point(1.0, 3.6)],'','foo');
   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Polyline',[Point(0,1.5),Point(0.5,1),
                                                    Point(1,1.5), Point(0.5,2)],'','');
   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Text',[Point(0,0.5)],'Hello World!','');


   (* "datagraph.txt" data format:
   id#circle;layer#green_layer;title#;x#0.00;y#0.50;r#0.50
   id#line;layer#green_layer;title#;x#0.30 1.00;y#3.60 2.00
   id#polygon;layer#green_layer;title#;x#0.50 1.00 1.50 1.00;y#1.50 1.00 1.50 2.00
   id#text;layer#green_layer;title#This is Pascal!;x#1.00;y#1.00
   id#point;layer#green_layer;title#;x#-0.30;y#2.00
   *)

   FPNoGUIGraphicsBridge1.LoadEntitiesFromFile('datagraph.txt');  //layer_green

   FPNoGUIGraphicsBridge1.PaintViewPort;
   FPNoGUIGraphicsBridge1.PaintGrid(True);

   ViewPort1.PenColor:= colbrBlue;
   FPNoGUIGraphicsBridge1.DrawEntities('blue_layer');

   ViewPort1.PenColor:= colbrGreen;
   FPNoGUIGraphicsBridge1.DrawEntities('green_layer');   //from "datagraph.txt"

   ViewPort1.PenColor:= colbrRed;
   FPNoGUIGraphicsBridge1.DrawFunction(False {not clear screen}, 0 {index});
   FPNoGUIGraphicsBridge1.DrawFunction(False {not clear screen}, 1 {index});

   //Prepare DXF                      {1-red} {3-green} {5-blue}
   FPDxfWriteBridge1.AddLayer('blue_layer', 'CONTINUOUS', 5 );
   FPDxfWriteBridge1.AddLayer('green_layer', 'CONTINUOUS', 3 );

   FPDxfWriteBridge1.Produce('blue_layer'); //restricted layer... event OnProduceEntity
   FPDxfWriteBridge1.SaveToFile('blue_layer.dxf');

   FPDxfWriteBridge1.Produce('green_layer'); //restricted layer... event OnProduceEntity
   FPDxfWriteBridge1.SaveToFile('green_layer.dxf');

   FPDxfWriteBridge1.Produce(''); //not restricted layer... event OnProduceEntity
   FPDxfWriteBridge1.SaveToFile('all_layer.dxf');

end;

end.

