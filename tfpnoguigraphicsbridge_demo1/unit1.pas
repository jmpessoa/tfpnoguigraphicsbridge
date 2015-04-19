unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, FPNoGUIGraphicsBridge, ViewPort, GridData, GeometryUtilsBridge,
  FPDxfWriteBridge, FPColorBridge;

type

  { TForm1 }

  TForm1 = class(TForm)
    FPDxfWriteBridge1: TFPDxfWriteBridge;
    FPNoGUIGraphicsBridge1: TFPNoGUIGraphicsBridge;
    GridData1: TGridData;
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
      if FPNoGUIGraphicsBridge1.DXFWriteBridge.RestrictedLayer <> '' then
      begin
        if Pos(FPNoGUIGraphicsBridge1.DXFWriteBridge.RestrictedLayer,
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
begin

   ViewPort1.Height:= PaintBox1.Height;
   ViewPort1.Width:= PaintBox1.Width;

   ViewPort1.DrawAxis:= True;
   ViewPort1.DrawGrid:= True;
   ViewPort1.SetScaleXY(-1.6 {xmin}, 1.6 {xmax}, -2.0 {ymin}, 6.0 {ymax});

   FPNoGUIGraphicsBridge1.AddFunction(@GenericFunction1,-1.6,1.6);
   FPNoGUIGraphicsBridge1.AddFunction(@GenericFunction2,-1.6,1.6);

   FPNoGUIGraphicsBridge1.SetSize(ViewPort1.Width, ViewPort1.Height);
   FPNoGUIGraphicsBridge1.PathToFontFile:= 'C:\Windows\Fonts\Arial.ttf';
   //hint [android]  FPNoGUIGraphicsBridge1.PathToFontFile:= '/system/fonts/Roboto-Regular.ttf';
   //hint [linux]    FPNoGUIGraphicsBridge1.PathToFontFile:= '/usr/share/fonts/TTF/Arial.ttf';

   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Circle',[ToRealPoint(0,1){left/top},
                                                           ToRealPoint(1,0){right/botom}],'This is a Circle!','foo');

   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Line',[ToRealPoint(0.0,1.5),ToRealPoint(1.0, 3.6)],'','foo');
   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Polyline',[ToRealPoint(0,1.5),ToRealPoint(0.5,1),
                                                    ToRealPoint(1,1.5), ToRealPoint(0.5,2)],'','');
   FPNoGUIGraphicsBridge1.AddEntity('blue_layer','Text',[ToRealPoint(0,0.5)],'Hello World!','');


   (* "datagraph.txt" data format:
   id#circle;layer#green_layer;title#;x#0.50 1.50;y#1.00 0.00
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
   FPNoGUIGraphicsBridge1.DrawFunction(False {not clear screnn}, 0 {index});
   FPNoGUIGraphicsBridge1.DrawFunction(False {not clear screnn}, 1 {index});

   //Prepare DXF                      {1-red} {3-green} {5-blue}
   FPNoGUIGraphicsBridge1.DXFWriteBridge.AddLayer('blue_layer', 'CONTINUOUS', 5 );
   FPNoGUIGraphicsBridge1.DXFWriteBridge.AddLayer('green_layer', 'CONTINUOUS', 3 );

   FPNoGUIGraphicsBridge1.DXFWriteBridge.ProduceEntity('blue_layer'); //restricted layer... event OnProduceEntity
   FPNoGUIGraphicsBridge1.DXFWriteBridge.SaveToFile('blue_layer.dxf');

   FPNoGUIGraphicsBridge1.DXFWriteBridge.ProduceEntity('green_layer'); //restricted layer... event OnProduceEntity
   FPNoGUIGraphicsBridge1.DXFWriteBridge.SaveToFile('green_layer.dxf');

   FPNoGUIGraphicsBridge1.DXFWriteBridge.ProduceEntity(''); //not restricted layer... event OnProduceEntity
   FPNoGUIGraphicsBridge1.DXFWriteBridge.SaveToFile('all_layer.dxf');

end;

end.

