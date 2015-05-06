unit FPNoGUIGraphicsBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  FPCanvas, FPImgCanv, FPWritePNG, FPReadPNG, FPImage, FTFont, freetype,
  types, GeometryUtilsBridge, FPColorBridge,  FPDxfWriteBridge, ViewPort;

(*
From: http://wiki.freepascal.org/Developing_with_Graphics

"You can draw images which won't be displayed in the screen without the LCL, by just using fcl-image directly.
For example a program running on a webserver without X11 could benefit from not having a visual library
as a dependency.
FPImage (alias fcl-image) is a very generic image and drawing library written completely in pascal.
In fact the LCL uses FPImage too for all the loading and saving from/to files and implements the
drawing function through calls to the widgetset (winapi, gtk, carbon, ...).
Fcl-image on the other hand also has drawing routines.
For more information, please read the article about fcl-image" (http://wiki.freepascal.org/fcl-image).

*)

const
  MAXPOINTS = 599; // indeed 600!  0--99

type

jbyte=shortint;       // signed 8 bits
Pjbyte=^jbyte;

TColorRGB=packed record {copy from  ...\fcl-image\src\BMPcomn.pp}
   B,G,R:Byte;
end;

TColorRGBA=packed record  {copy from  ...\fcl-image\src\BMPcomn.pp}
case Boolean of
  False:(B,G,R,A:Byte);
  True:(RGB:TColorRGB);
end;

TEntityState = (etNone, etText, etPoint, etLine, etParallelLine,etParallelLineHor,etParallelLineVer,
                etHorizontalLine, etVerticalLine, etAngularLine, etTrimLine, etExtendLine, etTranslateLine,
                etCircle, etRectangle, etPolyline, etArc, etArrow, etLinearAlignedDim, etLinearVerticalDim,
                etLinearHorizontalDim,etRadialDim, etDiameterDim, etArcDim, etAngleDim,etSelect, etErase,
                etZoomCapture, etPan, etConnectPoly);

TLineToggleSide = (tsSide1, tsSide2);

TRealPoints = array[0..MAXPOINTS] of TRealPoint;
TFX = function(X: real): real;
TDesignFX = procedure(x: real; out y:real; out skip: boolean) of object;

 TFCLImageBridge = class(TObject)
  private
    FImgMem: TFPCustomImage;
    FPngImgReader: TFPCustomImageReader;
    FPngImgWriter: TFPCustomImageWriter;
    FFreeTypeFont: TFreeTypeFont;
    function FPColorToRGBA(Const Color : TFPColor) : TColorRGBA;
  public
    Width: integer;
    Height: integer;
    PRGBAImg: PByte; //array of byte... That's where the RGBA array goes.
    Canvas: TFPCustomCanvas;

    constructor Create(W,H: integer);
    destructor Destroy; override;

    procedure SaveToFilePNG(path: string);
    procedure LoadFromFilePNG(path: string);
    procedure CopyToCanvas(ShiftX,ShiftY: integer; ACanvas: TFPCustomCanvas);
    procedure CopyFromCanvas(ShiftX,ShiftY: integer; ACanvas: TFPCustomCanvas);
    procedure CopyFromCanvas(ACanvas: TFPCustomCanvas);

    function GetRGBAGraphics(const buffer: PByte): boolean;  overload;
    function GetRGBAGraphics(const buffer: PJByte): boolean; overload; //android

    procedure SetSize(W,H: integer);
    procedure SetSize(backgroundPNGFile: string);
    procedure SetFont(pathToFile: string);

    property  FreeTypeFont: TFreeTypeFont read FFreeTypeFont write FFreeTypeFont;

end;

TFunction = class
  private
  public
   FX: TFX;
   MinX, MaxX: real;
   CountPoints: integer;
   Points: array of TRealPoint;

   function GetMinY:real;
   function GetMaxY:real;
   function GetMinX:real;
   function GetMaxX:real;

   procedure Draw(VP: TViewPort;  Canvas: TFPCustomCanvas);
   procedure MakePoints(nPoints: integer; xmin, xmax: real);

   procedure CrossHatch(VP: TViewPort;  Canvas: TFPCustomCanvas; a,b: real);
   procedure CrossHatch(VP: TViewPort;  Canvas: TFPCustomCanvas; a,b: real; objF: TFunction );

   function IntegralSimpson(a, b: real; n: integer): real;
   constructor Create(F: TFX; xmix, xmax: real);
   constructor Create(Pts: Array of TRealPoint);
   destructor Destroy; override;
end;

TEntityData = class(TPersistent)
  private
    FDimExtensionLineGAP: integer;    //TOD0: event drive here!
    FDimArrowSize: integer;
    FDimArrowWidth: integer;
    FDimLineOffset: integer;
    FDimTextHeight: integer;
    FLineToggleSide: TLineToggleSide;
    FTitle: string;
    FLayer: string;
    FPolylineClosed: boolean;
    FAngleOfText: real; //degrees
    FOffset: real;
    FTagX: integer;
    FTagY: integer;
    FTagLabel: string;
    FFieldSeparator: char;
    FNameValueSeparator: char;

  public
    Vertice: TVertice;
    constructor Create;
    Destructor Destroy; override;
  published
    property DimExtensionLineGAP: integer read FDimExtensionLineGAP write FDimExtensionLineGAP;
    property DimArrowSize: integer read FDimArrowSize write FDimArrowSize;
    property DimArrowWidth: integer read FDimArrowWidth write FDimArrowWidth;
    property DimLineOffset: integer read FDimLineOffset write FDimLineOffset;
    property DimTextHeight: integer read FDimTextHeight write  FDimTextHeight;
    property LineToggleSide: TLineToggleSide read FLineToggleSide write FLineToggleSide;
    property Title: string read FTitle  write FTitle;
    property Layer: string read FLayer write FLayer;
    property PolylineClosed: boolean read FPolylineClosed write FPolylineClosed;
    property AngleOfText: real read FAngleOfText write FAngleOfText;
    property Offset: real read FOffset write FOffset;
    property TagX: integer read FTagX write FTagX;
    property TagY: integer read FTagY write FTagY;
    property TagLabel: string read FTagLabel write FTagLabel;
    property FieldSeparator: char read FFieldSeparator write FFieldSeparator;
    property NameValueSeparator: char read FNameValueSeparator write FNameValueSeparator;
end;

TEntity = class
  private
    FEntityData: TEntityData;
    FEntityState: TEntityState;
    procedure ParallelLineByXYPoint(px, py: real);
    procedure ParallelRectangleByXYPoint(px, py: real);
    procedure ParallelPolygonByXYPoint(px, py: real);
    procedure ParallelEllipseByXYPoint(px, py: real);
    procedure ParallelTextByXYPoint(px, py: real);
    procedure ParallelArcByXYPoint(px, py: real);

    procedure ParallelLinearAlignedDimByXYPoint(px, py: real);

    procedure MiddlePointText(out x: real; out y: real);
    procedure MiddlePointLine(out x: real; out y: real);
    procedure MiddlePointCircle(out x: real; out y: real);
    procedure MiddlePointArc(out x: real; out y: real);
    procedure MiddlePointRectangle(out x: real; out y: real);

    procedure MiddlePointPolygon(out x: real; out y: real);

    procedure MiddlePointLinearAlignedDim(out x: real; out y: real);

    function AreaPolygon: real;
    function AreaCircle: real;

    function GetCX: real;
    function GetCY: real;
    function GetRadius: real;

    function GetX1: real;
    function GetY1: real;
    function GetX2: real;
    function GetY2: real;
    function GetX3: real;
    function GetY3: real;
  public
    constructor Create(layerName: string; entityType: TEntityState; V: array of TRealPoint;  sTitle, sTag: string);
    Destructor Destroy; override;

    procedure ReSizeLine(dxy: real; refPoint: integer);
    procedure TranslateLine(dxy: real);
    function DataToString(fieldSeparator: char; nameValueSeparator: char): string;
    function DataToDXF: string;
    function DataToSVG: string;
    function DataToXML: string;
    function DataToNameSpace(fieldSeparator: char; nameValueSeparator: char): string;
    procedure MiddlePoint(out x: real; out y: real);  //generic middlepoint
    procedure TranslateByXY(VP:TViewPort; X, Y:integer); //generic translate
    function Area: real;                                 //generic area
    procedure Draw(VP: TViewPort; Canvas: TFPCustomCanvas); //generic draw
    procedure DrawPoint(Canvas: TFPCustomCanvas; x,y: integer; sTitle: string);
    procedure DrawPoint(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawText(Canvas: TFPCustomCanvas; x,y: integer; sTitle: string);
    procedure DrawText(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawLine(Canvas: TFPCustomCanvas; px1, py1, px2, py2: integer; sTitle: string);
    procedure DrawLine(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawPolyline(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawRectangle(Canvas: TFPCustomCanvas; px1, py1, px2, py2: integer; sTitle: string);
    procedure DrawRectangle(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawCircle(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawArc(Canvas: TFPCustomCanvas; px1, py1, px2, py2, px3, py3:integer;  sTitle: string);
    procedure DrawArc(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawArcDimension(VP: TViewPort; Canvas: TFPCustomCanvas; dx1, dy1, dx2, dy2, dx3, dy3: integer; sTitle: string);
    procedure DrawArcDimension(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawParallelLine(VP: TViewPort; Canvas: TFPCustomCanvas; dx1, dy1, dx2, dy2: real; sTitle: string);
    procedure DrawParallelLine(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawLinearDimension(VP: TViewPort; Canvas: TFPCustomCanvas; dx1, dy1, dx2, dy2: integer; sTitle: string);
    procedure DrawLinearDimension(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawRadialDimension(VP: TViewPort; Canvas: TFPCustomCanvas; cx, cy, r: integer; sTitle: string);
    procedure DrawRadialDimension(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure DrawArrow(Canvas: TFPCustomCanvas; V: array of TPoint; sTitle: string);
    procedure DrawArrow(VP: TViewPort; Canvas: TFPCustomCanvas);

    procedure ResetPoints(V: array of TRealPoint);

    property EntityState: TEntityState read FEntityState write FEntityState;
    property CX :real read GetCX;
    property CY: real read GetCY;
    property R:  real read GetRadius;
    property X1: real read GetX1;
    property Y1: real read GetY1;
    property X2: real read GetX2;
    property Y2: real read GetY2;
    property X3: real read GetX3;
    property Y3: real read GetY3;
    property EntityData: TEntityData  read FEntityData write FEntityData;
 end;

 TFPNoGUIGraphicsBridge = class(TComponent)
    private
       FWidth: integer;
       FHeight: integer;
       FPathToFontFile: string;
       FEntityData: TEntityData;
       FViewPort: TViewPort;
       FOnDesignFunction: TDesignFX;
       FDXFWriteBridge: TFPDxfWriteBridge;
       FClrscr: boolean;

       procedure SetDXFWriteBridge(AValue: TFPDxfWriteBridge);
       procedure SetViewPort(AValue: TViewPort);
       procedure DoDesignFunction(x: real; out y:real; out skip: boolean);
       procedure SetWidth(AValue: integer);
       procedure SetHeight(AValue: integer);
       procedure SetPathToFontFile(pathToFile: string);
       procedure SetSize(W,H: integer);
       procedure SetSize(backgroundPNGFile: string);
    public
       Surface: TFCLImageBridge;
       EntityList: TList;
       FunctionList: TList;

       constructor Create(AOwner: TComponent); override;
       destructor Destroy; override;

       procedure SetSurfaceSize(W,H: integer);
       procedure SetSurfaceSize(backgroundPNGFile: string);

       procedure PaintGrid(VP: TViewPort; clearscr: boolean);
       procedure PaintGrid(clearscr: boolean);

       procedure PaintViewPort(VP: TViewPort);
       procedure PaintViewPort;

       procedure DrawFunction(reset: boolean; SelectedIndex: integer);
       procedure DrawFunction(reset: boolean; VP:TViewPort; SelectedIndex: integer);
       procedure DrawFunction(reset: boolean; xmin, xmax: real);
       procedure DrawFunction(reset: boolean);
       procedure CopyToCanvas(Canvas: TFPCustomCanvas);
       function DrawEntity(VP: TViewPort; SelectedIndex: integer): TEntity;
       function DrawEntity(SelectedIndex: integer): TEntity;
       function DrawEntity(layerName, entityName: string; V: array of TRealPoint; sTitle, sTag: string): integer;
       function DrawEntity(entity: TEntityState; V: array of TRealPoint; sTitle, sTag: string): integer;
       procedure DrawEntity(index: integer; bridgeColor: TTFPColorBridge; thickness: integer);
       procedure DrawEntities;
       procedure DrawEntities(layerName: string);

       procedure LoadEntitiesFromFile(fileName: string);
       procedure SaveEntitiesToFile(fileName: string);

       function AddEntity(layerName:string; Entity: TEntityState; V: array of TRealPoint; sTitle, sTag: string): integer;
       function AddEntity(layerName , entityName: string; V: array of TRealPoint; sTitle, sTag: string): integer;
       function AddEntity(stringData: string;  sTag: string): integer;
       function AddEntity(AEntity: TEntity): integer;
       function AddFunction(F: TFX; xmin, xmax: real): integer;
       function AddFunction(Pts: array of TRealPoint): integer;
       procedure ClearEntityList;
       procedure ClearEntityListByLayer(layer: string);
       procedure DeleteEntity(index: integer);

       procedure ClearFunctionList;
       procedure ClearSurface;
       procedure ClearSurfaceByViewPort(VP: TViewPort);
       function GetFunction(index: integer): TFunction;
       function GetEntity(index: integer): TEntity;
       function GetEntity(VP:TViewPort; X,Y: integer; out index: integer): TEntity;

       function GetEntity(VP:TViewPort; X,Y: integer; out index: integer; layerName: string): TEntity;

       function TranslateEntityXY(VP:TViewPort; index: integer; X,Y: integer): TEntity;
       function TranslateEntityXY(index: integer; X,Y: integer): TEntity;
       procedure PanX(VP:TViewPort; dx: real);
       procedure PanXY(VP:TViewPort; X0, Y0, X, Y: integer);
       procedure PanY(VP:TViewPort; dy: real);

       procedure SetBackGroundColor(colorBridge: TTFPColorBridge);
       procedure SetPenThickness(penThickness: integer);
       procedure SetPenColor(colorBridge: TTFPColorBridge);

       procedure SetFontColor(colorBridge: TTFPColorBridge);
       procedure SetFontHeight(fontHeight: integer);
       procedure SetFontAntiAliased(antiAliased: boolean);
       procedure SetFontResolution(dpiResolution: integer);

       property PathToFontFile: string read FPathToFontFile write SetPathToFontFile;
       property Clearscreen: boolean read FClrscr write FClrscr;
    protected
       procedure Loaded; override;
       procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    published
       property Width: integer read FWidth write SetWidth;
       property Height: integer read  FHeight write setHeight;

       property DXFWriteBridge: TFPDxfWriteBridge read FDXFWriteBridge write SetDXFWriteBridge;
       property EntityData: TEntityData read FEntityData write FEntityData;
       property OnDesignFunction: TDesignFX read FOnDesignFunction write FOnDesignFunction;
       property ActiveViewPort: TViewPort read FViewPort write SetViewPort;
 end;

function ReplaceChar(query: string; oldchar, newchar: char):string;
function SplitStr(var theString : string; delimiter : string) : string;

implementation


{TFEntityData}
constructor TEntityData.Create;
begin
    FDimExtensionLineGAP:= 4;
    FDimArrowSize:= 12;
    FDimArrowWidth:= 4;
    FDimLineOffset:= 20;
    FDimTextHeight:= 9;
    LineToggleSide:= tsSide1;
    DimExtensionLineGAP:= 4;
    DimArrowSize:= 12;
    DimArrowWidth:= 4;
    DimLineOffset:= 20;
    DimTextHeight:= 9;
    FLineToggleSide:= tsSide1;
    FTitle:= 'Title1';
    FTagLabel:= 'Label1';
    FPolylineClosed:= True; {Polyline Closed} //TODO
    FAngleOfText:= 0;  //TODO
    FOffset:= 0;
    FLayer:='0';   //default dxf layer
    FFieldSeparator:= ';';
    FNameValueSeparator:= '#';
end;

Destructor TEntityData.Destroy;
begin
   //
   SetLength(Vertice, 0);
   Vertice:= nil;
   inherited Destroy;
end;

  {TFunction}
procedure TFunction.CrossHatch(VP: TViewPort;  Canvas: TFPCustomCanvas; a,b: real);
var
   i,dx: real;
   X1,Y1,X2,Y2: integer;
begin
   i:=a;
   dx:= (b-a)/30;
   while i <= b do
   begin
      VP.WorldToSurfaceXY(i,FX(i),X1,Y1);
      VP.WorldToSurfaceXY(i,0,X2,Y2);
      Canvas.Line(X1,Y1,X2,Y2);
      i:= i + dx;
   end;
end;

procedure TFunction.CrossHatch(VP: TViewPort;  Canvas: TFPCustomCanvas; a,b: real; objF: TFunction );
var
   i,dx: real;
   X1,Y1,X2,Y2: integer;
begin
   i:=a;
   dx:= (b-a)/30;
   while i <= b do
   begin
      VP.WorldToSurfaceXY(i,FX(i),X1,Y1);
      VP.WorldToSurfaceXY(i,objF.FX(i),X2,Y2);
      Canvas.Line(X1,Y1,X2,Y2);
      i:= i + dx;
   end;
end;

procedure TFunction.Draw(VP: TViewPort;  Canvas: TFPCustomCanvas);
var
  i,X,Y: integer;
  inside: boolean;
begin
   VP.WorldToSurfaceXY(Points[0].x, FX(Points[0].x),X,Y);
   Canvas.MoveTo(X,Y);
   for i:= 1 to CountPoints-1 do
   begin
      inside:= VP.WorldToSurfaceXY(Points[i].x,Points[i].y ,X,Y);
      if not VP.Cliping then
      begin
         Canvas.LineTo(X,Y)
      end
      else //clip
      begin
         if not inside then Canvas.MoveTo(X,Y) else  Canvas.LineTo(X,Y);
      end;
   end;
end;

function TFunction.GetMaxY: real;
var
  temp:real;
  i:integer;
begin
  temp:= Points[0].y;
  for i:=1 to CountPoints-1 do if(temp < Points[i].y) then temp:= Points[i].y;
  Result:= temp;
end;

function TFunction.GetMinY: real;
var
  temp:real;
  i: integer;
begin
  temp:= Points[0].y;
  for i:=1 to CountPoints-1 do if(temp > Points[i].y) then temp:= Points[i].y;
  Result:=temp;
end;

function TFunction.GetMaxX: real;
var
  temp:real;
  i:integer;
begin
  temp:= Points[0].x;
  for i:=1 to CountPoints-1 do if(temp < Points[i].x) then temp:= Points[i].x;
  Result:= temp;
end;

function TFunction.GetMinX: real;
var
  temp:real;
  i: integer;
begin
  temp:= Points[0].x;
  for i:=1 to CountPoints-1 do if(temp > Points[i].x) then temp:= Points[i].x;
  Result:=temp;
end;

function TFunction.IntegralSimpson(a, b: real; n: integer): real;
  var     {Role 1/3 Simpson}
      I1,I2,I3, h: real;
      i,j,m: integer;
      X: array of real;
begin
      m:= n;
      if Odd(m)then inc(m);   {m even!}
      if n < 10 then  m:= 10;
      SetLength(X, m);
      h := (b - a)/(m);  {m even!}
      for i:=0 to m do
      begin
          X[i]:= a+i*h;
      end;
      I1:= FX(X[0])+ FX(X[m]);
      I2:= 0;
      I3:= 0;
      for j:=1 to m-1 do
      begin
          if Odd(j) then // odd
            I3:= I3 + FX(X[j])
          else //even
            I2:= I2 + FX(X[j]);
      end;
      SetLength(X, 0);
      Result:=(h/3)*(I1+2*I2+4*I3);
end;

procedure TFunction.MakePoints(nPoints: integer; xmin, xmax: real);
var
  i: integer;
begin
   MaxX:= xmax;
   MinX:= xmin;
   CountPoints:= nPoints;
   SetLength(Points, CountPoints);
   for i:=1 to CountPoints do
   begin
         Points[i-1].x:= xmin + (i-1)*(xmax-xmin)/(CountPoints);
         Points[i-1].y:= FX(Points[i-1].x);
   end;
end;

constructor TFunction.Create(F: TFX; xmix, xmax: real);
begin
   CountPoints:= MAXPOINTS;
   FX:= F;
   MakePoints(CountPoints, xmix, xmax);
end;

constructor TFunction.Create(Pts: Array of TRealPoint);
var
  i: integer;
begin
   CountPoints:= High(Pts) + 1;
   SetLength(Points, CountPoints);
   FX:= nil;
   for i:=0 to CountPoints-1 do
   begin
      Points[i].x:= Pts[0].x;
      Points[i].y:= Pts[0].y;
   end;
end;

destructor TFunction.Destroy;
begin
    SetLength(Points, 0);
    Points:= nil;
    inherited Destroy;
end;

  {TEntity}

function TEntity.GetRadius: real;
begin
   Result:= Abs(FEntityData.Vertice[1].x - FEntityData.Vertice[0].x);
end;

function TEntity.GetCX: real;
begin
   Result:= FEntityData.Vertice[0].x;
end;

function TEntity.GetCY: real;
begin
   Result:= FEntityData.Vertice[0].y;
end;

function TEntity.GetX1: real;
begin
   Result:= FEntityData.Vertice[0].x;
end;

function TEntity.GetY1: real;
begin
   Result:= FEntityData.Vertice[0].y;
end;

function TEntity.GetX2: real;
begin
   Result:= FEntityData.Vertice[1].x;
end;

function TEntity.GetY2: real;
begin
   Result:= FEntityData.Vertice[1].y;
end;

function TEntity.GetX3: real;
begin
   Result:= FEntityData.Vertice[2].x;
end;

function TEntity.GetY3: real;
begin
   Result:= FEntityData.Vertice[2].y;
end;

procedure TEntity.ParallelLineByXYPoint(px, py: real);
var
  pmx, pmy: real;
  dx,dy: real;
begin
  MiddlePointLine(pmx,pmy);
  dx:= px - pmx;
  dy:= py - pmy;

  FEntityData.Vertice[0].x:=FEntityData.Vertice[0].x+dx;
  FEntityData.Vertice[0].y:=FEntityData.Vertice[0].y+dy;

  FEntityData.Vertice[1].x:=FEntityData.Vertice[1].x+dx;
  FEntityData.Vertice[1].y:=FEntityData.Vertice[1].y+dy;
end;

procedure TEntity.ParallelRectangleByXYPoint(px, py: real);
var
  pmx, pmy: real;
  dx,dy: real;
begin
  MiddlePointLine(pmx,pmy);
  dx:= px - pmx;
  dy:= py - pmy;

  FEntityData.Vertice[0].x:=FEntityData.Vertice[0].x+dx;
  FEntityData.Vertice[0].y:=FEntityData.Vertice[0].y+dy;

  FEntityData.Vertice[1].x:=FEntityData.Vertice[1].x+dx;
  FEntityData.Vertice[1].y:=FEntityData.Vertice[1].y+dy;
end;

procedure TEntity.ParallelLinearAlignedDimByXYPoint(px, py: real);
var
  pmx, pmy: real;
  dx,dy: real;
begin
  MiddlePointLine(pmx,pmy);
  dx:= px - pmx;
  dy:= py - pmy;

  FEntityData.Vertice[0].x:=FEntityData.Vertice[0].x+dx;
  FEntityData.Vertice[0].y:=FEntityData.Vertice[0].y+dy;

  FEntityData.Vertice[1].x:=FEntityData.Vertice[1].x+dx;
  FEntityData.Vertice[1].y:=FEntityData.Vertice[1].y+dy;
end;


procedure  TEntity.ParallelPolygonByXYPoint(px, py: real{; V: array of TRealPoint});
var
  pmx, pmy: real;
  dx,dy: real;
  count, i: integer;
begin
    MiddlePointPolygon(pmx,pmy);
    dx:= px - pmx;
    dy:= py - pmy;
    count := High(FEntityData.Vertice) + 1;
    for i := 0 to count - 1 do
    begin
      FEntityData.Vertice[i].x:= FEntityData.Vertice[i].x + dx;
      FEntityData.Vertice[i].y:= FEntityData.Vertice[i].y + dy;
    end;
end;

procedure  TEntity.ParallelEllipseByXYPoint(px, py: real);
var
  pmx, pmy: real;
  dx,dy: real;
begin
  MiddlePointCircle(pmx,pmy);
  dx:= px - pmx;
  dy:= py - pmy;
  FEntityData.Vertice[0].x:=FEntityData.Vertice[0].x+dx;
  FEntityData.Vertice[0].y:=FEntityData.Vertice[0].y+dy;

  FEntityData.Vertice[1].x:=FEntityData.Vertice[1].x+dx;
  FEntityData.Vertice[1].y:=FEntityData.Vertice[1].y+dy;
end;


procedure  TEntity.ParallelArcByXYPoint(px, py: real);
var
  pmx, pmy: real;
  dx,dy: real;
begin
  MiddlePointArc(pmx,pmy);
  dx:= px - pmx;
  dy:= py - pmy;
  FEntityData.Vertice[0].x:=FEntityData.Vertice[0].x+dx;
  FEntityData.Vertice[0].y:=FEntityData.Vertice[0].y+dy;

  FEntityData.Vertice[1].x:=FEntityData.Vertice[1].x+dx;
  FEntityData.Vertice[1].y:=FEntityData.Vertice[1].y+dy;
end;


procedure TEntity.ParallelTextByXYPoint(px, py: real);
var
  pmx, pmy: real;
  dx,dy: real;
begin
  MiddlePointText(pmx,pmy);
  dx:= px - pmx;
  dy:= py - pmy;
  FEntityData.Vertice[0].x:= FEntityData.Vertice[0].x+dx;
  FEntityData.Vertice[0].y:= FEntityData.Vertice[0].y+dy;
end;

procedure TEntity.MiddlePointText(out x: real; out y: real);
begin
   x:= FEntityData.Vertice[0].x;
   y:= FEntityData.Vertice[0].y;
end;

procedure TEntity.MiddlePointLine(out x: real; out y: real);
begin
    x:= (FEntityData.Vertice[0].x + FEntityData.Vertice[1].x)/2;
    y:= (FEntityData.Vertice[0].y + FEntityData.Vertice[1].y)/2;
end;

procedure TEntity.MiddlePointCircle(out x: real; out y: real);
begin
  x:= (FEntityData.Vertice[0].x + FEntityData.Vertice[1].x)/2;
  y:= (FEntityData.Vertice[0].y + FEntityData.Vertice[1].y)/2;
end;

procedure TEntity.MiddlePointArc(out x: real; out y: real);
begin
  x:= (FEntityData.Vertice[1].x + FEntityData.Vertice[2].x)/2;
  y:= (FEntityData.Vertice[1].y + FEntityData.Vertice[2].y)/2;
end;

procedure TEntity.MiddlePointRectangle(out x: real; out y: real);
begin
    x:= (FEntityData.Vertice[0].x + FEntityData.Vertice[1].x)/2;
    y:= (FEntityData.Vertice[0].y + FEntityData.Vertice[1].y)/2;
end;

procedure TEntity.MiddlePointLinearAlignedDim(out x: real; out y: real);
begin
    x:= (FEntityData.Vertice[0].x + FEntityData.Vertice[1].x)/2;
    y:= (FEntityData.Vertice[0].y + FEntityData.Vertice[1].y)/2;
end;

//http://dan-scientia.blogspot.com.br/2009/10/centroide-de-um-poligono.html
procedure TEntity.MiddlePointPolygon(out x: real; out y: real);
var
   i, countV, countVX: integer;
   S, Scx, Scy, A, pcx, pcy: real;
   VX: array of TRealPoint;
begin
     countV := High(FEntityData.Vertice) + 1;

     countVX:= countV +1;    //extend V
     SetLength(VX, countVX);

     for i:=0 to countV-1 do
     begin
        VX[i].x:=FEntityData.Vertice[i].x;
        VX[i].y:=FEntityData.Vertice[i].y;
     end;
     VX[countV].x:=FEntityData.Vertice[0].x; //close polygon VX
     VX[countV].y:=FEntityData.Vertice[0].y;

     S:=0;
     for i:=0 to countVX-1 do    //0,1,2
     begin
        S:=  S + ( VX[i].x*VX[i+1].y - VX[i+1].x*VX[i].y );
     end;
     A:= S/2;
     Scx:=0;
     for i:=0 to countVX-1 do
     begin
        Scx:=  Scx +(VX[i].x+VX[i+1].x)*
                    (VX[i].x*VX[i+1].y -
                     VX[i+1].x*VX[i].y)
     end;
     pcx:= (1/(6*A))*Scx;

     Scy:=0;
     for i:=0 to countVX-1 do
     begin
        Scy:=  Scy +(VX[i].y+VX[i+1].y)*
                    (VX[i].x*VX[i+1].y -
                     VX[i+1].x*VX[i].y)
     end;
     pcy:= (1/(6*A))*Scy;

     x:=pcx;
     y:=pcy;
end;

{
    VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, X1,Y1);  //cx,cy, r= X2-X1
    VP.WorldToSurfaceXY(, FEntityData.Vertice[1].y, X2,Y2);
    if VP.Clip then inside:= VP.LineClipSurface(X1, Y1, X2, Y2);//TODO: more linclip on others directions
    if inside then DrawCircle(Canvas, X1,Y1,Trunc(Abs(X2-X1)),Title);
}

function TEntity.AreaCircle: real;
var
   rad: real;
begin
    rad:= Abs((FEntityData.Vertice[1].x - FEntityData.Vertice[0].x)/2);
    Result:= 3.1416*rad*rad;
end;

function TEntity.AreaPolygon: real;
var
   i, countV, countVX: integer;
   S, A: real;
   VX: array of TRealPoint;
begin
   countV := High(FEntityData.Vertice) + 1;
   countVX:= countV +1;    //extend V
   SetLength(VX, countVX);

   for i:=0 to countV-1 do
   begin
      VX[i].x:=FEntityData.Vertice[i].x;
      VX[i].y:=FEntityData.Vertice[i].y;
   end;
   VX[countV].x:=FEntityData.Vertice[0].x; //close polygon VX
   VX[countV].y:=FEntityData.Vertice[0].y;

   S:=0;
   for i:=0 to countVX-1 do    //0,1,2
   begin
      S:=  S + ( VX[i].x*VX[i+1].y - VX[i+1].x*VX[i].y );
   end;
   A:= S/2;
   Result:= A;
end;

procedure TEntity.MiddlePoint(out x: real; out y: real);
begin
   case EntityState of
        etText: MiddlePointText(x,y);
        etLine: MiddlePointLine(x,y);
        etPolyline: MiddlePointPolygon(x,y);
        etCircle: MiddlePointCircle(x,y);
        etRectangle: MiddlePointRectangle(x,y);
        etArc: MiddlePointArc(x,y);
        etLinearAlignedDim: MiddlePointLinearAlignedDim(x,y);
   end;
end;

function TEntity.Area: real;
begin
    Result:= 0;
    case EntityState of
        etPolyline: Result:=AreaPolygon;
        etCircle: Result:=AreaCircle;
        //etRectangle: Result:=AreaRectangle;
        //etEllipse: Result:=AreaEllpse;
    end;
end;

    //TODO: more entity here!
procedure TEntity.TranslateByXY(VP:TViewPort; X, Y:integer);
begin
     if EntityState = etText then
     begin
          ParallelTextByXYPoint(VP.SurfaceToWorldX(X),
                                        VP.SurfaceToWorldY(Y));
     end;
     if EntityState = etLine then
     begin
       ParallelLineByXYPoint(VP.SurfaceToWorldX(X),
                                     VP.SurfaceToWorldY(Y));
     end;
     if EntityState = etCircle then
     begin
       ParallelEllipseByXYPoint(VP.SurfaceToWorldX(X),
                                     VP.SurfaceToWorldY(Y));
     end;
     if EntityState = etArc then
     begin
       ParallelArcByXYPoint(VP.SurfaceToWorldX(X),
                                     VP.SurfaceToWorldY(Y));
     end;
     if EntityState = etPolyline then
     begin
        ParallelPolygonByXYPoint(VP.SurfaceToWorldX(X),
                                           VP.SurfaceToWorldY(Y));
     end;
     if EntityState = etRectangle then
     begin
        ParallelRectangleByXYPoint(VP.SurfaceToWorldX(X),
                                           VP.SurfaceToWorldY(Y));
     end;
     if EntityState = etLinearAlignedDim then
     begin
        ParallelLinearAlignedDimByXYPoint(VP.SurfaceToWorldX(X),
                                           VP.SurfaceToWorldY(Y));
     end;
end;

function TEntity.DataToDXF: string;
var
  count,i: integer;
  dxfList: TStringList;
  cirx, ciry, ra: real;
begin
     dxfList:= TStringList.Create;
     if  EntityState = etPoint then
     begin
        dxfList.Add('0');
        dxfList.Add('POINT');
        dxfList.Add('100');
        dxfList.Add('AcDbEntity');
        dxfList.Add('8');
        if FEntityData.Layer = '' then
           dxfList.Add('0') {default Layer}
        else
           dxfList.Add(FEntityData.Layer); {Layer}
        dxfList.Add('62');
        dxfList.Add('256');
        dxfList.Add('6');
        dxfList.Add('BYLAYER');
        dxfList.Add('39');  {THICKNESS}
        dxfList.Add('0.00');
        dxfList.Add('100');
        dxfList.Add('AcDbPoint');
        dxfList.Add('10');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2),',','.'));
        dxfList.Add('20');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2),',','.'));
        dxfList.Add('30');
        dxfList.Add('0.0');
     end;
     if  EntityState = etLine then
     begin
        dxfList.Add('0');
        dxfList.Add('LINE');
        dxfList.Add('100');
        dxfList.Add('AcDbEntity');
        dxfList.Add('8');
        if FEntityData.Layer = '' then
           dxfList.Add('0') {default Layer}
        else
           dxfList.Add(FEntityData.Layer); {Layer}
        dxfList.Add('62');
        dxfList.Add('256');
        dxfList.Add('6');
        dxfList.Add('BYLAYER');
        dxfList.Add('39');  {THICKNESS}
        dxfList.Add('0.00');
        dxfList.Add('100');
        dxfList.Add('AcDbLine');
        dxfList.Add('10');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2),',','.'));
        dxfList.Add('20');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2),',','.'));
        dxfList.Add('30');
        dxfList.Add('0.0');
        dxfList.Add('11');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[1].x, ffFixed, 0,2),',','.'));
        dxfList.Add('21');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[1].y, ffFixed, 0,2),',','.'));
        dxfList.Add('31');
        dxfList.Add('0.0');
     end;
     if  EntityState = etText then
     begin
        dxfList.Add('0');
        dxfList.Add('TEXT');
        dxfList.Add('100');
        dxfList.Add('AcDbEntity');
        dxfList.Add('8');
        if FEntityData.Layer = '' then
           dxfList.Add('0') {default Layer}
        else
           dxfList.Add(FEntityData.Layer); {Layer}
        dxfList.Add('62');
        dxfList.Add('256');
        dxfList.Add('6');
        dxfList.Add('BYLAYER');
        dxfList.Add('100');
        dxfList.Add('AcDbText');
        dxfList.Add('10');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2),',','.'));
        dxfList.Add('20');
        dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2),',','.'));
        dxfList.Add('30');
        dxfList.Add('0.0');
        dxfList.Add('1');
        dxfList.Add(FEntityData.Title);
        dxfList.Add('7');
        dxfList.Add('STANDARD');
        dxfList.Add('40');
        dxfList.Add('0.18');
        dxfList.Add('50');
        dxfList.Add('0');
     end;
     if  EntityState = etCircle then
     begin;
        dxfList.Add('0');
        dxfList.Add('CIRCLE');
        dxfList.Add('100');
        dxfList.Add('AcDbEntity');
        dxfList.Add('8');
        if FEntityData.Layer = '' then
           dxfList.Add('0') {default Layer}
        else
           dxfList.Add(FEntityData.Layer); {Layer}
        dxfList.Add('62');
        dxfList.Add('256');
        dxfList.Add('6');
        dxfList.Add('BYLAYER');
        dxfList.Add('39');
        dxfList.Add('0.00');
        dxfList.Add('100');
        dxfList.Add('AcDbCircle');
        dxfList.Add('10');  //nedd fix here!
        cirx:= (FEntityData.Vertice[1].x + FEntityData.Vertice[0].x)/2;
        dxfList.Add(ReplaceChar(FloatToStrF(cirx, ffFixed, 0,2),',','.'));
        dxfList.Add('20');
        ciry:= (FEntityData.Vertice[1].y + FEntityData.Vertice[0].y)/2;
        dxfList.Add(ReplaceChar(FloatToStrF(ciry, ffFixed, 0,2),',','.'));
        dxfList.Add('30');
        dxfList.Add('0.0');
        dxfList.Add('40'); // //nedd fix here!
        ra:= Abs(FEntityData.Vertice[1].x - FEntityData.Vertice[0].x)/2;
        dxfList.Add(ReplaceChar(FloatToStrF(ra, ffFixed, 0,2),',','.'));
     end;
     if EntityState = etPolyline then
     begin
       dxfList.Add('0');
       dxfList.Add('POLYLINE');
       dxfList.Add('100');
       dxfList.Add('AcDbEntity');
       dxfList.Add('8');
        if FEntityData.Layer = '' then
           dxfList.Add('0') {default Layer}
        else
           dxfList.Add(FEntityData.Layer); {Layer}
       dxfList.Add('62');
       dxfList.Add('256');
       dxfList.Add('6');
       dxfList.Add('BYLAYER');
       dxfList.Add('39');
       dxfList.Add('0.00');
       dxfList.Add('100');
       dxfList.Add('AcDb2dPolyline');
       dxfList.Add('70');
       dxfList.Add('1');
       dxfList.Add('66');
       dxfList.Add('1');
       count:= High(FEntityData.Vertice)+1;
       for i:=0 to count-1do
       begin
          dxfList.Add('0');
          dxfList.Add('VERTEX');
          dxfList.Add('100');
          dxfList.Add('AcDbEntity');
          dxfList.Add('8');
           if FEntityData.Layer = '' then
              dxfList.Add('0') {default Layer}
           else
              dxfList.Add(FEntityData.Layer); {Layer}
          dxfList.Add('62');
          dxfList.Add('256');
          dxfList.Add('6');
          dxfList.Add('BYLAYER');
          dxfList.Add('39');    {THICKNESS}
          dxfList.Add('0.00');
          dxfList.Add('100');
          dxfList.Add('AcDbVertex');
          dxfList.Add('100');
          dxfList.Add('AcDb2dVertex');

          dxfList.Add('10');
          dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[i].x, ffFixed,0,2),',','.'));
          dxfList.Add('20');
          dxfList.Add(ReplaceChar(FloatToStrF(FEntityData.Vertice[i].y, ffFixed,0,2),',','.'));
          dxfList.Add('30');
          dxfList.Add('0.0');
       end;
       dxfList.Add('0');
       dxfList.Add('SEQEND');
     end;
     Result:= Trim(dxfList.Text);
     dxfList.Free;
end;

function TEntity.DataToSVG: string;
var
   count,i: integer;
   dataSVG: string;
begin
   if EntityState = etPoint then
   begin                       //MUST TODO !!!: ReplaceChar(FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2), ',','.')
     dataSVG:= '<point x="'+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2) +'"'+
                        ' y="'+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2)+'"/>';
   end;
   if  EntityState = etLine then
   begin                           //MUST TODO !!!: ReplaceChar
     dataSVG:= '<line x1="'+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2) +'"'+
                    ' y1="'+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2) +'"'+
                    ' x2="'+ FloatToStrF(FEntityData.Vertice[1].x, ffFixed, 0,2) +'"'+
                    ' y2="'+ FloatToStrF(FEntityData.Vertice[1].y, ffFixed, 0,2)+ '"/> ';

   end;
   if  EntityState = etText then
   begin                                       //MUST TODO !!!: ReplaceChar
     dataSVG:= '<text x="'+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2) +'"'+
                       ' y="'+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2) +'">'+
                         FEntityData.Title+'</text>';
   end;
   if  EntityState = etCircle then
   begin;                                   //MUST TODO !!!: ReplaceChar
     dataSVG:= '<circle cx="'+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2) +'"'+
                    ' cy="'+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2) +'"'+
                    ' r="'+ FloatToStrF(Abs(FEntityData.Vertice[1].x - FEntityData.Vertice[0].x), ffFixed, 0,2)+'"/>';
   end;
   if EntityState = etPolyline then
   begin
     count:= High(FEntityData.Vertice)+1;
     dataSVG:= '';
     for i:=0 to count-1 do
     begin                                        //MUST TODO !!!: ReplaceChar
        dataSVG:= dataSVG+ FloatToStrF(FEntityData.Vertice[i].x, ffFixed,0,2)+','+
                                 FloatToStrF(FEntityData.Vertice[i].y, ffFixed,0,2)+' ';
     end;
     dataSVG:= '<polygon points="' + dataSVG + '"/>';
   end;
   Result:= dataSVG;
end;

function TEntity.DataToXML: string;
begin
   Result:= 'Wait. Not currently implemented...'
end;

//TODO....
function TEntity.DataToNameSpace(fieldSeparator: char; nameValueSeparator: char): string;
var
   dummyPage: integer;
begin
   dummyPage:= 0;
   Result:= 'draw.page('+IntToStr(dummyPage)+').layer('+EntityData.Layer+').entities$entity('+DataToString(fieldSeparator,nameValueSeparator)+')';
end;

//dataSpace:= 'draw.page('+IntToStr(idPage)+').layer('+EntityData.Layer+').entities$entity('+dataSpace+')';
//draw.page(1).layer(0).entities$entity(id#line x#2,0 4,3;y#12,1 14,6;z#0 0);

function TEntity.DataToString(fieldSeparator: char; nameValueSeparator: char): string;
var
   dataStr, strX, strY: string;
   count, i: integer;
begin
   if  EntityState = etText then
   begin
     dataStr:='id'+nameValueSeparator+'text'+
                      fieldSeparator+'layer'+nameValueSeparator+FEntityData.Layer+
                      fieldSeparator+'title'+nameValueSeparator+FEntityData.Title+
                      fieldSeparator+'x'+nameValueSeparator+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2)+
                      fieldSeparator+'y'+nameValueSeparator+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2);
   end;
   if  EntityState = etPoint then
   begin
      dataStr:= 'id'+nameValueSeparator+'point'+
                        fieldSeparator+'layer'+nameValueSeparator+FEntityData.Layer+
                        fieldSeparator+'title'+nameValueSeparator+FEntityData.Title+
                        fieldSeparator+'x'+nameValueSeparator+FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2)+
                        fieldSeparator+'y'+nameValueSeparator+FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2);
   end;
   if  EntityState = etLine then
   begin
     dataStr:= 'id'+nameValueSeparator+'line'+
                      fieldSeparator+'layer'+nameValueSeparator+FEntityData.Layer+
                      fieldSeparator+'title'+nameValueSeparator+FEntityData.Title+
                      fieldSeparator+'x'+nameValueSeparator+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2)+' '+
                                FloatToStrF(FEntityData.Vertice[1].x, ffFixed, 0,2)+
                      fieldSeparator+'y'+nameValueSeparator+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2)+' '+
                               FloatToStrF(FEntityData.Vertice[1].y, ffFixed, 0,2);
   end;
   if EntityState = etCircle then
   begin;

     dataStr:= 'id'+nameValueSeparator+'circle'+
                      fieldSeparator+'layer'+nameValueSeparator+FEntityData.Layer+
                      fieldSeparator+'title'+nameValueSeparator+FEntityData.Title+
                      fieldSeparator+'x'+nameValueSeparator+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2)+' '+
                             FloatToStrF(FEntityData.Vertice[1].x, ffFixed, 0,2)+
                      fieldSeparator+'y'+nameValueSeparator+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2)+' '+
                               FloatToStrF(FEntityData.Vertice[1].y, ffFixed, 0,2);
     (*
     dataStr:= 'id#circle;layer#'+FEntityData.Layer+';title#'+FEntityData.Title+
                        ';x#'+ FloatToStrF(FEntityData.Vertice[0].x, ffFixed, 0,2)+
                        ';y#'+ FloatToStrF(FEntityData.Vertice[0].y, ffFixed, 0,2)+
                        ';r#'+ FloatToStrF(Abs(FEntityData.Vertice[1].x -
                                                  FEntityData.Vertice[0].x), ffFixed, 0,2);
     *)

   end;
   if EntityState = etPolyline then
   begin
     count:= High(FEntityData.Vertice) + 1;
     strX:='';
     strY:='';
     for i:=0 to count-1 do
     begin
        strX:= strX + FloatToStrF(FEntityData.Vertice[i].x, ffFixed,0,2) +' ';
        strY:= strY + FloatToStrF(FEntityData.Vertice[i].y, ffFixed,0,2) +' ';
     end;
     dataStr:= 'id'+nameValueSeparator+'polygon'+
                         fieldSeparator+'layer'+nameValueSeparator+FEntityData.Layer+
                         fieldSeparator+'title'+nameValueSeparator+FEntityData.Title+
                         fieldSeparator+'x'+nameValueSeparator+Trim(strX)+
                         fieldSeparator+'y'+nameValueSeparator+Trim(strY);
   end;
   Result:= dataStr;
end;

procedure TEntity.DrawCircle(VP: TViewPort; Canvas: TFPCustomCanvas);
var
 pX{Left}, pY{Top}: integer;
 pX2{right}, pY2{bottom}: integer;
begin
  VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX, pY);
  VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);
  if Canvas <> nil then Canvas.Ellipse(pX, pY, pX2, pY2);
end;

{
 Rec := Rect(px1 - Round(Radius), py1 - Round(Radius), px1 + Round(Radius), py1 + Round(Radius));
 GetPositionForAngle(ToRadians(15), Radius, px1, py1, P1X, P1Y);
 GetPositionForAngle(ToRadians(70),  Radius, px1, py1, P2X, P2Y);
 Canvas.Arc(Rec.Left, Rec.Top, Rec.Right, Rec.Bottom, P1X, P1Y, P2X, P2Y);
}

{   l = 1 , q= ...
FPNoGUIGraphicsBridge3.AddEntity(layer,'Arc',[Point(l, 0.0)= centro, Point(l/2, 0.0) = r ,Point(l, q) = final],
                                                FloatToStrF(q, ffFixed, 0,1),'');
}
procedure TEntity.DrawArc(Canvas: TFPCustomCanvas; px1, py1, px2, py2, px3, py3: integer;  sTitle: string);
var
   ar:  real;
   P3X, P3Y, P4X, P4Y: real;
   px4, py4: real;
   i, distP2P3: integer;
begin
  ar:= GetDistanceBetweenTwoPoints(px1, py1, px2, py2);
  GetPointByOffset(-ar, px1, py1, px3, py3, P3X, P3Y, 1);
  distP2P3:= Round(GetDistanceBetweenTwoPoints(px2, py2,P3X,P3Y));

  Canvas.MoveTo(px2,py2);
  for i:=1 to distP2P3 do
  begin
      GetPointByOffset(-i, px2, py2, P3X, P3Y, px4, py4, 1);
      GetPointByOffset(-ar, px1, py1, px4, py4, P4X, P4Y, 1);
      Canvas.LineTo(Round(P4X),Round(P4Y));
  end;
   {
   Rec := Rect(px1 - Round(ar), py1 - Round(ar), px1 + Round(ar), py1 + Round(ar));
   Canvas.Arc(Rec.Left, Rec.Top, Rec.Right, Rec.Bottom, px2, py2, Round(P3X), Round(P3Y));
   Canvas.PolyBezier([ToIntegerPoint(px2, py2),
                     ToIntegerPoint(Round(P4X),Round(P4Y)),
                     ToIntegerPoint(Round(P3X), Round(P3Y))],False,False);
  }
end;

procedure TEntity.DrawArc(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   ar:  real;
   P3X, P3Y, P4X, P4Y: real;
   px4, py4: real;
   i, distP2P3: integer;
   px1, py1, px2, py2, px3, py3: integer;
begin
  VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, px1, py1);
  VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, px2, py2);
  VP.WorldToSurfaceXY(FEntityData.Vertice[2].x, FEntityData.Vertice[2].y, px3, py3);
  ar:= GetDistanceBetweenTwoPoints(px1, py1, px2, py2);
  GetPointByOffset(-ar, px1, py1, px3, py3, P3X, P3Y, 1);
  distP2P3:= Round(GetDistanceBetweenTwoPoints(px2, py2,P3X,P3Y));
  Canvas.MoveTo(px2,py2);
  for i:=1 to distP2P3 do
  begin
      GetPointByOffset(-i, px2, py2, P3X, P3Y, px4, py4, 1);
      GetPointByOffset(-ar, px1, py1, px4, py4, P4X, P4Y, 1);
      Canvas.LineTo(Round(P4X),Round(P4Y));
  end;
   {Rec := Rect(px1 - Round(ar), py1 - Round(ar), px1 + Round(ar), py1 + Round(ar));
   Canvas.Arc(Rec.Left, Rec.Top, Rec.Right, Rec.Bottom, px2, py2, Round(P3X), Round(P3Y));
   Canvas.PolyBezier([ToIntegerPoint(px2, py2),
                     ToIntegerPoint(Round(P4X),Round(P4Y)),
                     ToIntegerPoint(Round(P3X), Round(P3Y))],False,False);}
end;

procedure TEntity.DrawPoint(Canvas: TFPCustomCanvas; x,y: integer; sTitle: string);
begin
   if Canvas <> nil then
   begin
     Canvas.Line(x,y,x,y);
   end;
end;

procedure TEntity.DrawPoint(VP: TViewPort; Canvas: TFPCustomCanvas);
var
  x,y:integer;
begin
  VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, x, y);
  if Canvas <> nil then
  begin
     Canvas.Line(x,y,x,y);
  end;
end;

procedure TEntity.DrawText(VP: TViewPort; Canvas: TFPCustomCanvas);
var
  x, y: integer;
begin
   VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, x, y);
   if Canvas <> nil then
   begin
     //Canvas.Brush.Style := bsClear;
     Canvas.TextOut(x,y,FEntityData.Title);
     //Canvas.Brush.Style := bsSolid;
   end;
end;

procedure TEntity.DrawText(Canvas: TFPCustomCanvas; x,y: integer; sTitle: string);
begin
   if Canvas <> nil then
   begin
     //Canvas.Brush.Style := bsClear;
     Canvas.TextOut(x,y,sTitle);
     //Canvas.Brush.Style := bsSolid;
   end;
end;

procedure TEntity.DrawLine(VP: TViewPort; Canvas: TFPCustomCanvas);
var
  px1, py1, px2, py2: integer;
begin
   VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, px1, py1);
   VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, px2, py2);
   if Canvas <> nil then
   begin
       Canvas.MoveTo(px1,py1);
       Canvas.LineTo(px2,py2);
   end;
end;

procedure TEntity.DrawPolyline(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   L: TRealLine;
   v1, v2: TRealPoint;
   i, count, pX1, pY1, pX2, pY2: integer;
begin
      count:= High(FEntityData.Vertice)+1;
      for i:=0 to count-1 do
      begin
          L:= GetLineFromIndex(FEntityData.Vertice, i);
          GetVerticesFromLine(L, v1, v2);
          VP.WorldToSurfaceXY(v1.x, v1.y, pX1, pY1);
          VP.WorldToSurfaceXY(v2.x, v2.y, pX2, pY2);
          //VP.LineClipSurface(pX1, pY1, pX2, pY2);
          //if VP.Clip then inside:=VP.LineClipSurface(pX1, pY1, pX2, pY2);
          //if inside then DrawLine(Canvas, pX1, pY1, pX2, pY2, Title);
          DrawLine(Canvas, pX1, pY1, pX2, pY2, FEntityData.Title);
      end;
end;

procedure TEntity.DrawRectangle(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   px1, py1, px2, py2: integer;
begin
   VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, px1, py1);
   VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, px2, py2);
   if Canvas <> nil then
   begin
       //Canvas.Brush.Style := bsClear;
       Canvas.Rectangle(px1, py1, px2, py2);
       //Canvas.Brush.Style := bsSolid;
   end;
end;

procedure TEntity.DrawLine(Canvas: TFPCustomCanvas; px1, py1, px2, py2: integer; sTitle: string);
begin
   if Canvas <> nil then
   begin
       Canvas.MoveTo(px1,py1);
       Canvas.LineTo(px2,py2);
   end;
end;

procedure TEntity.DrawRectangle(Canvas: TFPCustomCanvas; px1, py1, px2, py2: integer; sTitle: string);
begin
   if Canvas <> nil then
   begin
     //  Canvas.Brush.Style := bsClear;
       Canvas.Rectangle(px1, py1, px2, py2);
      // Canvas.Brush.Style := bsSolid;
   end;
end;

procedure TEntity.DrawArrow(Canvas: TFPCustomCanvas; V: array of TPoint; sTitle: string);
begin
   if Canvas <> nil then
   begin
       Canvas.Polygon(V);
   end;
end;

procedure TEntity.DrawArrow(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   i, count: integer;
   iV: array of TPoint;
   x, y: integer;
begin
   count:= High(FEntityData.Vertice) + 1;
   SetLength(iV, count);
   for i:= 0 to count-1 do
   begin
     VP.WorldToSurfaceXY(FEntityData.Vertice[i].x, FEntityData.Vertice[i].y, x, y);
     iV[i].x:= x;
     iV[i].y:= y;
   end;

   if Canvas <> nil then
   begin
       Canvas.Polygon(iV);
   end;
end;

procedure TEntity.DrawLinearDimension(VP: TViewPort; Canvas: TFPCustomCanvas; dx1, dy1, dx2, dy2: integer; sTitle: string);
var
   px,py, px1,py1, px2,py2, angle: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   distP1P2, dLineX1, dLineY1, dLineX2, dLineY2 : real;
   idLineX1, idLineY1, idLineX2, idLineY2: integer;
   iorthoX1, iorthoY1, iorthoX2, iorthoY2: integer;
   ipx1, ipy1, ipx2, ipy2: integer;
   x0, y0, x , y : integer;
   lineSide: integer;
begin
   x0:=dx1;
   y0:=dy1;
   x:= dx2;
   y:= dy2;

   if FEntityData.LineToggleSide = tsSide1 then lineSide:= 1
   else lineSide:= -1;

   if EntityState = etLinearHorizontalDim then y:= y0;
   if EntityState = etLinearVerticalDim then x:= x0;

   distP1P2:= GetDistanceBetweenTwoPoints(VP.SurfaceToWorldX(x0),VP.SurfaceToWorldY(y0),
                                          VP.SurfaceToWorldX(x),VP.SurfaceToWorldY(y));

   GetLineParallel(lineSide*FEntityData.FDimLineOffset, x0, y0, x, y, dLineX1,dLineY1,dLineX2,dLineY2);

   idLineX1:= Round(dLineX1);
   idLineY1:= Round(dLineY1);
   idLineX2:= Round(dLineX2);
   idLineY2:= Round(dLineY2);

   DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, 'DimLine');  //dimension line

   GetLineOrthogonal(-FEntityData.FDimArrowSize{offset}, FEntityData.FDimArrowWidth {r}, dLineX1,dLineY1,dLineX2,dLineY2,
                      orthoX1,orthoY1,orthoX2,orthoY2, px, py, 1);

   iorthoX1:= Round(orthoX1);
   iorthoY1:= Round(orthoY1);
   iorthoX2:= Round(orthoX2);
   iorthoY2:= Round(orthoY2);

   DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
                      ToIntegerPoint(idLineX1,idLineY1),ToIntegerPoint(idLineX1,idLineY1)],'Arrow1');

   GetLineOrthogonal(-FEntityData.FDimArrowSize{offset}, FEntityData.FDimArrowWidth {r},
                      dLineX1,dLineY1,dLineX2,dLineY2, orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);

   iorthoX1:= Round(orthoX1);
   iorthoY1:= Round(orthoY1);
   iorthoX2:= Round(orthoX2);
   iorthoY2:= Round(orthoY2);

   DrawArrow(Canvas, [ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
             ToIntegerPoint(idLineX2,idLineY2),ToIntegerPoint(idLineX2,idLineY2)],'Arrow2');

   GetLineTranslated(FEntityData.FDimExtensionLineGAP,x0, y0, dLineX1,dLineY1, px1, py1, px2, py2);
   ipx1:=Round(px1);
   ipy1:=Round(py1);
   ipx2:=Round(px2);
   ipy2:=Round(py2);
   DrawLine(Canvas, ipx1, ipy1, ipx2, ipy2, 'ExtLine1');   //extLine1

   GetLineTranslated(FEntityData.FDimExtensionLineGAP, x, y, dLineX2,dLineY2, px1, py1, px2, py2);
   ipx1:=Round(px1);
   ipy1:=Round(py1);
   ipx2:=Round(px2);
   ipy2:=Round(py2);
   DrawLine(Canvas, ipx1, ipy1, ipx2, ipy2,'ExtLine2');  //extLine2

   angle:= GetAngleOfLine(dLineX1,dLineY1,dLineX2,dLineY2);
   FEntityData.AngleOfText:= ToDegrees(angle);
   DrawText(Canvas, Round((dLineX1+dLineX2)/2),Round((dLineY1+dLineY2)/2),
                   FloatToStrF(distP1P2,ffFixed,0,2));
end;


procedure TEntity.DrawLinearDimension(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   px,py, px1,py1, px2,py2, angle: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   distP1P2, dLineX1, dLineY1, dLineX2, dLineY2 : real;
   idLineX1, idLineY1, idLineX2, idLineY2: integer;
   iorthoX1, iorthoY1, iorthoX2, iorthoY2: integer;
   ipx1, ipy1, ipx2, ipy2: integer;
   x0, y0, x , y : integer;
   lineSide: integer;
   dx1, dy1, dx2, dy2: integer;
begin
 if EntityState = etLine  then
 begin
   dx1:= VP.WorldToSurfaceX(FEntityData.Vertice[0].x);
   dy1:= VP.WorldToSurfaceY(FEntityData.Vertice[0].y);
   dx2:= VP.WorldToSurfaceX(FEntityData.Vertice[1].x);
   dy2:= VP.WorldToSurfaceY(FEntityData.Vertice[1].y);
   //sTitle:= Title;

   x0:=dx1;
   y0:=dy1;
   x:= dx2;
   y:= dy2;

   if FEntityData.LineToggleSide = tsSide1 then lineSide:= 1
   else lineSide:= -1;

   if EntityState = etLinearHorizontalDim then y:= y0;
   if EntityState = etLinearVerticalDim then x:= x0;

   distP1P2:= GetDistanceBetweenTwoPoints(VP.SurfaceToWorldX(x0),VP.SurfaceToWorldY(y0),
                                          VP.SurfaceToWorldX(x),VP.SurfaceToWorldY(y));

   GetLineParallel(lineSide*FEntityData.FDimLineOffset, x0, y0, x, y, dLineX1,dLineY1,dLineX2,dLineY2);

   idLineX1:= Round(dLineX1);
   idLineY1:= Round(dLineY1);
   idLineX2:= Round(dLineX2);
   idLineY2:= Round(dLineY2);

   DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, 'DimLine');  //dimension line

   GetLineOrthogonal(-FEntityData.FDimArrowSize{offset}, FEntityData.FDimArrowWidth {r}, dLineX1,dLineY1,dLineX2,dLineY2,
                      orthoX1,orthoY1,orthoX2,orthoY2, px, py, 1);

   iorthoX1:= Round(orthoX1);
   iorthoY1:= Round(orthoY1);
   iorthoX2:= Round(orthoX2);
   iorthoY2:= Round(orthoY2);

   DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
                      ToIntegerPoint(idLineX1,idLineY1),ToIntegerPoint(idLineX1,idLineY1)],'Arrow1');

   GetLineOrthogonal(-FEntityData.FDimArrowSize{offset}, FEntityData.FDimArrowWidth {r},
                      dLineX1,dLineY1,dLineX2,dLineY2, orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);

   iorthoX1:= Round(orthoX1);
   iorthoY1:= Round(orthoY1);
   iorthoX2:= Round(orthoX2);
   iorthoY2:= Round(orthoY2);

   DrawArrow(Canvas, [ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
             ToIntegerPoint(idLineX2,idLineY2),ToIntegerPoint(idLineX2,idLineY2)],'Arrow2');

   GetLineTranslated(FEntityData.FDimExtensionLineGAP,x0, y0, dLineX1,dLineY1, px1, py1, px2, py2);
   ipx1:=Round(px1);
   ipy1:=Round(py1);
   ipx2:=Round(px2);
   ipy2:=Round(py2);
   DrawLine(Canvas, ipx1, ipy1, ipx2, ipy2, 'ExtLine1');   //extLine1

   GetLineTranslated(FEntityData.FDimExtensionLineGAP, x, y, dLineX2,dLineY2, px1, py1, px2, py2);
   ipx1:=Round(px1);
   ipy1:=Round(py1);
   ipx2:=Round(px2);
   ipy2:=Round(py2);
   DrawLine(Canvas, ipx1, ipy1, ipx2, ipy2,'ExtLine2');  //extLine2

   angle:= GetAngleOfLine(dLineX1,dLineY1,dLineX2,dLineY2);
   FEntityData.AngleOfText:= ToDegrees(angle);
   DrawText(Canvas, Round((dLineX1+dLineX2)/2),Round((dLineY1+dLineY2)/2),
                   FloatToStrF(distP1P2,ffFixed,0,2));
 end;
end;

procedure TEntity.DrawParallelLine(VP: TViewPort; Canvas: TFPCustomCanvas; dx1, dy1, dx2, dy2: real; sTitle: string);
var
   dLineX1, dLineY1, dLineX2, dLineY2 : real;
   idLineX1, idLineY1, idLineX2, idLineY2: integer;
   x0, y0, x , y : integer;
   lineSide: integer;
   dist0,dist1: real;
   middleX, middleY: real;
   rx, ry: real;
   doffset, woffsetx, woffsety: real;
   idx1, idy1, idx2, idy2: integer;
begin
   idx1:= VP.WorldToSurfaceX(dx1);
   idy1:= VP.WorldToSurfaceY(dy1);
   idx2:= VP.WorldToSurfaceX(dx2);
   idy2:= VP.WorldToSurfaceY(dy2);

   x0:=idx1;
   y0:=idy1;
   x:= idx2;
   y:= idy2;

   dist0:= GetDistanceBetweenTwoPoints(idx1 ,idy1, FEntityData.TagX, FEntityData.TagY);
   dist1:= GetDistanceBetweenTwoPoints(idx2 ,idy2, FEntityData.TagX, FEntityData.TagY);

   if EntityState = etParallelLineHor then
   begin
       if dist0 < dist1 then y:= y0 else Y0:= y;
       FEntityData.Offset:= 0;
   end;

   if EntityState = etParallelLineVer then
   begin
       if dist0 < dist1 then x:= x0 else X0:= x;
       FEntityData.Offset:= 0;
   end;

   middleX:= (dx1+dx2)/2;
   middleY:= (dy1+dy2)/2;

   if FEntityData.Offset <> 0 then  //parallel
   begin

     if FEntityData.TagY > VP.WorldToSurfaceY(middleY) then  lineSide:= -1
     else lineSide:= 1;

     GetPointByOffset(FEntityData.Offset, middleX, middleY, dx2, dy2, rx, ry, 1);
     GetExtremeLinePointByRotation(PI/2, middleX, middleY, rx, ry, woffsetx, woffsety, 2);
     doffset:= GetDistanceBetweenTwoPoints(VP.WorldToSurfaceX(middleX), VP.WorldToSurfaceY(middleY),
                                           VP.WorldToSurfaceX(woffsetx), VP.WorldToSurfaceY(woffsety));

     GetLineParallel(lineSide*doffset, idx1, idy1, idx2, idy2, dLineX1,dLineY1,dLineX2,dLineY2);

     idLineX1:= Round(dLineX1);
     idLineY1:= Round(dLineY1);
     idLineX2:= Round(dLineX2);
     idLineY2:= Round(dLineY2);

     DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, sTitle);

     FEntityData.Vertice[0].x:= VP.SurfaceToWorldX(idLineX1);
     FEntityData.Vertice[0].y:= VP.SurfaceToWorldY(idLineY1);
     FEntityData.Vertice[1].x:= VP.SurfaceToWorldX(idLineX2);
     FEntityData.Vertice[1].y:= VP.SurfaceToWorldY(idLineY2);
   end
   else
   begin
     DrawLine(Canvas,x0, y0, x, y, sTitle);
     FEntityData.Vertice[0].x:= VP.SurfaceToWorldX(x0);
     FEntityData.Vertice[0].y:= VP.SurfaceToWorldY(y0);
     FEntityData.Vertice[1].x:= VP.SurfaceToWorldX(x);
     FEntityData.Vertice[1].y:= VP.SurfaceToWorldY(y);
   end;
   EntityState:= etLine;
end;

procedure TEntity.TranslateLine(dxy: real);
var
   px1, py1, px2, py2:  real;
begin
   if EntityState = etLine  then
   begin
       GetLineTranslated(dxy,FEntityData.Vertice[0].x,FEntityData.Vertice[0].y,FEntityData.Vertice[1].x,FEntityData.Vertice[1].y,px1,py1,px2,py2);
       FEntityData.Vertice[0].x:= px1;
       FEntityData.Vertice[0].y:= py1;
       FEntityData.Vertice[1].x:= px2;
       FEntityData.Vertice[1].y:= py2;
   end;
end;

procedure TEntity.ReSizeLine(dxy: real; refPoint: integer);
var
   px1, py1, px2, py2:  real;
begin
 if EntityState = etLine  then
 begin
   if dxy < 0 then
      GetLineTrim(dxy,  FEntityData.Vertice[0].x,FEntityData.Vertice[0].y,FEntityData.Vertice[1].x,FEntityData.Vertice[1].y,px1,py1,px2,py2,refPoint)
   else
      GetLineExtend(dxy,FEntityData.Vertice[0].x,FEntityData.Vertice[0].y,FEntityData.Vertice[1].x,FEntityData.Vertice[1].y,px1,py1,px2,py2,refPoint);
   FEntityData.Vertice[0].x:= px1;
   FEntityData.Vertice[0].y:= py1;
   FEntityData.Vertice[1].x:= px2;
   FEntityData.Vertice[1].y:= py2;
 end;
end;

procedure TEntity.DrawParallelLine(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   dLineX1, dLineY1, dLineX2, dLineY2 : real;
   idLineX1, idLineY1, idLineX2, idLineY2: integer;
   x0, y0, x , y : integer;
   lineSide: integer;
   dist0,dist1: real;
   middleX, middleY: real;
   rx, ry: real;
   doffset, woffsetx, woffsety: real;
   idx1, idy1, idx2, idy2: integer;
   dx1, dy1, dx2, dy2: real;
   sTitle: string;
begin
   dx1:= FEntityData.Vertice[0].x;
   dy1:= FEntityData.Vertice[0].y;
   dx2:= FEntityData.Vertice[1].x;
   dy2:= FEntityData.Vertice[1].y;
   sTitle:= FEntityData.Title;

   idx1:= VP.WorldToSurfaceX(dx1);
   idy1:= VP.WorldToSurfaceY(dy1);
   idx2:= VP.WorldToSurfaceX(dx2);
   idy2:= VP.WorldToSurfaceY(dy2);

   x0:=idx1;
   y0:=idy1;
   x:= idx2;
   y:= idy2;

   dist0:= GetDistanceBetweenTwoPoints(idx1 ,idy1, FEntityData.TagX, FEntityData.TagY);
   dist1:= GetDistanceBetweenTwoPoints(idx2 ,idy2, FEntityData.TagX, FEntityData.TagY);

   if EntityState = etParallelLineHor then
   begin
       if dist0 < dist1 then y:= y0 else Y0:= y;
       FEntityData.Offset:= 0;
   end;

   if EntityState = etParallelLineVer then
   begin
       if dist0 < dist1 then x:= x0 else X0:= x;
       FEntityData.Offset:= 0;
   end;

   middleX:= (dx1+dx2)/2;
   middleY:= (dy1+dy2)/2;

   if FEntityData.Offset <> 0 then  //parallel
   begin

     if FEntityData.TagY > VP.WorldToSurfaceY(middleY) then  lineSide:= -1
     else lineSide:= 1;

     GetPointByOffset(FEntityData.Offset, middleX, middleY, dx2, dy2, rx, ry, 1);
     GetExtremeLinePointByRotation(PI/2, middleX, middleY, rx, ry, woffsetx, woffsety, 2);
     doffset:= GetDistanceBetweenTwoPoints(VP.WorldToSurfaceX(middleX), VP.WorldToSurfaceY(middleY),
                                           VP.WorldToSurfaceX(woffsetx), VP.WorldToSurfaceY(woffsety));

     GetLineParallel(lineSide*doffset, idx1, idy1, idx2, idy2, dLineX1,dLineY1,dLineX2,dLineY2);

     idLineX1:= Round(dLineX1);
     idLineY1:= Round(dLineY1);
     idLineX2:= Round(dLineX2);
     idLineY2:= Round(dLineY2);

     DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, sTitle);

     FEntityData.Vertice[0].x:= VP.SurfaceToWorldX(idLineX1);
     FEntityData.Vertice[0].y:= VP.SurfaceToWorldY(idLineY1);
     FEntityData.Vertice[1].x:= VP.SurfaceToWorldX(idLineX2);
     FEntityData.Vertice[1].y:= VP.SurfaceToWorldY(idLineY2);
   end
   else
   begin
     DrawLine(Canvas,x0, y0, x, y, sTitle);
     FEntityData.Vertice[0].x:= VP.SurfaceToWorldX(x0);
     FEntityData.Vertice[0].y:= VP.SurfaceToWorldY(y0);
     FEntityData.Vertice[1].x:= VP.SurfaceToWorldX(x);
     FEntityData.Vertice[1].y:= VP.SurfaceToWorldY(y);
   end;
   EntityState:= etLine;
end;

procedure TEntity.DrawRadialDimension(VP: TViewPort; Canvas: TFPCustomCanvas; cx, cy, r:integer; sTitle: string);
var
   px,py, angleRad, LeaderLength: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   iorthoX1,iorthoY1,iorthoX2,iorthoY2,idefPx,idefPy, idefOrigy, idefOrigx: integer;
   defOrigx, defOrigy, defPx, defPy, rx, ry: real;
begin
   angleRad:= ToRadians(FEntityData.AngleOfText); {default =0}
   rx:= r*cos(angleRad);
   ry:= r*sin(angleRad);
   defPx:= cx + rx;
   defPy:= cy + ry;
   if EntityState = etRadialDim then
   begin
     defOrigx:= cx;
     defOrigy:= cy;
   end;
   if EntityState = etDiameterDim then
   begin
     defOrigx:= cx - rx;
     defOrigy:= cy - ry;
   end;
   idefOrigx:= Round(defOrigx);
   idefOrigy:= Round(defOrigy);
   idefPx:= Round(defPx);
   idefPy:= Round(defPy);

   LeaderLength:= GetDistanceBetweenTwoPoints(VP.SurfaceToWorldX(idefOrigx),VP.SurfaceToWorldY(idefOrigy),
                                          VP.SurfaceToWorldX(idefPx),VP.SurfaceToWorldY(idefPy));

   DrawLine(Canvas, idefOrigx, idefOrigy, idefPx, idefPy , 'DimLine');  //dimension line
   if EntityState = etDiameterDim then
   begin
       GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth {r},
                            defOrigx, defOrigy, defPx, defPy,
                            orthoX1,orthoY1,orthoX2,orthoY2, px, py, 1);
       iorthoX1:= Round(orthoX1);
       iorthoY1:= Round(orthoY1);
       iorthoX2:= Round(orthoX2);
       iorthoY2:= Round(orthoY2);
       DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
                          ToIntegerPoint(idefOrigx,idefOrigy),ToIntegerPoint(idefOrigx,idefOrigy)],'Arrow1');

   end;
   GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth {r},
                        defOrigx, defOrigy, defPx, defPy,
                        orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);
   iorthoX1:= Round(orthoX1);
   iorthoY1:= Round(orthoY1);
   iorthoX2:= Round(orthoX2);
   iorthoY2:= Round(orthoY2);
   DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
                      ToIntegerPoint(idefPx,idefPy),ToIntegerPoint(idefPx,idefPy)],'Arrow2');

   DrawText(Canvas, Round(cx),Round(cy),
                  FloatToStrF(LeaderLength,ffFixed,0,2));
end;

procedure TEntity.DrawRadialDimension(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   px,py, angleRad, LeaderLength: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   iorthoX1,iorthoY1,iorthoX2,iorthoY2,idefPx,idefPy, idefOrigy, idefOrigx: integer;
   defOrigx, defOrigy, defPx, defPy, rx, ry: real;
   cx1, cy1, r1:integer;
   pX2, pY2: integer;
begin
   VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, cx1, cy1);  //cx,cy
   VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);  //r = Abs(pX2-cx1)
   r1:= Abs(pX2-cx1);

   angleRad:= ToRadians(FEntityData.AngleOfText); {default =0}
   rx:= r1*cos(angleRad);
   ry:= r1*sin(angleRad);

   defPx:= cx1 + rx;
   defPy:= cy1 + ry;

   if EntityState = etRadialDim then
   begin
     defOrigx:= cx1;
     defOrigy:= cy1;
   end;
   if EntityState = etDiameterDim then
   begin
     defOrigx:= cx1 - rx;
     defOrigy:= cy1 - ry;
   end;
   idefOrigx:= Round(defOrigx);
   idefOrigy:= Round(defOrigy);
   idefPx:= Round(defPx);
   idefPy:= Round(defPy);

   LeaderLength:= GetDistanceBetweenTwoPoints(VP.SurfaceToWorldX(idefOrigx),VP.SurfaceToWorldY(idefOrigy),
                                          VP.SurfaceToWorldX(idefPx),VP.SurfaceToWorldY(idefPy));

   DrawLine(Canvas, idefOrigx, idefOrigy, idefPx, idefPy , 'DimLine');  //dimension line
   if EntityState = etDiameterDim then
   begin
       GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth {r},
                            defOrigx, defOrigy, defPx, defPy,
                            orthoX1,orthoY1,orthoX2,orthoY2, px, py, 1);
       iorthoX1:= Round(orthoX1);
       iorthoY1:= Round(orthoY1);
       iorthoX2:= Round(orthoX2);
       iorthoY2:= Round(orthoY2);
       DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
                          ToIntegerPoint(idefOrigx,idefOrigy),ToIntegerPoint(idefOrigx,idefOrigy)],'Arrow1');

   end;
   GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth {r},
                        defOrigx, defOrigy, defPx, defPy,
                        orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);
   iorthoX1:= Round(orthoX1);
   iorthoY1:= Round(orthoY1);
   iorthoX2:= Round(orthoX2);
   iorthoY2:= Round(orthoY2);
   DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
                      ToIntegerPoint(idefPx,idefPy),ToIntegerPoint(idefPx,idefPy)],'Arrow2');

   DrawText(Canvas, Round(cx1),Round(cy1),
                  FloatToStrF(LeaderLength,ffFixed,0,2));
end;

procedure TEntity.DrawArcDimension(VP: TViewPort; Canvas: TFPCustomCanvas; dx1, dy1, dx2, dy2, dx3, dy3: integer; sTitle: string);
var
   px,py: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   dLineX1, dLineY1, dLineX2, dLineY2 : real;
   idLineX1, idLineY1, idLineX2, idLineY2: integer;
   iorthoX1, iorthoY1, iorthoX2, iorthoY2: integer;
   ar: real;
   D3X, D3Y, P2X, P2Y, P3X, P3Y: real;
   auxX1,auxY1,auxX2,auxY2: real;
   arcLen, wRadius, sRadius, angle_P2_P3_ByP1: real;
   wdx1, wdy1, wdx2, wdy2, wdx3, wdy3: real;
   {lenChord,} middleChordx, middleChordy: real;
   distP1ToMidleChord: real;
begin

   GetPointByOffset(FEntityData.FDimLineOffset, dx1, dy1, dx2, dy2, P2X, P2Y, 2);

   DrawArc(Canvas, dx1, dy1, Round(P2X), Round(P2Y), dx3{dummy}, dy3{dummy},  sTitle);

   GetLineTranslated(FEntityData.FDimExtensionLineGAP, dx2,dy2, P2X, P2Y,
                     dLineX1, dLineY1, dLineX2, dLineY2);

   idLineX1:= Round(dLineX1);
   idLineY1:= Round(dLineY1);
   idLineX2:= Round(dLineX2);
   idLineY2:= Round(dLineY2);

   DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, 'ExtLine1');  //dimension line

   GetLineOrthogonal(0.0, -2*FEntityData.FDimArrowSize,
                                       dx2,dy2, P2X, P2Y,
                                       orthoX1,orthoY1,orthoX2,orthoY2,
                                       px, py, 2);
   GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth{r},
                               orthoX1,orthoY1,orthoX2,orthoY2,
                               auxX1,auxY1,auxX2,auxY2,
                               px, py, 1);

   iorthoX1:= Round(auxX1);
   iorthoY1:= Round(auxY1);
   iorthoX2:= Round(auxX2);
   iorthoY2:= Round(auxY2);

   DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),
                     ToIntegerPoint(iorthoX2,iorthoY2),
                     ToIntegerPoint(Round(P2X), Round(P2Y)),
                     ToIntegerPoint(Round(P2X), Round(P2Y))],'Arrow1');
//------------------
    ar:= GetDistanceBetweenTwoPoints(dx1, dy1, dx2, dy2);

    GetPointByOffset(-ar, dx1, dy1, dx3, dy3, D3X, D3Y, 1);

    GetPointByOffset(FEntityData.FDimLineOffset, dx1, dy1, D3X, D3Y, P3X, P3Y, 2);

    GetLineTranslated(FEntityData.FDimExtensionLineGAP, D3X, D3Y, P3X, P3Y,
                    dLineX1, dLineY1, dLineX2, dLineY2);

    idLineX1:= Round(dLineX1);
    idLineY1:= Round(dLineY1);
    idLineX2:= Round(dLineX2);
    idLineY2:= Round(dLineY2);

    DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, 'ExtLine2');  //dimension line

    GetLineOrthogonal(0.0, -2*FEntityData.FDimArrowSize,
                                      D3X, D3Y, P3X, P3Y,
                                      orthoX1,orthoY1,orthoX2,orthoY2,
                                      px, py, 2);

    GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth{r},
                              orthoX1,orthoY1,orthoX2,orthoY2,
                              auxX1,auxY1,auxX2,auxY2,
                              px, py, 2);

    iorthoX1:= Round(auxX1);
    iorthoY1:= Round(auxY1);
    iorthoX2:= Round(auxX2);
    iorthoY2:= Round(auxY2);

    DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),
                    ToIntegerPoint(iorthoX2,iorthoY2),
                    ToIntegerPoint(Round(P3X), Round(P3Y)),
                    ToIntegerPoint(Round(P3X), Round(P3Y))],'Arrow2');
//-------------------------------
    wdx1:= VP.SurfaceToWorldX(dx1);
    wdy1:= VP.SurfaceToWorldY(dy1);

    wdx2:= VP.SurfaceToWorldX(dx2);
    wdy2:= VP.SurfaceToWorldY(dy2);

    sRadius:= GetDistanceBetweenTwoPoints(dx1,dy1,dx2,dy2);

    wRadius:= GetDistanceBetweenTwoPoints(wdx1,wdy1,wdx2,wdy2);

    wdx3:= VP.SurfaceToWorldX(DX3);
    wdy3:= VP.SurfaceToWorldY(DY3);

    //lenChord:= GetDistanceBetweenTwoPoints(wdx2,wdy2,wdx3,wdy3);
    middlechordx:= (dx2+dx3)/2;
    middlechordy:= (dy2+dy3)/2;
    distP1ToMidleChord:= GetDistanceBetweenTwoPoints(dx1,dy1,middlechordx,middlechordy);

    GetLineExtend(sRadius - distP1ToMidleChord, dx1, dy1, middlechordx, middlechordy,
                  dLineX1, dLineY1, dLineX2, dLineY2, 2);

    DrawLine(Canvas, Round(dLineX1), Round(dLineY1), Round(dLineX2), Round(dLineY2), 'RadialLine');  //dimension line

    GetLineOrthogonal(-FEntityData.FDimArrowSize{offset}, FEntityData.FDimArrowWidth {r},
                       dLineX1,dLineY1,dLineX2,dLineY2, orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);

    iorthoX1:= Round(orthoX1);
    iorthoY1:= Round(orthoY1);
    iorthoX2:= Round(orthoX2);
    iorthoY2:= Round(orthoY2);

    DrawArrow(Canvas, [ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
              ToIntegerPoint(Round(dLineX2),Round(dLineY2)),ToIntegerPoint(Round(dLineX2),Round(dLineY2))],'RadialArrow');

    DrawText(Canvas, Round((dLineX1+dLineX2)/2), Round((dLineY1+dLineY2)/2),
                     FloatToStrF(wRadius,ffFixed,0,2));


    angle_P2_P3_ByP1:= GetAngleBetweenPointsByVertice(wdx1, wdy1,wdx2, wdy2,wdx3, wdy3);

    arcLen:= wRadius*angle_P2_P3_ByP1;

    FEntityData.AngleOfText:= ToDegrees(angle_P2_P3_ByP1);

    GetLineExtend(FEntityData.FDimLineOffset+ sRadius - distP1ToMidleChord, dx1, dy1, middlechordx, middlechordy,
                  dLineX1, dLineY1, dLineX2, dLineY2, 2);

    DrawText(Canvas, Round(dLineX2), Round(dLineY2),
                     FloatToStrF(arcLen,ffFixed,0,2));

end;
procedure TEntity.DrawArcDimension(VP: TViewPort; Canvas: TFPCustomCanvas);
var
   px,py: real;
   orthoX1,orthoY1,orthoX2,orthoY2: real;
   dLineX1, dLineY1, dLineX2, dLineY2 : real;
   idLineX1, idLineY1, idLineX2, idLineY2: integer;
   iorthoX1, iorthoY1, iorthoX2, iorthoY2: integer;
   ar: real;
   D3X, D3Y, P2X, P2Y, P3X, P3Y: real;
   auxX1,auxY1,auxX2,auxY2: real;
   arcLen, wRadius, sRadius, angle_P2_P3_ByP1: real;
   wdx1, wdy1, wdx2, wdy2, wdx3, wdy3: real;
   {lenChord,} middleChordx, middleChordy: real;
   distP1ToMidleChord: real;
   dx1, dy1, dx2, dy2, dx3, dy3: integer;
   sTitle: string;
begin
   VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, dx1, dy1);
   VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, dx2, dy2);
   VP.WorldToSurfaceXY(FEntityData.Vertice[2].x, FEntityData.Vertice[2].y, dx3, dy3);
   sTitle:= FEntityData.Title;

   GetPointByOffset(FEntityData.FDimLineOffset, dx1, dy1, dx2, dy2, P2X, P2Y, 2);

   DrawArc(Canvas, dx1, dy1, Round(P2X), Round(P2Y), dx3{dummy}, dy3{dummy},  sTitle);

   GetLineTranslated(FEntityData.FDimExtensionLineGAP, dx2,dy2, P2X, P2Y,
                     dLineX1, dLineY1, dLineX2, dLineY2);

   idLineX1:= Round(dLineX1);
   idLineY1:= Round(dLineY1);
   idLineX2:= Round(dLineX2);
   idLineY2:= Round(dLineY2);

   DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, 'ExtLine1');  //dimension line

   GetLineOrthogonal(0.0, -2*FEntityData.FDimArrowSize,
                                       dx2,dy2, P2X, P2Y,
                                       orthoX1,orthoY1,orthoX2,orthoY2,
                                       px, py, 2);
   GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth{r},
                               orthoX1,orthoY1,orthoX2,orthoY2,
                               auxX1,auxY1,auxX2,auxY2,
                               px, py, 1);
   iorthoX1:= Round(auxX1);
   iorthoY1:= Round(auxY1);
   iorthoX2:= Round(auxX2);
   iorthoY2:= Round(auxY2);

   DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),
                     ToIntegerPoint(iorthoX2,iorthoY2),
                     ToIntegerPoint(Round(P2X), Round(P2Y)),
                     ToIntegerPoint(Round(P2X), Round(P2Y))],'Arrow1');
//------------------
    ar:= GetDistanceBetweenTwoPoints(dx1, dy1, dx2, dy2);

    GetPointByOffset(-ar, dx1, dy1, dx3, dy3, D3X, D3Y, 1);

    GetPointByOffset(FEntityData.FDimLineOffset, dx1, dy1, D3X, D3Y, P3X, P3Y, 2);

    GetLineTranslated(FEntityData.FDimExtensionLineGAP, D3X, D3Y, P3X, P3Y,
                    dLineX1, dLineY1, dLineX2, dLineY2);

    idLineX1:= Round(dLineX1);
    idLineY1:= Round(dLineY1);
    idLineX2:= Round(dLineX2);
    idLineY2:= Round(dLineY2);

    DrawLine(Canvas, idLineX1, idLineY1, idLineX2, idLineY2, 'ExtLine2');  //dimension line

    GetLineOrthogonal(0.0, -2*FEntityData.FDimArrowSize,
                                      D3X, D3Y, P3X, P3Y,
                                      orthoX1,orthoY1,orthoX2,orthoY2,
                                      px, py, 2);

    GetLineOrthogonal(-FEntityData.FDimArrowSize, FEntityData.FDimArrowWidth{r},
                              orthoX1,orthoY1,orthoX2,orthoY2,
                              auxX1,auxY1,auxX2,auxY2,
                              px, py, 2);

    iorthoX1:= Round(auxX1);
    iorthoY1:= Round(auxY1);
    iorthoX2:= Round(auxX2);
    iorthoY2:= Round(auxY2);

    DrawArrow(Canvas,[ToIntegerPoint(iorthoX1,iorthoY1),
                    ToIntegerPoint(iorthoX2,iorthoY2),
                    ToIntegerPoint(Round(P3X), Round(P3Y)),
                    ToIntegerPoint(Round(P3X), Round(P3Y))],'Arrow2');
//-------------------------------
    wdx1:= VP.SurfaceToWorldX(dx1);
    wdy1:= VP.SurfaceToWorldY(dy1);

    wdx2:= VP.SurfaceToWorldX(dx2);
    wdy2:= VP.SurfaceToWorldY(dy2);

    sRadius:= GetDistanceBetweenTwoPoints(dx1,dy1,dx2,dy2);

    wRadius:= GetDistanceBetweenTwoPoints(wdx1,wdy1,wdx2,wdy2);

    wdx3:= VP.SurfaceToWorldX(DX3);
    wdy3:= VP.SurfaceToWorldY(DY3);

    //lenChord:= GetDistanceBetweenTwoPoints(wdx2,wdy2,wdx3,wdy3);

    middlechordx:= (dx2+dx3)/2;
    middlechordy:= (dy2+dy3)/2;
    distP1ToMidleChord:= GetDistanceBetweenTwoPoints(dx1,dy1,middlechordx,middlechordy);

    GetLineExtend(sRadius - distP1ToMidleChord, dx1, dy1, middlechordx, middlechordy,
                  dLineX1, dLineY1, dLineX2, dLineY2, 2);

    DrawLine(Canvas, Round(dLineX1), Round(dLineY1), Round(dLineX2), Round(dLineY2), 'RadialLine');  //dimension line

    GetLineOrthogonal(-FEntityData.FDimArrowSize{offset}, FEntityData.FDimArrowWidth {r},
                       dLineX1,dLineY1,dLineX2,dLineY2, orthoX1,orthoY1,orthoX2,orthoY2, px, py, 2);

    iorthoX1:= Round(orthoX1);
    iorthoY1:= Round(orthoY1);
    iorthoX2:= Round(orthoX2);
    iorthoY2:= Round(orthoY2);

    DrawArrow(Canvas, [ToIntegerPoint(iorthoX1,iorthoY1),ToIntegerPoint(iorthoX2,iorthoY2),
              ToIntegerPoint(Round(dLineX2),Round(dLineY2)),ToIntegerPoint(Round(dLineX2),Round(dLineY2))],'RadialArrow');

    DrawText(Canvas, Round((dLineX1+dLineX2)/2), Round((dLineY1+dLineY2)/2),
                     FloatToStrF(wRadius,ffFixed,0,2));


    angle_P2_P3_ByP1:= GetAngleBetweenPointsByVertice(wdx1, wdy1,wdx2, wdy2,wdx3, wdy3);

    arcLen:= wRadius*angle_P2_P3_ByP1;

    FEntityData.AngleOfText:= ToDegrees(angle_P2_P3_ByP1);

    GetLineExtend(FEntityData.FDimLineOffset+ sRadius - distP1ToMidleChord, dx1, dy1, middlechordx, middlechordy,
                  dLineX1, dLineY1, dLineX2, dLineY2, 2);

    DrawText(Canvas, Round(dLineX2), Round(dLineY2),
                     FloatToStrF(arcLen,ffFixed,0,2));

end;

procedure TEntity.Draw(VP: TViewPort; Canvas: TFPCustomCanvas);
var
  // L: TRealLine;
   pX1, pY1, pX2, pY2{,pX3, pY3, w, h}: integer;
   //i, count: integer;
   //v1, v2: TRealPoint;
  // inside: boolean;
   saveColor: TFPColor;
begin
   if EntityState = etPoint then
   begin
     {w:= Canvas.TextWidth(Title);
      h:= Canvas.TextHeight(Title);
      VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1,pY1);
      pX2:= pX1+w;
      pY2:= pY1+h;
      if VP.Clip then inside:= VP.LineClipSurface(pX1, pY1, pX2, pY2);
      if inside then DrawPoint(Canvas, pX1, pY1, Title);}
      DrawPoint(VP, Canvas)
   end;
   if EntityState = etText then
   begin
      {w:= Canvas.TextWidth(Title);
      h:= Canvas.TextHeight(Title);

      VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1,pY1);
      pX2:= pX1+w;
      pY2:= pY1+h;
      if VP.Clip then inside:= VP.LineClipSurface(pX1, pY1, pX2, pY2);
      if inside then DrawText(Canvas, pX1, pY1, Title); }
      DrawText(VP, Canvas);
    end;
    if EntityState = etLine then
    begin
       {VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1,pY1);
       VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2,pY2);
       VP.LineClipSurface(pX1, pY1, pX2, pY2);
       if VP.Clip then inside:=VP.LineClipSurface(pX1, pY1, pX2, pY2);
       if inside then DrawLine(Canvas, pX1, pY1, pX2, pY2, Title); }
       DrawLine(VP, Canvas);
    end;
    if EntityState = etRectangle then
    begin
       {VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1,pY1);
       VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2,pY2);
       VP.LineClipSurface(pX1, pY1, pX2, pY2);
       if VP.Clip then inside:=VP.LineClipSurface(pX1, pY1, pX2, pY2);
       if inside then DrawRectangle(Canvas, pX1, pY1, pX2, pY2, Title);}
       DrawRectangle(VP, Canvas);
    end;
    if (EntityState = etLinearAlignedDim) or (EntityState = etLinearVerticalDim) or
       (EntityState = etLinearHorizontalDim)  then
    begin
       {VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1,pY1);
       VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2,pY2);
       VP.LineClipSurface(pX1, pY1, pX2, pY2);
       if VP.Clip then inside:=VP.LineClipSurface(pX1, pY1, pX2, pY2);
       if inside then DrawLinearDimension(VP, Canvas, pX1, pY1, pX2, pY2, Title);}
       DrawLinearDimension(VP, Canvas);
    end;
    if (EntityState = etParallelLine) or (EntityState = etParallelLineHor) or
       (EntityState = etParallelLineVer)
    then
    begin
       {  DrawParallelLine(VP, Canvas, FEntityData.Vertice[0].x, FEntityData.Vertice[0].y,
                                      FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, Title); }
         DrawParallelLine(VP, Canvas);
    end;

    if (EntityState = etRadialDim) or (EntityState = etDiameterDim) then
    begin
        (*VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1, pY1);  //cx,cy,r
        VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);                               //poor clip! TODO!
        if VP.Clip then inside:= VP.LineClipSurface(pX1, pY1, pX2, pY2);//TODO: more poor clip on others directions
        if inside then DrawRadialDimension(VP, Canvas, pX1,pY1, Abs(pX2-pX1), Title); *)
        DrawRadialDimension(VP, Canvas);
    end;
    if EntityState = etArcDim then
    begin
       (* VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1, pY1);
        VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);
        VP.WorldToSurfaceXY(FEntityData.Vertice[2].x, FEntityData.Vertice[2].y, pX3, pY3);

        //if VP.Clip then inside:= VP.LineClipSurface(pX1, pY1, pX2, pY2);
        //if inside then  *)
        //DrawArcDimension(VP, Canvas,pX1, pY1, pX2, pY2, pX3,pY3, Title);
        DrawArcDimension(VP, Canvas);
    end;
    if EntityState = etPolyline then
    begin
     (* count:= High(FEntityData.Vertice)+1;
      for i:=0 to count-1 do
      begin
          L:= GetLineFromIndex(FEntityData.Vertice, i);
          GetVerticesFromLine(L, v1, v2);
          VP.WorldToSurfaceXY(v1.x, v1.y, pX1, pY1);
          VP.WorldToSurfaceXY(v2.x, v2.y, pX2, pY2);
          VP.LineClipSurface(pX1, pY1, pX2, pY2);
          if VP.Clip then inside:=VP.LineClipSurface(pX1, pY1, pX2, pY2);
          if inside then DrawLine(Canvas, pX1, pY1, pX2, pY2, Title);

      end; *)
      DrawPolyline(VP,Canvas);
    end;
    if EntityState = etCircle then {x1,y1,x2,y2=y1} //NOTE: r= Abs(x2-x1);
    begin
      (* VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1, pY1);  //cx,cy,r
       VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);
                                //poor clip! TODO!
       if VP.Clip then inside:= VP.LineClipSurface(pX1, pY1, pX2, pY2);//TODO: more poor clip...
                                                                      //TODO: others directions
       if inside then DrawCircle(Canvas, pX1,pY1, Abs(pX2-pX1), Title); *)
       DrawCircle(VP, Canvas);
    end;
    if EntityState = etArc then
    begin
        (*  VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1, pY1);
          VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);
          VP.WorldToSurfaceXY(FEntityData.Vertice[2].x, FEntityData.Vertice[2].y, pX3, pY3);
          VP.LineClipSurface(pX1, pY1, pX2, pY2);
          if VP.Clip then inside:=VP.LineClipSurface(pX1, pY1, pX2, pY2);
          DrawArc(Canvas, pX1, pY1, pX2, pY2,pX3, pY3, Title);    *)
          DrawArc(VP, Canvas);
    end;
    if EntityState = etZoomCapture then
    begin
        VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1, pY1);
        VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);
       { VP.LineClipSurface(pX1, pY1, pX2, pY2);
        if VP.Clip then inside:=VP.LineClipSurface(pX1, pY1, pX2, pY2); }
        Canvas.Pen.Style:= psDash; //psDot; //psDashDot;
        saveColor:= Canvas.Pen.FPColor;
        Canvas.Pen.FPColor:= colYellow; //0000ff;
        Canvas.Rectangle(pX1,pY1,pX2,pY2);
        Canvas.Pen.Style:=psSolid;
        Canvas.Pen.FPColor:= saveColor;
    end;
    if EntityState = etConnectPoly then
    begin
         VP.WorldToSurfaceXY(FEntityData.Vertice[0].x, FEntityData.Vertice[0].y, pX1, pY1);  //cx,cy,r
         VP.WorldToSurfaceXY(FEntityData.Vertice[1].x, FEntityData.Vertice[1].y, pX2, pY2);
                               //poor clip!
         //if VP.Clip then inside:= VP.LineClipSurface(pX1, pY1, pX2, pY2);//TODO: more poor clip on others directions
         Canvas.Pen.Style:= psDash; //psDot; //psDashDot;
         saveColor:= Canvas.Pen.FPColor;
         Canvas.Pen.FPColor:= colYellow; //0000ff;
         Canvas.EllipseC(pX1,pY1,5,5); //x2,y2);
         Canvas.Pen.Style:=psSolid;
         Canvas.Pen.FPColor:= saveColor;
    end;
end;

procedure TEntity.ResetPoints(V: array of TRealPoint);
var
   i: integer;
   count: integer;
begin
   SetLength(FEntityData.Vertice,0);
   count:= High(V) + 1;
   SetLength(FEntityData.Vertice, count);
   for i:= 0 to count - 1 do
   begin
      FEntityData.Vertice[i].x:= V[i].x;
      FEntityData.Vertice[i].y:= V[i].y;
   end;
end;

constructor TEntity.Create(layerName: string; entityType: TEntityState; V: array of TRealPoint;
                           sTitle, sTag: string);
var
   i: integer;
   count: integer;
begin
   FEntityData:= TEntityData.Create;

   EntityState:= entityType;

   FEntityData.Title:= sTitle;
   FEntityData.TagLabel:= sTag;

   if layerName= '' then FEntityData.Layer:='0'
   else FEntityData.Layer:= layerName;

   count:= High(V) + 1;
   SetLength(FEntityData.Vertice, count);

   for i:= 0 to count - 1 do
   begin
      FEntityData.Vertice[i].x:= V[i].x;
      FEntityData.Vertice[i].y:= V[i].y;
   end;

end;

destructor TEntity.Destroy;
begin
  FEntityData.Free;
  inherited Destroy;
end;

{TFCLImageBridge}

constructor TFCLImageBridge.Create(W, H: integer);
begin
  Width:= W;
  Height:= H;
  FPngImgReader:= TFPReaderPNG.Create;
  FPngImgWriter := TFPWriterPNG.Create;
  FImgMem := TFPMemoryImage.Create(Width,Height);
  FImgMem.UsePalette:= False;
  Canvas := TFPImageCanvas.Create(FImgMem);
  Canvas.Brush.FPColor:= ToTFPColor(colbrLavender);
  Canvas.Pen.FPColor:= ToTFPColor(colbrLavender);
  Canvas.Pen.Mode:= pmCopy;
  Canvas.Pen.Style:= psSolid;
  Canvas.Brush.Style:= bsSolid;
  Canvas.FillRect(0,0,Width,Height);
  Canvas.Brush.Style:= bsClear;
  Canvas.Pen.Width:= 1;
  Canvas.Pen.FPColor:= colRed;

  FFreeTypeFont:=TFreeTypeFont.Create; //On Windows install: zlib1.dll, freetype6.dll -->Rename to: freetype-6.dll
  FFreeTypeFont.Resolution:= 72; { or default resolution [97]  ??}
  //FFreeTypeFont.AntiAliased:= True;

  FFreeTypeFont.FontIndex:= 0;
  FFreeTypeFont.Size:= 8;
  FFreeTypeFont.FPColor:= colBlack;
end;

destructor TFCLImageBridge.Destroy;
begin
   FPngImgReader.Free;
   FPngImgWriter.Free;
   Canvas.Free;
   FImgMem.Free;
   FFreeTypeFont.Free;
   inherited Destroy;
end;


(*
Canvas.Font := TFreeTypeFont.create;
FontMgr.SearchPath := '/usr/share/fonts/TTF/';

'/system/fonts/Roboto-Regular.ttf'         //android
'C:\Windows\Fonts\Arial.ttf'               //windows
'/usr/share/fonts/TTF/Arial.ttf'            //linux
*)

procedure TFCLImageBridge.SetFont(pathToFile: string);
begin
   FTfont.InitEngine;
   Canvas.Font:= FFreeTypeFont;
   Canvas.Font.Name:=pathToFile;
end;

function TFCLImageBridge.FPColorToRGBA(Const Color : TFPColor) : TColorRGBA;
begin
  with Result,Color do
  begin
    R:=(Red   and $FF00) shr 8;
    G:=(Green and $FF00) shr 8;
    B:=(Blue  and $FF00) shr 8;
    A:=(Alpha and $FF00) shr 8;
  end;
end;

 function TFCLImageBridge.GetRGBAGraphics(const buffer: PByte): boolean;
 var
   i, col, row: integer;
   rgba: TColorRGBA;
 begin
   Result:= True;
   try
     i:=0;
     for row:= 0 to FImgMem.Height-1 do
     begin
       for col:= 0 to FImgMem.Width-1 do
       begin
         rgba:= FPColorToRGBA(FImgMem.Colors[col,row]);
         buffer[i*4]:=   rgba.R;
         buffer[i*4+1]:= rgba.G;
         buffer[i*4+2]:= rgba.B;
         buffer[i*4+3]:= rgba.A; //-1, that's the alpha.
         inc(i);
       end;
     end;
   except
     Result:= False;
   end;
 end;

function TFCLImageBridge.GetRGBAGraphics(const buffer: PJByte): boolean;
begin
    Result:= Self.GetRGBAGraphics(PByte(buffer));
end;

procedure TFCLImageBridge.SetSize(W, H: integer);
var
   savePenColor: TFPColor;
   saveBrushColor: TFPColor;
   savePenWidth: integer;
begin
   Width:= W;
   Height:= H;
   savePenWidth:= Canvas.Pen.Width;
   savePenColor:= Canvas.Pen.FPColor;
   saveBrushColor:= Canvas.Brush.FPColor;
   Canvas.Free;
   FImgMem.Free;
   FImgMem := TFPMemoryImage.Create(Width,Height);
   FImgMem.UsePalette:= False;
   Canvas := TFPImageCanvas.Create(FImgMem);
   Canvas.Pen.Mode:= pmCopy;
   Canvas.Pen.Style:= psSolid;
   Canvas.Pen.FPColor:= saveBrushColor;
   Canvas.Brush.FPColor:= saveBrushColor;
   Canvas.Brush.Style:= bsSolid;
   Canvas.FillRect(0,0,Width,Height);
   Canvas.Brush.Style:= bsClear;
   Canvas.Pen.FPColor:= savePenColor;
   Canvas.Pen.Width:= savePenWidth;
   Canvas.Font:= FFreeTypeFont;
end;

procedure TFCLImageBridge.SetSize(backgroundPNGFile: string);
var
   savePenColor: TFPColor;
   saveBrushColor: TFPColor;
   savePenWidth: integer;
begin
 if backgroundPNGFile <> '' then
 begin
   savePenWidth:= Canvas.Pen.Width;
   savePenColor:= Canvas.Pen.FPColor;
   saveBrushColor:= Canvas.Brush.FPColor;
   Canvas.Free;
   FImgMem.Free;
   FImgMem.LoadFromFile(backgroundPNGFile, FPngImgReader);
   Width:= FImgMem.Width;
   Height:= FImgMem.Height;
   FImgMem.UsePalette:= False;
   Canvas := TFPImageCanvas.Create(FImgMem);
   Canvas.Pen.FPColor:= savePenColor;
   Canvas.Brush.FPColor:= saveBrushColor;
   Canvas.Brush.Style:= bsClear;
   Canvas.Pen.Width:= savePenWidth;
   Canvas.Font:= FFreeTypeFont;
 end;
end;

procedure TFCLImageBridge.CopyToCanvas(ShiftX,ShiftY: integer; ACanvas: TFPCustomCanvas);
begin
  ACanvas.CopyRect(0,0,Self.Canvas,Rect(ShiftX,ShiftY, Self.Canvas.Width,Canvas.Height));
end;

procedure TFCLImageBridge.CopyFromCanvas(ShiftX,ShiftY: integer; ACanvas: TFPCustomCanvas);
begin
  Self.Canvas.CopyRect(ShiftX,ShiftY,ACanvas,Rect(0,0,ACanvas.Width,ACanvas.Height));
end;

procedure TFCLImageBridge.CopyFromCanvas(ACanvas: TFPCustomCanvas);
begin
  Self.Canvas.CopyRect(0,0,ACanvas,Rect(0,0,ACanvas.Width,ACanvas.Height));
end;

procedure TFCLImageBridge.SaveToFilePNG(path: string);
begin
  FImgMem.SaveToFile(path,FPngImgWriter);
end;

procedure TFCLImageBridge.LoadFromFilePNG(path: string);
begin
  FImgMem.LoadFromFile(path, FPngImgReader);
end;

{TFPNoGUIGraphicsBridge}

(*
'/system/fonts/Roboto-Regular.ttf'   //android
'C:\Windows\Fonts\Arial.ttf'         //windows
'/usr/share/fonts/TTF/Arial.ttf'     //linux
*)

procedure TFPNoGUIGraphicsBridge.SetPathToFontFile(pathToFile: string);
begin
   Surface.SetFont(pathToFile);
end;

procedure TFPNoGUIGraphicsBridge.CopyToCanvas(Canvas: TFPCustomCanvas);
begin
   Surface.CopyToCanvas(FViewPort.XLeft,FViewPort.YTop,Canvas);
end;

procedure TFPNoGUIGraphicsBridge.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
      if AComponent = FViewPort then
      begin
        FViewPort:= nil;
      end
      else if AComponent = FDXFWriteBridge then
      begin
        FDXFWriteBridge:= nil;
      end;
  end;
end;

procedure TFPNoGUIGraphicsBridge.SetViewPort(AValue: TViewPort);
begin
  if AValue <> FViewPort then
   begin
      if Assigned(FViewPort) then
      begin
         FViewPort.RemoveFreeNotification(Self); //remove free notification...
      end;
      FViewPort:= AValue;
      if AValue <> nil then  //re- add free notification...
      begin
         AValue.FreeNotification(self);
      end;
   end;
end;


procedure TFPNoGUIGraphicsBridge.SetDXFWriteBridge(AValue: TFPDxfWriteBridge);
begin
  if AValue <> FDXFWriteBridge then
   begin
      if Assigned(FDXFWriteBridge) then
      begin
         FDXFWriteBridge.RemoveFreeNotification(Self); //remove free notification...
      end;
      FDXFWriteBridge:= AValue;
      if AValue <> nil then  //re- add free notification...
      begin
         AValue.FreeNotification(self);
      end;
   end;
end;

procedure TFPNoGUIGraphicsBridge.DoDesignFunction(x: real; out y:real; out skip: boolean);
begin
   y:=0;
   skip:= False;
   if Assigned(FOnDesignFunction) then FOnDesignFunction(x,y,skip);
end;

function TFPNoGUIGraphicsBridge.AddEntity(AEntity: TEntity): integer;
begin
  Result:= -1;
  if EntityList <> nil then
  begin
     EntityList.Add(AEntity);
     Result:= EntityList.Count - 1;
  end;
end;

function TFPNoGUIGraphicsBridge.AddEntity(layerName: string; entity: TEntityState; V: array of TRealPoint; sTitle, sTag: string): integer;
var
  objEntity : TEntity;
begin
  Result:= -1;
  if EntityList <> nil then
  begin
     objEntity := TEntity.Create(layerName, entity, V, sTitle, sTag);

     //override defaults
     objEntity.FEntityData.DimExtensionLineGAP:= FEntityData.DimExtensionLineGAP;

     objEntity.FEntityData.DimArrowSize:= FEntityData.DimArrowSize;
     objEntity.FEntityData.DimArrowWidth:= FEntityData.DimArrowWidth;
     objEntity.FEntityData.DimLineOffset:= FEntityData.DimLineOffset;

     objEntity.FEntityData.DimTextHeight:= FEntityData.DimTextHeight;

     objEntity.FEntityData.LineToggleSide:= FEntityData.LineToggleSide;
     objEntity.FEntityData.Offset:= FEntityData.Offset;
     objEntity.FEntityData.TagX:= FEntityData.TagX;
     objEntity.FEntityData.TagY:= FEntityData.TagY;
     objEntity.FEntityData.TagLabel:= FEntityData.TagLabel;
     EntityList.Add(objEntity);

     Result:= EntityList.Count - 1;
  end;
end;

(*  file format example:
id#circle;layer#0;title#this is a circle;x#0.00;y#1.1;r#0.55
id#line;layer#0;title#this is a line;x#0.10 -0.50;y#1.1 4.15
id#polygon;layer#0;title#this is a polygon;x#0.15 0.65 1.1 0.65;y#0.50 0.00 0.50 1.00
id#text;layer#0;title#this is a text;x#0.80;y#1.00
id#point;layer#0;title#this is a point;x#0.80;y#1.00
*)

function TFPNoGUIGraphicsBridge.AddEntity(stringData: string; sTag: string): integer;
var
  strX0,strY0, strX,strY: string;
  V: array of TRealPoint;
  dataList, listX, listY: TStringList;
  title, layerName: string;
  i: integer;
begin
  dataList:= TStringList.Create;
  dataList.Delimiter:= Self.EntityData.FieldSeparator; //';';
  dataList.StrictDelimiter:= True;
  dataList.DelimitedText:= stringData;
  if Pos('circle', dataList.Strings[0]) > 0 then //id#line;layer#0;title#this is a circle;x#0.10 -0.50;y#1.1 4.15
  begin
    layerName:= dataList.Strings[1];
    SplitStr(layerName,Self.EntityData.NameValueSeparator); //'#'
    title:= dataList.Strings[2];
    SplitStr(title,Self.EntityData.NameValueSeparator); //'#'

    strX:= dataList.Strings[3]; //x#0.00 -0.50
    splitStr(strX,Self.EntityData.NameValueSeparator);//0.00 -0.50 '#'
    strX0:= SplitStr(strX, ' ');

    strY:= dataList.Strings[4]; //y#1.00 4.00
    splitStr(strY,Self.EntityData.NameValueSeparator);//1.00 4.00
    strY0:= SplitStr(strY, ' ');  //'#'

    Result:= AddEntity(layerName, etCircle,[ToRealPoint(StrToFloat(strX0){left},StrToFloat(strY0){top}),
                                          ToRealPoint( StrToFloat(strX){right}, StrToFloat(strY){bottom})],title,sTag);

  end;

  if Pos('line', dataList.Strings[0]) > 0 then //id#line;layer#0;title#this is a line;x#0.10 -0.50;y#1.1 4.15
  begin                                        //id#line;x#0.10 -0.50;y#1.1 4.15

    layerName:= dataList.Strings[1];
    SplitStr(layerName,Self.EntityData.NameValueSeparator);
    title:= dataList.Strings[2];
    SplitStr(title,Self.EntityData.NameValueSeparator);

    strX:= dataList.Strings[3]; //x#0.00 -0.50
    splitStr(strX,Self.EntityData.NameValueSeparator);//0.00 -0.50
    strX0:= SplitStr(strX, ' ');

    strY:= dataList.Strings[4]; //y#1.00 4.00
    splitStr(strY,Self.EntityData.NameValueSeparator);//1.00 4.00
    strY0:= SplitStr(strY, ' ');

    Result:= AddEntity(layerName, etLine,[ToRealPoint(StrToFloat(strX0),StrToFloat(strY0)),
                                          ToRealPoint( StrToFloat(strX), StrToFloat(strY))],title,sTag);

    end;

  if Pos('point', dataList.Strings[0]) > 0 then //id#point;layer#0;title#this is a point;x#0.80;y#1.00
  begin
      layerName:= dataList.Strings[1];
      SplitStr(layerName,Self.EntityData.NameValueSeparator);
      title:= dataList.Strings[2];
      SplitStr(title,Self.EntityData.NameValueSeparator);
      strX:= dataList.Strings[3];
      splitStr(strX,Self.EntityData.NameValueSeparator);
      strY:= dataList.Strings[4];
      splitStr(strY,Self.EntityData.NameValueSeparator);
      Result:= AddEntity(layerName, etPoint, [ToRealPoint(StrToFloat(strX),StrToFloat(strY))],title,sTag);
  end;

  if Pos('text', dataList.Strings[0]) > 0 then //id#text;layer#0;title#this is a text;x#0.80;y#1.00
  begin
       layerName:= dataList.Strings[1];
       SplitStr(layerName,Self.EntityData.NameValueSeparator);
       title:= dataList.Strings[2];
       SplitStr(title,Self.EntityData.NameValueSeparator);

       strX:= dataList.Strings[3];
       splitStr(strX,Self.EntityData.NameValueSeparator);
       strY:= dataList.Strings[4];
       splitStr(strY,Self.EntityData.NameValueSeparator);
       Result:= AddEntity(layerName, etText, [ToRealPoint(StrToFloat(strX),StrToFloat(strY))],title,sTag);
  end;
  if Pos('polygon',dataList.Strings[0]) > 0 then //id#polygon;layer#0;title#this is a polygon;x#0.15 0.65 1.1 0.65;y#0.50 0.00 0.50 1.00
  begin
      layerName:= dataList.Strings[1];
      SplitStr(layerName,Self.EntityData.NameValueSeparator);
      title:= dataList.Strings[2];
      SplitStr(title,Self.EntityData.NameValueSeparator);

      listX:= TStringList.Create;
      listY:= TStringList.Create;
      listX.Delimiter:=' ';
      listX.StrictDelimiter:= True;
      listY.Delimiter:=' ';
      listY.StrictDelimiter:= True;

      strX:= dataList.Strings[3]; //x#0.00 0.50 1.00 0.50
      splitStr(strX,Self.EntityData.NameValueSeparator);         //0.00 0.50 1.00 0.50
      listX.DelimitedText:=strX;

      strY:= dataList.Strings[4]; //y#0.50 0.00 0.50 1.00
      splitStr(strY,Self.EntityData.NameValueSeparator);         //0.50 0.00 0.50 1.00
      listY.DelimitedText:=StrY;

      SetLength(V,listX.Count);
      for i:=0  to listX.Count-1 do
         V[i]:= ToRealPoint(StrToFloat(listX.Strings[i]),StrToFloat(listY.Strings[i]));

      Result:= AddEntity(layerName, etPolyline, V,title, '');
      listX.Free;
      listY.Free;
  end;
  dataList.Free;
end;

procedure TFPNoGUIGraphicsBridge.LoadEntitiesFromFile(fileName: string);
var
  listEntity: TStringList;
  i: integer;
begin
  listEntity:= TStringList.Create;
  listEntity.LoadFromFile(fileName);
  for i:=0 to listEntity.Count-1 do
  begin
     if listEntity.Strings[i] <> '' then AddEntity(listEntity.Strings[i],'');
  end;
  listEntity.Free;
end;

procedure TFPNoGUIGraphicsBridge.SaveEntitiesToFile(fileName: string);
var
  listEntity: TStringList;
  i: integer;
begin
   listEntity:= TStringList.Create;
   for i:=0 to Self.EntityList.Count do
   begin
     listEntity.Add(GetEntity(i).DataToString(Self.EntityData.FieldSeparator, Self.EntityData.NameValueSeparator));
   end;
   listEntity.SaveToFile(fileName);
   listEntity.Free;
end;

function TFPNoGUIGraphicsBridge.AddEntity(layerName, entityName: string; V: array of TRealPoint; sTitle, sTag: string): integer;
var
   entType: TEntityState;
begin
   if CompareText('Line',entityName) = 0 then entType:= etLine
   else if CompareText('Text',entityName) = 0 then entType:= etText
   else if CompareText('Rectangle',entityName) = 0 then entType:= etRectangle
   else if CompareText('Circle',entityName) = 0 then entType:= etCircle
   else if CompareText('Arc',entityName) = 0 then entType:= etArc
   else if CompareText('Point',entityName) = 0 then entType:= etPoint
   else if CompareText('Polyline',entityName) = 0 then entType:= etPolyline
   else if CompareText('LinearAlignedDim',entityName) = 0 then entType:= etLinearAlignedDim
   else if CompareText('LinearVerticalDim',entityName) = 0 then entType:= etLinearVerticalDim
   else if CompareText('LinearHorizontalDim',entityName) = 0 then entType:= etLinearHorizontalDim
   else if CompareText('RadialDim',entityName) = 0 then entType:= etLinearAlignedDim
   else if CompareText('DiameterDim',entityName) = 0 then entType:= etDiameterDim
   else if CompareText('ArcDim',entityName) = 0 then entType:= etArcDim
   else if CompareText('AngleDim',entityName) = 0 then entType:= etAngleDim
   else if CompareText('ParallelLine',entityName) = 0 then entType:= etParallelLine
   else if CompareText('ParallelLineHor',entityName) = 0 then entType:= etParallelLineHor
   else if CompareText('ParallelLineVer',entityName) = 0 then entType:= etParallelLineVer
   else if CompareText('HorizontalLine',entityName) = 0 then entType:= etHorizontalLine
   else if CompareText('VerticalLine',entityName) = 0 then entType:= etVerticalLine
   else if CompareText('ExtendLine',entityName) = 0 then entType:= etExtendLine
   else if CompareText('TrimLine',entityName) = 0 then entType:= etTrimLine
   else if CompareText('Arrow',entityName) = 0 then entType:= etArrow;
   Result:= AddEntity(layerName, entType, V, sTitle, sTag);
end;

function TFPNoGUIGraphicsBridge.AddFunction(F: TFX; xmin, xmax: real): integer;
var
  objFunction: TObject;
begin
  Result:= -1;
  if FunctionList <> nil then
  begin
     objFunction := TFunction.Create(F, xmin, xmax);
     FunctionList.Add(objFunction);
     Result:= FunctionList.Count - 1;
  end;
end;

function TFPNoGUIGraphicsBridge.AddFunction(Pts: array of TRealPoint): integer;
var
  objFunction: TObject;
begin
  Result:= -1;
  if FunctionList <> nil then
  begin
     objFunction := TFunction.Create(Pts);
     FunctionList.Add(objFunction);
     Result:= FunctionList.Count - 1;
  end;
end;

procedure TFPNoGUIGraphicsBridge.ClearFunctionList;
var
    i: integer;
begin
    if FunctionList <> nil then
    begin
      for i := 0 to FunctionList.Count-1 do
      begin
          TFunction(FunctionList.Items[i]).Free;    //empty list
      end;
      FunctionList.Free;
      FunctionList := nil;
    end;
    FunctionList:= TList.Create;
end;

procedure TFPNoGUIGraphicsBridge.ClearEntityList;
var
    i: integer;
begin
    if EntityList <> nil then
    begin
      for i := 0 to EntityList.Count-1 do
      begin
          TEntity(EntityList.Items[i]).Free;    //empty list
      end;
      EntityList.Free;
      EntityList := nil;
    end;
    EntityList:= TList.Create;
end;

procedure TFPNoGUIGraphicsBridge.ClearEntityListByLayer(layer: string);
var
    i: integer;
begin
    if EntityList <> nil then
    begin
      i:= 0;
      while i < EntityList.Count do
      begin
         if CompareText(layer, TEntity(EntityList.Items[i]).FEntityData.Layer) = 0 then
         begin
            TEntity(EntityList.Items[i]).Free;
            EntityList.Delete(i);    //empty list
         end
         else inc(i);
      end;
    end;
end;

procedure TFPNoGUIGraphicsBridge.ClearSurface;
var
   saveColor: TFPColor;
begin
    saveColor:= Surface.Canvas.Brush.FPColor;
    Surface.Canvas.Brush.Style := bsSolid;
    Surface.Canvas.Brush.FPColor := ToTFPColor(FViewPort.BackGroundColor);
    Surface.Canvas.FillRect(0, 0, Surface.Width, Surface.Height);
    Surface.Canvas.Brush.Style := bsClear;
    Surface.Canvas.Brush.FPColor:= saveColor;
end;

procedure TFPNoGUIGraphicsBridge.ClearSurfaceByViewPort(VP: TViewPort);
var
   saveColor: TFPColor;
begin
   saveColor:= Surface.Canvas.Brush.FPColor;
   Surface.Canvas.Brush.Style:= bsSolid;
   Surface.Canvas.Brush.FPColor:= ToTFPColor(VP.BackGroundColor);
   Surface.Canvas.FillRect(VP.XLeft,VP.YTop, VP.XLeft+ VP.Width, VP.YTop+ VP.Height);
   Surface.Canvas.Brush.Style:= bsClear;
   Surface.Canvas.Brush.FPColor:= saveColor;
end;

procedure TFPNoGUIGraphicsBridge.DeleteEntity(index: integer);
begin
    if (index >= 0) and (index < EntityList.Count) then
    begin
      TEntity(EntityList.Items[index]).Free;
      EntityList.Delete(index);
    end;
end;

function TFPNoGUIGraphicsBridge.GetEntity(index: integer): TEntity;
var
  count: integer;
begin
  if (EntityList <> nil) and (index > -1) then
  begin
      count:= EntityList.Count;
      if index < count then Result:= TEntity(EntityList.Items[index]);
  end;
end;

function TFPNoGUIGraphicsBridge.GetFunction(index: integer): TFunction;
begin
   if index >= 0 then
   begin
      if FunctionList <> nil then
        if (FunctionList.Count > 0) and (index <= (FunctionList.Count-1) )then
          Result:= TFunction(FunctionList.Items[index]);
   end;
end;

function TFPNoGUIGraphicsBridge.GetEntity(VP:TViewPort; X,Y: integer; out index: integer): TEntity;
var
   i, count, saveIndex:integer;
   dist, saveDist,pmx, pmy: real;
   objEntity: TEntity;
   wX, wY: real;
begin
   Result:= nil;
   wX:= VP.SurfaceToWorldX(X);
   wY:= VP.SurfaceToWorldY(Y);
   saveDist:= -1;
   saveIndex:= -1;
   index:= -1; {out}
   if EntityList <> nil then
   begin
     count:= EntityList.Count;
     if count > 0 then
     begin
       objEntity := GetEntity(0);
       objEntity.MiddlePoint(pmx,pmy);
       saveDist:= GetDistanceBetweenTwoPoints(pmx,pmy, wX, wY);
       saveIndex:=0;
       for i:=1 to count-1 do
       begin
           objEntity := GetEntity(i);
           objEntity.MiddlePoint(pmx,pmy);
           dist:= GetDistanceBetweenTwoPoints(pmx,pmy, wX, wY);;
           if dist <= saveDist then
           begin
               saveDist:= dist;
               saveIndex:= i;
          end;
       end;
       if saveIndex > -1 then
       begin
          Result:= TEntity(EntityList.Items[saveIndex]);
          index:= saveIndex;
       end
       else Result:= nil;
     end; //count >0
   end; //<> nil
end;

function TFPNoGUIGraphicsBridge.GetEntity(VP:TViewPort; X,Y: integer; out index: integer; layerName: string): TEntity;
var
   i, count, saveIndex:integer;
   dist, saveDist,pmx, pmy: real;
   objEntity: TEntity;
   wX, wY: real;
begin
   Result:= nil;
   wX:= VP.SurfaceToWorldX(X);
   wY:= VP.SurfaceToWorldY(Y);
   saveIndex:= -1;
   index:= -1; {out}
   if EntityList <> nil then
   begin
     count:= EntityList.Count;
     if count > 0 then
     begin
       for i:=0 to count-1 do
       begin
           objEntity:= GetEntity(i);
           if CompareText(objEntity.FEntityData.Layer, layerName) = 0 then
           begin
               objEntity.MiddlePoint(pmx,pmy);
               dist:= GetDistanceBetweenTwoPoints(pmx,pmy, wX, wY);
               if saveIndex < 0 then
               begin
                  saveDist:= dist;
                  saveIndex:= i;
               end;
               if dist < saveDist then
               begin
                  saveDist:= dist;
                  saveIndex:= i;
               end;
           end;
       end;
       if saveIndex > -1 then
       begin
          Result:= TEntity(EntityList.Items[saveIndex]);
          index:= saveIndex;
       end
       else Result:= nil;
     end; //count >0
   end; //<> nil
end;

function TFPNoGUIGraphicsBridge.DrawEntity(Entity: TEntityState; V: array of TRealPoint; sTitle, sTag: string): integer;
var
   objEntity : TEntity;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   saveFontSize: integer;
begin
   saveFontSize:= Surface.FreeTypeFont.Size;
   saveColorPen:= Surface.Canvas.Pen.FPColor;
   saveThickness:= Surface.Canvas.Pen.Width;

   Surface.FreeTypeFont.Size:= FViewPort.FontSize;
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor);
   Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

   objEntity:= TEntity.Create('', Entity, V, sTitle, sTag);
   objEntity.Draw(FViewPort, Surface.Canvas);
   objEntity.FEntityData.Layer:= FEntityData.Layer;
   AddEntity(objEntity);
   Result:= EntityList.Count -1;

   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
   Surface.FreeTypeFont.Size:= saveFontSize;
end;


function TFPNoGUIGraphicsBridge.DrawEntity(layerName , entityName: string; V: array of TRealPoint; sTitle, sTag: string): integer;
var
   objEntity : TEntity;
   entType: TEntityState;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   saveFontSize: integer;
begin
   saveFontSize:= Surface.FreeTypeFont.Size;
   saveColorPen:= Surface.Canvas.Pen.FPColor;
   saveThickness:= Surface.Canvas.Pen.Width;

   Surface.FreeTypeFont.Size:= FViewPort.FontSize;
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor);
   Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

   if CompareText('Line',entityName) = 0 then entType:= etLine
   else if CompareText('Text',entityName) = 0 then entType:= etText
   else if CompareText('Rectangle',entityName) = 0 then entType:= etRectangle
   else if CompareText('Circle',entityName) = 0 then entType:= etCircle
   else if CompareText('Arc',entityName) = 0 then entType:= etArc
   else if CompareText('Point',entityName) = 0 then entType:= etPoint
   else if CompareText('Polyline',entityName) = 0 then entType:= etPolyline
   else if CompareText('LinearAlignedDim',entityName) = 0 then entType:= etLinearAlignedDim
   else if CompareText('LinearVerticalDim',entityName) = 0 then entType:= etLinearVerticalDim
   else if CompareText('LinearHorizontalDim',entityName) = 0 then entType:= etLinearHorizontalDim
   else if CompareText('etRadialDim',entityName) = 0 then entType:= etLinearAlignedDim
   else if CompareText('DiameterDim',entityName) = 0 then entType:= etDiameterDim
   else if CompareText('ArcDim',entityName) = 0 then entType:= etArcDim
   else if CompareText('AngleDim',entityName) = 0 then entType:= etAngleDim
   else if CompareText('ParallelLine',entityName) = 0 then entType:= etParallelLine
   else if CompareText('ParallelLineHor',entityName) = 0 then entType:= etParallelLineHor
   else if CompareText('ParallelLineVer',entityName) = 0 then entType:= etParallelLineVer
   else if CompareText('HorizontalLine',entityName) = 0 then entType:= etHorizontalLine
   else if CompareText('VerticalLine',entityName) = 0 then entType:= etVerticalLine
   else if CompareText('ExtendLine',entityName) = 0 then entType:= etExtendLine
   else if CompareText('TrimLine',entityName) = 0 then entType:= etTrimLine
   else if CompareText('Arrow',entityName) = 0 then entType:= etArrow;

   if layerName <> '' then
      objEntity:= TEntity.Create(layerName, entType, V, sTitle, sTag)
   else
      objEntity:= TEntity.Create(FEntityData.Layer, entType, V, sTitle, sTag);

   objEntity.Draw(FViewPort, Surface.Canvas);
   AddEntity(objEntity);
   Result:= EntityList.Count - 1;

   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
   Surface.FreeTypeFont.Size:= saveFontSize;
end;

function TFPNoGUIGraphicsBridge.DrawEntity(VP: TViewPort; SelectedIndex: integer): TEntity;
var
   i, count: integer;
   objEntity : TEntity;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   saveFontSize: integer;
begin
   saveFontSize:= Surface.FreeTypeFont.Size;
   saveThickness:= Surface.Canvas.Pen.Width;
   saveColorPen:= Surface.Canvas.Pen.FPColor;

   Surface.FreeTypeFont.Size:= FViewPort.FontSize;

   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor);
   Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

   if VP = nil then VP:= FViewPort;
   count:=EntityList.Count;
   if SelectedIndex < 0 then //Draw All
   begin
       for i:=0 to count - 1 do
       begin
             objEntity := GetEntity(i);
             objEntity.Draw(VP, Surface.Canvas);
       end;
   end
   else if SelectedIndex < count then
   begin
       objEntity := GetEntity(SelectedIndex);
       objEntity.Draw(VP, Surface.Canvas);
   end;
   Result:= objEntity;
   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
   Surface.FreeTypeFont.Size:= saveFontSize;
end;


procedure TFPNoGUIGraphicsBridge.DrawEntity(index: integer; bridgeColor: TTFPColorBridge; thickness: integer);
var
   count: integer;
   objEntity : TEntity;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   saveFontSize: integer;
begin
   saveFontSize:= Surface.FreeTypeFont.Size;
   saveColorPen:= Surface.Canvas.Pen.FPColor;
   saveThickness:= Surface.Canvas.Pen.Width;

   Surface.FreeTypeFont.Size:= FViewPort.FontSize;
   Surface.Canvas.Pen.FPColor:= ToTFPColor(bridgeColor);
   Surface.Canvas.Pen.Width:= thickness;

   count:=EntityList.Count;
   if index < count then
   begin
       objEntity := GetEntity(index);
       objEntity.Draw(FViewPort, Surface.Canvas);
   end;

   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
   Surface.FreeTypeFont.Size:= saveFontSize;
end;

procedure TFPNoGUIGraphicsBridge.DrawEntities;
var
   i, count: integer;
   objEntity : TEntity;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   saveFontSize: integer;
begin
   saveFontSize:= Surface.FreeTypeFont.Size;
   saveColorPen:= Surface.Canvas.Pen.FPColor;
   saveThickness:= Surface.Canvas.Pen.Width;

   Surface.FreeTypeFont.Size:= FViewPort.FontSize;
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor);
   Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

   count:=EntityList.Count;
   for i:=0 to count - 1 do
   begin
      objEntity := GetEntity(i);
      objEntity.Draw(FViewPort, Surface.Canvas);
   end;
   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
   Surface.FreeTypeFont.Size:= saveFontSize;
end;

procedure TFPNoGUIGraphicsBridge.DrawEntities(layerName: string);
var
   i, count: integer;
   objEntity : TEntity;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   saveFontSize: integer;
begin
   saveFontSize:= Surface.FreeTypeFont.Size;
   saveColorPen:= Surface.Canvas.Pen.FPColor;
   saveThickness:= Surface.Canvas.Pen.Width;

   Surface.FreeTypeFont.Size:= FViewPort.FontSize;
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor);
   Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

   count:=EntityList.Count;
   for i:=0 to count - 1 do
   begin
      objEntity := GetEntity(i);
      if CompareText(layerName, objEntity.FEntityData.Layer) = 0 then
         objEntity.Draw(FViewPort, Surface.Canvas);
   end;

   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
   Surface.FreeTypeFont.Size:= saveFontSize;
end;


function TFPNoGUIGraphicsBridge.DrawEntity(SelectedIndex: integer): TEntity;
begin
   Result:= DrawEntity(FViewPort, SelectedIndex);
end;

function TFPNoGUIGraphicsBridge.TranslateEntityXY(VP:TViewPort; index: integer; X, Y: integer): TEntity;
var
  objEntity: TEntity;
begin
   if VP = nil then VP:= FViewPort;
   objEntity:= GetEntity(index);
   if objEntity <> nil then
   begin
     objEntity.TranslateByXY(VP, X,Y);
   end;
   Result:= objEntity;
end;

function TFPNoGUIGraphicsBridge.TranslateEntityXY(index: integer; X, Y: integer): TEntity;
begin
    Result:= TranslateEntityXY(FViewPort, index,X, Y);
end;

procedure TFPNoGUIGraphicsBridge.PanY(VP:TViewPort; dy: real);
begin
    VP.MinY:= VP.MinY - dy;
    VP.MaxY:= VP.MaxY - dy;
    VP.ScaleXY(VP.MinX, VP.MaxX, VP.MinY, VP.MaxY);
end;

procedure TFPNoGUIGraphicsBridge.PanXY(VP:TViewPort; X0, Y0, X, Y: integer);
var
   dx,dy: real;
begin
  // dx:= SurfaceToWorldX(X) -  SurfaceToWorldX(X0);
   if X >= X0 then dx:= -0.1 else dx:= 0.1;
   if Y <= Y0 then dy:= -0.1 else dy:= 0.1;
   VP.MinX:= VP.MinX + dx;
   VP.MaxX:= VP.MaxX + dx;
   VP.MinY:= VP.MinY + dy;
   VP.MaxY:= VP.MaxY + dy;
   VP.ScaleXY(VP.MinX, VP.MaxX, VP.MinY, VP.MaxY);
end;

procedure TFPNoGUIGraphicsBridge.PanX(VP:TViewPort; dx: real);
begin
    VP.MinX:= VP.MinX + dx;
    VP.MaxX:= VP.MaxX + dx;
    VP.ScaleXY(VP.MinX, VP.MaxX, VP.MinY, VP.MaxY);
end;

procedure TFPNoGUIGraphicsBridge.DrawFunction(reset: boolean; VP: TViewPort; SelectedIndex: integer);
var
   i: integer;
   count: integer;
   objFunction : TFunction;
   saveColorPen:  TFPColor;
   saveThickness: integer;
begin
   saveColorPen:= Surface.Canvas.Pen.FPColor;
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor); //PenColorEx
   saveThickness:= Surface.Canvas.Pen.Width;
   Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

   if reset then Surface.Canvas.Clear;

   count:= FunctionList.Count;
   if SelectedIndex < 0 then
   begin
       for i:=0 to count-1 do
       begin
             objFunction := GetFunction(i);
             objFunction.Draw(VP, Surface.Canvas);
       end;
   end
   else if SelectedIndex < count then
        begin
             objFunction := GetFunction(SelectedIndex);
             objFunction.Draw(VP, Surface.Canvas);
        end;

   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
end;

procedure TFPNoGUIGraphicsBridge.DrawFunction(reset: boolean; SelectedIndex: integer);
begin
    DrawFunction(reset, FViewPort,SelectedIndex);
end;

procedure TFPNoGUIGraphicsBridge.DrawFunction(reset: boolean; xmin, xmax: real);
var
   wY: real;
   wX: real;
   Y: integer;
   X: integer;
   dx: real;
   inside: boolean;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   skip: boolean;
begin

   dx:= (xmax - xmin)/MAXPOINTS;
   if dx = 0 then exit;

   saveColorPen:= Surface.Canvas.Pen.FPColor;
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor);
   saveThickness:= Surface.Canvas.Pen.Width;
   Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

   if reset then Surface.Canvas.Clear;

   wX:= xmin;
   DoDesignFunction(wX, wY, skip);
   FViewPort.WorldToSurfaceXY(wX, wY, X,Y);
   Surface.Canvas.MoveTo(X,Y);
   wX:= wX + dx;
   while wX <= xmax do
   begin
     DoDesignFunction(wX, wY, skip);
     inside:= FViewPort.WorldToSurfaceXY(wX,wY,X,Y);
     if not FViewPort.Cliping then
     begin
        if (not skip) then
           Surface.Canvas.LineTo(X,Y)
        else
           Surface.Canvas.MoveTo(X,Y)
     end
     else //clip
     begin
        if (not inside) or (skip) then
           Surface.Canvas.MoveTo(X,Y)
        else
           Surface.Canvas.LineTo(X,Y);
     end;
     wX:= wX + dx;
   end;

   Surface.Canvas.Pen.FPColor:= saveColorPen;
   Surface.Canvas.Pen.Width:= saveThickness;
end;

procedure TFPNoGUIGraphicsBridge.DrawFunction(reset: boolean);
var
   wY: real;
   wX, xmax, xmin: real;
   Y: integer;
   X: integer;
   dx: real;
   inside: boolean;
   saveColorPen:  TFPColor;
   saveThickness: integer;
   skip: boolean;
begin
     xmax:= FViewPort.MaxX;
     xmin:= FViewPort.MinX;

     dx:= (xmax - xmin)/MAXPOINTS;
     if dx = 0 then exit;

     saveColorPen:= Surface.Canvas.Pen.FPColor;
     Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.PenColor);

     saveThickness:= Surface.Canvas.Pen.Width;
     Surface.Canvas.Pen.Width:= FViewPort.PenThickness;

     if reset then Surface.Canvas.Clear;

     wX:= xmin;
     DoDesignFunction(wX, wY, skip);
     FViewPort.WorldToSurfaceXY(wX, wY, X,Y);
     Surface.Canvas.MoveTo(X,Y);
     wX:= wX + dx;
     while wX <= xmax do
     begin
       DoDesignFunction(wX, wY, skip);
       inside:= FViewPort.WorldToSurfaceXY(wX,wY,X,Y);
       if not FViewPort.Cliping then
       begin
          if (not skip) then
             Surface.Canvas.LineTo(X,Y)
          else
             Surface.Canvas.MoveTo(X,Y);
       end
       else //clip
       begin
          if (not inside) or (skip) then
             Surface.Canvas.MoveTo(X,Y)
          else
             Surface.Canvas.LineTo(X,Y);
       end;
       wX:= wX + dx;
     end;
     Surface.Canvas.Pen.FPColor:= saveColorPen;
     Surface.Canvas.Pen.Width:= saveThickness;
end;

procedure TFPNoGUIGraphicsBridge.PaintGrid(VP: TViewPort; clearscr: boolean);
var
   dk, dn, dn1: real;
   tw, th, tws, X,Y: integer;
   savePenColor, saveFontColor, saveBrushColor: TFPColor;
   insideX, insideY: boolean;
   titX, titY: string;
   saveBrushStyle: TFPBrushStyle;
   saveThickness: integer;
   saveFontSize: integer;
begin

   if VP = nil then VP:= FViewPort;

   titX:= FViewPort.GridData.XTitle;
   titY:= FViewPort.GridData.YTitle;

   saveFontSize:= Surface.FreeTypeFont.Size;
   saveFontColor:= Surface.Canvas.Font.FPColor;
   saveThickness:= Surface.Canvas.Pen.Width;
   savePenColor:= Surface.Canvas.Pen.FPColor;

   saveBrushStyle:= Surface.Canvas.Brush.Style;
   saveBrushColor:= Surface.Canvas.Brush.FPColor;

   if clearscr then
   begin
     Surface.Canvas.Brush.Style:= bsSolid;
     Surface.Canvas.Brush.FPColor:= ToTFPColor(FViewPort.GridData.BackGroundColor);
     Surface.Canvas.FillRect(VP.XLeftGrid,VP.YTopGrid, VP.XLeftGrid+ VP.WidthGrid, VP.YTopGrid+ VP.HeightGrid);
   end;
   Surface.Canvas.Brush.FPColor:= saveBrushColor;

   //frame grid color
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.GridData.Color);
   Surface.Canvas.Brush.Style:= bsClear;
   Surface.Canvas.Rectangle(VP.XLeftGrid,VP.YTopGrid, VP.XLeftGrid+ VP.WidthGrid, VP.YTopGrid+ VP.HeightGrid);

   //grid color
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.GridData.Color);
   Surface.Canvas.Font.FPColor:= ToTFPColor(FViewPort.FontColor);
   Surface.FreeTypeFont.Size:= FViewPort.FontSize;

   tw:= Trunc(Surface.Canvas.GetTextWidth(VP.Title)/2);
   Surface.Canvas.TextOut(VP.XLeftGrid-12,VP.YTopGrid,titY);
   Surface.Canvas.TextOut(VP.XLeftGrid+VP.WidthGrid, VP.YTopGrid+ VP.HeightGrid+4,titX);
   Surface.Canvas.TextOut(VP.XLeft + Trunc(VP.Width/2) - tw, VP.YTop+ Trunc(VP.MarginTop/2) ,VP.Title);

   if FViewPort.DrawGrid then
   begin
       dk:= (VP.MaxX - VP.MinX)/FViewPort.GridData.XInterval;
       dn:= VP.MinX;
       while dn < VP.MaxX do
       begin
           Surface.Canvas.Line(VP.WorldToSurfaceX(dn),VP.WorldToSurfaceY(VP.MinY),
                                 VP.WorldToSurfaceX(dn),VP.WorldToSurfaceY(VP.MaxY));  //grid axisX Verticals lines!
           dn:= dn + dk;
       end;
   end;

   if FViewPort.DrawGrid then
   begin
       dk:= (VP.MaxY - VP.MinY)/FViewPort.GridData.YInterval;
       dn:= VP.MinY;
       while dn < VP.MaxY do   //ticks axis y
       begin
            Surface.Canvas.Line(VP.WorldToSurfaceX(VP.MinX),VP.WorldToSurfaceY(dn),
                                  VP.WorldToSurfaceX(VP.MaxX),VP.WorldToSurfaceY(dn));  //grid axis y Horizontals lines
            dn:= dn + dk;
       end;
   end;

   //axisColor
   Surface.Canvas.Pen.FPColor:= ToTFPColor(FViewPort.GridData.AxisColor);

   if FViewPort.DrawAxis = True then
   begin

       insideX:= VP.WorldToSurfaceXY(VP.MinX,0,X,Y);
       Surface.Canvas.MoveTo(X,Y);

       insideX:= VP.WorldToSurfaceXY(VP.MaxX,0,X,Y);
       if insideX then Surface.Canvas.LineTo(X,Y);  //axisX

       tws:= Surface.Canvas.GetTextWidth('-');
       dk:= (VP.MaxX - VP.MinX)/FViewPort.GridData.XInterval;
       dn:= VP.MinX;

       if insideX then
       begin
            th:= Surface.Canvas.GetTextHeight(FloatToStrF(dn,ffFixed,0,1));
            while dn < 1.01*VP.MaxX do    //ticks and legend axisX
            begin

               Surface.Canvas.MoveTo(VP.WorldToSurfaceX(dn),VP.YTopGrid + VP.HeightGrid+3);
               Surface.Canvas.LineTo(VP.WorldToSurfaceX(dn),VP.YTopGrid + VP.HeightGrid);

               tw:= Surface.Canvas.GetTextWidth(FloatToStrF(dn,ffFixed,0,1));

               if FViewPort.GridData.XLegendInterval > 0 then
               begin
                 dn1:= dn + FViewPort.GridData.XLengendOffsetValue;  //makeup/maquiagem...

                 if dn1 < 0 then
                 begin
                      tws:= Surface.Canvas.GetTextWidth('-');
                      if FViewPort.GridData.XShowLengend then
                         Surface.Canvas.TextOut(VP.WorldToSurfaceX(dn) - Round(tw/2),VP.YTopGrid + VP.HeightGrid+7+th,'-');
                 end else tws:=0;

                 if FViewPort.GridData.XShowLengend then
                    Surface.Canvas.TextOut(VP.WorldToSurfaceX(dn)- Round(tw/2)+ tws,VP.YTopGrid + VP.HeightGrid+7+th,
                                            FloatToStrF(Abs(dn1),ffFixed,0,1));
               end
               else dn:= dn + 1*dk;

               dn:= dn + FViewPort.GridData.XLegendInterval*dk;
            end;
            //dk:= (VP.MaxY - VP.MinY)/FViewPort.GridData.YInterval;
            //dn:= VP.MinY;
       end;

       insideY:= VP.WorldToSurfaceXY(0,VP.MinY,X,Y);
       Surface.Canvas.MoveTo(X,Y);
       insideY:= VP.WorldToSurfaceXY(0,VP.MaxY,X,Y);
       if insideY then Surface.Canvas.LineTo(X,Y);  //axisY

       dk:= (VP.MaxY - VP.MinY)/FViewPort.GridData.YInterval;
       dn:= VP.MinY;
       if insideY then
       begin
            th:= Surface.Canvas.GetTextHeight(FloatToStrF(dn,ffFixed,0,1));
            while dn < 1.01*VP.MaxY do   //ticks and legend axisY
            begin
               Surface.Canvas.MoveTo(VP.XLeftGrid-3,VP.WorldToSurfaceY(dn));
               Surface.Canvas.LineTo(VP.XLeftGrid,VP.WorldToSurfaceY(dn));

               tw:= Surface.Canvas.GetTextWidth(FloatToStrF(dn,ffFixed,0,1))+2;

               if FViewPort.GridData.YLegendInterval > 0 then
               begin
                 dn1:= dn + FViewPort.GridData.YLengendOffsetValue;  //makeup/maquiagem...
                 if dn1 < 0 then
                 begin
                      tws:= Surface.Canvas.GetTextWidth('-');
                      if FViewPort.GridData.YShowLengend then
                         Surface.Canvas.TextOut(VP.XLeftGrid-3-tw-4,VP.WorldToSurfaceY(dn)+Round(th/2),'-')
                 end else tws:=0;

                 if FViewPort.GridData.YShowLengend then
                    Surface.Canvas.TextOut(VP.XLeftGrid-3-tw-4+tws,VP.WorldToSurfaceY(dn)+Round(th/2),
                                            FloatToStrF(Abs(dn1),ffFixed,0,1));
               end
               else dn:= dn + 1*dk;

               dn:= dn + FViewPort.GridData.YLegendInterval*dk;
            end;
       end;
   end;

   Surface.Canvas.Brush.FPColor:= saveBrushColor;
   Surface.Canvas.Brush.Style:= saveBrushStyle;

   Surface.Canvas.Pen.FPColor:= savePenColor;
   Surface.Canvas.Font.FPColor:= saveFontColor;
   Surface.Canvas.Pen.Width:=  saveThickness;
   Surface.FreeTypeFont.Size:= saveFontSize;

end;

procedure TFPNoGUIGraphicsBridge.PaintGrid(clearscr: boolean);
begin
   PaintGrid(FViewPort,clearscr);
end;

procedure TFPNoGUIGraphicsBridge.PaintViewPort(VP: TViewPort);
var
   saveColor: TFPColor;
begin

   saveColor:= Surface.Canvas.Brush.FPColor;
   Surface.Canvas.Brush.Style:= bsSolid;
   Surface.Canvas.Brush.FPColor:= ToTFPColor(FViewPort.BackGroundColor);
   Surface.Canvas.FillRect(VP.XLeft,VP.YTop, VP.XLeft+ VP.Width, VP.YTop+ VP.Height);

   Surface.Canvas.Brush.Style:= bsClear;
   Surface.Canvas.Brush.FPColor:= saveColor;
end;

procedure TFPNoGUIGraphicsBridge.PaintViewPort;
begin
  PaintViewPort(FViewPort);
end;

procedure TFPNoGUIGraphicsBridge.Loaded;
begin
     inherited Loaded;
     { Perform any component setup that depends on the property
       values having been set }
end;

procedure TFPNoGUIGraphicsBridge.SetSize(W,H: integer);
begin
   Surface.SetSize(W,H);
end;

procedure TFPNoGUIGraphicsBridge.SetSurfaceSize(W,H: integer);
begin
   Surface.SetSize(W,H);
end;

procedure TFPNoGUIGraphicsBridge.SetSize(backgroundPNGFile: string);
begin
   Surface.SetSize(backgroundPNGFile);
end;

procedure TFPNoGUIGraphicsBridge.SetSurfaceSize(backgroundPNGFile: string);
begin
   Surface.SetSize(backgroundPNGFile);
end;

procedure TFPNoGUIGraphicsBridge.SetPenColor(colorBridge: TTFPColorBridge);
begin
   Surface.Canvas.Pen.FPColor:= ToTFPColor(colorBridge);
end;

procedure TFPNoGUIGraphicsBridge.SetBackGroundColor(colorBridge: TTFPColorBridge);
begin
   Surface.Canvas.Brush.FPColor:= ToTFPColor(colorBridge);
end;

procedure TFPNoGUIGraphicsBridge.SetFontColor(colorBridge: TTFPColorBridge);
begin
   Surface.Canvas.Font.FPColor:= ToTFPColor(colorBridge);
end;

procedure TFPNoGUIGraphicsBridge.SetFontHeight(fontHeight: integer);
begin
   Surface.Canvas.Font.Size:= fontHeight;
end;

procedure TFPNoGUIGraphicsBridge.SetFontAntiAliased(antiAliased: boolean);
begin
   Surface.FreeTypeFont.AntiAliased:= antiAliased;
end;

procedure TFPNoGUIGraphicsBridge.SetFontResolution(dpiResolution: integer);
begin
   Surface.FreeTypeFont.Resolution:= dpiResolution;
end;

procedure TFPNoGUIGraphicsBridge.SetPenThickness(penThickness: integer);
begin
   Surface.Canvas.Pen.Width:= penThickness;
end;

procedure TFPNoGUIGraphicsBridge.SetWidth(AValue: integer);
var
   saveH: integer;
begin
  if (AValue > 150) and (AValue <> FWidth)  then
  begin
    FWidth:= AValue;
    saveH:= Surface.Height;
    Surface.SetSize(FWidth, saveH);
  end;
end;

procedure TFPNoGUIGraphicsBridge.SetHeight(AValue: integer);
var
   saveW: integer;
begin
  if (AValue > 150)  and (AValue <> FHeight) then
  begin
    FHeight:= AValue;
    saveW:= Surface.Width;
    Surface.SetSize(saveW, FHeight);
  end;
end;

constructor TFPNoGUIGraphicsBridge.Create(AOwner: TComponent);
begin
   inherited Create(AOwner);

   EntityList:= TList.Create;
   FunctionList:= TList.Create;
   FEntityData:= TEntityData.Create;
   FWidth:= 400;
   FHeight:= 400;
   Surface:= TFCLImageBridge.Create(FWidth,FHeight); //dammy
end;

destructor TFPNoGUIGraphicsBridge.Destroy;
var
   i: integer;
begin
   if EntityList <> nil then
   begin
     for i := 0 to EntityList.Count-1 do
     begin
         TEntity(EntityList.Items[i]).Free;
     end;
     EntityList.Free;
     EntityList := nil;
   end;
   if FunctionList <> nil then
   begin
      for i := 0 to FunctionList.Count-1 do
      begin
          TFunction(FunctionList.Items[i]).Free;
      end;
      FunctionList.Free;
      FunctionList:= nil;
   end;
   Surface.Free;
   FEntityData.Free;
   inherited Destroy;
end;

function ReplaceChar(query: string; oldchar, newchar: char):string;
begin
  if query <> '' then
  begin
     while Pos(oldchar,query) > 0 do query[pos(oldchar,query)]:= newchar;
     Result:= query;
  end;
end;

function SplitStr(var theString: string; delimiter: string): string;
var
  i: integer;
begin
  Result:= '';
  if theString <> '' then
  begin
    i:= Pos(delimiter, theString);
    if i > 0 then
    begin
       Result:= Copy(theString, 1, i-1);
       theString:= Copy(theString, i+Length(delimiter), maxLongInt);
    end
    else
    begin
       Result:= theString;
       theString:= '';
    end;
  end;
end;

end.
