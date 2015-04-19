unit ViewPort;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GridData, FPColorBridge{, Forms, Controls, Graphics, ,Dialogs};

const

 CodeBottom = 1; CodeTop    = 2;             {LineClipScreen: BitFields for output codes }
 CodeLeft   = 4; CodeRight  = 8;

type

  TViewPort = class(TComponent)
  private

     FTitle: string;

     FXLeft: integer;
     FYTop: integer;
     FWidth: integer;
     FHeight: integer;

     FLeftMargin: integer;
     FTopMargin: integer;
     FRightMargin: integer;
     FBottomMargin: integer;
     FCliping: boolean;

     FGridData: TGridData;

     FDrawGrid: boolean;
     FDrawAxis: boolean;
     FXLeftGrid: integer;
     FYTopGrid: integer;
     FWidthGrid: integer;
     FHeightGrid: integer;

     FPenThickness: integer;
     FPenColor: TTFPColorBridge;

     FFontHeight: Integer;
     FFontAntiAliased: boolean;
     FFontColor: TTFPColorBridge;
     FBackGroundColor: TTFPColorBridge;

     FOnChangePenThickness: TNotifyEvent;
     FOnChangePenColor: TNotifyEvent;
     FOnChangePenColorEx: TNotifyEvent;
     FOnChangeFontColor: TNotifyEvent;
     FOnChangeFontAntiAliased: TNotifyEvent;
     FOnChangeFontHeight: TNotifyEvent;
     FOnChangeBackGroundColor: TNotifyEvent;

     procedure SetPenThickness(AValue: integer);
     procedure SetPenColor(AValue: TTFPColorBridge);

     procedure SetFontHeight(AValue: Integer);
     procedure SetFontAntiAliased(AValue: boolean);
     procedure SetFontColor(AValue: TTFPColorBridge);
     procedure SetBackGroundColor(AValue: TTFPColorBridge);
     procedure SetGridData(AValue: TGridData);

     procedure SetXLeft(AValue: integer);
     procedure SetYTop(AValue: integer);
     procedure SetWidth(AValue: integer);
     procedure SetHeight(AValue: integer);
     procedure SetLeftMargin(AValue: integer);
     procedure SetTopMargin(AValue: integer);
     procedure SetRightMargin(AValue: integer);
     procedure SetBottomMargin(AValue: integer);
     procedure SetTitle(AValue: string);

     function SurfaceToViewPortY(scrny:integer): integer;
     function ViewPortToWorldY(wpy:integer): real;
     function SurfaceToViewPortX(scrnx:integer): integer;
     function ViewPortToWorldX(wpx:integer): real;
     function WorldToViewPortX(wrldx:real):integer;
     function WorldToViewPortY(wrldy:real):integer;
     function ViewPortToSurfaceX(wpx: integer):integer;
     function ViewPortToSurfaceY(wpy: integer):integer;
     FUNCTION CompOutCode(X, Y : INTEGER) : BYTE;  {Nested function}

   protected
     procedure Notification(AComponent: TComponent; Operation: TOperation); override;
   public

     ScaleX: real;
     ScaleY: real;
     MinX,MaxX,MinY,MaxY: real;

     constructor Create(AOwner: TComponent); override;
     destructor Destroy; override;

     function SurfaceToWorldY(scrny: integer): real;
     function SurfaceToWorldX(scrnx: integer): real;

     function WorldToSurfaceX(wrldx:real):integer;
     function WorldToSurfaceY(wrldy:real):integer;

     procedure ScaleXY(xmin, xmax, ymin, ymax: real);
     procedure SetScaleXY(xmin, xmax, ymin, ymax: real);

     function WorldToSurfaceXY(xworld, yworld: real; out scrnx: integer; out scrny: integer): boolean;
     function SurfaceToWorldXY(scrnx ,scrny: integer; out xworld: real; out yworld: real): boolean;
     function SurfaceToViewPortXY(scrnx, scrny:integer; out xvp: integer; out yvp: integer): boolean;

     function IsInside(P: TPoint): boolean;
     function LineClipSurface(var XVP: integer; var YVP:integer; var X2: integer; var Y2:integer): boolean;
     procedure SetSize(W: integer; H: integer);

     property XLeftGrid: integer read FXLeftGrid;
     property YTopGrid: integer read FYTopGrid;
     property WidthGrid: integer read FWidthGrid;
     property HeightGrid: integer read  FHeightGrid;
   published
     property PenThickness: integer read FPenThickness write SetPenThickness;
     property PenColor: TTFPColorBridge read FPenColor write SetPenColor;
     property FontSize: Integer read FFontHeight  write SetFontHeight;
     property FontAntiAliased: boolean read FFontAntiAliased write SetFontAntiAliased;
     property FontColor: TTFPColorBridge read FFontColor write SetFontColor;
     property BackgroundColor: TTFPColorBridge read FBackGroundColor write SetBackGroundColor;
     property DrawGrid: boolean read FDrawGrid write FDrawGrid;
     property DrawAxis: boolean read FDrawAxis write FDrawAxis;

     property Title: string read FTitle write SetTitle;
     property XLeft: integer read FXLeft write SetXLeft;
     property YTop: integer read FYTop write SetYTop;
     property Width: integer read FWidth write SetWidth;
     property Height: integer read FHeight write SetHeight;
     property MarginLeft: integer read FLeftMargin write SetLeftMargin;
     property MarginTop: integer read FTopMargin write SetTopMargin;
     property MarginRight: integer read FRightMargin write SetRightMargin;
     property MarginBottom: integer read FBottomMargin write SetBottomMargin;
     property Cliping: boolean read FCliping write FCliping;

     property GridData: TGridData read FGridData write SetGridData;

     property OnChangePenThickness: TNotifyEvent read FOnChangePenThickness write FOnChangePenThickness;
     property OnChangePenColor: TNotifyEvent read FOnChangePenColor write FOnChangePenColor;

     property OnChangeFontSize: TNotifyEvent read FOnChangeFontHeight write FOnChangeFontHeight;
     property OnChangeFontAntiAliased: TNotifyEvent read FOnChangeFontAntiAliased write  FOnChangeFontAntiAliased;
     property OnChangeFontColor: TNotifyEvent read FOnChangeFontColor write FOnChangeFontColor;
     property OnChangeBackGroundColor: TNotifyEvent read FOnChangeBackGroundColor write FOnChangeBackGroundColor;
end;

implementation

procedure TViewPort.SetTitle(AValue: string);
begin
    FTitle:= AValue;
end;

procedure TViewPort.SetXLeft(AValue: integer);
begin
  FXLeft:= AValue;
  FXLeftGrid:= FXLeft + FLeftMargin;
end;

procedure TViewPort.SetYTop(AValue: integer);
begin
  FYTop:= AValue;
  FYTopGrid:= FYTop + FTopMargin;
end;

procedure TViewPort.SetWidth(AValue: integer);
begin
  FWidth:= AValue;
  FWidthGrid:= FWidth - FLeftMargin - FRightMargin;
end;

procedure TViewPort.SetHeight(AValue: integer);
begin
  FHeight:= AValue;
  FHeightGrid:= FHeight -  FTopMargin - FBottomMargin;
end;

procedure TViewPort.SetSize(W: integer; H: integer);
begin
  FWidth:= W;
  FWidthGrid:= FWidth - FLeftMargin - FRightMargin;

  FHeight:= H;
  FHeightGrid:= FHeight -  FTopMargin - FBottomMargin;
end;

procedure TViewPort.SetLeftMargin(AValue: integer);
begin
  FLeftMargin:= AValue;
  FXLeftGrid:= FXLeft + FLeftMargin;
end;

procedure TViewPort.SetTopMargin(AValue: integer);
begin
  FTopMargin:= AValue;
  FYTopGrid:= FYTop + FTopMargin;
end;

procedure TViewPort.SetRightMargin(AValue: integer);
begin
  FRightMargin:= AValue;
  FWidthGrid:= FWidth - FLeftMargin - FRightMargin;
end;

procedure TViewPort.SetBottomMargin(AValue: integer);
begin
  FBottomMargin:= AValue;
  FHeightGrid:= FHeight -  FTopMargin - FBottomMargin;
end;

procedure TViewPort.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FGridData) then
  begin
    FGridData := nil;
  end;
end;

procedure TViewPort.SetGridData(AValue: TGridData);
begin
  if AValue <> FGridData then
   begin
      if Assigned(FGridData) then
      begin
         FGridData.RemoveFreeNotification(Self); //remove free notification...
      end;
      FGridData:= AValue;
      if AValue <> nil then  //re- add free notification...
      begin
         AValue.FreeNotification(self);
      end;
   end;
end;

procedure TViewPort.SetPenThickness(AValue: integer);
begin
  if FPenThickness = AValue then Exit;
  FPenThickness:= AValue;
  if Assigned(FOnChangePenThickness) then FOnChangePenThickness(Self);
end;

procedure TViewPort.SetPenColor(AValue: TTFPColorBridge);
begin
  if FPenColor = AValue then Exit;
  FPenColor:= AValue;
  if Assigned(FOnChangePenColor) then FOnChangePenColor(Self);
end;

procedure TViewPort.SetFontHeight(AValue: integer);
begin
  if FFontHeight = AValue then Exit;
  FFontHeight:= AValue;
  if Assigned(FOnChangeFontHeight) then FOnChangeFontHeight(Self);
end;

procedure TViewPort.SetFontAntiAliased(AValue: boolean);
begin
  if FFontAntiAliased = AValue then Exit;
  FFontAntiAliased:= AValue;
  if Assigned(FOnChangeFontAntiAliased) then FOnChangeFontAntiAliased(Self);
end;

procedure TViewPort.SetFontColor(AValue: TTFPColorBridge);
begin
  if FFontColor = AValue then Exit;
  FFontColor:= AValue;
  if Assigned(FOnChangeFontColor) then FOnChangeFontColor(Self);
end;

procedure TViewPort.SetBackGroundColor(AValue: TTFPColorBridge);
begin
  if FBackGroundColor = AValue then Exit;
  FBackGroundColor:= AValue;
  if Assigned(FOnChangeBackGroundColor) then FOnChangeBackGroundColor(Self);
end;

{TViewPort}

//function PtInRect(const Rect: TRect; const p: TPoint):Boolean
//unit types.pp --Check whether a point is inside a rectangle.
//Note that the rectangle inside is defined as:(left, top, right-VP, bottom-VP)

function TViewPort.IsInside(P: TPoint): boolean;
begin
   Result:=  True; //TODO {true/false}
   //PtInRect(Rect(FXLeftGrid-VP,FYTopGrid-VP,FXLeftGrid+FWidthGrid+VP, FYTopGrid+FHeightGrid+VP), P);
end;

//http://nondot.org/sabre/graphpro/line6.html#ClipCode
FUNCTION TViewPort.CompOutCode(X, Y : INTEGER) : BYTE;
VAR Code : BYTE;
BEGIN
   Code := 0;
   IF      Y > (FYTopGrid+FHeightGrid) {Y2} THEN Code := CodeBottom
   ELSE IF Y <  FYTopGrid {YVP} THEN Code := CodeTop;
   IF      X > (FXLeftGrid+FWidthGrid) {X2} THEN Code := Code+CodeRight
   ELSE IF X <  FXLeftGrid {XVP} THEN Code := Code+CodeLeft;
   Result := Code;
END;

//http://nondot.org/sabre/graphpro/line6.html#ClipCode
function TViewPort.LineClipSurface(var XVP: integer; var YVP:integer; var X2: integer; var Y2:integer): boolean;
VAR
  OutCode0,         { The code of the first endpoint  }
  OutCodeVP,         { The code of the second endpoint }
  OutCodeOut : BYTE;
  X, Y : INTEGER;
  //inside: boolean;
BEGIN

  Result:= True;

  {inside:= IsInside(XVP, YVP);
  inside:= IsInside(X2, Y2);
  if not inside then Result:= False;}
  OutCode0 := CompOutCode(XVP, YVP);            { Compute the original codes   }
  OutCodeVP := CompOutCode(X2, Y2);
  WHILE (OutCode0 <> 0) OR (OutCodeVP <> 0) DO { While not Trivially Accepted }
  BEGIN
    IF (OutCode0 AND OutCodeVP) <> 0 THEN      { Trivial Reject }
    begin
       Result:= False;
       EXIT;
    end
    ELSE
    BEGIN        { Failed both tests, so calculate the line segment to clip }
      IF OutCode0 > 0 THEN
        OutCodeOut := OutCode0    { Clip the first point }
      ELSE
        OutCodeOut := OutCodeVP;   { Clip the last point  }

      IF (OutCodeOut AND CodeBottom) = CodeBottom THEN
      BEGIN               { Clip the line to the bottom of the viewport     }
        Y := FYTopGrid + FHeightGrid;  {Y2}
        X := XVP+{LONGINT}INTEGER(X2-XVP)*{LONGINT}INTEGER(Y-YVP) DIV (Y2 - YVP);
      END
      ELSE IF (OutCodeOut AND CodeTop) = CodeTop THEN
      BEGIN               { Clip the line to the top of the viewport        }
        Y := FYTopGrid; {YVP}
        X := XVP+{LONGINT}INTEGER(X2-XVP)*{LONGINT}INTEGER(Y-YVP) DIV (Y2 - YVP);
      END
      ELSE IF (OutCodeOut AND CodeRight) = CodeRight THEN
      BEGIN               { Clip the line to the right edge of the viewport }
        X := FXLeftGrid + FWidthGrid; {X2}
        Y := YVP+{LONGINT}INTEGER(Y2-YVP)*{LONGINT}INTEGER(X-XVP) DIV (X2-XVP);
      END
      ELSE IF (OutCodeOut AND CodeLeft) = CodeLeft THEN
      BEGIN               { Clip the line to the left edge of the viewport  }
        X := FXLeftGrid; {XVP}
        Y := YVP+{LONGINT}INTEGER(Y2-YVP)*{LONGINT}INTEGER(X-XVP) DIV (X2-XVP);
      END;

      IF (OutCodeOut = OutCode0) THEN       { Modify the first coordinate   }
      BEGIN
        XVP := X; YVP := Y;                   { Update temporary variables    }
        OutCode0 := CompOutCode(XVP, YVP);    { Recalculate the OutCode       }
      END
      ELSE                                  { Modify the second coordinate  }
      BEGIN
        X2 := X; Y2 := Y;                   { Update temporary variables    }
        OutCodeVP := CompOutCode(X2, Y2);    { Recalculate the OutCode       }
      END;

    END;
  END;
END;

procedure TViewPort.ScaleXY(xmin, xmax, ymin, ymax: real);
begin
  if (xmax-xmin) <> 0 then ScaleX:=((FWidthGrid)/(xmax-xmin))
  else ScaleX:=0;
  if (ymax-ymin) <> 0 then ScaleY:= -((FHeightGrid)/(ymax-ymin))
  else ScaleY:=0;
end;

procedure TViewPort.SetScaleXY(xmin, xmax, ymin, ymax: real);
begin
  MinX:= xmin;
  MaxX:= xmax;
  MinY:= ymin;
  MaxY:= ymax;
  ScaleXY(xmin, xmax, ymin, ymax);
end;

function TViewPort.WorldToViewPortX(wrldx:real):integer;
begin
  Result:=Round(ScaleX*(wrldx-MinX));
end;

function TViewPort.ViewPortToSurfaceX(wpx: integer):integer;
begin
  Result:= FXLeftGrid + wpx;
end;

function TViewPort.WorldToSurfaceX(wrldx:real):integer;
begin
   Result:=ViewPortToSurfaceX(WorldToViewPortX(wrldx));
end;

function TViewPort.WorldToViewPortY(wrldy:real):integer;
begin
   Result:= Round(ScaleY*(wrldy-MaxY));
end;

function TViewPort.ViewPortToSurfaceY(wpy: integer):integer;
begin
   Result:= FYTopGrid + wpy;
end;

function TViewPort.WorldToSurfaceY(wrldy:real):integer;
begin
   Result:= ViewPortToSurfaceY(WorldToViewPortY(wrldy));
end;

function TViewPort.SurfaceToViewPortX(scrnx:integer): integer;
begin
   Result:= scrnx - FXLeftGrid;
end;

function TViewPort.ViewPortToWorldX(wpx:integer): real;
begin
   if ScaleX <> 0 then
     Result:=(wpx+ScaleX*MinX)/ScaleX
   else Result:= 0;
end;

function TViewPort.SurfaceToWorldX(scrnx: integer): real;
begin
    Result:= ViewPortToWorldX(SurfaceToViewPortX(scrnx));
end;

function TViewPort.SurfaceToViewPortY(scrny:integer): integer;
begin
   Result:= scrny - FYTopGrid;
end;

function TViewPort.SurfaceToViewPortXY(scrnx, scrny:integer; out xvp: integer; out yvp: integer): boolean;
begin
     Result:= False;
   {  if IsInside(Point(scrnx ,scrny)) then
     begin  }
        xvp:= SurfaceToViewPortX(scrnx);
        yvp:= SurfaceToViewPortY(scrny);

        if IsInside(Point(scrnx ,scrny)) then Result:= True;
   {  end;  }
end;

function TViewPort.ViewPortToWorldY(wpy:integer): real;
begin
   if ScaleY <> 0 then
     Result:=(wpy+ScaleY*MaxY)/ScaleY
   else Result:= 0;
end;

function TViewPort.SurfaceToWorldY(scrny: integer): real;
begin
    Result:= ViewPortToWorldY(SurfaceToViewPortY(scrny));
end;

function TViewPort.SurfaceToWorldXY(scrnx ,scrny: integer; out xworld: real; out yworld: real): boolean;
begin
  Result:= False;
 {if IsInside(Point(scrnx ,scrny))  then
  begin}
    xworld:= SurfaceToWorldX(scrnx);
    yworld:= SurfaceToWorldY(scrny);
    if IsInside(Point(scrnx ,scrny)) then Result:= True;
 {end;}
end;

function TViewPort.WorldToSurfaceXY(xworld, yworld: real; out scrnx ,scrny: integer): boolean;
begin
  Result:= False;
  scrnx:= WorldToSurfaceX(xworld);
  scrny:= WorldToSurfaceY(yworld);
  if IsInside(Point(scrnx ,scrny)) then Result:= True;
end;

constructor TViewPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FTitle:= 'ViewPort';
  FXLeft:= 0;
  FYTop:= 0;
  FWidth:= 800;
  FHeight:= 100;
  FLeftMargin:= 60;
  FTopMargin:= 30;
  FRightMargin:= 40;
  FBottomMargin:= 25;
  FXLeftGrid:= FXLeft + FLeftMargin;
  FYTopGrid:= FYTop + FTopMargin;
  FWidthGrid:= FWidth - FLeftMargin - FRightMargin;
  FHeightGrid:= FHeight -  FTopMargin - FBottomMargin;
  Cliping:= False;
  FPenThickness:= 1;
  FPenColor:= colbrRed;
  FFontHeight:= 9;
  FFontAntiAliased:= True;
  FFontColor:= colbrBlack ;
  FBackGroundColor:= colbrLavender;
  FDrawGrid:= False;
  FDrawAxis:= False;

end;

destructor TViewPort.Destroy;
begin
  //
   inherited Destroy;
end;

end.
