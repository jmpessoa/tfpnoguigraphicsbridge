unit GeometryUtilsBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math{, Dialogs};

const
  PI =	3.1415926535;

type

  TRealPoint = record
     x, y: real;
  end;

  TRealLine = record
       v1: TRealPoint;
       v2: TRealPoint;
  end;

  TIntegerLine = record
       v1: TPoint;
       v2: TPoint;
  end;

  TVertice = array of TRealPoint;

function ToRealPoint(x, y: real): TRealPoint;

function Point(x, y: real): TRealPoint; overload;

function ToIntegerPoint(x, y: integer): TPoint;

function Point(x, y: integer): TPoint; overload;

function IsInside(r , r0, r1: real): boolean;

function GetPointIntersectLines(x0, y0, x1, y1: real; x2,y2,x3,y3: real; out px: real; out py: real): boolean;

procedure GetLineTrim(offset: real;  x1, y1, x2, y2: real;
                                                    out px1:  real;
                                                    out py1:  real;
                                                    out px2:  real;
                                                    out py2:  real;
                                                    referencePoint {1 or 2}: integer);

procedure GetLineExtend(offset: real;  x1, y1, x2, y2: real;
                                                    out px1:  real;
                                                    out py1:  real;
                                                    out px2:  real;
                                                    out py2:  real;
                                                    referencePoint {1 or 2}: integer);
procedure GetLineTranslated(offset: real;  x1, y1, x2, y2: real;
                                                    out px1:  real;
                                                    out py1:  real;
                                                    out px2:  real;
                                                    out py2:  real);

function GetAngleOfLine(x1, y1, x2, y2: real): real;

function GetAngleBetweenPointsByVertice(px, py, x1, y1, x2, y2: real): real;

function GetAngleBetweenTwoLines(rx1, ry1, rx2, ry2, sx1, sy1, sx2, sy2: real): real;

function ToDegrees(radians: real): real;
function ToRadians(degrees: real): real;
procedure GetLineOrthogonal(offset, r{orthoRadius}, x1, y1, x2, y2: real;
                                              out orthoX1: real;
                                              out orthoY1: real;
                                              out orthoX2: real;
                                              out orthoY2: real;
                                              out offsetX: real;
                                              out offsetY: real;
                                              referencePoint {1 or 2}: integer);

procedure GetLineCrossedByAngle(angle, offset, r{crossRadius}, x1, y1, x2, y2: real;
                                              out orthoX1: real;
                                              out orthoY1: real;
                                              out orthoX2: real;
                                              out orthoY2: real;
                                              out offsetX: real;
                                              out offsetY: real;
                                              referencePoint {1 or 2}: integer);

procedure GetHerringbonePatternsLines(angle, offset, r{crossRadius}, x1, y1, x2, y2: real;
                                              out px1: real;
                                              out py1: real;
                                              out px2: real;
                                              out py2: real;
                                              out middleX: real;
                                              out middleY: real;
                                              referencePoint {1 or 2}: integer);

procedure GetLineParallel(offset: real;  x1, y1, x2, y2: real;
                                                    out px1:  real;
                                                    out py1:  real;
                                                    out px2:  real;
                                                    out py2:  real);

procedure GetSignalDxDyByAngle(angle: real; y1, y2: real;
                               out sigdx: integer; out sigdy: integer;
                               referencePoint: integer);

procedure GetPointByOffset(offset: real;  x1, y1, x2, y2: real;
                                                    out offsetX:  real;
                                                    out offsetY:  real;
                                                    referencePoint {1 or 2}: integer);

function GetDistanceFromPointToLine(px, py, x1, y1, x2, y2: real): real;

function GetDistanceBetweenTwoPoints(x1, y1, x2, y2: real): real;

function GetLineNearestXY(V: array of TRealPoint; X,Y: real;  out distMin: real): TRealLine;
function GetLineFromIndex(V: array of TRealPoint; index: integer): TRealLine;
function GetLineFromIndex(V: array of TPoint; index: integer): TIntegerLine;
function GetLineFromVertices(v1, v2:TRealPoint): TRealLine;
procedure GetVerticesFromLine(L: TIntegerLine; out v1, v2: TPoint);
procedure GetVerticesFromLine(L: TRealLine; out v1, v2: TRealPoint);

//function CosUV2(ux,uy,vx,vy: real):real;
function CosUV(ux,uy,vx,vy: real):real;

procedure GetPointByRotation(angle: real; x0, y0: real; out x1: real; out y1: real);

procedure GetExtremeLinePointByRotation(angle: real; x1, y1, x2, y2: real;
                                        out x3: real; out y3: real;
                                        referencePoint: integer);

procedure GetLineRotationByMiddlePoint(angle: real; x1, y1, x2, y2: real;
                                        out x3: real; out y3:real; out x4: real; out y4:real);

implementation

procedure GetPointByRotation(angle: real; x0, y0: real; out x1: real; out y1: real);
begin
    x1:= x0*cos(angle) - y0*sin(angle);
    y1:= x0*sin(angle) + y0*cos(angle);
end;

procedure GetExtremeLinePointByRotation(angle: real; x1, y1, x2, y2: real;
                                        out x3: real; out y3: real;
                                        referencePoint: integer);
var
    x0, y0: real;
    dx, dy: real;
begin
   if referencePoint = 2 then //extreme (x2, y2)
   begin
    x0:= x2 - x1;
    y0:= y2 - y1;
    GetPointByRotation(angle, x0, y0, dx, dy);
    x3:= x1 + dx;
    y3:= y1 + dy;
   end
   else
   begin     //extreme (x1, y1)
       x0:= x1 - x2;
       y0:= y1 - y2;
       GetPointByRotation(angle, x0, y0, dx, dy);
       x3:= x2 + dx;
       y3:= y2 + dy;
   end;
end;

procedure GetLineRotationByMiddlePoint(angle: real; x1, y1, x2, y2: real;
                                        out x3: real; out y3: real; out x4: real; out y4:real);
var
    x0, y0: real;
    dx, dy: real;
    midPx, midPy: real;
begin
   midPx:= (x1+x2)/2;
   midPy:= (y1+y2)/2;

   x0:= x1 - midPx;
   y0:= y1 - midPy;
   GetPointByRotation(angle, x0, y0, dx, dy);
   x3:= midPx + dx;
   y3:= midPy + dy;

   x0:= x2 - midPx;
   y0:= y2 - midPy;
   GetPointByRotation(angle, x0, y0, dx, dy);
   x4:= midPx + dx;
   y4:= midPy + dy;
end;

 {
function CosUV2(ux,uy,vx,vy: real):real;
var
    u_dot_v:real;
    Nv, Nu: real;
begin
  Nu := sqrt(ux*ux + uy*uy);
  Nv := sqrt(vx*vx + vy*vy);
  u_dot_v := Abs(ux*vx + uy*vy);  //u_dot_v = 0 --> orthogonal
             //Abs....     0 < angle <90
  Result:= (u_dot_v)/(Nu*Nv);
end;
  }

function CosUV(ux,uy,vx,vy: real):real;
var
    u_dot_v:real;
    Nv, Nu: real;
begin
  Nu := sqrt(ux*ux + uy*uy);
  Nv := sqrt(vx*vx + vy*vy);
  u_dot_v := ux*vx + uy*vy;  //u_dot_v = 0 --> orthogonal

  Result:= (u_dot_v)/(Nu*Nv);
end;

                             //P1(x1, y1)
    //vertice(px,py)

                             //P2(x2, y2)
function GetAngleBetweenPointsByVertice(px, py, x1, y1, x2, y2: real): real;
var
    ux, uy, vx, vy: real;
begin
   ux:= x1 - px;
   uy:= y1 - py;
   vx:= x2 - px;
   vy:= y2 - py;
   Result:=  arccos(CosUV(ux,uy,vx,vy));
end;

function GetAngleBetweenTwoLines(rx1, ry1, rx2, ry2, sx1, sy1, sx2, sy2: real): real;
var
   ux, uy, vx, vy: real;
begin
   ux:= rx2 - rx1;
   uy:= ry2 - ry1;
   vx:= sx2 - sx1;
   vy:= sy2 - sy1;
   Result:=  arccos(CosUV(ux,uy,vx,vy));
end;

function GetAngleOfLine(x1, y1, x2, y2: real): real;
var
    ux, uy, vx, vy: real;
    sx1, sy1, sx2, sy2: real;
begin
   sx1:=0;
   sy1:=0;
   sx2:=1;
   sy2:=0;
   ux:= x2 - x1;
   uy:= y2 - y1;
   vx:= sx2 - sx1;
   vy:= sy2 - sy1;
   Result:=  arccos(CosUV(ux,uy,vx,vy));
end;

procedure GetVerticesFromLine(L: TRealLine; out v1, v2: TRealPoint);
begin
    v1.x:= L.v1.x;
    v1.y:= L.v1.y;
    v2.x:= L.v2.x;
    v2.y:= L.v2.y;
end;

procedure GetVerticesFromLine(L: TIntegerLine; out v1, v2: TPoint);
begin
    v1.x:= L.v1.x;
    v1.y:= L.v1.y;
    v2.x:= L.v2.x;
    v2.y:= L.v2.y;
end;

function GetLineFromVertices(v1, v2:TRealPoint): TRealLine;
begin
     Result.v1.x:= v1.x;
     Result.v1.y:= v1.y;
     Result.v2.x:= v2.x;
     Result.v2.y:= v2.y;
end;

function GetLineFromIndex(V: array of TRealPoint; index: integer): TRealLine;
var
  last: integer;
begin
    last:= High(V);
    if index = last then
    begin
        Result.v1.x:= V[last].x;
        Result.v1.y:= V[last].y;
        Result.v2.x:= V[0].x;
        Result.v2.y:= V[0].y;
    end
    else
    begin
       Result.v1.x:= V[index].x;
       Result.v1.y:= V[index].y;
       Result.v2.x:= V[index+1].x;
       Result.v2.y:= V[index+1].y;
    end;
end;

function GetLineFromIndex(V: array of TPoint; index: integer): TIntegerLine;
var
  last: integer;
begin
    last:= High(V);
    if index = last then
    begin
        Result.v1.x:= V[last].x;
        Result.v1.y:= V[last].y;
        Result.v2.x:= V[0].x;
        Result.v2.y:= V[0].y;
    end
    else
    begin
       Result.v1.x:= V[index].x;
       Result.v1.y:= V[index].y;
       Result.v2.x:= V[index+1].x;
       Result.v2.y:= V[index+1].y;
    end;
end;

function GetDistanceFromPointToLine(px, py, x1, y1, x2, y2: real): real;
var
    a,b,c, angle: real;  {ax+by = c}
begin
       angle:= GetAngleOfLine(x1, y1, x2, y2);

       if Abs(angle) < 0.0005 then    ////line parallel axis X
       begin
           Result:= Abs(y1-py);
       end
       else if Abs(angle - PI/2) < 0.0005 then //line parallel axis Y
       begin
          Result:= Abs(x1-px);
       end
       else //ax + by  = c;
       begin
          a:= arctan(angle);
          b:= 1;
          c:= y1 - a*x1;
          Result:= (abs(-a*px + b*py - c))/(sqrt(a*a+b*b));
       end;
end;

//--------
function GetLineNearestXY(V: array of TRealPoint; X,Y: real; out distMin: real): TRealLine;
var
    i, count: integer;
    distTemp: real;
    lin: TRealLine;
begin
    count:= High(V) + 1;
    Result:= GetLineFromIndex(V, 0);
    distMin:= GetDistanceFromPointToLine(X, Y, Result.v1.x, Result.v1.y,
                                              Result.v2.x, Result.v2.y);
    for i:= 1 to count-1 do
    begin
        lin:= GetLineFromIndex(V, i);
        distTemp:= GetDistanceFromPointToLine(X, Y, lin.v1.x, lin.v1.y,lin.v2.x, lin.v2.y);
        if distTemp < distMin then
        begin
           distMin:= distTemp;
           Result.v1.x:= lin.v1.x;
           Result.v1.y:= lin.v1.y;
           Result.v2.x:= lin.v2.x;
           Result.v2.y:= lin.v2.y;
        end;
    end;
end;

function GetDistanceBetweenTwoPoints(x1, y1, x2, y2: real): real;
begin
    Result:= sqrt(sqr(x2 - x1) + sqr(y2 - y1));
end;

function ToRealPoint(x,y: real): TRealPoint;
begin
   Result.x:=x;
   Result.y:=y;
end;

function Point(x, y: real): TRealPoint;
begin
   Result.x:=x;
   Result.y:=y;
end;

function ToIntegerPoint(x,y: integer): TPoint;
begin
   Result.x:=x;
   Result.y:=y;
end;

function Point(x,y: integer): TPoint;
begin
   Result.x:=x;
   Result.y:=y;
end;

function ToDegrees(radians: real): real;
begin
   Result:= radians*180/PI;
end;

function ToRadians(degrees: real): real;
begin
   Result:= degrees*0.01745322925;
end;

//min(x0,x1) ≤ px ≤ max(x0,x1)
function IsInside(r , r0, r1: real): boolean;
var
  min, max: real;
begin
     if r0 <= r1 then
     begin
        min:= r1;
        max:= r0
     end
     else
     begin
        min:= r0;
        max:= r1
     end;
     if (r >= min) and (r <= max) then
         result:= True;
end;

//http://www.gamedev.net/page/resources/_/technical/math-and-physics/fast-2d-line-intersection-algorithm-r423
function GetPointIntersectLines(x0, y0, x1, y1: real; x2,y2,x3,y3: real; out px: real; out py: real): boolean;
// this function returns the intersection point, note that the function assumes
// the lines intersect. the function can handle vertical as well
// as horizontal lines.
var
   a1,b1,c1: real;
   // constants of linear equations
   a2,b2,c2: real;
   det_inv: real;
   // the inverse of the determinant of the coefficient matrix
   m1,m2: real;
   insideX, insideY: boolean;
begin
   // the slopes of each line
   // compute slopes, note the cludge for infinity, however, this will be close enough
   if (x1-x0) <> 0 then
      m1 := (y1-y0)/(x1-x0)
   else
       m1 := 1E+10; // close enough to infinity

   if (x3-x2) <> 0 then
      m2 := (y3-y2)/(x3-x2)
   else
      m2 := 1E+10; // close enough to infinity or  1E100;

  //compute constants
  a1 := m1;
  a2 := m2;
  b1 := -1;
  b2 := -1;
  c1 := (y0-m1*x0);
  c2 := (y2-m2*x2);

  //compute the inverse of the determinate
  det_inv := 1/(a1*b2 - a2*b1);

  // use Kramers rule to compute px and py
  px:=((b1*c2 - b2*c1)*det_inv);
  py:=((a2*c1 - a1*c2)*det_inv);

  //Add Code by jmpessoa@hotmail.com
  Result:= False;
  insideX:= IsInside(px, x0,x1);
  insideY:= IsInside(py, y0,y1);
  if insideX and insideY then Result:= True;

end; // end Intersect_Lines


procedure GetLineTrim(offset: real;  x1, y1, x2, y2: real;
                                                    out px1:  real;
                                                    out py1:  real;
                                                    out px2:  real;
                                                    out py2:  real;
                                                    referencePoint {1 or 2}: integer);
var
  offsetX, offsetY: real;
  trimOffset: real;
begin
   trimOffset:= offset;
   if offset > 0 then trimOffset:= -offset;
    GetPointByOffset(trimOffset, x1, y1, x2, y2, offsetX, offsetY , referencePoint);
    if referencePoint = 1 then
    begin
        px1:= offsetX;
        py1:= offsetY;
        px2:= x2;
        py2:= y2;
    end
    else //referencePoint = 2
    begin
        px1:= x1;
        py1:= y1;
        px2:= offsetX;
        py2:= offsetY;
    end;
end;

procedure GetLineTranslated(offset: real;  x1, y1, x2, y2: real;
                                                    out px1:  real;
                                                    out py1:  real;
                                                    out px2:  real;
                                                    out py2:  real);
begin
   GetPointByOffset(-offset,x1, y1, x2, y2, px1, py1,1);
   GetPointByOffset(offset, x1, y1, x2, y2, px2, py2,2);
end;

procedure GetLineOrthogonal(offset {number positive/out or number negative/in},
                             r {orthoRadius}, x1, y1, x2, y2: real;
                             out orthoX1: real;
                             out orthoY1: real;
                             out orthoX2: real;
                             out orthoY2: real;
                             out offsetX: real;
                             out offsetY: real;
                             referencePoint {1 or 2}: integer);
begin
   GetLineCrossedByAngle(PI/2, offset, r, x1, y1, x2, y2,
                                              orthoX1,
                                              orthoY1,
                                              orthoX2,
                                              orthoY2,
                                              offsetX,
                                              offsetY,
                                              referencePoint);
end;

procedure GetLineCrossedByAngle(angle, offset, r{crossRadius}, x1, y1, x2, y2: real;
                                              out orthoX1: real;
                                              out orthoY1: real;
                                              out orthoX2: real;
                                              out orthoY2: real;
                                              out offsetX: real;
                                              out offsetY: real;
                                              referencePoint {1 or 2}: integer);
var
    rx, ry: real;
begin
    GetPointByOffset(offset,   x1, y1, x2, y2, offsetX, offsetY, referencePoint);
    GetPointByOffset(offset+Abs(r), x1, y1, x2, y2, rx, ry, referencePoint);
    GetExtremeLinePointByRotation(Abs(angle), offsetX, offsetY, rx, ry, orthoX1,orthoY1, 2{ok!});
    GetExtremeLinePointByRotation(-PI+Abs(angle), offsetX, offsetY, rx, ry, orthoX2,orthoY2, 2{ok!});
end;

procedure GetHerringbonePatternsLines(angle, offset, r{crossRadius}, x1, y1, x2, y2: real;
                                              out px1: real;
                                              out py1: real;
                                              out px2: real;
                                              out py2: real;
                                              out middleX: real;
                                              out middleY: real;
                                              referencePoint {1 or 2}: integer);
var
    rx, ry: real;
begin
    GetPointByOffset(offset,   x1, y1, x2, y2, middleX, middleY, referencePoint);
    GetPointByOffset(offset+Abs(r), x1, y1, x2, y2, rx, ry, referencePoint);
    GetExtremeLinePointByRotation(Abs(angle), middleX, middleY, rx, ry, px1,py1, 2{ok!});
    GetExtremeLinePointByRotation(-Abs(angle), middleX, middleY, rx, ry, px2,py2, 2{ok!});
end;

procedure GetSignalDxDyByAngle(angle: real; y1, y2: real;
                               out sigdx: integer; out sigdy: integer;
                               referencePoint: integer);
begin
   if angle < PI/2 then    //increasing
   begin
      if y2 > y1 then
      begin
        if referencePoint = 2  then
        begin
            sigdx:= 1;
            sigdy:= 1;
        end
        else
        begin
            sigdx:= -1;
            sigdy:= -1;
        end;
      end
      else
      begin
        if referencePoint = 2  then
        begin
            sigdx:=  1;
            sigdy:= -1;
        end
        else
        begin
           sigdx:= -1;
           sigdy:=  1;
        end;
      end;
   end
   else
   begin //decreasing
      if y1 > y2 then
      begin
         if referencePoint = 1  then
         begin
             sigdx:= -1;
             sigdy:=  1;
         end
         else
         begin
            sigdx:=  1;
            sigdy:= -1;
         end;
      end
      else
      begin
         if referencePoint = 1  then
         begin
             sigdx:= -1;
             sigdy:= -1;
         end
         else
         begin
            sigdx:= 1;
            sigdy:= 1;
         end;
      end;
   end;
end;

procedure GetPointByOffset(offset: real;  x1, y1, x2, y2: real;
                                                    out offsetX:  real;
                                                    out offsetY:  real;
                                                    referencePoint {1 or 2}: integer);
var
   angle, dx, dy: real;
   signaldx, signaldy: integer;
begin
   angle:=GetAngleOfLine(x1, y1, x2, y2);
   dx:=  offset*cos(angle);
   dy:=  offset*sin(angle);
   GetSignalDxDyByAngle(angle ,y1, y2, signaldx, signaldy, referencePoint);
   if referencePoint = 1 then
   begin
      offsetX:= x1 + signaldx*dx;
      offsetY:= y1 + signaldy*dy;
   end
   else //referencePoint = 2
   begin
     offsetX:= x2 + signaldx*dx;
     offsetY:= y2 + signaldy*dy;
   end;
end;

procedure GetLineExtend(offset: real;  x1, y1, x2, y2: real;
                        out px1:  real;
                        out py1:  real;
                        out px2:  real;
                        out py2:  real;
                        referencePoint {1 or 2}: integer);
var
  offsetX, offsetY: real;
  extendOffset: real;
begin
    extendOffset:= offset;
    if offset < 0 then  extendOffset:= -offset;
    GetPointByOffset(extendOffset, x1, y1, x2, y2, offsetX, offsetY , referencePoint);
    if referencePoint = 1 then
    begin
        px1:= offsetX;
        py1:= offsetY;
        px2:= x2;
        py2:= y2;
    end
    else //referencePoint = 2
    begin
        px1:= x1;
        py1:= y1;
        px2:= offsetX;
        py2:= offsetY;
    end;
end;

procedure GetLineParallel(offset: real;  x1, y1, x2, y2: real;
                                                    out px1:  real;
                                                    out py1:  real;
                                                    out px2:  real;
                                                    out py2:  real);
var
   ortx1, orty1, ortx2, orty2, offsetX, offsetY: real;
begin
   if offset >= 0 then
   begin
     GetLineOrthogonal(0.0, offset, x1, y1, x2, y2, ortx1, orty1, ortx2, orty2, offsetX, offsetY, 1);
     px1:= ortx1;
     py1:= orty1;
     GetLineOrthogonal(0.0, offset, x1, y1, x2, y2, ortx1, orty1, ortx2, orty2, offsetX, offsetY, 2);
     px2:= ortx2;
     py2:= orty2;
   end
   else
   begin
       GetLineOrthogonal(0.0, Abs(offset), x2,y2,x1,y1, ortx1, orty1, ortx2, orty2, offsetX, offsetY, 1);
       px1:= ortx1;
       py1:= orty1;
       GetLineOrthogonal(0.0, Abs(offset), x2,y2,x1,y1, ortx1, orty1, ortx2, orty2, offsetX, offsetY, 2);
       px2:= ortx2;
       py2:= orty2;
   end;
end;

end.

