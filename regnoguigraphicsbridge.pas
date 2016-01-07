unit regnoguigraphicsbridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ViewPort, FPNoGUIGraphicsBridge,
  LResources ;

Procedure Register;

implementation

Procedure Register;

begin
  //RegisterPropertyEditor(TypeInfo(TTFPColorBridge), nil,'',TTFPColorBridgePropertyEditor);
  {$I fpnoguigraphicsbridge_icon.lrs}
  {$I viewport_icon.lrs}
  RegisterComponents('Graphics Bridges',[TFPNoGUIGraphicsBridge, TViewPort]);
end;

end.

