unit regnoguigraphicsbridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  {FPDxfWriteBridge,FPColorBridge, TFPColorBridgePropertyEditor,} GridData, ViewPort, FPNoGUIGraphicsBridge,
  LResources {LCLIntf, PropEdits, TypInfo, LCLProc, LCLType};

Procedure Register;

implementation

Procedure Register;

begin
  //RegisterPropertyEditor(TypeInfo(TTFPColorBridge), nil,'',TTFPColorBridgePropertyEditor);
  //{$I fpdxfwritebridge1_icon.lrs}
  {$I fpnoguigraphicsbridge_icon.lrs}
  {$I griddata_icon.lrs}
  {$I viewport_icon.lrs}
  RegisterComponents('Graphics Bridges',[{TFPDXFWriteBridge, }TFPNoGUIGraphicsBridge, TViewPort, TGridData]);
end;

initialization

end.

end;
