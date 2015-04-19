unit TFPColorBridgePropertyEditor;

interface

uses

Classes, TypInfo, SysUtils, LCLProc, LCLType, Graphics, LCLIntf,
FPImage, PropEdits, FPColorBridge;

type

  TTFPColorBridgePropertyEditor = class(TEnumPropertyEditor)
  public
    procedure ListMeasureWidth(const AValue: ansistring; Index:integer;
                               ACanvas: TCanvas; var AWidth: Integer); override;
    procedure ListMeasureHeight(const AValue: ansistring; Index:integer;
                                   ACanvas:TCanvas; var AHeight: Integer); override;
    procedure ListDrawValue(const AValue: ansistring; Index:integer;
                            ACanvas:TCanvas; const ARect: TRect; AState: TPropEditDrawState); override;
    procedure PropDrawValue(ACanvas:TCanvas; const ARect: TRect; AState: TPropEditDrawState); override;

  end;

//procedure Register;

implementation

//Register editor for TTFPColorBridge enumerated type:
(*
procedure Register;
begin
   RegisterPropertyEditor(TypeInfo(TTFPColorBridge), nil,'',TTFPColorBridgePropertyEditor);
end;
*)
procedure TTFPColorBridgePropertyEditor.ListMeasureWidth(const AValue:ansistring; Index:integer;
                                                            ACanvas:TCanvas; var AWidth:Integer);
begin
   AWidth := ACanvas.TextWidth('colbrLightGoldenRodYellow') + 25;
end;

procedure TTFPColorBridgePropertyEditor.ListMeasureHeight(const AValue: ansistring; Index:integer;
                               ACanvas:TCanvas; var AHeight: Integer);
begin
   AHeight:= ACanvas.TextHeight('Mg')+ 2;
end;

{------ListDrawValue-------
This is called during the item/value render phase of the drop down list's render.}
procedure TTFPColorBridgePropertyEditor.ListDrawValue(const AValue: ansistring; Index:integer;
                        ACanvas:TCanvas; const ARect: TRect; AState: TPropEditDrawState);
var
    vRight, vBottom: Integer;
    vOldPenColor, vOldBrushColor: TColor;
    vOldPenStyle: TPenStyle;
    tfpColBridge: TTFPColorBridge;
begin
    vRight := (ARect.Bottom - ARect.Top) + ARect.Left - 3;
    vBottom:=ARect.Bottom - 3;
    with ACanvas do
    begin
        // save off things
        vOldPenStyle := Pen.Style;
        vOldPenColor := Pen.Color;
        vOldBrushColor := Brush.Color;
        // frame things
        if pedsInEdit in AState then
        begin
           if pedsSelected in AState then Brush.Color := clWindow
           else Brush.Color := ACanvas.Brush.Color;
        end
        else
        begin
          if pedsSelected in AState then Brush.Color := clHighlightText
          else Brush.Color := clWindow;
        end;
        Pen.Color := Brush.Color;
        Pen.Style := psSolid;
        FillRect(ARect);
        Rectangle(ARect.Left, ARect.Top, vRight, vBottom);

        // set things up and do the work
        tfpColBridge:= TTFPColorBridge(GetEnumValue(GetPropType, AValue));
        Brush.Color := FPColorToTColor(ToTFPColor(tfpColBridge));
        Pen.Color := clBlack;
        Rectangle(ARect.Left + 2, ARect.Top + 2, vRight - 2, vBottom - 2);

        // restore the things we twiddled with
        Brush.Color := vOldBrushColor;
        Pen.Color := vOldPenColor;
        Pen.Style := vOldPenStyle;
    end;
    inherited ListDrawValue(AValue, Index, ACanvas,
                  Rect(vRight, ARect.Top, ARect.Right, ARect.Bottom), AState);
end;

{-------PropDrawValue------
  Called during the render of the value column of the property list.}
procedure TTFPColorBridgePropertyEditor.PropDrawValue(ACanvas:TCanvas; const ARect: TRect;
                                                         AState: TPropEditDrawState);
begin
  if GetVisualValue <> '' then
      ListDrawValue(GetVisualValue, -1, ACanvas, ARect, [pedsInEdit])
  else
     inherited PropDrawValue(ACanvas, ARect, AState);
end;

end.

