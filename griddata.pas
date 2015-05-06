unit GridData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPColorBridge;

type
  TGridData = class(TPersistent)
  private
    FTitleX: string;
    FTitleY: string;

    FAxisColor: TTFPColorBridge;
    FColor: TTFPColorBridge;
    FBackGroundColor: TTFPColorBridge;

    FXInterval: integer;
    FYInterval: integer;
    FXLegendInterval: integer;
    FYLegendInterval: integer;
    FXShowLengend: boolean;
    FYShowLengend: boolean;

  public
   XLengendOffsetValue: real;
   YLengendOffsetValue: real;

   constructor Create;
   destructor Destroy; override;

  published
   property XTitle: string read FTitleX write FTitleX;
   property YTitle: string read FTitleY write FTitleY;
   property AxisColor: TTFPColorBridge read FAxisColor write FAxisColor;
   property Color: TTFPColorBridge read FColor write FColor;
   property BackgroundColor: TTFPColorBridge read FBackGroundColor write FBackGroundColor;

   property XInterval: integer read FXInterval write FXInterval;
   property YInterval: integer read FYInterval write FYInterval;
   property XLegendInterval: integer read FXLegendInterval write FXLegendInterval;
   property YLegendInterval: integer read FYLegendInterval write FYLegendInterval;

   property XShowLengend: boolean read FXShowLengend write FXShowLengend;
   property YShowLengend: boolean read FYShowLengend write FYShowLengend;

  end;

implementation

constructor TGridData.Create;
begin
   FAxisColor:= colbrLime;
   FColor:= colbrLightSteelBlue;
   FBackGroundColor:= colbrGreenYellow;
   FTitleX:= '';
   FTitleY:= '';
   FXInterval:= 10;
   FYInterval:= 10;
   FXLegendInterval:= 1;
   FYLegendInterval:= 2;
   XLengendOffsetValue:= 0;
   YLengendOffsetValue:= 0;
   FXShowLengend:= False;
   FYShowLengend:= False;
end;

destructor TGridData.Destroy;
begin
   //
   inherited Destroy;
end;

end.
