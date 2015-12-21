unit FPColorBridge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPImage;

const

  TFPColorBridgeArray: array[0..142] of longint = (
    $000000,$98FB98,$9932CC,$9ACD32,$A0522D,
    $A52A2A,$A9A9A9,$ADD8E6,$ADFF2F,$AFEEEE,
    $B0C4DE,$B0E0E6,$B22222,$B8860B,$BA55D3,
    $BC8F8F,$BDB76B,$C0C0C0,$000080,$C71585,
    $CD5C5C,$CD853F,$D02090,$D19275,$D2691E,
    $D2B48C,$D3D3D3,$00008B,$D87093,$D8BFD8,
    $DA70D6,$DAA520,$DC143C,$DCDCDC,$DDA0DD,
    $DEB887,$E0FFFF,$E6E6FA,$E9967A,$EE82EE,
    $EEE8AA,$F08080,$F0E68C,$F0F8FF,$F0FFF0,
    $F0FFFF,$F4A460,$F5DEB3,$F5F5DC,$F5F5F5,
    $F5FFFA,$F8F8FF,$FA8072,$FAEBD7,$FAF0E6,
    $FAFAD2,$191970,$FDF5E6,$FF0000,$FF00FF,
    $FF00FF,$FF1493,$FF4500,$FF6347,$FF69B4,
    $FF7F50,$FF8C00,$FFA07A,$FFA500,$FFB6C1,
    $FFC0CB,$FFD700,$FFDAB9,$FFDEAD,$FFE4B5,
    $FFE4C4,$FFE4E1,$FFEBCD,$FFEFD5,$FFF0F5,
    $FFF5EE,$FFF8DC,$FFFACD,$FFFAF0,$FFFAFA,
    $FFFF00,$FFFFE0,$FFFFF0,$FFFFFF,$1E90FF,
    $0000CD,$20B2AA,$228B22,$0000FF,$006400,
    $2E8B57,$2F4F4F,$008000,$008080,$32CD32,
    $008B8B,$3CB371,$40E0D0,$4169E1,$4682B4,
    $483D8B,$48D1CC,$00BFFF,$4B0082,$00CED1,
    $556B2F,$5F9EA0,$00FA9A,$00FF00,$00FF7F,
    $00FFFF,$00FFFF,$6495ED,$66CDAA,$696969,
    $6A5ACD,$6B8E23,$708090,$778899,$7B68EE,
    $7CFC00,$7FFF00,$7FFFD4,$800000,$800080,
    $808000,$808080,$8470FF,$87CEEB,$87CEFA,
    $8A2BE2,$8B0000,$8B008B,$8B4513,$8FBC8F,
    $90EE90,$9370D8,$9400D3);

type

 //http://www.abpsoft.com/criacaoweb/tabcores.html
TTFPColorBridge = (
  colbrBlack,colbrPaleGreen,colbrDarkOrchid,colbrYellowGreen,colbrSienna,
  colbrBrown,colbrDarkGray,colbrLightBlue,colbrHoneyDew,colbrPaleTurquoise,
  colbrLightSteelBlue,colbrPowderBlue,colbrFireBrick,colbrDarkGoldenRod,
  colbrMediumOrchid,colbrRosyBrown,colbrDarkKhaki,colbrSilver,colbrNavy,
  colbrMediumVioletRed,colbrIndianRed,colbrPeru,colbrVioletRed,colbrFeldspar,
  colbrChocolate,colbrTan,colbrLightGray,colbrDarkBlue,colbrPaleVioletRed,
  colbrThistle,colbrOrchid,colbrGoldenRod,colbrCrimson,colbrGainsboro,
  colbrPlum,colbrBurlyWood,colbrLightCyan,colbrLavender,colbrDarkSalmon,
  colbrViolet,colbrPaleGoldenRod,colbrLightCoral,colbrKhaki,colbrAliceBlue,
  colbrGreenYellow,colbrAzure,colbrSandyBrown,colbrWheat,colbrBeige,
  colbrWhiteSmoke,colbrMintCream,colbrGhostWhite,colbrSalmon,colbrAntiqueWhite,
  colbrLinen,colbrLightGoldenRodYellow,colbrMidnightBlue,colbrOldLace,colbrRed,
  colbrFuchsia,colbrMagenta,colbrDeepPink,colbrOrangeRed,colbrTomato,
  colbrHotPink,colbrCoral,colbrDarkOrange,colbrLightSalmon,colbrOrange,
  colbrLightPink,colbrPink,colbrGold,colbrPeachPuff,colbrNavajoWhite,
  colbrMoccasin,colbrBisque,colbrMistyRose,colbrBlanchedAlmond,colbrPapayaWhip,
  colbrLavenderBlush,colbrSeaShell,colbrCornsilk,colbrLemonChiffon,colbrFloralWhite,
  colbrSnow,colbrYellow,colbrLightYellow,colbrIvory,colbrWhite,colbrDodgerBlue,
  colbrMediumBlue,colbrLightSeaGreen,colbrForestGreen,colbrBlue,colbrDarkGreen,
  colbrSeaGreen,colbrDarkSlateGray,colbrGreen,colbrTeal,colbrLimeGreen,
  colbrDarkCyan,colbrMediumSeaGreen,colbrTurquoise,colbrRoyalBlue,colbrSteelBlue,
  colbrDarkSlateBlue,colbrMediumTurquoise,colbrDeepSkyBlue,colbrIndigo,colbrDarkTurquoise,
  colbrDarkOliveGreen,colbrCadetBlue,colbrMediumSpringGreen,colbrLime,colbrSpringGreen,
  colbrAqua,colbrCyan,colbrCornflowerBlue,colbrMediumAquaMarine,colbrDimGray,
  colbrSlateBlue,colbrOliveDrab,colbrSlateGray,colbrLightSlateGray,colbrMediumSlateBlue,
  colbrLawnGreen,colbrChartreuse,colbrAquamarine,colbrMaroon,colbrPurple,
  colbrOlive,colbrGray,colbrLightSlateBlue,colbrSkyBlue,colbrLightSkyBlue,
  colbrBlueViolet,colbrDarkRed,colbrDarkMagenta,colbrSaddleBrown,colbrDarkSeaGreen,
  colbrLightGreen,colbrMediumPurple,colbrDarkViolet);

procedure GetRedGreenBlue(rgb: longInt; out Red, Green, Blue: word);
function ToTFPColor(colbrColor: TTFPColorBridge): TFPColor;
function ToTFPColorTransparent: TFPColor;


implementation


procedure GetRedGreenBlue(rgb: longInt; out Red, Green, Blue: word);
begin
    red:=   ( (rgb and $ff0000)  shr 16) shl 8;
    Green:= ( (rgb and $ff00  )  shr  8) shl 8;
    blue:=  ( (rgb and $ff)            ) shl 8;
end;

function ToTFPColor(colbrColor: TTFPColorBridge):  TFPColor;
var
    index: integer;
    red, green, blue: word;
begin
    index:= (Ord(colbrColor));
    GetRedGreenBlue(TFPColorBridgeArray[index], red, green, blue);
    Result.Red:=   red;
    Result.Green:= green;
    Result.Blue:=  blue;
    Result.Alpha:= AlphaOpaque;
end;

function ToTFPColorTransparent: TFPColor;
var
   red, green, blue: word;
begin
    GetRedGreenBlue(TFPColorBridgeArray[0], red, green, blue);
    Result.Red:=   red;
    Result.Green:= green;
    Result.Blue:=  blue;
    Result.Alpha:= AlphaTransparent;
end;

end.

