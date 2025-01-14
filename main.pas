program XFrames;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}

uses
  Math, fpjson, sysutils, Classes, Generics.Collections, fgl;


const
  ImGuiCol_Text = 0;
  ImGuiCol_TextDisabled = 1;
  ImGuiCol_WindowBg = 2;
  ImGuiCol_ChildBg = 3;
  ImGuiCol_PopupBg = 4;
  ImGuiCol_Border = 5;
  ImGuiCol_BorderShadow = 6;
  ImGuiCol_FrameBg = 7;
  ImGuiCol_FrameBgHovered = 8;
  ImGuiCol_FrameBgActive = 9;
  ImGuiCol_TitleBg = 10;
  ImGuiCol_TitleBgActive = 11;
  ImGuiCol_TitleBgCollapsed = 12;
  ImGuiCol_MenuBarBg = 13;
  ImGuiCol_ScrollbarBg = 14;
  ImGuiCol_ScrollbarGrab = 15;
  ImGuiCol_ScrollbarGrabHovered = 16;
  ImGuiCol_ScrollbarGrabActive = 17;
  ImGuiCol_CheckMark = 18;
  ImGuiCol_SliderGrab = 19;
  ImGuiCol_SliderGrabActive = 20;
  ImGuiCol_Button = 21;
  ImGuiCol_ButtonHovered = 22;
  ImGuiCol_ButtonActive = 23;
  ImGuiCol_Header = 24;
  ImGuiCol_HeaderHovered = 25;
  ImGuiCol_HeaderActive = 26;
  ImGuiCol_Separator = 27;
  ImGuiCol_SeparatorHovered = 28;
  ImGuiCol_SeparatorActive = 29;
  ImGuiCol_ResizeGrip = 30;
  ImGuiCol_ResizeGripHovered = 31;
  ImGuiCol_ResizeGripActive = 32;
  ImGuiCol_Tab = 33;
  ImGuiCol_TabHovered = 34;
  ImGuiCol_TabActive = 35;
  ImGuiCol_TabUnfocused = 36;
  ImGuiCol_TabUnfocusedActive = 37;
  ImGuiCol_PlotLines = 38;
  ImGuiCol_PlotLinesHovered = 39;
  ImGuiCol_PlotHistogram = 40;
  ImGuiCol_PlotHistogramHovered = 41;
  ImGuiCol_TableHeaderBg = 42;
  ImGuiCol_TableBorderStrong = 43;
  ImGuiCol_TableBorderLight = 44;
  ImGuiCol_TableRowBg = 45;
  ImGuiCol_TableRowBgAlt = 46;
  ImGuiCol_TextSelectedBg = 47;
  ImGuiCol_DragDropTarget = 48;
  ImGuiCol_NavHighlight = 49;
  ImGuiCol_NavWindowingHighlight = 50;
  ImGuiCol_NavWindowingDimBg = 51;
  ImGuiCol_ModalWindowDimBg = 52;

type
  TFontDef = record
    name: String;
    size: Integer;
  end;

type
  THEXA = record
    color: string;
    alpha: Double;
  end;

function CreateTHEXA(aColor: string; aAlpha: Double): THEXA;
begin
  Result.color := aColor;
  Result.alpha := aAlpha;
end;

type
  TThemeMap = specialize TFPGMap<Integer, THEXA>;

type
  TOnInitCb = procedure; cdecl;
  TOnTextChangedCb = procedure(id: Integer; text: PChar); cdecl;
  TOnComboChangedCb = procedure(id: Integer; index: Integer); cdecl;
  TOnNumericValueChangedCb = procedure(id: Integer; value: Single); cdecl;
  TOnBooleanValueChangedCb = procedure(id: Integer; value: Boolean); cdecl;
  TOnMultipleNumericValuesChangedCb = procedure(id: Integer; values: PSingle; numValues: Integer); cdecl;
  TOnClickCb = procedure(id: Integer); cdecl;

procedure init(
  assetsBasePath: PChar;
  rawFontDefinitions: PChar;
  rawStyleOverrideDefinitions: PChar;
  onInit: TOnInitCb;
  onTextChanged: TOnTextChangedCb;
  onComboChanged: TOnComboChangedCb;
  onNumericValueChanged: TOnNumericValueChangedCb;
  onBooleanValueChanged: TOnBooleanValueChangedCb;
  onMultipleNumericValuesChanged: TOnMultipleNumericValuesChangedCb;
  onClick: TOnClickCb
); cdecl; external 'xframesshared';

procedure setElement(elementJson: PChar); cdecl; external 'xframesshared';

procedure setChildren(id: Integer; childrenIds: PChar); cdecl; external 'xframesshared';


procedure MyOnInit; cdecl;
var
  rootNode: TJSONObject;
  unformattedText: TJSONObject;
  childrenIds: TJSONArray;
begin
  rootNode := TJSONObject.Create;
  rootNode.Add('type', 'node');
  rootNode.Add('id', 0);
  rootNode.Add('root', True);

  unformattedText := TJSONObject.Create;
  unformattedText.Add('type', 'unformatted-text');
  unformattedText.Add('id', 1);
  unformattedText.Add('text', 'Hello, world');

  childrenIds := TJSONArray.Create;
  childrenIds.Add(1);

  setElement(PChar(rootNode.AsJSON));
  setElement(PChar(unformattedText.AsJSON));
  setChildren(0, PChar(childrenIds.AsJSON));
end;

procedure MyOnTextChanged(id: Integer; text: PChar); cdecl;
begin
  WriteLn('Text changed: ID = ', id, ', Text = ', text);
end;

procedure MyOnComboChanged(id: Integer; index: Integer); cdecl;
begin
  WriteLn('Combo changed: ID = ', id, ', Index = ', index);
end;

procedure MyOnNumericValueChanged(id: Integer; value: Single); cdecl;
begin
  WriteLn('Numeric value changed: ID = ', id, ', Value = ', value:0:2);
end;

procedure MyOnBooleanValueChanged(id: Integer; value: Boolean); cdecl;
begin
  WriteLn('Boolean value changed: ID = ', id, ', Value = ', value);
end;

procedure MyOnMultipleNumericValuesChanged(id: Integer; values: PSingle; numValues: Integer); cdecl;
var
  i: Integer;
begin
  Write('Multiple numeric values changed: ID = ', id, ', Values = ');
  for i := 0 to numValues - 1 do
    Write(values[i]:0:2, ' ');
  WriteLn;
end;

procedure MyOnClick(id: Integer); cdecl;
begin
  WriteLn('Click event: ID = ', id);
end;

var
  themeDef: TJSONObject;
  colorDefs: TJSONObject;

  fontDef: TJSONObject;
  fontDefs: TJSONObject;
  defsArray: TJSONArray;
  HEXAJsonArray: TJSONArray;
  fontDefArray: array of TFontDef;
  fontSizes: array of Integer;
  theme2Colors: TStringList;
  theme2: TThemeMap;
  i: Integer;

begin
  try
    theme2Colors := TStringList.Create;
    theme2 := TThemeMap.Create;
    themeDef := TJSONObject.Create;
    colorDefs := TJSONObject.Create;
    fontSizes := [16, 18, 20, 24, 28, 32, 36, 48];

    theme2Colors.Add('darkestGrey=#141f2c');
    theme2Colors.Add('darkerGrey=#2a2e39');
    theme2Colors.Add('darkGrey=#363b4a');
    theme2Colors.Add('lightGrey=#5a5a5a');
    theme2Colors.Add('lighterGrey=#7A818C');
    theme2Colors.Add('evenLighterGrey=#8491a3');
    theme2Colors.Add('black=#0A0B0D');
    theme2Colors.Add('green=#75f986');
    theme2Colors.Add('red=#ff0062');
    theme2Colors.Add('white=#fff');

    theme2.Add(ImGuiCol_Text, CreateTHEXA(theme2Colors.Values['white'], 1.0));
    theme2.Add(ImGuiCol_TextDisabled, CreateTHEXA(theme2Colors.Values['lighterGrey'], 1));
    theme2.Add(ImGuiCol_WindowBg, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_ChildBg, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_PopupBg, CreateTHEXA(theme2Colors.Values['white'], 1));
    theme2.Add(ImGuiCol_Border, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_BorderShadow, CreateTHEXA(theme2Colors.Values['darkestGrey'], 1));
    theme2.Add(ImGuiCol_FrameBg, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_FrameBgHovered, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_FrameBgActive, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_TitleBg, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_TitleBgActive, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_TitleBgCollapsed, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_MenuBarBg, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_ScrollbarBg, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_ScrollbarGrab, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_ScrollbarGrabHovered, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_ScrollbarGrabActive, CreateTHEXA(theme2Colors.Values['darkestGrey'], 1));
    theme2.Add(ImGuiCol_CheckMark, CreateTHEXA(theme2Colors.Values['darkestGrey'], 1));
    theme2.Add(ImGuiCol_SliderGrab, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_SliderGrabActive, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_Button, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_ButtonHovered, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_ButtonActive, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_Header, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_HeaderHovered, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_HeaderActive, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_Separator, CreateTHEXA(theme2Colors.Values['darkestGrey'], 1));
    theme2.Add(ImGuiCol_SeparatorHovered, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_SeparatorActive, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_ResizeGrip, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_ResizeGripHovered, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_ResizeGripActive, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_Tab, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_TabHovered, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_TabActive, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_TabUnfocused, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_TabUnfocusedActive, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_PlotLines, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_PlotLinesHovered, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_PlotHistogram, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_PlotHistogramHovered, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_TableHeaderBg, CreateTHEXA(theme2Colors.Values['black'], 1));
    theme2.Add(ImGuiCol_TableBorderStrong, CreateTHEXA(theme2Colors.Values['lightGrey'], 1));
    theme2.Add(ImGuiCol_TableBorderLight, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_TableRowBg, CreateTHEXA(theme2Colors.Values['darkGrey'], 1));
    theme2.Add(ImGuiCol_TableRowBgAlt, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_TextSelectedBg, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_DragDropTarget, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_NavHighlight, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_NavWindowingHighlight, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_NavWindowingDimBg, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));
    theme2.Add(ImGuiCol_ModalWindowDimBg, CreateTHEXA(theme2Colors.Values['darkerGrey'], 1));

    for i := 0 to theme2.Count - 1 do
      begin
        HEXAJsonArray := TJSONArray.Create;
        HEXAJsonArray.Add(theme2.Data[i].color);
        HEXAJsonArray.Add(theme2.Data[i].alpha);

        colorDefs.Add(IntToStr(theme2.Keys[i]), HEXAJsonArray);
      end;

    themeDef.Add('colors', colorDefs);


    SetLength(fontDefArray, Length(fontSizes));
    for i := 0 to High(fontSizes) do
    begin
      fontDefArray[i].name := 'roboto-regular';
      fontDefArray[i].size := fontSizes[i];
    end;

    fontDefs := TJSONObject.Create;

    defsArray := TJSONArray.Create;
    for i := 0 to High(fontDefArray) do
    begin
      fontDef := TJSONObject.Create;
      fontDef.Add('name', fontDefArray[i].name);
      fontDef.Add('size', fontDefArray[i].size);

      defsArray.Add(fontDef);
    end;
    fontDefs.Add('defs', defsArray);

    init(
      './assets',
      PChar(fontDefs.AsJSON),
      PChar(themeDef.AsJSON),
      @MyOnInit,
      @MyOnTextChanged,
      @MyOnComboChanged,
      @MyOnNumericValueChanged,
      @MyOnBooleanValueChanged,
      @MyOnMultipleNumericValuesChanged,
      @MyOnClick
    );

    WriteLn('Press CTRL+C to exit.');

    while True do
    begin
      Sleep(1000);
    end;
  finally
    fontDefs.Free;
    theme2Colors.Free;
    theme2.Free;
  end;
end.
