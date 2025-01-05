program XFrames;

{$mode objfpc}{$H+}

uses
  fpjson;

type
  TFontDef = record
    name: String;
    size: Integer;
  end;

var
  fontDef: TJSONObject;
  fontDefs: TJSONObject;
  defsArray: TJSONArray;
  fontDefArray: array of TFontDef;
  sizes: array of Integer;
  i: Integer;

begin
  sizes := [16, 18, 20, 24, 28, 32, 36, 48];

  SetLength(fontDefArray, Length(sizes));
  for i := 0 to High(sizes) do
  begin
    fontDefArray[i].name := 'roboto-regular';
    fontDefArray[i].size := sizes[i];
  end;

  fontDefs := TJSONObject.Create;
  try
    defsArray := TJSONArray.Create;
    for i := 0 to High(fontDefArray) do
    begin
      fontDef := TJSONObject.Create;
      fontDef.Add('name', fontDefArray[i].name);
      fontDef.Add('size', fontDefArray[i].size);

      defsArray.Add(fontDef);
    end;
    fontDefs.Add('defs', defsArray);

    WriteLn('Generated JSON: ', fontDefs.AsJSON);
  finally
    fontDefs.Free;
  end;
end.
