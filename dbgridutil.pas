unit dbgridutil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, math;

procedure AutoStretchDBGridColumns(Grid: TDBGrid; Columns, MinWidths: Array of integer);

implementation

procedure AutoStretchDBGridColumns(Grid: TDBGrid; Columns, MinWidths: Array of integer);
var
  x, i, ww: integer;
begin
  // Stretches TDBGrid columns
  // Columns contains columns to stretch
  // MinWidths contains columns minimum widhts
  // To stretch grids columns 1,2 and 5 automatically and set minimum widths to 80, 150 and 150 call
  // AutoStretchDBGridColumns(DBGrid1, [1,2,5], [80, 150, 150]);
  Assert(Length(Columns) = Length(MinWidths), 'Length(Columns) <> Length(MinWidths)');
  ww := 0;
  for i := 0 to Grid.Columns.Count - 1 do
  begin
    if Grid.Columns[i].Visible then
      ww := ww + Grid.Columns[i].Width + 1; //** +1 for grid line width
  end;
  if dgIndicator in Grid.Options then
    ww := ww + 12;
  x := (Grid.ClientWidth - ww) div Length(Columns);
  for i := 0 to  High(Columns) do
    Grid.Columns[Columns[i]].Width := Max(Grid.Columns[Columns[i]].Width + x, MinWidths[i]);
end;
end.

