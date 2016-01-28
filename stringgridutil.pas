unit stringgridutil;
{ Use csvdocument to import csv data into stringgrid. Offers more robust support for CSV file variations than TStringGrid.LoadFromCSVFile }

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, Grids, CSVDocument, FileUtil;

procedure LoadGridFromCSVFile(Grid: TStringGrid;AFilename: string;
  ADelimiter:Char=','; WithHeader:boolean=true; AddRows:boolean=true);
// Loads (quasi)CSV document in AFilename into Grid, using ADelimiter as
// delimiter. If WithHeader is true, it won't import the first row.
// If AddRows is true, it will add rows if the existing grid is too short.
procedure LoadGridFromCSVDocument(Grid: TStringGrid; CsvDocument: TCsvDocument;
                                  WithHeader:boolean=true;AddRows:boolean=true);

implementation

procedure LoadGridFromCSVFile(Grid: TStringGrid;AFilename: string;
  ADelimiter:Char=','; WithHeader:boolean=true;AddRows:boolean=true);
const
  DefaultRowCount=10; //Number of rows to show by default
var
  FileStream: TFileStream;
  Parser: TCSVParser;
  RowOffset: integer;
begin
  Grid.BeginUpdate;
  // Reset the grid:
  Grid.Clear;
  Grid.RowCount:=DefaultRowCount;
  Grid.ColCount:=6; //Vaguely sensible
  if not(FileExistsUTF8(AFileName)) then exit;

  Parser:=TCSVParser.Create;
  FileStream := TFileStream.Create(AFilename, fmOpenRead+fmShareDenyWrite);
  try
    Parser.Delimiter:=ADelimiter;
    Parser.SetSource(FileStream);

    // If the grid has fixed rows, those will not receive data, so we need to
    // calculate the offset
    RowOffset:=Grid.FixedRows;
    // However, if we have a header row in our CSV data, we need to
    // discount that
    if WithHeader then RowOffset:=RowOffset-1;

    while Parser.ParseNextCell do
    begin
      // Stop if we've filled all existing rows. Todo: check for fixed grids etc, but not relevant for our case
      if AddRows=false then
        if Parser.CurrentRow+1>Grid.RowCount then break; //VisibleRowCount doesn't seem to work.

      // Widen grid if necessary. Slimming the grid will come after import done.
      if Grid.Columns.Enabled then
      begin
        if Grid.Columns.VisibleCount<Parser.CurrentCol+1 then Grid.Columns.Add;
      end
      else
      begin
        if Grid.ColCount<Parser.CurrentCol+1 then Grid.ColCount:=Parser.CurrentCol+1;
      end;

      // If header data found, and a fixed row is available, set the caption
      if (WithHeader) and
        (Parser.CurrentRow=0) and
        (Parser.CurrentRow<Grid.FixedRows-1) then
      begin
        // Assign header data to the first fixed row in the grid:
        Grid.Columns[Parser.CurrentCol].Title.Caption:=Parser.CurrentCellText;
      end;

      // Actual data import into grid cell, minding fixed rows and header
      if Grid.RowCount<Parser.CurrentRow+1 then
        Grid.RowCount:=Parser.CurrentRow+1;
      Grid.Cells[Parser.CurrentCol,Parser.CurrentRow+RowOffset]:=Parser.CurrentCellText;
    end;

    // Now we know the widest row in the import, we can snip the grid's
    // columns if necessary.
    if Grid.Columns.Enabled then
    begin
      while Grid.Columns.VisibleCount>Parser.MaxColCount do
      begin
        Grid.Columns.Delete(Grid.Columns.Count-1);
      end;
    end
    else
    begin
      if Grid.ColCount>Parser.MaxColCount then
        Grid.ColCount:=Parser.MaxColCount;
    end;

  finally
    Parser.Free;
    FileStream.Free;
    Grid.EndUpdate;
  end;
end;


procedure LoadGridFromCSVDocument(Grid: TStringGrid; CsvDocument: TCsvDocument;
                                  WithHeader:boolean=true;AddRows:boolean=true);
const
  DefaultRowCount=10; //Number of rows to show by default
var
  MaxRowCount: integer;
  MaxColCount: integer;
  RowOffset: integer;
  r: integer;
  c: integer;
begin
  Grid.BeginUpdate;
  // Reset the grid:
  Grid.Clear;
  Grid.RowCount:=DefaultRowCount;
  Grid.ColCount:=6; //Vaguely sensible

  try
    // If the grid has fixed rows, those will not receive data, so we need to
    // calculate the offset
    RowOffset:=Grid.FixedRows;
    // However, if we have a header row in our CSV data, we need to
    // discount that
    if WithHeader then RowOffset:=RowOffset-1;

    MaxRowCount := CsvDocument.RowCount;
    MaxColCount := CsvDocument.MaxColCount;

    //while Parser.ParseNextCell do
    for r := 0 to MaxRowCount - 1 do
      for c := 0 to MaxColCount - 1 do
      begin
        // Stop if we've filled all existing rows. Todo: check for fixed grids etc, but not relevant for our case
        if AddRows=false then
          if r+1>Grid.RowCount then break; //VisibleRowCount doesn't seem to work.

        // Widen grid if necessary. Slimming the grid will come after import done.
        if Grid.Columns.Enabled then
        begin
          if Grid.Columns.VisibleCount<c+1 then Grid.Columns.Add;
        end
        else
        begin
          if Grid.ColCount<c+1 then Grid.ColCount:=c+1;
        end;

        // If header data found, and a fixed row is available, set the caption
        if (WithHeader) and
          (r=0) and
          (r<Grid.FixedRows-1) then
        begin
          // Assign header data to the first fixed row in the grid:
          Grid.Columns[c].Title.Caption:=CsvDocument.Cells[r, c];
        end;

        // Actual data import into grid cell, minding fixed rows and header
        if Grid.RowCount<r+1 then
          Grid.RowCount:=r+1;
        Grid.Cells[c,r+RowOffset]:=CsvDocument.Cells[c, r];
      end;

    // Now we know the widest row in the import, we can snip the grid's
    // columns if necessary.
    if Grid.Columns.Enabled then
    begin
      while Grid.Columns.VisibleCount>MaxColCount do
      begin
        Grid.Columns.Delete(Grid.Columns.Count-1);
      end;
    end
    else
    begin
      if Grid.ColCount>MaxColCount then
        Grid.ColCount:=MaxColCount;
    end;

  finally
    Grid.EndUpdate;
  end;
end;

end.

