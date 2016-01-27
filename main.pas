unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, Grids, CsvDocument, stringgridutil, dbgridutil;

type

  { TFormMain }

  TFormMain = class(TForm)
    Button1: TButton;
    BtnImport: TButton;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure BtnImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    CsvDocumentContact: TCSVDocument;
    BtDbFile: string;
    CsvFile: string;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  BtDbFile := Application.Location + '\BT.db';
  CsvFile := Application.Location + '\contact.csv';

  Label1.Caption := 'Current Path: ' + GetCurrentDir + sLineBreak
                  + 'App Location: ' + Application.Location;
  // Check BT.db
  if not FileExists(BtDbFile) then
  begin
    ShowMessage('Cannot Found Database: ' + BtDbFile);
    Application.ShowMainForm := False;
    Application.Terminate;
    exit;
  end;
  // Check contact.csv
  if not FileExists(CsvFile) then
  begin
    ShowMessage('Cannot Found CSV: ' + CsvFile);
    Application.ShowMainForm := False;
    Application.Terminate;
    exit;
  end;
  // Load BT.db from BT disk
  SQLite3Connection1.DatabaseName := BtDbFile;
  SQLite3Connection1.Open;
  SQLQuery1.Active := True;
  DBGrid1.AutoSizeColumns;
  DBGrid1.Columns[1].Width := 80;
  AutoStretchDBGridColumns(DBGrid1, [0, 1, 2,3], [20, 10, 80, 150]);
  // Load Contact.csv
  CsvDocumentContact := TCSVDocument.Create;
  CsvDocumentContact.LoadFromFile('contact.csv');
  LoadGridFromCSVDocument(StringGrid1, CsvDocumentContact);
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  filename: string;
begin
if OpenDialog1.Execute then
begin
  filename := OpenDialog1.Filename;
  ShowMessage(filename);
end;
end;

procedure TFormMain.BtnImportClick(Sender: TObject);
var
  insert_sql: string;
  r: integer;
begin
  insert_sql := 'INSERT INTO Contact (DeviceName, Name, PhoneNum) VALUES (?, ?, ?)';
  for r := 0 to CsvDocumentContact.RowCount do
  begin

  end;
end;

end.

