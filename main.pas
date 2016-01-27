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
    CsvDocmentContact: TCSVDocument;
  public
    { public declarations }
  end;

const
  BTDisk = '\BT\CeApp'
var
  FormMain: TFormMain;

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Label1.Caption := 'Current Path: ' + GetCurrentDir + sLineBreak
                  + 'App Location: ' + Application.Location;
  // Load BT.db from BT disk
  SQLite3Connection1.DatabaseName := Application.Location + '\BT.db';
  SQLite3Connection1.Open;
  SQLQuery1.Active := True;
  DBGrid1.AutoSizeColumns;
  DBGrid1.Columns[1].Width := 80;
  AutoStretchDBGridColumns(DBGrid1, [0, 1, 2,3], [20, 10, 80, 150]);
  // Load Contact.csv
  CsvDocmentContact := TCSVDocument.Create;
  CsvDocmentContact.LoadFromFile('contact.csv');
  LoadGridFromCSVDocument(StringGrid1, CsvDocmentContact);
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
begin
  insert_sql := 'INSERT INTO Contact (DeviceName, Name, PhoneNum) VALUES (?, ?, ?)'
end;

end.

