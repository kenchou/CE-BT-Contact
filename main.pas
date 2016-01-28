unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, Grids, DbCtrls, CsvDocument,
  stringgridutil, dbgridutil;

type

  { TFormMain }

  TFormMain = class(TForm)
    Button1: TButton;
    BtnImport: TButton;
    chkClear: TCheckBox;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    DBLookupComboBox1: TDBLookupComboBox;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
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
    procedure RefreshContact(aDeviceName: string='');
    procedure CleanContact(aDeviceName: string='');
    function getMaxId(aTableName: string): integer;
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
  aSQLText: string;
  aDeviceName: string;
  r: integer;
  count: integer;
begin
  aDeviceName := '张小米';
  // Delete record if check the checkbox
  if chkClear.checked then
  begin
    CleanContact(aDeviceName);
  end;

  aSQLText := 'INSERT INTO Contact (DeviceName, Name, PhoneNum) VALUES (:DeviceName, :Name, :PhoneNum)';
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := aSQLText;
  //SQLQuery1.FieldByName('ID').Required:=false;
  //SQLTransaction1.StartTransaction;
  count := 0;
  for r := 1 to CsvDocumentContact.RowCount - 1 do
  begin
    //SQLQuery1.AppendRecord([aDeviceName, CsvDocumentContact.Cells[0, r], CsvDocumentContact.Cells[1, r]]);
    SQLQuery1.ParamByName('DeviceName').AsString := aDeviceName;
    SQLQuery1.ParamByName('Name').AsString := CsvDocumentContact.Cells[0, r];
    SQLQuery1.ParamByName('PhoneNum').AsString := CsvDocumentContact.Cells[1, r];
    SQLQuery1.ExecSQL;
    count := count + 1;
  end;
  SQLTransaction1.Commit;

  RefreshContact;
  ShowMessage('导入完成! 共计 ' + intToStr(count) + ' 条记录.');
end;

function TFormMain.getMaxId(aTableName: string): integer;
var
  aSQLText: string;
begin
  aSQLText := Format('SELECT MAX(ID) FROM %s', [aTableName]);
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := aSQLText;
  SQLQuery1.ExecSQL;
end;

procedure TFormMain.RefreshContact(aDeviceName: string='');
begin
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Add('SELECT * FROM Contact');
  if aDeviceName <> '' then
  begin
    SQLQuery1.SQL.Add('WHERE DeviceName = :DeviceName');
    SQLQuery1.ParamByName('DeviceName').AsString := aDeviceName;
  end;
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;
  SQLQuery1.Active := True;
end;

procedure TFormMain.CleanContact(aDeviceName: string='');
begin
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Add('DELETE FROM Contact');
  if aDeviceName <> '' then
  begin
    SQLQuery1.SQL.Add('WHERE DeviceName = :DeviceName');
    SQLQuery1.ParamByName('DeviceName').AsString := aDeviceName;
  end;
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;
end;

end.

