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
    BtnImport: TButton;
    chkClear: TCheckBox;
    cboxDevice: TComboBox;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StringGrid1: TStringGrid;
    procedure BtnImportClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    CsvDocumentContact: TCSVDocument;
    BtDbFile: string;
    CsvFile: string;
    procedure RefreshContact(aDeviceName: string='');
    procedure CleanContact(aDeviceName: string='');
    procedure PopulateDevice;
    function GetTableMaxId(aTableName: string): Integer;
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
  // Detect DB & CSV path
  BtDbFile := Application.Location + '\BT.db';
  CsvFile := Application.Location + '\contact.csv';

  //Label1.Caption := 'Current Path: ' + GetCurrentDir;

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

  PopulateDevice;
  RefreshContact;

  // Load Contact.csv
  CsvDocumentContact := TCSVDocument.Create;
  CsvDocumentContact.LoadFromFile('contact.csv');
  LoadGridFromCSVDocument(StringGrid1, CsvDocumentContact);
end;

procedure TFormMain.PopulateDevice;
var
  Field: TField;
begin
  SQLTransaction1.StartTransaction;
  SQLQuery1.Close;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := 'SELECT * FROM Paired';
  SQLQuery1.Open;

  cboxDevice.Items.Clear;
  for Field in SQLQuery1.Fields do
  begin
    if (Field.FieldName = 'ID') or (Field.AsString = '')
        or (Field.AsString = '插装车间')
        or (Field.AsString = '我们的') then continue;
    cboxDevice.Items.Add(Field.AsString);
  end;
  SQLQuery1.Close;
  SQLTransaction1.Commit;
end;

procedure TFormMain.BtnImportClick(Sender: TObject);
var
  aSQLText: string;
  aDeviceName: string;
  r: integer;
  count: integer;
  ID: Integer;
begin
  if cboxDevice.ItemIndex = -1 then
  begin
    ShowMessage('请先选择一个已配对的设备');
    exit;
  end;
  aDeviceName := cboxDevice.Items[cboxDevice.ItemIndex];

  // Delete record if check the checkbox
  if chkClear.checked then
  begin
    CleanContact(aDeviceName);
  end;

  SQLTransaction1.Rollback;
  //DBGrid1.DataSource := nil;

  ID := GetTableMaxId('Contact');
  SQLTransaction1.StartTransaction;
  aSQLText := 'INSERT INTO Contact (ID, DeviceName, Name, PhoneNum) VALUES (:ID, :DeviceName, :Name, :PhoneNum)';
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := aSQLText;
  count := 0;
  for r := 1 to CsvDocumentContact.RowCount - 1 do
  begin
    ID := ID + 1;
    SQLQuery1.ParamByName('ID').AsInteger := ID;
    SQLQuery1.ParamByName('DeviceName').AsString := aDeviceName;
    SQLQuery1.ParamByName('Name').AsString := CsvDocumentContact.Cells[0, r];
    SQLQuery1.ParamByName('PhoneNum').AsString := CsvDocumentContact.Cells[1, r];
    SQLQuery1.ExecSQL;
    count := count + 1;
  end;
  SQLTransaction1.Commit;

  //DBGrid1.DataSource := DataSource1;
  RefreshContact;
  ShowMessage('导入完成! 共计 ' + IntToStr(count) + ' 条记录.');
end;

function TFormMain.GetTableMaxId(aTableName: string): Integer;
var
  aSQLText: string;
begin
  aSQLText := Format('SELECT MAX(ID) FROM %s', [aTableName]);
  //SQLTransaction1.StartTransaction;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := aSQLText;
  SQLQuery1.Open;
  GetTableMaxId := SQLQuery1.Fields[0].AsInteger;
  //ShowMessage(SQLQuery1.Fields[0].FieldName + ' Value:' + SQLQuery1.Fields[0].AsString);
  SQLQuery1.Close;
  SQLTransaction1.Commit;
end;

procedure TFormMain.RefreshContact(aDeviceName: string='');
begin
  if not SQLTransaction1.Active then
     SQLTransaction1.StartTransaction;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Add('SELECT * FROM Contact');
  if aDeviceName <> '' then
  begin
    SQLQuery1.SQL.Add('WHERE DeviceName = :DeviceName');
    SQLQuery1.ParamByName('DeviceName').AsString := aDeviceName;
  end;
  SQLQuery1.Open;
  SQLTransaction1.Commit;
  SQLQuery1.Active := True;

  DBGrid1.AutoSizeColumns;
  DBGrid1.Columns[1].Width := 80;
  AutoStretchDBGridColumns(DBGrid1, [0, 1, 2, 3], [20, 80, 50, 150]);

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

