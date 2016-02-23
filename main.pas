unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, Grids, DbCtrls, CsvDocument,
  stringgridutil, dbgridutil, lconvencoding, IniFiles, LazUTF8;

type

  { TFormMain }

  TFormMain = class(TForm)
    BtnImport: TButton;
    btnExit: TButton;
    chkClear: TCheckBox;
    cboxDevice: TComboBox;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    DBGrid1: TDBGrid;
    Label1: TLabel;
    lstLog: TListBox;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StringGrid1: TStringGrid;
    procedure btnExitClick(Sender: TObject);
    procedure BtnImportClick(Sender: TObject);
    procedure cboxDeviceChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    CsvDocumentContact: TCSVDocument;
    DeviceList: TStringList;
    procedure CheckRequired(expr: Boolean; message: string);
    procedure RefreshContact(aDeviceName: string='');
    procedure CleanContact(aDeviceName: string='');
    procedure PopulateDevice;
    procedure PopulateDeviceFromConfig;
    function GetTableMaxId(aTableName: string): Integer;
    function GetSelectedDevice(): string;
  public
    { public declarations }
    BtIniFile: string;
    BtDbFile: string;
    CsvFile: string;
  end;

var
  FormMain: TFormMain;

const
  BtAppPath = '\BT Disk\CeApp\BT';
  SearchPath = '\SDMemory2;\SDMemory3';

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Left := 0;
  Top := 25;

  DeviceList := TStringList.Create;

  // Detect DB & CSV path
  BtDbFile := ConcatPaths([BtAppPath, 'BT.db']);
  BtIniFile := ConcatPaths([BtAppPath, 'BlueTooth.ini']);

  {$IfDef Win32}
  BtDbFile := ConcatPaths([Application.Location, BtDbFile]);
  BtIniFile := ConcatPaths([Application.Location, BtIniFile]);
  {$EndIf}

  CsvFile := ExpandFileNameUTF8(FileSearchUTF8('contact.csv', Application.Location + ';' + SearchPath));

//  lstLog.Items.Clear;
  lstLog.Items.Add('Temp Path: ' + GetTempDir);
  lstLog.Items.Add('BT Ini File: ' + BtIniFile);
  lstLog.Items.Add('BT Db File: ' + BtDbFile);
  lstLog.Items.Add('Csv File: ' + CsvFile);

  PopulateDeviceFromConfig;

  // Check BlueTooth.ini
  CheckRequired(FileExistsUTF8(BtIniFile), '没在指定位置找到蓝牙配置文件, 这车不是秦？' + BtDbFile);
  // Check BT.db
  CheckRequired(FileExistsUTF8(BtDbFile), '没找到通讯录，这车不是秦？' + BtDbFile);
  // Check contact.csv
  CheckRequired(FileExistsUTF8(CsvFile), '没找到要导入的通讯录 CSV 文件。请将 Contact.csv 文件放置在 TF 卡根目录' + CsvFile);

  // Connecte to SQLite
  SQLite3Connection1.DatabaseName := BtDbFile;
  SQLite3Connection1.Open;

  //PopulateDevice;
  RefreshContact(GetSelectedDevice);

  // Load Contact.csv
  CsvDocumentContact := TCSVDocument.Create;
  CsvDocumentContact.LoadFromFile(CsvFile);
  LoadGridFromCSVDocument(StringGrid1, CsvDocumentContact);

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  CsvDocumentContact.Free;
  DeviceList.Free;
end;

procedure TFormMain.CheckRequired(expr: Boolean; message: string);
begin
  if not expr then
  begin
    ShowMessage(message);
    Application.ShowMainForm := False;
    Application.Terminate;
    Close;
    //halt;
  end;
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

  //aDeviceName := cboxDevice.Items[cboxDevice.ItemIndex];
  aDeviceName := GetSelectedDevice();

  // Delete record if check the checkbox
  if chkClear.checked then
  begin
    CleanContact(aDeviceName);
  end;

  // Ignored unsaved data on dbgrid and start new transaction
  SQLTransaction1.Rollback;

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

  RefreshContact(aDeviceName);
  lstLog.Items.Add('"' + aDeviceName + '" 导入 ' + IntToStr(count) + ' 条记录.');
  ShowMessage('完成! 共导入 ' + IntToStr(count) + ' 条记录.');
end;

procedure TFormMain.cboxDeviceChange(Sender: TObject);
begin
  RefreshContact(DeviceList[cboxDevice.ItemIndex]);
end;

function TFormMain.GetTableMaxId(aTableName: string): Integer;
var
  aSQLText: string;
begin
  aSQLText := Format('SELECT MAX(ID) FROM %s', [aTableName]);
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := aSQLText;
  SQLQuery1.Open;
  if SQLQuery1.Fields[0].AsString = '' then
    Result := 0
  else
    Result := SQLQuery1.Fields[0].AsInteger;
  SQLQuery1.Close;
  SQLTransaction1.Commit;
end;

function TFormMain.GetSelectedDevice(): string;
begin
    Result := DeviceList[cboxDevice.ItemIndex];
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
    DeviceList.Add(Field.AsString);
  end;
  if cboxDevice.Items.Count >= 1 then
  begin
    cboxDevice.ItemIndex := 0;
  end;
  SQLQuery1.Close;
  SQLTransaction1.Commit;
end;

procedure TFormMain.PopulateDeviceFromConfig;
var
  tmpFile: string;
  StrList: TStringList;
  Sections: TStringList;
  Section: string;
  DevAddr: string;
  DevName: string;
  IniFile: TIniFile;
  FromEncoding: string;
  S: string;
begin
  tmpFile := GetTempFileName('', 'BlueTooth.ini');

  // for Test, Read Unicode File
  StrList := TStringList.Create;
  StrList.TextLineBreakStyle := TTextLineBreakStyle.tlbsLF;
  try
    StrList.LoadFromFile(BtIniFile);
    S := StrList.Text;
    FromEncoding := GuessEncoding(S);
    lstLog.Items.Add('BlueTooth.ini Encoding(Guess): ' + FromEncoding);
    S := ConvertEncoding(S, FromEncoding, EncodingUTF8);
    StrList.Text := Utf8BomToUtf8(S);
    StrList.SaveToFile(tmpFile);
  finally
    StrList.Free;
  end;

  lstLog.Items.Add('Device List from BlueTooth.ini:');
    // Iterate sections in ini
  Sections := TStringList.Create;
  IniFile := TIniFile.Create(tmpFile);
  try
    IniFile.ReadSections(Sections);
    for Section in Sections do
    begin
      if 1 = UTF8Pos('RemoteDevice', Section) then
      begin
        DevName := IniFile.ReadString(Section, 'DevName', '');
        DevAddr := IniFile.ReadString(Section, 'DevAddr', '');
        lstLog.Items.Add(DevAddr + ' : ' + DevName);
        cboxDevice.Items.Add(DevName + '(' + DevAddr + ')');
        DeviceList.Add(DevName);
      end;
    end;
  finally
    IniFile.Free;
    Sections.Free;
    DeleteFileUtf8(tmpFile);
  end;

  if cboxDevice.Items.Count >= 1 then
  begin
    cboxDevice.ItemIndex := 0;
  end;
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

procedure TFormMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

end.

