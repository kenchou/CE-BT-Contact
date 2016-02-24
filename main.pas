unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, Grids, DbCtrls, ExtCtrls, CsvDocument,
  stringgridutil, dbgridutil, lconvencoding, IniFiles, LazUTF8;

type

  { TFormMain }

  TFormMain = class(TForm)
    btnImport: TButton;
    btnExit: TButton;
    btnRemove: TButton;
    DataSource1: TDataSource;
    grdTargetView: TDBGrid;
    lstLog: TListBox;
    grpPairedDevice: TRadioGroup;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    grdSourceView: TStringGrid;
    procedure btnExitClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure grpPairedDeviceSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FBtConfigFile: string;
    FBtDbFile: string;
    FCsvFile: string;
    FCsvContact: TCSVDocument;
    FDeviceList: TStringList;
    procedure CheckRequired(AExpr: Boolean; AMessage: string);
    procedure RefreshContact(ADeviceName: string='');
    procedure CleanContact(ADeviceName: string='');
    procedure PopulateDevice;
    procedure PopulateDeviceFromConfig;
    function GetTableMaxId(aTableName: string): Integer;
    function GetSelectedDevice(): string;
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;

const
  SBtAppPath = '\BT Disk\CeApp\BT';
  SSearchPath = '\SDMemory2;\SDMemory3';
  SBtSqliteDb = 'BT.db';
  SBtConfigFile = 'BlueTooth.ini';
  SSourceFile = 'contact.csv';

implementation

{$R *.lfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Left := 0;
  Top := 25;

  FDeviceList := TStringList.Create;

  // Detect DB & CSV path
  FBtDbFile := ConcatPaths([SBtAppPath, SBtSqliteDb]);
  FBtConfigFile := ConcatPaths([SBtAppPath, SBtConfigFile]);
  {$IfDef Win32}
  FBtDbFile := ConcatPaths([Application.Location, FBtDbFile]);
  FBtConfigFile := ConcatPaths([Application.Location, FBtConfigFile]);
  {$EndIf}
  FCsvFile := ExpandFileNameUTF8(FileSearchUTF8(SSourceFile, Application.Location + ';' + SSearchPath));

//  lstLog.Items.Clear;
  lstLog.Items.Add('Temp Path: ' + GetTempDir);
  lstLog.Items.Add('BT Ini File: ' + FBtConfigFile);
  lstLog.Items.Add('BT Db File: ' + FBtDbFile);
  lstLog.Items.Add('Csv File: ' + FCsvFile);

  // Check BlueTooth.ini
  CheckRequired(FileExistsUTF8(FBtConfigFile), '没在指定位置找到蓝牙配置文件, 这车不是秦？' + FBtDbFile);
  // Check BT.db
  CheckRequired(FileExistsUTF8(FBtDbFile), '没找到通讯录，这车不是秦？' + FBtDbFile);
  // Check contact.csv
  CheckRequired(FileExistsUTF8(FCsvFile), '没找到要导入的通讯录 CSV 文件。请将 Contact.csv 文件放置在 TF 卡根目录' + FCsvFile);

  // Connecte to SQLite
  SQLite3Connection1.DatabaseName := FBtDbFile;
  SQLite3Connection1.Open;

  //PopulateDevice;
  PopulateDeviceFromConfig;
  //RefreshContact(GetSelectedDevice);

  // Load Contact.csv
  FCsvContact := TCSVDocument.Create;
  FCsvContact.LoadFromFile(FCsvFile);
  LoadGridFromCSVDocument(grdSourceView, FCsvContact);

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FCsvContact.Free;
  FDeviceList.Free;
end;

procedure TFormMain.grpPairedDeviceSelectionChanged(Sender: TObject);
begin
  RefreshContact(GetSelectedDevice);
end;

procedure TFormMain.CheckRequired(AExpr: Boolean; AMessage: string);
begin
  if not AExpr then
  begin
    ShowMessage(AMessage);
    Application.ShowMainForm := False;
    Application.Terminate;
    Close;
  end;
end;

procedure TFormMain.btnImportClick(Sender: TObject);
var
  LSqlText: string;
  LDeviceName: string;
  LRow: integer;
  LCount: integer;
  ID: Integer;
begin
  if grpPairedDevice.ItemIndex = -1 then
  begin
    ShowMessage('请先选择一个已配对的设备');
    Exit;
  end;

  LDeviceName := GetSelectedDevice();

  // Ignored unsaved data on dbgrid and start new transaction
  SQLTransaction1.Rollback;

  ID := GetTableMaxId('Contact');
  SQLTransaction1.StartTransaction;
  LSqlText := 'INSERT INTO Contact (ID, DeviceName, Name, PhoneNum) VALUES (:ID, :DeviceName, :Name, :PhoneNum)';
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := LSqlText;
  LCount := 0;
  for LRow := 1 to FCsvContact.RowCount - 1 do
  begin
    ID := ID + 1;
    SQLQuery1.ParamByName('ID').AsInteger := ID;
    SQLQuery1.ParamByName('DeviceName').AsString := LDeviceName;
    SQLQuery1.ParamByName('Name').AsString := FCsvContact.Cells[0, LRow];
    SQLQuery1.ParamByName('PhoneNum').AsString := FCsvContact.Cells[1, LRow];
    SQLQuery1.ExecSQL;
    LCount := LCount + 1;
  end;
  SQLTransaction1.Commit;

  RefreshContact(LDeviceName);
  lstLog.Items.Add('"' + LDeviceName + '" 导入 ' + IntToStr(LCount) + ' 条记录.');
  ShowMessage('完成! 共导入 ' + IntToStr(LCount) + ' 条记录.');
end;

procedure TFormMain.btnRemoveClick(Sender: TObject);
var
  LDeviceName: string;
begin
  LDeviceName := GetSelectedDevice;
  if mrYes = MessageDlg('删除操作','你想要删除车上 "' + LDeviceName + '" 的通讯录吗？',
                        mtConfirmation, [mbYes, mbNo],0) then
  begin
    CleanContact(LDeviceName);
    RefreshContact(LDeviceName);
  end;
end;

function TFormMain.GetTableMaxId(aTableName: string): Integer;
var
  LSqlText: string;
begin
  LSqlText := Format('SELECT MAX(ID) FROM %s', [aTableName]);
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Text := LSqlText;
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
    if grpPairedDevice.ItemIndex = -1 then
       Result := ''
    else
       Result := FDeviceList[grpPairedDevice.ItemIndex];
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

  grpPairedDevice.Items.Clear;
  for Field in SQLQuery1.Fields do
  begin
    if (Field.FieldName = 'ID') or (Field.AsString = '')
        or (Field.AsString = '插装车间')
        or (Field.AsString = '我们的') then continue;
    grpPairedDevice.Items.Add(Field.AsString);
    FDeviceList.Add(Field.AsString);
  end;
  if grpPairedDevice.Items.Count > 0 then
  begin
    grpPairedDevice.ItemIndex := 0;
  end;
  SQLQuery1.Close;
  SQLTransaction1.Commit;
end;

procedure TFormMain.PopulateDeviceFromConfig;
var
  LTmpFile: string;
  LStringList: TStringList;
  LSection: string;
  LDeviceAddr: string;
  LDeviceName: string;
  LIniFile: TIniFile;
  LGuessEncoding: string;
  Str: string;
  i: integer;
begin
  LTmpFile := GetTempFileName('', SBtConfigFile);

  // for Test, Read Unicode File
  LStringList := TStringList.Create;
  LStringList.TextLineBreakStyle := TTextLineBreakStyle.tlbsLF;
  try
    LStringList.LoadFromFile(FBtConfigFile);
    Str := LStringList.Text;
    LGuessEncoding := GuessEncoding(Str);
    lstLog.Items.Add('BlueTooth.ini Encoding(Guess): ' + LGuessEncoding);
    Str := ConvertEncoding(Str, LGuessEncoding, EncodingUTF8);
    LStringList.Text := Utf8BomToUtf8(Str);
    LStringList.SaveToFile(LTmpFile);
  finally
    LStringList.Free;
  end;

  grpPairedDevice.Items.Clear;
  lstLog.Items.Add('Device List from ' + SBtConfigFile + ':');

  LIniFile := TIniFile.Create(LTmpFile);
  try
    for i := 1 to 12 do
    begin
      LSection := 'RemoteDevice' + IntToStr(i);
      LDeviceName := LIniFile.ReadString(LSection, 'DevName', '');
      LDeviceAddr := LIniFile.ReadString(LSection, 'DevAddr', '');
      if LDeviceAddr = '' then continue;
      FDeviceList.Add(LDeviceName);
      grpPairedDevice.Items.Add(LDeviceName + '(' + LDeviceAddr + ')');
      lstLog.Items.Add(LDeviceAddr + ' = ' + LDeviceName);
    end;
  finally
    LIniFile.Free;
    DeleteFileUtf8(LTmpFile);
  end;

  if grpPairedDevice.Items.Count > 0 then
  begin
    grpPairedDevice.ItemIndex := 0;
  end;
end;

procedure TFormMain.RefreshContact(ADeviceName: string='');
begin
  if not SQLTransaction1.Active then
     SQLTransaction1.StartTransaction;
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Add('SELECT * FROM Contact');
  if ADeviceName <> '' then
  begin
    SQLQuery1.SQL.Add('WHERE DeviceName = :DeviceName');
    SQLQuery1.ParamByName('DeviceName').AsString := ADeviceName;
  end;
  SQLQuery1.Open;
  SQLTransaction1.Commit;
  SQLQuery1.Active := True;

  grdTargetView.AutoSizeColumns;
  grdTargetView.Columns[1].Width := 80;
  AutoStretchDBGridColumns(grdTargetView, [0, 1, 2, 3], [20, 80, 50, 150]);
end;

procedure TFormMain.CleanContact(ADeviceName: string='');
begin
  SQLQuery1.SQL.Clear;
  SQLQuery1.SQL.Add('DELETE FROM Contact');
  if ADeviceName <> '' then
  begin
    SQLQuery1.SQL.Add('WHERE DeviceName = :DeviceName');
    SQLQuery1.ParamByName('DeviceName').AsString := ADeviceName;
  end;
  SQLQuery1.ExecSQL;
  SQLTransaction1.Commit;
end;

procedure TFormMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

end.

