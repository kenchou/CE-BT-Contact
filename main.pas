unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, CSVDocument;

type

  { TFormMain }

  TFormMain = class(TForm)
      Button1: TButton;
      DataSource1: TDataSource;
      DBGrid1: TDBGrid;
      Label1: TLabel;
      OpenDialog1: TOpenDialog;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
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
  Label1.Caption := 'Current Path: ' + GetCurrentDir + sLineBreak
                  + 'App Location: ' + Application.Location;
  SQLite3Connection1.DatabaseName := Application.Location + '\BT.db';
  SQLite3Connection1.Open;
  SQLQuery1.Active := True;
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

end.

