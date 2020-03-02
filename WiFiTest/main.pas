unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, lNetComponents, Forms, Controls, Graphics,
  Dialogs, StdCtrls, IniPropStorage, lNet;

type

  { TFMain }

  TFMain = class(TForm)
    BSend: TButton;
    EditIP: TEdit;
    EditSend: TEdit;
    IniPropStorage: TIniPropStorage;
    Label1: TLabel;
    MemoIn: TMemo;
    UDP: TLUDPComponent;
    procedure BSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UDPReceive(aSocket: TLSocket);
  private

  public
    port: integer;
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FormShow(Sender: TObject);
begin
  if Udp.Listen(port) then begin
    MemoIn.Append(format('Listening on Port %d', [port]));
  end else begin
    MemoIn.Append(format('Error opening Port %d', [port]));
  end;
end;

procedure TFMain.UDPReceive(aSocket: TLSocket);
var msg: string;
begin
  UDP.GetMessage(msg);
  MemoIn.Append(format('From (%s:%d) "%s"', [aSocket.PeerAddress, aSocket.PeerPort, msg]));

  if msg = 'IWP' then begin
    UDP.SendMessage(EditSend.Text, format('%s:%d', [aSocket.PeerAddress, aSocket.PeerPort]));
  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  port := $AF20;
end;

procedure TFMain.BSendClick(Sender: TObject);
begin
  UDP.SendMessage(EditSend.Text, EditIP.Text + format(':%d', [port]));
end;

end.

