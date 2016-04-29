unit LEDmachines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage, ExtCtrls, SdpoSerial, LCLType, LCLIntf;

const
  LED_red = 4;
  LED_green = 2;
  LED_blue = 1;

type

  TMachineState = record
    LEDColor: integer;
    inside: boolean;
    ReqLEDColor: integer;
    ReqColorCycle: boolean;

    InTime: double;
    BoxLED: TShape;
    EditTime: TEdit;
  end;

  { TFLedMachines }

  TFLedMachines = class(TForm)
    BCloseCom: TButton;
    BOpenCom: TButton;
    BSend: TButton;
    BRandomize: TButton;
    CBRawDebug: TCheckBox;
    CBTimer: TCheckBox;
    CBStationType: TComboBox;
    EditM0Time: TEdit;
    EditM1Time: TEdit;
    EditM2Time: TEdit;
    EditM3Time: TEdit;
    EditM4Time: TEdit;
    EditSend: TEdit;
    EditComPort: TEdit;
    IniPropStorage: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Memo: TMemo;
    Serial: TSdpoSerial;
    ShapeLED1: TShape;
    ShapeLED2: TShape;
    ShapeLED3: TShape;
    ShapeLED0: TShape;
    ShapeLED4: TShape;
    ShapeLEDRand: TShape;
    IPCServer: TSimpleIPCServer;
    Timer: TTimer;
    procedure BCloseComClick(Sender: TObject);
    procedure BOpenComClick(Sender: TObject);
    procedure BRandomizeClick(Sender: TObject);
    procedure BSendClick(Sender: TObject);
    procedure CBStationTypeChange(Sender: TObject);
    procedure CBTimerChange(Sender: TObject);
    procedure EditSendKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IPCServerMessage(Sender: TObject);
    procedure SerialRxData(Sender: TObject);
    procedure ShapeLEDMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
  private
    procedure ActivateComPort(open: boolean; Edit: TEdit);
    procedure Debug(s: string);
    procedure processFrame(channel: char; value: integer);
    procedure ReceiveData(s: string);
    procedure SendChannel(channel: char; value: integer);
    { private declarations }
  public
    serialData: string;

    channel: char;
    frame: integer;
    frameData: string;

    max_machine_idx: integer;
    StationType: string;
    RandStart: boolean;
    RandColor, RandAnimation: integer;

    machine_idx, act_machine_idx: integer;
    MachineStates: array[0..4] of TMachineState;

  end;

var
  FLedMachines: TFLedMachines;

implementation

//{$IFDEF Windows}Windows, ShellApi,{$ENDIF}

{$R *.lfm}

{ TFLedMachines }

procedure TFLedMachines.ActivateComPort(open: boolean; Edit: TEdit);
begin
  if open then begin
    Serial.Device := Edit.Text;
    Serial.Open;
    SerialData := '';
  end else begin
    Serial.close;
  end;

  if Serial.Active then begin
    Edit.Color := clgreen;
  end else begin
    Edit.Color := clSkyBlue;
  end;
end;

procedure TFLedMachines.FormCreate(Sender: TObject);
var path: string;
begin
  DefaultFormatSettings.DecimalSeparator := '.';

  path := ExtractFilePath(Application.ExeName) + DirectorySeparator;
  if Paramcount > 0 then begin
    IniPropStorage.IniFileName := path + ParamStr(1);
    caption := caption + ': ' + ParamStr(1);
  end else begin
    IniPropStorage.IniFileName := path + 'config.ini';
  end;

  max_machine_idx := 3;
  with MachineStates[0] do begin
    BoxLED := ShapeLED0;
    EditTime := EditM0Time;
  end;
  with MachineStates[1] do begin
    BoxLED := ShapeLED1;
    EditTime := EditM1Time;
  end;
  with MachineStates[2] do begin
    BoxLED := ShapeLED2;
    EditTime := EditM2Time;
  end;
  with MachineStates[3] do begin
    BoxLED := ShapeLED3;
    EditTime := EditM3Time;
  end;
  with MachineStates[4] do begin
    BoxLED := ShapeLED4;
    EditTime := EditM4Time;
  end;

  Randomize;
  RandColor := 1;
  RandAnimation := 0;
end;

procedure TFLedMachines.BOpenComClick(Sender: TObject);
begin
  ActivateComPort(true, EditComPort);
end;

procedure TFLedMachines.BRandomizeClick(Sender: TObject);
begin
  RandStart := true;
  RandAnimation := 5;
  //Debug('Rand');
end;

procedure TFLedMachines.BSendClick(Sender: TObject);
begin
  Serial.WriteData(EditSend.Text);
end;



procedure TFLedMachines.CBStationTypeChange(Sender: TObject);
var i: integer;
begin
  case CBStationType.ItemIndex of
    // Machines Type A
    0: begin
      max_machine_idx := 3;
      StationType := 'MA';
      if IPCServer.Active then IPCServer.StopServer;
    end;
    // Machines Type B
    1: begin
      max_machine_idx := 3;
      StationType := 'MB';
      if IPCServer.Active then IPCServer.StopServer;
    end;
    // Incoming Warehouse - 1st
    2: begin
      max_machine_idx := 4;
      StationType := 'I1';
      if IPCServer.Active then IPCServer.StopServer;
    end;
    // Incoming Warehouse - 2nd
    3: begin
      max_machine_idx := 4;
      StationType := 'I2';
      RandStart := true;
      IPCServer.ServerID := 'PCM_IN23';
      IPCServer.StartServer;
    end;
    // Incoming Warehouse - 3rd
    4: begin
      max_machine_idx := 4;
      StationType := 'I3';
      RandStart := true;
      IPCServer.ServerID := 'PCM_IN23';
      IPCServer.StartServer;
    end;
    // Outgoing WareHouse
    5: begin
      max_machine_idx := 4;
      StationType := 'OU';
      if IPCServer.Active then IPCServer.StopServer;
    end;
    // Test
    6: begin
      max_machine_idx := 4;
      StationType := 'CY';
      if IPCServer.Active then IPCServer.StopServer;
    end;
    7: begin
      max_machine_idx := 4;
      StationType := 'TE';
      if IPCServer.Active then IPCServer.StopServer;
    end;
  end;

  for i:= 0 to max_machine_idx do begin
    MachineStates[i].ReqLEDColor := 0;
  end;

end;

procedure TFLedMachines.CBTimerChange(Sender: TObject);
begin
  Timer.Enabled := CBTimer.Checked;
end;

procedure TFLedMachines.EditSendKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_RETURN then BSend.Click();
end;

procedure TFLedMachines.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IniPropStorage.WriteInteger('left', left);
  IniPropStorage.WriteInteger('top', top);
  IniPropStorage.WriteBoolean('ComPortOpen', Serial.Active);
  Serial.Close;
end;

procedure TFLedMachines.BCloseComClick(Sender: TObject);
begin
  ActivateComPort(false, EditComPort);
end;

procedure TFLedMachines.FormShow(Sender: TObject);
begin
  IniPropStorage.Restore;
  left := IniPropStorage.ReadInteger('Left', left);
  top := IniPropStorage.ReadInteger('top', top);

  ActivateComPort(IniPropStorage.ReadBoolean('ComPortOpen', false), EditComPort);

  CBTimerChange(Sender);
  CBStationTypeChange(Sender);

  //IPCServer.ServerID := 'PCM';
  //IPCServer.StartServer;

end;

procedure TFLedMachines.IPCServerMessage(Sender: TObject);
var mess: string;
begin
  mess := IPCServer.StringMessage;
  if mess = 'PS' then begin
    BRandomize.Click();
  end;
  //Debug(mess);
end;

procedure TFLedMachines.Debug(s: string);
begin
  Memo.Lines.Add(s);
  while Memo.Lines.Count > 200 do begin
    Memo.Lines.Delete(0);
  end;
end;

function isHexDigit(c: char): boolean;
begin
  result := c in ['0'..'9', 'A'..'F'];
end;


procedure TFLedMachines.SerialRxData(Sender: TObject);
var s: string;
begin
  s := Serial.ReadData;

  ReceiveData(s);
  if CBRawDebug.Checked then Debug(s);
end;

procedure TFLedMachines.ShapeLEDMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i: integer;
begin
  i := (Sender as TShape).Tag;
  MachineStates[i].ReqColorCycle := true;
end;


procedure TFLedMachines.ReceiveData(s: string);
var //b: byte;
    c: char;
    value: integer;
begin
  if s = '' then exit;
  serialData := serialData + s;

  if CBRawDebug.Checked then begin
    Debug(s);
  end;

  while Length(serialData) > 0 do begin
    c := serialData[1];
    serialData := copy(serialData, 2, maxint);
    if frame = -1 then begin
      if (c in ['G'..']']) or (c in ['g'..'}']) or (c = ';') then begin
        frame := 0;
        channel := c;
        frameData := '';
      end;
    end else begin
      if isHexDigit(c) then begin
        frameData := frameData + c;
        inc(frame);
        if frame = 2 then begin
          value := StrToIntDef('$' + frameData, -1);
          processFrame(channel, value);
          frame := -1;
        end;
      end else begin
        frame := -1;
      end;
    end;
  end;
end;


procedure RandomSwap(var a: integer; var b: integer);
var t: integer;
begin
  if Random(10) < 5 then begin
    t := a;
    a := b;
    b := t;
  end;
end;

procedure SureSwap(var a: integer; var b: integer);
var t: integer;
begin
  t := a;
  a := b;
  b := t;
end;


procedure TFLedMachines.processFrame(channel: char; value: integer);
var newColor: TColor;
    delta: double;
    i, c: integer;
begin
  if channel = 'p' then begin // Metapacket delimiter
    if value = 0 then begin

    end;
  end else if channel in ['t'] then begin
    newColor := RGBToColor((value and $4) shl 5, (value and $2) shl 6, (value and $1) shl 7);
    with MachineStates[act_machine_idx] do begin
      //if LEDColor <> value then begin
      if (LEDColor = -1) and (value <> -1) then begin
        InTime := gettickcount();
      end;
      LEDColor := value;
    end;
  end else if channel in ['N'] then begin
    act_machine_idx := value;
  end else if channel in ['U'] then begin
    if value > 5 then begin
      MachineStates[act_machine_idx].inside := false;
      MachineStates[act_machine_idx].LEDColor := -1;
      delta := 0;
    end else begin
      MachineStates[act_machine_idx].inside := true;
      delta := GetTickCount() - MachineStates[act_machine_idx].InTime;
    end;

    with MachineStates[act_machine_idx] do begin
      if StationType = 'TE' then begin
        // Cycle Led Color after 2 seconds
        if ReqColorCycle then begin
          c := LEDColor shl 1;
          if c > 4 then c := 1;
          SendChannel('l', c);
          ReqColorCycle := false;
        end;
      end;

      if StationType = 'CY' then begin
        // Cycle Led Color after 2 seconds
        if (delta > 1500) or ReqColorCycle then begin
          InTime := gettickcount();
          c := LEDColor shl 1;
          if c > 4 then c := 1;
          SendChannel('l', c);
          ReqColorCycle := false;
        end;
      end;

      if StationType = 'MA' then begin
        // Change Green Parts to Blue Parts after 30s
        if (delta > 29000) and (LEDColor = LED_green) then begin
          SendChannel('l', LED_blue);
        end;
      end;

      if StationType = 'MB' then begin
        // Change Red Parts to Green Parts after 30s
        if (delta > 29000) and (LEDColor = LED_red) then begin
          SendChannel('l', LED_green);
        end;
      end;

      if StationType = 'I1' then begin
        // Change any Parts to Blue Parts
        SendChannel('l', LED_blue);
      end;

      if StationType = 'I2' then begin
        // Prepare the random initial configuration
        if RandStart then begin
          MachineStates[0].ReqLEDColor := LED_blue;
          MachineStates[1].ReqLEDColor := LED_blue;
          MachineStates[2].ReqLEDColor := LED_blue;
          MachineStates[3].ReqLEDColor := LED_blue;
          MachineStates[4].ReqLEDColor := LED_green;
          // Now the random part
          MachineStates[1 + random(3)].ReqLEDColor := LED_green;
          // OK! Done...
          RandStart := false;
        end;
        // Update the Box Led
        if ReqLEDColor <> LEDColor then begin
          SendChannel('l', ReqLEDColor);
        end;
      end;

      if StationType = 'I3' then begin
        // Prepare the random initial configuration
        if RandStart then begin
          MachineStates[0].ReqLEDColor := LED_blue;
          MachineStates[1].ReqLEDColor := LED_green;
          MachineStates[2].ReqLEDColor := LED_green;
          MachineStates[3].ReqLEDColor := LED_red;
          MachineStates[4].ReqLEDColor := LED_blue;
          // Now the random part
          for i := 0 to 63 do begin
            SureSwap(MachineStates[1 + random(4)].ReqLEDColor, MachineStates[1 + random(4)].ReqLEDColor);
          end;

          // OK! Done...
          RandStart := false;
        end;
        // Update the Box Led
        if ReqLEDColor <> LEDColor then begin
          SendChannel('l', ReqLEDColor);
        end;
      end;

    end;
  end;
end;


procedure TFLedMachines.SendChannel(channel: char; value: integer);
begin
  if Serial.Active then begin
    Serial.WriteData(channel + IntToHex(value, 2));
  end;
end;


procedure TFLedMachines.TimerTimer(Sender: TObject);
var i: integer;
    delta: double;
begin
  inc(machine_idx);
  if machine_idx > max_machine_idx then machine_idx := 0;
  SendChannel('M', machine_idx);
  SendChannel('N', 0);
  //Serial.WriteData('t00');
  SendChannel('t', 0);
  SendChannel('U', machine_idx);

  for i := 0 to max_machine_idx do begin
    with MachineStates[i] do begin
      if inside then begin
        delta := GetTickCount() - InTime;
        BoxLED.Brush.Color := RGBToColor((LEDColor and $4) shl 5, (LEDColor and $2) shl 6, (LEDColor and $1) shl 7);;
      end else begin
        delta := 0;
        BoxLED.Brush.Color := clSilver;
      end;
      BoxLED.pen.Color := RGBToColor((reqLEDColor and $4) shl 5, (reqLEDColor and $2) shl 6, (reqLEDColor and $1) shl 7);;
      EditTime.Text := FormatFloat('###0.00',1e-3 * delta);
    end;
  end;

  if (RandAnimation > 0) then begin
    ShapeLEDRand.Brush.Color := RGBToColor((RandColor and $4) shl 5, (RandColor and $2) shl 6, (RandColor and $1) shl 7);;
    RandColor := RandColor shl 1;
    if RandColor > 4 then RandColor := 1;
    dec(RandAnimation);
    //ShapeLEDRand.Paint;
  end else begin
    ShapeLEDRand.Brush.Color := clGray;
  end;
end;

end.

