unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, simpleipc, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLIntf, Menus;

type
  TClockMode = (cmPaused, cmCounting, cmLap);
  { TFMain }

  { TDelayClock }

  TDelayClock = class
  private

  public
    Mode: TClockMode;
    Start: longword;
    Clock: longword;
    LabelClock: TLabel;

    constructor Create;
    destructor Destroy; override;

    procedure ClockStart;
    procedure Pause;
    procedure Continue;
    procedure Reset;
    procedure ShowClock;
  end;

  TFMain = class(TForm)
    BGlobalPause: TButton;
    BGlobalContinue: TButton;
    BPartialStart: TButton;
    BPartialStop: TButton;
    BGlobalReset: TButton;
    BPartialReset: TButton;
    BPartialLap: TButton;
    LabelMA1: TLabel;
    LabelClock: TLabel;
    BGlobalStart: TButton;
    LabelClockPartial: TLabel;
    LabelMA2: TLabel;
    LabelMA3: TLabel;
    LabelMA4: TLabel;
    LabelMA5: TLabel;
    LabelMA6: TLabel;
    LabelMA7: TLabel;
    LabelMA8: TLabel;
    MainMenu: TMainMenu;
    MemoLog: TMemo;
    MenuGlobal: TMenuItem;
    MenuGlobalStart: TMenuItem;
    MenuPartialStart: TMenuItem;
    MenuPartialLap: TMenuItem;
    MenuPartial: TMenuItem;
    IPCClient: TSimpleIPCClient;
    Timer: TTimer;
    procedure BGlobalContinueClick(Sender: TObject);
    procedure BGlobalPauseClick(Sender: TObject);
    procedure BGlobalResetClick(Sender: TObject);
    procedure BGlobalStartClick(Sender: TObject);
    procedure BPartialLapClick(Sender: TObject);
    procedure BPartialResetClick(Sender: TObject);
    procedure BPartialStartClick(Sender: TObject);
    procedure BPartialStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LabelMachineMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure LabelMachineClick(Sender: TObject);
    procedure MenuGlobalStartClick(Sender: TObject);
    procedure MenuPartialLapClick(Sender: TObject);
    procedure MenuPartialStartClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    function getComponentByName(cname: string): TComponent;
    procedure IPCSend(mess: string);
    { private declarations }
  public
    GlobalClockMode, PartialClockMode: TClockMode;
    GlobalClockStart, PartialClockStart: longword;
    GlobalClock, PartialClock: longword;
    GlobalClockStartTime: longword;

    MachineClocks: array [0..7] of TDelayClock;
  end;

var
  FMain: TFMain;

implementation

{ TDelayClock }

constructor TDelayClock.Create;
begin

end;

destructor TDelayClock.Destroy;
begin
  inherited Destroy;
end;

procedure TDelayClock.ClockStart;
begin
  Clock := 0;
  Start := gettickcount();
  Mode := cmCounting;
  if Assigned(LabelClock) then begin
    LabelClock.Color := clRed;
  end;
end;

procedure TDelayClock.Pause;
begin
  Mode := cmPaused;
end;

procedure TDelayClock.Continue;
begin
  start := GetTickCount() - Clock;
  Mode := cmCounting;
end;

procedure TDelayClock.Reset;
begin
  Mode := cmPaused;
  Clock := 0;
  if Assigned(LabelClock) then begin
    LabelClock.Color := clNone;
  end;
end;

procedure TDelayClock.ShowClock;
var ActClock: Longword;
    mins, secs, dsecs: integer;
begin
  ActClock := gettickcount();
  if Mode = cmCounting then begin
    Clock := ActClock - start;
  end;
  mins := trunc(Clock / (1000 * 60));
  secs := trunc(Clock / 1000 - mins * 60);
  dsecs := trunc(Clock / 100 - (mins * 600  + secs * 10));
  if not Assigned(LabelClock) then exit;
  if secs >= 30 then begin
    LabelClock.Color := clGreen;
    Mode := cmPaused;
  end;
  LabelClock.Caption := format('%.2d.%d', [secs, dsecs]);
end;

{$R *.lfm}

{ TFMain }

procedure TFMain.BGlobalStartClick(Sender: TObject);
begin
  GlobalClock := 0;
  PartialClock := 0;
  GlobalClockStart := gettickcount();
  PartialClockStart := GlobalClockStart;
  GlobalClockMode := cmCounting;
  PartialClockMode := cmCounting;
  LabelClock.Color := clNone;
  IPCSend('PS');
end;

procedure TFMain.BPartialLapClick(Sender: TObject);
begin
  if PartialClockMode = cmLap then begin
    PartialClockMode := cmCounting;
  end else if PartialClockMode = cmCounting then begin
    PartialClockMode := cmLap;
  end;
end;

procedure TFMain.BPartialResetClick(Sender: TObject);
begin
  PartialClockMode := cmPaused;
  PartialClock := 0;
end;

procedure TFMain.BPartialStartClick(Sender: TObject);
begin
  PartialClock := 0;
  PartialClockStart := gettickcount();
  PartialClockMode := cmCounting;
  IPCSend('PS');
end;

procedure TFMain.BPartialStopClick(Sender: TObject);
begin
  PartialClockMode := cmPaused;
end;

procedure TFMain.IPCSend(mess: string);
begin
  if IPCClient.ServerRunning then begin
    IPCClient.Connect;
    if IPCClient.Active then
      IPCClient.SendStringMessage(mess);
    IPCClient.Disconnect;
  end;
end;


procedure TFMain.BGlobalPauseClick(Sender: TObject);
begin
  GlobalClockMode := cmPaused;
  PartialClockMode := cmPaused;
end;

procedure TFMain.BGlobalResetClick(Sender: TObject);
begin
  GlobalClockMode := cmPaused;
  GlobalClock := 0;
end;

procedure TFMain.BGlobalContinueClick(Sender: TObject);
begin
  GlobalClockstart := GetTickCount() - GlobalClock;
  GlobalClockMode := cmCounting;
  PartialClockstart := GetTickCount() - PartialClock;
  PartialClockMode := cmCounting;
end;

function TFMain.getComponentByName(cname: string): TComponent;
var i: integer;
begin
  result := nil;
  for i:= 0 to ComponentCount - 1 do begin
    if Components[i].Name = cname then begin
      result := Components[i];
      exit;
    end;
  end;

end;

procedure TFMain.FormCreate(Sender: TObject);
var i: integer;
begin
  GlobalClockStartTime := 10 * 60 * 1000; // 10 minutes
  GlobalClockMode := cmPaused;
  GlobalClock := 0;

  PartialClockMode := cmPaused;
  PartialClock := 0;

  for i := 0 to 7 do begin
    MachineClocks[i] := TDelayClock.Create;
    //MachineClocks[i].LabelClock := TLabel(getComponentByName('LabelMA' + inttostr(i)));
  end;
  MachineClocks[0].LabelClock := LabelMA1;
  MachineClocks[1].LabelClock := LabelMA2;
  MachineClocks[2].LabelClock := LabelMA3;
  MachineClocks[3].LabelClock := LabelMA4;
  MachineClocks[4].LabelClock := LabelMA5;
  MachineClocks[5].LabelClock := LabelMA6;
  MachineClocks[6].LabelClock := LabelMA7;
  MachineClocks[7].LabelClock := LabelMA8;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  IPCClient.ServerID :='PCM_IN23';
  {if IPCClient.ServerRunning then begin
    IPCClient.Connect;
  end else begin
    MemoLog.Lines.Add('IPC server not running');
  end;}
end;

procedure TFMain.LabelMachineMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then begin
    MachineClocks[(Sender as TLabel).tag].ClockStart;
  end else if button = mbRight then begin
    MachineClocks[(Sender as TLabel).tag].Reset;
  end;
end;

procedure TFMain.LabelMachineClick(Sender: TObject);
begin
  //
end;

procedure TFMain.MenuGlobalStartClick(Sender: TObject);
begin
  BGlobalStart.Click;
end;

procedure TFMain.MenuPartialLapClick(Sender: TObject);
begin
  BPartialLap.Click;
end;

procedure TFMain.MenuPartialStartClick(Sender: TObject);
begin
  BPartialStart.Click;
end;

procedure TFMain.TimerTimer(Sender: TObject);
var ActClock, GlobalDelta, PartialDelta: Longword;
    mins, secs, dsecs: integer;
    GlobalClockShow: integer;
    i: integer;
begin
  ActClock := gettickcount();
  if GlobalClockMode = cmCounting then begin
    GlobalClock := ActClock - GlobalClockstart;
  end;
  GlobalClockShow :=  GlobalClockStartTime - GlobalClock;
  mins := trunc(abs(GlobalClockShow) / (1000 * 60));
  secs := trunc(abs(GlobalClockShow) / 1000 - mins * 60);
  dsecs := trunc(abs(GlobalClockShow) / 100 - (mins * 600  + secs * 10));
  LabelClock.Caption := format('%.2d:%.2d.%d', [mins, secs, dsecs]);
  if GlobalClockShow <= 0 then begin
    LabelClock.Color := clRed;
  end;

  if PartialClockMode = cmCounting then begin
    PartialClock := ActClock - PartialClockstart;
  end;
  mins := trunc(partialClock / (1000 * 60));
  secs := trunc(partialClock / 1000 - mins * 60);
  dsecs := trunc(partialClock / 100 - (mins * 600  + secs * 10));
  LabelClockPartial.Caption := format('%.2d:%.2d.%d', [mins, secs, dsecs]);

  for i := 0 to 7 do begin
    MachineClocks[i].ShowClock;
  end;

  //CheckBox1.Checked := IPCClient.Active and IPCClient.ServerRunning;
end;

end.

