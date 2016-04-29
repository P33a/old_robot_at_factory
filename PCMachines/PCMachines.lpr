program PCMachines;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LEDmachines, sdposeriallaz;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFLedMachines, FLedMachines);
  Application.Run;
end.

