program test_observer;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces,
  Forms, lfm_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

