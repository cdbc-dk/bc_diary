program dayli_diary;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms, lfm_main;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Dayli Diary';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfrmDailyDiary, frmDailyDiary);
  Application.Run;
end.

