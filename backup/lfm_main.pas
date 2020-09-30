

{------------------------------------------------------------------------------|
| Project name: Daily Diary                                                    |
| Unit name   : lfm_main.pas                                                   |
| Copyright   : (c) 2020 cdbc.dk                                               |
| Programmer  : Benny Christensen /bc                                          |
| Created     : 2020.09.28 /bc initial design and coding,(kind of MVC pattern) |
| Updated     : 2020.09.28 /bc Setting up environment, structure and vision    |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
| Abstract:                                                                    |
|   Make an application to help with remembering what happened that day        |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
-------------------------------------------------------------------------------}

unit lfm_main;
{$mode objfpc}{$H+}
{$define debug}
interface
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  bc_observer,
  blowfish;

const
  UnitVersion = '00.28.09.2020'; // initial version

type
  TfrmDailyDiary = class(TForm)
  private

  public

  end;

var
  frmDailyDiary: TfrmDailyDiary;

implementation
{$R *.lfm}

end.

