

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

const
  UnitVersion = '00.28.09.2020'; // initial version

type
  { TDailyDiary observed ~ observer pattern }

  { TddObserved }

  TddObserved = class(TInterfacedObject,IFPObserved)
  private
    fObservers: TFPList;
  protected
    fExtra: ptrint;
  public
    constructor Create;
    destructor Destroy; override;
    Procedure FPOAttachObserver(AObserver : TObject); { attach a new observer }
    Procedure FPODetachObserver(AObserver : TObject); { detach an observer }
    procedure Notify(Ptr: Pointer;Action: TListNotification); virtual; { base notify, calls all attached observers via: }
    Procedure FPONotifyObservers(ASender : TObject; AOperation : TFPObservedOperation; Data : Pointer); { Notify all observers of a change }
  end; { TddObserved }

  { TDailyDiary observer ~ observer pattern }
  TddObserver = class(TInterfacedObject,IFPObserver)
    Procedure FPOObservedChanged(ASender: TObject;Operation: TFPObservedOperation;Data: Pointer);
  end; { TddObserver }

  TfrmDailyDiary = class(TForm)
  private

  public

  end;

var
  frmDailyDiary: TfrmDailyDiary;

implementation
{$R *.lfm}
uses RtlConsts;

{ *** TddObserved *** }
constructor TddObserved.Create;
begin
  inherited Create;
  fObservers:= nil;
  // ???
end;

destructor TddObserved.Destroy;
begin
  if assigned(fObservers) then begin
    FPONotifyObservers(Self,ooFree,Nil);
    FreeAndNil(fObservers);
  end;
  inherited Destroy;
end;

procedure TddObserved.FPOAttachObserver(aObserver: TObject);
var I: IFPObserver;
begin
  if not aObserver.GetInterface(SGUIDObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[aObserver.ClassName]);
  if not assigned(fObservers) then fObservers:= TFPList.Create; { lazy creation }
  fObservers.Add(I);
end;

procedure TddObserved.FPODetachObserver(aObserver: TObject);
var I: IFPObserver;
begin
  if not aObserver.GetInterface(SGUIDObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[aObserver.ClassName]);
  if assigned(fObservers) then begin
    fObservers.Remove(I);
    if (fObservers.Count = 0) then FreeAndNil(fObservers);
  end;
end;

procedure TddObserved.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if assigned(fObservers) then case Action of
    lnAdded    : FPONotifyObservers(Self,ooAddItem,Ptr);
    lnExtracted: FPONotifyObservers(Self,ooDeleteItem,Ptr);
    lnDeleted  : FPONotifyObservers(Self,ooDeleteItem,Ptr);
  end;
end;

procedure TddObserved.FPONotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data: pointer);
var
  I: integer;
  Obs: IFPObserver;
begin
  if assigned(fObservers) then begin
    for I:= fObservers.Count-1 downto 0 do begin
      Obs:= IFPObserver(fObservers[I]);
      Obs.FPOObservedChanged(aSender,aOperation,Data);
    end;
  end;
end;

{ TddObserver }
procedure TddObserver.FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
begin
  case Operation of
    ooAddItem   : ;// call insert routine
    ooChange    : ;// call update routine
    ooCustom    : ;// ?!? time will tell
    ooDeleteItem: ;// call delete routine
    ooFree      : ;// call free routine
  end;
  // TODO
end;

end.

