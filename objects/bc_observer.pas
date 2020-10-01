{------------------------------------------------------------------------------|
| Project name: Daily Diary                                                    |
| Unit name   : bc_observer.pas                                                |
| Copyright   : (c) 2020 cdbc.dk                                               |
| Programmer  : Benny Christensen /bc                                          |
| Created     : 2020.09.30 /bc initial design and coding,(observer pattern)    |
| Updated     : 2020.09.30 /bc Setting up environment, structure and vision    |
|                          /bc refactored observer to live in his own unit     |
|                                                                              |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
| Abstract:                                                                    |
|   This unit implements the observer pattern.                                 |
|   TObserved, a subject to observe                                            |
|   TObserver, one or more observers connected to the subject.                 |
|                                                                              |
|                                                                              |
-------------------------------------------------------------------------------}

unit bc_observer;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils;

const
  UnitVersion = '00.30.09.2020'; { initial version }

type
  { TObserved ~ observer pattern }
  TObserved = class(TInterfacedObject,IFPObserved)
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
    property Extra: ptrint read fExtra write fExtra; { well, you never know :-) }
  end; { TObserved }

  { TObserver ~ observer pattern }
  TObserver = class(TInterfacedObject,IFPObserver)
    Procedure FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
  end; { TddObserver }

implementation
uses RtlConsts;

{ *** TObserver *** }
procedure TObserver.FPOObservedChanged(aSender: TObject;Operation: TFPObservedOperation;Data: Pointer);
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

{ *** TObserved *** }
constructor TObserved.Create;
begin
  inherited Create;
  fObservers:= nil;
  // ???
end;

destructor TObserved.Destroy;
begin
  if assigned(fObservers) then begin
    FPONotifyObservers(Self,ooFree,Nil);
    FreeAndNil(fObservers);
  end;
  inherited Destroy;
end;

procedure TObserved.FPOAttachObserver(aObserver: TObject);
var I: IFPObserver;
begin
  if not aObserver.GetInterface(SGUIDObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[aObserver.ClassName]);
  if not assigned(fObservers) then fObservers:= TFPList.Create; { lazy creation }
  fObservers.Add(I);
end;

procedure TObserved.FPODetachObserver(aObserver: TObject);
var I: IFPObserver;
begin
  if not aObserver.GetInterface(SGUIDObserver,I) then
    raise EObserver.CreateFmt(SErrNotObserver,[aObserver.ClassName]);
  if assigned(fObservers) then begin
    fObservers.Remove(I);
    if (fObservers.Count = 0) then FreeAndNil(fObservers);
  end;
end;

procedure TObserved.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if assigned(fObservers) then case Action of
    lnAdded    : FPONotifyObservers(Self,ooAddItem,Ptr);
    lnExtracted: FPONotifyObservers(Self,ooDeleteItem,Ptr);
    lnDeleted  : FPONotifyObservers(Self,ooDeleteItem,Ptr);
  end;
end;

procedure TObserved.FPONotifyObservers(aSender: TObject;aOperation: TFPObservedOperation;Data: pointer);
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

end.

