unit lfm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  LCLType,
  bc_guardian,
  bc_mtlinklist,
  bc_observer;

type
{------------------------------------------------------------}

  { TbcObservedStack }

  TbcObservedStack = class(TbcStack)
  private
    fSubject: TObserved;
  protected
    Procedure NotifyObservers(aSender: TObject;anOperation: TFPObservedOperation;Data: Pointer); { Notify all observers of a change }
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(anObject: TObject);
    function Pop: TObject;
    Procedure AttachObserver(anObserver : TObject);    { attach a new observer }
    Procedure DetachObserver(anObserver : TObject);       { detach an observer }
    procedure Notify(Ptr: Pointer;Action: TListNotification); virtual; { base notify, calls all attached observers via: }

  end;

{------------------------------------------------------------}
  TForm1 = class(TForm)
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TbcObservedStack }
procedure TbcObservedStack.NotifyObservers(aSender: TObject;anOperation: TFPObservedOperation;Data: Pointer);
begin
  fSubject.FPONotifyObservers(aSender,anOperation,Data);
end;

constructor TbcObservedStack.Create;
begin
  inherited Create(bc_guardian.Guardian);
  fSubject:= TObserved.Create;
end;

destructor TbcObservedStack.Destroy;
begin
  FreeAndNil(fSubject);
  inherited Destroy;
end;

procedure TbcObservedStack.Push(anObject: TObject);
begin
  _Push(pointer(anObject));
  fSubject.Notify(pointer(anObject),lnAdded);
end;

function TbcObservedStack.Pop: TObject;
begin

end;

procedure TbcObservedStack.AttachObserver(anObserver: TObject);
begin
  fSubject.FPOAttachObserver(anObserver);
end;

procedure TbcObservedStack.DetachObserver(anObserver: TObject);
begin
  fSubject.FPODetachObserver(anObserver);
end;

procedure TbcObservedStack.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if assigned(fSubject) then fSubject.Notify(Ptr,Action);
end;

end.

