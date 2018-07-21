unit EventDispatcher;

interface

uses
  System.Classes, System.SysUtils, Data.db;
(*
  Note:
  HINS OFF in order for hiding
  H2269 Overriding virtual method XX has lower visibility (strict protected) than base class YY (public)
*)

type
{$HINTS OFF}
  TEventDispatcher<T> = class abstract(TComponent)
  strict protected
    FClosure: TProc<T>;
    constructor Create(aOwner: TComponent); override;
    procedure Notify(Sender: T);
  end;
{$HINTS ON}

  TNotifyEventDispatcher = class sealed(TEventDispatcher<TObject>)
  public
    class function Construct(Owner: TComponent; Closure: TProc<TObject>): TNotifyEvent; overload;
    function Attach(Closure: TProc<TObject>): TNotifyEvent;
  end;

  TDataSetNotifyEventDispatcher = class sealed(TEventDispatcher<TDataSet>)
  public
    class function Construct(Owner: TComponent; Closure: TProc<TDataSet>): TDataSetNotifyEvent; overload;
    function Attach(Closure: TProc<TDataSet>): TDataSetNotifyEvent;
  end;

implementation

class function TNotifyEventDispatcher.Construct(Owner: TComponent; Closure: TProc<TObject>): TNotifyEvent;
begin
  Result := TNotifyEventDispatcher.Create(Owner).Attach(Closure)
end;

function TNotifyEventDispatcher.Attach(Closure: TProc<TObject>): TNotifyEvent;
begin
  FClosure := Closure;
  Result := Notify;
end;

{ TDataSetNotifyEventDispatcher }

function TDataSetNotifyEventDispatcher.Attach(Closure: TProc<TDataSet>): TDataSetNotifyEvent;
begin
  FClosure := Closure;
  Result := Notify;
end;

class function TDataSetNotifyEventDispatcher.Construct(Owner: TComponent; Closure: TProc<TDataSet>): TDataSetNotifyEvent;
begin
  Result := TDataSetNotifyEventDispatcher.Create(Owner).Attach(Closure);
end;

{ TEventDispatcher }

constructor TEventDispatcher<T>.Create(aOwner: TComponent);
begin
  inherited;
end;

procedure TEventDispatcher<T>.Notify(Sender: T);
begin
  if Assigned(FClosure) then
    FClosure(Sender)
end;

end.
