unit uThread;

interface
uses
    System.Classes, System.SysUtils,
    System.Generics.Collections;
type
    TLifeThread<T> = class(TThread)
    private
        FTasks : TList<T>;
        FFailedTasks : TList<T>;
        FSuccessTasks : TList<T>;
        FOnPause :  TNotifyEvent;
        FOnExecute : TFunc<T, Boolean>;
        FOnExecuteStep : TProc<T,Boolean>;
    protected
        procedure Execute; override;
        procedure DoOnPause;
        procedure DoOnExecute(const AValue : T);
        procedure DoStart;
        procedure DoStop;
        procedure DoOnExecuteStep(const AValue : T; ASuccess : Boolean);
    public
        constructor Create; overload;
        constructor Create(AThreadFunc: TFunc<T, Boolean>); overload;
        destructor Destroy; override;
        procedure AddTask(const ATasks : TArray<T>);
        procedure ExecuteFailedTask;

        property OnPause : TNotifyEvent read FOnPause write FOnPause;
        property OnExecute : TFunc<T, Boolean> read FOnExecute write FOnExecute;
        property OnExecuteStep : TProc<T,Boolean> read FOnExecuteStep write FOnExecuteStep;
    end;
implementation

{ TLifeThread<T> }

procedure TLifeThread<T>.AddTask(const ATasks: TArray<T>);
var
    i : integer;
    LValue : T;
begin
    if Terminated then
        Exit;
    for LValue in ATasks do
        if not FTasks.Contains(LValue) and not FSuccessTasks.Contains(LValue) and
            not FFailedTasks.Contains(LValue) then
            FTasks.Add(LValue);
    DoStart;
end;

constructor TLifeThread<T>.Create(AThreadFunc: TFunc<T, Boolean>);
begin
    Self.Create;
    OnExecute := AThreadFunc;
end;

constructor TLifeThread<T>.Create;
begin
    inherited Create(True);
    //FreeOnTerminate := True;
    FOnPause := nil;
    FOnExecute := nil;
    OnExecuteStep := nil;
    FTasks := TList<T>.Create;
    FFailedTasks := TList<T>.Create;
    FSuccessTasks := TList<T>.Create;
end;

destructor TLifeThread<T>.Destroy;
begin
    FreeAndNil(FTasks);
    FreeAndNil(FFailedTasks);
    FreeAndNil(FSuccessTasks);
    //FTasks.Free;
    //FFailedTasks.Free;
    //FSuccessTasks.Free;
  inherited;
end;

procedure TLifeThread<T>.DoOnExecute(const AValue : T);
var
    IsExecuted : Boolean;
begin
    if Terminated then
        exit;
    IsExecuted := Assigned(FOnExecute)and FOnExecute(AValue);
    if IsExecuted then
        FSuccessTasks.Add(AValue)
    else
        FFailedTasks.Add(AValue);
    FTasks.Remove(AValue);
    DoOnExecuteStep(AValue, IsExecuted);
end;

procedure TLifeThread<T>.DoOnExecuteStep(const AValue: T; ASuccess: Boolean);
begin
    if Assigned(FOnExecuteStep) then
        FOnExecuteStep(AValue, ASuccess);
end;

procedure TLifeThread<T>.DoOnPause;
begin
    if Assigned(FOnPause) then
        FOnPause(Self);
end;

procedure TLifeThread<T>.DoStart;
begin
    if Terminated then
        Exit;
    if self.Suspended and (FTasks.Count > 0) then
    begin
        Self.Suspended := False;
    end;
end;

procedure TLifeThread<T>.DoStop;
begin
    if Terminated then
        exit;
    if self.Started and (FTasks.Count < 1) then
    begin
        //Self.Suspended := True;
        self.Sleep(1000);
        DoOnPause;
    end;
end;

procedure TLifeThread<T>.Execute;
var
    LValue : T;
    LTasks : TArray<T>;
begin
  inherited;
    while not Terminated do
    begin
        LTasks := FTasks.ToArray;
        for LValue in LTasks do
            DoOnExecute(LValue);
        DoStop;
    end;
end;

procedure TLifeThread<T>.ExecuteFailedTask;
var
    LTasks : TArray<T>;
begin
    LTasks := FFailedTasks.ToArray;
    FFailedTasks.Clear;
    Self.AddTask(LTasks);
end;

end.
