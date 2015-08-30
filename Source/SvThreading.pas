(*
* Copyright (c) 2011, Linas Naginionis
* Contacts: lnaginionis@gmail.com or support@soundvibe.net
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of the <organization> nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)
//inspired from open source DSharp library
unit SvThreading;

interface

uses
  SysUtils, Classes, SyncObjs, Generics.Collections;

type
  ESvFuturesException = class(Exception);

  ISvFuture = interface
    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel;
    procedure WaitFor;
  end;

  ISvFuture<T> = interface(ISvFuture)
    function Value: T;
  end;


  TSvAbstractFutureThread = class(TThread)
  protected
    procedure DoTerminate; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TSvAbstractFuture = class(TInterfacedObject, ISvFuture)
  strict protected
    FCanceled: Boolean;
    FWorker: TSvAbstractFutureThread;
  public
    constructor Create;
    destructor Destroy; override;

    function Canceled: Boolean;
    function Finished: Boolean;

    procedure Cancel;
    procedure WaitFor;
  end;

  TSvFutureThread<T> = class(TSvAbstractFutureThread)
  strict private
    FAction: TFunc<T>;
    FResult: T;
  public
    constructor Create(const AAction: TFunc<T>);
    procedure Execute; override;
    property Result: T read FResult;
  end;

  TFutureType<T> = class(TSvAbstractFuture, ISvFuture<T>)
  public
    constructor Create(const AAction: TFunc<T>);

    function Value: T;
  end;
{$REGION 'Doc'}
    /// <summary>
    /// Future value
    /// </summary>
    /// <remarks>
    /// Contains any type value which will be calculated in the separate thread
    ///  Waits for the thread to finish when getting value if it's not already finished
    /// </remarks>
    /// <code>
    /// var vFuture: TSvFuture<Integer>;
    ///     intValue: Integer;
    ///  begin
    ///  vFuture.Assign(function: Integer begin Result := 10 * 1024; Sleep(1000); end);
    ///  //now main thread doesn't stop, because calculation executes in separate thread
    ///  //do something...
    ///  //when you need value just get it
    ///  intValue := vFuture; //intValue = 10240. If vFuture value isn't calculated yet, it waits for this to finish
    /// </code>
{$ENDREGION}
  TSvFuture<T> = record
  private
    FFuture: ISvFuture<T>;
  private
    function GetValue: T;
    function GetCanceled: Boolean;
    function GetFinished: Boolean;
  public
    constructor Create(const AFunc: TFunc<T>);
    procedure Assign(const AFunc: TFunc<T>);

    procedure Cancel;
    procedure WaitFor;

    class operator Implicit(const AFunc: TFunc<T>): TSvFuture<T>; inline;
    class operator Implicit(const AFuture: TSvFuture<T>): T; inline;

    property Canceled: Boolean read GetCanceled;
    property Finished: Boolean read GetFinished;
    property Value: T read GetValue;
  end;

  TParallelProc1 = reference to procedure(const i: NativeInt; var Abort: Boolean);
  TParallelProc2 = reference to procedure(const i, AThreadIndex: NativeInt; var Abort: Boolean);

  TParallelMode = (pmBlocking, pmNonBlocking);

  TSvThreadPool = class;

  TSvParallelThread = class(TThread)
  private
    FThreadIndex: NativeInt;
    FOwner: TSvThreadPool;
    FFreePool: Boolean;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(AOwner: TSvThreadPool); reintroduce;
    destructor Destroy; override;

    property FreePool: Boolean read FFreePool write FFreePool;
  end;

  TSvAsyncThread = class(TThread)
  private
    FProc: TProc;
    FFinishProc: TThreadProcedure;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(const AProc: TProc; AFinishProc: TThreadProcedure = nil);
  end;

  ISvThreadPool = interface
  ['{AA2B2700-659B-4DEE-BBBC-62B28C7D2431}']
    //getters and setters
    function GetAbort: Boolean;
    procedure SetAbort(const Value: Boolean);
    function GetMaxPos: NativeInt;
    procedure SetMaxPos(const Value: NativeInt);
    function GetCurrPos: NativeInt;
    procedure SetCurrPos(const Value: NativeInt);
    function GetMaxThreads: Integer;
    procedure SetMaxThreads(const Value: Integer);
    function GetProc1: TParallelProc1;
    procedure SetProc1(const Value: TParallelProc1);
    function GetProc2: TParallelProc2;
    procedure SetProc2(const Value: TParallelProc2);
    function GetMode: TParallelMode;
    procedure SetMode(const Value: TParallelMode);
    //methods
    function Add(const AThreadIndex: NativeInt): Integer;
    procedure Start();
    procedure Terminate();
    procedure WaitFor(AExceptIndex: NativeInt);
    //properties
    property Abort: Boolean read GetAbort write SetAbort;
    property CurrPos: NativeInt read GetCurrPos write SetCurrPos;
    property MaxPos: NativeInt read GetMaxPos write SetMaxPos;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    property Mode: TParallelMode read GetMode write SetMode;
    property Proc1: TParallelProc1 read GetProc1 write SetProc1;
    property Proc2: TParallelProc2 read GetProc2 write SetProc2;
  end;

  TSvThreadPool = class(TInterfacedObject, ISvThreadPool)
  private
    FCSection: TCriticalSection;
    FMaxThreads: Integer;
    FCurrPos: NativeInt; //current loop index
    FMaxPos: NativeInt;
    FAbort: Boolean;
    FThreads: TObjectList<TSvParallelThread>;
    FProc1: TParallelProc1;
    FProc2: TParallelProc2;
    FFinishProc: TThreadProcedure;
    FMode: TParallelMode;
    function GetThread(const AIndex: Integer): TSvParallelThread;
    function GetAbort: Boolean;
    procedure SetAbort(const Value: Boolean);
    function GetMaxPos: NativeInt;
    procedure SetMaxPos(const Value: NativeInt);
    function GetCurrPos: NativeInt;
    procedure SetCurrPos(const Value: NativeInt);
    function GetMaxThreads: Integer;
    procedure SetMaxThreads(const Value: Integer);
    function GetProc1: TParallelProc1;
    procedure SetProc1(const Value: TParallelProc1);
    function GetProc2: TParallelProc2;
    procedure SetProc2(const Value: TParallelProc2);
    function GetMode: TParallelMode;
    procedure SetMode(const Value: TParallelMode);
  protected
    function GetNextValue: NativeInt;
    procedure SetDontFreeFlag(AValue: Boolean);
  public
    constructor Create(); virtual;
    destructor Destroy; override;

    function Add(const AThreadIndex: NativeInt): Integer;
    procedure Start();
    procedure Terminate();
    procedure WaitFor(AExceptIndex: NativeInt = -1);
    procedure Release(AExceptIndex: NativeInt = -1);


    property Thread[const AIndex: Integer]: TSvParallelThread read GetThread; default;
    property Threads: TObjectList<TSvParallelThread> read FThreads;

    property Abort: Boolean read GetAbort write SetAbort;
    property CurrPos: NativeInt read GetCurrPos write SetCurrPos;
    property MaxPos: NativeInt read GetMaxPos write SetMaxPos;
    property MaxThreads: Integer read GetMaxThreads write SetMaxThreads;
    property Mode: TParallelMode read GetMode write SetMode;

    property Proc1: TParallelProc1 read GetProc1 write SetProc1;
    property Proc2: TParallelProc2 read GetProc2 write SetProc2;
  end;
  /// <summary>
  /// Parallel loop implementation
  /// </summary>
  TSvParallel = class sealed
  private
    class var
      FMaxThreads: Integer;
      FSyncEvents: Boolean;
  private
   class constructor Create;
   class destructor Destroy;

   class procedure DoForEach(const AFrom, ATo: NativeInt; AFunc1: TParallelProc1;
     AFunc2: TParallelProc2; AOnAllFinishProc: TThreadProcedure; AMode: TParallelMode);
  public
    /// <summary>
    /// Parallel loop implementation
    /// </summary>
    /// <param name="AFrom">Value from which to start loop</param>
    /// <param name="ATo">Last value to loop from</param>
    /// <param name="AFunc">Procedure which will be called after each iteration</param>
    /// <param name="AOnAllFinishProc">Procedure to call when loop completes</param>
    class procedure ForEach(const AFrom, ATo: NativeInt; AFunc: TParallelProc1;
      AOnAllFinishProc: TThreadProcedure = nil); overload;
    class procedure ForEach(const AFrom, ATo: NativeInt; AFunc: TParallelProc2;
      AOnAllFinishProc: TThreadProcedure = nil); overload;
    class procedure ForEachNonBlocking(const AFrom, ATo: NativeInt; AFunc: TParallelProc1;
      AOnAllFinishProc: TThreadProcedure = nil); overload;
    class procedure ForEachNonBlocking(const AFrom, ATo: NativeInt; AFunc: TParallelProc2;
      AOnAllFinishProc: TThreadProcedure = nil); overload;
    /// <summary>
    /// Calls anonymous method in separate thread. Additional method can be specified
    /// which will be called after main proc execution
    /// </summary>
    /// <param name="AProc">procedure to execute in separate thread</param>
    /// <param name="AProcAfterFinish">procedure to execute after main proc has finished execution</param>
    class function Async(const AProc: TProc; AProcAfterFinish: TThreadProcedure = nil): TSvAsyncThread;

    /// <summary>
    /// Defines number of threads to use while looping
    /// </summary>
    /// <remarks>
    /// Number of threads cannot exceed number of iterations, e.g. if you loops from 1 to 5 and
    ///  set MaxThreads to 10, TSvParallel will use maximum of 5 threads
    /// </remarks>
    class property MaxThreads: Integer read FMaxThreads write FMaxThreads;
    /// <summary>
    /// True - OnFinishAll event will be synchronized with main thread
    /// </summary>
    class property SyncFinishEvents: Boolean read FSyncEvents write FSyncEvents;
  end;

implementation

{ TSvFuture<T> }

function CreateAsyncThread(const AProc: TProc; AProcAfterFinish: TThreadProcedure = nil): TSvAsyncThread;
begin
  Result := TSvAsyncThread.Create(AProc, AProcAfterFinish);
end;

procedure TSvFuture<T>.Cancel;
begin
  FFuture.Cancel;
end;

constructor TSvFuture<T>.Create(const AFunc: TFunc<T>);
begin
  FFuture := TFutureType<T>.Create(AFunc);
end;

function TSvFuture<T>.GetCanceled: Boolean;
begin
  Result := FFuture.Canceled;
end;

function TSvFuture<T>.GetFinished: Boolean;
begin
  Result := FFuture.Finished;
end;

function TSvFuture<T>.GetValue: T;
begin
  Result := FFuture.Value;
end;

class operator TSvFuture<T>.Implicit(const AFuture: TSvFuture<T>): T;
begin
  Result := AFuture.Value;
end;

procedure TSvFuture<T>.Assign(const AFunc: TFunc<T>);
begin
  Create(AFunc);
end;

class operator TSvFuture<T>.Implicit(const AFunc: TFunc<T>): TSvFuture<T>;
begin
  Result := TSvFuture<T>.Create(AFunc);
end;

procedure TSvFuture<T>.WaitFor;
begin
  FFuture.WaitFor;
end;

{ TSvAbstractFuture }

constructor TSvAbstractFuture.Create;
begin
  inherited Create();
end;

destructor TSvAbstractFuture.Destroy;
begin
  FWorker.Free;
  inherited Destroy;
end;

procedure TSvAbstractFuture.Cancel;
begin
  if FCanceled then
    raise ESvFuturesException.Create('Action already canceled');

  if not FWorker.Finished then
  begin
    FWorker.Terminate();
    FCanceled := True;
  end;
end;

function TSvAbstractFuture.Canceled: Boolean;
begin
  Result := FCanceled;
end;

function TSvAbstractFuture.Finished: Boolean;
begin
  Result := FWorker.Finished;
end;

procedure TSvAbstractFuture.WaitFor;
begin
  FWorker.WaitFor();
end;

{ TSvAbstractFutureThread }

constructor TSvAbstractFutureThread.Create;
begin
  inherited Create(True);
end;

destructor TSvAbstractFutureThread.Destroy;
begin
  if not Finished and not Terminated then
  begin
    Terminate;
    WaitFor;
  end;
  inherited Destroy;
end;

procedure TSvAbstractFutureThread.DoTerminate;
begin
  inherited;
end;

{ TSvFutureThread<T> }

constructor TSvFutureThread<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create();
  FAction := AAction;
end;

procedure TSvFutureThread<T>.Execute;
begin
  inherited;
  FResult := FAction();
end;

{ TFutureType<T> }

constructor TFutureType<T>.Create(const AAction: TFunc<T>);
begin
  inherited Create;
  FWorker := TSvFutureThread<T>.Create(AAction);
  FWorker.Start();
end;

function TFutureType<T>.Value: T;
begin
  if FCanceled then
    raise ESvFuturesException.Create('Action was canceled');

  if not FWorker.Finished then
  begin
    FWorker.WaitFor();
  end;
  Result := TSvFutureThread<T>(FWorker).Result;
end;



{ TSvParallel }

class function TSvParallel.Async(const AProc: TProc; AProcAfterFinish: TThreadProcedure): TSvAsyncThread;
begin
  Result := CreateAsyncThread(AProc, AProcAfterFinish);
  Result.Start;
end;

class constructor TSvParallel.Create;
begin
  FMaxThreads := CPUCount;
  FSyncEvents := False;
end;

class destructor TSvParallel.Destroy;
begin
//
end;

class procedure TSvParallel.DoForEach(const AFrom, ATo: NativeInt; AFunc1: TParallelProc1;
  AFunc2: TParallelProc2; AOnAllFinishProc: TThreadProcedure; AMode: TParallelMode);
var
  pool: TSvThreadPool;
  I, iLoopCount: NativeInt;
  iThreads: Integer;
begin
  if AFrom > ATo then
    Exit;

  iLoopCount := (ATo - AFrom) + 1;

  pool := TSvThreadPool.Create;
  pool.CurrPos := AFrom;
  pool.MaxPos := ATo;
  pool.MaxThreads := FMaxThreads;

  if iLoopCount > FMaxThreads then
    iThreads := FMaxThreads
  else
    iThreads := iLoopCount;

  pool.Proc1 := AFunc1;
  pool.Proc2 := AFunc2;
  pool.Mode := AMode;
  pool.FFinishProc := AOnAllFinishProc;
 //create pool threads
  for i := 0 to iThreads - 1 do
  begin
    pool.Add(I);
  end;

  pool.Start;

  case AMode of
    pmBlocking:
    begin
      pool.WaitFor();
      pool.Free;
    end;
    pmNonBlocking: ;//do nothing;
  end;
end;

class procedure TSvParallel.ForEach(const AFrom, ATo: NativeInt; AFunc: TParallelProc2;
  AOnAllFinishProc: TThreadProcedure);
begin
  DoForEach(AFrom, ATo, nil, AFunc, AOnAllFinishProc, pmBlocking);
end;

class procedure TSvParallel.ForEachNonBlocking(const AFrom, ATo: NativeInt; AFunc: TParallelProc1;
  AOnAllFinishProc: TThreadProcedure);
begin
  DoForEach(AFrom, ATo, AFunc, nil, AOnAllFinishProc, pmNonBlocking);
end;

class procedure TSvParallel.ForEachNonBlocking(const AFrom, ATo: NativeInt; AFunc: TParallelProc2;
  AOnAllFinishProc: TThreadProcedure);
begin
  DoForEach(AFrom, ATo, nil, AFunc, AOnAllFinishProc, pmNonBlocking);
end;

class procedure TSvParallel.ForEach(const AFrom, ATo: NativeInt; AFunc: TParallelProc1;
  AOnAllFinishProc: TThreadProcedure);
begin
  DoForEach(AFrom, ATo, AFunc, nil, AOnAllFinishProc, pmBlocking);
end;

{ TSvParallelThread }

constructor TSvParallelThread.Create(AOwner: TSvThreadPool);
begin
  inherited Create(True);
  FThreadIndex := 0;
  FOwner := AOwner;
  FFreePool := True;
end;

destructor TSvParallelThread.Destroy;
begin
  inherited Destroy;
end;

procedure TSvParallelThread.DoTerminate;
var
  bLastThread: Boolean;
begin
  inherited;
  case FOwner.Mode of
    pmNonBlocking:
    begin
      FOwner.FCSection.Acquire;
      try
        bLastThread := FFreePool and (FOwner.FCurrPos >= FOwner.FMaxPos);
        if bLastThread then
          FOwner.SetDontFreeFlag(False);
      finally
        FOwner.FCSection.Release;
      end;

      if bLastThread then
      begin
        FOwner.Terminate();
        FOwner.Release(FThreadIndex);
        if Assigned(FOwner.FFinishProc) then
        begin
          if TSvParallel.SyncFinishEvents then
            Synchronize(FOwner.FFinishProc)
          else
            FOwner.FFinishProc();
        end;
        FOwner.Free;
        //set queue to release current thread
        Queue(procedure begin Self.Free; end);
      end;
    end;
  end;
end;

procedure TSvParallelThread.Execute;
var
  nCurrent: NativeInt;
begin
  FOwner.FAbort := False;
  nCurrent := FOwner.GetNextValue;
  while (nCurrent <= FOwner.FMaxPos) and not (Terminated) do
  begin
    if Assigned(FOwner.FProc1) then
      FOwner.FProc1(nCurrent, FOwner.FAbort)
    else
      FOwner.FProc2(nCurrent, FThreadIndex, FOwner.FAbort);

    nCurrent := FOwner.GetNextValue;
  end;
end;

{ TSvAsyncThread }

constructor TSvAsyncThread.Create(const AProc: TProc; AFinishProc: TThreadProcedure);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FProc := AProc;
  FFinishProc := AFinishProc;
end;

procedure TSvAsyncThread.DoTerminate;
begin
  inherited;
  if Assigned(FFinishProc) then
  begin
    if TSvParallel.SyncFinishEvents then
      Synchronize(FFinishProc)
    else
      FFinishProc();
  end;
end;

procedure TSvAsyncThread.Execute;
begin
  FProc();
end;

{ TSvThreadPool }

function TSvThreadPool.Add(const AThreadIndex: NativeInt): Integer;
var
  thread: TSvParallelThread;
begin
  thread := TSvParallelThread.Create(Self);
  thread.FThreadIndex := AThreadIndex;
  Result := FThreads.Add(thread);
end;

constructor TSvThreadPool.Create;
begin
  inherited Create();
  FThreads := TObjectList<TSvParallelThread>.Create(False);
  FCSection := TCriticalSection.Create;
  FProc1 := nil;
  FProc2 := nil;
  FFinishProc := nil;
end;

destructor TSvThreadPool.Destroy;
begin
  case FMode of
    pmBlocking:
    begin
      FThreads.OwnsObjects := True;
    end;
    pmNonBlocking:
    begin
      FThreads.OwnsObjects := False;
    end;
  end;
  FThreads.Free;
  FCSection.Free;
  inherited Destroy;
end;

function TSvThreadPool.GetAbort: Boolean;
begin
  Result := FAbort;
end;

function TSvThreadPool.GetCurrPos: NativeInt;
begin
  FCSection.Acquire;
  try
    Result := FCurrPos;
  finally
    FCSection.Release;
  end;
end;

function TSvThreadPool.GetMaxPos: NativeInt;
begin
  Result := FMaxPos;
end;

function TSvThreadPool.GetMaxThreads: Integer;
begin
  Result := FMaxThreads;
end;

function TSvThreadPool.GetMode: TParallelMode;
begin
  Result := FMode;
end;

function TSvThreadPool.GetNextValue: NativeInt;
begin
  FCSection.Acquire;
  try
    Result := FCurrPos;
    if FAbort then
      FCurrPos := FMaxPos + 1
    else
      Inc(FCurrPos);
  finally
    FCSection.Release;
  end;
end;

function TSvThreadPool.GetProc1: TParallelProc1;
begin
  Result := FProc1;
end;

function TSvThreadPool.GetProc2: TParallelProc2;
begin
  Result := FProc2;
end;

function TSvThreadPool.GetThread(const AIndex: Integer): TSvParallelThread;
begin
  Result := FThreads[AIndex];
end;

procedure TSvThreadPool.Release(AExceptIndex: NativeInt);
var
  i: Integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    if i <> AExceptIndex then
      FThreads[i].Free;
  end;
end;

procedure TSvThreadPool.SetAbort(const Value: Boolean);
begin
  if Value <> FAbort then
  begin
    FAbort := Value;
  end;
end;

procedure TSvThreadPool.SetCurrPos(const Value: NativeInt);
begin
  if Value <> FCurrPos then
  begin
    FCurrPos := Value;
  end;
end;

procedure TSvThreadPool.SetDontFreeFlag(AValue: Boolean);
var
  i: Integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    FThreads[i].FFreePool := AValue;
  end;
end;

procedure TSvThreadPool.SetMaxPos(const Value: NativeInt);
begin
  if Value <> FMaxPos then
  begin
    FMaxPos := Value;
  end;
end;

procedure TSvThreadPool.SetMaxThreads(const Value: Integer);
begin
  if Value <> FMaxThreads then
  begin
    FMaxThreads := Value;
  end;
end;

procedure TSvThreadPool.SetMode(const Value: TParallelMode);
begin
  if Value <> FMode then
  begin
    FMode := Value;
  end;
end;

procedure TSvThreadPool.SetProc1(const Value: TParallelProc1);
begin
  FProc1 := Value;
end;

procedure TSvThreadPool.SetProc2(const Value: TParallelProc2);
begin
  FProc2 := Value;
end;

procedure TSvThreadPool.Start;
var
  i: Integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    FThreads[i].Start;
  end;
end;

procedure TSvThreadPool.Terminate;
var
  i: Integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    FThreads[i].Terminate;
  end;
end;

procedure TSvThreadPool.WaitFor(AExceptIndex: NativeInt = -1);
var
  i: Integer;
begin
  for i := 0 to FThreads.Count - 1 do
  begin
    if i <> AExceptIndex then
      FThreads[i].WaitFor;
  end;
end;

end.
