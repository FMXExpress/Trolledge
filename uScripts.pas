unit uScripts;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.TMSMemo, FMX.Dialogs,
  uMemoFrame,
  PaxRunner, PaxInterpreter, PaxCompiler, PaxRegister;

procedure RunMemoUppercase(AMemo: TTMSFMXMemo);

implementation

function GetMemoText(AMemo: TTMSFMXMemo): string;
begin
  Result := AMemo.Lines.Text;
end;

procedure RunMemoUppercase(AMemo: TTMSFMXMemo);
var
  PaxCompiler: TPaxCompiler;
  PaxPascalLanguage: TPaxPascalLanguage;
  PaxProgram: TPaxInterpreter;
  I, H_TMemo: integer;
  MemoText: string;
begin
  PaxCompiler := TPaxCompiler.Create(nil);
  PaxPascalLanguage := TPaxPascalLanguage.Create(nil);
  PaxProgram := TPaxInterpreter.Create(nil);
  try
    PaxCompiler.Reset;
    PaxCompiler.RegisterLanguage(PaxPascalLanguage);
    PaxCompiler.AddModule('1', PaxPascalLanguage.LanguageName);
    H_TMemo := PaxCompiler.RegisterClassType(0, TTMSFMXMemo);
    PaxCompiler.RegisterVariable(0, 'Memo1', H_TMemo, @AMemo);
    PaxCompiler.RegisterVariable(0, 'MemoText', _typeSTRING, @MemoText);

    PaxCompiler.RegisterHeader(0, 'function UpperCase(const S: string): string;', @UpperCase);
    PaxCompiler.RegisterHeader(0, 'function GetMemoText(Memo: TTMSFMXMemo): string;', @GetMemoText);

    PaxCompiler.AddCode('1', 'begin');
    PaxCompiler.AddCode('1', '  MemoText := GetMemoText(Memo1);');
    PaxCompiler.AddCode('1', '  Memo1.Lines.Text := UpperCase(MemoText);');
    PaxCompiler.AddCode('1', 'end.');

    if PaxCompiler.Compile(PaxProgram) then
      PaxProgram.Run
    else
      for I := 0 to PaxCompiler.ErrorCount - 1 do
        ShowMessage(PaxCompiler.ErrorMessage[I]);

  finally
    PaxProgram.Free;
    PaxPascalLanguage.Free;
    PaxCompiler.Free;
  end;
end;


end.
