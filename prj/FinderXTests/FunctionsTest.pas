unit FunctionsTest;

interface

uses
  DUnitX.TestFramework, DynamicArray;

type
  [TestFixture]
  TFunctionsTest = class
  private
    FArr: THArrayG<string>;
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    [TestCase('TestA1', ',/,0')]
    [TestCase('TestA2', ',,0')]
    [TestCase('TestB1', ' ,/,1')]
    [TestCase('TestB2', ' , ,0')]
    [TestCase('TestB3', '111,1,0')]
    [TestCase('TestB4', '111 ,1,1')]
    [TestCase('TestB5', '12121,1,2')]
    [TestCase('TestC1', 'a,/,1')]
    [TestCase('TestC2', 'aa, ,1')]
    [TestCase('TestC3', 'a a, ,2')]
    [TestCase('TestC4', '/,/,0')]
    [TestCase('TestC5', '//,/,0')]
    [TestCase('TestC7', 'a/,/,1')]
    [TestCase('TestC8', '/a/,/,1')]
    [TestCase('TestC9', '//b b//,/,1')]
    [TestCase('TestD1', '//b /b//,/,2')]
    [TestCase('TestD2', '//b//b//,/,2')]
    [TestCase('TestD3', 'b//b,/,2')]
    [TestCase('TestD4', 'b//b/,/,2')]
    [TestCase('TestD5', '///ab///ab///ab///,/,3')]
    [TestCase('TestD6', '///ab///ab///ab///,a,4')]
    procedure TestStringToArray(const AStr: string; const Delim: Char; const ACount: Cardinal);

    [TestCase('TestA', '///ab///ab///ab///,a,4')]
    procedure TestPerformance(const AStr: string; const Delim: Char; const ACount: Cardinal);

    [TestCase('TestA', '///ab///ab///ab///,a,4')]
    procedure TestPerformanceAccum(const AStr: string; const Delim: Char; const ACount: Cardinal);

    [TestCase('TestA', '')]
    [TestCase('TestB', 'a')]
    [TestCase('TestC', 'ab')]
    [TestCase('TestD', '12;34')]
    [TestCase('TestE', '12;34;')]
    [TestCase('TestF', 'ab;cd;ab;;cd')]
    [TestCase('TestG', 'a;c;d;e')]
    [TestCase('TestK', 'ab1;cd1;ab1;1;cd123456789')]
    procedure TestDelphiArrayToZStrArray(const AStr: string);

    [TestCase('TestA', '')]
    [TestCase('TestB', 'a')]
    [TestCase('TestC', 'ab')]
    [TestCase('TestD', '12;34')]
    [TestCase('TestE', '12;34;')]
    [TestCase('TestF', 'ab;cd;ab;;cd')]
    [TestCase('TestG', 'a;c;d;e')]
    [TestCase('TestK', 'ab1;cd1;ab1;1;cd123456789')]
    procedure TestDelphiArrayToZStrArrayStr(const AStr: string);

    [TestCase('TestA', '')]
    [TestCase('TestB', 'a')]
    [TestCase('TestC', 'ab')]
    [TestCase('TestD', '12;34')]
    [TestCase('TestE', '12;34;')]
    [TestCase('TestF', 'ab;cd;ab;;cd')]
    [TestCase('TestG', 'a;c;d;e')]
    [TestCase('TestK', 'ab1;cd1;ab1;1;cd123456789')]
    procedure TestSplitByStrings(const ASearchStr: string; const AStr: string);

  end;

implementation

uses SysUtils, Windows, Functions;


procedure TFunctionsTest.Setup;
begin
  FArr := THArrayG<string>.Create(100);
end;

procedure TFunctionsTest.TearDown;
begin
  FreeAndNil(FArr);
end;

procedure TFunctionsTest.TestSplitByStrings(const ASearchStr: string; const AStr: string);
var
  res: THArrayG<SplitRec>;
  delim: THArrayG<string>;
begin
  res := THArrayG<SplitRec>.Create;
  delim := THArrayG<string>.Create;

  try
    HGetTokens(ASearchStr, '*?', False, delim);

    SplitByStrings(AStr, delim, res);


  finally
    res.Free;
    delim.Free;
  end;
end;

procedure TFunctionsTest.TestStringToArray(const AStr : string; const Delim: Char; const ACount: Cardinal);
begin
  StringToArray(AStr, FArr, Delim);
  Assert.AreEqual(ACount, FArr.Count);

  FArr.Clear;
  StringToArrayAccum(AStr, FArr, Delim);
  Assert.AreEqual(ACount, FArr.Count);
end;

procedure TFunctionsTest.TestDelphiArrayToZStrArray(const AStr: string);
begin
  var Arr := StringToArray(AStr, ';');
  var Res := DelphiArrayToZStrArray(Arr);

  var Curr: PChar := Res;
  var i: Integer := 0;
  while (True) do begin
    if Curr[0] = #0 then break;
    Assert.IsTrue(i < Length(Arr));
    Assert.AreEqual(string(Curr), Arr[i]);
    Inc(Curr, Strlen(Curr));
    Assert.AreEqual(#0, Curr[0]);
    Inc(Curr);
    Inc(i);
  end;

  Assert.AreEqual(Integer(Length(Arr)), i);

  FreeMem(Res);
end;

procedure TFunctionsTest.TestDelphiArrayToZStrArrayStr(const AStr: string);
begin
  var Arr := StringToArray(AStr, ';');
  var Res := DelphiArrayToZStrArrayStr(Arr);

  var Curr: PChar := @Res[1];
  var i: Integer := 0;
  while (True) do begin
    if Curr[0] = #0 then break;
    Assert.IsTrue(i < Length(Arr));
    Assert.AreEqual(string(Curr), Arr[i]);
    Inc(Curr, Strlen(Curr));
    Assert.AreEqual(#0, Curr[0]);
    Inc(Curr);
    Inc(i);
  end;

  Assert.AreEqual(Integer(Length(Arr)), i);

end;


procedure TFunctionsTest.TestPerformance(const AStr : string; const Delim: Char; const ACount: Cardinal);
const
  ITERATIONS = 3_000_000;
var
  i, start: Integer;
begin
  start := GetTickCount;
  for i := 1 to ITERATIONS do begin
    StringToArray(AStr, FArr, Delim);
    FArr.Clear;
  end;

  TDUnitX.CurrentRunner.Log(Format('TestPerformance(%d) = %s', [ITERATIONS, MillisecToStr(GetTickCount - start)]));

  Assert.IsTrue(True);
end;

procedure TFunctionsTest.TestPerformanceAccum(const AStr : string; const Delim: Char; const ACount: Cardinal);
const
  ITERATIONS = 3_000_000;
var
  i, start: Integer;
begin
  start := GetTickCount;
  for i := 1 to ITERATIONS do begin
    StringToArrayAccum(AStr, FArr, Delim);
    FArr.Clear;
  end;

  TDUnitX.CurrentRunner.Log(Format('TestPerformanceAccum(%d) = %s', [ITERATIONS, MillisecToStr(GetTickCount - start)]));

  Assert.IsTrue(True);
end;

initialization
  TDUnitX.RegisterTestFixture(TFunctionsTest);

end.
