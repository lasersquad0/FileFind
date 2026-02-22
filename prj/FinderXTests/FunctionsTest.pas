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
    procedure TestStringToarray(const AStr: string; const Delim: Char; const ACount: Cardinal);

    [TestCase('TestA', '///ab///ab///ab///,a,4')]
    procedure TestPerformance(const AStr: string; const Delim: Char; const ACount: Cardinal);

    [TestCase('TestA', '///ab///ab///ab///,a,4')]
    procedure TestPerformanceAccum(const AStr : string; const Delim: Char; const ACount: Cardinal);
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

procedure TFunctionsTest.TestStringToArray(const AStr : string; const Delim: Char; const ACount: Cardinal);
begin
  StringToArray(AStr, FArr, Delim);
  Assert.AreEqual(ACount, FArr.Count);

  FArr.Clear;
  StringToArrayAccum(AStr, FArr, Delim);
  Assert.AreEqual(ACount, FArr.Count);
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
