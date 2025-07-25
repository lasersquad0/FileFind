unit MaskSearchTest;

interface

uses
  Classes, DUnitX.TestFramework, DynamicArray;

type
  [TestFixture]
  TMaskSearchTest = class
  private
     FMList: TStringList;
     FArrStr: THArrayG<string>;

  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure TestEmpty;

    //    <search str>, <mask>, result (1 - true, 0 - false)
    [TestCase('TestA1',', *, 1')] // empty search str
    [TestCase('TestA2',' , *, 1')]
    [TestCase('TestA3','1, *, 1')]
    [TestCase('TestA4','aa;, *, 1')]
    [TestCase('TestA5','aa;bb, *, 1')]
    [TestCase('TestA6','---, *, 1')]
    [TestCase('TestA7','*, *, 1')]
    [TestCase('TestA8','**, *, 1')]
    [TestCase('TestA9','?, *, 1')]
    [TestCase('TestA10','??, *, 1')]
    [TestCase('TestB1',', ?, 0')] // empty search str
    [TestCase('TestB2','a, ?, 1')]
    [TestCase('TestB3',';;, ?, 0')]
    [TestCase('TestB4','--, ?, 0')]  // if mask str is empty then we use '*' as filter in GrepList, that is why count=1 here
    [TestCase('TestB5','*, ?, 1')]
    [TestCase('TestB6','?, ?, 1')]
    [TestCase('TestB7','?*, ?, 0')]
    [TestCase('TestB8','*?, ?, 0')]
    [TestCase('TestB9','??, ?, 0')]
    [TestCase('TestB10','**, ?, 0')]
    [TestCase('TestB11','abcdef, ?, 0')]
    procedure TestOnlyWildCards(const ASearchStr: string; const AMask: string; const AResult: Integer);

    [Test]
    procedure TestCompileMaskEmpty;

    //       <mask>, <count of filters>, count of compiled items (total)
    [TestCase('TestA1','aa, 1, 1')]
    [TestCase('TestA2',';aa, 1, 1')]
    [TestCase('TestA3',';;aa, 1, 1')]
    [TestCase('TestB','aa;, 1, 1')]
    [TestCase('TestC','aa;bb, 2, 2')]
    [TestCase('TestD1','aa;bb;, 2, 2')]
    [TestCase('TestD2','aa;bb;;, 2, 2')]
    [TestCase('TestE1',';, 0, 0')]
    [TestCase('TestE2',';;, 0, 0')]
    [TestCase('TestF',', 1, 1')]  // if mask str is empty then we use '*' as filter in GrepList, that is why count=1 here
    [TestCase('TestG1','*, 1, 1')]
    [TestCase('TestG2','**, 1, 2')]
    [TestCase('TestG3','***, 1, 3')]
    [TestCase('TestG4','?, 1, 1')]
    [TestCase('TestG5','??, 1, 2')]
    [TestCase('TestG6','*?, 1, 2')]
    [TestCase('TestG7','?*, 1, 2')]
    [TestCase('TestK2','*;*, 2, 2')]
    [TestCase('TestK3','*;**, 2, 3')]
    [TestCase('TestK4','?, 1, 1')]
    [TestCase('TestK5','?;?, 2, 2')]
    [TestCase('TestK6','*;?;, 2, 2')]
    [TestCase('TestK7',';?;*;, 2, 2')]
    procedure TestCompileMask(const ASearchStr: string; const ACount1: Integer; const ACount2: Integer);

                   // <search str>,   <mask>    <result>
    [TestCase('TestA1','Browse.VC.DB2, Browse.VC.DB?, 1')] // should be true because ? matches 2 at the end
    [TestCase('TestA2','Browse.VC.db-, Browse.VC.db?, 1')]
    [TestCase('TestA3','Browse.VC.db2a, Browse.VC.db?, 0')]
    [TestCase('TestB1','arith, ?rith, 1')]
    [TestCase('TestB2','r, ?rith, 0')]
    [TestCase('TestB3','R, ?rith, 0')]
    [TestCase('TestB4','arith, ?rith, 0')]
    procedure TestWildCards(const ASearchStr: string; const AMask: string; const AResult: Integer);
  end;

implementation

uses
  SysUtils, MaskSearch;

procedure TMaskSearchTest.Setup;
begin
  FMList := TStringList.Create;
  FArrStr := THArrayG<string>.Create;
end;

procedure TMaskSearchTest.TearDown;
begin
  FreeAndNil(FMList);
  FreeAndNil(FArrStr);
end;

procedure TMaskSearchTest.TestEmpty;
begin
  Assert.IsTrue(CmpMask('', nil));
  Assert.IsTrue(CmpMask('', FMList)); // empty FMList
  Assert.IsTrue(CmpMask('g', nil));
  Assert.IsTrue(CmpMask('G', FMList));
end;

procedure TMaskSearchTest.TestOnlyWildCards(const ASearchStr: string; const AMask: string; const AResult: Integer);
var cmask: TStringList;
begin
  cmask := CompileMask(Trim(AMask));
  Assert.AreEqual(AResult = 1, CmpMask(Trim(ASearchStr), cmask)); // 1 - true, 0 - false
  FreeCompiledMask(cmask);
end;

procedure TMaskSearchTest.TestCompileMask(const ASearchStr: string; const ACount1: Integer; const ACount2: Integer);
var
  cmask: TStringList;
  i, total: Integer;
begin
  cmask := CompileMask(Trim(ASearchStr));
  Assert.AreEqual(ACount1, cmask.Count);
  total := 0;
  for i := 0 to cmask.Count - 1 do total := total + TStringList(cmask.Objects[i]).Count;
  Assert.AreEqual(ACount2, total);
  FreeCompiledMask(cmask);
end;

procedure TMaskSearchTest.TestCompileMaskEmpty;
var cmask: TStringList;
begin
  cmask := CompileMask('some str');
  Assert.AreEqual(1, cmask.Count);
  Assert.AreEqual(1, TStringList(cmask.Objects[0]).Count);
  FreeCompiledMask(cmask);

  cmask := CompileMask(''); // if mask is empty then we use '*' as mask, that is why count=1 here
  Assert.AreEqual(1, cmask.Count);
  Assert.AreEqual(1, TStringList(cmask.Objects[0]).Count);
  FreeCompiledMask(cmask);

  cmask := CompileMask('*');
  Assert.AreEqual(1, cmask.Count);
  Assert.AreEqual(1, TStringList(cmask.Objects[0]).Count);
  FreeCompiledMask(cmask);

  cmask := CompileMask('?');
  Assert.AreEqual(1, cmask.Count);
  Assert.AreEqual(1, TStringList(cmask.Objects[0]).Count);
  FreeCompiledMask(cmask);

  cmask := CompileMask('?*');
  Assert.AreEqual(1, cmask.Count);
  Assert.AreEqual(2, TStringList(cmask.Objects[0]).Count);
  FreeCompiledMask(cmask);
end;

procedure TMaskSearchTest.TestWildCards(const ASearchStr, AMask: string; const AResult: Integer);
var cmask: TStringList;
begin
  cmask := CompileMask(Trim(AMask));
  Assert.AreEqual(AResult = 1, CmpMask(Trim(ASearchStr), cmask)); // 1 - true, 0 - false
  FreeCompiledMask(cmask);
end;


initialization
  TDUnitX.RegisterTestFixture(TMaskSearchTest);

end.
