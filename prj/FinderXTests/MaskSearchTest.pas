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

    //          <search str>, <mask>
    [TestCase('TestA1',',*, 1')] // empty search str
    [TestCase('TestA2',' ,*, 1')]
    [TestCase('TestA3','1,*, 1')]
    [TestCase('TestA4','aa;,*, 1')]
    [TestCase('TestA5','aa;bb,*, 1')]
    [TestCase('TestA6','---,*, 1')]
    [TestCase('TestA7','*,*, 1')]
    [TestCase('TestA8','**,*, 1')]
    [TestCase('TestA9','?,*, 1')]
    [TestCase('TestA10','??,*, 1')]
    [TestCase('TestB1',',?, 0')] // empty search str
    [TestCase('TestB2','a,?, 1')]
    [TestCase('TestB3',';;,?, 0')]
    [TestCase('TestB4','--,?, 0')]  // if mask str is empty then we use '*' as filter in GrepList, that is why count=1 here
    [TestCase('TestB5','*,?, 1')]
    [TestCase('TestB6','?,?, 1')]
    [TestCase('TestB7','?*,?, 0')]
    [TestCase('TestB8','*?,?, 0')]
    [TestCase('TestB9','??,?, 0')]
    [TestCase('TestB10','**,?, 0')]
    [TestCase('TestB11','abcdef,?, 0')]
    procedure TestOnlyWildCards(const AValue1: string; const AValue2: string; const AValue3: Integer);

    [Test]
    procedure TestSetFiltersEmpty;

    //         <mask>, <count of filters>
    [TestCase('TestA1','aa,1')]
    [TestCase('TestA2',';aa,1')]
    [TestCase('TestA3',';;aa,1')]
    [TestCase('TestB','aa;,1')]
    [TestCase('TestC','aa;bb,2')]
    [TestCase('TestD1','aa;bb;,2')]
    [TestCase('TestD2','aa;bb;;,2')]
    [TestCase('TestE1',';,0')]
    [TestCase('TestE2',';;,0')]
    [TestCase('TestF',',1')]  // if mask str is empty then we use '*' as filter in GrepList, that is why count=1 here
    [TestCase('TestG1','*,1')]
    [TestCase('TestG2','**,1')]
    [TestCase('TestG3','***,1')]
    [TestCase('TestG4','?,1')]
    [TestCase('TestG5','??,1')]
    [TestCase('TestG6','*?,1')]
    [TestCase('TestG7','?*,1')]
    [TestCase('TestK2','*;*,2')]
    [TestCase('TestK3','*;**,2')]
    [TestCase('TestK4','?,1')]
    [TestCase('TestK5','?;?,2')]
    [TestCase('TestK6','*;?;,2')]
    [TestCase('TestK7',';?;*;,2')]
    procedure TestSetFilters(const AValue1: string; const AValue2: Integer);

    [TestCase('TestA1','Browse.VC.DB2,Browse.VC.DB?,1')] // should be true because ? matches 2 at the end
    [TestCase('TestA2','Browse.VC.db-,Browse.VC.db?,1')]
    [TestCase('TestA3','Browse.VC.db2a,Browse.VC.db?,0')]
    procedure TestWildCards(const SearchStr: string; const Mask: string; const Res: Integer);
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
  Assert.IsTrue(CmpMask('G', FMList)); // empty FMList
end;

procedure TMaskSearchTest.TestOnlyWildCards(const AValue1: string; const AValue2: string; const AValue3: Integer);
begin
  SetFilters(AValue2, FMList);
  Assert.AreEqual(AValue3 = 1, CmpMask(AValue1, FMList)); // 1 - true, 0 - false
end;

procedure TMaskSearchTest.TestSetFilters(const AValue1: string; const AValue2: Integer);
begin
  //HGetTokens(AValue1, ';', False, FArrStr);

  SetFilters(AValue1, FMList);
  Assert.AreEqual(AValue2, FMList.Count);
  //Assert.AreEqual(Cardinal(AValue2), FArrStr.Count);
end;

procedure TMaskSearchTest.TestSetFiltersEmpty;
begin
  SetFilters('some str', nil);
  SetFilters('', nil);
  SetFilters('', FMList);
  SetFilters('*', FMList);
  SetFilters('?', FMList);
  SetFilters('?*', FMList);

  Assert.IsTrue(True); // no exception raised during statememnts above
end;

procedure TMaskSearchTest.TestWildCards(const SearchStr, Mask: string; const Res: Integer);
begin
  SetFilters(Mask, FMList);
  Assert.AreEqual(Res = 1, CmpMask(SearchStr, FMList)); // 1 - true, 0 - false
end;

initialization
  TDUnitX.RegisterTestFixture(TMaskSearchTest);

end.
