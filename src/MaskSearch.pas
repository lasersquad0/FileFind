unit MaskSearch;

interface

uses Classes, SysUtils, StrUtils, Windows{, ShellAPI};


function CompileMask(MaskStr: string): TStringList;
procedure FreeCompiledMask(GrepList: TStringList);

// main mask search function.
// tests whether the string 'SearchText' fits to the search masks in GrepList
function CmpMask(SearchText: string; GrepList: TStringList{; FindFile: Boolean; MatchCase: Boolean}): Boolean;

function CmpFile(FileName: string; GrepList: TStringList{; MatchCase: Boolean}): Boolean;

function WildcardMatch(const Text, Pattern: string): Boolean;

// fills the GrepList with the parts of 'a' (divided by ',' or ';')
//procedure SetFilters(MaskStr: string; GrepList: TStringList{; FindFile:boolean; MatchCase: Boolean});

implementation


procedure FreeCompiledMask(GrepList: TStringList);
var
  i: Integer;
begin
  if Assigned(GrepList) then begin
    for i := 0 to Pred(GrepList.Count) do GrepList.Objects[i].Free;
    GrepList.Free;
  end;
end;

// creates StringList and fills it with the MaskStr items (substrings of MaskStr divided by ',' or ';')
// This StringList mut be then freed with the call FreeCompiledMask
// compiles each part of MaskStr into TStringList object added to StringList item as StringList.Object[i]
// StringList.Object[i] further used in cmpmask1 for doing mask search
// findfile describes whether to use for find files or text in files
function CompileMask(MaskStr: string): TStringList; {; FindFile: Boolean; MatchCase: Boolean}
var
  ct: Integer;
  b: string;
  ps1,ps2    : Integer;
 // dontcare   : Boolean;
  onechar    : Char;
  //grepItem: string;
  tmpList, GrepList: TStringList;
begin
     GrepList := TStringList.Create;
     GrepList.Sorted := False;
     Result := GrepList;

     if MaskStr = '' then MaskStr := '*';

     // replace all ',' by ';'
     MaskStr := ReplaceStr(MaskStr, ',', ';');

     // split MaskStr by ';' in GrepList
     GrepList.Delimiter := ';';
     GrepList.StrictDelimiter := True;
     GrepList.DelimitedText := MaskStr;

     // delete empty MaskStr items if any
     ct := 0;
     while ct < GrepList.Count do
       if GrepList[ct] = '' then GrepList.Delete(ct) else Inc(ct);

     // compile each MaskStr item into number of 'substrings', put then into TStringList
     // and connect it to appropriate MaskStr item
     for ct := 0 to Pred(GrepList.Count) do begin
       b := GrepList[ct];
       tmpList := TStringList.Create;
       // divide partial strings ('?','*' or text) to tmp_list
       repeat
         onechar := b[1];
         if (onechar = '*') or (onechar = '?') then begin
           tmpList.Add(onechar);
           Delete(b, 1, 1);
         end else begin
           ps1 := Pos('?', b);
           if ps1 = 0 then ps1 := MaxInt;
           ps2 := Pos('*', b);
           if ps2 = 0 then ps2 := MaxInt;
           if ps2 > ps1 then ps2 := ps1;
           tmpList.Add(Copy(b, 1, ps2 - 1));
           b := Copy(b, ps2, MaxInt);
         end;
       until b = '';

       GrepList.Objects[ct] := tmpList; // adding compiled list to MaskStr appropriate item
     end;

     {ct := Pos(',', MaskStr);
     while ct > 0 do begin
       MaskStr[ct] := ';';
       ct := Pos(',', MaskStr);
     end;

     if MaskStr[length(MaskStr)] <> ';' then MaskStr := MaskStr + ';';

     // divide the string
     ct := Pos(';', MaskStr);
     while ct > 0 do begin
       grepItem := Trim(Copy(MaskStr, 1, ct - 1));
       if grepItem <> '' then GrepList.Add(grepItem); // do not add empty strings
       //GrepList.Add(Trim(Copy(MaskStr, 1, ct - 1) {CaseAware(Trim(Copy(a, 1, ct - 1)), MatchCase}{));
       MaskStr := Copy(MaskStr, ct + 1, MaxInt);
       ct := Pos(';', MaskStr);
     end;}

     // replace a 'xxx' term (without a '.') with '*xxx*' (for compatibility
     // with win95's file-search-dialog)
     // only if findfile
     {if FindFile then begin
       if GrepList.Count > 0 then for ct := 0 to Pred(GrepList.Count) do begin
         a := GrepList[ct];
         if (Pos('*', a) = 0) and (Pos('?', a) = 0) and (Pos('.', a) = 0) then
           GrepList[ct] := '*' + a + '*'
         else if Pos('.', a) = 0 then if a[length(a)] <> '*' then
           GrepList[ct] := a + '*';
       end;
     end;}

     GrepList.Sorted := True;
     GrepList.Duplicates := dupIgnore;
end;

// tests whether the string 'a' fits to the compiled search mask in cMask
function cmpmask1(a: string; cMask: TStringList{; FindFile: Boolean}): Boolean;
var
  sr             : string;
  ps1,ps2,ps3    : Integer;
  dontcare       : Boolean;
begin
  // now compare the string with the partial search masks
  dontcare := False;
  ps2 := 1;
  if cMask.Count > 0 then
    for ps1 := 0 to Pred(cMask.Count) do begin
      sr := cMask[ps1];
      if sr = '?' then begin
        if ps2 > Length(a) then Exit(False); // a is over and '?' came from masks => retrun False;
        Inc(ps2, 1);
      end else if sr = '*' then dontcare := True
      else begin
        if not dontcare then begin
          if Copy(a, ps2, Length(sr)) <> sr then Exit(False);
          ps2 := ps2 + Length(sr);
        end else begin
          ps3 := Pos(sr, a, ps2); // Copy(a, ps2, MaxInt));
          if ps3 = 0 then Exit(False);
          ps2 := ps3 + Length(sr);
          dontcare := False;
        end;
      end;
    end;

  if not dontcare then
    if ps2 <> Length(a) + 1 then Exit(False);
  // ps2 <= Length(a) means that part of a is not covered by masks => return False then
  Result := True;
end;

// tests whether the string SearchText fits to the search masks in GrepList
// If mask is empty (Greplist is empty), we consider that any search string fits such mask
// Empty search string may NOT fit the mask e.g. when mask = '?'
function CmpMask(SearchText: string; GrepList: TStringList{; FindFile: Boolean; MatchCase: Boolean}): Boolean;
var
  ct : Integer;
  mask: string;
begin
  Result := True;
  // if SearchStr = '' then exit; // if no search string, the always return TRUE
  // a := CaseAware(a, MatchCase);
  if (GrepList = nil) or (GrepList.Count < 1) then Exit; // any search string fits empty mask

  // Result := True;
  for ct := 0 to Pred(GrepList.Count) do begin
    mask := GrepList[ct];
    if mask = '' then Exit; // any search string fits empty mask
    if mask = '*' then Exit; // fits always
    if mask = '*.*' then
      if Pos('.', SearchText) > 0 then Exit; // fits, too
    if (Pos('*', mask) = 0) and (Pos('?', mask) = 0) then Exit(Pos(mask, SearchText) > 0); // search without wildcards

    if cmpmask1(SearchText, TStringList(GrepList.Objects[ct]) { , FindFile } ) then Exit;
    // compare with the whole GrepList until one fits

  end;

  Result := False;
end;

// tests whether a file's contents fit to the specified mask;
function CmpFile(FileName: string; GrepList: TStringList { ; MatchCase: Boolean } ): Boolean;
var
  fl: string;
  ts: TFileStream;
  // ct:Integer;
begin
  Result := True;
  // different handling between file find an text find
  // true if no or each text is wanted
  if (GrepList.Count < 1) or (GrepList[0] = '*') then Exit;

  Result := False;
  try
    ts := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    Exit;
  end;
  try
    SetLength(fl, ts.Size + 1);
    ts.Position := 0;
    ts.Read(fl[1], ts.Size);
    ts.Free;
    Result := CmpMask(fl { CaseAware(fl, MatchCase) } , GrepList { , false, MatchCase } );
  finally
    SetLength(fl, 0);
  end;
end;

  // Iterative wildcard matching (supports ? and *)
function WildcardMatch(const Text, Pattern: string): Boolean;
var
  iText, iPat, lenPat: Integer;
  starPat, starText: Integer;
  {chText,} chPat: Char;

begin
  iText := 1;
  iPat  := 1;
  starPat := 0;    // remembers position of last '*' in pattern (0 = none)
  starText := 0;   // remembers text position paired with last '*'
  lenPat := Length(Pattern);

  while (iText <= Length(Text)) do begin
    if iPat <= lenPat // Length(Pattern)
      then chPat := Pattern[iPat]
      else chPat := #0;

    // Multi-char wildcard '*'
    if (chPat = '*') then begin
      while (iPat <= lenPat {Length(Pattern)}) and (Pattern[iPat] = '*') do Inc(iPat);  // Skip consecutive '*' to normalize

      // If '*' is at the end, it matches the rest of Text
      if iPat > lenPat {Length(Pattern)} then Exit(True);

      // Record positions to allow backtracking if next literal/segment fails
      starPat := iPat;
      starText := iText;
      Continue;
    end;

    // Direct match or single-char wildcard '?'
    if (chPat <> #0) and ((chPat = '?') or (UpCase(chPat) = UpCase(Text[iText]))) then begin
      Inc(iText);
      Inc(iPat);
      Continue;
    end;

    // If mismatch but we had a previous '*', backtrack:
    if (starPat <> 0) then begin
      // Extend the match of the last '*' by one character in Text
      Inc(starText);
      if starText <= Length(Text) then begin
        iText := starText;
        iPat  := starPat; // re-check from the char after '*'
        Continue;
      end
      else Exit(False);
    end;

    // No '*' to rescue the mismatch
    Exit(False);
  end;

  // We consumed all Text; remaining Pattern must be empty or only '*'
  while (iPat <= lenPat {Length(Pattern)}) and (Pattern[iPat] = '*') do Inc(iPat);

  Result := (iPat > lenPat {Length(Pattern)});
end;


end.
