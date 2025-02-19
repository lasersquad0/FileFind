unit MaskSearch;

interface
uses Classes, SysUtils, Windows, ShellAPI;

//function GetTimeModified(a:tfiletime):string;

// File Search utility
procedure SetFilters(a: string; GrepList: TStringList{; FindFile:boolean; MatchCase: Boolean});
function cmpmask(a: string; GrepList: TStringList{; FindFile: Boolean; MatchCase: Boolean}): Boolean;
function cmpfile(FileName: string; GrepList: TStringList{; MatchCase: Boolean}): Boolean;

implementation

//
// STRING ROUTINES
//

{
function GetTimeModified(a:tfiletime):string;
// This function retrieves the last time, the given file was written to disk
var
  mtm :TSystemTime;
  at  :TFileTime;
  ds,ts:string;
  //ds,ts:ShortString;
const
  MAX_DATETIME_STR = 255;
begin
  // Time must get converted, else there is an error of one hour
  // Does anybody know what this function does ?
  // Maybe something like summertime/wintertime (or what you call it out of Germany) ?
  FileTimeToLocalFileTime(a,at);
  FileTimeToSystemTime(at,mtm);
  SetLength(ds, GetDateFormat(LOCALE_USER_DEFAULT, 0, @mtm, NIL, @ds[1], 255) - 1);
  SetLength(ts, GetTimeFormat(LOCALE_USER_DEFAULT, TIME_NOSECONDS, @mtm, NIL, @ts[1], 255)  - 1);
  Result:=ds+'  '+ts;
end; // End getmod
 }

 {
function CaseAware(S: string; Match: Boolean): string;
begin
  if Match
  then Result := S
  else Result := AnsiLowerCase(S);
end;
  }
//
// Original File Search Routine by Marcus Stephany
//
procedure SetFilters(a: string; GrepList: TStringList{; FindFile: Boolean; MatchCase: Boolean});
// fills the grep_list with the parts of 'a' (divided by ',' or ';')
// findfile describes whether to use for find files or text in files
// + aml modified : Match Case
var
  ct: Integer;
begin
     GrepList.Clear;
     GrepList.Sorted := False;
     if a = '' then begin
        GrepList.add('*');
        exit;
     end;

     // replace all ',' by ';'
     ct := Pos(',', a);
     while ct > 0 do begin
       a[ct] := ';';
       ct := Pos(',', a);
     end;

     if a[length(a)] <> ';' then a := a + ';';

     // divide the string
     ct := Pos(';', a);
     while ct > 0 do begin
       GrepList.Add(Trim(Copy(a, 1, ct - 1) {CaseAware(Trim(Copy(a, 1, ct - 1)), MatchCase}));
       a := Copy(a, ct + 1, MaxInt);
       ct := Pos(';', a);
     end;

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

function cmpmask1(a, b: string{; FindFile: Boolean}): Boolean;
// tests whether the string 'a' fits to the search mask in 'b'
var sr             : string;
    ps1,ps2,ps3    : Integer;
    dontcare       : Boolean;
    onechar        : Char;
    tmp_list       : TStrings;
begin
     Result := True;
     if b = '*' then Exit; // fits always
     if b = '*.*' then if Pos('.', a) > 0 then Exit; // fits, too
     if (Pos('*', b) = 0) and (Pos('?', b) = 0) then
       // if not FindFile then begin
           if Pos(b, a) > 0 then Exit;
           // searched text was found (searchstring IN text)
       // end else
       //    if a = b then exit;
           // searched file was found (searchstring IS text)


     Result := False;
     if b = '' then Exit;

     try
        tmp_list := TStringList.create;
        // divide partial strings ('?','*' or text) to tmp_list
        repeat
              onechar := b[1];
              if (onechar = '*') or (onechar = '?') then begin
                 tmp_list.Add(onechar);
                 Delete(b, 1, 1);
              end else begin
                  ps1 := Pos('?', b);
                  if ps1 = 0 then ps1 := MaxInt;
                  ps2 := Pos('*', b);
                  if ps2 = 0 then ps2 := MaxInt;
                  if ps2 > ps1 then ps2 := ps1;
                  tmp_list.Add(Copy(b, 1, ps2 - 1));
                  b := Copy(b, ps2, MaxInt);
              end;
        until b = '';
        // now compare the string with the partial search masks
        dontcare := False;
        ps2      := 1;
        if tmp_list.Count > 0 then for ps1 := 0 to Pred(tmp_list.Count) do begin
           sr := tmp_list[ps1];
           if sr = '?' then begin
              Inc(ps2, 1);
              if ps2 > Length(a) then Exit;
           end else
           if sr = '*' then
              dontcare := True
           else begin
                if not dontcare then begin
                   if Copy(a, ps2, Length(sr)) <> sr then Exit;
                   dontcare := False;
                   ps2 := ps2 + Length(sr);
                end else begin
                   ps3:= Pos(sr, Copy(a, ps2, MaxInt));
                   if ps3 = 0 then Exit;
                   ps2 := ps3 + length(sr);
                   dontcare := False;
                end;
           end;
        end;
        if not dontcare then if ps2 <> Length(a) + 1 then Exit;
        Result := True;
     finally
        tmp_list.Free;
     end;
end;

function cmpmask(a: string; GrepList: TStringList{; FindFile: Boolean; MatchCase: Boolean}): Boolean;
// tests whether the string 'a' fits to the search masks in grep_list
var ct : Integer;
begin
     Result := True;
     if a = '' then exit; // if no search string, the always return TRUE
     //a := CaseAware(a, MatchCase);
     Result := False;
     if (GrepList = nil) or (GrepList.Count < 1) then Exit;
     Result := True;
     for ct := 0 to Pred(GrepList.Count) do
         if cmpmask1(a, GrepList[ct]{, FindFile}) then Exit; // compare with the whole grep_list until one fits
     Result := False;
end;

function cmpfile(FileName: string; GrepList: TStringList{; MatchCase: Boolean}): Boolean;
// tests whether a file's contents fit to the specified mask;
var
   fl: string;
   ts: TFileStream;
   //ct:Integer;
begin
     Result := True;
     // different handling between file find an text find
     // true if no or each text is wanted
     if (GrepList.Count < 1) or (GrepList[0] = '*') then exit;

     Result := False;
     try
       ts := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
     except
       exit;
     end;
     try
       SetLength(fl, ts.Size + 1);
       ts.Position := 0;
       ts.Read(fl[1], ts.Size);
       ts.Free;
       Result := cmpmask(fl {CaseAware(fl, MatchCase)}, GrepList{, false, MatchCase});
     finally
       SetLength(fl, 0);
     end;
end;


end.
