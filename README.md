# FinderX - search at the speed of thought

## CONTACTS

Any questions or suggestions are welcome. <br>
Author: Andrey Romanchenko (lasersquad@gmail.com) <br>
Web site: https://github.com/lasersquad0/FileFind <br>  
Questions/comments: https://github.com/lasersquad0/FileFind/discussions


## SUMMARY

**FinderX** - application for searching files/folders faster than you can type. 

**FinderX** loads all files and folders (names and their attributes) into memory and does memory search from there. That is why search is extremely fast. 
Most of searches take less then one second.


## GETTING STARTED

**FinderX** application does not require installation.
Just unpack **FinderX-x.y.z.zip** file into any suitable folder on you PC and run **FinderX.exe**.

On first run:
- You will see yellow line on top of the main window telling "**It's time to refresh search index...**"
- To refresh search index, press "**Refresh Index**" button and wait several seacond while app reads files and folders
- Progrees bar will be shown in the bottom of main window together with button Cancel.
- Once finished will can see statistics on the status bar.
- You are all set and ready for fast search!
- Push button "**Refresh Index**" from time to time to update search index. If you don't do it you will see "**It's time to refresh search index...**" message on the top.

## DESCRIPTION

Application is built with **Delphi 12.1** and tested on **Windows 10 and 11**. 
It also should work with older Windows versions back to Windows 7.

With **FinderX** you can make search through all your drives (including removable like **USB Flash drive**).
Search is done by file and folder names and by its main attributes such as: file size, modified/created/accessed time, hidden, system, compressed, encrypted, and so on.

Search by file content is not supported yet.

Once you created filesystem search index by pressing "**Refresh Index**" button you can quickly do your searches unlimited number of times.

When you think your file system has significantly changed (contains many news files, etc.) - re-build filesystem index by pressing "**Refresh Index**" button.
You don't need to re-build filesystem index every search, do it once a week or so.


## FEATURES

- **Fast search**. Search takes less than a second.
- **Fast indexing**. Automatic discovering and indexing new volumes e.g. removable one.
- **Don't need to press Search button**. When you are typing text, search starts in 0.5 sec after you stopped typing, you do not need to press Search button.
- **Wildcards supported**. You can do wildcards search. Two wildcards are supported: **\*** and **?**.
- **Match whole word**. You can do whole word search. If search string enclosed in quotes (**"myfile"** or **'myfile'**) then it will search for a file with exact name "**myfile**" (without quotes).
  It means that files with names like **myfile.exe, myfilename, supermyfile.log** will NOT appear in search results. 
  This feature helps to find files by name that a part of other commonly used name like "**win**" or "**system**".
- **Search results highlighting**. Search term is automatically highlighted in search results for your convenience.
- **Search history**. All your searches are stored in search history and automatically shown once you type new search.
- **Extended search**. In addition to by name you can do search by file/folder size, modified/created/access date, and file attributes. Press Advanced search button to see additional search options.
- **Sorting**. Results can be sorted by Name, Size, File Type, Modified Date, Created, Last Access, Path.
- **Configurable columns**. You can hide columns that you don't need (right mouse click on column header). Width and order of columns is automatically saved and restored next **FinderX** run. 
- **Run as Admin**. You can run **FinderX** in Administrator mode to search more files (configured in settings).
- **Flexible configuration**. You can make search case sensitive. You can make sorting case sensitive. You can collect all folders on top during sort (like it works in Windows File Explorer).
- **Statistics**. You can view files and folders statistics, go to File/Statistics main menu item.
- **Exceptions**. You can exclude some folders from being indexed e.g. various Temp folders.
- **SystemTray**. Avility to minimize application to system tray.
- **Status bar**. Shows important information such as: number of found items, time spent on last search, total number of items in index, data load time, top level indexed folder size.
- **Open folder in Windows Explorer**. Double click on item in search results will open item's folder in Windows File Explorer.





