
FinderX application for fast and easy search 
=============================================

CONTACTS
------------

Any questions or suggestions are welcome to this email: 
   Andrey Romanchenko
   lasersquad@gmail.com

Sources are available here
https://github.com/lasersquad0/FileFind   

SUMMARY
-------

FinderX - application for fast and easy search for files and folders. 
FinderX loads all files and folders (names and their attributes) into memory and does memory search from there. That is why search is extremely fast. 
Most of searches take less then one second.


GETTING STARTED
---------------

FinderX application does not require installation.
Just unpack .zip file into any suitable folder on you PC and run FinderX.exe

On first run:
- You will see yellow line on top of the main window telling "It's time to refresh search index..."
- To refresh index, press "Refresh Index" button and wait several seacond while app reads files and folders
- Progrees bar will be shown in the bottom of main window together with button Cancel.
- Once finished will can see statistics on the status bar.
- You are all set and ready for fast search!


DESCRIPTION
-----------
Application is built with Delphi 12.1 and tested on Windows 10 and 11. 
It also should work with older Windows versions back to Windows 7.

With FinderX you can make search through all your drives (including removable).
Search is done by file and folder names and by its main attributes such as: size, modified/created/accessed time, hidden, system, compressed, encrypted, and so on.
Search by file content is not supported yet.
Once you created filesystem index you can quickly do your searches unlimited number of times.
When you think your file system has significantly changed (contains many news files, etc.) - re-build filesystem index by pressing "Refresh Index" button.
You don't need to re-build filesystem index every search, do it once a week or so.


FEATURES
--------

- Fast search, less than a second
- Fast indexing anf automatic discovering and indexing new volumes e.g. removable one.
- When you are typing text serch is started in 0.5 sec after you stopped typing, you do not need to press Search button 
- You can do wildcards search. Two wildcards are supported: * and ?.
- Search term is automatically highlighted in search results for your convenience
- All your searches are stored in search history and automatically shown once you type new search
- In addition to by name you can do search by file/folder size, modified/created/access date, and file attributes. Press Advanced search button to see additional search options. 
- Results can be sorted by Name, Size, File Type, Modified Date, Created, Last Access, Path.
- You can hide columns that you don't need.
- Width and order of columns is automatically saved and restored next FinderX run. 
- You can make search case sensitive.
- You can make sorting case sensitive.
- You can collect all folders on top during sort (like it works in Windows File Explorer)
- You can view files and folders statistics, go to File/Statistics main manu item
- You can exclude some folders from being indexed e.g. various Temp folders.
- App icon in system tray
- Status bar shows important information such as: number of found items, time spent on last search, total number of items in index, data load time, top level indexed folder size
- Double click on item in search results will open item's folder in Windows File Explorer

