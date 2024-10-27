
FileFind application for fast and easy file system search 
=========================================================

CONTACTS
------------

Any questions or suggestions are welcome to this email: 
   Andrey Romanchenko
   lasersquad@gmail.com
   

GETTING STARTED
---------------------

FileFind application does not require installation.
Just unpack it into any suitable folder and run FileFind.exe

What to do on first use:
- start FileFind.exe application
- go to Options/Settings in main menu
- in "Folder to Index" edit box select top level folder you want to search to. It can be either your folder or entire disk - C: or D: for instance
- press "Build Index" button and wait several seconds while indexing is done
- you are all set and ready for fast search!


DESCRIPTION
---------------
Application is built with Delphi 12.1 and tested on Windows 10 and 11. 
It also should work with older Windows versions till Windows Vista.

With FileFind application you can make search through your entire file system or search inside a specified directory.
Once you created filesystem index you can do your searches unlimited number of times.
When you think your file system has significantly changed (contains many news files, etc.) - re-build filesystem index as described in Getting Started section.
You can use "Build Index" button on the main window now.
You don't need to re-build filesystem index every search, do it once a week or so.


FEATURES
-----------

- fast search, less than a second
- when you are typing search is started in 0.5 sec after you stopped typing, you do no need to press Search button
- you can do wildcards search. two wildcards are supported: * and ?.
- in addition to by name you can do search by file/folder size, modified date, and file attributes. Press Advanced search button to see additional search options. 
- you can make search case sensitive, configured in Options/Settings window
- sort sort results by Name, Size, File Type, Modified date, Last Access date, Path.
- you can make sort case sensitive, configured in Options/Settings window
- you can collect all folders on top during sort (like it works in Windows File Explorer)
- you can view Indexing log for the case when some folders are not indexes due to permission errors, can be found in Options/Settings window
- you can view files and folders statistics, go to File/Statistics main menu
- status bar shows important information such as: number of found items, time spent on last search, total number of items in index, data load time, top level indexed folder size
- double click on item in search results will open item's folder in Windows File Explorer


