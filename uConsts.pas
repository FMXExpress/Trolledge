unit uConsts;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants;

const
  CAppName          = 'Trolledge';
  CCaption          = '%s - ' + CAppName +' v%s';
  CDefFileName      = 'Untitled';
  CDefNone          = 'Plain Text';
  CImageExt         = '.jpeg;.jpg;.jpe;.tif;.tiff;.png;.bmp;.ico;.gif';
  CBinaryExt        = '.exe;.bin;.dll;*.dcu;*.o';
  CZipExt           = '*.zip;*.apk;*.ipa';
  CPascalFormExt    = '.dfm;.fmx;.lfm';
  CCaretPos         = 'Ln %d, Col %d';
  CSaveDialogText   = 'Text has been changed. Save changes?';
  CWorkFilesChanged = 'There are some unsaved WorkFiles. Save changes?';
  CWorkItemCloseText  = 'WorkFile has been changed. Save changes?';
  COpenLargeBinFile = 'File size is over 20 MB. (%sMB)' + sLineBreak +
                      'Opening the file takes a few seconds and depends file size.' + sLineBreak +
                      'Continue opening?';
  {$IFDEF MSWINDOWS}
  CAllFileMask      = '*.*';
  {$ELSE}
  CAllFileMask      = '*';
  {$ENDIF}
  CFilePlugins      = 'filePlugins.plugins';
  CSearchFoldersDelim = ';';
  CCR: string = #$D;
  CLF: string = #$A;

  MaxHistoryFiles   = 10;
  DefTabSize        = 4;
  MinGutterWidth    = 10;
  MaxGutterWidth    = 100;
  DefGutterWidth    = 50;
  MinRightMargin    = 10;
  MaxRightMargin    = 200;
  DefRightMargin    = 80;

implementation

end.
