{

  Author:
    Ondrej Pokorny, http://www.kluug.net
    All Rights Reserved.

  License:
    CPAL 1.0 or commercial
    Please see the /license.txt file for more information.

}

{$IFDEF FPC}
  //FPC
  {$DEFINE CONDITIONALEXPRESSIONS}
  {$DEFINE O_TRYENCODEDATETIME}
  {$IFDEF FPC_HAS_TYPE_EXTENDED}
    {$DEFINE O_EXTENDEDTYPE}//e.g. Windows CE doesn't have Extended type (Extended = Double)
  {$ENDIF}
  {$DEFINE O_UTF8}
{$ELSE}
  //DELPHI
  {$DEFINE O_EXTENDEDTYPE}
  {$IFNDEF CONDITIONALEXPRESSIONS}
    //DELPHI 5
    {$DEFINE O_DELPHI_2007_DOWN}
    {$DEFINE O_DELPHI_7_DOWN}
    {$DEFINE O_DELPHI_6_DOWN}
    {$DEFINE O_DELPHI_5_DOWN}
    {$DEFINE MSWINDOWS}
    {$DEFINE O_TRYENCODEDATETIME}

    {$IFDEF VER120}//DELPHI 4
    {$DEFINE O_DELPHI_4_DOWN}
    {$ELSE}
    {$DEFINE O_DELPHI_5_UP}
    {$ENDIF}

  {$ELSE}
    {$DEFINE O_DELPHI_5_UP}

    //DELPHI 6+
    {$IF (CompilerVersion >= 25)}
      {$LEGACYIFEND ON}
    {$IFEND}

    {$IF CompilerVersion >= 14}
      {$DEFINE O_DELPHI_6_UP}
    {$IFEND}
    {$IF CompilerVersion >= 15}
      {$DEFINE O_DELPHI_7_UP}
    {$IFEND}
    {$IF CompilerVersion >= 17}
      {$DEFINE O_DELPHI_2005_UP}
    {$IFEND}
    {$IF CompilerVersion >= 18}
      {$DEFINE O_DELPHI_2006_UP}
    {$IFEND}
    {$IF CompilerVersion >= 20}
      {$DEFINE O_DELPHI_2009_UP}
    {$IFEND}
    {$IF CompilerVersion = 20}
      {$DEFINE O_DELPHI_2009}
    {$IFEND}
    {$IF CompilerVersion >= 21}
      {$DEFINE O_DELPHI_2010_UP}
    {$IFEND}
    {$IF CompilerVersion >= 22}
      {$DEFINE O_DELPHI_XE_UP}
    {$IFEND}
    {$IF CompilerVersion >= 23}
      {$DEFINE O_DELPHI_XE2_UP}
    {$IFEND}
    {$IF CompilerVersion >= 24}
      {$DEFINE O_DELPHI_XE3_UP}
    {$IFEND}
    {$IF CompilerVersion >= 25}
      {$DEFINE O_DELPHI_XE4_UP}
    {$IFEND}
    {$IF CompilerVersion >= 26}
      {$DEFINE O_DELPHI_XE5_UP}
    {$IFEND}
    {$IF CompilerVersion >= 29}
      {$DEFINE O_DELPHI_XE8_UP}
    {$IFEND}


    {$IFNDEF O_DELPHI_XE_UP}
      {$DEFINE O_DELPHI_2010_DOWN}
    {$ENDIF}
    {$IFNDEF O_DELPHI_2009_UP}
      {$DEFINE O_DELPHI_2007_DOWN}
    {$ENDIF}
    {$IF CompilerVersion < 17}
      {$DEFINE O_DELPHI_7_DOWN}
    {$IFEND}
    {$IFNDEF O_DELPHI_7_UP}
      {$DEFINE O_DELPHI_6_DOWN}
    {$ENDIF}
  {$ENDIF CONDITIONALEXPRESSIONS}
{$ENDIF FPC}

//COMMON CONSTANTS
{$IFDEF MSWINDOWS}
  {$DEFINE O_ENCODINGS_FULL}
{$ENDIF}

{$IFDEF NEXTGEN}
  {$DEFINE O_ARC}
{$ELSE}
  {$DEFINE O_HASBYTESTRINGS}
{$ENDIF}

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF DEFINED(O_DELPHI_2009_UP) OR DEFINED(FPC)}
    {$DEFINE O_UNICODE}
  {$IFEND}

  {$IF DEFINED(O_DELPHI_2009_UP)}
    {$DEFINE O_ENCODINGS_FULL}
  {$IFEND}

  {$IF DEFINED(O_DELPHI_2010_UP)}
    {$DEFINE O_GENERICS}           //IMPORTANT!!! GENERICS ARE BUGGY IN DELPHI 2009 -> DON'T USE THEM !!!
    {$DEFINE O_ANONYMOUS_METHODS}  //IMPORTANT!!! ANONYMOUS METHODS ARE BUGGY IN DELPHI 2009 -> DON'T USE THEM !!!
  {$IFEND}

  {$IF DEFINED(O_DELPHI_XE_UP)}
    {$DEFINE O_GENERICARRAY}
  {$IFEND}

  {$IF DEFINED(O_DELPHI_2009_UP) OR DEFINED(FPC)}
    {$DEFINE O_ENUMERATORS}
  {$IFEND}

  {$IF DEFINED(O_DELPHI_2006_UP) OR DEFINED(FPC)}
    {$DEFINE O_CLASS_HELPERS}
    {$DEFINE O_EXTRECORDS}
  {$IFEND}

  {$IF DEFINED(O_DELPHI_2009_UP) OR DEFINED(FPC)}
    {$DEFINE O_INLINE}//inline has a bug in Delphi 2007
  {$IFEND}

  {$IF DEFINED(O_DELPHI_XE2_UP)}
    {$DEFINE O_NAMESPACES}
  {$IFEND}
{$ENDIF CONDITIONALEXPRESSIONS}

