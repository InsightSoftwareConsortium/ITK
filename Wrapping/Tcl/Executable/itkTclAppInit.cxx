/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTclAppInit.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkLightObject.h"
#include "itkTclConfigure.h"

#include <sys/stat.h>
#include <string.h>

#ifndef ITK_TCL_NO_TK
#  include <tk.h>
#else
#  include <tcl.h>
#endif

//----------------------------------------------------------------------------
// Definitions related to build tree locations:
//   ITK_TCL_EXE_DIR  = Location of this executable.
//   ITK_TCL_LIB_DIR  = Location of the pkgIndex.tcl file for ITK.
#if defined(CMAKE_INTDIR)
# define ITK_TCL_EXE_DIR ITK_TCL_EXE_DIR_BUILD "/" CMAKE_INTDIR
# define ITK_TCL_LIB_DIR ITK_BINARY_DIR "/Wrapping/Tcl/" CMAKE_INTDIR
#else
# define ITK_TCL_EXE_DIR ITK_TCL_EXE_DIR_BUILD
# define ITK_TCL_LIB_DIR ITK_BINARY_DIR "/Wrapping/Tcl"
#endif

// ITK_TCL_EXE_NAME = Name of this executable.
#if defined(_WIN32)
# define ITK_TCL_EXE_NAME ITK_TCL_EXE_NAME_ROOT ".exe"
#else
# define ITK_TCL_EXE_NAME ITK_TCL_EXE_NAME_ROOT
#endif


//----------------------------------------------------------------------------
int itkTclAppInit(Tcl_Interp* interp);
bool itkTclAppInitCheckSameExecutable(Tcl_Interp* interp);

//----------------------------------------------------------------------------
/** Program entry point.  */
int main(int argc, char** argv)
{
  std::ios::sync_with_stdio();
#ifndef ITK_TCL_NO_TK
  Tk_Main(argc, argv, &itkTclAppInit);
#else
  Tcl_Main(argc, argv, &itkTclAppInit);
#endif
  return 0;
}

//----------------------------------------------------------------------------
// Get the Tcl package initialization functions to call directly.
extern "C"
{
  int Vxlnumericstcl_Init(Tcl_Interp*);
  int Itknumericstcl_Init(Tcl_Interp*);
  int Itkcommontcl_Init(Tcl_Interp*);
  int Itkiotcl_Init(Tcl_Interp*);
  int Itkbasicfilterstcl_Init(Tcl_Interp*);
  int Itkalgorithmstcl_Init(Tcl_Interp*);
}

//----------------------------------------------------------------------------
/** Main application initialization function.  */
int itkTclAppInit(Tcl_Interp* interp)
{
  // Initialize Tcl.
  if(Tcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  
#ifndef ITK_TCL_NO_TK
  // Initialize Tk.
  if(Tk_Init(interp) != TCL_OK) { return TCL_ERROR; }
#endif
  
  if(itkTclAppInitCheckSameExecutable(interp))
    {
    // Running from build tree, load the pkgIndex.tcl file from it.
    char pkgIndexScript[] = "source {" ITK_TCL_LIB_DIR "/pkgIndex.tcl}\n";
    if(Tcl_GlobalEval(interp, pkgIndexScript) != TCL_OK) { return TCL_ERROR; }
    }
  else
    {
    // Not running from build tree, load the pkgIndex.tcl file if
    // there is one next to the exectuable.  If it does not exist,
    // just assume the user has configured TCLLIBPATH correctly.
    char pkgIndexScript[] =
      "set itkTclAppInit_pkgIndex_tcl \\\n"
      "  [file join [file dirname [info nameofexecutable]] pkgIndex.tcl]\n"
      "if {[file exists $itkTclAppInit_pkgIndex_tcl]} {\n"
      "  source $itkTclAppInit_pkgIndex_tcl\n"
      "}\n"
      "unset itkTclAppInit_pkgIndex_tcl";
    if(Tcl_GlobalEval(interp, pkgIndexScript) != TCL_OK) { return TCL_ERROR; }
    }
  
  // Initialize the built-in packages.
  if(Vxlnumericstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itknumericstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkcommontcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkiotcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkbasicfilterstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  if(Itkalgorithmstcl_Init(interp) != TCL_OK) { return TCL_ERROR; }
  
  // Initialize all ITK Tcl packages.
  static char initScript[] = "package require InsightToolkit 0.7";
  if(Tcl_GlobalEval(interp, initScript) != TCL_OK) { return TCL_ERROR; }
  
  // Allow users to have an initialization file for interactive mode.
  static char rcFileNameVariable[] = "tcl_rcFileName";
  static char rcFileNameValue[] = "~/.itktclrc";
  Tcl_SetVar(interp, rcFileNameVariable, rcFileNameValue, TCL_GLOBAL_ONLY);
  
  return TCL_OK;
}

//----------------------------------------------------------------------------
bool itkTclAppInitCheckSameExecutable(Tcl_Interp* interp)
{
  // Get the name of the actual executable.
  char nameScript[] = "info nameofexecutable";
  if(Tcl_GlobalEval(interp, nameScript) != TCL_OK) { return TCL_ERROR; }
  std::string nameOfExecutable = Tcl_GetStringResult(interp);
  
  // Get the name of the executable in the build tree.
  std::string buildExecutable = ITK_TCL_EXE_DIR "/" ITK_TCL_EXE_NAME;
  
  const char* file1 = nameOfExecutable.c_str();
  const char* file2 = buildExecutable.c_str();
  
#if defined(_WIN32)
  struct stat fileStat1, fileStat2;
  if (stat(file1, &fileStat1) == 0 && stat(file2, &fileStat2) == 0)
    {
    HANDLE hFile1, hFile2;
    
    hFile1 = CreateFile(file1, GENERIC_READ, FILE_SHARE_READ, NULL,
                        OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    hFile2 = CreateFile(file2, GENERIC_READ, FILE_SHARE_READ, NULL,
                        OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);
    if( hFile1 == INVALID_HANDLE_VALUE || hFile2 == INVALID_HANDLE_VALUE)
      {
      if(hFile1 != INVALID_HANDLE_VALUE)
        {
        CloseHandle(hFile1);
        }
      if(hFile2 != INVALID_HANDLE_VALUE)
        {
        CloseHandle(hFile2);
        }
      return false;
      }
    
    BY_HANDLE_FILE_INFORMATION fiBuf1;
    BY_HANDLE_FILE_INFORMATION fiBuf2;
    GetFileInformationByHandle( hFile1, &fiBuf1 );
    GetFileInformationByHandle( hFile2, &fiBuf2 );
    CloseHandle(hFile1);
    CloseHandle(hFile2);
    return (fiBuf1.nFileIndexHigh == fiBuf2.nFileIndexHigh &&
            fiBuf1.nFileIndexLow == fiBuf2.nFileIndexLow);
    }
  return false;
#else
  struct stat fileStat1, fileStat2;
  if (stat(file1, &fileStat1) == 0 && stat(file2, &fileStat2) == 0)
    {
    // see if the files are the same file
    // check the device inode and size
    if(memcmp(&fileStat2.st_dev, &fileStat1.st_dev, sizeof(fileStat1.st_dev)) == 0 && 
       memcmp(&fileStat2.st_ino, &fileStat1.st_ino, sizeof(fileStat1.st_ino)) == 0 &&
       fileStat2.st_size == fileStat1.st_size 
      ) 
      {
      return true;
      }
    }
  return false;
#endif
}
