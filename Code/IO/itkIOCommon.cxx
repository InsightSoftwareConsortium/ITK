/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkIOCommon.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkIOCommon.h"
#include <sys/stat.h>


namespace itk
{
const char *const ITK_OnDiskStorageTypeName = "ITK_OnDiskStorageTypeName"; 
const char *const ITK_ImageFileBaseName = "ITK_ImageFileBaseName";
const char *const ITK_VoxelUnits = "ITK_VoxelUnits";
const char *const ITK_OnDiskBitPerPixel = "ITK_OnDiskBitPerPixel";
const char *const SPM_ROI_SCALE = "SPM_ROI_SCALE";
const char *const ITK_FileNotes = "ITK_FileNotes";
const char *const ITK_Orientation = "ITK_Orientation";
const char *const ITK_FileOriginator = "ITK_FileOriginator";
const char *const ITK_OriginationDate = "ITK_OriginationDate";
const char *const ITK_PatientID = "ITK_PatientID";
const char *const ITK_ExperimentDate = "ITK_ExperimentDate";
const char *const ITK_ExperimentTime = "ITK_ExperimentTime";
const char *const ITK_InputFilterName = "ITK_InputFilterName";
const char *const ROI_NAME = "ROI_NAME";
const char *const ROI_X_SIZE = "ROI_X_SIZE";
const char *const ROI_X_RESOLUTION = "ROI_X_RESOLUTION";
const char *const ROI_Y_SIZE = "ROI_Y_SIZE";
const char *const ROI_Y_RESOLUTION = "ROI_Y_RESOLUTION";
const char *const ROI_Z_SIZE = "ROI_Z_SIZE";
const char *const ROI_Z_RESOLUTION = "ROI_Z_RESOLUTION";
const char *const ROI_NUM_SEGMENTS = "ROI_NUM_SEGMENTS";
const char *const ROI_PLANE = "ROI_PLANE";
const char *const ROI_SCAN_ID = "ROI_SCAN_ID";

std::string IOCommon
::AtomicPixelTypeToString(const AtomicPixelType pixelType)
{
  switch(pixelType)
    {
    case ITK_UCHAR:
      return "unsigned char";
      break;
    case ITK_CHAR:
      return "char";
      break;
    case ITK_USHORT:
      return "unsigned short";
      break;
    case ITK_SHORT:
      return "short";
      break;
    case ITK_UINT:
      return "unsigned int";
      break;
    case ITK_INT:
      return "int";
      break;
    case ITK_ULONG:
      return "unsigned long";
      break;
    case ITK_LONG:
      return "long";
      break;
    case ITK_FLOAT:
      return "float";
      break;
    case ITK_DOUBLE:
      return "double";
      break;
    default:
      return "unknown";
      break;
    }
}

unsigned int IOCommon
::ComputeSizeOfAtomicPixelType(const AtomicPixelType pixelType)
{
  switch (pixelType)
    {
    case ITK_CHAR:
      return sizeof(char);
      break;
    case ITK_UCHAR:
      return sizeof(unsigned char);
      break;
    case ITK_SHORT:
      return sizeof(short);
      break;
    case ITK_USHORT:
      return sizeof(unsigned short);
      break;
    case ITK_INT:
      return sizeof(int);
      break;
    case ITK_UINT:
      return sizeof(unsigned int);
      break;
    case ITK_LONG:
      return sizeof(long);
      break;
    case ITK_ULONG:
      return sizeof(unsigned long);
      break;
    case ITK_FLOAT:
      return sizeof(float);
      break;
    case ITK_DOUBLE:
      return sizeof(double);
      break;
    default:
      return sizeof(char);
      break;
    }
}

//   int IOCommon
//   ::Strucmp(const char *s1, const char *s2)
//   {
//     char* s1_up = new char[strlen(s1)+1];
//     char* s2_up = new char[strlen(s2)+1];
//     int i;

//     strcpy(s1_up, s1);
//     strcpy(s2_up, s2);

//     for (i = 0; s1_up[i]; i++)
//       {
//         s1_up[i] = islower(s1_up[i]) ? toupper(s1_up[i]) : s1_up[i];
//       }
//     for (i = 0; s2_up[i]; i++)
//       {
//         s2_up[i] = islower(s2_up[i]) ? toupper(s2_up[i]) : s2_up[i];
//       }
//     int rc = strcmp(s1_up, s2_up);
//     delete [] s1_up;
//     delete [] s2_up;
//     return rc;
//   }

char* IOCommon
::ExtractFileName (const char* fileName)
{
  const char* dot;
  const char* slash;
  char* fName = NULL;

  if (fileName != NULL)
    {
    slash = strrchr(fileName, '/');
    if (slash == NULL)
      {
      slash = strrchr(fileName, '\\');
      }
    if (slash == NULL)
      {
      slash = (const char*) fileName;
      }
    else
      {
      slash++;
      }
    dot = strrchr(fileName, '.');
    if (dot == NULL)
      {
      dot = (const char*) fileName + strlen(fileName);
      }
    fName = new char[strlen(slash) - strlen(dot) + 1];
    strncpy(fName, slash, strlen(slash) - strlen(dot));
    fName[strlen(slash) - strlen(dot)] = '\0';
    }

  return fName;
}

char* IOCommon
::ExtractFileExtension (const char* fileName)
{
  const char* dot;
  char* fExtension = NULL;

  dot = strrchr(fileName, '.');
  if (dot != NULL)
    {
    dot++;
    fExtension = new char[strlen(dot)+1];
    strcpy(fExtension, dot);
    fExtension[strlen(dot)] = '\0';
    }

  return fExtension;
}

char* IOCommon
::ExtractFilePath (const char* fileName)
{
  const char* slash;
  char* fPath = NULL;

  if (fileName != NULL)
    {
    slash = strrchr(fileName, '/');
    if (slash == NULL)
      {
      slash = strrchr(fileName, '\\');
      }
    if (slash == NULL)
      {
      fPath = NULL;
      }
    else
      {
      slash++;
      fPath = new char[strlen(fileName) - strlen(slash) + 1];
      strncpy(fPath, fileName, strlen(fileName) - strlen(slash));
      fPath[strlen(fileName) - strlen(slash)] = '\0';
      }
    }

  return fPath;
}

//   // return true if the file exists
//   bool IOCommon
//   ::FileExists(const char* filename)
//   {
//     struct stat fs;
//     if (stat(filename, &fs) != 0) 
//       {
//         return false;
//       }
//     else
//       {
//         return true;
//       }
//   }
//   // return size of file; also returns zero if no file exists
//   unsigned long IOCommon
//   ::FileLength(const char* filename)
//   {
//     struct stat fs;
//     if (stat(filename, &fs) != 0) 
//       {
//         return 0;
//       }
//     else
//       {
//         return fs.st_size;
//       }
//   }

//   char *IOCommon
//   ::RealPath(const char *path, char *resolved_path)
//   {
// #if defined(_WIN32)
//     char pathpart[itk::IOCommon::ITK_MAXPATHLEN];
//     strcpy(pathpart,path);
//     char fnamepart[itk::IOCommon::ITK_MAXPATHLEN];
//     char *slash;

//     if((slash = strrchr(pathpart,'/')) == NULL)
//       {
//         slash = strrchr(pathpart,'\\');
//       }

//     if(slash == NULL) // no path part, so just use current dir.
//       {
//         Getcwd(pathpart,sizeof(pathpart));
//         strcpy(fnamepart,path);
//       } 
//     else // change directory to path part, getcwd to find OS resolved path
//       {
//         *slash = '\0';
//         strcpy(fnamepart,slash+1);

//         char savedir[itk::IOCommon::ITK_MAXPATHLEN];
//         Getcwd(savedir,sizeof(savedir));
//         Chdir(pathpart);
//         Getcwd(pathpart,sizeof(pathpart));
//         Chdir(savedir);
//       }
//     strcpy(resolved_path,pathpart);
//     strcat(resolved_path,"/");
//     strcat(resolved_path,fnamepart);
//     return resolved_path;
// #else
//     return realpath(path,resolved_path);
// #endif
//   }

} // namespace itk
