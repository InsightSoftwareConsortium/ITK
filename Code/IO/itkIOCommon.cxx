/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIOCommon.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkIOCommon.h"
#include <sys/stat.h>
#include <cstring>


namespace itk
{
const char *const ITK_OnDiskStorageTypeName = "ITK_OnDiskStorageTypeName"; 
const char *const ITK_ImageFileBaseName = "ITK_ImageFileBaseName";
const char *const ITK_VoxelUnits = "ITK_VoxelUnits";
const char *const ITK_OnDiskBitPerPixel = "ITK_OnDiskBitPerPixel";
const char *const SPM_ROI_SCALE = "SPM_ROI_SCALE";
const char *const ITK_FileNotes = "ITK_FileNotes";
#if defined(ITKIO_DEPRECATED_METADATA_ORIENTATION)
const char *const ITK_CoordinateOrientation = "ITK_CoordinateOrientation";
#endif
const char *const ITK_Origin = "ITK_Origin";
const char *const ITK_Spacing = "ITK_Spacing";
const char *const ITK_FileOriginator = "ITK_FileOriginator";
const char *const ITK_OriginationDate = "ITK_OriginationDate";
const char *const ITK_PatientID = "ITK_PatientID";
const char *const ITK_ExperimentDate = "ITK_ExperimentDate";
const char *const ITK_ExperimentTime = "ITK_ExperimentTime";
const char *const ITK_InputFilterName = "ITK_InputFilterName";
const char *const ITK_NumberOfDimensions = "ITK_NumberOfDimensions";
const char *const ITK_PatientName = "ITK_PatientName";
const char *const ITK_ScanID = "ITK_ScanID";
const char *const ITK_ImageType = "ITK_ImageType";
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


char* IOCommon
::ExtractFileName (const char* fileName)
{
  itkGenericLegacyReplaceBodyMacro(itk::IOCommon::ExtractFileName, 
                                   3.6, 
                                   itksys::SystemTools::GetFilenameName);

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
  itkGenericLegacyReplaceBodyMacro(itk::IOCommon::ExtractFileExtension, 
                                   3.6, 
                                   itksys::SystemTools::GetFilenameExtension);

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
  itkGenericLegacyReplaceBodyMacro(itk::IOCommon::ExtractFilePath, 
                                   3.6, 
                                   itksys::SystemTools::GetFilenamePath);

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


} // namespace itk
