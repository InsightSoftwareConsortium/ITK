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

int IOCommon
::Strucmp(const char *s1, const char *s2)
{
  char* s1_up = new char[strlen(s1)+1];
  char* s2_up = new char[strlen(s2)+1];
  int i;

  strcpy(s1_up, s1);
  strcpy(s2_up, s2);

  for (i = 0; s1_up[i]; i++)
    {
    s1_up[i] = islower(s1_up[i]) ? toupper(s1_up[i]) : s1_up[i];
    }
  for (i = 0; s2_up[i]; i++)
    {
    s2_up[i] = islower(s2_up[i]) ? toupper(s2_up[i]) : s2_up[i];
    }
  int rc = strcmp(s1_up, s2_up);
  delete [] s1_up;
  delete [] s2_up;
  return rc;
}

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
      slash = (char*) fileName;
      }
    else
      {
      slash++;
      }
    dot = strrchr(fileName, '.');
    if (dot == NULL)
      {
      dot = (char*) fileName + strlen(fileName);
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

// return true if the file exists
bool IOCommon
::FileExists(const char* filename)
{
  struct stat fs;
  if (stat(filename, &fs) != 0) 
    {
    return false;
    }
  else
    {
    return true;
    }
}


} // namespace itk
