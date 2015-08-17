/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkIPLFileNameList_h
#define itkIPLFileNameList_h
#include "ITKIOIPLExport.h"

#include "itkMath.h"
#include "itkMacro.h"
#include "itkObject.h"

#include <cstdio>
#include <string>
#include <list>
/** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility()); */
#define IPLSetMacroDeclaration(name, type)              \
  virtual void Set##name (const type _arg);

#define IPLSetMacroDefinition(class, name, type)              \
  void class::Set##name (const type _arg) \
  {                                        \
CLANG_PRAGMA_PUSH                          \
CLANG_SUPPRESS_Wfloat_equal                \
    if ( this->m_##name != _arg )          \
CLANG_PRAGMA_POP                           \
    {                                      \
      this->m_##name = _arg;               \
    }                                      \
  }

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility()); */
#define IPLGetMacroDeclaration(name, type) \
  virtual type Get##name ();

#define IPLGetMacroDefinition(class, name, type) \
  type class::Get##name ()   \
  {                           \
    return this->m_##name;    \
  }

namespace itk
{
/** \class IPLSortInfo
 *  \brief -- holds info on one file for IPLCommonImageIO
 * \ingroup ITKIOIPL
 */
class IPLFileSortInfo
{
public:
  IPLFileSortInfo()
  {
    m_SliceLocation = 0;
    m_SliceOffset = 0;
    m_EchoNumber = 0;
    m_ImageNumber = 0;
    m_Data = ITK_NULLPTR;
  }

  IPLFileSortInfo(const char *const filename, float sliceLocation,
                  int sliceOffset, int echoNumber, int imageNumber,
                  void *data = ITK_NULLPTR)
  {
    m_ImageFileName = filename;
    m_SliceLocation = sliceLocation;
    m_SliceOffset = sliceOffset;
    m_EchoNumber = echoNumber;
    m_ImageNumber = imageNumber;
    m_Data = data;
  }

  virtual ~IPLFileSortInfo();

  IPLSetMacroDeclaration(ImageFileName, std::string);
  IPLGetMacroDeclaration(ImageFileName, std::string);
  IPLSetMacroDeclaration(SliceLocation, float);
  IPLGetMacroDeclaration(SliceLocation, float);
  IPLSetMacroDeclaration(SliceOffset, int);
  IPLGetMacroDeclaration(SliceOffset, int);
  IPLSetMacroDeclaration(EchoNumber, int);
  IPLGetMacroDeclaration(EchoNumber, int);
  IPLSetMacroDeclaration(ImageNumber, int);
  IPLGetMacroDeclaration(ImageNumber, int);
  IPLSetMacroDeclaration(Data, void *);
  IPLGetMacroDeclaration(Data, const void *);

private:
  std::string m_ImageFileName;
  float       m_SliceLocation;
  int         m_SliceOffset;
  int         m_EchoNumber;
  int         m_ImageNumber;
  const void *m_Data;
};

/** \class IPLFileNameList
 *  \brief -- stores filename+info to be enumerated for IPLCommonImageIO
 * \ingroup ITKIOIPL
 */
class ITKIOIPL_EXPORT IPLFileNameList
{
public:
  typedef std::vector< IPLFileSortInfo * > ListType;
  typedef ListType::iterator               IteratorType;
  typedef size_t                           ListSizeType;

  enum {
    SortGlobalAscend = 0,
    SortGlobalDescend = 1,
    SortByNameAscend = 2,
    SortByNameDescend = 3
    };

  IPLFileNameList()
  {
    m_XDim = 0;
    m_YDim = 0;
    m_XRes = 0.0;
    m_YRes = 0.0;
    m_Key1 = 0;  /** Key that must be matched for image to be used,
                   i.e. seriesNumber, extensionkey */
    m_Key2 = 0;  /** Key that must be matched for image to be used, i.e.
                   echoNumber*/
    m_SortOrder = SortGlobalAscend;
  }

  virtual ~IPLFileNameList();

  IteratorType begin()
  {
    return m_List.begin();
  }

  IteratorType end()
  {
    return m_List.end();
  }

  IPLFileSortInfo * operator[](unsigned int __n)
  {
    IteratorType it = begin();
    IteratorType itend = end();

    for ( unsigned int i = 0; it != itend && i != __n; it++, i++ )
        {}
    if ( it == itend )
      {
      return ITK_NULLPTR;
      }
    return *it;
  }

  ListSizeType NumFiles() const
  {
    return m_List.size();
  }

  bool AddElementToList(char const *const filename,
                        const float sliceLocation,
                        const int offset,
                        const int XDim,
                        const int YDim,
                        const float XRes,
                        const float YRes,
                        const int imageNumber,
                        const int Key1,
                        const int Key2)
  {
    if ( m_List.empty() )
      {
      m_XDim = XDim;
      m_YDim = YDim;
      m_XRes = XRes;
      m_YRes = YRes;
      m_Key1 = Key1;
      m_Key2 = Key2;
      }
    else if ( XDim != m_XDim || YDim != m_YDim )
      {
      return false;
      }
    else if(Math::NotAlmostEquals( XRes, m_XRes ) ||
            Math::NotAlmostEquals( YRes, m_YRes) )
      {
      return false;
      }
    else if ( Key1 != m_Key1 || Key2 != m_Key2 )
      {
      return true;
      }
    IteratorType it = begin();
    IteratorType itend = end();
    while ( it != itend )
      {
      if ( std::string(filename) == ( *it )->GetImageFileName() )
        {
        return true;
        }
      it++;
      }
    m_List.push_back( new IPLFileSortInfo(filename,
                                          sliceLocation,
                                          offset,
                                          0, // echo number
                                          imageNumber) );
    return true;
  }

  void RemoveElementFromList(const int ElementToRemove)
  {
    IteratorType it = m_List.begin();
    IteratorType itend = m_List.end();
    int          i = 0;

    for ( i = 0; it != itend; i++, it++ )
      {
      if ( i != ElementToRemove )
        {
        break;
        }
      }
    if ( it == itend )
      {
      return;
      }
    m_List.erase(it);
  }

  void sortImageList();

  void sortImageListAscend();

  void sortImageListDescend();

  ListSizeType GetnumImageInfoStructs() const
  {
    return m_List.size();
  }

  IPLSetMacroDeclaration(XDim, int);
  IPLGetMacroDeclaration(XDim, int);
  IPLSetMacroDeclaration(YDim, int);
  IPLGetMacroDeclaration(YDim, int);
  IPLSetMacroDeclaration(XRes, float);
  IPLGetMacroDeclaration(XRes, float);
  IPLSetMacroDeclaration(YRes, float);
  IPLGetMacroDeclaration(YRes, float);
  IPLSetMacroDeclaration(Key1, int);
  IPLGetMacroDeclaration(Key1, int);
  IPLSetMacroDeclaration(Key2, int);
  IPLGetMacroDeclaration(Key2, int);
  IPLSetMacroDeclaration(SortOrder, int);

private:
  ListType m_List;
  int      m_XDim;
  int      m_YDim;
  float    m_XRes;
  float    m_YRes;
  int      m_Key1; /** Key that must be matched for image to be used,
                     i.e. seriesNumber, extensionkey */
  int m_Key2;      /** Key that must be matched for image to be used,
                     i.e. echoNumber */
  int m_SortOrder;
};
}
#endif                          /* itkIPLFileNameList_h */
