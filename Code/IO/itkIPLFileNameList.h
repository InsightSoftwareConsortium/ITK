/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIPLFileNameList.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIPLFileNameList_h
#define __itkIPLFileNameList_h

#include "itkMacro.h"
#include "itkObject.h"

#include <stdio.h>
#include <string>
#include <list>
/** Set built-in type.  Creates member Set"name"() (e.g., SetVisibility()); */
#define IPLSetMacro(name,type) \
  virtual void Set##name (const type _arg) \
  { \
    if (this->m_##name != _arg) \
      { \
      this->m_##name = _arg; \
      } \
  } 

/** Get built-in type.  Creates member Get"name"() (e.g., GetVisibility()); */
#define IPLGetMacro(name,type) \
  virtual type Get##name () \
  { \
    return this->m_##name; \
  }

namespace itk {
  /** \class IPLSortInfo
   *  \brief -- holds info on one file for IPLCommonImageIO
   **/
  class IPLFileSortInfo
  {
  public:
    IPLFileSortInfo() {
      m_SliceLocation = 0;
      m_SliceOffset = 0;
      m_echoNumber = 0;
      m_imageNumber = 0;
      m_data = 0;
    }
    virtual ~IPLFileSortInfo() {
    }
    IPLFileSortInfo(const char *const filename,float SliceLocation,
                 int SliceOffset,int echoNumber,int imageNumber,void *data = 0)
    {
      m_imageFileName = filename;
      m_SliceLocation = SliceLocation;
      m_SliceOffset = SliceOffset;
      m_echoNumber = echoNumber;
      m_imageNumber = imageNumber;
      m_data = data;
    }
      
    IPLSetMacro(imageFileName,std::string );
    IPLGetMacro(imageFileName,std::string );
    IPLSetMacro(SliceLocation,float );
    IPLGetMacro(SliceLocation,float );
    IPLSetMacro(SliceOffset,int );
    IPLGetMacro(SliceOffset,int );
    IPLSetMacro(echoNumber,int );
    IPLGetMacro(echoNumber,int );
    IPLSetMacro(imageNumber,int );
    IPLGetMacro(imageNumber,int );
    IPLSetMacro(data,void *);
    IPLGetMacro(data,const void *);
  private:
    std::string m_imageFileName;
    float m_SliceLocation;
    int m_SliceOffset;
    int m_echoNumber;
    int m_imageNumber;
    const void *m_data;
  };


  /** \class IPLFileNameList
   *  \brief -- stores filename+info to be enumerated for IPLCommonImageIO
   **/
  class IPLFileNameList
  {
  public:
    typedef std::vector<IPLFileSortInfo *> ListType;
    typedef ListType::iterator IteratorType;
    
   enum { SortGlobalAscend = 0,
          SortGlobalDescend = 1,
          SortByNameAscend = 2,
          SortByNameDescend = 3
   };
    
    
    IPLFileNameList() 
      {
      m_XDim = 0;
      m_YDim = 0;
      m_Key1 = 0;  /** Key that must be matched for image to be used, 
                     i.e. seriesNumber, extensionkey*/
      m_Key2 = 0;  /** Key that must be matched for image to be used, i.e. echoNumber*/
      m_SortOrder = SortGlobalAscend; 
      }
    virtual ~IPLFileNameList()
    {
      IteratorType it = begin();
      IteratorType itend = end();
      while(it != itend)
        {
        delete (*it);
        it++;
        }
    }
    IteratorType begin() { return m_List.begin(); }
    IteratorType end() { return m_List.end(); }
    IPLFileSortInfo *operator[](unsigned int __n)
    {
      IteratorType it = begin();
      IteratorType itend= end();
      for(unsigned int i = 0; it != itend && i != __n; it++, i++)
        ;
      if(it == itend)
        return 0;
      return *it;
    }
    signed int NumFiles() const {
      return m_List.size();
    }
    bool AddElementToList(char const *const filename, 
                                   const float sliceLocation, 
                                   const int offset, 
                                   const int XDim, 
                                   const int YDim, 
                                   const int imageNumber, 
                                   const int Key1,
                                   const int Key2)
      {
      if(m_List.empty())
        {
        m_XDim = XDim;
        m_YDim = YDim;
        m_Key1 = Key1;
        m_Key2 = Key2;
        }
      else if(XDim != m_XDim || YDim != YDim)
        {
        return false;
        }
      else if(Key1 != m_Key1 || Key2 != m_Key2)
        {
        return true;
        }
      IteratorType it = begin();
      IteratorType itend = end();
      while(it != itend)
        {
        if(std::string(filename) == (*it)->GetimageFileName())
          return true;
        it++;
        }
      m_List.push_back(new IPLFileSortInfo(filename,
                                        sliceLocation,
                                        offset,
                                        0, // echo number
                                        imageNumber));
      return true;
      }
    void RemoveElementFromList(const int ElementToRemove)
      {
      IteratorType it = m_List.begin();
      IteratorType itend = m_List.end();
      int i = 0;
      for(i = 0; it != itend; i++, it++)
        {
        if(i != ElementToRemove)
          break;
        }
      if(it == itend)
        return;
      m_List.erase(it);
   }

         
    void sortImageList();
    void sortImageListAscend();
    void sortImageListDescend();

    int GetnumImageInfoStructs() const
    {
       return m_List.size();
    }
    IPLSetMacro(XDim,int );
    IPLGetMacro(XDim,int );
    IPLSetMacro(YDim,int );
    IPLGetMacro(YDim,int );
    IPLSetMacro(Key1,int );
    IPLGetMacro(Key1,int );
    IPLSetMacro(Key2,int );
    IPLGetMacro(Key2,int );
    IPLSetMacro(SortOrder,int );
  private:
    ListType m_List;
    int m_XDim;
    int m_YDim;
    int m_Key1;  /** Key that must be matched for image to be used, 
                     i.e. seriesNumber, extensionkey*/
    int m_Key2;  /** Key that must be matched for image to be used, i.e. echoNumber*/
    int m_SortOrder;
  };

}

#endif                          /* __itkIPLFileNameList_h */
