/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIPLFileNameList.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/

#include <stdlib.h>
#include "itkIPLFileNameList.h"
#include <assert.h>
#include <string.h>
#include <ctype.h>

#include <list>
#include <functional>
namespace itk
{
struct IPLFileSortInfo_ascend_compare :
    public std::greater<IPLFileSortInfo *>
{
private:
  int qsort_IPLFileSortInfo_ascend_compar (IPLFileSortInfo *item1,IPLFileSortInfo *item2)
  {
    const int ImageNoDiff= item1->GetimageNumber() -  item2->GetimageNumber();
    if( ImageNoDiff < 0)
      {
      return -1;
      }
    if( ImageNoDiff > 0 )
      {
      return 1;
      }
    const int echoNumDiff=item1->GetechoNumber() - item2->GetechoNumber();
    if (echoNumDiff < 0)
      {
      return -1;
      }
    else if (echoNumDiff > 0 )
      {
      return 1;
      }
    const float sliceGap = item1->GetSliceLocation() - item2->GetSliceLocation();
    if (sliceGap < 0.0)
      {
      return -1;
      }
    if (sliceGap > 0.0)
      {
      return 1;
      }
    return item2->GetimageFileName() < item1->GetimageFileName();
  }
public:
  bool operator()(IPLFileSortInfo *item1,IPLFileSortInfo *item2)
  {
    return qsort_IPLFileSortInfo_ascend_compar(item1,item2) < 0;
  }
 };

struct IPLFileSortInfo_descend_compare :
    public std::greater<IPLFileSortInfo *>
{
private:
  int qsort_IPLFileSortInfo_descend_compar (IPLFileSortInfo *item1,  IPLFileSortInfo *item2)
  {
    const int ImageNoDiff= item1->GetimageNumber() -  item2->GetimageNumber();
    if( ImageNoDiff < 0)
      {
      return 1;
      }
    if( ImageNoDiff > 0 )
      {
      return -1;
      }
    const int echoNumDiff=item1->GetechoNumber() - item2->GetechoNumber();
    if ( echoNumDiff < 0)
      {
      return 1;
      }
    if ( echoNumDiff > 0)
      {
      return -1;
      }
    const float sliceGap = item1->GetSliceLocation() - item2->GetSliceLocation();
    if (sliceGap < 0.0)
      {
      return 1;
      }
    if (sliceGap > 0.0)
      {
      return -1;
      }
    return item1->GetimageFileName()  >= item2->GetimageFileName();
    
  }


public:
  bool operator()(IPLFileSortInfo *item1,IPLFileSortInfo *item2)
  {
    return qsort_IPLFileSortInfo_descend_compar(item1,item2) < 0;
  }
 };

void
IPLFileNameList::
sortImageListAscend ()
{
#if 0
  IPLFileSortInfo_ascend_compare comp;
  //  qsort (fnList->Info, fnList->numImageInfoStructs, sizeof (IPLFileSortInfo),
  //         qsort_IPLFileSortInfo_ascend_compar);
  m_List.sort<IPLFileSortInfo_ascend_compare>(comp);
#else
  m_List.sort( IPLFileSortInfo_ascend_compare() );
#endif
  return;
}

void
IPLFileNameList::
sortImageListDescend ()
{
  //  qsort (fnList->Info, fnList->numImageInfoStructs, sizeof (IPLFileSortInfo),
  //     qsort_IPLFileSortInfo_descend_compar);
#if 0
  IPLFileSortInfo_descend_compare comp;
  m_List.sort<IPLFileSortInfo_descend_compare>(comp);
#else
  m_List.sort( IPLFileSortInfo_descend_compare() );
#endif
  return;
}

}
