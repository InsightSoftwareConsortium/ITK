/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkROISpatialObject.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itk_ROISpatialObject_txx_
#define __itk_ROISpatialObject_txx_

#include "itkROISpatialObject.h"

namespace itk
{


template <unsigned int TDimension >
bool ROISpatialObject<TDimension>::
AddStrand(ROIStrandSpatialObject<TDimension> *toAdd)
{
  this->AddSpatialObject(toAdd);
  return true;
}

template <unsigned int TDimension >
bool ROISpatialObject<TDimension>::
DeleteStrand(ROIStrandSpatialObject<TDimension> *toDelete)
{
  this->RemoveSpatialObject(toDelete);
  return true;
}

template <unsigned int TDimension >
bool ROISpatialObject<TDimension>::
ReplaceStrand(ROIStrandSpatialObject<TDimension> *toReplace,ROIStrandSpatialObject<TDimension> *replacement)
{
  typename SpatialObject<TDimension>::iterator it = this->m_Children.begin();
  typename SpatialObject<TDimension>::iterator itend = this->m_Children.end();
  while(it != itend)
    {
    if((*it) == toReplace)
      {
      typename SpatialObject<TDimension>::iterator after = it;
      after++;
      this->m_Children.insert(after,1,replacement);
      this->m_Children.erase(it);
      return true;
      }
    it++;
    }
  return false;
}

template <unsigned int TDimension >
bool ROISpatialObject<TDimension>::
IsClosed()
{
  typename SpatialObject<TDimension>::iterator it = this->m_Children.begin();
  typename SpatialObject<TDimension>::iterator itend = this->m_Children.end();
  while(it != itend) 
    {
    ROIStrandSpatialObject<TDimension> *curstrand =
        dynamic_cast<ROIStrandSpatialObject<TDimension> *>((*it));
      if(curstrand != 0)
        {
        if (!curstrand->IsClosed())
          {
          return false;
          }
        }
      it++;
    }
  return true;
}

template <unsigned int TDimension >
unsigned ROISpatialObject<TDimension>::
NumberOfStrands()
{
  return this->m_Children.size();
  return 0;
}

template <unsigned int TDimension >
double ROISpatialObject<TDimension>::
Volume()
{
  double volume = 0;
  typename SpatialObject<TDimension>::iterator it = this->m_Children.begin();
  typename SpatialObject<TDimension>::iterator itend = this->m_Children.end();
  while(it != itend)
    {
    volume += (*it)->MeasureVolume();
    it++;
    }
  return volume;
}

template <unsigned int TDimension >
double ROISpatialObject<TDimension>::
MeasureVolume()
{
  return this->Volume();
}

template <unsigned int TDimension >
bool ROISpatialObject<TDimension>::
IsInside( const PointType & point,unsigned int depth,char * name) const
{
  // want to encompass all children, at least 2 levels, but to be
  // safe say 4;
  const_cast<Self *>(this)->ComputeBoundingBox(4,NULL);
  BoundingBoxType *bounds = const_cast<Self *>(this)->GetBoundingBox();
  if(!bounds->IsInside(point))
    {
    return false;
    }
  return this->SpatialObject<TDimension>::IsInside(point,depth,name);
}

}
#endif
