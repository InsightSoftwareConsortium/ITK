/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkPolygonGroupSpatialObject.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPolygonGroupSpatialObject_txx
#define __itkPolygonGroupSpatialObject_txx

#include "itkPolygonGroupSpatialObject.h"

namespace itk
{


template <unsigned int TDimension >
bool PolygonGroupSpatialObject<TDimension>::
AddStrand(PolygonSpatialObject<TDimension> *toAdd)
{
  this->AddSpatialObject(toAdd);
  return true;
}

template <unsigned int TDimension >
bool PolygonGroupSpatialObject<TDimension>::
DeleteStrand(PolygonSpatialObject<TDimension> *toDelete)
{
  this->RemoveSpatialObject(toDelete);
  return true;
}

template <unsigned int TDimension >
bool PolygonGroupSpatialObject<TDimension>::
ReplaceStrand(PolygonSpatialObject<TDimension> *toReplace,PolygonSpatialObject<TDimension> *replacement)
{
  TreeNodeChildrenListType & children = this->GetTreeNode()->GetChildrenList();
  typename TreeNodeChildrenListType::iterator it = children.begin();
  typename TreeNodeChildrenListType::iterator itend = children.end();
  while(it != itend)
    {
    if(static_cast<void *>((*it)) == static_cast<void *>(toReplace))
      {
      typename TreeNodeChildrenListType::iterator after = it;
      after++;
      children.insert(after,1,replacement->GetTreeNode());
      children.erase(it);
      return true;
      }
    it++;
    }
  return false;
}

template <unsigned int TDimension >
bool PolygonGroupSpatialObject<TDimension>::
IsClosed()
{
  TreeNodeChildrenListType & children = this->GetTreeNode()->GetChildrenList();
  typename TreeNodeChildrenListType::iterator it = children.begin();
  typename TreeNodeChildrenListType::iterator itend = children.end();
  while(it != itend) 
    {
    PolygonSpatialObject<TDimension> *curstrand =
      dynamic_cast<PolygonSpatialObject<TDimension> *>((*it).GetPointer());
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
unsigned PolygonGroupSpatialObject<TDimension>::
NumberOfStrands()
{
  return this->GetTreeNode()->GetNumberOfChildren();
}

template <unsigned int TDimension >
double PolygonGroupSpatialObject<TDimension>::
Volume()
{
  double volume = 0;
  ChildrenListType * children = this->GetChildren();
  typename ChildrenListType::iterator it = children->begin();
  typename ChildrenListType::iterator itend = children->end();
  while(it != itend)
    {
    PolygonSpatialObject<TDimension> *curstrand =
      dynamic_cast<PolygonSpatialObject<TDimension> *>((*it).GetPointer());
    volume += curstrand->MeasureVolume();
    it++;
    }
  delete children;
  return volume;
}

template <unsigned int TDimension >
double PolygonGroupSpatialObject<TDimension>::
MeasureVolume()
{
  return this->Volume();
}

template <unsigned int TDimension >
bool PolygonGroupSpatialObject<TDimension>::
IsInside( const PointType & point,unsigned int ,char * name) const
{
  // want to encompass all children, at least 2 levels, but to be
  // safe say 4;
  const_cast<Self *>(this)->SetBoundingBoxChildrenDepth(4);
  const_cast<Self *>(this)->SetBoundingBoxChildrenName("");
  const_cast<Self *>(this)->ComputeBoundingBox();
  BoundingBoxType *bounds = const_cast<Self *>(this)->GetBoundingBox();
  if(!bounds->IsInside(point))
    {
    return false;
    }
  return this->SpatialObject<TDimension>::IsInside(point,4,name);
}

}
#endif
