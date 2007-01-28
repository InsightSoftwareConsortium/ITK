/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaContourConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaContourConverter_txx
#define __itkMetaContourConverter_txx

#include "itkMetaContourConverter.h"

namespace itk  
{

/** Constructor */ 
template <unsigned int NDimensions>
MetaContourConverter<NDimensions>
::MetaContourConverter()
{
}

/** Convert a metaContour into an Contour SpatialObject  */
template <unsigned int NDimensions>
typename MetaContourConverter<NDimensions>::SpatialObjectPointer
MetaContourConverter<NDimensions>
::MetaContourToContourSpatialObject(MetaContour * Contour)
{ 

  typedef itk::ContourSpatialObject<NDimensions> ContourSpatialObjectType;
  typename ContourSpatialObjectType::Pointer contour = 
                                          ContourSpatialObjectType::New();

  double spacing[NDimensions];
  
  unsigned int ndims = Contour->NDims();
  for(unsigned int i=0;i<ndims;i++)
    {
    spacing[i]=Contour->ElementSpacing()[i];
    }
  contour->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  contour->GetProperty()->SetName(Contour->Name());
  contour->SetId(Contour->ID());
  contour->SetParentId(Contour->ParentID());
  contour->GetProperty()->SetRed(Contour->Color()[0]);
  contour->GetProperty()->SetGreen(Contour->Color()[1]);
  contour->GetProperty()->SetBlue(Contour->Color()[2]);
  contour->GetProperty()->SetAlpha(Contour->Color()[3]);
  contour->SetClosed(Contour->Closed());
  contour->SetAttachedToSlice(Contour->AttachedToSlice());
  contour->SetDisplayOrientation(Contour->DisplayOrientation());

  // First the control points
  typedef typename ContourSpatialObjectType::ControlPointType ControlPointType;
  
  typedef MetaContour::ControlPointListType ControlListType;
  ControlListType::iterator itCP = Contour->GetControlPoints().begin();
      
  for(unsigned int id=0;id< Contour->GetControlPoints().size();id++)
    {
    ControlPointType pnt;
    
    typedef typename ControlPointType::PointType PointType;
    PointType point;
    PointType pickedPoint;

    typedef typename ControlPointType::VectorType VectorType;
    VectorType normal;

    for(unsigned int i=0;i<ndims;i++)
      {
      point[i]=(*itCP)->m_X[i];
      }

    for(unsigned int i=0;i<ndims;i++)
      {
      pickedPoint[i]=(*itCP)->m_XPicked[i];
      }

    for(unsigned int i=0;i<ndims;i++)
      {
      normal[i]=(*itCP)->m_V[i];
      }

    pnt.SetID((*itCP)->m_Id);
    pnt.SetRed((*itCP)->m_Color[0]);
    pnt.SetGreen((*itCP)->m_Color[1]);
    pnt.SetBlue((*itCP)->m_Color[2]);
    pnt.SetAlpha((*itCP)->m_Color[3]);

    pnt.SetPosition(point);
    pnt.SetPickedPoint(pickedPoint);
    pnt.SetNormal(normal);

    contour->GetControlPoints().push_back(pnt);
    itCP++;
    }

  // Then the interpolated points
  typedef typename ContourSpatialObjectType::InterpolatedPointType 
                                                        InterpolatedPointType;
  typedef MetaContour::InterpolatedPointListType        InterpolatedListType;

  InterpolatedListType::iterator itI = Contour->GetInterpolatedPoints().begin();
      
  for(unsigned int id=0;id< Contour->GetInterpolatedPoints().size();id++)
    {
    InterpolatedPointType pnt;
    
    typedef typename ControlPointType::PointType PointType;
    PointType point;

    typedef typename ControlPointType::VectorType VectorType;
    VectorType normal;

    for(unsigned int i=0;i<ndims;i++)
      {
      point[i]=(*itI)->m_X[i];
      }
   
    pnt.SetID((*itI)->m_Id);
    pnt.SetRed((*itI)->m_Color[0]);
    pnt.SetGreen((*itI)->m_Color[1]);
    pnt.SetBlue((*itI)->m_Color[2]);
    pnt.SetAlpha((*itI)->m_Color[3]);

    pnt.SetPosition(point);
    contour->GetInterpolatedPoints().push_back(pnt);
    itI++;
    }

  return contour;
}

/** Convert an Contour SpatialObject into a metaContour */
template <unsigned int NDimensions>
MetaContour*
MetaContourConverter<NDimensions>
::ContourSpatialObjectToMetaContour(SpatialObjectType * spatialObject)
{ 
  MetaContour* Contour = new MetaContour(NDimensions);

  // fill in the control points information
  typename SpatialObjectType::ControlPointListType::const_iterator itCP;
  for(itCP = dynamic_cast<SpatialObjectType*>(spatialObject)->GetControlPoints().begin(); 
      itCP != dynamic_cast<SpatialObjectType*>(spatialObject)->GetControlPoints().end(); 
      itCP++)
    {
    ContourControlPnt* pnt = new ContourControlPnt(NDimensions);
   
    pnt->m_Id = (*itCP).GetID();

    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_X[d]=(*itCP).GetPosition()[d];
      } 

    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_XPicked[d]=(*itCP).GetPickedPoint()[d];
      } 

    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_V[d]=(*itCP).GetNormal()[d];
      }  

    pnt->m_Color[0] = (*itCP).GetRed();
    pnt->m_Color[1] = (*itCP).GetGreen();
    pnt->m_Color[2] = (*itCP).GetBlue();
    pnt->m_Color[3] = (*itCP).GetAlpha();

    Contour->GetControlPoints().push_back(pnt); 
    }
    
  if(NDimensions == 2)
    {
    Contour->ControlPointDim("id x y xp yp v1 v2 r g b a");
    }
  else if (NDimensions == 3)
    {
    Contour->ControlPointDim("id x y z xp yp zp v1 v2 v3 r gn be a");
    }

  // fill in the interpolated points information
  typename SpatialObjectType::InterpolatedPointListType::const_iterator itI;
  for(itI = dynamic_cast<SpatialObjectType*>(spatialObject)->GetInterpolatedPoints().begin(); 
      itI != dynamic_cast<SpatialObjectType*>(spatialObject)->GetInterpolatedPoints().end(); 
      itI++)
    {
    ContourInterpolatedPnt* pnt = new ContourInterpolatedPnt(NDimensions);
   
    pnt->m_Id = (*itI).GetID();
    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_X[d]=(*itI).GetPosition()[d];
      }

    pnt->m_Color[0] = (*itI).GetRed();
    pnt->m_Color[1] = (*itI).GetGreen();
    pnt->m_Color[2] = (*itI).GetBlue();
    pnt->m_Color[3] = (*itI).GetAlpha();

    Contour->GetInterpolatedPoints().push_back(pnt); 
    }
    
  if(NDimensions == 2)
    {
    Contour->InterpolatedPointDim("id x y r g b a");
    }
  else if (NDimensions == 3)
    {
    Contour->InterpolatedPointDim("id x y z r g b a");
    }

  // Set the interpolation type
  switch(spatialObject->GetInterpolationType())
    {
    case SpatialObjectType::EXPLICIT_INTERPOLATION :
      Contour->Interpolation(MET_EXPLICIT_INTERPOLATION);
      break;
    case SpatialObjectType::LINEAR_INTERPOLATION :
      Contour->Interpolation(MET_LINEAR_INTERPOLATION);
      break;
    case SpatialObjectType::BEZIER_INTERPOLATION :
      Contour->Interpolation(MET_BEZIER_INTERPOLATION);
      break;
    default:
      Contour->Interpolation(MET_NO_INTERPOLATION);
    }

  float color[4];
  for(unsigned int i=0;i<4;i++)
    {
    color[i]=spatialObject->GetProperty()->GetColor()[i];
    }
  Contour->Color(color);
  Contour->ID( spatialObject->GetId());
  Contour->Closed(spatialObject->GetClosed());
  Contour->AttachedToSlice(spatialObject->GetAttachedToSlice());
  Contour->DisplayOrientation(spatialObject->GetDisplayOrientation());

  if(spatialObject->GetParent())
    {
    Contour->ParentID(spatialObject->GetParent()->GetId());
    }

  for(unsigned int i=0;i<NDimensions;i++)
    {
    Contour->ElementSpacing(i, spatialObject->GetIndexToObjectTransform()
                                            ->GetScaleComponent()[i]);
    }
  return Contour;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>
typename MetaContourConverter<NDimensions>::SpatialObjectPointer
MetaContourConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaContour* Contour = new MetaContour();
  Contour->Read(name);
  spatialObject = MetaContourToContourSpatialObject(Contour);

  return spatialObject;
}


/** Write a meta Contour file */
template <unsigned int NDimensions>
bool
MetaContourConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaContour* Contour = ContourSpatialObjectToMetaContour(spatialObject);
  Contour->Write(name);
  return true;
}

} // end namespace itk 


#endif
