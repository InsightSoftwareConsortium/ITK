/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaArrowConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaArrowConverter__txx
#define __MetaArrowConverter__txx

#include "itkMetaArrowConverter.h"

namespace itk  
{

/** Constructor */ 
template <unsigned int NDimensions>                                          
MetaArrowConverter<NDimensions>
::MetaArrowConverter()
{
  
}


/** Convert a metaArrow into an arrow SpatialObject  */
template <unsigned int NDimensions>       
typename MetaArrowConverter<NDimensions>::SpatialObjectPointer
MetaArrowConverter<NDimensions>
::MetaArrowToArrowSpatialObject(MetaArrow * arrow)
{ 
  SpatialObjectPointer spatialObject = SpatialObjectType::New();

  double spacing[NDimensions];
  float lenght=arrow->Lenght();
  
  for(unsigned int i=0;i<NDimensions;i++)
    {
    spacing[i]=arrow->ElementSpacing()[i];
    }

  spatialObject->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  spatialObject->SetLenght(lenght);
  spatialObject->GetProperty()->SetName(arrow->Name());
  spatialObject->SetId(arrow->ID());
  spatialObject->SetParentId(arrow->ParentID());
  spatialObject->GetProperty()->SetRed(arrow->Color()[0]);
  spatialObject->GetProperty()->SetGreen(arrow->Color()[1]);
  spatialObject->GetProperty()->SetBlue(arrow->Color()[2]);
  spatialObject->GetProperty()->SetAlpha(arrow->Color()[3]);

  return spatialObject;
}

/** Convert an arrow SpatialObject into a metaArrow */
template <unsigned int NDimensions>       
MetaArrow*
MetaArrowConverter<NDimensions>
::ArrowSpatialObjectToMetaArrow(SpatialObjectType * spatialObject)
{ 
  MetaArrow* arrow = new MetaArrow(NDimensions);

  float lenght = spatialObject->GetLenght();

  if(spatialObject->GetParent())
    {
    arrow->ParentID(spatialObject->GetParent()->GetId());
    }
  arrow->Lenght(lenght);
  arrow->ID(spatialObject->GetId());

  arrow->Color(spatialObject->GetProperty()->GetRed(),
                 spatialObject->GetProperty()->GetGreen(),
                 spatialObject->GetProperty()->GetBlue(),
                 spatialObject->GetProperty()->GetAlpha());
  
  for(unsigned int i=0;i<NDimensions;i++)
    {
    arrow->ElementSpacing(i,spatialObject->GetIndexToObjectTransform()
                                         ->GetScaleComponent()[i]);
    }

  return arrow;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>       
typename MetaArrowConverter<NDimensions>::SpatialObjectPointer
MetaArrowConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaArrow* arrow = new MetaArrow();
  arrow->Read(name);
  spatialObject = MetaArrowToArrowSpatialObject(arrow);

  return spatialObject;
}


/** Write a meta arrow file */
template <unsigned int NDimensions>
bool
MetaArrowConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaArrow* arrow = ArrowSpatialObjectToMetaArrow(spatialObject);
  arrow->Write(name);
  return true;
}

} // end namespace itk 

#endif
