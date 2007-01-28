/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaGaussianConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaGaussianConverter_txx
#define __itkMetaGaussianConverter_txx

#include "itkMetaGaussianConverter.h"

namespace itk  
{

/** Constructor */ 
template <unsigned int NDimensions>
MetaGaussianConverter<NDimensions>
::MetaGaussianConverter()
{
  
}


/** Convert a metaGaussian into a gaussian SpatialObject  */
template <unsigned int NDimensions>
typename MetaGaussianConverter<NDimensions>::SpatialObjectPointer
MetaGaussianConverter<NDimensions>
::MetaGaussianToGaussianSpatialObject(MetaGaussian * gaussian)
{ 
  SpatialObjectPointer spatialObject = SpatialObjectType::New();
  spatialObject->SetMaximum( gaussian->Maximum() );
  spatialObject->SetRadius( gaussian->Radius() );
  spatialObject->GetProperty()->SetName(gaussian->Name());
  spatialObject->SetId(gaussian->ID());
  spatialObject->SetParentId(gaussian->ParentID());
  return spatialObject;
}

/** Convert a gaussian SpatialObject into a metaGaussian */
template <unsigned int NDimensions>
MetaGaussian*
MetaGaussianConverter<NDimensions>
::GaussianSpatialObjectToMetaGaussian(SpatialObjectType * spatialObject)
{ 
  MetaGaussian* gaussian = new MetaGaussian(NDimensions);

  if(spatialObject->GetParent())
    {
    gaussian->ParentID(spatialObject->GetParent()->GetId());
    }
  gaussian->Maximum( spatialObject->GetMaximum() );
  gaussian->Radius( spatialObject->GetRadius() );
  gaussian->ID(spatialObject->GetId());
  return gaussian;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>
typename MetaGaussianConverter<NDimensions>::SpatialObjectPointer
MetaGaussianConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaGaussian* gaussian = new MetaGaussian();
  gaussian->Read(name);
  spatialObject = MetaGaussianToGaussianSpatialObject(gaussian);

  return spatialObject;
}


/** Write a meta gaussian file */
template <unsigned int NDimensions>
bool
MetaGaussianConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaGaussian* gaussian = GaussianSpatialObjectToMetaGaussian(spatialObject);
  gaussian->Write(name);
  return true;
}

} // end namespace itk 

#endif
