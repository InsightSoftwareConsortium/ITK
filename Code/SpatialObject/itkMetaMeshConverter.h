/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaMeshConverter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMetaMeshConverter_h
#define __itkMetaMeshConverter_h


#include "metaMesh.h"
#include "itkMeshSpatialObject.h"
#include "itkSpatialObject.h"

namespace itk 
{

template <unsigned int NDimensions, 
          typename PixelType = unsigned char,
          typename TMeshTraits = 
            DefaultStaticMeshTraits< PixelType , NDimensions, NDimensions >
         >
class MetaMeshConverter
{

public:

  MetaMeshConverter();
  ~MetaMeshConverter() {};

  typedef itk::Mesh<PixelType,NDimensions,TMeshTraits> MeshType;
  typedef itk::MeshSpatialObject<MeshType>             SpatialObjectType;

  typedef typename SpatialObjectType::TransformType TransformType;

  typedef typename SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaMeshToMeshSpatialObject(MetaMesh * Mesh);
  MetaMesh* MeshSpatialObjectToMetaMesh(SpatialObjectType * spatialObject);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
  #include "itkMetaMeshConverter.txx"
#endif


#endif
