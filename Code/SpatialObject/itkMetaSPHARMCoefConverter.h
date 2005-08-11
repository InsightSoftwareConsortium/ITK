/*************************************************************************
Author: Christine Xu
=========================================================================*/
#ifndef __MetaSPHARMCoefConverter__h
#define __MetaSPHARMCoefConverter__h


#include "metaCoef.h"
#include "itkSPHARMCoefSpatialObject.h"
#include "itkSpatialObject.h"

namespace itk 
{

class MetaSPHARMCoefConverter
{

public:

  MetaSPHARMCoefConverter();
  ~MetaSPHARMCoefConverter() {};

  typedef itk::SPHARMCoefSpatialObject SpatialObjectType;


  typedef SpatialObjectType::TransformType TransformType;

  typedef SpatialObjectType::Pointer SpatialObjectPointer;

  SpatialObjectPointer ReadMeta(const char* name);

  bool WriteMeta(SpatialObjectType* spatialObject,const char* name);

  SpatialObjectPointer MetaCoefToSPHARMCoefSpatialObject(MetaCoef * coef);
  MetaCoef* SPHARMCoefSpatialObjectToMetaCoef(SpatialObjectType * spatialObject);

};

} // end namespace itk


#endif
