/*************************************************************************

Author: Christine Xu

=========================================================================*/

#include "itkMetaSPHARMCoefConverter.h"


namespace itk  
{

/** Constructor */ 
MetaSPHARMCoefConverter
::MetaSPHARMCoefConverter()
{
  
}


/** Convert a metaCoef into an SPHARMCoefSpatialObject  */
MetaSPHARMCoefConverter::SpatialObjectPointer
MetaSPHARMCoefConverter
::MetaCoefToSPHARMCoefSpatialObject(MetaCoef * Coef)
{ 

  typedef itk::SPHARMCoefSpatialObject SPHARMCoefSpatialObjectType;
  SPHARMCoefSpatialObjectType::Pointer coef = SPHARMCoefSpatialObjectType::New();

  double spacing[3];
  
  unsigned int ndims = Coef->NDims();
  for(unsigned int i=0;i<ndims;i++)
  {
    spacing[i]=Coef->ElementSpacing()[i];
  }
  coef->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  coef->GetProperty()->SetName(Coef->Name());
  coef->SetId(Coef->ID());
  coef->SetParentId(Coef->ParentID());

  typedef SPHARMCoefSpatialObjectType::CoefListType CoefListType;
  typedef SPHARMCoefSpatialObjectType::CoefType CoefType;
  typedef SPHARMCoefSpatialObjectType::ScalarType ScalarType;
  CoefListType coefs;
  
  typedef MetaCoef::PointListType ListType;
  ListType::iterator it2 = Coef->GetPoints().begin();
    
  
  for(unsigned int id=0;id< Coef->GetPoints().size();id++)
  {

    CoefType point;
    ScalarType pnt[3];

    for(unsigned int i=0;i<ndims;i++)
    {
      pnt[i]=(*it2)->m_X[i];
    }
    
    point = pnt;
    coefs.push_back(point);
   
    it2++;
  }
  
  coef->SetCoefs(coefs);
 
  return coef;
}

/** Convert an SPHARMCoefSpatialObject into a metaCoef */
      
MetaCoef*
MetaSPHARMCoefConverter
::SPHARMCoefSpatialObjectToMetaCoef(SpatialObjectType * spatialObject)
{ 
  MetaCoef* Coef = new MetaCoef(3);

  // fill in the Coef information
   
  SpatialObjectType::CoefListType::const_iterator i;
  for(i = const_cast<SpatialObjectType*>(spatialObject)->GetCoefs().begin(); i != const_cast<SpatialObjectType*>(spatialObject)->GetCoefs().end(); i++)
  {
    CoefPoint* pnt = new CoefPoint(3);
   
    for(unsigned int d=0;d<3;d++)
    {
      pnt->m_X[d]=(*i)[d];
    }

    Coef->GetPoints().push_back(pnt); 
  }
    

  Coef->PointDim("x y z ...");

  Coef->ID( spatialObject->GetId());
  if(spatialObject->GetParent())
  {
    Coef->ParentID(spatialObject->GetParent()->GetId());
  }
  Coef->NPoints(Coef->GetPoints().size());

 for(unsigned int i=0;i<3;i++)
  {
    Coef->ElementSpacing(i, spatialObject->GetIndexToObjectTransform()
                                            ->GetScaleComponent()[i]);
  }

  return Coef;
}


/** Read a meta file give the type */
MetaSPHARMCoefConverter::SpatialObjectPointer
MetaSPHARMCoefConverter
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaCoef* Coef = new MetaCoef();
  Coef->Read(name);
  spatialObject = MetaCoefToSPHARMCoefSpatialObject(Coef);

  return spatialObject;
}


/** Write a meta Coef file */
bool
MetaSPHARMCoefConverter
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaCoef* Coef = SPHARMCoefSpatialObjectToMetaCoef(spatialObject);
  Coef->Write(name);
  return true;
}

} // end namespace itk 
