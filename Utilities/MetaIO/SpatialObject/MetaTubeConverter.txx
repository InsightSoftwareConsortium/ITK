/*=========================================================================

  Program:   itkUNC
  Module:    MetaTubeConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
  Author:    Julien Jomier (julien@jomier.com)

  Copyright (c) 2002 CADDLab @ UNC. All rights reserved.
  See itkUNCCopyright.txt for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaTubeConverter__txx
#define __MetaTubeConverter__txx

#include "MetaTubeConverter.h"

/** Constructor */ 
template <unsigned int NDimensions>                                          
MetaTubeConverter<NDimensions>
::MetaTubeConverter()
{
  
}


/** Convert a metaTube into an Tube SpatialObject  */
template <unsigned int NDimensions>       
typename MetaTubeConverter<NDimensions>::SpatialObjectPointer
MetaTubeConverter<NDimensions>
::MetaTubeToTubeSpatialObject(MetaTube * tube)
{ 
  //SpatialObjectPointer spatialObject = SpatialObjectType::New();

  typedef itk::TubeSpatialObject<NDimensions> TubeSpatialObjectType;
  typename TubeSpatialObjectType::Pointer tub = TubeSpatialObjectType::New();
  
  //typedef TubeSpatialObjectType::VectorType VectorType;
  //typedef vnl_vector<double> VectorType;
  //VectorType vect(NDimensions);
  double spacing[NDimensions];

  tub->SetReferenceCount(2);

  unsigned int ndims = tube->NDims();
  for(unsigned int i=0;i<ndims;i++)
  {
    spacing[i]=tube->ElementSpacing()[i];
  }

  tub->SetSpacing(spacing);
  tub->GetProperty()->SetName((char*)tube->Name());
  tub->SetParentId(tube->ParentID());
  tub->SetId(tube->ID());
  tub->GetProperty()->SetRed(tube->Color()[0]);
  tub->GetProperty()->SetGreen(tube->Color()[1]);
  tub->GetProperty()->SetBlue(tube->Color()[2]);
  tub->GetProperty()->SetAlpha(tube->Color()[3]);

  typedef itk::TubeSpatialObjectPoint<NDimensions> TubePointType;
  typedef TubePointType* TubePointPointer;

  typedef MetaTube::PointListType ListType;
  ListType::iterator it2 = tube->GetPoints().begin();
    
  itk::Vector<double,NDimensions> v;
  //vnl_vector<double> v(ndims);
  
  for(unsigned int id=0;id< tube->GetPoints().size();id++)
  {
    TubePointType pnt;
    
    typedef typename TubeSpatialObjectType::PointType PointType;
    PointType point;

    for(unsigned int i=0;i<ndims;i++)
    {
      point[i]=(*it2)->m_X[i];
    }

    pnt.SetPosition(point);
    pnt.SetRadius((*it2)->m_R);
    pnt.SetMedialness((*it2)->m_Medialness);
    pnt.SetRidgeness((*it2)->m_Ridgeness); 
    pnt.SetBranchness((*it2)->m_Branchness);
    pnt.SetMark((*it2)->m_Mark);  
    for(unsigned int i=0;i<ndims;i++)
    {
      v[i]=(*it2)->m_V1[i];
    }

    pnt.SetV1(v);

    for(unsigned int i=0;i<ndims;i++)
    {
      v[i]=(*it2)->m_V2[i];
    }

    pnt.SetV2(v);
    pnt.SetAlpha1((*it2)->m_Alpha1);
    pnt.SetAlpha2((*it2)->m_Alpha2);
   
    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    tub->GetPoints().push_back(pnt);
    it2++;
  }
 
  return tub;
}

/** Convert an Tube SpatialObject into a metaTube */
template <unsigned int NDimensions>       
MetaTube*
MetaTubeConverter<NDimensions>
::TubeSpatialObjectToMetaTube(SpatialObjectType * spatialObject)
{ 
  unsigned int dim = spatialObject->GetDimension();
  MetaTube* tube = new MetaTube(dim);

  // fill in the tube information
   
  typename SpatialObjectType::PointListType::const_iterator i;
  for(i = dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().begin(); i != dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().end(); i++)
  {
    TubePnt* pnt = new TubePnt(dim);
    //TubePnt pnt(NDimensions);

    //float x[NDimensions];
    for(unsigned int d=0;d<dim;d++)
    {
      pnt->m_X[d] = (*i).GetPosition()[d];
    }

    //pnt.SetX(x);
    //std::cout << pnt.m_X[0] << std::endl;
      
    pnt->m_R=(*i).GetRadius();
    pnt->m_Alpha1=(*i).GetAlpha1();
      
    pnt->m_Alpha2=(*i).GetAlpha2();
    pnt->m_Medialness=(*i).GetMedialness();
    pnt->m_Ridgeness=(*i).GetRidgeness();
    pnt->m_Branchness=(*i).GetBranchness();
    pnt->m_Mark=(*i).GetMark();

    for(unsigned int d=0;d<dim;d++)
    {
      pnt->m_V1[d]=(*i).GetV1()[d];
    }

    for(unsigned int d=0;d<dim;d++)
    {
      pnt->m_V2[d]=(*i).GetV2()[d];
    }

    pnt->m_Color[0] = (*i).GetRed();
    pnt->m_Color[1] = (*i).GetGreen();
    pnt->m_Color[2] = (*i).GetBlue();
    pnt->m_Color[3] = (*i).GetAlpha();

    tube->GetPoints().push_back(pnt); 
  }
    
  if(dim == 2)
  {
    tube->PointDim("x y r rn mn bn mk v1x v1y v2x v2y a1 a2 red green blue alpha");
  }
  else
  {
    tube->PointDim("x y z r rn mn bn mk v1x v1y v1z v2x v2y v2z a1 a2 red green blue alpha");
  }

  float color[4];
  for(unsigned int i=0;i<4;i++)
  {
    color[i]=spatialObject->GetProperty()->GetColor()[i];
  }

  tube->Color(color);
  tube->ID( spatialObject->GetId());
  tube->ParentID(spatialObject->GetParentId());
  tube->NPoints(tube->GetPoints().size());

  for(unsigned int i=0;i<dim;i++)
  {
    tube->ElementSpacing(i,spatialObject->GetSpacing()[i]);
  }
  return tube;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>       
typename MetaTubeConverter<NDimensions>::SpatialObjectPointer
MetaTubeConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaTube* Tube = new MetaTube();
  Tube->Read(name);
  spatialObject = MetaTubeToTubeSpatialObject(Tube);
  delete Tube;
  return spatialObject;
}


/** Write a meta Tube file */
template <unsigned int NDimensions>
bool
MetaTubeConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaTube* Tube = TubeSpatialObjectToMetaTube(spatialObject);
  Tube->Write(name);
  delete Tube;
  return true;
}

#endif
