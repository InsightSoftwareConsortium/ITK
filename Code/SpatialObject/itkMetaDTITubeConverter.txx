/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaDTITubeConverter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __MetaDTITubeConverter__txx
#define __MetaDTITubeConverter__txx

#include "itkMetaDTITubeConverter.h"

namespace itk  
{


/** Constructor */ 
template <unsigned int NDimensions>                                          
MetaDTITubeConverter<NDimensions>
::MetaDTITubeConverter()
{
  
}


/** Convert a MetaDTITube into an Tube SpatialObject  */
template <unsigned int NDimensions>       
typename MetaDTITubeConverter<NDimensions>::SpatialObjectPointer
MetaDTITubeConverter<NDimensions>
::MetaDTITubeToDTITubeSpatialObject(MetaDTITube * tube)
{ 
  typedef itk::DTITubeSpatialObject<NDimensions> DTITubeSpatialObjectType;
  typename DTITubeSpatialObjectType::Pointer tub = DTITubeSpatialObjectType::New();
  double spacing[NDimensions];

  unsigned int ndims = tube->NDims();
  for(unsigned int i=0;i<ndims;i++)
  {
    spacing[i]=tube->ElementSpacing()[i];
  }

  tub->GetIndexToObjectTransform()->SetScaleComponent(spacing);
  tub->GetProperty()->SetName(tube->Name());
  tub->SetParentPoint(tube->ParentPoint());
  tub->SetId(tube->ID());
  tub->SetParentId(tube->ParentID());
  tub->GetProperty()->SetRed(tube->Color()[0]);
  tub->GetProperty()->SetGreen(tube->Color()[1]);
  tub->GetProperty()->SetBlue(tube->Color()[2]);
  tub->GetProperty()->SetAlpha(tube->Color()[3]);

  typedef itk::DTITubeSpatialObjectPoint<NDimensions> TubePointType;
  typedef TubePointType* TubePointPointer;

  typedef MetaDTITube::PointListType ListType;
  ListType::iterator it2 = tube->GetPoints().begin();
    
  itk::CovariantVector<double,NDimensions> v; 
  itk::Vector<double,NDimensions> t;
  
  for(unsigned int id=0;id< tube->GetPoints().size();id++)
  {
    TubePointType pnt;
    
    typedef typename DTITubeSpatialObjectType::PointType PointType;
    PointType point;

    for(unsigned int i=0;i<ndims;i++)
    {
      point[i]=(*it2)->m_X[i];
    }

    pnt.SetPosition(point);
    pnt.SetRadius((*it2)->m_R);
    pnt.SetFA((*it2)->m_FA);
    pnt.SetADC((*it2)->m_ADC);
    pnt.SetGA((*it2)->m_GA);
    pnt.SetInterpolation((*it2)->m_Interpolation);
    pnt.SetLambda1((*it2)->m_Lambda1);
    pnt.SetLambda2((*it2)->m_Lambda2);
    pnt.SetLambda3((*it2)->m_Lambda3);

    float* ev = new float[3];
    for(unsigned int i=0;i<3;i++)
      {
      ev[i]=(*it2)->m_MinEV[i];
      }
    pnt.SetMinEigenVector(ev);

    for(unsigned int i=0;i<3;i++)
      {
      ev[i]=(*it2)->m_MedEV[i];
      }
    pnt.SetMedEigenVector(ev);

    for(unsigned int i=0;i<3;i++)
      {
      ev[i]=(*it2)->m_MaxEV[i];
      }
    pnt.SetMaxEigenVector(ev);

    delete ev;
    
    float* mri = new float[5];
    for(unsigned int i=0;i<5;i++)
      {
      mri[i]=(*it2)->m_MRI[i];
      }
    pnt.SetMRI(mri);

    delete mri;

    float* tensor = new float[6];

    for(unsigned int i=0;i<6;i++)
      {
      tensor[i]=(*it2)->m_TensorMatrix[i];
      }
    pnt.SetTensorMatrix(tensor);

    delete tensor;

    for(unsigned int i=0;i<ndims;i++)
      {
      v[i]=(*it2)->m_V1[i];
      }
    pnt.SetNormal1(v);

    for(unsigned int i=0;i<ndims;i++)
      {
      v[i]=(*it2)->m_V2[i];
      }
    pnt.SetNormal2(v);

    for(unsigned int i=0;i<ndims;i++)
      {
      t[i]=(*it2)->m_T[i];
      }
    pnt.SetTangent(t);
    
    pnt.SetRed((*it2)->m_Color[0]);
    pnt.SetGreen((*it2)->m_Color[1]);
    pnt.SetBlue((*it2)->m_Color[2]);
    pnt.SetAlpha((*it2)->m_Color[3]);

    pnt.SetID((*it2)->m_ID);

    tub->GetPoints().push_back(pnt);

    it2++;
  }
  return tub;
}

/** Convert an Tube SpatialObject into a MetaDTITube */
template <unsigned int NDimensions>       
MetaDTITube*
MetaDTITubeConverter<NDimensions>
::DTITubeSpatialObjectToMetaDTITube(SpatialObjectType * spatialObject)
{ 
  MetaDTITube* tube = new MetaDTITube(NDimensions);

  // fill in the tube information
   
  typename SpatialObjectType::PointListType::const_iterator i;
  for(i = dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().begin(); 
      i != dynamic_cast<SpatialObjectType*>(spatialObject)->GetPoints().end();
      i++)
  {
    DTITubePnt* pnt = new DTITubePnt(NDimensions);

    for(unsigned int d=0;d<NDimensions;d++)
    {
      pnt->m_X[d] = (*i).GetPosition()[d];
    }
      
    pnt->m_ID = (*i).GetID();
    pnt->m_R=(*i).GetRadius();
 
    pnt->m_FA = (*i).GetFA();
    pnt->m_ADC = (*i).GetADC();
    pnt->m_GA = (*i).GetGA();
    pnt->m_Interpolation = (*i).GetInterpolation();

    pnt->m_Lambda1 = (*i).GetLambda1();
    pnt->m_Lambda2 = (*i).GetLambda2();
    pnt->m_Lambda3 = (*i).GetLambda3();

    for(unsigned int d=0;d<3;d++)
      {
      pnt->m_MinEV[d]=(*i).GetMinEigenVector()[d];
      pnt->m_MedEV[d]=(*i).GetMedEigenVector()[d];
      pnt->m_MaxEV[d]=(*i).GetMaxEigenVector()[d];  
      }
    for(unsigned int d=0;d<5;d++)
      {
      pnt->m_MRI[d]=(*i).GetMRI()[d];
      }

    for(unsigned int d=0;d<6;d++)
      {
      pnt->m_TensorMatrix[d]=(*i).GetTensorMatrix()[d];
      }

    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_V1[d]=(*i).GetNormal1()[d];
      }

    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_V2[d]=(*i).GetNormal2()[d];
      }

    for(unsigned int d=0;d<NDimensions;d++)
      {
      pnt->m_T[d]=(*i).GetTangent()[d];
      }
              
    pnt->m_Color[0] = (*i).GetRed();
    pnt->m_Color[1] = (*i).GetGreen();
    pnt->m_Color[2] = (*i).GetBlue();
    pnt->m_Color[3] = (*i).GetAlpha();

    tube->GetPoints().push_back(pnt); 
  }
   
  tube->PointDim("x y z fa adc ga i l1 l2 l3 xevmin yevmin zevmin xevmed yevmed zevmed xevmax yevmax zevmax mri1 mri2 mri3 mri4 mri5 tensor1 tensor2 tensor3 tensor4 tensor5 tensor6 v1x v1y v1z v2x v2y v2z tx ty tz r red green blue alpha id");


  float color[4];
  for(unsigned int i=0;i<4;i++)
    {
    color[i]=spatialObject->GetProperty()->GetColor()[i];
    }

  tube->Color(color);
  tube->ID( spatialObject->GetId());

  if(spatialObject->GetParent())
    {
    tube->ParentID(spatialObject->GetParent()->GetId());
    }
  tube->ParentPoint(spatialObject->GetParentPoint());
  tube->NPoints(tube->GetPoints().size());

  for(unsigned int i=0;i<NDimensions;i++)
    {
    tube->ElementSpacing(i, spatialObject->GetIndexToObjectTransform()
                                         ->GetScaleComponent()[i]);
    }
  return tube;
}


/** Read a meta file give the type */
template <unsigned int NDimensions>       
typename MetaDTITubeConverter<NDimensions>::SpatialObjectPointer
MetaDTITubeConverter<NDimensions>
::ReadMeta(const char* name)
{
  SpatialObjectPointer spatialObject;
  MetaDTITube* Tube = new MetaDTITube();
  Tube->Read(name);
  spatialObject = MetaDTITubeToDTITubeSpatialObject(Tube);
  delete Tube;
  return spatialObject;
}


/** Write a meta Tube file */
template <unsigned int NDimensions>
bool
MetaDTITubeConverter<NDimensions>
::WriteMeta(SpatialObjectType* spatialObject,const char* name)
{
  MetaDTITube* Tube = DTITubeSpatialObjectToMetaDTITube(spatialObject);
  Tube->Write(name);
  delete Tube;
  return true;
}

} // end namespace itk 

#endif
