/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformFileReader.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTransformFileReader_cxx
#define __itkTransformFileReader_cxx

#include "itkTransformFileReader.h"
#include "metaScene.h"
#include "itkAffineTransform.h"
#include "itkSimilarity2DTransform.h"

#include "itkCenteredEuler3DTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkRigid2DTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DTransform.h"
#include "itkSimilarity2DTransform.h"
#include "itkVersorTransform.h"
#include "itkVersorRigid3DTransform.h"
#include "itkScaleSkewVersor3DTransform.h"

#include "itkCenteredAffineTransform.h"
#include "itkScalableAffineTransform.h"
#include "itkFixedCenterOfRotationAffineTransform.h"
#include "itkQuaternionRigidTransform.h"
#include "itkScaleLogarithmicTransform.h"
#include "itkScaleTransform.h"
#include "itkTranslationTransform.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkIdentityTransform.h"
#include "itkKernelTransform.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkVolumeSplineKernelTransform.h"

#include "itkBSplineDeformableTransform.h"

namespace itk
{


#define ITK_CONVERTMETATOITKTRANSFORM(name,scalartype)\
  if( (!strcmp(metaTransform->ObjectSubTypeName(),(char *)(#name))) )\
  { \
    typedef itk::name<scalartype> InternalTransformType;\
    transform = InternalTransformType::New();\
    hasTransform = true;\
  } \

#define ITK_CONVERTMETATOITKTRANSFORM_2(name,scalartype,dimension)\
  if( (!strcmp(metaTransform->ObjectSubTypeName(),(char *)(#name))) \
    && (metaTransform->NDims() == (int)(dimension)) \
    ) \
  { \
    typedef itk::name<scalartype,dimension> InternalTransformType;\
    transform = InternalTransformType::New();\
    hasTransform = true;\
  } \

#define ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(name,scalartype,dimension)\
  if((!strcmp(metaTransform->ObjectSubTypeName(),(char *)(#name)))\
    && (metaTransform->NDims() == (int)(dimension))\
    )\
  {\
  typedef itk::name<scalartype,dimension> InternalTransformType;\
  transform = InternalTransformType::New();\
  itk::Point<scalartype,dimension> cor;\
  for(unsigned int i=0;i<(int)(dimension);i++){cor[i] = metaTransform->CenterOfRotation()[i];}\
  static_cast<InternalTransformType*>(transform.GetPointer())->SetCenter(cor);\
  hasTransform = true;\
  } 

#define ITK_CONVERTMETATO_ITK_BSPLINEDEFORMABLETRANSFORM(name,scalartype,dimension,order)\
  if( (!strcmp(metaTransform->ObjectSubTypeName(),(char *)(#name))) \
    && (metaTransform->NDims() == (int)(dimension)) \
    && (metaTransform->TransformOrder() == (int)(order))\
    ) \
  { \
    typedef itk::name<scalartype,dimension,order> InternalTransformType;\
    transform = InternalTransformType::New();\
    InternalTransformType::RegionType region;\
    InternalTransformType::SizeType size; \
    for(unsigned int j=0;j<(int)(dimension);j++) {size[j] = (long unsigned int)metaTransform->GridRegionSize()[j];}\
    region.SetSize(size);\
    static_cast<InternalTransformType*>(transform.GetPointer())->SetGridRegion(region);\
    hasTransform = true;\
  } \


/** Constructor */
TransformFileReader
::TransformFileReader()
{
  m_FileName = "";
}

/** Destructor */
TransformFileReader
::~TransformFileReader()
{
  // we clean the list of parameters
  ParametersListType::const_iterator it = m_ParametersList.begin();
  while(it!= m_ParametersList.end())
    {
    delete *it;
    it++;
    }
  m_ParametersList.clear();
}

/** This function does the real conversion */
bool TransformFileReader
::ConvertMetaToITKTransform(MetaTransform * metaTransform)
{
  bool hasTransform = false;
  TransformPointer transform;
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(AffineTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(CenteredAffineTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(ScalableAffineTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(FixedCenterOfRotationAffineTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(ScaleLogarithmicTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(ScaleTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(TranslationTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(ElasticBodyReciprocalSplineKernelTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(ElasticBodySplineKernelTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(IdentityTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(KernelTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(ThinPlateR2LogRSplineKernelTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(ThinPlateSplineKernelTransform,double,3);
  ITK_CONVERTMETATOITKTRANSFORM_2(VolumeSplineKernelTransform,double,3);

  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(AffineTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(ScalableAffineTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(FixedCenterOfRotationAffineTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(CenteredAffineTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(ScaleLogarithmicTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2_WITH_CENTER(ScaleTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(TranslationTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(ElasticBodyReciprocalSplineKernelTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(ElasticBodySplineKernelTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(IdentityTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(KernelTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(ThinPlateR2LogRSplineKernelTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(ThinPlateSplineKernelTransform,double,2);
  ITK_CONVERTMETATOITKTRANSFORM_2(VolumeSplineKernelTransform,double,2);

  ITK_CONVERTMETATOITKTRANSFORM(QuaternionRigidTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(Similarity2DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(CenteredEuler3DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(CenteredRigid2DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(Euler2DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(Euler3DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(Rigid2DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(Rigid3DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(Rigid3DPerspectiveTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(VersorTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(VersorRigid3DTransform,double);
  ITK_CONVERTMETATOITKTRANSFORM(ScaleSkewVersor3DTransform,double);

  ITK_CONVERTMETATO_ITK_BSPLINEDEFORMABLETRANSFORM(BSplineDeformableTransform,double,3,3);

  if(hasTransform)
    {
    unsigned nParameters = metaTransform->NParameters();
    typedef  Array< double >  ParametersType;
    ParametersType* parameters = new ParametersType(nParameters);
    for(unsigned int i=0;i<nParameters;i++)
      {
      (*parameters)[i] = metaTransform->Parameters()[i];
      }

    // We need to keep the parameters in a list so they are not deleted;
    m_ParametersList.push_back(parameters);
    transform->SetParameters(*parameters);
    m_TransformList.push_back(transform.GetPointer());
    return true;
    }
  else
    {
    std::cout << "Not transform" << std::endl;
    }
  return false;
}

/** Update the Reader */
void TransformFileReader
::Update()
{  
  m_ParametersList.clear();
  m_TransformList.clear();
  MetaScene scene;
  scene.Read(m_FileName.c_str());
  MetaScene::ObjectListType * objects = scene.GetObjectList();
  MetaScene::ObjectListType::const_iterator it = objects->begin();

  while(it != objects->end())
    {
    if(!strcmp((*it)->ObjectTypeName(),"Transform"))
      {
      this->ConvertMetaToITKTransform(static_cast<MetaTransform*>(*it));
      }
    it++;
    }
}


} // namespace itk

#endif
