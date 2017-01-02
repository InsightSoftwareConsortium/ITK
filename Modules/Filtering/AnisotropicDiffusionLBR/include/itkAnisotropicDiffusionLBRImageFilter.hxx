/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
//
//  Created by Jean-Marie Mirebeau on 28/02/2014.
//
//

#ifndef itkAnisotropicDiffusionLBRImageFilter_hxx
#define itkAnisotropicDiffusionLBRImageFilter_hxx

#include "itkAnisotropicDiffusionLBRImageFilter.h"

namespace itk
{

template< typename TImage, typename TScalar >
AnisotropicDiffusionLBRImageFilter< TImage, TScalar >
::AnisotropicDiffusionLBRImageFilter():
  m_NoiseScale( 0.5 ),
  m_FeatureScale( 2 ),
  m_RatioToMaxStableTimeStep( 0.7 ),
  m_MaxTimeStepsBetweenTensorUpdates( 5 ),
  m_DiffusionTime( 1 ),
  m_Adimensionize( true )
{
}


template< typename TImage, typename TScalar >
void
AnisotropicDiffusionLBRImageFilter< TImage, TScalar >
::GenerateData()
{
  typename ImageType::Pointer inputImage = const_cast<ImageType*>(this->GetInput());
  typename ImageType::Pointer image = inputImage;

  typedef typename ImageType::SpacingType SpacingType;
  const SpacingType referenceSpacing = inputImage->GetSpacing();

  //        const SpacingType unitSpacing(1); // Better below for non-uniform spacing.
  double minSpacing = referenceSpacing[0];
  for( unsigned int i = 1; i < Dimension; ++i )
    {
    minSpacing = std::min(minSpacing,referenceSpacing[i]);
    }
  const SpacingType unitSpacing = referenceSpacing/minSpacing;

  if( m_Adimensionize )
    {
    inputImage->SetSpacing(unitSpacing);
    }

  ScalarType remainingTime = m_DiffusionTime;

  while( remainingTime > 0 )
    {
    ComputeDiffusionTensors(image);
    typename LinearDiffusionFilterType::Pointer linearDiffusionFilter = LinearDiffusionFilterType::New();
    linearDiffusionFilter->SetMaxNumberOfTimeSteps(m_MaxTimeStepsBetweenTensorUpdates);
    linearDiffusionFilter->SetRatioToMaxStableTimeStep(m_RatioToMaxStableTimeStep);

    linearDiffusionFilter->SetInputImage(image);
    linearDiffusionFilter->SetInputTensor(m_TensorImage);
    linearDiffusionFilter->SetMaxDiffusionTime(remainingTime);
    linearDiffusionFilter->Update();
    image = linearDiffusionFilter->GetOutput();
    remainingTime -= linearDiffusionFilter->GetEffectiveDiffusionTime();

    m_LinearFilterEffectiveTimesAndIterations.push_back(std::pair<ScalarType,int>(linearDiffusionFilter->GetEffectiveDiffusionTime(),linearDiffusionFilter->GetEffectiveNumberOfTimeSteps()));

    this->UpdateProgress(1.-remainingTime/m_DiffusionTime);
    }

  if(m_Adimensionize)
    {
    inputImage->SetSpacing(referenceSpacing);
    image->SetSpacing(referenceSpacing);
    }
  this->GraftOutput(image);
}


template< typename TImage, typename TScalar >
struct AnisotropicDiffusionLBRImageFilter< TImage, TScalar >
::DiffusionTensorFunctor
{
  Self * eigenValuesFunctor;
  struct OrderingType;
  TensorType operator()(const TensorType & S)
    {
      EigenValuesArrayType eigenValues;
      typename TensorType::EigenVectorsMatrixType eigenVectors;
      S.ComputeEigenAnalysis(eigenValues,eigenVectors);

      // For convenience, eigenvalues are sorted by increasing order
      Vector<int,Dimension> order;
      for(int i=0; i<(int)Dimension; ++i) order[i]=i;

      OrderingType ordering(eigenValues);

      std::sort(order.Begin(), order.End(),ordering);

      std::sort(eigenValues.Begin(), eigenValues.End());
      EigenValuesArrayType ev = this->eigenValuesFunctor->EigenValuesTransform(eigenValues);

      TensorType DiffusionTensor;
      for(int i=0; i<(int)Dimension; ++i){
          DiffusionTensor(order[i],order[i]) = ev[i];
          for(int j=0; j<i; ++j) DiffusionTensor(i,j) = 0.;
      }
      return DiffusionTensor.Rotate(eigenVectors.GetTranspose());
  }
};


// c++ 11 would be : [& eigenValues](int i, int j)->bool {return eigenValues[i]<eigenValues[j];}
template< typename TImage, typename TScalar >
struct AnisotropicDiffusionLBRImageFilter< TImage, TScalar >
::DiffusionTensorFunctor
::OrderingType
{
  bool operator()(int i, int j) const {return this->e[i]<this->e[j];}
  const EigenValuesArrayType & e;
  OrderingType(const EigenValuesArrayType & e_):e(e_){};
};

template< typename TImage, typename TScalar >
void
AnisotropicDiffusionLBRImageFilter< TImage, TScalar >
::ComputeDiffusionTensors( ImageType * image )
{
  typename StructureTensorFilterType::Pointer structureTensorFilter = StructureTensorFilterType::New();

  structureTensorFilter->SetNoiseScale(m_NoiseScale);
  structureTensorFilter->SetFeatureScale(m_FeatureScale);
  structureTensorFilter->SetRescaleForUnitMaximumTrace(m_Adimensionize);
  structureTensorFilter->SetInput(image);

  typedef UnaryFunctorImageFilter<TensorImageType, TensorImageType, DiffusionTensorFunctor> ImageFunctorType;
  typename ImageFunctorType::Pointer imageFunctor = ImageFunctorType::New();
  imageFunctor->GetFunctor().eigenValuesFunctor = this;
  imageFunctor->SetInput(structureTensorFilter->GetOutput());

  imageFunctor->Update();
  m_TensorImage = imageFunctor->GetOutput();
}

} // end namespace itk

#endif
