/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageKmeansImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkScalarImageKmeansImageFilter_txx
#define _itkScalarImageKmeansImageFilter_txx
#include "itkScalarImageKmeansImageFilter.h"

#include "itkProgressReporter.h"

namespace itk
{

template <class TInputImage>
ScalarImageKmeansImageFilter<TInputImage>
::ScalarImageKmeansImageFilter()
{
}

template< class TInputImage >
void
ScalarImageKmeansImageFilter< TInputImage >
::GenerateData()
{
  typename AdaptorType::Pointer adaptor = AdaptorType::New();
  adaptor->SetImage( this->GetInput() );

  typename TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New();

  treeGenerator->SetSample( adaptor );
  treeGenerator->SetBucketSize( 16 );
  treeGenerator->Update();

  typename EstimatorType::Pointer estimator = EstimatorType::New();

  const unsigned int numberOfClasses = this->m_InitialMeans.size();

  ParametersType  initialMeans( numberOfClasses );
  for(unsigned int cl=0; cl<numberOfClasses; cl++)
    {
    initialMeans[0] = this->m_InitialMeans[cl];
    }

  estimator->SetParameters( initialMeans );
  
  estimator->SetKdTree( treeGenerator->GetOutput() );
  estimator->SetMaximumIteration( 200 );
  estimator->SetCentroidPositionChangesThreshold(0.0);
  estimator->StartOptimization();

  this->m_FinalMeans = estimator->GetParameters();

  typedef typename InputImageType::RegionType RegionType;
  typedef typename InputImageType::SizeType   SizeType;

  // Add here the labeling of the pixels...
  //
  //
}

/**
 * Add a new class for the classifier. This requires to explicitly set the
 * initial mean value for that class.
 */
template <class TInputImage >
void
ScalarImageKmeansImageFilter<TInputImage >
::AddClassWithInitialMean( RealPixelType mean )
{
  this->m_InitialMeans.push_back( mean );
}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage >
void
ScalarImageKmeansImageFilter<TInputImage >
::PrintSelf(
  std::ostream& os, 
  Indent indent) const
{
  Superclass::PrintSelf( os, indent );
}

} // end namespace itk

#endif
