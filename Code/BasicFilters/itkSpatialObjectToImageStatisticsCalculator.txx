 /*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialObjectToImageStatisticsCalculator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSpatialObjectToImageStatisticsCalculator_txx
#define _itkSpatialObjectToImageStatisticsCalculator_txx

#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkVector.h"
#include "itkListSample.h"
#include "itkMeanCalculator.h"
#include "itkCovarianceCalculator.h"

namespace itk
{ 
    
/** Constructor */
template<class TInputImage, class TInputSpatialObject, unsigned int TSampleDimension>
SpatialObjectToImageStatisticsCalculator<TInputImage,TInputSpatialObject,TSampleDimension>
::SpatialObjectToImageStatisticsCalculator()
{
  m_Image = NULL;
  m_SpatialObject = NULL;
  m_Mean.Fill(0);
  m_CovarianceMatrix.SetIdentity();
  m_SampleDirection = TSampleDimension-1;
  m_InternalImageTime = 0;
  m_InternalSpatialObjectTime = 0;
}

/** */
template<class TInputImage,class TInputSpatialObject, unsigned int TSampleDimension>
void
SpatialObjectToImageStatisticsCalculator<TInputImage,TInputSpatialObject,TSampleDimension>
::Update(void)
{
  if(!m_Image || !m_SpatialObject)
  {
    std::cout << "SpatialObjectToImageStatisticsCalculator: "; 
    std::cout << "Please set the image AND the spatialb object first." << std::endl;
    return;
  }

  // Update only if the image or the spatial object has been modified
  if((m_Image->GetMTime() == m_InternalImageTime)
     &&
     (m_SpatialObject->GetMTime() == m_InternalSpatialObjectTime) 
    )
  {
    return; // No need to update
  }

  m_InternalImageTime = m_Image->GetMTime();
  m_InternalSpatialObjectTime = m_SpatialObject->GetMTime();

  // Get the bounding box
  typename SpatialObjectType::BoundingBoxType::Pointer boundingBox;
  boundingBox = m_SpatialObject->GetBoundingBox();
  
  Point<double,itkGetStaticConstMacro(ObjectDimension)> pt;
  for(unsigned int i=0;i<itkGetStaticConstMacro(ObjectDimension);i++)
  {
    pt[i]=boundingBox->GetBounds()[i*2]+(boundingBox->GetBounds()[i*2+1]-boundingBox->GetBounds()[i*2])/2;
  }

  IndexType index;
  pt = m_SpatialObject->GetIndexToWorldTransform()->TransformPoint(pt);
  for(unsigned int i=0;i<itkGetStaticConstMacro(ObjectDimension);i++)
  {
    index[i]=pt[i];
  }

  IteratorType it = IteratorType(m_Image,m_SpatialObject,index);
  it.SetOriginInclusionStrategy();
  it.GoToBegin();

  std::cout << "Adding Samples" << std::endl;

  typedef typename ImageType::PixelType PixelType;
  typedef itk::Statistics::ListSample< VectorType > SampleType;
  SampleType::Pointer sample = SampleType::New();
 
  while(!it.IsAtEnd())
  {
    IndexType ind = it.GetIndex();
    VectorType mv ;
    mv[0] = it.Get();
    for(unsigned int i=1;i<itkGetStaticConstMacro(SampleDimension);i++)
    {
      ind[m_SampleDirection] += 1;
      mv[i]= m_Image->GetPixel(ind);
    }
    sample->PushBack( mv ) ;
    ++it;
  }

  std::cout << "Computing Mean" << std::endl;

  typedef itk::Statistics::MeanCalculator< SampleType >
    MeanAlgorithmType ;
  
  MeanAlgorithmType::Pointer meanAlgorithm = MeanAlgorithmType::New() ;

  meanAlgorithm->SetInputSample( sample ) ;
  meanAlgorithm->Update() ;

  m_Mean = *(meanAlgorithm->GetOutput());

  std::cout << "Computing Covariance" << std::endl;

  typedef itk::Statistics::CovarianceCalculator< SampleType >
    CovarianceAlgorithmType ;
  
  CovarianceAlgorithmType::Pointer covarianceAlgorithm = 
    CovarianceAlgorithmType::New() ;

  covarianceAlgorithm->SetInputSample( sample ) ;
  covarianceAlgorithm->SetMean( meanAlgorithm->GetOutput() ) ;
  covarianceAlgorithm->Update();
  
  m_CovarianceMatrix = *(covarianceAlgorithm->GetOutput());

  std::cout << "Update done in filter." << std::endl;
}

template<class TInputImage,class TInputSpatialObject, unsigned int TSampleDimension>
void
SpatialObjectToImageStatisticsCalculator<TInputImage,TInputSpatialObject,TSampleDimension>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Image: " << m_Image << std::endl;
  os << indent << "SpatialObject: " << m_SpatialObject << std::endl;
  os << indent << "Mean: " << m_Mean << std::endl;
  os << indent << "Covariance Matrix: " << m_CovarianceMatrix << std::endl;
  os << indent << "Internal Image Time: " << m_InternalImageTime << std::endl;
  os << indent << "Internal Spatial Object Time: " << m_InternalSpatialObjectTime << std::endl;
  os << indent << "SampleDirection: " << m_SampleDirection << std::endl;
}

} // end namespace itk

#endif
