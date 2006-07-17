/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSpatialObjectToImageStatisticsCalculator.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSpatialObjectToImageStatisticsCalculator_txx
#define _itkSpatialObjectToImageStatisticsCalculator_txx

#include "itkSpatialObjectToImageStatisticsCalculator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkMeanCalculator.h"
#include "itkCovarianceCalculator.h"
#include "itkImageMaskSpatialObject.h"

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
  m_Sum = 0;
  m_NumberOfPixels = 0;
  m_Sample = SampleType::New();
}


/** Compute Statistics from the Sample vector */
template<class TInputImage,class TInputSpatialObject, unsigned int TSampleDimension>
bool 
SpatialObjectToImageStatisticsCalculator<TInputImage,TInputSpatialObject,TSampleDimension>
::ComputeStatistics()
{
  typedef itk::Statistics::MeanCalculator< SampleType >
    MeanAlgorithmType ;
  
  typename MeanAlgorithmType::Pointer meanAlgorithm = MeanAlgorithmType::New() ;
  meanAlgorithm->SetInputSample( m_Sample ) ;
  meanAlgorithm->Update() ;

  typename MeanAlgorithmType::OutputType mean = 
                                *(meanAlgorithm->GetOutput());
  for( unsigned int i=0; i< SampleDimension; i++ )
    {
    m_Mean[i] = mean[i];
    }

  typedef itk::Statistics::CovarianceCalculator< SampleType >
    CovarianceAlgorithmType ;
  
  typename CovarianceAlgorithmType::Pointer covarianceAlgorithm = 
    CovarianceAlgorithmType::New() ;

  covarianceAlgorithm->SetInputSample( m_Sample ) ;
  covarianceAlgorithm->SetMean( meanAlgorithm->GetOutput() ) ;
  covarianceAlgorithm->Update();
  
  typename CovarianceAlgorithmType::OutputType covarianceMatrix
      = *(covarianceAlgorithm->GetOutput());
  for( unsigned int i=0; i< covarianceMatrix.Rows(); i++ )
    {
    for( unsigned int j=0; j< covarianceMatrix.Rows(); j++ )
      {
      m_CovarianceMatrix(i,j) = covarianceMatrix(i,j);
      }
    }

  return true;
}


/** */
template<class TInputImage,class TInputSpatialObject, unsigned int TSampleDimension>
void
SpatialObjectToImageStatisticsCalculator<TInputImage,TInputSpatialObject,TSampleDimension>
::Update(void)
{
  if(!m_Image || !m_SpatialObject)
    {
    itkExceptionMacro("SpatialObjectToImageStatisticsCalculator: Please set the image AND the spatial object first.");
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
  
  typedef typename ImageType::PixelType PixelType;
  m_Sample = SampleType::New();
  m_Sample->SetMeasurementVectorSize( SampleDimension );
   
  m_NumberOfPixels = 0;

  // If this is an ImageMaskSpatialObject we cannot use the flood filled iterator
  if(!strcmp(m_SpatialObject->GetTypeName(),"ImageMaskSpatialObject"))
    {
    typedef Image<unsigned char,itkGetStaticConstMacro(ObjectDimension)> MaskImageType;
    typedef ImageMaskSpatialObject<itkGetStaticConstMacro(ObjectDimension)> MaskSOType;
    typedef ImageRegionConstIterator<MaskImageType> MaskIteratorType;
    typename MaskImageType::ConstPointer maskImage =  
                        static_cast<MaskSOType*>(m_SpatialObject.GetPointer())->GetImage();
    MaskIteratorType it(maskImage,maskImage->GetLargestPossibleRegion());
    it.GoToBegin();
    while(!it.IsAtEnd())
      {
      if(it.Get()>0) // if inside the mask
        {
        IndexType ind = it.GetIndex();
        VectorType mv;
        mv[0] = m_Image->GetPixel(ind);
        for(unsigned int i=1;i<itkGetStaticConstMacro(SampleDimension);i++)
          {
          ind[m_SampleDirection] += 1;
          mv[i]= m_Image->GetPixel(ind);
          m_Sum += static_cast< AccumulateType >(mv[i]);
          }
        m_Sample->PushBack( mv );
        m_NumberOfPixels++;
        }
      ++it;
      }
    }
  else
    {
    // Get the bounding box
    typename SpatialObjectType::BoundingBoxType::Pointer boundingBox;
    m_SpatialObject->ComputeBoundingBox();
    boundingBox = m_SpatialObject->GetBoundingBox();
    
    Point<double,itkGetStaticConstMacro(ObjectDimension)> pt;
    for(unsigned int i=0;i<itkGetStaticConstMacro(ObjectDimension);i++)
      {
      pt[i]=boundingBox->GetBounds()[i*2]+(boundingBox->GetBounds()[i*2+1]-boundingBox->GetBounds()[i*2])/2;
      }

    IndexType index;
    
    // We should remove the spacing and the origin of the image since the FloodFill iterator is
    // considering them.
    for(unsigned int i=0;i<itkGetStaticConstMacro(ObjectDimension);i++)
      {
      index[i]=(long int)((pt[i]-m_Image->GetOrigin()[i])/m_Image->GetSpacing()[i]);
      }

    IteratorType it = IteratorType(m_Image,m_SpatialObject,index);
    it.SetOriginInclusionStrategy();
    it.GoToBegin();

    while(!it.IsAtEnd())
      {
      IndexType ind = it.GetIndex();
      VectorType mv ;
      mv[0] = it.Get();
      for(unsigned int i=1;i<itkGetStaticConstMacro(SampleDimension);i++)
        {
        ind[m_SampleDirection] += 1;
        mv[i]= m_Image->GetPixel(ind);
        m_Sum += static_cast< AccumulateType >(mv[i]);
        }
      m_Sample->PushBack( mv );
      m_NumberOfPixels++;
      ++it;
      }
    }

  this->ComputeStatistics();
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
  os << indent << "Sum: " << m_Sum << std::endl;
  os << indent << "m_NumberOfPixels: " << m_NumberOfPixels << std::endl;
  os << indent << "Internal Image Time: " << m_InternalImageTime << std::endl;
  os << indent << "Internal Spatial Object Time: " << m_InternalSpatialObjectTime << std::endl;
  os << indent << "SampleDirection: " << m_SampleDirection << std::endl;
  os << indent << "Sample: " << m_Sample << std::endl;
}

} // end namespace itk

#endif
