/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ValidationSampleGenerator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __ValidationSampleGenerator_h
#define __ValidationSampleGenerator_h

#include "itkMacro.h"
#include "itkNumericTraits.h"
#include "itk_hash_map.h"

#include "itkPixelTraits.h"
#include "itkImage.h"
#include "itkMetaImageIOFactory.h"
#include "itkImageFileReader.h"
#include "itkScalarToArrayCastImageFilter.h"
#include "itkImageToListAdaptor.h"
#include "SliceFiller.h"

#include "itkStatisticsAlgorithm.h"
#include "itkHistogram.h"
#include "itkSubsample.h"
#include "itkListSampleToHistogramFilter.h"
#include "itkWeightedCentroidKdTreeGenerator.h"

template< class TImage, class TClassMaskImage, class TVectorImage >
class ValidationSampleGenerator
{
public:
  ValidationSampleGenerator() ;
  ~ValidationSampleGenerator() ;

  typedef itk::PixelTraits< typename TVectorImage::PixelType > PixelTraitsType ;

  typedef typename TImage::PixelType ImagePixelType ;
  
  typedef itk::ImageFileReader< TImage > ImageReaderType ;

  typedef itk::ImageFileReader< TClassMaskImage > ClassMaskImageReaderType ;

  typedef typename TImage::PixelType MeasurementType ;
  typedef typename TVectorImage::PixelType MeasurementVectorType ;

  typedef itk::Statistics::ImageToListAdaptor< TVectorImage > ImageSampleType ;

  typedef itk::Statistics::Subsample< ImageSampleType > SubsampleType ;

  typedef itk::Statistics::Histogram< float, PixelTraitsType::Dimension > HistogramType ;

  typedef itk::Statistics::ListSampleToHistogramFilter< SubsampleType, 
                                                        HistogramType > ImporterType ;

  typedef itk::Statistics::WeightedCentroidKdTreeGenerator< SubsampleType > TreeGeneratorType ;

  typedef itk::Statistics::KdTree< SubsampleType > KdTreeType ;

  typedef itk::Vector< double, PixelTraitsType::Dimension > MeanType ;

  typedef itk::Matrix< double, 
                       PixelTraitsType::Dimension, 
                       PixelTraitsType::Dimension > CovarianceType ;


  void SetImageFileNames(const std::vector< std::string >& fileNames) ;

  void SetClassMaskImageFileName(const char* fileName, 
                                 unsigned int defaultClassLabel,
                                 int sliceOffset = 
                                 itk::NumericTraits< int >::min()) ;

  /** VECTOR_IMAGE 
   *  LIST_SAMPLE                -- implies VECTOR_IMAGE
   *  HISTOGRAM                  -- implies VECTOR_IMAGE &
   *                                           LIST_SAMPLE 
   *  WEIGHTED_CENTROID_KD_TREE -- implies VECTOR_IMAGE,
   *                                        LIST_SAMPLE */
  enum SampleTypeEnum { VECTOR_IMAGE,
                        LIST_SAMPLE,
                        HISTOGRAM,
                        WEIGHTED_CENTROID_KD_TREE } ;
                            
  void SetOutputSampleType(SampleTypeEnum outputType)
  { m_OutputSampleType = outputType ; }

  void SetKdTreeBucketSize(unsigned int size)
  { m_KdTreeBucketSize = size ; }

  void SetOutputNormalized(bool flag = true, double normalizationScale = 100)
  { 
    m_OutputNormalized = flag ; 
    m_NormalizationScale = normalizationScale ;
  }

  /** If you choose to use masked sample, the corresponding class labels will be
   * also generated. And you can get the class labels by calling the 
   * GetClassLabels() method. However, if you chose VECTOR_IMAGE as output,
   * this method won't have any effect on output */
  void SetSelectedClasses(const std::vector< unsigned int >& labelsOfSelectedClasses)
  { m_SelectedClassLabels = labelsOfSelectedClasses ; }

  TImage* GetImage(const char* fileName)
  { return m_ImageReaders[this->GetFileIndex(fileName)]->GetOutput() ; }

  TClassMaskImage* GetClassMaskImage()
  { 
    return m_ClassMaskImage ;
  }

  TVectorImage* GetVectorImage()
  { return m_VectorImage.GetPointer() ; }

  SubsampleType* GetListSample()
  { return m_Subsample.GetPointer() ;}

  HistogramType* GetHistogram()
  { return m_Histogram.GetPointer() ; }

  KdTreeType* GetKdTree() 
  { return m_KdTree.GetPointer() ; } 

  typedef itk::hash_map< unsigned long, unsigned int > ClassLabelsType ;
//   std::vector< unsigned int >* GetClassLabels()
//   { return &m_ClassLabels ; }

  ClassLabelsType* GetClassLabels()
   { return &m_ClassLabels ; }

  /** gets image statistics */
  double GetImageMean(const char* fileName)
  { return m_ImageMeans[this->GetFileIndex(fileName)] ; }

  double GetImageStandardDeviation(const char* fileName)
  { return m_ImageStandardDeviations[this->GetFileIndex(fileName)] ; }

  MeasurementType GetImageMin(const char* fileName)
  { return m_ImageMins[this->GetFileIndex(fileName)] ; }

  MeasurementType GetImageMax(const char* fileName)
  { return m_ImageMaxes[this->GetFileIndex(fileName)] ; }

  /** gets class statistics */
  unsigned int GetClassSize(unsigned int classLabel)
  { return m_ClassSizes[this->GetClassIndex(classLabel)] ; }

  MeanType GetClassMean(unsigned int classLabel)
  { return m_ClassMeans[this->GetClassIndex(classLabel)] ; }
  
  CovarianceType GetClassCovariance(unsigned int classLabel)
  { return m_ClassCovariances[this->GetClassIndex(classLabel)] ; }
  
  /** gets measurment bounds */
  MeasurementVectorType GetMeasurementsLowerBound() 
  { return m_MeasurementsLowerBound ; }
  
  MeasurementVectorType GetMeasurementsUpperBound()
  { return m_MeasurementsUpperBound ; }
  
  void GenerateData() ;
  
protected:
  int GetFileIndex(const char* fileName) ;

  int GetClassIndex(unsigned int classLabel) ;

  void LoadImages() ;
  void LoadClassMaskImage() ;
  void GenerateVectorImage() ;
  void CalculateImageStatistics() ;
  void Normalize() ;
  void GenerateListSample() ;
  void GenerateHistogram() ;
  void GenerateWeightedCentroidKdTree() ;

private:
  /** inputs */
  unsigned int m_NumberOfMeasurements ;
  std::vector< std::string > m_ImageFileNames ;
  const char* m_ClassMaskImageFileName ;
  bool m_UseSliceFiller ;
  unsigned int m_DefaultClassLabel ;
  int m_ClassMaskImageOffset ;
  TClassMaskImage* m_ClassMaskImage ;
  std::vector< unsigned int > m_SelectedClassLabels ;
  SampleTypeEnum m_OutputSampleType ;
  unsigned int m_KdTreeBucketSize ;
  bool m_OutputNormalized ;
  double m_NormalizationScale ;
  std::vector< unsigned int > m_UniqueClassLabels ;

  /** outputss */
  typename TVectorImage::Pointer m_VectorImage ;
  ImageSampleType::Pointer m_Sample ;
  SubsampleType::Pointer m_Subsample ;
  HistogramType::Pointer m_Histogram ;
  KdTreeType::Pointer m_KdTree ;
  ClassLabelsType m_ClassLabels ;
  MeasurementVectorType m_ImageMins ;
  MeasurementVectorType m_ImageMaxes ;
  MeanType m_ImageMeans ;
  MeanType m_ImageStandardDeviations ;

  std::vector< unsigned int > m_ClassSizes ;
  std::vector< MeanType > m_ClassMeans ;
  std::vector< CovarianceType > m_ClassCovariances ;

  MeasurementVectorType m_MeasurementsLowerBound ;
  MeasurementVectorType m_MeasurementsUpperBound ;

  /** helper classes */
  typename SliceFiller< TClassMaskImage >::Pointer m_SliceFiller ;
  std::vector< typename ImageReaderType::Pointer > m_ImageReaders ;
  typename ClassMaskImageReaderType::Pointer m_ClassMaskImageReader ;
  ImporterType::Pointer m_Importer ;
  TreeGeneratorType::Pointer m_TreeGenerator ;
} ; // end of class

#ifndef ITK_MANUAL_INSTANTIATION
#include "ValidationSampleGenerator.txx"
#endif

#endif
