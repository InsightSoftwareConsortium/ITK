/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    ValidationSampleGenerator.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __ValidationSampleGenerator_txx
#define __ValidationSampleGenerator_txx

#include "ValidationSampleGenerator.h"
#include <time.h>

template< class TImage, class TClassMaskImage, class TVectorImage >
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::ValidationSampleGenerator()
{
  m_NumberOfMeasurements = PixelTraitsType::Dimension ;
  m_VectorImage = TVectorImage::New() ;
  m_Sample = ImageSampleType::New() ;
  m_Subsample = SubsampleType::New() ;
  m_Subsample->SetSample(m_Sample.GetPointer()) ;
  m_ClassMaskImageOffset = 0 ;
  m_UseSliceFiller = false ;
  m_Histogram = HistogramType::New() ;
  m_ClassMaskImageReader = ClassMaskImageReaderType::New() ;
  m_SliceFiller = SliceFiller< TClassMaskImage >::New() ;
  m_Importer = ImporterType::New() ;
  m_TreeGenerator = TreeGeneratorType::New() ;
  m_KdTreeBucketSize = 100 ;
}

template< class TImage, class TClassMaskImage, class TVectorImage >
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::~ValidationSampleGenerator()
{
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void 
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::SetImageFileNames(const std::vector< std::string >& fileNames)
{
  if ( fileNames.size() != m_NumberOfMeasurements )
    {
      std::cout 
        << "ERROR: the number of file names doesn't match "
        << "the VectorImage's vector length"<< std::endl ;
      return ;
    }

  m_ImageFileNames = fileNames ;
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::SetClassMaskImageFileName(const char* fileName, 
                            unsigned int defaultClassLabel,
                            int sliceOffset)
{
  m_ClassMaskImageFileName = fileName ;
  if ( sliceOffset != itk::NumericTraits< int >::min())
    {
      m_ClassMaskImageOffset = sliceOffset ;
      m_DefaultClassLabel = defaultClassLabel ;
      m_UseSliceFiller = true ;
    }
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::GenerateData()
{
  std::cout << "DEBUG: Loading the images..." << std::endl ;
  this->LoadImages() ;

  std::cout << "DEBUG: Loading the class mask image..." << std::endl ;
  this->LoadClassMaskImage()  ;

  std::cout << "DEBUG: Generating the vector image..." << std::endl ;
  this->GenerateVectorImage() ;

  if ( m_OutputNormalized )
    {
      std::cout << "DEBUG: Calculating the statistics of the images..." << std::endl ;
      this->CalculateImageStatistics() ;
      std::cout << "DEBUG: Normalizing the vector image..." << std::endl ;
      this->Normalize() ;
    }

  if ( m_OutputSampleType == VECTOR_IMAGE )
    {
      return ;
    }

  std::cout << "DEBUG: Generating the list sample..." << std::endl ;
  this->GenerateListSample() ;

  if ( m_OutputSampleType == LIST_SAMPLE )
    {
      return ;
    }

  if ( m_OutputSampleType == HISTOGRAM )
    {
      std::cout << "DEBUG: Generating the histogram..." << std::endl ;
      this->GenerateHistogram() ;
      return ;
    }

  if ( m_OutputSampleType == WEIGHTED_CENTEROID_KD_TREE )
    {
      std::cout << "DEBUG: Generating the k-d tree..." << std::endl ;
      this->GenerateWeightedCenteroidKdTree() ;
      return ;
    }
}

template< class TImage, class TClassMaskImage, class TVectorImage >
int
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::GetFileIndex(const char* fileName)
{
  for (unsigned int i = 0 ; i < m_ImageFileNames.size() ; i++)
    {
      if ( m_ImageFileNames[i] == fileName )
        {
          return i ;
        }
    }

  return -1 ;
}

template< class TImage, class TClassMaskImage, class TVectorImage >
int
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::GetClassIndex(unsigned int classLabel)
{
  for (unsigned int i = 0 ; i < m_UniqueClassLabels.size() ; i++)
    {
      if ( m_UniqueClassLabels[i] == classLabel )
        {
          return i ;
        }
    }

  return -1 ;
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::LoadImages()
{
  std::vector< std::string >::iterator iter = m_ImageFileNames.begin() ;

  unsigned int index = 0 ;
  while ( iter != m_ImageFileNames.end() )
    {
      m_ImageReaders.push_back(ImageReaderType::New()) ;
      m_ImageReaders[index]->SetFileName(m_ImageFileNames[index].c_str()) ;
      m_ImageReaders[index]->Update() ;
      ++iter ;
      ++index ;
    }
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::LoadClassMaskImage()
{
  // load class label mask image
  m_ClassMaskImageReader->SetFileName(m_ClassMaskImageFileName) ;
  m_ClassMaskImageReader->Update() ;

  if ( m_UseSliceFiller )
    {
      // make the size of the mask same as that of image
      // by filling and dropping slices.
      m_SliceFiller->SetInput(m_ClassMaskImageReader->GetOutput()) ;
      m_SliceFiller->SetStartingSliceNumber(m_ClassMaskImageOffset) ;
      m_SliceFiller->SetDesiredSize(m_ImageReaders[0]->
                                    GetOutput()->GetLargestPossibleRegion().GetSize()) ;
      m_SliceFiller->SetBackgroundPixelValue(m_DefaultClassLabel) ;
      m_SliceFiller->Update() ;
      
      m_ClassMaskImage = m_SliceFiller->GetOutput() ;
    }
  else
    {
      m_ClassMaskImage = m_ClassMaskImageReader->GetOutput() ;
    }
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::GenerateVectorImage()
{
  unsigned int i ;
  typename TImage::RegionType region = 
    m_ImageReaders[0]->GetOutput()->GetLargestPossibleRegion() ;

  typename TVectorImage::PixelType vi_pixel ;


  m_VectorImage->SetLargestPossibleRegion(region) ;
  m_VectorImage->SetBufferedRegion(region) ;
  m_VectorImage->Allocate() ;

  itk::ImageRegionIteratorWithIndex< TVectorImage >
    vi_iter(m_VectorImage, region) ;

  while (!vi_iter.IsAtEnd())
    {
      for ( i = 0 ; i < m_ImageFileNames.size() ; i++ )
        {
          vi_pixel[i] = 
            m_ImageReaders[i]->GetOutput()->GetPixel(vi_iter.GetIndex()) ;
        }
      vi_iter.Set(vi_pixel) ;
      ++vi_iter ;
    }
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::CalculateImageStatistics()
{
  unsigned int i ;
  unsigned int sampleSize = 1 ;

  typename TVectorImage::RegionType region = 
    m_VectorImage->GetLargestPossibleRegion() ;
  for ( i = 0 ; i < TImage::ImageDimension ; i++ )
    {
      sampleSize *= region.GetSize()[i] ;
    }

  MeasurementVectorType tempMins ;
  MeasurementVectorType tempMaxes ;

  typename TVectorImage::PixelType pixel ;
  m_ImageMins.Fill(itk::NumericTraits< MeasurementType >::Zero) ;
  m_ImageMaxes.Fill(itk::NumericTraits< MeasurementType >::Zero) ;
  m_ImageMeans.Fill(0.0) ;
  
  itk::ImageRegionIteratorWithIndex< TVectorImage >
    iter(m_VectorImage, region) ;

  while (!iter.IsAtEnd())
    {
      for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
        {
          pixel = iter.Get() ;

          m_ImageMeans[i] += (double) pixel[i] ;

          if ( pixel[i] < m_ImageMins[i] )
            {
              m_ImageMins[i] = pixel[i] ;
            }

          if ( pixel[i] > m_ImageMaxes[i] )
            {
              m_ImageMaxes[i] = pixel[i] ;
            }
        }
      ++iter ;
    }

  for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
    {
      m_ImageMeans[i] /= (double) sampleSize ;
      std::cout << "DEBUG: image means[" << i << "] = " << m_ImageMeans[i]
                << std::endl ;
      m_MeasurementsLowerBound[i] = m_ImageMins[i] ;
      m_MeasurementsUpperBound[i] = m_ImageMaxes[i] ;
    }

  double temp ;
  m_ImageStandardDeviations.Fill(0.0) ;
  iter.GoToBegin() ;

  while (!iter.IsAtEnd())
    {
      for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
        {
          pixel = iter.Get() ;

          temp = (double) pixel[i] - m_ImageMeans[i] ;
          temp *= temp ;
          m_ImageStandardDeviations[i] += temp ;
        }
      ++iter ;
    }

  for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
    {
      m_ImageStandardDeviations[i] = 
        vcl_sqrt(m_ImageStandardDeviations[i] / (double) sampleSize) ;
      std::cout << "DEBUG: image standard deviations[" << i << "] = " 
                << m_ImageStandardDeviations[i]
                << std::endl ;
    }
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::Normalize()
{
  unsigned int i ;
  typename TVectorImage::PixelType pixel ;

  itk::ImageRegionIteratorWithIndex< TVectorImage >
    iter(m_VectorImage, m_VectorImage->GetLargestPossibleRegion()) ;

  while ( !iter.IsAtEnd() )
    {
      pixel = iter.Get() ;

      for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
        { 
          pixel[i] = 
            (MeasurementType) ((pixel[i] - m_ImageMeans[i]) / 
            m_ImageStandardDeviations[i] * m_NormalizationScale) ;
        }

      iter.Set(pixel) ;
      ++iter ;
    }
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::GenerateListSample()
{
  unsigned int i ;
  int classIndex ;
  
  m_MeasurementsLowerBound.Fill(itk::NumericTraits< MeasurementType >::max()) ;
  m_MeasurementsUpperBound.Fill(itk::NumericTraits< MeasurementType >::min()) ;

  m_Sample->SetImage(m_VectorImage) ;

  itk::ImageRegionIteratorWithIndex< TClassMaskImage >  
    m_iter( m_ClassMaskImage, m_ClassMaskImage->GetLargestPossibleRegion()) ;
  
  ImageSampleType::Iterator s_iter = m_Sample->Begin() ;
  ImageSampleType::Iterator s_end = m_Sample->End() ;

  MeasurementVectorType temp ;
  unsigned int numberOfClasses = m_UniqueClassLabels.size() ;
  while ( s_iter != s_end )
    {
      classIndex = this->GetClassIndex(m_iter.Get()) ;
//       std::cout << "DEBUG: class index = " << classIndex 
//                 << " labels size = " << m_UniqueClassLabels.size()
//                 << std::endl ;
      if ( classIndex == -1 )
        {
          m_UniqueClassLabels.push_back(m_iter.Get()) ;
          numberOfClasses = m_UniqueClassLabels.size() ;
          classIndex = m_UniqueClassLabels.size() - 1 ;
          m_ClassMeans.resize(m_UniqueClassLabels.size()) ;
          m_ClassMeans[classIndex].Fill(0.0) ;
          m_ClassCovariances.resize(m_UniqueClassLabels.size()) ;
          m_ClassCovariances[classIndex].Fill(0.0) ;
          m_ClassSizes.resize(m_UniqueClassLabels.size()) ;
          m_ClassSizes[classIndex] = 0 ;
        }

      temp = s_iter.GetMeasurementVector() ;

      if ( std::find(m_SelectedClassLabels.begin(), m_SelectedClassLabels.end(), 
                    m_iter.Get()) != m_SelectedClassLabels.end() )
        {
          // selected class
          m_Subsample->AddInstance(s_iter.GetInstanceIdentifier()) ;

          // add the class label
          m_ClassLabels[s_iter.GetInstanceIdentifier()] = m_iter.Get() ;

          // update the sample space bound
          for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
            {
              if ( temp[i] < m_MeasurementsLowerBound[i] )
                {
                  m_MeasurementsLowerBound[i] = temp[i] ;
                }
              
              if ( temp[i] > m_MeasurementsUpperBound[i] )
                {
                  m_MeasurementsUpperBound[i] = temp[i] ;
                }
            }
        }

      for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
        {
          // used as a temporary sum storage
          m_ClassMeans[classIndex][i] += temp[i] ;
        }

      m_ClassSizes[classIndex] += 1 ;

      ++m_iter ;
      ++s_iter ;
    }

  // calculate the means
  for ( i = 0 ; i < numberOfClasses ; i++ )
    {
      m_ClassMeans[i] /= (double) m_ClassSizes[i] ;
      std::cout << " class labels = " << m_UniqueClassLabels[i] 
                << " mean = " << m_ClassMeans[i] 
                << " size = " << m_ClassSizes[i] << std::endl ;
    }

  m_iter.GoToBegin() ;
  s_iter = m_Sample->Begin() ;

  unsigned int row, col ;
  MeanType diff ;
  // calculate the covariances
  while (s_iter != s_end)
    {
      classIndex = this->GetClassIndex(m_iter.Get()) ;
      temp = s_iter.GetMeasurementVector() ;

      for (i = 0 ; i < m_NumberOfMeasurements ; i++)
        {
          diff[i] = temp[i] - m_ClassMeans[classIndex][i] ;
        }

      for ( row = 0; row < m_NumberOfMeasurements ; row++)
        {
          for ( col = 0; col < row + 1 ; col++)
            {
              m_ClassCovariances[classIndex].GetVnlMatrix()(row,col) += 
                diff[row] * diff[col] ;
            }
        }
      ++m_iter ;
      ++s_iter ;
    }

  // fills the upper triangle using the lower triangle  

  for ( i = 0 ; i < numberOfClasses ; i++ )
    {
      for (row = 1 ; row < m_NumberOfMeasurements ; row++)
        {
          for (col = 0 ; col < row ; col++)
            {
              m_ClassCovariances[i].GetVnlMatrix()(col, row) = 
                m_ClassCovariances[i].GetVnlMatrix()(row, col) ;
            } 
        }
      m_ClassCovariances[i].GetVnlMatrix() /= m_ClassSizes[i] ;
    }
}



template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::GenerateHistogram()
{
  unsigned int i ;
  HistogramType::SizeType histogramSize ;
  HistogramType::MeasurementVectorType lowerBound ;
  HistogramType::MeasurementVectorType upperBound ;
  
  for ( i = 0 ; i < m_NumberOfMeasurements ; i++ )
    {
      histogramSize[i] = 
        (unsigned long) (m_MeasurementsUpperBound[i] - 
                         m_MeasurementsLowerBound[i] + 1) ;
      lowerBound[i] = m_MeasurementsLowerBound[i] - 0.5 ;
      upperBound[i] = m_MeasurementsUpperBound[i] + 0.5 ;
    }
  
  // creats equal size bins ;
  m_Histogram->Initialize(histogramSize, lowerBound, upperBound) ;

  // imports the subsample to histogram
  m_Importer->SetListSample(m_Subsample.GetPointer()) ;
  m_Importer->SetHistogram(m_Histogram.GetPointer()) ;
  time_t begin = clock() ;
  m_Importer->Run() ;
  time_t end = clock() ;
  std::cout << "DEBUG: histogram genererated in " 
            << double(end - begin) /CLOCKS_PER_SEC 
            << " seconds" << std::endl ;
}

template< class TImage, class TClassMaskImage, class TVectorImage >
void
ValidationSampleGenerator< TImage, TClassMaskImage, TVectorImage >
::GenerateWeightedCenteroidKdTree()
{
  m_TreeGenerator->SetSample(m_Subsample) ;
  m_TreeGenerator->SetBucketSize(m_KdTreeBucketSize) ;
  time_t begin = clock() ;
  m_TreeGenerator->GenerateData() ;
  time_t end = clock() ;
  m_KdTree = m_TreeGenerator->GetOutput() ;
  std::cout << "DEBUG: k-d tree genererated in " 
            << double(end - begin) /CLOCKS_PER_SEC 
            << " seconds" << std::endl ;
}

#endif
