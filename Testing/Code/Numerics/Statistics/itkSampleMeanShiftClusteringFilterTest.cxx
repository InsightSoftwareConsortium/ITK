/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleMeanShiftClusteringFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImageFileReader.h"
#include "itkImageRegionIterator.h"
#include "itkScalarImageToListAdaptor.h"
#include "itkKdTree.h"
#include "itkKdTreeGenerator.h"
#include "itkMeanShiftModeCacheMethod.h"
#include "itkHypersphereKernelMeanShiftModeSeeker.h"
#include "itkSampleMeanShiftBlurringFilter.h"
#include "itkSampleMeanShiftClusteringFilter.h"

int itkSampleMeanShiftClusteringFilterTest(int argc, char* argv[] ) 
{
  std::cout << "SampleMeanShiftClusteringFilter Test \n \n"; 

  if (argc < 2)
    {
      std::cout << "ERROR: data file name argument missing." 
                << std::endl ;
      return EXIT_FAILURE;
    }

  typedef unsigned char PixelType ;
  typedef itk::Image< PixelType, 2 > ImageType ;
  typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  ImageReaderType::Pointer imageReader = ImageReaderType::New() ;

  imageReader->SetFileName( argv[1] ) ;
  imageReader->Update() ;
  ImageType::Pointer image = imageReader->GetOutput() ;
  
  typedef itk::Statistics::ScalarImageToListAdaptor< ImageType >
    ListSampleType ;
  
  ListSampleType::Pointer listSample = 
    ListSampleType::New() ;
  listSample->SetImage( image ) ;

  typedef itk::Statistics::KdTreeGenerator< ListSampleType > 
    TreeGeneratorType ;
  TreeGeneratorType::Pointer treeGenerator = TreeGeneratorType::New() ;
  treeGenerator->SetSample( listSample ) ;
  treeGenerator->SetBucketSize( 500 ) ;
  treeGenerator->Update() ;

  typedef TreeGeneratorType::KdTreeType TreeType ;
  TreeType::Pointer tree = treeGenerator->GetOutput() ;

  typedef itk::Statistics::HypersphereKernelMeanShiftModeSeeker< 
    TreeType > ModeSeekerType ;
  ModeSeekerType::Pointer modeSeeker = ModeSeekerType::New() ;
  modeSeeker->SetInputSample( tree ) ;
  modeSeeker->SetSearchRadius( 4.0 ) ;

  typedef itk::Statistics::MeanShiftModeCacheMethod< TreeType::MeasurementVectorType > CacheMethodType ;
  CacheMethodType::Pointer cacheMethod = CacheMethodType::New() ;
  cacheMethod->SetMaximumEntries(255) ;
  cacheMethod->SetMaximumConsecutiveFailures(100) ;
  cacheMethod->SetHitRatioThreshold( 0.5 ) ;
  modeSeeker->SetCacheMethod( cacheMethod.GetPointer() ) ;

  typedef itk::Statistics::SampleMeanShiftBlurringFilter< TreeType >
    FilterType ;
  FilterType::Pointer filter = FilterType::New() ;
  filter->SetInputSample( tree ) ;
  filter->SetMeanShiftModeSeeker( modeSeeker ) ;
  try 
    {
    filter->Update() ;
    }
  catch ( ... )
    {
    std::cout << "Test failed. - blurring proces" << std::endl;
    return EXIT_FAILURE;
    }

  typedef ImageType OutputImageType ;
  OutputImageType::Pointer outputImage = OutputImageType::New() ;
  outputImage->SetRegions( image->GetLargestPossibleRegion() ) ;
  outputImage->Allocate() ;

  typedef itk::ImageRegionIterator< OutputImageType > ImageIteratorType ;
  ImageIteratorType io_iter( outputImage,
                             outputImage->GetLargestPossibleRegion() ) ;
  io_iter.GoToBegin() ;

  FilterType::OutputType::Pointer output = filter->GetOutput() ;
  FilterType::OutputType::Iterator fo_iter = output->Begin() ;
  FilterType::OutputType::Iterator fo_end = output->End() ;

  while ( fo_iter != fo_end )
    {
    io_iter.Set( (PixelType) fo_iter.GetMeasurementVector()[0]) ;
    ++fo_iter ;
    ++io_iter ;
    }

  ListSampleType::Pointer listSample2 = ListSampleType::New() ;
  listSample2->SetImage( outputImage ) ;

  TreeGeneratorType::Pointer treeGenerator2 = TreeGeneratorType::New() ;
  treeGenerator2->SetSample( listSample2 ) ;
  treeGenerator2->SetBucketSize( 500 ) ;
  treeGenerator2->Update() ;

  typedef itk::Statistics::SampleMeanShiftClusteringFilter< TreeType >
    ClusteringMethodType ;

  ClusteringMethodType::Pointer clusteringMethod =
    ClusteringMethodType::New() ;
  clusteringMethod->SetInputSample( treeGenerator2->GetOutput() ) ;
  clusteringMethod->SetThreshold( 0.5 ) ;
  clusteringMethod->SetMinimumClusterSize( 16 ) ;
  try 
    {
    clusteringMethod->Update() ;
    }
  catch ( ... )
    {
    std::cout << "Test failed. - clustering proces" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}



