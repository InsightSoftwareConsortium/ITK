/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSubsampleTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkImageToListAdaptor.h"
#include "itkSubsample.h"
#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"
#include "itkScalarToArrayCastImageFilter.h"
#include "itkFixedArray.h"

int itkSubsampleTest(int, char* [] ) 
{
  std::cout << "Subsample Test \n \n"; 
  bool pass = true;
  std::string whereFail = "" ;

  typedef itk::Image< float, 3 > FloatImage ;

  // Now generate a random image
  typedef itk::RandomImageSource<FloatImage> SourceType;
  SourceType::Pointer source = SourceType::New();
  unsigned long size[3] = {17, 8, 20} ;
  unsigned long totalSize = size[0] * size[1] * size[2] ;

  source->SetSize(size);
  float minValue = -100.0;
  float maxValue = 1000.0;

  source->SetMin( static_cast< FloatImage::PixelType >( minValue ) );
  source->SetMax( static_cast< FloatImage::PixelType >( maxValue ) );
  source->Update() ;


  // creat a new image with array pixel type from the source
  typedef itk::FixedArray< FloatImage::PixelType, 1 > ArrayPixelType ;
  typedef itk::Image< ArrayPixelType, 3 > ArrayPixelImageType ;
  typedef itk::ScalarToArrayCastImageFilter< FloatImage, ArrayPixelImageType >
    ImageCastFilterType ;
  ImageCastFilterType::Pointer castFilter = ImageCastFilterType::New() ;
  castFilter->SetInput(source->GetOutput()) ;
  castFilter->Update() ;

  typedef  itk::Statistics::ImageToListAdaptor< ArrayPixelImageType >
    ImageToListAdaptorType ;

  ImageToListAdaptorType::Pointer sample = ImageToListAdaptorType::New() ;
  sample->SetImage(castFilter->GetOutput()) ;

  typedef itk::Statistics::Subsample< ImageToListAdaptorType >
    SubsampleType ;
  
  SubsampleType::Pointer subsample = 
    SubsampleType::New() ;

  subsample->SetSample(sample) ;
  
  // tests begin
  
  // add only the first half of instances of the sample
  for (ImageToListAdaptorType::InstanceIdentifier id = 0 ; 
       id < static_cast< ImageToListAdaptorType::InstanceIdentifier >
         (sample->Size() / 2) ;
       id++)
    {
      subsample->AddInstance(id) ;
    }

  if ((totalSize / 2) != subsample->Size())
    {
      pass = false ;
      whereFail = "Size()" ;
    }

  if ((totalSize / 2 ) != subsample->Size(0))
    {
      pass = false ;
      whereFail = "Size(0)" ;
    }

  if ((totalSize / 2) != subsample->GetNumberOfInstances())
    {
      pass = false ;
      whereFail = "GetNumberOfInstances()" ;
    }

  ArrayPixelImageType::IndexType index ;
  index.Fill(2) ;// index {2, 2, 2} = instance identifier (offset from image) 
  ArrayPixelImageType::PixelType pixel = sample->GetImage()->GetPixel(index) ;
  ImageToListAdaptorType::InstanceIdentifier id = 
    static_cast< FloatImage::OffsetValueType >(sample->GetImage()
                                               ->ComputeOffset(index)) ;

  if (pixel[0] != subsample->GetMeasurementVector(id)[0])
    {
      pass = false ;
      whereFail = "GetMeasurementVector()" ;
    }

  // iterator test
  typedef itk::ImageRegionIterator< ArrayPixelImageType > ImageIterator ;
  ImageIterator i_iter(sample->GetImage(),
                       sample->GetImage()->GetLargestPossibleRegion()) ;

  SubsampleType::Iterator s_iter = subsample->Begin() ;
  unsigned int count = 0 ;
  while (count < subsample->Size())
    {
      if (i_iter.Get()[0] != s_iter.GetMeasurementVector()[0])
        {
          pass = false ;
          whereFail = "Iterator: GetMeasurementVector()" ;
        }
      ++count ;
      ++i_iter ;
      ++s_iter ;
    }

  if (s_iter != subsample->End())
    {
      pass = false ;
      whereFail = "Iterator: End()" ;
    }

  if( !pass )
    {
      std::cout << "Test failed in " << whereFail << "." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;


}



