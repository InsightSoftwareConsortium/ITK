/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMembershipSampleTest.cxx
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
#include "itkMembershipSample.h"
#include "itkRandomImageSource.h"
#include "itkImageRegionIterator.h"
#include "itkScalarToArrayCastImageFilter.h"
#include "itkFixedArray.h"

int itkMembershipSampleTest(int, char* [] ) 
{
  std::cout << "MembershipSample Test \n \n"; 
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

  // creats a sample
  typedef  itk::Statistics::ImageToListAdaptor< ArrayPixelImageType >
    ImageToListAdaptorType ;

  ImageToListAdaptorType::Pointer sample = ImageToListAdaptorType::New() ;
  sample->SetImage(castFilter->GetOutput()) ;

  //creates a memberhip sample
  typedef itk::Statistics::MembershipSample< ImageToListAdaptorType >
    MembershipSampleType ;
  
  MembershipSampleType::Pointer membershipSample = 
    MembershipSampleType::New() ;

  unsigned int numberOfClasses = 5 ;
  membershipSample->SetSample(sample) ;
  membershipSample->SetNumberOfClasses(numberOfClasses) ;

  
  // tests begin
  if (membershipSample->GetNumberOfClasses() != numberOfClasses)
    {
      pass = false ;
      whereFail = "GetNumberOfClasses()" ;
    }

  for (ImageToListAdaptorType::InstanceIdentifier id = 0 ; 
       id < static_cast< ImageToListAdaptorType::InstanceIdentifier >
         (sample->Size()) ;
       id++)
    {
      membershipSample->AddInstance( id % numberOfClasses, id) ;
      if (membershipSample->GetClassLabel(id) != (id % numberOfClasses))
        {
          pass = false ;
          whereFail = "AddInstance + GetClassLabel" ;
        }
    }

  for (unsigned int i = 0 ; i < numberOfClasses ; i++)
    {
      if (membershipSample->GetClassSampleSize(i) != 
          totalSize / numberOfClasses)
        {
          pass = false ;
          whereFail = "GetClassSampleSize()" ;
        }
    }

  if (totalSize != membershipSample->Size())
    {
      pass = false ;
      whereFail = "Size()" ;
    }

  ArrayPixelImageType::IndexType index ;
  index.Fill(2) ;// index {2, 2, 2} = instance identifier (offset from image) 
  ArrayPixelImageType::PixelType pixel = sample->GetImage()->GetPixel(index) ;
  ImageToListAdaptorType::InstanceIdentifier id = 
    static_cast< FloatImage::OffsetValueType >(sample->GetImage()
                                               ->ComputeOffset(index)) ;

  if (pixel[0] != membershipSample->GetMeasurementVector(id)[0])
    {
      pass = false ;
      whereFail = "GetMeasurementVector()" ;
    }

  // iterator test
  typedef itk::ImageRegionIterator< ArrayPixelImageType > ImageIterator ;
  ImageIterator i_iter(sample->GetImage(),
                       sample->GetImage()->GetLargestPossibleRegion()) ;

  MembershipSampleType::Iterator s_iter = membershipSample->Begin() ;

  while (!i_iter.IsAtEnd())
    {
      if (i_iter.Get()[0] != s_iter.GetMeasurementVector()[0])
        {
          pass = false ;
          whereFail = "Iterator: GetMeasurementVector()" ;
        }
      ++i_iter ;
      ++s_iter ;
    }

  if (s_iter != membershipSample->End())
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



