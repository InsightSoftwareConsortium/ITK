/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSampleSelectiveMeanShiftBlurringFilterTest.cxx
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
#include "itkJointDomainImageToListAdaptor.h"
#include "itkSampleSelectiveMeanShiftBlurringFilter.h"
#include "itkHypersphereKernelMeanShiftModeSeeker.h"

int itkSampleSelectiveMeanShiftBlurringFilterTest(int argc, char* argv[] ) 
{
  std::cout << "SampleSelectiveMeanShiftBlurringFilter Test \n \n"; 

  if (argc < 2)
    {
      std::cout << "ERROR: data file name argument missing." 
                << std::endl ;
      return EXIT_FAILURE;
    }

  typedef itk::Image< unsigned char, 2 > ImageType ;
  typedef itk::ImageFileReader< ImageType > ImageReaderType ;
  ImageReaderType::Pointer imageReader = ImageReaderType::New() ;

  imageReader->SetFileName( argv[1] ) ;
  imageReader->Update() ;
  ImageType::Pointer image = imageReader->GetOutput() ;
  
  typedef itk::Statistics::JointDomainImageToListAdaptor< ImageType >
    ListSampleType ;
  
  ListSampleType::Pointer listSample = ListSampleType::New() ;
  listSample->SetImage( image ) ;

  ListSampleType::NormalizationFactorsType factors ;
  factors[0] = 4 ;
  factors[1] = 4 ;
  factors[2] = 8 ;
  listSample->SetNormalizationFactors( factors ) ;

  typedef itk::Statistics::HypersphereKernelMeanShiftModeSeeker< 
    ListSampleType > ModeSeekerType ;
  ModeSeekerType::Pointer modeSeeker = ModeSeekerType::New() ;
  modeSeeker->SetInputSample( listSample ) ;
  modeSeeker->SetSearchRadius( 1.0 ) ;

  typedef itk::Statistics::SampleSelectiveMeanShiftBlurringFilter< 
    ListSampleType > FilterType ;
  FilterType::Pointer filter = FilterType::New() ;
  filter->SetInputSample( listSample ) ;
  filter->SetMeanShiftModeSeeker( modeSeeker ) ;

  FilterType::ComponentSelectionsType componentSelections ;
  componentSelections.Fill( false ) ;
  componentSelections[2] = true ;
  filter->SetComponentSelections( componentSelections ) ;
  try
    {
    filter->Update() ;
    }
  catch ( ... )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}



