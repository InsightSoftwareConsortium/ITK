/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkNeighborhoodSamplerTest.cxx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkWin32Header.h"

#include <fstream>

#include "itkArray.h"

#include "itkListSample.h"
#include "itkSubsample.h"
#include "itkNeighborhoodSampler.h"

#define NO_OF_DIMENSIONS 2
typedef double ValueType ;

int itkNeighborhoodSamplerTest(int argc, char* argv[] )
{
  namespace stat = itk::Statistics ;

  typedef itk::Vector< ValueType, NO_OF_DIMENSIONS > MeasurementVectorType ; 
  typedef stat::ListSample< MeasurementVectorType > SampleType ;

  SampleType::Pointer sample = SampleType::New() ;

  MeasurementVectorType center ;
  center.Fill(0.0) ;
  
  double radius = 1.0 ;

  // add four vectors within the radius
  MeasurementVectorType tempVector ;
  tempVector[0] = 0.0 ;
  tempVector[1] = 0.5 ;
  sample->PushBack(tempVector) ;

  tempVector[0] = 0.0 ;
  tempVector[1] = -0.5 ;
  sample->PushBack(tempVector) ;

  tempVector[0] = 0.5 ;
  tempVector[1] = 0.0 ;
  sample->PushBack(tempVector) ;

  tempVector[0] = -0.5 ;
  tempVector[1] = 0.0 ;
  sample->PushBack(tempVector) ;

  // add four vectors outside of the radius
  tempVector[0] = 0.0 ;
  tempVector[1] = 2.0 ;
  sample->PushBack(tempVector) ;

  tempVector[0] = 0.0 ;
  tempVector[1] = -2.0 ;
  sample->PushBack(tempVector) ;

  tempVector[0] = 2.0 ;
  tempVector[1] = 0.0 ;
  sample->PushBack(tempVector) ;

  tempVector[0] = -2.0 ;
  tempVector[1] = 0.0 ;
  sample->PushBack(tempVector) ;

  typedef stat::NeighborhoodSampler< SampleType > SamplerType ;
  
  SamplerType::Pointer sampler = SamplerType::New() ;

  sampler->SetInputSample(sample) ;
  sampler->SetCenter(&center) ;
  sampler->SetRadius(&radius) ;
  sampler->Update() ;

  SamplerType::OutputType::Pointer output = sampler->GetOutput() ;
  
  if ( output->Size() != 4 )
    {
      std::cout 
        << "Test failed: The output size should be 4. The actual size =" 
        << output->Size() << std::endl;
      return EXIT_FAILURE;
    }

  for ( unsigned int i = 0 ; i < 4 ; i++ )
    {
      if ( output->GetInstanceIdentifier(i) != i )
        {
          std::cout << "Test failed." << std::endl;
          return EXIT_FAILURE;
        }
    }

  std::cout << "Test succeeded." << std::endl;
  return EXIT_SUCCESS ;
}
