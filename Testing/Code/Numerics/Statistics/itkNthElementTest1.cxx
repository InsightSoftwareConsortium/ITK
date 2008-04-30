/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNthElementTest1.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkListSample.h"
#include "itkStatisticsAlgorithm.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include <fstream>

#include <vector>
#include <algorithm>

int itkNthElementTest1(int argc, char * argv [] )
{
  std::cout << "Statistics Algorithm Test \n \n";
  bool pass = true;

  typedef float MeasurementType;
  const unsigned int Dimension = 1;
  const unsigned int testDimension = 0;

  typedef itk::FixedArray< MeasurementType, Dimension > MeasurementVectorType;

  typedef itk::Statistics::ListSample< MeasurementVectorType > SampleType;
  typedef itk::Statistics::Subsample< SampleType > SubsampleType;

  SampleType::Pointer sample = SampleType::New();

  sample->SetMeasurementVectorSize( Dimension );

  typedef std::vector< MeasurementType > VerificationVectorType;
  VerificationVectorType verificationVector;

  if( argc > 1 )
    {
    //
    // Read the values from a file
    //
    std::cout << "Reading input file " << argv[1] << std::endl;

    std::ifstream valuesFile;
    valuesFile.open( argv[1] );
    
    MeasurementVectorType vector;
    valuesFile >> vector[testDimension];

    while( !valuesFile.eof() )
      {
      sample->PushBack( vector );
      MeasurementType value;
      valuesFile >> value;
      vector[testDimension] = value;
      verificationVector.push_back( value );
      }

    valuesFile.close();

    }
  else
    {
    // 
    // Generate values using a Random number generator
    //
    typedef itk::Statistics::MersenneTwisterRandomVariateGenerator NumberGeneratorType;

    NumberGeneratorType::Pointer randomNumberGenerator = NumberGeneratorType::New();
    randomNumberGenerator->Initialize();
 
    unsigned int numberOfValues = 100;
    MeasurementVectorType vector;

    for(unsigned int i=0; i<numberOfValues; i++)
      {
      MeasurementType value = randomNumberGenerator->GetNormalVariate( 0.0, 1.0 );
      vector[testDimension] = value;
      sample->PushBack( vector );
      verificationVector.push_back( value );
      }
    }


  SubsampleType::Pointer subsample1 = SubsampleType::New();

  subsample1->SetSample( sample );

  subsample1->InitializeWithAllInstances();

  // Sort all the values in subsample1
  itk::Statistics::HeapSort< SubsampleType >( subsample1, testDimension, 0, subsample1->Size());

  try
    {
    //
    // Test the NthElement Algorithm by asking k-th elements in subsample2 and
    // comparing them to the entries in the sorted subsample1.
    //
    for( unsigned int kth = 0; kth < subsample1->Size(); kth++)
      {
      SubsampleType::Pointer subsample2 = SubsampleType::New();
      subsample2->SetSample( sample );
      subsample2->InitializeWithAllInstances();

      MeasurementType kthValue1 = itk::Statistics::NthElement< SubsampleType >(
        subsample2, testDimension, 0, subsample2->Size(), kth );

      MeasurementType kthValue2 =
        subsample1->GetMeasurementVectorByIndex( kth )[testDimension];

      for(unsigned int i=0; i<sample->Size(); i++)
        {
        verificationVector[i] = sample->GetMeasurementVector(i)[testDimension];
        }

      std::nth_element( verificationVector.begin(), 
                        verificationVector.begin() + kth,
                        verificationVector.end() );

      MeasurementType kthValue3 = verificationVector[kth];

      if( vnl_math_abs( kthValue1 - kthValue2 ) > vnl_math::eps )
        {
        std::cerr << "Comparison failed for component kth= " << kth << std::endl;
        pass = false;
        }

      if( vnl_math_abs( kthValue1 - kthValue3 ) > vnl_math::eps )
        {
        std::cerr << "Comparison with std::nth_element failed for component kth= " << kth << std::endl;
        for(unsigned int k=0; k<verificationVector.size(); k++)
          {
          std::cerr << "STL: " << verificationVector[k] << " : " << subsample2->GetMeasurementVectorByIndex(k)[testDimension] << std::endl;
          }
        pass = false;
        }

      }
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  if( !pass )
    {
    std::cerr << "Test FAILED " << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}

