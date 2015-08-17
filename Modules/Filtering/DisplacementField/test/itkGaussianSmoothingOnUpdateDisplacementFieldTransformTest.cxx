/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

/**
 * Test the UpdateTransformParameters and related methods,
 * introduced by this derivation.
 *
 * TODO: Create a more complete numerical test for the smoothing.
 */

int itkGaussianSmoothingOnUpdateDisplacementFieldTransformTest(int ,char *[] )
{

  const unsigned int dimensions = 2;
  typedef itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<
                                                  double, dimensions>
                                                    DisplacementTransformType;

  /* Create a displacement field transform */
  DisplacementTransformType::Pointer displacementTransform =
      DisplacementTransformType::New();
  typedef DisplacementTransformType::DisplacementFieldType FieldType;
  FieldType::Pointer field = FieldType::New(); //This is based on itk::Image

  FieldType::SizeType size;
  FieldType::IndexType start;
  FieldType::RegionType region;
  int dimLength = 20;
  size.Fill( dimLength );
  start.Fill( 0 );
  region.SetSize( size );
  region.SetIndex( start );
  field->SetRegions( region );
  field->Allocate();

  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  field->FillBuffer( zeroVector );

  displacementTransform->SetDisplacementField( field );

  /* Test SmoothDisplacementFieldGauss */
  std::cout << "Test SmoothDisplacementFieldGauss" << std::endl;
  typedef DisplacementTransformType::ParametersValueType ParametersValueType;
  ParametersValueType paramsZero = itk::NumericTraits< itk::NumericTraits< ParametersValueType >::ValueType >::ZeroValue();
  DisplacementTransformType::ParametersType params;
  DisplacementTransformType::ParametersType
                  paramsFill( displacementTransform->GetNumberOfParameters() );
  DisplacementTransformType::ParametersValueType paramsFillValue = 0.0;
  paramsFill.Fill( paramsFillValue );
  // Add an outlier to visually see that some smoothing is taking place.
  unsigned int outlier = dimLength*dimensions*4 + dimLength*dimensions / 2;
  paramsFill( outlier ) = 99.0;
  paramsFill( outlier + 1 ) = 99.0;
  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField(0.25);
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField(3);
  displacementTransform->SetParameters( paramsFill );
  //params = displacementTransform->GetParameters();
  //std::cout << "params *before* SmoothDisplacementFieldGauss: " << std::endl
  //          << params << std::endl;
  params = displacementTransform->GetParameters();
  //std::cout << "field->GetPixelContainter *after* Smooth: "
  //          << field->GetPixelContainer() << std::endl;
  /* We should see 0's on all boundaries from the smoothing routine */
  unsigned int linelength = dimLength * dimensions;
  for( unsigned int i=0; i < displacementTransform->GetNumberOfParameters();
        i++ )
    {
    bool ok = true;
    if( i < linelength && itk::Math::NotAlmostEquals( params[i], paramsZero ) )
      ok = false;
    if( i % linelength == 0 && itk::Math::NotAlmostEquals( params[i], paramsZero ) )
      ok = false;
    if( i % linelength == (linelength - 1) && itk::Math::NotAlmostEquals( params[i], paramsZero ) )
      ok = false;
    if( !ok )
      {
      std::cout << "0-valued boundaries not found when expected "
                << "after smoothing." << std::endl;
      std::cout << "params: " << std::endl << params << std::endl;
      return EXIT_FAILURE;
      }
    }
  /* Check that we have some smoothing around the outlier we set above. */
  std::cout << "Parameters *after* SmoothDisplacementFieldGauss, around "
            << "outlier: " << std::endl;
  for(int i=-2; i< 3; i++ )
    {
     for(int j=-2; j< 3; j++ )
      {
      unsigned int index = outlier +
        (unsigned int) (i * (signed int)(dimLength*dimensions) + j);
      std::cout << params(index) << " ";
      }
    std::cout << std::endl;
    }

  /* Test UpdateTransformParameters */
  std::cout << "Testing UpdateTransformParameters..." << std::endl;
  /* fill with 0 */
  field->FillBuffer( zeroVector );
  DisplacementTransformType::DerivativeType
                update( displacementTransform->GetNumberOfParameters() );
  update.Fill(1.2);
  displacementTransform->UpdateTransformParameters( update );
  params = displacementTransform->GetParameters();
  //std::cout  << "params: " << std::endl << params << std::endl;
             //<< "derivativeTruth: " << std::endl << derivative << std::endl
  /* We should see 0's on all boundaries from the smoothing routine */
  {
  linelength = dimLength * dimensions;
  for( unsigned int i=0; i < displacementTransform->GetNumberOfParameters();
        i++ )
    {
    bool ok = true;
    if( i < linelength && itk::Math::NotAlmostEquals( params[i], paramsZero ) )
      ok = false;
    if( i % linelength == 0 && itk::Math::NotAlmostEquals( params[i], paramsZero ) )
      ok = false;
    if( i % linelength == (linelength - 1) && itk::Math::NotAlmostEquals( params[i], paramsZero ) )
      ok = false;
    if( !ok )
      {
      std::cout << "0-valued boundaries not found when expected "
                << "after UpdateTransformParameters:" << std::endl;
      std::cout << "params: " << std::endl << params << std::endl;
      return EXIT_FAILURE;
      }
    }
  }

  /* Update with an uneven field to verify some smoothing is happening. */
  field->FillBuffer( zeroVector );
  update.Fill( 1.0 );
  update( outlier ) = 99.0;
  update( outlier + 1 ) = 99.0;
  displacementTransform->UpdateTransformParameters( update );
  params = displacementTransform->GetParameters();
  std::cout << "UpdateTransformParameters with uneven update: " << std::endl
            << "params: " << std::endl << params << std::endl;
  /* Check that we have some smoothing around the outlier we set above. */
  std::cout << "Parameters *after* UpdateTransformParameters with "
            << "uneven field, around outlier: " << std::endl;
  for(int i=-2; i< 3; i++ )
    {
     for(int j=-2; j< 3; j++ )
      {
      unsigned int index = outlier +
        (unsigned int) (i * (signed int)(dimLength*dimensions) + j);
      std::cout << params(index) << " ";
      if( itk::Math::AlmostEquals( params(index), paramsFillValue ) )
        {
        std::cout << "Expected to read a smoothed value at this index."
                  << " Instead, read " << params(index) << std::endl;
        return EXIT_FAILURE;
        }
      }
    std::cout << std::endl;
    }

  /* Exercise Get/Set sigma */
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField(2);
  std::cout << "sigma: "
            << displacementTransform->GetGaussianSmoothingVarianceForTheUpdateField()
            << std::endl;

  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField(2);
  std::cout << "sigma: "
            << displacementTransform->GetGaussianSmoothingVarianceForTheTotalField()
            << std::endl;

  return EXIT_SUCCESS;
}
