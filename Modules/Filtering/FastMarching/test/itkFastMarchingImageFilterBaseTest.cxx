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

#include "itkFastMarchingImageFilterBase.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"

template< unsigned int VDimension >
int FastMarchingImageFilterBase( )
  {
  typedef float PixelType;

  typedef itk::Image< PixelType, VDimension > ImageType;
  typename ImageType::Pointer input = ImageType::New();

  typedef itk::FastMarchingImageFilterBase< ImageType, ImageType >
      FMMType;
  typename FMMType::Pointer fmm = FMMType::New();
  fmm->SetInput( input );

  bool exception_caught = false;

  try
    {
    fmm->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    exception_caught = true;
    }

  if( !exception_caught )
    {
    std::cout <<"Exception is not caught!" <<std::endl;
    return EXIT_FAILURE;
    }

  typename ImageType::Pointer output = fmm->GetOutput();

  return EXIT_SUCCESS;
  }


// ----------------------------------------------------------------------------
int itkFastMarchingImageFilterBaseTest( int , char * [] )
  {
  if( FastMarchingImageFilterBase< 2 >() == EXIT_FAILURE )
    {
    std::cerr << "2D Fails" <<std::endl;
    return EXIT_FAILURE;
    }
  if( FastMarchingImageFilterBase< 3 >() == EXIT_FAILURE )
    {
    std::cerr << "3D Fails" <<std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
  }
