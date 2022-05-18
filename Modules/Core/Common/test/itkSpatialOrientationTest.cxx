/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkSpatialOrientation.h"
#include <iostream>
#include <set>


int
itkSpatialOrientationTest(int, char *[])
{

  // Test streaming enumeration for SpatialOrientationEnums::CoordinateTerms elements
  const std::set<itk::SpatialOrientationEnums::CoordinateTerms> allCoordinateTerms{
    itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_UNKNOWN,
    itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Right,
    itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Left,
    itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Posterior,
    itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Anterior,
    itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Inferior,
    itk::SpatialOrientationEnums::CoordinateTerms::ITK_COORDINATE_Superior
  };
  for (const auto & ee : allCoordinateTerms)
  {
    std::cout << "STREAMED ENUM VALUE SpatialOrientationEnums::CoordinateTerms: " << ee << std::endl;
  }

  // Test streaming enumeration for SpatialOrientationEnums::CoordinateMajornessTerms elements
  const std::set<itk::SpatialOrientationEnums::CoordinateMajornessTerms> allCoordinateMajornessTerms{
    itk::SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_PrimaryMinor,
    itk::SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_SecondaryMinor,
    itk::SpatialOrientationEnums::CoordinateMajornessTerms::ITK_COORDINATE_TertiaryMinor
  };
  for (const auto & ee : allCoordinateMajornessTerms)
  {
    std::cout << "STREAMED ENUM VALUE SpatialOrientationEnums::CoordinateMajornessTerms: " << ee << std::endl;
  }

  // Test streaming enumeration for SpatialOrientationEnums::ValidCoordinateOrientations elements
  const std::set<itk::SpatialOrientationEnums::ValidCoordinateOrientations> allValidCoordinateOrientations{
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_INVALID,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RIA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LIA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RSA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LSA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLP,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IRA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ILA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SRA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SLA,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RPS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LPS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_RAS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_LAS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALI,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PRS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PLS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ARS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ALS,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IPL,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SPL,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_IAL,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_SAL,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASR,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PIL,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_PSL,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_AIL,
    itk::SpatialOrientationEnums::ValidCoordinateOrientations::ITK_COORDINATE_ORIENTATION_ASL
  };
  for (const auto & ee : allValidCoordinateOrientations)
  {
    std::cout << "STREAMED ENUM VALUE SpatialOrientationEnums::ValidCoordinateOrientations: " << ee << std::endl;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
