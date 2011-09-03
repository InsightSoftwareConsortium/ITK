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
#include "itkSpatialOrientationAdapter.h"
#include "itkQuaternionOrientationAdapter.h"

typedef itk::SpatialOrientation::CoordinateTerms                 SO_CoordTermsType;
typedef itk::SpatialOrientation::ValidCoordinateOrientationFlags SO_OrientationType;

typedef itk::SpatialOrientationAdapter<3> SO_AdapterType;
typedef SO_AdapterType::DirectionType     DirectionType;

typedef itk::QuaternionOrientationAdapter<3> Q_AdapterType;
typedef Q_AdapterType::OrientationType       Q_OrientationType;


std::string SO_OrientationToString(SO_OrientationType in)
{
  switch(in)
    {
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP:
      return std::string("RIP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP:
      return std::string("LIP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP:
      return std::string("RSP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP:
      return std::string("LSP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA:
      return std::string("RIA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA:
      return std::string("LIA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA:
      return std::string("RSA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA:
      return std::string("LSA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP:
      return std::string("IRP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP:
      return std::string("ILP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP:
      return std::string("SRP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP:
      return std::string("SLP");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA:
      return std::string("IRA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA:
      return std::string("ILA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA:
      return std::string("SRA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA:
      return std::string("SLA");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI:
      return std::string("RPI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI:
      return std::string("LPI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI:
      return std::string("RAI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI:
      return std::string("LAI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS:
      return std::string("RPS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS:
      return std::string("LPS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS:
      return std::string("RAS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS:
      return std::string("LAS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI:
      return std::string("PRI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI:
      return std::string("PLI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI:
      return std::string("ARI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI:
      return std::string("ALI");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS:
      return std::string("PRS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS:
      return std::string("PLS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS:
      return std::string("ARS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS:
      return std::string("ALS");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR:
      return std::string("IPR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR:
      return std::string("SPR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR:
      return std::string("IAR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR:
      return std::string("SAR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL:
      return std::string("IPL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL:
      return std::string("SPL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL:
      return std::string("IAL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL:
      return std::string("SAL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR:
      return std::string("PIR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR:
      return std::string("PSR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR:
      return std::string("AIR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR:
      return std::string("ASR");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL:
      return std::string("PIL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL:
      return std::string("PSL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL:
      return std::string("AIL");
    case itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL:
      return "ASL";
    default:
      {
      std::stringstream x;
      x << (in & 0xff) << ", " << ((in >> 8) & 0xff) << ", " << ((in >> 16) && 0xff);
      return x.str();
      }
    }
}

SO_OrientationType allOrientations[] =
  {
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RIA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LIA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RSA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LSA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLP,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IRA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ILA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SRA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SLA,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RPS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LPS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_RAS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_LAS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALI,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PRS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PLS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ARS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ALS,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IPL,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SPL,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_IAL,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_SAL,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASR,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PIL,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_PSL,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_AIL,
    itk::SpatialOrientation::ITK_COORDINATE_ORIENTATION_ASL,
  };

void printDirections(const std::string &prompt,
                     const SO_AdapterType::DirectionType &dir)
{
  std::cerr << prompt << std::endl;
  for(unsigned ii=0; ii < 3; ii++)
    {
    if(ii > 0)
      std::cout << "    ";
    for(unsigned jj=0; jj < 3; jj++)
      {
      if(dir[ii][jj] >= 0)
        {
        std::cout << "| " << dir[ii][jj] << " | ";
        }
      else
        std::cout << "|" << dir[ii][jj] << " | ";
      }
    std::cout << std::endl;
    }
  std::cout << std::endl;
}


int itkQuaternionOrientationAdapterTest(int argc, char *argv[])
{

  for(unsigned i = 0; i < sizeof(allOrientations)/sizeof(SO_OrientationType); i++)
    {
    SO_OrientationType orient = allOrientations[i];
    std::cout << SO_OrientationToString(orient) << std::endl;
    SO_AdapterType::DirectionType dir;
    Q_AdapterType::DirectionType dir2;
    Q_OrientationType qorient;

    dir = SO_AdapterType().ToDirectionCosines(orient);
    qorient = Q_AdapterType().FromDirectionCosines(dir);
    dir2 = Q_AdapterType().ToDirectionCosines(qorient);

    printDirections("Before",dir);
    printDirections("After",dir2);
    for(unsigned ii = 0; ii < 3; ii++)
      {
      for(unsigned jj=0; jj < 3; jj++)
        {
        if(dir[ii][jj] != dir2[ii][jj])
          {
          std::cerr << "Input and output matrices don't match" << std::endl;
          return EXIT_FAILURE;
          }
        }
      }
    }
  return EXIT_SUCCESS;
}
