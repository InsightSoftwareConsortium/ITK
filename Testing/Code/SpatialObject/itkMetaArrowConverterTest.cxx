/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMetaArrowConverterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#include "itkMetaArrowConverter.h"
#include <iostream>

/**
 * This is a test file for the itkitkMetaArrowConverter class.
 */
int itkMetaArrowConverterTest(int ac, char* av[])
{
  
  // check number of arguments
  if (ac != 2)
    {
    std::cout << "Must specify output path as argument" << std::endl;
    return EXIT_FAILURE;
    }
  
  
  // typedefs
  typedef itk::ArrowSpatialObject<> SpatialObjectType;
  typedef itk::MetaArrowConverter<> ConverterType;
  
  // instantiate new converter and object (I don't think I'm supposed to do it like this in ITK!!!)
  ConverterType* converter = new ConverterType();
  
  
  //
  // create the test data
  //
  SpatialObjectType::Pointer itkArrow = SpatialObjectType::New();
  SpatialObjectType::VectorType direction;
  direction[0] = 0;
  direction[1] = 1;
  direction[2] = 2;
  SpatialObjectType::PointType position;
  position[0] = -1;
  position[1] = -2;
  position[2] = -3;
  float length = 2.3;
  itkArrow->SetDirection(direction);
  itkArrow->SetPosition(position);
  itkArrow->SetLength(length);
  
  
  
  //
  // test itk to metaArrow (assuming that Orientation should correspond to Direction)
  //
  MetaArrow* metaArrow = converter->ArrowSpatialObjectToMetaArrow(itkArrow);
  
  // check length
  double metaLength = metaArrow->Length();
  if (metaLength != length)
    {
    std::cout << "Conversion to MetaArrow failed to convert length [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  
  /*
  // check position
  const double* metaPosition = metaArrow->Position();
  if (metaPosition[0] != (double)position[0] || metaPosition[1] != (double)position[1] 
     || metaPosition[2] != (double)position[2])
    {
    std::cout << "Conversion to MetaArrow failed to convert position [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  
  // check direction
  const double* metaOrientation = metaArrow->Orientation();
  if (metaOrientation[0] != (double)direction[0] || metaOrientation[1] != (double)direction[1] 
     || metaOrientation[2] != (double)direction[2])
    {
    std::cout << "Conversion to MetaArrow failed to convert direction [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  */
  
  
  //
  // test metaArrow to itk (assuming that Orientation should correspond to Direction)
  //
  MetaArrow* newMetaArrow = new MetaArrow(3);
  newMetaArrow->Length((float)length);
  double newMetaPosition[3];
  newMetaPosition[0] = position[0];
  newMetaPosition[1] = position[1];
  newMetaPosition[2] = position[2];
  newMetaArrow->Position((const double*)newMetaPosition);
  double newMetaOrientation[3];
  newMetaOrientation[0] = position[0];
  newMetaOrientation[1] = position[1];
  newMetaOrientation[2] = position[2];
  newMetaArrow->Orientation((const double*)newMetaOrientation);
  
  SpatialObjectType::Pointer newItkArrow = converter->MetaArrowToArrowSpatialObject(newMetaArrow);
  
  // check length
  if (newItkArrow->GetLength() != newMetaArrow->Length())
    {
    std::cout << "Conversion to SpatialObject failed to convert length [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  
  /*
  // check position
  SpatialObjectType::PointType itkPosition = newItkArrow->GetPosition();
  if ((double)itkPosition[0] != newMetaPosition[0] || (double)itkPosition[1] != newMetaPosition[1] 
     || (double)itkPosition[2] != newMetaPosition[2])
    {
    std::cout << "Conversion to SpatialObject failed to convert position [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  
  // check direction
  SpatialObjectType::VectorType itkDirection = newItkArrow->GetDirection();
  if ((double)itkDirection[0] != newMetaOrientation[0] || (double)itkDirection[1] != newMetaOrientation[1] 
     || (double)itkDirection[2] != newMetaOrientation[2])
    {
    std::cout << "Conversion to SpatialObject failed to convert direction [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  */
  
  
  //
  // test writing
  //
  if (!converter->WriteMeta(itkArrow, av[1]))
    {
    std::cout << "Didn't write properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  
  
  
  //
  // test reading
  //
  SpatialObjectType::Pointer reLoad = converter->ReadMeta(av[1]);
  
  // check length
  if (reLoad->GetLength() != length)
    {
    std::cout << "Didn't read length properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "[PASSED]" << std::endl;
  
  /*
  // check direction  
  if (reLoad->GetDirection() != direction)
    {
    std::cout << "Didn't read direction properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  
  // check position  
  if (reLoad->GetPosition() != position)
    {
    std::cout << "Didn't read position properly [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  */
  
  
  
  // All tests executed successfully
  return EXIT_SUCCESS;
  
}












