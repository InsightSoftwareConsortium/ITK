/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkPointSetToSpatialObjectDemonsRegistrationTest.cxx
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

#include "itkMesh.h"
#include "itkEllipseSpatialObject.h"
#include "itkPointSetToSpatialObjectDemonsRegistration.h"

#include "itkRegularSphereMeshSource.h"


/** test */
int itkPointSetToSpatialObjectDemonsRegistrationTest(int, char* [] )
{
  const unsigned int Dimension = 3;

  typedef itk::EllipseSpatialObject< Dimension > EllipseType;

  // Create a ellipse.
  EllipseType::Pointer ellipse = EllipseType::New();

  // Set the radius
  ellipse->SetRadius( 50 );

  // Set its position
  EllipseType::TransformType::OffsetType offset;
  offset[0]=50;
  offset[1]=50;
  offset[1]=50;

  ellipse->ComputeObjectToWorldTransform();

  typedef itk::Mesh< float, Dimension >  PointSetType;

  typedef itk::RegularSphereMeshSource< 
                                 PointSetType > SphereType;

  SphereType::Pointer sphereSource = SphereType::New();

  sphereSource->Update();

    
  typedef itk::PointSetToSpatialObjectDemonsRegistration<
                                      PointSetType,
                                      EllipseType
                                        > DemonsRegistrationType;

  DemonsRegistrationType::Pointer  demonsRegistration = DemonsRegistrationType::New();

  demonsRegistration->SetFixedPointSet( sphereSource->GetOutput() );
  demonsRegistration->SetMovingSpatialObject( ellipse );


  try
    {
    demonsRegistration->StartRegistration();
    }
  catch( itk::ExceptionObject & excp )
    { 
    std::cerr << "Exception thrown during the registration process" << std::endl;
    std::cerr << excp << std::endl;
    }


  std::cout<<"Test Succeed!"<<std::endl;
  return EXIT_SUCCESS;

}

