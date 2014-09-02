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

#include "itkEllipseSpatialObject.h"
#include "itkPointSetToSpatialObjectDemonsRegistration.h"

#include "itkRegularSphereMeshSource.h"


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
  offset[0] = 50;
  offset[1] = 50;
  offset[2] = 50;

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
    demonsRegistration->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown during the registration process" << std::endl;
    std::cerr << excp << std::endl;
    }


  std::cout<<"Test Succeed!"<<std::endl;
  return EXIT_SUCCESS;

}
