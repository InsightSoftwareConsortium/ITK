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

#include <iostream>
#include "itkFEMFactoryBase.h"
#include "itkFEMElement2DC0LinearQuadrilateralMembrane.h"

//
int
itkFEMPArrayTest(int, char *[])
{
  // Need to register default FEM object types,
  // and setup SpatialReader to recognize FEM types
  // which is all currently done as a HACK in
  // the initialization of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  using ElementType = itk::fem::Element;
  using NodeType = ElementType::Node;
  using ArrayType = NodeType::ArrayType;
  using FEMPointer = itk::fem::FEMP<NodeType>;

  ArrayType array;

  auto                    n1 = NodeType::New();
  ElementType::VectorType pt(2);

  pt[0] = 0.;
  pt[1] = 0.;
  n1->SetCoordinates(pt);
  array.push_back(FEMPointer(n1));

  n1 = NodeType::New();
  pt[0] = 1.;
  pt[1] = 1.;
  n1->SetCoordinates(pt);
  array.push_back(FEMPointer(n1));

  n1 = NodeType::New();
  pt[0] = 3.;
  pt[1] = 2.;
  n1->SetCoordinates(pt);
  array.push_back(FEMPointer(n1));

  n1 = NodeType::New();
  pt[0] = 0.;
  pt[1] = 3.;
  n1->SetCoordinates(pt);
  array.push_back(FEMPointer(n1));

  array.Renumber();

  std::cout << "Nodes\n";

  try
  {
    array.Find(3);
    array.Find(1);
    array.Find(2);
    array.Find(0);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
  }

  // try an element with GN larger than the array size
  auto n2 = NodeType::New();
  pt[0] = 0.;
  pt[1] = 3.;
  n2->SetCoordinates(pt);

  // changes made - kiran
  // n1->GN = 200;
  n2->SetGlobalNumber(200);

  std::cout << "New Node " << n2->GetGlobalNumber() << std::endl;
  // changes made - kiran
  array.push_back(FEMPointer(n2));
  std::cout << "Node 0 " << array[0]->GetGlobalNumber() << std::endl;
  std::cout << "Node 1 " << array[1]->GetGlobalNumber() << std::endl;
  std::cout << "Node 2 " << array[2]->GetGlobalNumber() << std::endl;
  std::cout << "Node 3 " << array[3]->GetGlobalNumber() << std::endl;
  std::cout << "Node 4 " << array[4]->GetGlobalNumber() << std::endl;
  NodeType::Pointer node;

  try
  {
    node = array.Find(200);
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Exception caught: " << e << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    // Intentionally fail, by asking for a non-existing element
    node = array.Find(1000);
    std::cout << "Error: exception should have been thrown here... " << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cout << "Passed Exception test: " << e << std::endl;
  }

  // Use the node in order to avoid warning for unused variable
  ElementType::VectorType coordinates = node->GetCoordinates();

  std::cout << "Coordinates = " << std::endl;
  for (double coordinate : coordinates)
  {
    std::cout << coordinate << "  " << std::endl;
  }

  std::cout << "Test PASSED!\n";
  return EXIT_SUCCESS;
}
