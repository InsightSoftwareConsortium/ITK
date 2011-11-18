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

#include "itkFEMException.h"
#include "itkFEMFactoryBase.h"

#include <iostream>

int itkFEMExceptionTest(int, char *[])
{
  //Need to register default FEM object types,
  //and setup SpatialReader to recognize FEM types
  //which is all currently done as a HACK in
  //the initializaiton of the itk::FEMFactoryBase::GetFactory()
  itk::FEMFactoryBase::GetFactory()->RegisterDefaultTypes();

  try
    {
    throw itk::fem::FEMException(__FILE__, __LINE__, "itkFEMException");
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Exception caught\n";
    }

  try
    {
    throw itk::fem::FEMExceptionIO(__FILE__, __LINE__, "itkFEMExceptionIO", "IO exception");
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "IO exception caught\n";
    }

  try
    {
    throw itk::fem::FEMExceptionWrongClass(__FILE__, __LINE__, "itkFEMExceptionWrongClass");
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Wrong class exception caught\n";
    }

  try
    {
    throw itk::fem::FEMExceptionObjectNotFound(__FILE__, __LINE__, "itkFEMExceptionObjectNotFound", "baseClassName", 0);
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Not found exception caught\n";
    }

  try
    {
    throw itk::fem::FEMExceptionSolution(__FILE__, __LINE__, "itkFEMExceptionSolution", "Solution exception");
    }
  catch( itk::ExceptionObject & )
    {
    std::cout << "Solution exception caught\n";
    }

  std::cout << "Test PASSED!\n";
  return EXIT_SUCCESS;
}
