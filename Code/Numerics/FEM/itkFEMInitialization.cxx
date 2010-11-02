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
#include "itkFEMInitialization.h"

namespace itk {
namespace fem {

unsigned int FEMInitialization::count = 0;

/**
 * \brief Register all Load implementations of all Element classes.
 */
extern void LoadImplementationsRegister(void);

/**
 * Constructor of the FEMInitialization class does all
 * the initialization.
 */
FEMInitialization::FEMInitialization()
{
  if ( 0 == count++)
  {
    // Perform initialization


    // Register all loads with the VisitorDispatcher class
    LoadImplementationsRegister();

  }
}

/**
 * Destructor of the FEMInitialization class does all
 * the cleanup required by the FEM library.
 */
FEMInitialization::~FEMInitialization()
{
  if ( 0 == --count)
    {
    // perform the cleanup and housekeeping
    }
}

}} // end namespace itk::fem
