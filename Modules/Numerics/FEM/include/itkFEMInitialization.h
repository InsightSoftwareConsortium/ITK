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
#ifndef __itkFEMInitialization_h
#define __itkFEMInitialization_h

/**
 * \file itkFEMInitialization.h
 * \brief Initialization routines required by FEM library.
 *
 * This header needs to be included by all other header files that
 * depend on the library being initialized.
 */

namespace itk {
namespace fem {

/**
 * \class FEMInitialization
 * \brief FEM Library initialization and housekeeping.
 *
 * Construction of FEMInitialization class is triggered whenever
 * FEM library is linked to a program. Before the library can
 * be used, some initialization must be performed. This is
 * done in a constructor of FEMInitialization class.
 * \ingroup ITK-FEM
 */
class FEMInitialization
{
  static unsigned int count;
public:
  FEMInitialization();
  ~FEMInitialization();
};

/**
 * Trigger constructor and destructor calls in each compilation unit.
 * Unnamed namespace are used to avoid name collisions.
 */
namespace {
static FEMInitialization FEMInitialization_var;
}

}} // end namespace itk::fem

#endif // #ifndef __itkFEMInitialization_h
