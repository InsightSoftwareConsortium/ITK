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
#ifndef __itkFEM_h
#define __itkFEM_h
/**
 * \file itkFEM.h
 * \brief Master include file for FEM toolkit.
 *
 * Include this file to make sure that all the necessary includes are
 * in place when using FEM classes.
 */
#include "itkFEMElements.h"
#include "itkFEMLoads.h"
#include "itkFEMMaterials.h"

#include "itkFEMSolverHyperbolic.h"
#include "itkFEMSolverCrankNicolson.h"

#include "itkFEMObjectFactory.h"
#include "itkFEMUtility.h"

#include "itkFEMException.h"

#include "itkFEMGenerateMesh.h"


// Perform the initialization of the library when this header is included
#include "itkFEMInitialization.h"

/**
 * \namespace itk::fem
 * \brief Contains finite element modeling (FEM) classes and support routines.
 */

#endif
