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
#ifndef itkFEMElementTest_h
#define itkFEMElementTest_h
#include "itkFEMSolver.h"

#include "itkFEMLinearSystemWrappers.h"

#include "itkMacro.h"

#include <iostream>
#include <fstream>
#include <exception>

#define DEFAULT_COMMENT     '.'
#define MATLAB_COMMENT      '%'
#define IDL_COMMENT         ';'

// Only one of these _OUTPUT variables should be nonzero, otherwise
// things will become confusing!  If both are zero, no output will be
// generated.
#define MATLAB_OUTPUT       1
#define IDL_OUTPUT          0
#define DEBUG_FEM_TESTS     ( MATLAB_OUTPUT || IDL_OUTPUT )

int itkFEMElementTest(int, char * [] );

// void PrintResults(Solver&, int, char);
void PrintK( itk::fem::Solver &, int, char);

void PrintF( itk::fem::Solver &, int, char);

void PrintNodalCoordinates( itk::fem::Solver &, int, char);

void PrintU( itk::fem::Solver &, int, char);

#endif // itkFEMElementTest_h
