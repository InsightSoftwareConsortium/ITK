/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElementTest.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include "itkFEM.h"
#include "itkFEMSolver.h"

#include "itkFEMLinearSystemWrappers.h"

#include "itkExceptionObject.h"

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

using namespace std;
using namespace itk;
using namespace fem;

int itkFEMElementTest(int, char* [] );
//void PrintResults(Solver&, int, char);
void PrintK(Solver&, int, char);
void PrintF(Solver&, int, char);
void PrintNodalCoordinates(Solver&, int, char);
void PrintU(Solver&, int, char);
