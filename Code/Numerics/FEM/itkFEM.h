/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEM.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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

#include "itkFEMSolver.h"
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
