/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMInitialization.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
 */
class FEMInitialization
{
  static unsigned int count;
public:
  FEMInitialization();
  ~FEMInitialization();
};

/*
 * Trigger constructor and destructor calls in each compilation unit.
 * Unnamed namespace are used to avoid name collisions.
 */
namespace {
  static FEMInitialization FEMInitialization_var; 
};




}} // end namespace itk::fem

#endif // #ifndef __itkFEMInitialization_h
