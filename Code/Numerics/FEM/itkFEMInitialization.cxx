/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMInitialization.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

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
