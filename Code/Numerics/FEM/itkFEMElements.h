/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFEMElements.h
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
 * \file itkFEMElements.h
 * \brief Include all finite element classes defined in FEM toolkit.
 *
 * To make sure you have everything, just include this header file.
 */
#include "itkFEMElementBar2D.h"
#include "itkFEMElementBeam2D.h"
#include "itkFEMElementTriC02D.h"
#include "itkFEMElementQuadC02D.h"
#include "itkFEMElementMembraneC02D.h"
#include "itkFEMElementC1IsoCurve2D.h"
#include "itkFEMElementHexahedronC03D.h"
#include "itkFEMElementTetrahedronC03D.h"
#include "itkFEMElement2DC0LinearQuadrilateralStress.h"
#include "itkFEMElement3DC0LinearHexahedronStrain.h"
#include "itkFEMElement3DC0LinearTetrahedronStrain.h"
