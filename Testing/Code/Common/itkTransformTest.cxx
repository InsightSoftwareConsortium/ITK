/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include <iostream>

#include "itkTransform.h"
#include "itkMatrix.h"

   

int main()
{


  typedef  itk::Matrix<double,3,3>   MatrixType;
  typedef  itk::Vector<double,3>     VectorType;
  typedef  itk::Point<double,3>      ParametersType;
  typedef  itk::Matrix<double,3,3>   JacobianType;

  typedef  itk::Transform<double,3,3,ParametersType,JacobianType> TransformType;

  TransformType::Pointer transform = TransformType::New();
    

  return EXIT_SUCCESS;

}
