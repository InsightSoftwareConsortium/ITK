/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTranslationTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <iostream>

#include "itkTranslationTransform.h"
#include "itkPhysicalImage.h"
#include "vnl/vnl_vector_fixed.h"

typedef  itk::Vector<double,2>     VectorType;


void PrintVector( const VectorType & v )
{
  for( unsigned int i=0; i<VectorType::VectorDimension; i++)
  {
    std::cout << v[i] << ", ";
  }
  std::cout << std::endl;
}


int main(
    int argc,
    char *argv[])
{


    int any = 0;       // Any errors detected in testing?

    VectorType                   vector2;

    int i;

    /* FIXME: This code exercises most of the methods but doesn't
       actually check that the results are correct. */
    any = 0;
    std::cout << "The TranslationTransform class is still being implemented"
              << std::endl;

    /* Create a 2D identity transformation and show its parameters */
    itk::TranslationTransform<double,2> id2;
    vector2 = id2.GetOffset();
        std::cout << "Vector from instantiating an identity transform:"
              << std::endl;
    PrintVector( vector2 );
    
    /* Create and show a simple 2D transform from given parameters */
    vector2[0] = 5;
    vector2[1] = 6;
    itk::TranslationTransform<double,2> aff2(vector2);
    for (i = 0; i < 2; i++) 
    {
        vector2[i]    = 0.0;
    }
    std::cout << "Instantiation of a given 2D transform:" 
              << std::endl << aff2;

    itk::TranslationTransform<double,2> inverse2;
    inverse2 = aff2.Inverse();
    std::cout << "Inverse of the given transform:"
              << std::endl << inverse2;

    /* Set parameters of a 2D transform */
    vector2[0] = 2;
    vector2[1] = 1;
    aff2.SetOffset(vector2);
    for (i = 0; i < 2; i++) 
    {
        vector2[i]    = 0.0;
    }
   
    vector2 = aff2.GetOffset();
    std::cout << "Setting the offset in an existing  transform:"
              << std::endl;
    PrintVector( vector2 );

    /* Try composition of two transformations */
    aff2.Compose(aff2);
    std::cout << "Result of a composition:"
              << std::endl << aff2;

    /* Compose with a translation */
    VectorType trans;
    trans[0] = 1;
    trans[1] = 2;
    aff2.Translate(trans);
    std::cout << "Result of a translation:"
              << std::endl << aff2;

        /* Transform a point */
    itk::Point<double, 2> u2, v2;
    u2[0] = 3;
    u2[1] = 5;
    v2 = aff2.Transform(u2);
    std::cout << "Transform a point:" << std::endl
              << v2[0] << " , " << v2[1] << std::endl;

    /* Back transform a point */
    v2 = aff2.BackTransform(u2);
    std::cout << "Back transform a point:" << std::endl
              << v2[0] << " , " << v2[1] << std::endl;

    /* Transform a vnl_vector */
    vnl_vector_fixed<double, 2> x2, y2;
    x2[0] = 1;
    x2[1] = 2;
    y2 = aff2.Transform(x2);
    std::cout << "Transform a vnl_vector:" << std::endl
              << y2[0] << " , " << y2[1] << std::endl;

    /* Back transform a vector */
    y2 = aff2.BackTransform(x2);
    std::cout << "Back transform a vnl_vector:" << std::endl
              << y2[0] << " , " << y2[1] << std::endl;

    /* Transform a vector */
    itk::Vector<double, 2> u3, v3;
    u3[0] = 3;
    u3[1] = 5;
    v3 = aff2.Transform(u3);
    std::cout << "Transform a vector:" << std::endl
              << v3[0] << " , " << v3[1] << std::endl;

    /* Back transform a vector */
    v3 = aff2.BackTransform(u3);
    std::cout << "Back transform a vector :" << std::endl
              << v3[0] << " , " << v3[1] << std::endl;

    /* Transform a Covariant vector */
    itk::Vector<double, 2> u4, v4;
    u4[0] = 3;
    u4[1] = 5;
    v4 = aff2.Transform(u4);
    std::cout << "Transform a Covariant vector:" << std::endl
              << v4[0] << " , " << v4[1] << std::endl;

    /* Back transform a vector */
    v4 = aff2.BackTransform(u4);
    std::cout << "Back transform a vector :" << std::endl
              << v4[0] << " , " << v4[1] << std::endl;

    return any;
}
