/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAffineTransformTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include <iostream>

#include "itkAffineTransform.h"

int
main(
    int argc,
    char *argv[])
{
    int err = 0;       // Error detected in current test?
    int any = 0;       // Any errors detected in testing?
    vnl_matrix_fixed<double,2,2> matrix2;
    vnl_matrix_fixed<double,2,2> inverse2;
    vnl_vector_fixed<double,2>   vector2;
    int i, j;

    /* FIXME: This is still mostly an empty shell for testing */
    any = 1;
    std::cout << "The AffineTransform class is still being implemented"
              << std::endl;

    /* Create a 2D identity transformation and show its parameters */
    err = 0;
    itk::AffineTransform<double,2> id2;
    matrix2 = id2.GetMatrix();
    vector2 = id2.GetOffset();
    std::cout << "Matrix from instantiating an identity transform:"
              << std::endl << matrix2;
    std::cout << "Vector from instantiating an identity transform:"
              << std::endl << vector2 << std::endl;

    /* Create and show a simple 2D transform from given parameters */
    matrix2[0][0] = 1;
    matrix2[0][1] = 2;
    matrix2[1][0] = 3;
    matrix2[1][1] = 4;
    vector2[0] = 5;
    vector2[1] = 6;
    itk::AffineTransform<double,2> aff2(matrix2, vector2);
    for (i = 0; i < 2; i++) {
        for (j = 0; j < 2; j++)
            matrix2[i][j] = 0.0;
        vector2[i]    = 0.0;
    }
    std::cout << "Instantiation of a given 2D transform:" 
              << std::endl << aff2;
    inverse2 = aff2.GetInverse();
    std::cout << "Inverse matrix for the given transform:"
              << std::endl << inverse2;

    /* Set parameters of a 2D transform */
    matrix2[0][0] = 6;
    matrix2[0][1] = 5;
    matrix2[1][0] = 4;
    matrix2[1][1] = 3;
    vector2[0] = 2;
    vector2[1] = 1;
    aff2.SetMatrix(matrix2);
    aff2.SetOffset(vector2);
    for (i = 0; i < 2; i++) {
        for (j = 0; j < 2; j++)
            matrix2[i][j] = 0.0;
        vector2[i]    = 0.0;
    }
    matrix2 = aff2.GetMatrix();
    vector2 = aff2.GetOffset();
    std::cout << "Setting the matrix in an existing transform:"
              << std::endl << matrix2;
    std::cout << "Setting the offset in an existing  transform:"
              << std::endl << vector2 << std::endl;

    /* Try composition of two transformation */
    /* FIXME: Self with self isn't a very rigorous test */
    aff2.Compose(aff2);
    std::cout << "Result of a composition:"
              << std::endl << aff2;
    //matrix2 = aff2.GetMatrix();
    //vector2 = aff2.GetOffset();
    //std::cout << "Matrix of a composition:"
    //          << std::endl << matrix2;
    //std::cout << "Offset of a composition:"
    //          << std::endl << vector2 << std::endl;

    return any;
}
