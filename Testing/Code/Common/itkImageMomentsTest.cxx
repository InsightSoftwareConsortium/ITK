/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMomentsTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include "itkImage.h"
#include "itkIndex.h"

#include "itkImageMoments.h"

int 
main(
    int argc,
    char *argv[])
{
    /* Define the image size and physical coordinates */
    unsigned long size[3] = {20, 40, 80};
    float origin [3] = { 0.5,   0.5,   0.5};
    float spacing[3] = { 0.1,   0.05 , 0.025};

    /* Define positions of the test masses in index coordinates */
    unsigned short mass = 1;           // Test mass
    long point[8][3] = {
	{ 10+8, 20+12, 40+0},
	{ 10-8, 20-12, 40-0},
	{ 10+3, 20-8,  40+0},
	{ 10-3, 20+8,  40-0},
	{ 10+0, 20+0,  40+10},
	{ 10-0, 20-0,  40-10},
    };

    /* Define the expected results for comparison */
    double tm = 6.0;                       // Total mass
    double gcd[3] = {1.5,   1.5, 1.5 };    // Center of gravity
    double pmd[3] = {0.125, 0.5, 2.0 };    // Principal moments
    double pad[3][3] = {                   // Principal axes
	{ 0.0,  0.0, 1.0},
	{ 0.6, -0.8, 0.0},
	{ 0.8,  0.6, 0.0},
    };
    vnl_vector_fixed<double,3> gc;
    gc.set(gcd);
    vnl_vector_fixed<double,3> pm;
    pm.set(pmd);
    vnl_matrix_fixed<double,3,3> pa;
    pa.set((double *)pad);

    /* Allocate a simple test image */
    itk::Image<unsigned short, 3>::Pointer
	image = itk::Image<unsigned short,3>::New();
    image->SetSize(size);

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    /* Set a few mass points within the image */
    /* FIXME: The method used here to set the points is rather klutzy,
       but appears to be the only method currently supported. */
    itk::Index<3> index;          /* Index over pixels */
    for ( int i = 0; i < 6; i++) {
	index.SetIndex(point[i]);
	image->SetPixel(index, mass);
    }

    /* Compute the moments */
    itk::ImageMoments<unsigned short, 3>
	moments(image);

    /* Report the various non-central moments */
    // FIXME:  Indentation is not handled correctly in matrix output
    cout << "\nTotal mass = " << moments.GetTotalMass() << "\n";
    cout << "Correct total mass = " << tm << "\n";
    cout << "\nFirst moments about index origin =\n";
    cout << "   " << moments.GetFirstMoments() << "\n";
    cout << "\nSecond moments about index origin =\n";
    cout << "   " << moments.GetSecondMoments() << "\n";

    /* Report the center of gravity and central moments */
    cout << "\nCenter of gravity =\n";
    cout << "   " << moments.GetCenterOfGravity() << "\n";
    cout << "Correct center of gravity =\n";
    cout << "   " << gc << "\n";
    cout << "\nSecond central moments =\n";
    cout << "   " << moments.GetCentralMoments() << "\n";

    /* Report principal moments and axes */
    cout << "\nPrincipal moments = \n";
    cout << "   " << moments.GetPrincipalMoments() << "\n";
    cout << "Correct principal moments = \n";
    cout << "   " << pm << "\n";
    cout << "\nPrincipal axes = \n";
    cout << "   " << moments.GetPrincipalAxes() << "\n";
    cout << "Correct principal axes = \n";
    cout << "   " << pa << "\n";

    /* FIXME:  Automatically compare computed to expected values */
    return 0;
}
