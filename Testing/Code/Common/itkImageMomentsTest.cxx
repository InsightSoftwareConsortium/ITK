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
    /* Define acceptable (absolute) error in computed results */
    /* FIXME:  There's no theory to back up this choice of limit. */
    double maxerr = 1.0e-7;

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

    /* Define the expected (true) results for comparison */
    double ttm = 6.0;                      // Total mass
    double cgd[3] = {1.5,   1.5, 1.5 };    // Center of gravity
    double pmd[3] = {0.125, 0.5, 2.0 };    // Principal moments
    double pad[3][3] = {                   // Principal axes
	{ 0.0,  0.0, 1.0},
	{ 0.6, -0.8, 0.0},
	{ 0.8,  0.6, 0.0},
    };
    vnl_vector_fixed<double,3> tcg;
    tcg.set(cgd);
    vnl_vector_fixed<double,3> tpm;
    tpm.set(pmd);
    vnl_matrix_fixed<double,3,3> tpa;
    tpa.set((double *)pad);

    /* Allocate a simple test image */
    itk::Image<unsigned short, 3>::Pointer
	image = itk::Image<unsigned short,3>::New();
    image->SetSize(size);

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);

    /* Set a few mass points within the image */
    /* FIXME: The method used here to set the points is klutzy,
       but appears to be the only method currently supported. */
    itk::Index<3> index;          /* Index over pixels */
    for ( int i = 0; i < 6; i++) {
	index.SetIndex(point[i]);
	image->SetPixel(index, mass);
    }

    /* Compute the moments */
    itk::ImageMoments<unsigned short, 3>
	moments(image);
    double ctm = moments.GetTotalMass();
    vnl_vector_fixed<double,3>
        ccg = moments.GetCenterOfGravity();
    vnl_vector_fixed<double,3>
        cpm = moments.GetPrincipalMoments();
    vnl_matrix_fixed<double,3,3>
        cpa = moments.GetPrincipalAxes();

    /* Report the various non-central moments */
    // FIXME:  Indentation is not handled correctly in matrix output
    cout << "\nTotal mass = " << ctm << "\n";
    cout << "True total mass = " << ttm << "\n";
    cout << "\nFirst moments about index origin =\n";
    cout << "   " << moments.GetFirstMoments() << "\n";
    cout << "\nSecond moments about index origin =\n";
    cout << "   " << moments.GetSecondMoments() << "\n";

    /* Report the center of gravity and central moments */
    cout << "\nCenter of gravity =\n";
    cout << "   " << ccg << "\n";
    cout << "True center of gravity =\n";
    cout << "   " << tcg << "\n";
    cout << "\nSecond central moments =\n";
    cout << "   " << moments.GetCentralMoments() << "\n";

    /* Report principal moments and axes */
    cout << "\nPrincipal moments = \n";
    cout << "   " << cpm << "\n";
    cout << "True principal moments = \n";
    cout << "   " << tpm << "\n";
    cout << "\nPrincipal axes = \n";
    cout << "   " << cpa << "\n";
    cout << "True principal axes = \n";
    cout << "   " << tpa << "\n";

    /* Compute and report max abs error in computed */
    double tmerr = abs(ttm - ctm);  // Error in total mass
    double cgerr = 0.0;             // Error in center of gravity
    double pmerr = 0.0;             // Error in moments
    double paerr = 0.0;             // Error in axes
    for ( int i = 0; i < 3; ++i) {
        if ( abs(ccg[i] - tcg[i]) > cgerr )
            cgerr = abs(ccg[i] - tcg[i]);
        if ( abs(cpm[i] - tpm[i]) > pmerr )
            pmerr = abs(cpm[i] - tpm[i]);
        for (int j = 0; j < 3; ++j) {
            if (abs(cpa[i][j] - tpa[i][j]) > paerr)
                paerr = abs(cpa[i][j] - tpa[i][j]);
        }
    }
    cout << "\nMaximum absolute errors in:\n";
    cout << "   Total mass        = " << tmerr << "\n";
    cout << "   Center of gravity = " << cgerr << "\n";
    cout << "   Principal moments = " << pmerr << "\n";
    cout << "   Principal axes    = " << paerr << "\n";

    /* Return error if differences are too large */
    int stat = 
        tmerr > maxerr || cgerr > maxerr ||
        pmerr > maxerr || paerr > maxerr;
    if (stat)
        cout << "\n*** Errors are larger than defined maximum value.\n";
    else
        cout << "\n*** Errors are acceptable.\n";
    return stat;
}
