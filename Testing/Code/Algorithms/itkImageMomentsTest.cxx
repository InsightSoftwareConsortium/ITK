/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMomentsTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkAffineTransform.h"
#include "itkImage.h"
#include "itkImageMomentsCalculator.h"


typedef unsigned short         PixelType;
typedef itk::Vector<double,3>  VectorType;
typedef itk::Matrix<double,3>  MatrixType;
typedef itk::Image<PixelType, 3> ImageType;
typedef itk::ImageMomentsCalculator<ImageType> CalculatorType;
typedef CalculatorType::AffineTransformType AffineTransformType;


int 
itkImageMomentsTest( int itkNotUsed(argc), char ** itkNotUsed(argv) )
{
    /* Define acceptable (absolute) error in computed results.
       All the calculations are done in double and are well-conditioned,
       so we should be able to get within a few epsilon of the right
       values.  So choose maxerr to be 10*epsilon for IEEE 754 double.
       FIXME: For some reason as yet undetermined, the Intel compiler 
       produces results that are off by 12*epsilon.  This is still
       reasonably close but might deserve investigation some day when all
       the worse problems have been fixed. */
    double maxerr = 1.9e-15;

    /* Define the image size and physical coordinates */
    itk::Size<3> size = {{20, 40, 80}};
    double origin [3] = { 0.5,   0.5,   0.5};
    double spacing[3] = { 0.1,   0.05 , 0.025};

    /* Define positions of the test masses in index coordinates */
    unsigned short mass = 1;           // Test mass
    itk::Index<3>::IndexValueType point[8][3] = {
  { 10+8, 20+12, 40+0},
  { 10-8, 20-12, 40-0},
  { 10+3, 20-8,  40+0},
  { 10-3, 20+8,  40-0},
  { 10+0, 20+0,  40+10},
  { 10-0, 20-0,  40-10},
    };

    /* Define the expected (true) results for comparison */
    double ttm = 6.0;                      // Total mass
    double pad[3][3] = {                   // Principal axes
      { 0.0,  0.0, 1.0},
      { 0.6, -0.8, 0.0},
      { 0.8,  0.6, 0.0},
    };

    VectorType tcg;
    tcg[0] = 1.5;
    tcg[1] = 1.5;
    tcg[2] = 1.5;

    VectorType tpm;
    tpm[0] = 0.125;
    tpm[1] = 0.5;
    tpm[2] = 2.0;    // Principal moments

    MatrixType tpa;
    tpa.GetVnlMatrix().set((double *)pad);

    /* Allocate a simple test image */
    ImageType::Pointer image = ImageType::New();

    ImageType::RegionType region;
    region.SetSize(size);
    image->SetLargestPossibleRegion(region);
    image->SetBufferedRegion(region);
    image->SetRequestedRegion(region);

    /* Set origin and spacing of physical coordinates */
    image->SetOrigin(origin);
    image->SetSpacing(spacing);
    image->Allocate();
    
    image->FillBuffer( itk::NumericTraits<PixelType>::Zero );

    /* Set a few mass points within the image */
    /* FIXME: The method used here to set the points is klutzy,
       but appears to be the only method currently supported. */
    itk::Index<3> index;          /* Index over pixels */
    for ( int i = 0; i < 6; i++) 
    {
      index.SetIndex(point[i]);
      image->SetPixel(index, mass);
    }

    /* Compute the moments */
    CalculatorType moments(image);
    double ctm = moments.GetTotalMass();
    VectorType ccg = moments.GetCenterOfGravity();
    VectorType cpm = moments.GetPrincipalMoments();
    MatrixType cpa = moments.GetPrincipalAxes();

    /* Report the various non-central moments */
    // FIXME:  Indentation is not handled correctly in matrix output
    std::cout << "\nTotal mass = " << ctm << std::endl;
    std::cout << "True total mass = " << ttm << std::endl;
    std::cout << "\nFirst moments about index origin =\n";
    std::cout << "   " <<  moments.GetFirstMoments() << std::endl;
    std::cout << "\nSecond moments about index origin =\n";
    std::cout << "   " << moments.GetSecondMoments() << std::endl;

    /* Report the center of gravity and central moments */
    std::cout << "\nCenter of gravity =\n";
    std::cout << "   " << ccg << "\n";
    std::cout << "True center of gravity =\n";
    std::cout << "   " << tcg << "\n";
    std::cout << "\nSecond central moments =\n";
    std::cout << "   " << moments.GetCentralMoments() << "\n";

    /* Report principal moments and axes */
    std::cout << "\nPrincipal moments = \n";
    std::cout << "   " << cpm << "\n";
    std::cout << "True principal moments = \n";
    std::cout << "   " << tpm << "\n";
    std::cout << "\nPrincipal axes = \n";
    std::cout << "   " << cpa << "\n";
    std::cout << "True principal axes = \n";
    std::cout << "   " << tpa << "\n";

    /* Compute transforms between principal and physical axes */
    /* FIXME: Automatically check correctness of these results? */
    AffineTransformType::Pointer
        pa2p = moments.GetPrincipalAxesToPhysicalAxesTransform();
    std::cout << "\nPrincipal axes to physical axes transform:\n";
    std::cout << pa2p->GetMatrix() << std::endl;
    AffineTransformType::Pointer
        p2pa = moments.GetPhysicalAxesToPrincipalAxesTransform();
    std::cout << "\nPhysical axes to principal axes transform:\n";
    std::cout << p2pa->GetMatrix() << std::endl;

    /* Do some error checking on the transforms */
    double dist = pa2p->Metric( pa2p );
    std::cout << "Distance from self to self = " << dist << std::endl;
    AffineTransformType::Pointer p2pa2p = AffineTransformType::New();
    p2pa2p->Compose( p2pa );
    p2pa2p->Compose( pa2p );
    double trerr = p2pa2p->Metric();
    std::cout << "Distance from composition to identity = ";
    std::cout << trerr << std::endl;


    /* Compute and report max abs error in computed */
    double tmerr = fabs(ttm - ctm);  // Error in total mass
    double cgerr = 0.0;             // Error in center of gravity
    double pmerr = 0.0;             // Error in moments
    double paerr = 0.0;             // Error in axes

    for ( int i = 0; i < 3; ++i) 
    {
      if ( fabs(ccg[i] - tcg[i]) > cgerr ) 
      {
        cgerr = fabs(ccg[i] - tcg[i]);
      }
      if ( fabs(cpm[i] - tpm[i]) > pmerr ) 
      {
        pmerr = fabs(cpm[i] - tpm[i]);
      }
      for (int j = 0; j < 3; ++j) 
      {
        if ( fabs(cpa[i][j] - tpa[i][j]) > paerr)
        {
          paerr = fabs(cpa[i][j] - tpa[i][j]);
        }
      }
    }

    std::cout << "\nErrors found in:\n";
    std::cout << "   Total mass        = " << tmerr << std::endl;
    std::cout << "   Center of gravity = " << cgerr << std::endl;
    std::cout << "   Principal moments = " << pmerr << std::endl;
    std::cout << "   Principal axes    = " << paerr << std::endl;
    std::cout << "   Transformations   = " << trerr << std::endl;

    /* Return error if differences are too large */
    int stat = 
        tmerr > maxerr || cgerr > maxerr ||
        pmerr > maxerr || paerr > maxerr ||
        trerr > maxerr;

    std::cout << std::endl;
    bool pass = false; 
    if (stat)
      {
      std::cout << "Errors are larger than defined maximum value." << std::endl;
      std::cout << "Test FAILED !" << std::endl;
      pass = false;
      }
    else
      {
      std::cout << "Errors are acceptable" << std::endl;
      std::cout << "Test PASSED !" << std::endl;
      pass = true;
      }

    if( !pass )
      {
      return EXIT_FAILURE;
      }

    return EXIT_SUCCESS;

}


