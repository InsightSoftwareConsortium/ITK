/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxCoreAtomImage_txx
#define __itkBloxCoreAtomImage_txx

#include <iostream>
#include "itkBloxCoreAtomImage.h"

#include "itkImageRegionIterator.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

namespace itk
{

template <unsigned int dim>
BloxCoreAtomImage<dim>
::BloxCoreAtomImage()
{

}

template <unsigned int dim>
BloxCoreAtomImage<dim>
::~BloxCoreAtomImage()
{

}

template <unsigned int dim>
void
BloxCoreAtomImage<dim>
::DoEigenanalysis()
{
  itk::ImageRegionIterator<Self> bloxIt = 
    itk::ImageRegionIterator<Self>(this, this->GetLargestPossibleRegion() );

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
      ( &bloxIt.Value() )->DoCoreAtomEigenanalysis();
    }
}

template <unsigned int dim>
void
BloxCoreAtomImage<dim>
::DoCoreAtomVoting()
{
  // Iterator to access all pixels in the image
  ImageRegionIterator<Self> bloxIt = 
    ImageRegionIterator<Self>(this, this->GetLargestPossibleRegion() );

  // Pointer for accessing pixels
  BloxCoreAtomPixel<NDimensions>* pPixel = 0;

  // Results of eigenanalysis from each pixel
  BloxCoreAtomPixel<NDimensions>::TEigenvalueType eigenvalues;
  BloxCoreAtomPixel<NDimensions>::TEigenvectorType eigenvectors;

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    // Get a pointer to the pixel
    pPixel = &bloxIt.Value();

    // If there are no core atoms in this pixel, it doesn't get to vote
    if( pPixel->empty() )
      continue;

    // Get eigenanalysis results
    eigenvalues = pPixel->GetEigenvalues();
    eigenvectors = pPixel->GetEigenvectors();

    // Ellipsoid axis length array
    Point<double, NDimensions> axisLengthArray;

    // Compute first length
    axisLengthArray[0] = 0.5 * pPixel->GetMeanCoreAtomDiameter();

    // printf("Mean core atom diameter is %f\n", pPixel->GetMeanCoreAtomDiameter() );

    // Precompute alphaOne
    double alphaOne = 1 - eigenvalues[0];

    // Watch out for /0 problems
    if(alphaOne==0)
      alphaOne = 0.001;

    // Now compute the rest of the lengths
    for(int i = 1; i < NDimensions; i++)
      {
      axisLengthArray[i] = ( (1 - eigenvalues[i]) / alphaOne) * axisLengthArray[0];
      }

    // Dump the axis length vector
    for(int i = 0; i < NDimensions; i++)
      {
      printf("Axis length %i is %f\n", i, axisLengthArray[i]);
      }

    // Dump the ellipsoid orientation
    for(int i = 0; i < NDimensions; i++)
      {
      printf("Axis %i has orientation ", i);
      for(int j = 0; j < NDimensions; j++)
        {
        printf("%f ", eigenvectors.get_column(i)[j]);
        }
      printf("\n");
      }

 
    // Build the ellipsoid voting region
    typedef EllipsoidInteriorExteriorSpatialFunction<NDimensions, TPositionType> TVoteFunctionType;
    TVoteFunctionType::Pointer ellipsoid = TVoteFunctionType::New();

    // Create an iterator to traverse the ellipsoid region
    typedef FloodFilledSpatialFunctionConditionalIterator
      <Self, TVoteFunctionType> TItType;

    // The seed position for the ellipsoid is the current pixel's index in data space
    // since this is always at the center of the voting ellipsoid
    Self::IndexType seedPos = bloxIt.GetIndex();
    
    // Figure out the center of the ellipsoid, which is the center
    // of the voting pixel
    TVoteFunctionType::InputType centerPosition;

    ContinuousIndex<double, dim> contIndex;

    for(int i = 0; i < dim; i ++ )
      {
      contIndex[i] = (double)seedPos[i] + 0.5;
      }

    // Get the physical location of this center index
    this->TransformContinuousIndexToPhysicalPoint(contIndex, centerPosition);

    ellipsoid->SetCenter(centerPosition);
    ellipsoid->SetOrientations(eigenvectors);
    ellipsoid->SetAxes(axisLengthArray);
    
    // Instantiate the iterator
    TItType sfi = TItType(this, ellipsoid, seedPos);

    // Get the position of the voting blox
    typedef Point<double, NDimensions> TPosition;
    TPosition voterPosition;
    Self::IndexType voterIndex = bloxIt.GetIndex();
    this->TransformIndexToPhysicalPoint(voterIndex, voterPosition);

    int voteeCount = 0;

    sfi.SetCenterInclusionStrategy();

    // Iterate through the ellipsoid and cast votes
    for( sfi.GoToBegin(); !( sfi.IsAtEnd() ); ++sfi)
      {
      // The voting process and variables are explained in
      // IEEE TRANSACTIONS ON MEDICAL IMAGING, VOL. 18, NO. 10, OCTOBER 1999
      // page 1029 

      // The votee does not get voted for if it's empty
      if( sfi.Get().GetSize() == 0 )
        continue;

      voteeCount ++;

      // get the position of the current votee (the dereferenced sfi)
      TPosition voteePosition;
      Self::IndexType voteeIndex = sfi.GetIndex();
      this->TransformIndexToPhysicalPoint(voteeIndex, voteePosition);

      // vector from voting blox to current votee
      TPosition::VectorType dbar = voterPosition - voteePosition;

      // form the ellipsoidal distance de
      double de = 0;

      for (int i = 0; i < NDimensions; i++)
        {
        de += pow((dot_product(eigenvectors.get_column(i), dbar.Get_vnl_vector() ) /
          axisLengthArray[i] ), 2);
        }

      de = sqrt(de);

      printf("De = %f\n", de);

      // vote strength
      double voteStrength = pPixel->size()*exp(-1.0*de*de);
      printf("Vote strength = %f\n", voteStrength);

      // cast the vote
      sfi.Get().CollectVote(pPixel->GetRawCMatrixPointer(), de, pPixel->size() );
      } // end cast votes from this pixel

    printf("Blox voted for %i other pixels\n", voteeCount);
    } // end cast votes from all pixels

  // The final task is to normalize all of the voted blox
  // and recompute the eigenanalysis on the new matrix
  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    (&bloxIt.Value())->NormalizeVotedCMatrix();
    (&bloxIt.Value())->DoVotedEigenanalysis();
    }
}

template <unsigned int dim>
void
BloxCoreAtomImage<dim>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk

#endif
