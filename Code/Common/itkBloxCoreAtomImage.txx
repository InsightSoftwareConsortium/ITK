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

#include "itkBloxCoreAtomImage.h"

#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"

#include "vnl/vnl_matrix.h"

namespace itk
{

template <unsigned int dim>
BloxCoreAtomImage<dim>
::BloxCoreAtomImage()
{
  m_MedialNodeCount = 0;
  m_NodePointerList = new std::vector<BloxCoreAtomPixel<NDimensions>*>();
}

template <unsigned int dim>
BloxCoreAtomImage<dim>
::~BloxCoreAtomImage()
{
  delete m_NodePointerList;
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
  //cerr << "BloxCoreAtomImage::DoCoreAtomVoting()" << endl;
  
  // Iterator to access all pixels in the image
  ImageRegionIterator<Self> bloxIt = 
    ImageRegionIterator<Self>(this, this->GetLargestPossibleRegion() );

  // Pointer for accessing pixels
  BloxCoreAtomPixel<NDimensions>* pPixel = 0;

  // Results of eigenanalysis from each pixel
  typename BloxCoreAtomPixel<NDimensions>::EigenvalueType eigenvalues;
  typename BloxCoreAtomPixel<NDimensions>::EigenvectorType eigenvectors;

  // Results of eigenanalysis from each pixel
  typename BloxCoreAtomPixel<NDimensions>::EigenvalueType sf_eigenvalues;
  typename BloxCoreAtomPixel<NDimensions>::EigenvectorType sf_eigenvectors;

  unsigned int voterCount = 0;
  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    // Get a pointer to the pixel
    pPixel = &bloxIt.Value();

    // If there are no core atoms in this pixel, it doesn't get to vote
    if( pPixel->empty() )
      continue;

    //populate the NodePointerList
    m_NodePointerList->push_back(pPixel);

    voterCount++;

    // Get eigenanalysis results
    eigenvalues = pPixel->GetEigenvalues();
    eigenvectors = pPixel->GetEigenvectors();

    //std::cerr << "eigen values: " << eigenvalues[0] << " " << eigenvalues[1] << " " << eigenvalues[2] << std::endl;

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
      axisLengthArray[i] = ( (1 - eigenvalues[i]) / alphaOne) * axisLengthArray[0] ;
      }

    // Build the ellipsoid voting region
    typedef EllipsoidInteriorExteriorSpatialFunction<NDimensions, PositionType> VoteFunctionType;
    typename VoteFunctionType::Pointer ellipsoid = VoteFunctionType::New();

    // Create an iterator to traverse the ellipsoid region
    typedef FloodFilledSpatialFunctionConditionalIterator
      <Self, VoteFunctionType> ItType;

    // The seed position for the ellipsoid is the current pixel's index in data space
    // since this is always at the center of the voting ellipsoid
    typename Self::IndexType seedPos = bloxIt.GetIndex();
    
    // Figure out the center of the ellipsoid, which is the center
    // of the voting pixel
    typename VoteFunctionType::InputType centerPosition;

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
    ItType sfi = ItType(this, ellipsoid, seedPos);

    // Get the position of the voting blox
    typedef Point<double, NDimensions> TPosition;
    TPosition voterPosition;
    typename Self::IndexType voterIndex = bloxIt.GetIndex();
    this->TransformIndexToPhysicalPoint(voterIndex, voterPosition);

    int voteeCount = 0;

    sfi.SetCenterInclusionStrategy();

    // Iterate through the ellipsoid and cast votes
    for( sfi.GoToBegin(); !( sfi.IsAtEnd() ); ++sfi)
      {
      TPosition voteePosition;
      typename Self::IndexType voteeIndex = sfi.GetIndex();
      //std::cerr << "voteeIndex "<< voteeIndex << std::endl ;
      
      this->TransformIndexToPhysicalPoint(voteeIndex, voteePosition);

      // vector from voting blox to current votee
      typename TPosition::VectorType dbar = voterPosition - voteePosition;
      voteeCount ++;

      // The voting process and variables are explained in
      // IEEE TRANSACTIONS ON MEDICAL IMAGING, VOL. 18, NO. 10, OCTOBER 1999
      // page 1029 

      // The votee does not get voted for if it's empty
      if( sfi.Get().GetSize() == 0 )
        continue;

      // form the ellipsoidal distance de
      double de = 0;
      double sf_de_sqr = 0;

      for (int i = 0; i < NDimensions; i++)
        {
        de += pow((dot_product(eigenvectors.get_column(i), dbar.Get_vnl_vector() ) /
          axisLengthArray[i] ), 2);
        }

      de = sqrt(de);

      //printf("De = %f\n", de);

      double weight_factor = exp(-1.0*de*de);

      // vote strength
      double voteStrength = pPixel->size()*weight_factor;
      //printf("Vote strength = %f\n", voteStrength);
      //printf("weight_factor = %f\n", weight_factor);

      // Get eigenanalysis results
      sf_eigenvalues = sfi.Get().GetEigenvalues();
      sf_eigenvectors = sfi.Get().GetEigenvectors();

      for (int i = 0; i < NDimensions; i++)
        {
        sf_de_sqr += pow((dot_product(sf_eigenvectors.get_column(i), dbar.Get_vnl_vector() ) /
          axisLengthArray[i] ), 2);
        }

      //printf("sf_de = %f\n", sqrt(sf_de_sqr));

      //CALCULATE WEIGHT FACTOR FOR INDEX OF SPATIAL FUNCTION ITERATION
      double sf_weight_factor = exp(-1.0*sf_de_sqr);

      //HERE WE CALL CalcWeightedCoreAtomLocation using de to keep track of the weighted location of
      //each voted medial node based on constituent core atom locations
      // Reminder: sfi.Get() is the pixel being voted for
      // and pPixel is doing the voting
      sfi.Get().CalcWeightedCoreAtomLocation(sf_weight_factor, pPixel);

      // cast the vote
      sfi.Get().CollectVote(pPixel->GetRawCMatrixPointer(), voteStrength, pPixel->size() );
      
      } // end cast votes from this pixel

    //printf("Blox voted for %i other pixels\n", voteeCount);
    } // end cast votes from all pixels

  // The final task is to normalize all of the voted blox
  // and recompute the eigenanalysis on the new matrix
  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    (&bloxIt.Value())->NormalizeVotedCMatrix();
    (&bloxIt.Value())->DoVotedEigenanalysis();
    }

    m_MedialNodeCount = voterCount;
    //cerr << "MedialNodeCount = " << m_MedialNodeCount << endl;
}

template <unsigned int dim>
void
BloxCoreAtomImage<dim>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

    // Iterator to access all pixels in the image
  ImageRegionConstIterator<Self> bloxIt = 
    ImageRegionConstIterator<Self>(this, this->GetLargestPossibleRegion() );

  // Pointer for accessing pixels
  BloxCoreAtomPixel<NDimensions> pPixel;

  // Results of eigenanalysis from each pixel
  typename BloxCoreAtomPixel<NDimensions>::EigenvalueType eigenvalues;
  typename BloxCoreAtomPixel<NDimensions>::EigenvalueType veigenvalues;

  std::cerr << "Index\t# Core Atoms\tEigen Values\t\t\tMean CA Length\tVoted Eigen Values\n" 
            << "-----\t------------\t------------\t\t\t--------------\t------------------\n" << std::endl;

  int counter = 0;

  for(bloxIt.GoToBegin(); !bloxIt.IsAtEnd(); ++bloxIt)
    {
    // Get a pointer to the pixel
    pPixel = bloxIt.Value();

    eigenvalues = pPixel.GetEigenvalues();
    veigenvalues = pPixel.GetVotedEigenvalues();

    if(!pPixel.empty())
      {
      std::cerr << bloxIt.GetIndex() << "\t";
      std::cerr << pPixel.GetSize() << "\t";
      std::cerr << eigenvalues[0] << " " << eigenvalues[1] << " " << eigenvalues[2] << "\t";
      std::cerr << pPixel.GetMeanCoreAtomDiameter() << "\t\t";
      std::cerr << veigenvalues[0] << " " << veigenvalues[1] << " " << veigenvalues[2] << "\t" << std::endl;
      std::cerr << std::endl << "Node Pointer List: " << (*m_NodePointerList)[counter]->GetVotedLocation() << std::endl;
      counter++;
      }
    }  
  std::cerr << "Number of Medial Nodes: " << m_MedialNodeCount << std::endl;
  std::cerr << "Print Self Done" << std::endl;
}

} // end namespace itk

#endif
