/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomPixel.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxCoreAtomPixel_txx
#define __itkBloxCoreAtomPixel_txx

#include "itkBloxCoreAtomPixel.h"

namespace itk
{

template <unsigned int NDimensions>
BloxCoreAtomPixel<NDimensions>
::BloxCoreAtomPixel()
{
  m_RawCMatrix.fill(0);
  m_Eigenvalues.fill(0.0);
  m_Eigenvectors.fill(0.0);

  m_VotedCMatrix.fill(0);
  m_VotedEigenvalues.fill(0.0);
  m_VotedEigenvectors.fill(0.0);
  
  m_MeanCoreAtomDiameter = 0;
  m_ConstituencySize = 0;

  m_WeightSum = 0;

  m_MeanCoreAtomIntensity = 0.0;

  m_LocationSums[0] = 0;
  m_LocationSums[1] = 0;
  m_LocationSums[2] = 0;
}

template <unsigned int NDimensions>
BloxCoreAtomPixel<NDimensions>
::~BloxCoreAtomPixel()
{
  // The default destructor walks the pixel and deletes all bloxitems
}


//REMOVED: Boundary points dont know their intensities...profiles do!
template <unsigned int NDimensions>
void 
BloxCoreAtomPixel<NDimensions>
::CalcMeanCoreAtomIntensity()
{

/*
  double temp_intensity = 0.0;
  int num_core_atoms = 0;

  itk::BloxCoreAtomPixel<NDimensions>::iterator bpiterator;

  for (bpiterator = this->begin(); bpiterator != this->end(); ++bpiterator)
    {
    // Get the pointer of the core atom
    CoreAtomItemType* pCoreAtom = *bpiterator;

    //get mean intensity for this core atom
    temp_intensity = (pCoreAtom->GetBoundaryPointA()->GetValue() + pCoreAtom->GetBoundaryPointB()->GetValue())/2

    m_MeanCoreAtomIntensity += temp_intensity;
    num_core_atoms++;
    }
  m_MeanCoreAtomIntensity /= num_core_atoms;
*/
}

template <unsigned int NDimensions>
void 
BloxCoreAtomPixel<NDimensions>
::CalcWeightedCoreAtomLocation(double weight_factor, Self * votingPixel)
{
  // The iterator for accessing linked list info
  typename itk::BloxCoreAtomPixel<NDimensions>::iterator bpiterator;
  
  PositionType center;

  // Walk through all of the items in the voting pixel
  for (bpiterator = votingPixel->begin(); bpiterator != votingPixel->end(); ++bpiterator)
    {
    // Get the pointer of the core atom
    CoreAtomItemType* pCoreAtom = *bpiterator;

    // Get the center of the core atom
    center = pCoreAtom->GetCenterPosition();

    m_LocationSums[0] += (center[0]*weight_factor);
    m_LocationSums[1] += (center[1]*weight_factor);
    m_LocationSums[2] += (center[2]*weight_factor);

    m_WeightSum += weight_factor;
    }
}

template <unsigned int NDimensions>
double 
BloxCoreAtomPixel<NDimensions>
::CalcMeanCoreAtomDiameter()
{
  // Returns a mean of 0 if there are no core atoms present
  if( this->empty() )
    {
    return 0;
    }

  unsigned long int numCoreAtoms = 0;
  m_MeanCoreAtomDiameter = 0;

  // The iterator for accessing linked list info
  typename itk::BloxCoreAtomPixel<NDimensions>::iterator bpiterator;
  
  // Walk through all of the items at the pixel
  for (bpiterator = this->begin(); bpiterator != this->end(); ++bpiterator)
    {
    // Get the pointer of the core atom
    CoreAtomItemType* pCoreAtom = *bpiterator;

    m_MeanCoreAtomDiameter += pCoreAtom->GetDiameter();
    
    numCoreAtoms++;
    }

  if(numCoreAtoms>0) // Check for /0 to be safe
    m_MeanCoreAtomDiameter /= numCoreAtoms;
  else
    m_MeanCoreAtomDiameter = 0;

  return m_MeanCoreAtomDiameter;
}

template <unsigned int NDimensions>
bool
BloxCoreAtomPixel<NDimensions>
::DoCoreAtomEigenanalysis()
{
  // Don't attemp Eigenanalysis on an empty blox
  if( this->empty() )
    {
    return false;
    }

  // The iterator for accessing linked list info
  typename itk::BloxCoreAtomPixel<NDimensions>::iterator bpiterator;

  // The number of items stored in the pixel
  unsigned long int numItems = 0;

  // Walk through all of the items at the pixel
  for (bpiterator = this->begin(); bpiterator != this->end(); ++bpiterator)
    {
    // Get the pointer of the core atom
    CoreAtomItemType* pCoreAtom = *bpiterator;

    // Get the boundary points
    BPItemType* pBPOne = pCoreAtom->GetBoundaryPointA();
    BPItemType* pBPTwo = pCoreAtom->GetBoundaryPointB();
    
    // Get the physical positions of the two boundary points
    VectorType P1;
    P1 = pBPOne->GetPhysicalPosition().Get_vnl_vector();
    
    VectorType P2;
    P2 = pBPTwo->GetPhysicalPosition().Get_vnl_vector();

    // Figure out the "C" vector of the core atom
    VectorType cVector = P2 - P1;
    cVector.normalize();

    // Now, add to m_RawCMatrix
    for(unsigned int r = 0; r < NDimensions; r++) // row loop
      {
      for(unsigned int c = 0; c < NDimensions; c++) // column loop
        {
        m_RawCMatrix(r,c) += cVector[c]*cVector[r];
        } // end column loop
      } // end row loop

    numItems++;

    } // end walk all of the items in the pixel

  // Divide through by the number of items
  m_RawCMatrix /= numItems;

  // Create an identity matrix of size n
  vnl_matrix_fixed<double, NDimensions, NDimensions> identMatrix;
  identMatrix.fill(0);

  // Set the diagonal to 1
  for(unsigned int n = 0; n < NDimensions; n++) // row loop
    {
    identMatrix(n,n) = 1.0;
    } // end row loop

  // Do eigen analysis
  vnl_generalized_eigensystem* pEigenSys = new vnl_generalized_eigensystem(m_RawCMatrix, identMatrix);

  // Now, store the results
  
  // First the eigenvectors
  m_Eigenvectors = pEigenSys->V;

  // Now the eigenvalues (stored as a vector to save space)
  for(unsigned int i = 0; i < NDimensions; i++)
    {
    m_Eigenvalues[i] = pEigenSys->D(i,i);
    }

  delete pEigenSys;
  return true;
}

template <unsigned int NDimensions>
typename BloxCoreAtomPixel<NDimensions>::PositionType
BloxCoreAtomPixel<NDimensions>
::GetVotedLocation()
{
  m_VotedLocation[0] = m_LocationSums[0] / m_WeightSum;
  m_VotedLocation[1] = m_LocationSums[1] / m_WeightSum;
  m_VotedLocation[2] = m_LocationSums[2] / m_WeightSum;

  return m_VotedLocation;
}

template <unsigned int NDimensions>
void
BloxCoreAtomPixel<NDimensions>
::CollectVote(MatrixType* pMatrix, double strength, double count)
{
  m_VotedCMatrix += (*pMatrix)*strength;
  m_ConstituencySize += count;
}

template <unsigned int NDimensions>
void
BloxCoreAtomPixel<NDimensions>
::NormalizeVotedCMatrix()
{
  if(m_ConstituencySize != 0)
    m_VotedCMatrix /= m_ConstituencySize;
}

template <unsigned int NDimensions>
void
BloxCoreAtomPixel<NDimensions>
::DoVotedEigenanalysis()
{
  // Create an identity matrix of size n
  vnl_matrix_fixed<double, NDimensions, NDimensions> identMatrix;
  identMatrix.fill(0);

  // Set the diagonal to 1
  for(unsigned int n = 0; n < NDimensions; n++) // row loop
    {
    identMatrix(n,n) = 1.0;
    } // end row loop

  // Do eigen analysis
  vnl_generalized_eigensystem* pEigenSys = new vnl_generalized_eigensystem(m_VotedCMatrix, identMatrix);

  // Now, store the results
  
  // First the eigenvectors
  m_VotedEigenvectors = pEigenSys->V;

  // Now the eigenvalues (stored as a vector to save space)
  for(unsigned int i = 0; i < NDimensions; i++)
    {
    m_VotedEigenvalues[i] = pEigenSys->D(i,i);
    }

  delete pEigenSys;

  //printf("VotedCMatrix\n");
  for(int i = 0; i < 3; i++)
    {
    //printf("%f %f %f\n", m_VotedCMatrix(i,0), m_VotedCMatrix(i,1), m_VotedCMatrix(i,2) );
    }
  //printf("\n");

  //printf("Voted eigenvectors\n");
  for(int i = 0; i < 3; i++)
    {
    //printf("%f %f %f\n", m_VotedEigenvectors(i,0), m_VotedEigenvectors(i,1), m_VotedEigenvectors(i,2) );
    }
  //printf("\n");
}

} // end namespace itk

#endif
