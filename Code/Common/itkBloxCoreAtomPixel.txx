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
  m_Eigenvalues.fill(0.0);
  m_Eigenvectors.fill(0.0);
  
  m_MeanCoreAtomDiameter = 0;
}

template <unsigned int NDimensions>
BloxCoreAtomPixel<NDimensions>
::~BloxCoreAtomPixel()
{
  // The default destructor walks the pixel and deletes all bloxitems
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
  itk::BloxCoreAtomPixel<NDimensions>::iterator bpiterator;
  
    // Walk through all of the items at the pixel
  for (bpiterator = this->begin(); bpiterator != this->end(); ++bpiterator)
    {
    // Get the pointer of the core atom
    TCoreAtomItemType* pCoreAtom = *bpiterator;

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
  itk::BloxCoreAtomPixel<NDimensions>::iterator bpiterator;

  // The CMatrix - this is the matrix that we do eigen analysis on
  vnl_matrix_fixed<double, NDimensions, NDimensions> cMatrix;

  // Initialize the CMatrix to 0
  cMatrix.fill(0);

  // The number of items stored in the pixel
  unsigned long int numItems = 0;

  // Walk through all of the items at the pixel
  for (bpiterator = this->begin(); bpiterator != this->end(); ++bpiterator)
    {
    // Get the pointer of the core atom
    TCoreAtomItemType* pCoreAtom = *bpiterator;

    // Get the boundary points
    TBPItemType* pBPOne = pCoreAtom->GetBoundaryPointA();
    TBPItemType* pBPTwo = pCoreAtom->GetBoundaryPointB();
    
    // Get the physical positions of the two boundary points
    TVectorType P1;
    P1 = pBPOne->GetPhysicalPosition().Get_vnl_vector();
    
    TVectorType P2;
    P2 = pBPTwo->GetPhysicalPosition().Get_vnl_vector();

    // Figure out the "C" vector of the core atom
    TVectorType cVector = P2 - P1;
    cVector.normalize();

    // Now, add to the cMatrix
    for(unsigned int r = 0; r < NDimensions; r++) // row loop
      {
      for(unsigned int c = 0; c < NDimensions; c++) // column loop
        {
        cMatrix(r,c) += cVector[c]*cVector[r];
        } // end column loop
      } // end row loop

    numItems++;

    } // end walk all of the items in the pixel

  // Divide through by the number of items
  cMatrix /= numItems;

  // Create an identity matrix of size n
  vnl_matrix_fixed<double, NDimensions, NDimensions> identMatrix;
  identMatrix.fill(0);

  // Set the diagonal to 1
  for(unsigned int n = 0; n < NDimensions; n++) // row loop
    {
    identMatrix(n,n) = 1.0;
    } // end row loop

  // Do eigen analysis
  vnl_generalized_eigensystem* pEigenSys = new vnl_generalized_eigensystem(cMatrix, identMatrix);

  // Now, store the results
  
  // First the eigenvectors
  m_Eigenvectors = pEigenSys->V;

  // Now the eigenvalues (stored as a vector to save space)
  for(unsigned int i = 0; i < NDimensions; i++)
    {
    m_Eigenvalues[i] = pEigenSys->D(i,i);
    // Print the eigen values
    itkGenericOutputMacro(<< "Eigenvalue " << i << "=" << m_Eigenvalues[i]);
    }

  delete pEigenSys;
  return true;
}

} // end namespace itk

#endif
