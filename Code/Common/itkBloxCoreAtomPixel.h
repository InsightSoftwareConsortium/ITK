/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxCoreAtomPixel_h
#define __itkBloxCoreAtomPixel_h

#include "vnl/vnl_matrix_fixed.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/algo/vnl_generalized_eigensystem.h"

#include "itkObject.h"
#include "itkBloxCoreAtomItem.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkPoint.h"
#include "itkCovariantVector.h"
#include "itkBloxPixel.h"

namespace itk
{

/**
 * \class BloxCoreAtomPixel
 * \brief Holds a linked list of itk::BloxCoreAtomItem's
 *
 * \ingroup ImageObjects 
 * */

template <unsigned int NDimensions>
class BloxCoreAtomPixel : public BloxPixel< BloxCoreAtomItem<NDimensions> >
{
public:
  /** The type of core atom item we process. */
  typedef BloxCoreAtomItem<NDimensions> TCoreAtomItemType;

  /** The type of boundary point item we process. */
  typedef BloxBoundaryPointItem<NDimensions> TBPItemType;

  /** The type used to store the position of the BoundaryPointItem. */
  typedef Point<double, NDimensions> TPositionType;
  
  /** The type of vector used to store the gradient of the BoundaryPointItem. */
  typedef CovariantVector<double, NDimensions> TGradientType;

  /** VNL type used in eigenanalysis. */
  typedef vnl_vector_fixed<double, NDimensions> TVectorType;

  /** Vector type used to store eigenvalues. */
  typedef vnl_vector_fixed<double, NDimensions> TEigenvalueType;

  /** Matrix type used to store eigenvectors. */
  typedef vnl_matrix_fixed<double, NDimensions, NDimensions> TEigenvectorType;

  /** Calculate and store the mean of core atom diameters. */
  double CalcMeanCoreAtomDiameter();

  /** Perform eigenanalysis on the population of core atoms stored in this pixel. */
  bool DoCoreAtomEigenanalysis();

  /** Get the mean core atom diameter. */
  double GetMeanCoreAtomDiameter(void) 
    { return m_MeanCoreAtomDiameter; }

  /** Get eigenvalues. */
  TEigenvalueType GetEigenvalues(void) 
    { return m_Eigenvalues; }

  /** Get eigenvectors. */
  TEigenvectorType GetEigenvectors(void) 
    { return m_Eigenvectors; }

  BloxCoreAtomPixel();
  ~BloxCoreAtomPixel();

private:
  /** The eigenvalues of the core atom population in this pixel
   * These are stored in increasing order of value (not absolute value) from
   * indices 0 to n, where n is the number of dimensions in the source image */
  TEigenvalueType m_Eigenvalues;

  /** The eigenvectors of the core atom population in this pixel
   * Each eigenvector is a row? of this matrix */
  TEigenvectorType m_Eigenvectors;

  /** Average (arithmetic mean) of core atom diameters stored in this pixel. */
  double m_MeanCoreAtomDiameter;

  /** The raw CMatrix - this is the matrix that we do eigen analysis on. */
  vnl_matrix_fixed<double, NDimensions, NDimensions> m_RawCMatrix;

  /** The CMatrix that collects votes cast by other blox. */
  vnl_matrix_fixed<double, NDimensions, NDimensions> m_VotedCMatrix;
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxCoreAtomPixel.txx"
#endif

#endif
