/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnaryCorrespondenceMatrix.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkUnaryCorrespondenceMatrix_h
#define __itkUnaryCorrespondenceMatrix_h

#include "itkDataObject.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/**
 * \class UnaryCorrespondenceMatrix
 * \brief A matrix used to store the Unary Metric
 * for medial node comparisons between two images.
 *
 * \ingroup
 *
 */

template< typename TItemType >
class UnaryCorrespondenceMatrix:public DataObject, public vnl_matrix< TItemType >
{
public:

  /** Standard class typedefs. */
  typedef UnaryCorrespondenceMatrix  Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(UnaryCorrespondenceMatrix, DataObject);
protected:

  /** Default Constructor. */
  UnaryCorrespondenceMatrix();

  /** Default Destructor. */
  ~UnaryCorrespondenceMatrix() {}
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkUnaryCorrespondenceMatrix.txx"
#endif

#endif
