/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkUnaryCorrespondenceMatrix_h
#define itkUnaryCorrespondenceMatrix_h

#include "itkObjectFactory.h"
#include "itkDataObject.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/**
 * \class UnaryCorrespondenceMatrix
 * \brief A matrix used to store the Unary Metric
 * for medial node comparisons between two images.
 *
 * \ingroup ITKCommon
 */

template< typename TItemType >
class ITK_TEMPLATE_EXPORT UnaryCorrespondenceMatrix:public DataObject, public vnl_matrix< TItemType >
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
#include "itkUnaryCorrespondenceMatrix.hxx"
#endif

#endif
