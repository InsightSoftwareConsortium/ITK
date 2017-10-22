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
#ifndef itkMatrixResizeableDataObject_h
#define itkMatrixResizeableDataObject_h

#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
/**
 * \class MatrixResizeableDataObject
 * \brief Allows a VNL matrix to be a DataObject with the flexibility of
 *        being resizable.
 *
 * \ingroup DataProcessing
 * \ingroup ITKCommon
 */

template< typename TItemType >
class ITK_TEMPLATE_EXPORT MatrixResizeableDataObject:public DataObject, public vnl_matrix< TItemType >
{
public:

  /** Standard class typedefs. */
  typedef MatrixResizeableDataObject Self;
  typedef DataObject                 Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MatrixResizeableDataObject, DataObject);

protected:

  /** Default Constructor. */
  MatrixResizeableDataObject();

  /** Default Destructor. */
  ~MatrixResizeableDataObject() ITK_OVERRIDE;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMatrixResizeableDataObject.hxx"
#endif

#endif
