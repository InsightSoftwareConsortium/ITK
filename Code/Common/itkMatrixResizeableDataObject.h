/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMatrixResizeableDataObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMatrixResizeableDataObject_h
#define __itkMatrixResizeableDataObject_h

#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include "vnl/vnl_matrix.h"

namespace itk
{
       
/**
 * \class MatrixResizeableDataObject
 * \brief Allows for a vnl matrix to be a data
 * object with the flexibility of being resizable.
 *
 * \ingroup DataProcessing  
 **/

template<typename TItemType>
class MatrixResizeableDataObject : public DataObject, public vnl_matrix<TItemType> {
public:

  /** Standard class typedefs. */
  typedef MatrixResizeableDataObject Self;
  typedef DataObject Superclass;
  typedef SmartPointer<Self>   Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MatrixResizeableDataObject, DataObject);

protected:

    /** Default Constructor. */
  MatrixResizeableDataObject();

  /** Default Destructor. */
  ~MatrixResizeableDataObject();
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_MatrixResizeableDataObject(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT MatrixResizeableDataObject< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef MatrixResizeableDataObject< ITK_TEMPLATE_1 x > \
                                     MatrixResizeableDataObject##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkMatrixResizeableDataObject+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkMatrixResizeableDataObject.txx"
#endif

#endif
