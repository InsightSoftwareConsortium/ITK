/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPath.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPath_h
#define __itkPath_h

#include "itkDataObject.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include "itkNumericTraits.h"

namespace itk
{


/** \class Path
 * \brief  Represent a path through ND Space
 *
 * This base class is intended to represent a path through an image.   As a
 * path, it maps a 1D parameter (such as time or arc length, etc) to an index
 * (or possibly an offset or a point) in ND space.  This mapping is done via the
 * abstract Evaluate() method, which must be overridden in all instantiable
 * subclasses. The only geometric requirement for a gerneral path is that it be
 * continuous. A path may be open or closed, and may cross itself several
 * times.  A classic application of this class is the representation of contours
 * in 2D images using chaincodes or freeman codes.  Another use of a path is to
 * guide the movement of an iterator through an image.
 *
 * \sa Index
 * \sa Point
 * \sa ContinuousIndex
 *
 * \ingroup PathObjects
 */
template <class TInput, class TOutput, unsigned int VDimension>
class ITK_EXPORT Path : public DataObject
{
public:
  /** Standard class typedefs. */
  typedef Path                      Self;
  typedef DataObject                Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Path dimension. The dimension of a path is fixed at construction. */
  itkStaticConstMacro(PathDimension, unsigned int, VDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Path, FunctionBase);
  
  /** Input type */
  typedef TInput  InputType;
  
  /** Output type */
  typedef TOutput OutputType;
  
  
  /** All paths must be mapable to index space */
  typedef Index<  VDimension >  IndexType;
  typedef Offset< VDimension >  OffsetType;


  /** Where does the path begin?  For most types of paths, the path will begin
   * at zero.  This value can be overridden in children, and is necessary for
   * iterators to know how to go to the beginning of a path. */
  virtual inline InputType StartOfInput() const
    {
    return NumericTraits<InputType>::Zero;
    }

  /** Where does the path end (what is the last valid input value)?  This value
   * is sometimes used by IncrementInput() to go to the end of a path. */
  virtual inline InputType EndOfInput() const
    {
    return NumericTraits<InputType>::One;
    }
  
  /** Evaluate the path at specified location along the path.
    * Return data is the path's "natural" format. */
  virtual OutputType Evaluate( const InputType & input ) const = 0;
  
  /** Like Evaluate(), except always returns an index */
  virtual IndexType EvaluateToIndex( const InputType & input ) const = 0;

  /** Increment the input variable passed by reference such that the
   * ND index of the path moves to its next vertex-connected
   * (8-connected in 2D) neighbor.  Return the Index-space offset of
   * the path from its prior input to its new input.  If the path is
   * unable to increment, input is not changed and an offset of Zero
   * is returned. Children are not required to implement general
   * bounds checking, but are required to return an offset of zero
   * when trying to increment from the final valid input value. */
  virtual OffsetType IncrementInput(InputType & input) const = 0;
  
  
protected:
  Path();
  ~Path(){}

  void PrintSelf(std::ostream& os, Indent indent) const;

  itkGetConstMacro(ZeroOffset,OffsetType);
  itkGetConstMacro(ZeroIndex,IndexType);

private:
  Path(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  // These "constants" are initialized in the constructor
  OffsetType  m_ZeroOffset; // = 0 for all dimensions
  IndexType   m_ZeroIndex;  // = 0 for all dimensions
  
};

} // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_Path(_, EXPORT, x, y) namespace itk { \
  _(3(class EXPORT Path< ITK_TEMPLATE_3 x >)) \
  namespace Templates { typedef Path< ITK_TEMPLATE_3 x > Image##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkPath+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkPath.txx"
#endif
  
#endif
