/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkChainCodePath2D.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef _itkChainCodePath2D_h
#define _itkChainCodePath2D_h

#include "itkChainCodePath.h"
#include "itkObjectFactory.h"
#include "itkIndex.h"
#include "itkOffset.h"
#include <vector>
#include <string>


namespace itk
{


/** \class ChainCodePath2D
 * \brief  Represent a 2D path as a sequence of connected image index offsets
 *
 * This class is intended to represent sequences of connected indices in a 2D
 * image plane.  It does so by storing the offset of each index from its immediately
 * preceeding, connected, index using a standard Freeman code (1=up, 2=up to the right, and so on proceeding clockwise to 8=up to the left).  The only image index stored directly is that
 * of the first index.  ChainCodePath2D maps a 1D integer input (step number) to
 * a 2D interger output (either an offset or an image index, depending on
 * whether Evaluate or EvaluateToIndex is called).
 *
 * \sa ChainCodePath
 * \sa ParametricPath
 * \sa Path
 * \sa Index
 *
 * \ingroup PathObjects
 */
class ITK_EXPORT ChainCodePath2D : public
ChainCodePath<2>
{
public:
  /** Dimension underlying input image. */
  itkStaticConstMacro(Dimension, unsigned int, 2);

  /** Standard class typedefs. */
  typedef ChainCodePath2D   Self;
  typedef ChainCodePath<2>  Superclass;

  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ChainCodePath2D, ChainCodePath);

  
  /** OutputType typedef support. */
  typedef Superclass::OutputType   OutputType;
  typedef Superclass::InputType    InputType;

  /** The output type of this function is an Index */
  typedef OutputType  OffsetType;
  typedef Index<2>    IndexType;

  /** ChainCodeType is a usless relic of the parent class */
  typedef std::vector<OffsetType>  ChainCodeType;
  /** ChainCodePath2D stores its data as a Freeman-encoded chain code */
  typedef std::vector<int>         ChainCode2DType;



  // Functions inherited from Path
  
  /** Evaluate the chaincode for the offset at the specified path-position. */
  virtual OutputType Evaluate( const InputType & input ) const;
  
  /** Like Evaluate(), but returns the index at the specified path-position. */
  virtual IndexType EvaluateToIndex( const InputType & input ) const;
  
  /** Increment the input variable passed by reference and return the offset
   * stored at the previous path-position.  If the chaincode is unable to be
   * incremented, input is not changed and an offset of zero is returned, which
   * may be used to check for the end of the chain code. */
  virtual OffsetType IncrementInput(InputType & input) const;



  // Functions specific to ChainCodePath and its descendents

  /** New() method for dynamic construction */
  itkNewMacro( Self );
  
  /** How many steps in the chaincode? */
  inline unsigned int NumberOfSteps() const { return m_Chain2D.size(); }
  
  
  /** Insert a new step into the chaincode at a specified position */
  inline void InsertStep( InputType position, int encodedStep )
  {
    m_Chain2D.insert( m_Chain2D.begin()+position, encodedStep );
    this->Modified();
  }
  inline void InsertStep( InputType position, OffsetType step )
  {
    m_Chain2D.insert( m_Chain2D.begin()+position, EncodeOffset(step) );
    this->Modified();
  }
  
  
  /** Change the direction of a step in the chaincode */
  inline void ChangeStep( InputType position, int encodedStep )
  {
    m_Chain2D[position]=encodedStep;
    this->Modified();
  }
  inline void ChangeStep( InputType position, OffsetType step )
  {
    m_Chain2D[position]=EncodeOffset(step);
    this->Modified();
  }
  
  /** Remove all steps from the chain code */
  virtual inline void Clear()
    {
    m_Chain2D.clear();
    this->Modified();
    }
  
  std::string GetChainCodeAsString(void) const;



protected:
  ChainCodePath2D();
  ~ChainCodePath2D() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Encode and Decode between an offset and a Freeman code */
  inline int EncodeOffset( OffsetType  step ) const
    {
    return m_FreemanCode[  step[0] + 1  ][  step[1] + 1  ];
    }
  inline OffsetType DecodeOffset( int encodedStep ) const
    {
    return m_ReverseFreemanCode[ encodedStep ];
    }



private:
  ChainCodePath2D(const Self&); //purposely not implemented
  void operator=(const Self&);  //purposely not implemented
  
  ChainCode2DType  m_Chain2D;   // the Freeman-encoded chain code
  
  // FreemanCode[][] implements a lookup table for converting offsets to a
  // Freeman code.  Within each dimension, the only allowable offset values are
  // { -1, 0, 1 }.  The y-axis is assumed to point up.  It is initialized in the
  // constructor.  Use it as follows:
  //
  //   encodedValue = m_FreemanCode[ x offset + 1 ][ y offset + 1 ]
  //
  int m_FreemanCode[3][3];

  // m_ReverseFreemanCode[ encodedValue ] implements a lookup table for the
  // inverse of m_FreemanCode[][].  It is initialized in the constructor.
  OffsetType m_ReverseFreemanCode[9];
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkChainCodePath2D.txx"
#endif

#endif
