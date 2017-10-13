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
#ifndef itkChainCodePath2D_h
#define itkChainCodePath2D_h

#include "itkChainCodePath.h"
#include "itkIndex.h"

#include <vector>
#include <string>

namespace itk
{
/** \class ChainCodePath2D
 * \brief  Represent a 2D path as a sequence of connected image index offsets
 *
 * This class is intended to represent sequences of connected indices in a 2D
 * image plane.  It does so by storing the offset of each index from its
 * immediately preceding, connected, index using a standard Freeman code
 * (1=up, 2=up to the right, and so on proceeding clockwise to 8=up
 * to the left).  The only image index stored directly is thatof the first
 * index.
 * ChainCodePath2D maps a 1D integer input (step number) to
 * a 2D integer output (either an offset or an image index, depending on
 * whether Evaluate or EvaluateToIndex is called).
 *
 * \sa ChainCodePath
 * \sa ParametricPath
 * \sa Path
 * \sa Index
 *
 * \ingroup PathObjects
 * \ingroup ITKPath
 */
class ITK_TEMPLATE_EXPORT ChainCodePath2D:public
  ChainCodePath< 2 >
{
public:
  /** Dimension underlying input image. */
  itkStaticConstMacro(Dimension, unsigned int, 2);

  /** Standard class typedefs. */
  typedef ChainCodePath2D    Self;
  typedef ChainCodePath< 2 > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChainCodePath2D, ChainCodePath);

  /** OutputType typedef support. */
  typedef Superclass::OutputType OutputType;
  typedef Superclass::InputType  InputType;

  /** The output type of this function is an Index */
  typedef OutputType OffsetType;
  typedef Index< 2 > IndexType;

  /** ChainCodeType is a usless relic of the parent class */
  typedef Superclass::ChainCodeType     ChainCodeType;
  typedef Superclass::ChainCodeSizeType ChainCodeSizeType;

  /** ChainCodePath2D stores its data as a Freeman-encoded chain code */
  typedef std::vector< int > ChainCode2DType;

  // Functions inherited from Path

  /** Evaluate the chaincode for the offset at the specified path-position. */
  virtual OutputType Evaluate(const InputType & input) const ITK_OVERRIDE;

  /** Like Evaluate(), but returns the index at the specified path-position. */
  virtual IndexType EvaluateToIndex(const InputType & input) const ITK_OVERRIDE;

  /** Increment the input variable passed by reference and return the offset
   * stored at the previous path-position.  If the chaincode is unable to be
   * incremented, input is not changed and an offset of zero is returned, which
   * may be used to check for the end of the chain code. */
  virtual OffsetType IncrementInput(InputType & input) const ITK_OVERRIDE;

  // Functions specific to ChainCodePath and its descendents

  /** New() method for dynamic construction */
  itkNewMacro(Self);

  /** How many steps in the chaincode? */
  virtual ChainCodeSizeType NumberOfSteps() const ITK_OVERRIDE { return m_Chain2D.size(); }

  /** Insert a new step into the chaincode at a specified position */
  inline void InsertStep(InputType position, int encodedStep)
  {
    m_Chain2D.insert(m_Chain2D.begin() + position, encodedStep);
    this->Modified();
  }

  virtual void InsertStep(InputType position, OffsetType step) ITK_OVERRIDE
  {
    m_Chain2D.insert( m_Chain2D.begin() + position, EncodeOffset(step) );
    this->Modified();
  }

  /** Change the direction of a step in the chaincode */
  inline void ChangeStep(InputType position, int encodedStep)
  {
    m_Chain2D[position] = encodedStep;
    this->Modified();
  }

  virtual void ChangeStep(InputType position, OffsetType step) ITK_OVERRIDE
  {
    m_Chain2D[position] = EncodeOffset(step);
    this->Modified();
  }

  /** Remove all steps from the chain code */
  virtual void Clear() ITK_OVERRIDE
  {
    m_Chain2D.clear();
    this->Modified();
  }

  std::string GetChainCodeAsString() const;

protected:
  ChainCodePath2D();
  ~ChainCodePath2D() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Encode and Decode between an offset and a Freeman code */
  inline int EncodeOffset(OffsetType step) const
  {
    return m_FreemanCode[step[0] + 1][step[1] + 1];
  }

  inline OffsetType DecodeOffset(int encodedStep) const
  {
    return m_ReverseFreemanCode[encodedStep];
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ChainCodePath2D);

  ChainCode2DType m_Chain2D;    // the Freeman-encoded chain code

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
} // end namespace itk

#endif
