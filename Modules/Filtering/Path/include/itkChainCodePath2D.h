/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/**
 *\class ChainCodePath2D
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
class ITK_TEMPLATE_EXPORT ChainCodePath2D : public ChainCodePath<2>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ChainCodePath2D);

  /** Dimension underlying input image. */
  static constexpr unsigned int Dimension = 2;

  /** Standard class type aliases. */
  using Self = ChainCodePath2D;
  using Superclass = ChainCodePath<2>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChainCodePath2D, ChainCodePath);

  /** OutputType type alias support */
  using OutputType = Superclass::OutputType;
  using InputType = Superclass::InputType;

  /** The output type of this function is an Index */
  using OffsetType = OutputType;
  using IndexType = Index<2>;

  /** ChainCodeType is a useless relic of the parent class */
  using ChainCodeType = Superclass::ChainCodeType;
  using ChainCodeSizeType = Superclass::ChainCodeSizeType;

  /** ChainCodePath2D stores its data as a Freeman-encoded chain code */
  using ChainCode2DType = std::vector<int>;

  // Functions inherited from Path

  /** Evaluate the chaincode for the offset at the specified path-position. */
  OutputType
  Evaluate(const InputType & input) const override;

  /** Like Evaluate(), but returns the index at the specified path-position. */
  IndexType
  EvaluateToIndex(const InputType & input) const override;

  /** Increment the input variable passed by reference and return the offset
   * stored at the previous path-position.  If the chaincode is unable to be
   * incremented, input is not changed and an offset of zero is returned, which
   * may be used to check for the end of the chain code. */
  OffsetType
  IncrementInput(InputType & input) const override;

  // Functions specific to ChainCodePath and its descendents

  /** New() method for dynamic construction */
  itkNewMacro(Self);

  /** How many steps in the chaincode? */
  ChainCodeSizeType
  NumberOfSteps() const override
  {
    return m_Chain2D.size();
  }

  /** Insert a new step into the chaincode at a specified position */
  inline void
  InsertStep(InputType position, int encodedStep)
  {
    m_Chain2D.insert(m_Chain2D.begin() + position, encodedStep);
    this->Modified();
  }

  void
  InsertStep(InputType position, OffsetType step) override
  {
    m_Chain2D.insert(m_Chain2D.begin() + position, EncodeOffset(step));
    this->Modified();
  }

  /** Change the direction of a step in the chaincode */
  inline void
  ChangeStep(InputType position, int encodedStep)
  {
    m_Chain2D[position] = encodedStep;
    this->Modified();
  }

  void
  ChangeStep(InputType position, OffsetType step) override
  {
    m_Chain2D[position] = EncodeOffset(step);
    this->Modified();
  }

  /** Remove all steps from the chain code */
  void
  Clear() override
  {
    m_Chain2D.clear();
    this->Modified();
  }

  std::string
  GetChainCodeAsString() const;

protected:
  ChainCodePath2D();
  ~ChainCodePath2D() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Encode and Decode between an offset and a Freeman code */
  inline int
  EncodeOffset(OffsetType step) const
  {
    return m_FreemanCode[step[0] + 1][step[1] + 1];
  }

  inline OffsetType
  DecodeOffset(int encodedStep) const
  {
    return m_ReverseFreemanCode[encodedStep];
  }

private:
  ChainCode2DType m_Chain2D; // the Freeman-encoded chain code

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
