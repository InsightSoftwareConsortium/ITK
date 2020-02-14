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
#ifndef itkChainCodePath_h
#define itkChainCodePath_h

#include "itkPath.h"
#include "itkOffset.h"
#include "itkObjectFactory.h"

#include <vector>

namespace itk
{
/**
 *\class ChainCodePath
 * \brief  Represent a path as a sequence of connected image index offsets
 *
 * This class is intended to represent sequences of connected indices in an
 * image.  It does so by storing the offset of each index from its immediately
 * preceding, connected, index.  The only image index stored directly is that
 * of the first index.  ChainCodePath maps a 1D integer input (step number) to
 * an ND integer output (either an offset or an image index, depending on
 * whether Evaluate or EvaluateToIndex is called).
 *
 * \sa ChainCodePath2D
 * \sa ParametricPath
 * \sa Path
 * \sa Index
 * \sa Offset
 *
 * \ingroup PathObjects
 * \ingroup ITKPath
 */
template <unsigned int VDimension>
class ITK_TEMPLATE_EXPORT ChainCodePath : public Path<unsigned int, Offset<VDimension>, VDimension>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ChainCodePath);

  /** Dimension underlying input image. */
  static constexpr unsigned int Dimension = VDimension;

  /** Standard class type aliases. */
  using Self = ChainCodePath<VDimension>;
  using Superclass = Path<unsigned int, Offset<VDimension>, VDimension>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ChainCodePath, Path);

  /** OutputType type alias support */
  using OutputType = typename Superclass::OutputType;
  using InputType = typename Superclass::InputType;

  /** The output type of this function is an Index */
  using OffsetType = OutputType;
  using IndexType = Index<VDimension>;

  using ChainCodeType = std::vector<OffsetType>;

  using ChainCodeSizeType = typename ChainCodeType::size_type;

  // Functions inherited from Path

  /** Evaluate the chaincode for the offset at the specified path-position. */
  OutputType
  Evaluate(const InputType & input) const override
  {
    return m_Chain[input];
  }

  /** Like Evaluate(), but returns the index at the specified path-position. */
  IndexType
  EvaluateToIndex(const InputType & input) const override;

  /** Increment the input variable passed by reference and then return the
   * offset stored at the new path-position.  If the chaincode is unable to be
   * incremented, input is not changed and an offset of zero is returned, which
   * may be used to check for the end of the chain code. */
  OffsetType
  IncrementInput(InputType & input) const override;

  /** Where does the path end (what is the last valid input value)? */
  InputType
  EndOfInput() const override
  {
    return static_cast<InputType>(NumberOfSteps()); // 0 is before the first step, 1 is after it
  }

  /** New() method for dynamic construction */
  itkNewMacro(Self);

  /** Set/Get the index associated with the initial position of the path */
  itkSetMacro(Start, IndexType);
  itkGetConstReferenceMacro(Start, IndexType);

  /** Insert a new step into the chaincode at a specified position */
  virtual inline void
  InsertStep(InputType position, OffsetType step)
  {
    m_Chain.insert(m_Chain.begin() + position, step);
    this->Modified();
  }

  /** Change the direction of a step in the chaincode */
  virtual inline void
  ChangeStep(InputType position, OffsetType step)
  {
    m_Chain[position] = step;
    this->Modified();
  }

  /** Remove all steps from the chain code */
  virtual inline void
  Clear()
  {
    m_Chain.clear();
    this->Modified();
  }

  /** How many steps in the chaincode? */
  virtual inline ChainCodeSizeType
  NumberOfSteps() const
  {
    return m_Chain.size();
  }

  /** Needed for Pipelining */
  void
  Initialize() override
  {
    m_Start = this->GetZeroIndex();
    this->Clear();
  }

protected:
  ChainCodePath();
  ~ChainCodePath() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  IndexType     m_Start; // origin image index for the path
  ChainCodeType m_Chain; // the chain code (vector of offsets)
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkChainCodePath.hxx"
#endif

#endif
