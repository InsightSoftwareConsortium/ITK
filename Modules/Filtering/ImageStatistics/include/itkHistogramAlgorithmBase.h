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
#ifndef itkHistogramAlgorithmBase_h
#define itkHistogramAlgorithmBase_h

#include "itkMacro.h"
#include "itkObjectFactory.h"
#include "itkObject.h"

namespace itk
{
/** \class HistogramAlgorithmBase
 * \brief base class for algorithms operating on histograms
 *
 * You plug in the target sample data using SetInputHistogram method. Then call
 * the Compute() method to run the algorithm.
 *
 * \ingroup ITKImageStatistics
 */

template <typename TInputHistogram>
class ITK_TEMPLATE_EXPORT HistogramAlgorithmBase : public Object
{
public:
  /**Standard class type aliases. */
  using Self = HistogramAlgorithmBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /**Standard Macros */
  itkTypeMacro(HistogramAlgorithmBase, Object);

  /** Histogram type alias alias */
  using InputHistogramType = TInputHistogram;

  /** Stores the histogram pointer */
  void
  SetInputHistogram(const TInputHistogram * histogram)
  {
    if (m_InputHistogram != histogram)
    {
      m_InputHistogram = histogram;
      this->Modified();
    }
  }

  /** Returns the histogram const pointer */
  const TInputHistogram *
  GetInputHistogram() const
  {
    return m_InputHistogram.GetPointer();
  }

  /** \deprecated
   * Update() is deprecated because classes that do not
   * derive from ProcessObject are not part of the pipeline and
   * should therefore not have an Update() method.
   * It is included for backwards compatibility. */
#if !defined(ITK_LEGACY_REMOVE)
  void
  Update()
  {
    this->Compute();
  }
#endif

protected:
  HistogramAlgorithmBase();
  ~HistogramAlgorithmBase() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  virtual void
  Compute() = 0;

private:
  /** Target histogram data pointer */
  typename TInputHistogram::ConstPointer m_InputHistogram;
}; // end of class
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHistogramAlgorithmBase.hxx"
#endif

#endif
