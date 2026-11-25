/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkIterateNeighborhoodOptimizer_h
#define itkIterateNeighborhoodOptimizer_h

#include "MinimalPathExtractionExport.h"

#include "itkArray.h"
#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{

/** \class IterateNeighborhoodOptimizer
 * \brief Finds the local minima/maxima by iteratively choosing the
 *        minimum/maximum value in a neighborhood.
 *
 * This optimizer is designed to operate on a monotonic cost function
 * WITHOUT using gradient information (derivatives). The user must set
 * the Neighborhood size, and optionally the connectivity.
 *
 * \ingroup Numerics Optimizers
 *
 * \ingroup MinimalPathExtraction
 */
class MinimalPathExtraction_EXPORT IterateNeighborhoodOptimizer : public SingleValuedNonLinearOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IterateNeighborhoodOptimizer);

  /** Standard class type alias. */
  using Self = IterateNeighborhoodOptimizer;
  using Superclass = SingleValuedNonLinearOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(IterateNeighborhoodOptimizer);

  /** Configure whether the local maxima or minima is found.
   *  The default is to minimize the cost function (maximize = false ).*/
  itkGetConstReferenceMacro(Maximize, bool);
  itkSetMacro(Maximize, bool);
  itkBooleanMacro(Maximize);
  bool
  GetMinimize() const
  {
    return !m_Maximize;
  }
  void
  SetMinimize(bool v)
  {
    this->SetMaximize(!v);
  }
  void
  MinimizeOn()
  {
    this->MaximizeOff();
  }
  void
  MinimizeOff()
  {
    this->MaximizeOn();
  }

  /** Advance one step. */
  virtual void
  AdvanceOneStep();

  /** Start optimization. */
  void
  StartOptimization() override;

  /** Resume previously stopped optimization with current parameters
   * \sa StopOptimization. */
  void
  ResumeOptimization();

  /** Stop optimization.
   * \sa ResumeOptimization */
  void
  StopOptimization();

  /**
   * Get/set whether the nieghborhood is defined by face connectivity or
   * by face+edge+vertex connectivity.  Default is FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  /** Get/set the nieghborhood size (in physical space).
   *  The default is [1.0,1.0] and MUST be specified for all 3-D images
   *  and 2-D images with non-unity spacing. */
  using NeighborhoodSizeType = Array<double>;
  itkSetMacro(NeighborhoodSize, NeighborhoodSizeType);
  itkGetConstReferenceMacro(NeighborhoodSize, NeighborhoodSizeType);

  /** Get the current iteration number. */
  itkGetConstMacro(CurrentIteration, unsigned int);

  /** Get the current value. */
  itkGetConstReferenceMacro(CurrentValue, double);

protected:
  IterateNeighborhoodOptimizer();
  ~IterateNeighborhoodOptimizer() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  bool                 m_Stop;
  bool                 m_Maximize;
  bool                 m_FullyConnected;
  double               m_CurrentValue;
  unsigned long        m_CurrentIteration;
  NeighborhoodSizeType m_NeighborhoodSize;
};

} // end namespace itk

#endif
