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
#ifndef itkLevelSetFunctionWithRefitTerm_h
#define itkLevelSetFunctionWithRefitTerm_h

#include "itkLevelSetFunction.h"
#include "itkSparseImage.h"
#include "itkNumericTraits.h"

namespace itk
{
/**
 * \class LevelSetFunctionWithRefitTerm
 *
 * \brief This class extends the LevelSetFunction class by adding a grow term
 * based on a target curvature stored in a sparse image.
 *
 * \par
 * We extend the LevelSetFunction class to add a refitting term. This refitting
 * term forces the curvature of the level set interface to match a prescribed
 * curvature. The prescribed curvature is provided in a sparse image
 * format. The NodeType for the sparse image should contain the member
 * variables m_Curvature and m_CurvatureFlag. The refitting term is defined as
 * part of the propagation term of the original LevelSetFunction. To this
 * purpose we defined the PropagationSpeed method which computes the refitting
 * term and also adds to this term the value returned by the new virtual
 * OtherPropagationSpeed. Therefore, classes derived from this class MUST NOT
 * overwrite the PropagationSpeed method. Instead classes wishing to define a
 * prapagtion term must define OtherPropagationSpeed.
 *
 * \par IMPORTANT
 * Subclasses MUST NOT overwrite the PropagationSpeed method. Define
 * OtherPropagationSpeed instead.
 * \ingroup ITKLevelSets
 */
template< typename TImageType, typename TSparseImageType >
class ITK_TEMPLATE_EXPORT LevelSetFunctionWithRefitTerm:
  public LevelSetFunction< TImageType >
{
public:
  /** Standard class typedefs. */
  typedef LevelSetFunctionWithRefitTerm  Self;
  typedef LevelSetFunction< TImageType > Superclass;
  typedef SmartPointer< Self >           Pointer;
  typedef SmartPointer< const Self >     ConstPointer;

  /** Run-time type information (and related methods) */
  itkTypeMacro(LevelSetFunctionWithRefitTerm, LevelSetFunction);

  /** Standard New macro. */
  itkNewMacro (Self);

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType              ImageType;
  typedef typename Superclass::FloatOffsetType        FloatOffsetType;
  typedef typename Superclass::ScalarValueType        ScalarValueType;
  typedef typename Superclass::GlobalDataStruct       GlobalDataStruct;
  typedef typename Superclass::NeighborhoodType       NeighborhoodType;
  typedef typename Superclass::NeighborhoodScalesType NeighborhoodScalesType;
  typedef typename Superclass::TimeStepType           TimeStepType;

  typedef typename NeighborhoodType::SizeValueType    NeighborhoodSizeValueType;

  /** Index type derived from the ImageType. */
  typedef typename ImageType::IndexType IndexType;

  /** The sparse image type used for the curvature target. */
  typedef TSparseImageType SparseImageType;

  /** The node type of the sparse image. */
  typedef typename SparseImageType::NodeType NodeType;

  /** The type for the normal vectors of the level set image. */
  typedef typename NodeType::NodeDataType NormalVectorType;

  /** Set the relative weight of the refitting term. */
  void SetRefitWeight(const ScalarValueType w)
  {
    m_RefitWeight = w;
  }

  /** This is the weight for propagation terms (other than refitting)
   * that can be defined by subclasses. */
  void SetOtherPropagationWeight(const ScalarValueType w)
  {
    m_OtherPropagationWeight = w;
  }

  /** Sets the sparse image which has nodes containing the member variable
      m_Curvature used in refitting. */
  void SetSparseTargetImage(SparseImageType *im)
  { m_SparseTargetImage = im; }

  /** Returns the sparse image. */
  SparseImageType * GetSparseTargetImage() const
  { return m_SparseTargetImage; }

  /** Computes the time step for an update given a global data structure.
   * This calls the ComputeGlobalTimeStep method defined in LevelSetFunction
   * and then imposes our own restrictions for the refitting term on the
   * returned value. */
  virtual TimeStepType ComputeGlobalTimeStep(void *GlobalData) const ITK_OVERRIDE;

protected:
  /** The weight for the refitting term. */
  ScalarValueType m_RefitWeight;

  /** The weight for other scalar propagation terms that can be defined by
      classes derived from this class. */
  ScalarValueType m_OtherPropagationWeight;

  LevelSetFunctionWithRefitTerm ();
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Computes the curvature of a level set neighbothood in a way that matches
      the curvature computation from normal vectors. */
  ScalarValueType ComputeCurvature(const NeighborhoodType &) const;

  /** Defines the virtual function in LevelSetFunction to add the refitting
   * term. This function also calls OtherPropagationSpeed to provide a
   * mechanism for subclasses to define other propagation terms. */
  virtual ScalarValueType PropagationSpeed(const NeighborhoodType &,
                                           const FloatOffsetType &,
                                           GlobalDataStruct * = 0) const ITK_OVERRIDE;

  /** Called by PropagationSpeed and added on to the refitting term. Function
   * classes derived from this class should define this method for their
   * propagation speed, NOT the actual PropagationSpeed method. */
  virtual ScalarValueType OtherPropagationSpeed(const NeighborhoodType &,
                                                const FloatOffsetType &,
                                                GlobalDataStruct * = 0) const
  {
    return NumericTraits< ScalarValueType >::ZeroValue();
  }

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(LevelSetFunctionWithRefitTerm);

  /** The sparse image that contains the target curvature information. */
  typename SparseImageType::Pointer m_SparseTargetImage;

  /** The minimum vector norm parameter. */
  ScalarValueType m_MinVectorNorm;

  /** Constants used in computations. */
  static const NeighborhoodSizeValueType    m_NumVertex;
  static const ScalarValueType              m_DimConst;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetFunctionWithRefitTerm.hxx"
#endif

#endif
