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
#ifndef itkShapePriorSegmentationLevelSetFunction_h
#define itkShapePriorSegmentationLevelSetFunction_h

#include "itkSegmentationLevelSetFunction.h"
#include "itkShapeSignedDistanceFunction.h"

namespace itk
{
/** \class ShapePriorSegmentationLevelSetFunction
 *
 * \brief This function is used in ShapePriorSegmentationLevelSetFilter to
 * segment structures in an image based on user supplied edge potential map and
 * shape model.
 *
 * This class extends the basic LevelSetFunction with a shape prior term
 * as developed in [1].
 *
 * \f$ \zeta( \phi^{*} - \phi) \f$
 *
 * where \f$ \phi^{*} \f$ is the signed distance function from a target shape
 * and \f$ \zeta \f$ is a scalar constant.
 *
 * The target shape signed distance function is supplied through a
 * ShapeSignedDistanceFunction object.
 *
 * \sa LevelSetFunction
 * \sa SegmentationLevelSetImageFunction
 * \sa ShapeSignedDistanceFunction
 *
 * \par REFERENCES
 * \par
 * [1] Leventon, M.E. et al. "Statistical Shape Influence in Geodesic Active Contours", CVPR 2000.
 *
 * \ingroup FiniteDifferenceFunctions
 * \ingroup ITKLevelSets
 */
template< typename TImageType, typename TFeatureImageType = TImageType >
class ITK_TEMPLATE_EXPORT ShapePriorSegmentationLevelSetFunction:
  public SegmentationLevelSetFunction< TImageType, TFeatureImageType >
{
public:
  /** Standard class typedefs. */
  typedef ShapePriorSegmentationLevelSetFunction Self;
  typedef SegmentationLevelSetFunction< TImageType, TFeatureImageType >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef TFeatureImageType          FeatureImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro(ShapePriorSegmentationLevelSetFunction, SegmentationLevelSetFunction);

  /** Extract some parameters from the superclass. */
  typedef typename Superclass::ImageType           ImageType;
  typedef typename Superclass::NeighborhoodType    NeighborhoodType;
  typedef typename Superclass::ScalarValueType     ScalarValueType;
  typedef typename Superclass::FeatureScalarType   FeatureScalarType;
  typedef typename Superclass::RadiusType          RadiusType;
  typedef typename Superclass::FloatOffsetType     FloatOffsetType;
  typedef typename Superclass::VectorImageType     VectorImageType;
  typedef typename Superclass::PixelType           PixelType;
  typedef typename Superclass::TimeStepType        TimeStepType;
  typedef typename Superclass::IndexType           IndexType;
  typedef typename Superclass::ContinuousIndexType ContinuousIndexType;

  /** Extract some parameters from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** ShapeFunction typedef support. */
  typedef ShapeSignedDistanceFunction< double,
                                       itkGetStaticConstMacro(ImageDimension) > ShapeFunctionType;
  typedef typename ShapeFunctionType::ConstPointer ShapeFunctionPointer;

  /** Zeta. The ShapePriorWeight scales the shape prior term values. */
  void SetShapePriorWeight(const ScalarValueType p)
  { m_ShapePriorWeight = p; }
  ScalarValueType GetShapePriorWeight() const
  { return m_ShapePriorWeight; }

  /** The ShapeFunction encapsulates the signed distance to the shape used to
   * influence the evolution of the level set. */
  void SetShapeFunction(const ShapeFunctionType *ptr)
  { m_ShapeFunction = ptr; }
  const ShapeFunctionType * GetShapeFunction() const
  { return m_ShapeFunction; }

  /** Compute the equation value with the additional shape prior term. */
  virtual PixelType ComputeUpdate( const NeighborhoodType & neighborhood,
                                   void *globalData,
                                   const FloatOffsetType & = FloatOffsetType(0.0) ) ITK_OVERRIDE;

  /** Compute global time step from the global data structure. */
  virtual TimeStepType ComputeGlobalTimeStep(void *globalData) const ITK_OVERRIDE;

  /** A global data type used to store values needed to compute the time step.
    */
  typedef typename Superclass::GlobalDataStruct GlobalDataStruct;
  struct ShapePriorGlobalDataStruct:public GlobalDataStruct {
    ScalarValueType m_MaxShapePriorChange;
  };

  /** Returns a pointer to a global data structure for computing time step. */
  virtual void * GetGlobalDataPointer() const ITK_OVERRIDE
  {
    ShapePriorGlobalDataStruct *ans = new ShapePriorGlobalDataStruct();

    ans->m_MaxAdvectionChange   = NumericTraits< ScalarValueType >::ZeroValue();
    ans->m_MaxPropagationChange = NumericTraits< ScalarValueType >::ZeroValue();
    ans->m_MaxCurvatureChange   = NumericTraits< ScalarValueType >::ZeroValue();
    ans->m_MaxShapePriorChange  = NumericTraits< ScalarValueType >::ZeroValue();
    return ans;
  }

  /** Release the global data structure. */
  virtual void ReleaseGlobalDataPointer(void *GlobalData) const ITK_OVERRIDE
  { delete (ShapePriorGlobalDataStruct *)GlobalData; }

protected:
  ShapePriorSegmentationLevelSetFunction();
  virtual ~ShapePriorSegmentationLevelSetFunction() ITK_OVERRIDE {}

  ITK_DISALLOW_COPY_AND_ASSIGN(ShapePriorSegmentationLevelSetFunction);

  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:

  ShapeFunctionPointer m_ShapeFunction;
  ScalarValueType      m_ShapePriorWeight;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapePriorSegmentationLevelSetFunction.hxx"
#endif

#endif
