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
#ifndef itkRegistrationParameterScalesFromIndexShift_h
#define itkRegistrationParameterScalesFromIndexShift_h

#include "itkRegistrationParameterScalesFromShiftBase.h"

namespace itk
{

/** \class RegistrationParameterScalesFromIndexShift
 *  \brief Registration helper class for estimating scales of
 * transform parameters from the maximum voxel shift in image index space
 * caused by a parameter change.
 *
 * \ingroup ITKOptimizersv4
 */
template < typename TMetric >
class ITK_TEMPLATE_EXPORT RegistrationParameterScalesFromIndexShift :
  public RegistrationParameterScalesFromShiftBase< TMetric >
{
public:
  /** Standard class typedefs. */
  typedef RegistrationParameterScalesFromIndexShift           Self;
  typedef RegistrationParameterScalesFromShiftBase< TMetric > Superclass;
  typedef SmartPointer<Self>                                  Pointer;
  typedef SmartPointer<const Self>                            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self );

  /** Run-time type information (and related methods). */
  itkTypeMacro( RegistrationParameterScalesFromIndexShift, RegistrationParameterScalesFromShiftBase );

  /** Type of scales */
  typedef typename Superclass::ScalesType                ScalesType;
  /** Type of parameters of the optimizer */
  typedef typename Superclass::ParametersType            ParametersType;
  /** Type of float */
  typedef typename Superclass::FloatType                 FloatType;

  typedef typename Superclass::VirtualPointType          VirtualPointType;
  typedef typename Superclass::VirtualIndexType          VirtualIndexType;
  typedef typename Superclass::MovingTransformType       MovingTransformType;
  typedef typename Superclass::FixedTransformType        FixedTransformType;
  typedef typename Superclass::JacobianType              JacobianType;
  typedef typename Superclass::VirtualImageConstPointer  VirtualImageConstPointer;

  typedef typename TMetric::FixedImageType               FixedImageType;
  typedef typename TMetric::MovingImageType              MovingImageType;

  typedef typename FixedImageType::ConstPointer          FixedImageConstPointer;
  typedef typename MovingImageType::ConstPointer         MovingImageConstPointer;

  typedef typename FixedImageType::PointType             FixedPointType;
  typedef typename FixedImageType::IndexType             FixedIndexType;
  typedef typename FixedImageType::PointValueType        FixedPointValueType;

  typedef typename itk::ContinuousIndex< FixedPointValueType, FixedImageType::ImageDimension >          FixedContinuousIndexType;

  typedef typename MovingImageType::PointType            MovingPointType;
  typedef typename MovingImageType::IndexType            MovingIndexType;
  typedef typename MovingImageType::PointValueType       MovingPointValueType;

  typedef typename itk::ContinuousIndex< MovingPointValueType, MovingImageType::ImageDimension >         MovingContinuousIndexType;

protected:
  RegistrationParameterScalesFromIndexShift();
  ~RegistrationParameterScalesFromIndexShift() ITK_OVERRIDE {};

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  virtual void ComputeSampleShifts(const ParametersType &deltaParameters, ScalesType &localShifts) ITK_OVERRIDE;

  template<typename TContinuousIndexType>
  void TransformPointToContinuousIndex(const VirtualPointType &point, TContinuousIndexType &mappedIndex);

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(RegistrationParameterScalesFromIndexShift);

  template <typename TTransform>
  void ComputeSampleShiftsInternal(const ParametersType &deltaParameters, ScalesType &localShifts);

}; //class RegistrationParameterScalesFromIndexShift

}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRegistrationParameterScalesFromIndexShift.hxx"
#endif

#endif /* itkRegistrationParameterScalesFromIndexShift_h */
