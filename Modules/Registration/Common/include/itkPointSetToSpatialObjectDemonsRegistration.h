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
#ifndef itkPointSetToSpatialObjectDemonsRegistration_h
#define itkPointSetToSpatialObjectDemonsRegistration_h

#include "itkProcessObject.h"
#include "itkImage.h"
#include "itkPointSetToImageMetric.h"
#include "itkSingleValuedNonLinearOptimizer.h"

namespace itk
{
/** \class PointSetToSpatialObjectDemonsRegistration
 * \brief Implementation of Demons Registration between a PointSet and a SpatialObject
 *
 * The simplest case of Demons registration suggested by P. Thirion in his
 * paper[1] is defined by a Model and Scene. The Model should be able to
 * respond to the queries of whether a point is inside or outside of the object
 * of interest, while the Scene provides a number of points (the Demons) with
 * vector indicating the direction of inside-outside of the equivalent object
 * of interest. In this implementation, the Scene is represented by an
 * itk::PointSet that has Vectors as PixelData. These vectors should be
 * provided by the user of this class and must be pointing outside of the
 * object of interest. The Model is naturally represented by a
 * itk::SpatialObject since its interface responds to the IsInside() method.
 *
 * This class is intended to be derived in order to define the method that will
 * update the transform. Such method will be specific for the particular type of
 * transform used.
 *
 * [1] J-P. Thirion "Image matching as a Diffusion Process: and Analogy with
 * Maxwell's Demons", Medical Image Analysis, 1998, Vol. 2, No. 3, pp 243-260.
 *
 * \ingroup RegistrationFilters
 * \ingroup ITKRegistrationCommon
 */
template< typename TFixedPointSet, typename TMovingSpatialObject >
class ITK_TEMPLATE_EXPORT PointSetToSpatialObjectDemonsRegistration:public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef PointSetToSpatialObjectDemonsRegistration Self;
  typedef ProcessObject                             Superclass;
  typedef SmartPointer< Self >                      Pointer;
  typedef SmartPointer< const Self >                ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(PointSetToSpatialObjectDemonsRegistration, ProcessObject);

  /**  Type of the Fixed PointSet. */
  typedef          TFixedPointSet                  FixedPointSetType;
  typedef typename FixedPointSetType::ConstPointer FixedPointSetConstPointer;

  /**  Type of the Moving image. */
  typedef          TMovingSpatialObject                  MovingSpatialObjectType;
  typedef typename MovingSpatialObjectType::ConstPointer MovingSpatialObjectConstPointer;

  /** Set/Get the Fixed image. */
  itkSetConstObjectMacro(FixedPointSet, FixedPointSetType);
  itkGetConstObjectMacro(FixedPointSet, FixedPointSetType);

  /** Set/Get the Moving image. */
  itkSetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);
  itkGetConstObjectMacro(MovingSpatialObject, MovingSpatialObjectType);

#ifdef ITKV3_COMPATIBILITY
  /** Method that initiates the registration. This will Initialize and ensure
   * that all inputs the registration needs are in place, via a call to
   * Initialize() will then start the optimization process via a call to
   * StartOptimization()
   * StartRegistration is an old API from before
   * ImageRegistrationMethod was a subclass of ProcessObject.
   * Historically, one could call StartRegistration() instead of
   * calling Update().  However, when called directly by the user, the
   * inputs to ImageRegistrationMethod may not be up to date.  This
   * may cause an unexpected behavior.
   *
   * Since we cannot eliminate StartRegistration for backward
   * compatibility reasons, we check whether StartRegistration was
   * called directly or whether Update() (which in turn called
   * StartRegistration()). */
  void StartRegistration(void) { this->Update(); }
#endif

protected:
  PointSetToSpatialObjectDemonsRegistration();
  virtual ~PointSetToSpatialObjectDemonsRegistration() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(PointSetToSpatialObjectDemonsRegistration);

  MovingSpatialObjectConstPointer m_MovingSpatialObject;
  FixedPointSetConstPointer       m_FixedPointSet;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPointSetToSpatialObjectDemonsRegistration.hxx"
#endif

#endif
