/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionMutualInformationRigidRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultiResolutionMutualInformationRigidRegistration_h
#define __itkMultiResolutionMutualInformationRigidRegistration_h

#ifdef COMPILE_LONG_NAMES_REGISTRATION

#include "itkImageToImageRigidMutualInformationGradientDescentRegistration.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkMultiResolutionRegistration.h"

#include <vector>

namespace itk
{

/** \class MultiResolutionMutualInformationRigidRegistrationTraits
 * \brief Trait class which defines the types for
 * MultiResolutionRegistration
 *
 * This class contains define the traits for 
 * MultiResolutionMutualInformationRigidRegistration algorithm.
 *
 * This class illustrates the use of the multiresolution framework.
 */
template <class TReference, class TTarget>
class ITK_EXPORT 
MultiResolutionMutualInformationRigidRegistrationTraits
{
public:
  /** Type of the reference image. */
  typedef TReference ReferenceType;

  /** Type of the target image. */
  typedef TTarget TargetType;

  /** Type of the registration method. */
  typedef ImageToImageRigidMutualInformationGradientDescentRegistration<
    ReferenceType, TargetType> RegistrationType;

  /** Type of the reference image pyramid. */
  typedef RecursiveMultiResolutionPyramidImageFilter<
    ReferenceType, ReferenceType> ReferencePyramidType;

  /** Type fo the target image pyramid. */
  typedef RecursiveMultiResolutionPyramidImageFilter<
     TargetType, TargetType> TargetPyramidType;
};

/** \class MultiResolutionMutualInformationRigidRegistration
 * \brief Rigidly register two 3D volumes using a multi-resolution mutual
 * information algorithm
 *
 * MultiResolutionMutualInformationRigidRegistration performs an
 * rigid registration between a 3D target image and 3D reference image using 
 * a multi-resolution version of 
 * ImageToImageRigidMutualInformationGradientDescentRegistration.
 * Refer to documentation of
 * ImageToImageRigidMutualInformationGradientDescentRegistration for
 * details on the underlying algorithm.
 *
 * The two input images are defined via methods SetTarget()
 * and SetReference(). The number of resolution levels is specified via
 * SetNumberOfLevels(). GetReferencePyramid() and GetTargetPyramid() 
 * respectively returns pointers to the reference and target pyramids.
 * Refer to the documentation of MultiResolutionPyramidImageFilter for details on
 * how to set the multi-resolution downsampling schedule.
 *
 * For each resolution level, the user must define the number of 
 * iterations and the learning rate to be used
 * in the registration. These parameters are set via
 * SetNumberOfIterations() and SetLearningRates().
 * Refer to ImageToImageRigidMutualInformationGradientDescentRegistration
 * for details on these parameters and how to set them.
 *
 * GetInternalRegistrationMethod() returns a pointer to the internal
 * registration object. Initial transform parameters can be set via: 
 *
 * GetInternalRegistrationMethod()->SetParameters( params );
 *
 * The final transform parameters can be obtained via:
 *
 * GetInternalRegistrationMethod()->GetParameters();
 *
 * This class is templated over the reference image type and the
 * the target image type.
 *
 * Caveat: this class only work for 3D reference and target images
 *
 * \sa ImageToImageRigidMutualInformationGradientDescentRegistration
 * \sa MultiResolutionPyramidImageFilter
 *
 * \ingroup RigidImageRegistration 
 */
template <class TReference, class TTarget>
class ITK_EXPORT
MultiResolutionMutualInformationRigidRegistration :
public MultiResolutionRegistration<
  MultiResolutionMutualInformationRigidRegistrationTraits<
    TReference, TTarget> >
{
public:
  /** Standard class typedefs. */
  typedef MultiResolutionMutualInformationRigidRegistration Self;
  typedef MultiResolutionRegistration<
    MultiResolutionMutualInformationRigidRegistrationTraits<
      TReference, TTarget> >  Superclass;
   typedef SmartPointer<Self>   Pointer;
   typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
   itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiResolutionMutualInformationRigidRegistration, 
    MultiResolutionRegistration);

  /**  Type of the reference. */
   typedef TReference  ReferenceType;

  /**  Type of the target. */
   typedef TTarget TargetType;

  /** RegistrationType typedef support. */
  typedef typename Superclass::RegistrationType RegistrationType;

  /** TargetImageDimension enumeration. */
  enum{ TargetImageDimension = Superclass::TargetImageDimension };

  /** Set the number of multi-resolution computation levels.  This sets the
   * levels for both the target and reference pyramids.  If the argument is
   * less than 1, it is clamped to the value of 1. */
  virtual void SetNumberOfLevels(unsigned int num);

  /** Set the number of iterations for each level.  The number of levels must
   * be set before calling this method. */
  itkSetVectorMacro( NumberOfIterations, unsigned int, this->GetNumberOfLevels() );

  /** Get the number of iterations for each level. */
  virtual const unsigned int * GetNumberOfIterations()
    { return &(m_NumberOfIterations[0]); }

  /** Set the learning rate for each level.  The number of levels must be set
   * before calling this method. */
  itkSetVectorMacro( LearningRates, double, this->GetNumberOfLevels() );

  /** Get the learning rate for each level. */
  virtual const double * GetLearningRates()
    { return &(m_LearningRates[0]); }

protected:
  MultiResolutionMutualInformationRigidRegistration();
  ~MultiResolutionMutualInformationRigidRegistration(){};
  void PrintSelf(std::ostream&os, Indent indent) const;

  /** Initialize the internal registration algorithm at the
   * specified level. Subclasses may override this method. */
  virtual void OneLevelPreRegistration(unsigned int level);

  /** Post registration routine. Subclasse may override this method. */
  virtual void OneLevelPostRegistration(unsigned int level);

private:
  MultiResolutionMutualInformationRigidRegistration(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  std::vector<double>           m_LearningRates;
  std::vector<unsigned int>     m_NumberOfIterations;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiResolutionMutualInformationRigidRegistration.txx"
#endif

#endif
#endif
