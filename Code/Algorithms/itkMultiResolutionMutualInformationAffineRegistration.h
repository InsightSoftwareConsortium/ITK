/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionMutualInformationAffineRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkMultiResolutionMutualInformationAffineRegistration_h
#define __itkMultiResolutionMutualInformationAffineRegistration_h

#include "itkImageToImageAffineMutualInformationGradientDescentRegistration.h"
#include "itkMultiResolutionImagePyramid.h"
#include "itkMultiResolutionRegistration.h"

#include <vector>

namespace itk
{

/**
 * \class MultiResolutionMutualInformationAffineRegistrationTraits
 * \brief Trait class which defines the types for
 * MultiResolutionRegistration
 *
 * This class defines the traits for the
 * MultiResolutionMutualInformationAffineRegistration algorithm.
 *
 * This class illustrates the use of the multiresolution framework.
 *
 */
template <class TReference, class TTarget>
class ITK_EXPORT 
MultiResolutionMutualInformationAffineRegistrationTraits
{
public:
  /**
   * Type of the reference image
   */
  typedef TReference ReferenceType;

  /**
   * Type of the target image
   */
  typedef TTarget TargetType;

  /**
   * Type of the registration method
   */
  typedef ImageToImageAffineMutualInformationGradientDescentRegistration<
    ReferenceType, TargetType> RegistrationType;

  /**
   * Type of the reference image pyramid
   */
  typedef MultiResolutionImagePyramid<
    ReferenceType, ReferenceType> ReferencePyramidType;

  /**
   * Type fo the target image pyramid
   */
  typedef MultiResolutionImagePyramid<
     TargetType, TargetType> TargetPyramidType;


};


/**
 * \class MultiResolutionMutualInformationAffineRegistration
 * \brief Affinely register two 3D volumes using a multi-resolution mutual information algorithm
 *
 * MultiResolutionMutualInformationAffineRegistration performs an
 * affine registration between a 3D target image and 3D reference image using 
 * a multi-resolution version of 
 * ImageToImageAffineMutualInformationGradientDescentRegistration.
 * Refer to the documentation of
 * ImageToImageAffineMutualInformationGradientDescentRegistration for
 * details on the underlying algorithm.
 *
 * The two input images are defined using the methods SetTarget()
 * and SetReference().  The number of resolution levels is specified by
 * SetNumberOfLevels().  GetReferencePyramid() and GetTargetPyramid() 
 * respectively return pointers to the reference and target pyramids.
 * Refer to the documentation of MultiResolutionImagePyramid for details on
 * how to set the multi-resolution downsampling schedule.
 *
 * For each resolution level, the user must define the number of 
 * iterations, the learning rate and translation scale to be used
 * in the registration. These parameters are set by
 * SetNumberOfIterations(), SetLearningRates(), SetTranslationScales.
 * Refer to ImageToImageAffineMutualInformationGradientDescentRegistration
 * for details on these parameters and how to set them.
 *
 * GetInternalRegistrationMethod() returns a pointer to the internal
 * registration object.  Initial transform parameters can be set with: 
 *
 * GetInternalRegistrationMethod()->SetParameters( params );
 *
 * The final transform parameters can be obtained via:
 *
 * GetInternalRegistrationMethod()->GetParameters();
 *
 *
 * This class is templated over the reference image type and the
 * the target image type.
 *
 * FIXME: The caveat below holds for rigid registration; does it
 * also hold for affine registration?
 * Caveat: this class only work for 3D reference and target images
 *
 * \sa MultiResolutionMutualInformationRigidRegistration
 * \sa ImageToImageAffineMutualInformationGradientDescentRegistration
 * \sa MultiResolutionImagePyramid
 *
 * \ingroup AffineImageRegistration
 */
template <class TReference, class TTarget>
class ITK_EXPORT
MultiResolutionMutualInformationAffineRegistration :
public MultiResolutionRegistration<
  MultiResolutionMutualInformationAffineRegistrationTraits<
    TReference, TTarget> >
{
public:

  /**
   * Standard "Self" typedef
   */
  typedef MultiResolutionMutualInformationAffineRegistration Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef MultiResolutionRegistration<
    MultiResolutionMutualInformationAffineRegistrationTraits<
      TReference, TTarget> >  Superclass;

  /**
   * Smart pointer typedef support
   */
   typedef SmartPointer<Self>   Pointer;
   typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
   itkNewMacro(Self);

  /**
   *  Type of the Reference
   */
   typedef TReference  ReferenceType;

  /**
   *  Type of the Target
   */
   typedef TTarget TargetType;

  /**
   * RegistrationType typedef support
   */
  typedef typename Superclass::RegistrationType RegistrationType;

  /**
   * TargetImageDimension enumeration
   */
  enum{ TargetImageDimension = Superclass::TargetImageDimension };

  /**
   * Set the number of multi-resolution computation levels.
   * This sets the levels for both the target and reference
   * pyramids.
   * If the argument is less than 1, it is clamped to the
   * value of 1.
   */
  virtual void SetNumberOfLevels(unsigned int num);

  /**
   * Set the number of iterations for each level.
   * The number of levels must be set before calling this method.
   */
  itkSetVectorMacro( NumberOfIterations, unsigned int, this->GetNumberOfLevels() );

  /**
   * Set the learning rate for each level.
   * The number of levels must be set before calling this method.
   */
  itkSetVectorMacro( LearningRates, double, this->GetNumberOfLevels() );

  /**
   * Set the translation scales for each level.
   * The number of levels must be set before calling this method.
   */
  itkSetVectorMacro( TranslationScales, double, this->GetNumberOfLevels() );

protected:

  MultiResolutionMutualInformationAffineRegistration();
  ~MultiResolutionMutualInformationAffineRegistration(){};
  MultiResolutionMutualInformationAffineRegistration(const Self&);
  const Self & operator=(const Self&);
  void PrintSelf(std::ostream&os, Indent indent) const;

  /**
   * Initialize the internal registration algorithm at the
   * specified level. Subclasses may override this method.
   */
  virtual void OneLevelPreRegistration(unsigned int level);

  /**
   * Post registration routine. Subclasse may override this method.
   */
  virtual void OneLevelPostRegistration(unsigned int level);

private:

  std::vector<double>           m_LearningRates;
  std::vector<unsigned int>     m_NumberOfIterations;
  std::vector<double>           m_TranslationScales;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiResolutionMutualInformationAffineRegistration.txx"
#endif

#endif
