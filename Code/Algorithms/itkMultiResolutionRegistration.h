/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionRegistration.h
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
#ifndef __itkMultiResolutionRegistration_h
#define __itkMultiResolutionRegistration_h

#include "itkProcessObject.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/**
 * \class MultiResolutionRegistration
 * \brief Framework for performing multiresolution registration.
 *
 * MultiResolutionRegistration provides a generic framework to
 * peform multi-resolution registration. This class over a set
 * of traits.
 * 
 * The traits must consist of:
 *
 * a) the target image type
 * b) the reference image type
 * c) a registration method which register a reference image
 *    onto a target image. It must have method StartRegistration()
 *    which is used to evoke the registration at each resolution level.
 *    For example, see class RegistrationMethod.
 * d) the target image pyramid type. For example, see class
 *    MultiResolutionImagePyramid.
 * e) the reference image pyramid type
 *
 */
template <class TTraits>
class ITK_EXPORT MultiResolutionRegistration : public ProcessObject
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef MultiResolutionRegistration  Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ProcessObject  Superclass;

  /**
   * SmartPointer typedef support
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory
   */
  itkNewMacro(Self);

  /**
   * Run-time type information (and related methods)
   */
  itkTypeMacro(MultiResolutionRegistration, ProcessObject);

  /**
   * RegistrationType typedef support
   */
  typedef typename TTraits::RegistrationType RegistrationType;

  /**
   * RegistrationPointer typedef support
   */
  typedef typename RegistrationType::Pointer  RegistrationPointer;

  /**
   * TargetType typedef support
   */
  typedef typename TTraits::TargetType  TargetType;

  /**
   * TargetPointer typedef support
   */
  typedef typename TargetType::ConstPointer TargetConstPointer;

  /**
   * ReferenceType typedef support
   */
  typedef typename TTraits::ReferenceType  ReferenceType;

  /**
   * ReferencePointer typedef support
   */
  typedef typename ReferenceType::ConstPointer  ReferenceConstPointer;

  /**
   * TargetImageDimension enumeration
   */
  enum{ TargetImageDimension = TargetType::ImageDimension };

  /**
   * ReferenceImageDimension enumeration
   */
  enum{ ReferenceImageDimension = ReferenceType::ImageDimension };

  /**
   * TargetPyramid typedef support
   */
  typedef typename TTraits::TargetPyramidType TargetPyramidType;

  /**
   * TargetPyramidPointer typedef support
   */
  typedef typename TargetPyramidType::Pointer TargetPyramidPointer;

  /**
   * ReferencePyramid typedef support
   */
  typedef typename TTraits::ReferencePyramidType ReferencePyramidType;

  /**
   * ReferencePyramidPointer typedef support
   */
  typedef typename ReferencePyramidType::Pointer ReferencePyramidPointer;

  /**
   * Set the number of multi-resolution computation levels.
   * This sets the levels for both the target and reference
   * pyramids.
   * If the argument is less than 1, it is clamped to the
   * value of 1.
   */
  virtual void SetNumberOfLevels(unsigned int num);

  /**
   * Get the number of multi-resolution computation levels
   */
  itkGetConstMacro(NumberOfLevels, unsigned int);

  /**
   * Set the target
   */
  void SetTarget( TargetType * target );

  /**
   * Get the target
   */
  TargetConstPointer GetTarget( void );

  /**
   * Set the reference
   */
  void SetReference( ReferenceType * reference );

  /**
   * Get the reference
   */
  ReferenceConstPointer GetReference( void );

  /**
   * Get the target pyramid
   */
  TargetPyramidPointer  GetTargetPyramid(void)
  { return m_TargetPyramid; }

  /**
   * Get the reference pyramid
   */
  ReferencePyramidPointer GetReferencePyramid(void)
  { return m_ReferencePyramid; }

  /**
   * Get the internal registrator
   */
  RegistrationPointer GetInternalRegistrationMethod(void)
  { return m_InternalRegistrationMethod; }

  /**
   * Start the registration
   */
  void StartRegistration(void);

protected:
  MultiResolutionRegistration();
  ~MultiResolutionRegistration() {};
  MultiResolutionRegistration(const Self&) {};
  void operator=(const Self&) {};
  void PrintSelf(std::ostream&os, Indent indent);

  /**
   * Peform the necessary initialization before
   * running the internal registration algorithm at this level.
   * Subclasses should override this method.
   */
  virtual void OneLevelPreRegistration(unsigned int level){};

  /**
   * Peform the necessary operations after
   * running the internal registration algorithm at this level.
   * Subclasses should override this method.
   */
  virtual void OneLevelPostRegistration(unsigned int level){};
 
  /**
   * Run the internal registration algorithm at the
   * specified level. Subclasses may override this method.
   */
  virtual void OneLevelRegistration(unsigned int level);

private:

  unsigned int               m_NumberOfLevels;
  unsigned int               m_CurrentLevel;
  RegistrationPointer        m_InternalRegistrationMethod;
  TargetPyramidPointer       m_TargetPyramid;
  ReferencePyramidPointer    m_ReferencePyramid;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiResolutionRegistration.txx"
#endif

#endif


