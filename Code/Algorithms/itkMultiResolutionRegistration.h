/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultiResolutionRegistration_h
#define __itkMultiResolutionRegistration_h

#include "itkProcessObject.h"
#include "vnl/vnl_matrix.h"

namespace itk
{

/** \class MultiResolutionRegistration
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
 *    MultiResolutionPyramidImageFilter.
 * e) the reference image pyramid type
 *
 * \ingroup RegistrationFilters
 * \ingroup ImageRegistration
 *
 */
template <class TTraits>
class ITK_EXPORT MultiResolutionRegistration : public ProcessObject
{
public:
  /** Standard class typedefs. */
  typedef MultiResolutionRegistration  Self;
  typedef ProcessObject  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiResolutionRegistration, ProcessObject);

  /** RegistrationType typedef support. */
  typedef typename TTraits::RegistrationType RegistrationType;

  /** RegistrationPointer typedef support. */
  typedef typename RegistrationType::Pointer  RegistrationPointer;

  /** TargetType typedef support. */
  typedef typename TTraits::TargetType  TargetType;

  /** TargetPointer typedef support. */
  typedef typename TargetType::ConstPointer TargetConstPointer;

  /** ReferenceType typedef support. */
  typedef typename TTraits::ReferenceType  ReferenceType;

  /** ReferencePointer typedef support. */
  typedef typename ReferenceType::ConstPointer  ReferenceConstPointer;

  /** TargetImageDimension enumeration. */
  enum{ TargetImageDimension = TargetType::ImageDimension };

  /** ReferenceImageDimension enumeration. */
  enum{ ReferenceImageDimension = ReferenceType::ImageDimension };

  /** TargetPyramid typedef support. */
  typedef typename TTraits::TargetPyramidType TargetPyramidType;

  /** TargetPyramidPointer typedef support. */
  typedef typename TargetPyramidType::Pointer TargetPyramidPointer;

  /** ReferencePyramid typedef support. */
  typedef typename TTraits::ReferencePyramidType ReferencePyramidType;

  /** ReferencePyramidPointer typedef support. */
  typedef typename ReferencePyramidType::Pointer ReferencePyramidPointer;

  /** Set the number of multi-resolution computation levels.  This sets the
   * levels for both the target and reference pyramids.  If the argument is
   * less than 1, it is clamped to the value of 1. */
  virtual void SetNumberOfLevels(unsigned int num);

  /** Get the number of multi-resolution computation levels. */
  itkGetConstMacro(NumberOfLevels, unsigned int);

  /** Get the current computation level. */
  itkGetConstMacro(CurrentLevel, unsigned int);

  /** Set/Get the target. */
  void SetTarget( TargetType * target );
  TargetConstPointer GetTarget( void );

  /** Set/Get the reference. */
  void SetReference( ReferenceType * reference );
  ReferenceConstPointer GetReference( void );

  /** Get the target pyramid */
  TargetPyramidPointer  GetTargetPyramid(void)
    { return m_TargetPyramid; }

  /** Get the reference pyramid. */
  ReferencePyramidPointer GetReferencePyramid(void)
    { return m_ReferencePyramid; }

  /** Get the internal registrator. */
  RegistrationPointer GetInternalRegistrationMethod(void)
    { return m_InternalRegistrationMethod; }

  /** Start the registration. */
  void StartRegistration(void);

protected:
  MultiResolutionRegistration();
  ~MultiResolutionRegistration() {};
  void PrintSelf(std::ostream&os, Indent indent) const;

  /** Peform the necessary initialization before running the internal
   * registration algorithm at this level.  Subclasses should override this
   * method. */
  virtual void OneLevelPreRegistration(unsigned int level){};

  /** Peform the necessary operations after running the internal registration
   * algorithm at this level.  Subclasses should override this method. */
  virtual void OneLevelPostRegistration(unsigned int level){};
 
  /** Run the internal registration algorithm at the specified
   * level. Subclasses may override this method. */
  virtual void OneLevelRegistration(unsigned int level);

private:
  MultiResolutionRegistration(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
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


