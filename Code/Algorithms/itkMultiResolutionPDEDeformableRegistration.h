/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionPDEDeformableRegistration.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMultiResolutionPDEDeformableRegistration_h
#define __itkMultiResolutionPDEDeformableRegistration_h

#include "itkImage.h"
#include "itkImageToImageFilter.h"
#include "itkPDEDeformableRegistrationFilter.h"
#include "itkDemonsRegistrationFilter.h"
#include "itkRecursiveMultiResolutionPyramidImageFilter.h"
#include "itkVectorExpandImageFilter.h"

#include <vector>

namespace itk
{
/**
 * \class MultiResolutionPDEDeformableRegistration
 * \brief Framework for perfoming multi-resolution PDE deformable registration.
 *
 * MultiResolutionPDEDeformableRegistration provides a generic framework
 * to peform multi-resolution deformable registration.
 *
 * At each resolution level a PDEDeformableRegistrationFilter is used
 * to register two images by computing the deformation field which will 
 * map a reference image onto a target image.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the target image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * The internal PDEDeformationRegistrationFilter can be set using
 * SetRegistrationFilter. By default a DemonsRegistrationFilter is used.
 *
 * The input reference and target images are set via methods SetReference
 * and SetTarget respectively. An initial deformation field maybe set via
 * SetInitialDeformationField or SetInput. If no initial field is set
 * a zero field is used as the initial condition.
 *
 * MultiResolutionPyramidImageFilters are used to downsample the target
 * and reference images. A VectorExpandImageFilter is used to upsample
 * the deformation as we move from a coarse to fine solution.
 *
 * This class is templated over the Reference image type, Target image type,
 * the Deformation Field type.
 *
 * \warning This class assumes that the reference, target and deformation
 * field image types all have the same number of dimensions.
 *
 * \sa PDEDeformableRegistrationFilter
 * \sa DemonsRegistrationFilter
 * \sa MultiResolutionPyramidImageFilter
 * \sa VectorExpandImageFilter
 *
 * The current implementation of this class does not support streaming.
 *
 * \ingroup DeformableImageRegistration
 */
template <class TReference, class TTarget, class TDeformationField>
class MultiResolutionPDEDeformableRegistration :
  public ImageToImageFilter <TDeformationField, TDeformationField>
{
public:
  /** Standard class typedefs */
  typedef MultiResolutionPDEDeformableRegistration Self;
  typedef ImageToImageFilter<TDeformationField, TDeformationField>
    Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( MultiResolutionPDEDeformableRegistration, 
    ImageToImageFilter );

  /** Reference image type. */
  typedef TReference ReferenceType;
  typedef typename ReferenceType::Pointer ReferencePointer;

  /** Target image type. */
  typedef TTarget TargetType;
  typedef typename TargetType::Pointer TargetPointer;

  /** Deformation field image type. */
  typedef TDeformationField DeformationFieldType;
  typedef typename DeformationFieldType::Pointer DeformationFieldPointer;

  /** ImageDimension. */
  enum { ImageDimension = TargetType::ImageDimension };

  /** Internal float image type. */
  typedef Image<float,ImageDimension> FloatImageType;

  /** The internal registration type. */
  typedef PDEDeformableRegistrationFilter<
    FloatImageType, FloatImageType, DeformationFieldType > RegistrationType;
  typedef typename RegistrationType::Pointer RegistrationPointer;

  /** The default registration type. */
  typedef DemonsRegistrationFilter<
    FloatImageType, FloatImageType, DeformationFieldType > DefaultRegistrationType;

  /** The reference multi-resolution image pyramid type. */
  typedef RecursiveMultiResolutionPyramidImageFilter<
    ReferenceType, FloatImageType > ReferencePyramidType;
  typedef typename ReferencePyramidType::Pointer ReferencePyramidPointer;

  /** The target multi-resolution image pyramid type. */
  typedef RecursiveMultiResolutionPyramidImageFilter<
    TargetType, FloatImageType > TargetPyramidType;
  typedef typename TargetPyramidType::Pointer TargetPyramidPointer;
   
  /** The deformation field expander type. */
  typedef VectorExpandImageFilter<
    DeformationFieldType, DeformationFieldType > FieldExpanderType;
  typedef typename FieldExpanderType::Pointer  FieldExpanderPointer;

  /** Set the reference image. */
  virtual void SetReference( ReferenceType * ptr );

  /** Get the reference image. */
  ReferencePointer GetReference();

  /** Set the target image. */
  virtual void SetTarget( TargetType * ptr );

  /** Get the target image. */
  TargetPointer GetTarget();

  /** Set initial deformation field. */
  virtual void SetInitialDeformationField( DeformationFieldType * ptr )
    {
    itkExceptionMacro( << "This feature not implemented yet"  );
    // this->SetInput( ptr ); 
    }

  /** Get output deformation field. */
  DeformationFieldPointer GetDeformationField()
    { return this->GetOutput(); }

  /** Set the internal registrator. */
  itkSetObjectMacro( RegistrationFilter, RegistrationType );

  /** Get the internal registrator. */
  itkGetObjectMacro( RegistrationFilter, RegistrationType );
  
  /** Set the reference image pyramid. */
  itkSetObjectMacro( ReferencePyramid, ReferencePyramidType );

  /** Get the reference image pyramid. */
  itkGetObjectMacro( ReferencePyramid, ReferencePyramidType );

  /** Set the target image pyramid. */
  itkSetObjectMacro( TargetPyramid, TargetPyramidType );

  /** Get the target image pyramid. */
  itkGetObjectMacro( TargetPyramid, TargetPyramidType );

  /** Set number of multi-resolution levels. */
  virtual void SetNumberOfLevels( unsigned int num );

  /** Get number of multi-resolution levels. */
  itkGetMacro( NumberOfLevels, unsigned int );

  /** Get the current resolution level being processed. */
  itkGetMacro( CurrentLevel, unsigned int );

  /** Set number of iterations per multi-resolution levels. */
  itkSetVectorMacro( NumberOfIterations, unsigned int, m_NumberOfLevels );

  /** Get number of iterations per multi-resolution levels. */
  virtual const unsigned int * GetNumberOfIterations() const
    { return &(m_NumberOfIterations[0]); }

protected:
  MultiResolutionPDEDeformableRegistration();
  ~MultiResolutionPDEDeformableRegistration() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Generate output data by performing the registration
   * at each resolution level. */
  virtual void GenerateData();

  /** The current implementation of this class does not support
   * streaming. As such it requires the largest possible region
   * for the reference, target and input deformation field. */
  virtual void GenerateInputRequestedRegion();

  /** By default, the output deformation field has the same
   * spacing, origin and LargestPossibleRegion as the input/initial
   * deformation field.
   *
   * If the initial deformation field is not set, the output
   * information is copoed from the target image. */
  virtual void GenerateOutputInformation();

  /** The current implementation of this class does not supprot
   * streaming. As such it produces the output for the largest
   * possible region. */
  virtual void EnlargeOutputRequestedRegion( DataObject *ptr );

private:
  MultiResolutionPDEDeformableRegistration(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  RegistrationPointer        m_RegistrationFilter;
  ReferencePyramidPointer    m_ReferencePyramid;
  TargetPyramidPointer       m_TargetPyramid;
  FieldExpanderPointer       m_FieldExpander;

  unsigned int               m_NumberOfLevels;
  unsigned int               m_CurrentLevel;
  std::vector<unsigned int>  m_NumberOfIterations;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMultiResolutionPDEDeformableRegistration.txx"
#endif


#endif
