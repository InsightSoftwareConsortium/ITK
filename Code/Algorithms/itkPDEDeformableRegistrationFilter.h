/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPDEDeformableRegistrationFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPDEDeformableRegistrationFilter_h_
#define _itkPDEDeformableRegistrationFilter_h_

#include "itkDenseFiniteDifferenceImageFilter.h"
#include "itkPDEDeformableRegistrationFunction.h"

namespace itk {

/**
 * \class PDEDeformableRegistrationFilter
 * \brief Deformably register two images using a PDE algorithm.
 *
 * PDEDeformableRegistrationFilter is a base case for filter implementing
 * a PDE deformable algorithm that register two images by computing the 
 * deformation field which will map a moving image onto a fixed image.
 *
 * A deformation field is represented as a image whose pixel type is some
 * vector type with at least N elements, where N is the dimension of
 * the fixed image. The vector type must support element access via operator
 * []. It is assumed that the vector elements behave like floating point
 * scalars.
 *
 * This class is templated over the fixed image type, moving image type
 * and the deformation Field type.
 *
 * The input fixed and moving images are set via methods SetFixedImage
 * and SetMovingImage respectively. An initial deformation field maybe set via
 * SetInitialDeformationField or SetInput. If no initial field is set,
 * a zero field is used as the initial condition.
 *
 * The output deformation field can be obtained via methods GetOutput
 * or GetDeformationField.
 *
 * The PDE algorithm is run for a user defined number of iterations.
 * Typically the PDE algorithm requires period Gaussin smoothing of the
 * deformation field to enforce an elastic-like condition. The amount
 * of smoothing is governed by a set of user defined standard deviations
 * (one for each dimension).
 *
 * This class make use of the finite difference solver hierarchy. Update
 * for each iteration is computed using a PDEDeformableRegistrationFunction.
 *
 * \warning This filter assumes that the fixed image type, moving image type
 * and deformation field type all have the same number of dimensions.
 * 
 * \sa PDEDeformableRegistrationFunction.
 * \ingroup DeformableImageRegistration
 */
template<class TFixedImage, class TMovingImage, class TDeformationField>
class ITK_EXPORT PDEDeformableRegistrationFilter : 
    public DenseFiniteDifferenceImageFilter<TDeformationField,TDeformationField>
{
public:
  /** Standard class typedefs. */
  typedef PDEDeformableRegistrationFilter    Self;
  typedef DenseFiniteDifferenceImageFilter<
    TDeformationField,TDeformationField>    Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( PDEDeformableRegistrationFilter, 
                DenseFiniteDifferenceImageFilter );

  /** FixedImage image type. */
  typedef TFixedImage   FixedImageType;
  typedef typename FixedImageType::Pointer  FixedImagePointer;
  typedef typename FixedImageType::ConstPointer  FixedImageConstPointer;

  /** MovingImage image type. */
  typedef TMovingImage    MovingImageType;
  typedef typename MovingImageType::Pointer  MovingImagePointer;
  typedef typename MovingImageType::ConstPointer  MovingImageConstPointer;
  
  /** Deformation field type. */
  typedef TDeformationField    DeformationFieldType;
  typedef typename DeformationFieldType::Pointer  DeformationFieldPointer;

  /** Types inherithed from the superclass */
  typedef typename Superclass::OutputImageType    OutputImageType;

  /** FiniteDifferenceFunction type. */
  typedef typename Superclass::FiniteDifferenceFunctionType
  FiniteDifferenceFunctionType;

  /** PDEDeformableRegistrationFilterFunction type. */
  typedef PDEDeformableRegistrationFunction<FixedImageType,MovingImageType,
                                            DeformationFieldType>  PDEDeformableRegistrationFunctionType;

  /** Inherit some enums and typedefs from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** Set the fixed image. */
  void SetFixedImage( const FixedImageType * ptr );

  /** Get the fixed image. */
  const FixedImageType * GetFixedImage(void);

  /** Set the moving image. */
  void SetMovingImage( const MovingImageType * ptr );

  /** Get the moving image. */
  const MovingImageType * GetMovingImage(void);

  /** Set initial deformation field. */
  void SetInitialDeformationField( DeformationFieldType * ptr )
  { this->SetInput( ptr ); }

  /** Get output deformation field. */
  DeformationFieldType * GetDeformationField()
  { return this->GetOutput(); }

  /** Set the number of iterations to be performed. */
  itkSetMacro(NumberOfIterations, unsigned int);

  /** Get the number of iterations to be performed. */
  itkGetMacro(NumberOfIterations, unsigned int);

  /** Set the Gaussian smoothing standard deviations. The
   * values are set with respect to pixel coordinates. */
  itkSetVectorMacro( StandardDeviations, double, ImageDimension );
  virtual void SetStandardDeviations( double value );

  /** Get the Gaussian smoothing standard deviations. */
  const double * GetStandardDeviations(void) 
  { return (double *) m_StandardDeviations; }

protected:
  PDEDeformableRegistrationFilter();
  ~PDEDeformableRegistrationFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Supplies the halting criteria for this class of filters.  The
   * algorithm will stop after a user-specified number of iterations. */
  virtual bool Halt()
  {
    if (this->GetElapsedIterations() == m_NumberOfIterations) return true;
    else return false;
  }

  /** A simple method to copy the data from the input to the output.
   * If the input does not exist, a zero field is written to the output. */
  virtual void CopyInputToOutput();

  /** Initialize the state of filter and equation before each iteration.
   * Progress feeback is implemented as part of this method. */
  virtual void InitializeIteration();

  /** Utility to smooth the deformation field (represented in the Output)
   * using a Guassian operator. The amount of smoothing can be specified
   * by setting the StandardDeviations. */
  virtual void SmoothDeformationField();

  /** Utitlity to copy one buffered region of one deformation field to
   * another deformation field. For efficiency no region checking is done. */
  virtual void CopyDeformationField( DeformationFieldType * input,
                                     DeformationFieldType * output );

  /** By default the output deformation field has the same Spacing, Origin
   * and LargestPossibleRegion as the input/initial deformation field.  If
   * the initial deformation field is not set, the output information is
   * copied from the fixed image. */
  virtual void GenerateOutputInformation();

  /** It is difficult to compute in advance the input moving image region
   * required to compute the requested output region. Thus the safest
   * thing to do is to request for the whole moving image.
   *
   * For the fixed image and deformation field, the input requested region
   * set to be the same as that of the output requested region. */
  virtual void GenerateInputRequestedRegion();

private:
  PDEDeformableRegistrationFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  /** Number of iterations to be performed. */
  unsigned int             m_NumberOfIterations;
  
  /** Standard deviation for Gaussian smoothing */
  double                   m_StandardDeviations[ImageDimension];

  /** Temporary deformation field use for smoothing the
   * the deformation field. */
  DeformationFieldPointer   m_TempField;

  /** Maximum error for Gaussian operator approximation. */
  double                    m_MaximumError;

};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPDEDeformableRegistrationFilter.txx"
#endif

#endif
