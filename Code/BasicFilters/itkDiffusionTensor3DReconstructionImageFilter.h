/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiffusionTensor3DReconstructionImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDiffusionTensor3DReconstructionImageFilter_h_
#define __itkDiffusionTensor3DReconstructionImageFilter_h_

#include "itkImageToImageFilter.h"
#include "itkDiffusionTensor3D.h"
#include "vnl/vnl_matrix.h"
#include "vnl/vnl_vector_fixed.h"
#include "vnl/vnl_matrix_fixed.h"
#include "vnl/algo/vnl_svd.h"
#include "itkVectorContainer.h"
#include "itkVectorImage.h"

namespace itk{
/** \class DiffusionTensor3DReconstructionImageFilter
 * \brief This class takes as input a reference image (acquired in the 
 * absence of diffusion sensitizing gradients) and 'n' diffusion
 * weighted images and their gradient directions and computes an image of 
 * tensors. (with DiffusionTensor3D as the pixel type). Once that is done, you 
 * can apply filters on this tensor image to compute FA, ADC, RGB weighted 
 * maps etc. 
 *
 * \par Inputs
 * The Reference image (typically the B0 image) is supplied via SetReferenceImage.
 * 
 * The Gradient images can be input using one of two methods. The first method is
 * provided as a convenience to the user assuming that he/she has several gradient
 * images read in seperately. The second method is convenient, when the gradient
 * image is available as a single (multi-component) image.
 * 1. AddGradientImage: (Each of the \c n gradient images will be an Image)
 * 2. SetGradientImage: (The gradient image will be a VectorImage. It will be 
 *    assumed that the vectorImage has the same dimension as the Reference image 
 *    and a vector length parameter of \c n)
 * 
 * \par Outputs
 * The output image is an image of Tensors:
 \verbatim
 Image< DiffusionTensor3D< TTensorPixelType >, 3 >
 \endverbatim
 *
 * \par Parameters
 * Threshold -  Threshold on the reference image data. The output tensor will 
 * be a null tensor for pixels in the reference image that have a value less 
 * than this.
 *  
 * \par Template parameters
 * The class is templated over the pixel type of the reference and gradient 
 * images (expected to be scalar data types) and the internal representation
 * of the DiffusionTensor3D pixel (double, float etc).
 *  
 * \par References:
 * 1. C.F.Westin, S.E.Maier, H.Mamata, A.Nabavi, F.A.Jolesz, R.Kikinis,
 * "Processing and visualization for Diffusion tensor MRI", Medical image
 * Analysis, 2002, pp 93-108.
 * 
 * 2. splweb.bwh.harvard.edu:8000/pages/papers/westin/ISMRM2002.pdf
 * "A Dual Tensor Basis Solution to the Stejskal-Tanner Equations for DT-MRI"
 * 
 * \par WARNING:
 * Although this filter has been written to support multiple threads, please 
 * set the number of threads to 1.
 \verbatim
 filter->SetNumberOfThreads(1);
 \endverbatim
 * This is due to buggy code in netlib/dsvdc, that is called by vnl_svd. 
 * (used to compute the psudo-inverse to find the dual tensor basis).
 *
 * \author Based on code from Xiaodong Tao, GE CRD. 
 * 
 * \note
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \sa DiffusionTensor3D SymmetricSecondRankTensor 
 * 
 * \ingroup Multithreaded  TensorObjects
 */

template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType=double >
class ITK_EXPORT DiffusionTensor3DReconstructionImageFilter :
  public ImageToImageFilter< Image< TReferenceImagePixelType, 3 >, 
                             Image< DiffusionTensor3D< TTensorPixelType >, 3 > >
{

public:

  typedef DiffusionTensor3DReconstructionImageFilter Self;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;
  typedef ImageToImageFilter< Image< TReferenceImagePixelType, 3>, 
          Image< DiffusionTensor3D< TTensorPixelType >, 3 > >
                          Superclass;
  
   /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Runtime information support. */
  itkTypeMacro(DiffusionTensor3DReconstructionImageFilter, 
                                                      ImageToImageFilter);
 
  typedef TReferenceImagePixelType                 ReferencePixelType;

  typedef TGradientImagePixelType                  GradientPixelType;

  typedef DiffusionTensor3D< TTensorPixelType >    TensorPixelType;

  /** Reference image data,  This image is aquired in teh absence 
   * of a diffusion sensitizing field gradient */
  typedef typename Superclass::InputImageType      ReferenceImageType;
  
  typedef Image< TensorPixelType, 3 >              TensorImageType;
  
  typedef TensorImageType                          OutputImageType;

  typedef typename Superclass::OutputImageRegionType
                                                   OutputImageRegionType;

  /** Typedef defining one (of the many) gradient images.  */
  typedef Image< GradientPixelType, 3 >            GradientImageType;

  /** An alternative typedef defining one (of the many) gradient images. 
   * It will be assumed that the vectorImage has the same dimension as the 
   * Reference image and a vector length parameter of \c n (number of
   * gradient directions)*/
  typedef VectorImage< GradientPixelType, 3 >      GradientImagesType;

  /** Holds the tensor basis coefficients G_k */
  typedef vnl_matrix_fixed< double, 6, 6 >         TensorBasisMatrixType;
  
  typedef vnl_matrix< double >                     CoefficientMatrixType;

  /** Holds each magnetic field gradient used to acquire one DWImage */
  typedef vnl_vector_fixed< double, 3 >            GradientDirectionType;

  /** Container to hold gradient directions of the 'n' DW measurements */
  typedef VectorContainer< unsigned int, 
          GradientDirectionType >                  GradientDirectionContainerType;
  

  /** Set method to add a gradient direction and its corresponding image. */
  void AddGradientImage( const GradientDirectionType &, const GradientImageType *image);

  /** Another set method to add a gradient directions and its corresponding
   * image. The image here is a VectorImage. The user is expected to pass the 
   * gradient directions in a container. The ith element of the container 
   * corresponds to the gradient direction of the ith component image the 
   * VectorImage.  */
  void SetGradientImage( GradientDirectionContainerType *, 
                                             const GradientImagesType *image);
  
  /** Set method to set the reference image. */
  void SetReferenceImage( ReferenceImageType *referenceImage )
    {
    this->ProcessObject::SetNthInput( 0, referenceImage );
    }
    
  /** Get reference image */
  virtual ReferenceImageType * GetReferenceImage() 
  { return ( static_cast< ReferenceImageType *>(this->ProcessObject::GetInput(0)) ); }

  /** Return the gradient direction. idx is 0 based */
  virtual GradientDirectionType GetGradientDirection( unsigned int idx) const
    {
    if( idx >= m_NumberOfGradientDirections )
      {
      itkExceptionMacro( << "Gradient direction " << idx << "does not exist" );
      }
    return m_GradientDirectionContainer->ElementAt( idx+1 );
    }

  /** Threshold on the reference image data. The output tensor will be a null
   * tensor for pixels in the reference image that have a value less than this
   * threshold. */
  itkSetMacro( Threshold, ReferencePixelType );
  itkGetMacro( Threshold, ReferencePixelType );

  
protected:
  DiffusionTensor3DReconstructionImageFilter();
  ~DiffusionTensor3DReconstructionImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void ComputeTensorBasis();
  
  void BeforeThreadedGenerateData();
  void ThreadedGenerateData( const 
      OutputImageRegionType &outputRegionForThread, int);
  
  /** enum to indicate if the gradient image is specified as a single multi-
   * component image or as several separate images */
  typedef enum
    {
    GradientIsInASingleImage = 1,
    GradientIsInManyImages,
    Else
    } GradientImageTypeEnumeration;
    
private:
  
  /* Tensor basis coeffs */
  TensorBasisMatrixType                             m_TensorBasis;
  
  CoefficientMatrixType                             m_Coeffs;

  /** container to hold gradient directions */
  GradientDirectionContainerType::Pointer           m_GradientDirectionContainer;

  /** Number of gradient measurements */
  unsigned int                                      m_NumberOfGradientDirections;

  /** Threshold on the reference image data */
  ReferencePixelType                                m_Threshold;

  /** Gradient image was specified in a single image or in multiple images */
  GradientImageTypeEnumeration                      m_GradientImageTypeEnumeration;
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDiffusionTensor3DReconstructionImageFilter.txx"
#endif

#endif

