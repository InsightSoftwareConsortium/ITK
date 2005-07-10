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

namespace itk{
/** \class
 * \brief This class takes as input a reference image (acquired in the 
 * absence of diffusion sensitizing gradients) and 'n' diffusion
 * weighted images and their gradient directions. For details see
 * 
 * \par References:
 * C.F.Westin, S.E.Maier, H.Mamata, A.Nabavi, F.A.Jolesz, R.Kikinis,
 * "Processing and visualization for Diffusion tensor MRI", Medical image
 * Analysis, 2002, pp 93-108.
 *
 * \author: Based on code from Xiaodong Tao, GE CRD. 
 */

template< class TReferenceImagePixelType, 
          class TGradientImagePixelType, class TTensorPixelType=double >
class ITK_EXPORT DiffusionTensor3DReconstructionImageFilter :
  public ImageToImageFilter< Image< TReferenceImagePixelType, 3>, 
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

  typedef Image< GradientPixelType, 3 >            GradientImageType;

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

  /** Set method to set the reference image. */
  void SetReferenceImage( ReferenceImageType *referenceImage )
    {
    this->ProcessObject::SetNthInput( 0, referenceImage );
    }
    
  /** Get reference image */
  virtual ReferenceImageType * GetReferenceImage() 
  { return ( static_cast< ReferenceImageType *>(this->ProcessObject::GetInput(0)) ); }

  /** Return the gradient image. idx is 0 based */
  virtual GradientImageType * GetGradientImage( unsigned int idx ) 
  {
    if( idx >= m_NumberOfGradientDirections )
      {
      return NULL;
      }
    return ( static_cast< GradientImageType *>(this->ProcessObject::GetInput(idx+1)) ); 
  }

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
  void ThreadedGenerateData( const typename 
      ReferenceImageType::RegionType &outputRegionForThread, int);
  
  
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
  
  

  
  

};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDiffusionTensor3DReconstructionImageFilter.txx"
#endif

#endif

