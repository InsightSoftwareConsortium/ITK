/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDerivativeImageFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGaussianDerivativeImageFunction_h
#define _itkGaussianDerivativeImageFunction_h

#include "itkNeighborhoodOperatorImageFunction.h"
#include "itkImageFunction.h"
#include "itkGaussianDerivativeSpatialFunction.h"
#include "itkGaussianSpatialFunction.h"

namespace itk
{

/**
 * \class GaussianDerivativeImageFunction
 * \brief Compute the gaussian derivatives of an the image
 *        at a specific location in space, i.e. point, index or continuous
 *        index.
 * This class is templated over the input image type.
 * \sa NeighborhoodOperator
 * \sa ImageFunction
 */
template <class TInputImage,class TOutput=double>
class ITK_EXPORT GaussianDerivativeImageFunction :
public ImageFunction< TInputImage, Vector<TOutput,::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:

  /**Standard "Self" typedef */
  typedef GaussianDerivativeImageFunction Self;

  /** Standard "Superclass" typedef*/
  typedef ImageFunction<TInputImage,Vector<TOutput,::itk::GetImageDimension<TInputImage>::ImageDimension> > Superclass;

  /** Smart pointer typedef support. */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory.*/
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro( GaussianDerivativeImageFunction, ImageFunction );

  /** InputImageType typedef support.*/
  typedef TInputImage                                 InputImageType;
  typedef typename InputImageType::PixelType          InputPixelType;
  typedef typename Superclass::IndexType              IndexType;
  typedef typename Superclass::ContinuousIndexType    ContinuousIndexType;


  /** Dimension of the underlying image. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      InputImageType::ImageDimension);
  
  typedef Neighborhood<InputPixelType, itkGetStaticConstMacro(ImageDimension)> NeighborhoodType;

  typedef Vector<TOutput,itkGetStaticConstMacro(ImageDimension)>  VectorType;
  typedef Vector<TOutput,itkGetStaticConstMacro(ImageDimension)>  OutputType;
  typedef FixedArray<NeighborhoodType,2*itkGetStaticConstMacro(ImageDimension)> OperatorArrayType;
  typedef NeighborhoodOperatorImageFunction<InputImageType,
                                             TOutput> OperatorImageFunctionType;
  typedef typename OperatorImageFunctionType::Pointer OperatorImageFunctionPointer;

  typedef GaussianDerivativeSpatialFunction<TOutput,1>  GaussianDerivativeFunctionType;
  typedef typename GaussianDerivativeFunctionType::Pointer GaussianDerivativeFunctionPointer;

  typedef GaussianSpatialFunction<TOutput,1>  GaussianFunctionType;
  typedef typename GaussianFunctionType::Pointer GaussianFunctionPointer;

  /** Point typedef support. */
  typedef typename Superclass::PointType PointType;

  
  /** Evalutate the  in the given dimension at specified point */
  virtual OutputType Evaluate(const PointType& point) const;


  /** Evaluate the function at specified Index position*/
  virtual OutputType EvaluateAtIndex( const IndexType & index ) const;

  /** Evaluate the function at specified ContinousIndex position.*/
  virtual OutputType EvaluateAtContinuousIndex( 
    const ContinuousIndexType & index ) const;

  /** The variance for the discrete Gaussian kernel.  Sets the variance
   * independently for each dimension, but 
   * see also SetVariance(const double v). The default is 0.0 in each
   * dimension. If UseImageSpacing is true, the units are the physical units
   * of your image.  If UseImageSpacing is false then the units are pixels.*/
  void SetSigma( const double sigma[itkGetStaticConstMacro(ImageDimension)] );
  void SetSigma( const double sigma);
  const double* GetSigma() const {return m_Sigma;}
 
  /** Set the extent of the kernel */
  void SetExtent( const double extent[itkGetStaticConstMacro(ImageDimension)] );
  void SetExtent( const double extent);
  const double* GetExtent() const {return m_Extent;}

  /** Set the input image.
   * \warning this method caches BufferedRegion information.
   * If the BufferedRegion has changed, user must call
   * SetInputImage again to update cached values. */
  virtual void SetInputImage( const InputImageType * ptr );



protected:
  GaussianDerivativeImageFunction();
  GaussianDerivativeImageFunction( const Self& ){};

  ~GaussianDerivativeImageFunction(){};

  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent) const;

  void RecomputeGaussianKernel();
  void RecomputeContinuousGaussianKernel(
           const double offset[itkGetStaticConstMacro(ImageDimension)]) const;


private:

  double                        m_Sigma[itkGetStaticConstMacro(ImageDimension)];

  /** Array of 1D operators. Contains a derivative kernel and a gaussian kernel for
   *  each dimension */
  mutable OperatorArrayType     m_OperatorArray;
  mutable OperatorArrayType     m_ContinuousOperatorArray;

  /** OperatorImageFunction */
  OperatorImageFunctionPointer  m_OperatorImageFunction;
  double m_Extent[itkGetStaticConstMacro(ImageDimension)];

  /** Flag to indicate whether to use image spacing */
  bool m_UseImageSpacing;

  /** Neighborhood Image Function */
  GaussianDerivativeFunctionPointer m_GaussianDerivativeFunction;
  GaussianFunctionPointer           m_GaussianFunction;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGaussianDerivativeImageFunction.txx"
#endif

#endif

