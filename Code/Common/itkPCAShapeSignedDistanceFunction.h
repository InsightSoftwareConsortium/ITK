/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPCAShapeSignedDistanceFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPCAShapeSignedDistanceFunction_h
#define _itkPCAShapeSignedDistanceFunction_h


#include "itkShapeSignedDistanceFunction.h"
#include "itkImage.h"
#include "itkInterpolateImageFunction.h"
#include "itkExtrapolateImageFunction.h"

namespace itk
{


/** \class PCAShapeSignedDistanceFunction
 * \brief Compute the signed distance from a N-dimensional PCAShape
 *
 * This class is templated over the coordinate representation type 
 * (e.g. float or double) and the space dimension.
 *
 * \sa ShapeSignedDistanceFunction
 * \ingroup ImageFunctions
 * 
 * */
template <typename TCoordRep, unsigned int VSpaceDimension>
class ITK_EXPORT PCAShapeSignedDistanceFunction : 
  public ShapeSignedDistanceFunction<TCoordRep, VSpaceDimension>
{

public:
  /** Standard class typedefs. */
  typedef PCAShapeSignedDistanceFunction          Self;
  typedef ShapeSignedDistanceFunction<
    TCoordRep, VSpaceDimension>                   Superclass;
  typedef SmartPointer<Self>                      Pointer;
  typedef SmartPointer<const Self>                ConstPointer;
  

  /** Run-time type information (and related methods). */
  itkTypeMacro(PCAShapeSignedDistanceFunction, ShapeSignedDistancFunction);

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** Dimension underlying input image. */
  itkStaticConstMacro(SpaceDimension,unsigned int,Superclass::SpaceDimension);


  /** CoordRep typedef support. */
  typedef typename Superclass::CoordRepType       CoordRepType;

  /** InputeType typedef support. */
  typedef typename Superclass::InputType          InputType;

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType         OutputType;
  
  /** Point typedef support. */
  typedef typename Superclass::PointType          PointType;

  /** Parameters typedef support. */
  typedef typename Superclass::ParametersType     ParametersType;


  /** Image typedef support. */
  typedef Image<double, itkGetStaticConstMacro(SpaceDimension)> ImageType;
  typedef typename ImageType::Pointer                           ImagePointer;
  typedef std::vector<ImagePointer>                             ImagePointerVector;

  /** Transform typedef support. */
  typedef Transform<CoordRepType, 
                    itkGetStaticConstMacro(SpaceDimension), 
                    itkGetStaticConstMacro(SpaceDimension)> TransformType;

  /** Interpolator typedef support. */
  typedef InterpolateImageFunction<ImageType, CoordRepType> InterpolatorType;
  typedef typename InterpolatorType::Pointer                InterpolatorPointer;
  typedef std::vector<InterpolatorPointer>                  InterpolatorPointerVector;

  /** extrapolator typedef support. */
  typedef ExtrapolateImageFunction<ImageType, CoordRepType> ExtrapolatorType;
  typedef typename ExtrapolatorType::Pointer                ExtrapolatorPointer;
  typedef std::vector<ExtrapolatorPointer>                  ExtrapolatorPointerVector;

  /** function typedef support. */
  typedef ImageFunction<ImageType, double, CoordRepType> FunctionType;
  typedef typename FunctionType::Pointer                 FunctionPointer;
  typedef std::vector<FunctionPointer>                   FunctionPointerVector;


  /** Set/Get the number of principal components 
   * SetNumberOfPrincipalComponents must be called before SetParameters */
  void SetNumberOfPrincipalComponents(unsigned int n);
  itkGetMacro(NumberOfPrincipalComponents, unsigned int);

  /** Set/Get the mean image. */
  itkSetObjectMacro(MeanImage, ImageType);
  itkGetObjectMacro(MeanImage, ImageType);

  /** Set/Get the principal component images. */
  void SetPrincipalComponentImages(ImagePointerVector v)
    { m_PrincipalComponentImages = v; }
//  ImagePointerVector & GetPrincipalComponentImages()
//    { return m_PrincipalComponentImages; }

  /** Set/Get the principal component standard deviations. */
  itkSetMacro(PrincipalComponentStandardDeviations, ParametersType);
  itkGetMacro(PrincipalComponentStandardDeviations, ParametersType);

  /** Set/Get transform. */
  itkSetObjectMacro(Transform, TransformType);
  itkGetObjectMacro(Transform, TransformType);


  /** A PCAShape is defined by a set of shape and pose parameters. */
  virtual void SetParameters( const ParametersType & );
  virtual const ParametersType& GetParameters(void) const
    { return m_Parameters; }
  virtual unsigned int GetNumberOfShapeParameters(void) const
    { return m_NumberOfPrincipalComponents; }
  virtual unsigned int GetNumberOfPoseParameters(void) const
    { return m_Transform ? m_Transform->GetNumberOfParameters() : 0; }

  /** Evaluate the signed distance from a shape at a given position. */
  virtual OutputType Evaluate(const PointType& point) const;

  /** Initialize must be called before the first call of  
   Evaluate() to allow the class to validate any inputs. */
  virtual void Initialize() throw ( ExceptionObject );


protected:
  PCAShapeSignedDistanceFunction();
  ~PCAShapeSignedDistanceFunction(){};

  void PrintSelf(std::ostream& os, Indent indent) const;


private:
  PCAShapeSignedDistanceFunction(const Self&); //purposely not implemented
  void operator=( const Self& ); //purposely not implemented


  /** intrinsic data members */
  unsigned int                  m_NumberOfPrincipalComponents;
  unsigned int                  m_NumberOfTransformParameters;

  ImagePointer                  m_MeanImage;
  ImagePointerVector            m_PrincipalComponentImages;
  ParametersType                m_PrincipalComponentStandardDeviations;

  /** transform and interpolator/extrapolator for image interpolation */
  typename TransformType::Pointer        m_Transform;
  InterpolatorPointerVector              m_Interpolators;
  ExtrapolatorPointerVector              m_Extrapolators;
  mutable FunctionPointerVector          m_Selectors;

  /** shape and pose parameters */
  ParametersType                m_WeightOfPrincipalComponents;
  ParametersType                m_TransformParameters;         

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPCAShapeSignedDistanceFunction.txx"
#endif

#endif
