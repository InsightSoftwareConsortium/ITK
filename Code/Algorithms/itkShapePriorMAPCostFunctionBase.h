/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapePriorMAPCostFunctionBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePriorMAPCostFunctionBase_h
#define __itkShapePriorMAPCostFunctionBase_h

#include "itkSingleValuedCostFunction.h"
#include "itkLevelSet.h"
#include "itkShapeSignedDistanceFunction.h"

namespace itk
{
  
/** \class ShapePriorMAPCostFunctionBase
 * \brief Represents the base class of maximum aprior (MAP) cost function used
 * ShapePriorSegmentationLevelSetImageFilter to estimate the shape paramaeters.
 *
 * This class follows the shape and pose parameters estimation developed in [1].
 *
 * This class has two template parameters, the feature image type representing the 
 * edge potential map and the pixel type used to
 * represent the output level set in the ShapePriorSegmentationLevelSetImageFilter.
 *
 * \sa ShapePriorSegmentationLevelSetImageFilter
 *
 * \par REFERENCES
 * \par
 * [1] Leventon, M.E. et al. "Statistical Shape Influence in Geodesic Active Contours", CVPR 2000.
 *
 *
 * \ingroup Numerics Optimizers
 */
template <class TFeatureImage, class TOutputPixel>
class ITK_EXPORT ShapePriorMAPCostFunctionBase : 
          public SingleValuedCostFunction 
{
public:
  /** Standard class typedefs. */
  typedef ShapePriorMAPCostFunctionBase    Self;
  typedef SingleValuedCostFunction     Superclass;
  typedef SmartPointer<Self>           Pointer;
  typedef SmartPointer<const Self>     ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
   
  /** Run-time type information (and related methods). */
  itkTypeMacro( ShapePriorMAPCostFunctionBase, SingleValuedCostFunction );

  /**  MeasureType typedef.
   *  It defines a type used to return the cost function value. */
  typedef typename Superclass::MeasureType       MeasureType;

  /** DerivativeType typedef.
   *  It defines a type used to return the cost function derivative.  */
  typedef typename Superclass::DerivativeType    DerivativeType;
 
  /**  ParametersType typedef.
   *  It defines a position in the optimization search space. */
  typedef typename Superclass::ParametersType    ParametersType;

  /** Type of the feature image representing the edge potential map. */
  typedef TFeatureImage                              FeatureImageType;
  typedef typename FeatureImageType::ConstPointer    FeatureImagePointer;

  /** Dimension constant. */
  itkStaticConstMacro( ImageDimension, unsigned int, TFeatureImage::ImageDimension);

  /** Type of pixel used to represent the level set. */
  typedef TOutputPixel PixelType;

  /** Type of node used to represent the active region around the zero set. */
  typedef LevelSetNode<PixelType, itkGetStaticConstMacro(ImageDimension)>   NodeType;

  /** Type of container used to store the level set nodes. */
  typedef VectorContainer<unsigned int, NodeType>   NodeContainerType;
  typedef typename NodeContainerType::ConstPointer  NodeContainerPointer;

  /** Type of the shape signed distance function. */
  typedef ShapeSignedDistanceFunction<float,ImageDimension>  ShapeFunctionType;
  typedef typename ShapeFunctionType::Pointer            ShapeFunctionPointer;

  /** Set/Get the shape distance function. */
  itkSetObjectMacro( ShapeFunction, ShapeFunctionType );
  itkGetObjectMacro( ShapeFunction, ShapeFunctionType );

  /** Set/Get the active region. */
  itkSetConstObjectMacro( ActiveRegion, NodeContainerType );
  itkGetConstObjectMacro( ActiveRegion, NodeContainerType );

  /** Set/Get the feature image. */
  itkSetConstObjectMacro( FeatureImage, FeatureImageType );
  itkGetConstObjectMacro( FeatureImage, FeatureImageType );

  /** This method returns the value of the cost function corresponding
    * to the specified parameters.    */ 
  virtual MeasureType GetValue( const ParametersType & parameters ) const;


  /** This method returns the derivative of the cost function corresponding
    * to the specified parameters.   */ 
  virtual void GetDerivative( const ParametersType & parameters,
                                    DerivativeType & derivative ) const
    { itkExceptionMacro( << "This function is currently not supported." ); }

  /** Return the number of parameters. */
  virtual unsigned int GetNumberOfParameters(void) const
    { return m_ShapeFunction->GetNumberOfParameters(); }


  /** Compute the inside term component of the MAP cost function. 
   * Subclasses should override this function */
  virtual MeasureType ComputeLogInsideTerm( const ParametersType & parameters ) const
    { return 0.0; }

  /** Compute the gradient term component of the MAP cost function.
   * Subclasses should override this function */
  virtual MeasureType ComputeLogGradientTerm( const ParametersType & parameters ) const
    { return 0.0; }

  /** Compute the shape prior component of the MAP cost function.
   * Subclasses should override this function */
  virtual MeasureType ComputeLogShapePriorTerm( const ParametersType & parameters ) const
    { return 0.0; }

  /** Compute the pose prior component of the MAP cost function.
   * Subclasses should override this function */
  virtual MeasureType ComputeLogPosePriorTerm( const ParametersType & parameters ) const
    { return 0.0; }

  /** Initialize the cost function by making sure that all the components
   *  are present. */
  virtual void Initialize(void) throw ( ExceptionObject );

  
protected:
  ShapePriorMAPCostFunctionBase();
  virtual ~ShapePriorMAPCostFunctionBase() {};

  void PrintSelf(std::ostream& os, Indent indent) const;

  ShapeFunctionPointer    m_ShapeFunction;
  NodeContainerPointer    m_ActiveRegion;
  FeatureImagePointer     m_FeatureImage;

private:
  ShapePriorMAPCostFunctionBase(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};


} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapePriorMAPCostFunctionBase.txx"
#endif

#endif



