/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShapePriorSegmentationLevelSetImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkShapePriorSegmentationLevelSetImageFilter_h_
#define __itkShapePriorSegmentationLevelSetImageFilter_h_

#include "itkSegmentationLevelSetImageFilter.h"
#include "itkShapePriorSegmentationLevelSetFunction.h"
#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkShapePriorMAPCostFunctionBase.h"

namespace itk {

/**
 *
 * \class ShapePriorSegmentationLevelSetImageFilter
 * 
 * \brief A base class which defines the API for implementing a level set
 * segmentation filter with statistical shape influence.
 *
 * \par OVERVIEW
 * This class extends the functionality of SegmentationLevelSetImageFilter
 * with an additional statistical shape influence term in the level set evolution as 
 * developed in [1].
 *
 * \par TEMPLATE PARAMETERS
 * There are two required and one optional template parameter for these
 * filters.
 *
 * TInputImage is the image type of the initial model you will input to the
 * filter using SetInput() or SetInitialImage().
 *
 * TFeatureImage is the image type of the image from which the filter will
 * calculate the speed term for segmentation (see INPUTS).
 *
 * TOutputPixelType is the data type used for the output image phi, the implicit
 * level set image.  This should really only ever be set as float (default) or
 * double.
 *
 *
 * \par PARAMETERS
 *
 * \par
 * From a level set evolution point of view, the shape is represented by a 
 * signed distance function from the shape encapsulated in a ShapeSignedDistanceFunction
 * object.
 *
 * \sa ShapeSignedDistanceFunction
 * \sa ShapePriorSegmentationLevelSetFunction
 *
 * \par REFERENCES
 * \par
 * [1] Leventon, M.E. et al. "Statistical Shape Influence in Geodesic Active Contours", CVPR 2000.
 *
 */
template <class TInputImage,
          class TFeatureImage,
          class TOutputPixelType = float >
class ITK_EXPORT ShapePriorSegmentationLevelSetImageFilter
  : public SegmentationLevelSetImageFilter< TInputImage, TFeatureImage, 
                                            TOutputPixelType, Image<TOutputPixelType, 
                                            ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
  /** Standard class typedefs */
  typedef ShapePriorSegmentationLevelSetImageFilter Self;
  typedef SegmentationLevelSetImageFilter<TInputImage, TFeatureImage, TOutputPixelType, 
        Image<TOutputPixelType, ::itk::GetImageDimension<TInputImage>::ImageDimension> > Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ShapePriorSegmentationLevelSetImageFilter, SegmentationLevelSetImageFilter);

  /** Inherited typedef from the superclass. */
  typedef typename Superclass::ValueType ValueType;
  typedef typename Superclass::OutputImageType OutputImageType;
  typedef typename Superclass::FeatureImageType FeatureImageType;
  
  /** Type of the output pixel. */
  typedef TOutputPixelType OutputPixelType;

  /** Inherited constant from the superclass. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      Superclass::ImageDimension);

  /** The level set function with shape prior type */
  typedef ShapePriorSegmentationLevelSetFunction<OutputImageType, FeatureImageType>
      ShapePriorSegmentationFunctionType;

  /** The shape signed distance function type. */
  typedef typename ShapePriorSegmentationFunctionType::ShapeFunctionType 
      ShapeFunctionType;
  typedef typename ShapeFunctionType::Pointer  ShapeFunctionPointer;

  /** The type of the MAP estimate cost function. */
  typedef  ShapePriorMAPCostFunctionBase<TFeatureImage,TOutputPixelType>  CostFunctionType;
  typedef  typename CostFunctionType::Pointer         CostFunctionPointer;
  typedef  typename CostFunctionType::ParametersType  ParametersType;

  /** Type of node used to represent the active region around the zero set. */
  typedef  typename CostFunctionType::NodeType              NodeType;
  typedef  typename CostFunctionType::NodeContainerType     NodeContainerType;
  typedef  typename NodeContainerType::Pointer              NodeContainerPointer;

  /** The type of optimizer used to compute the MAP estimate of the shape and pose parameters. */
  typedef   SingleValuedNonLinearOptimizer         OptimizerType;
  typedef   typename OptimizerType::Pointer        OptimizerPointer;

  /** Set/Get the shape signed distance function. */
  virtual void SetShapeFunction(ShapeFunctionType *s);
  itkGetObjectMacro( ShapeFunction, ShapeFunctionType );

  /** Set/Get the shape prior MAP cost function. */
  itkSetObjectMacro( CostFunction, CostFunctionType );
  itkGetObjectMacro( CostFunction, CostFunctionType );

  /** Set/Get the optimizer. */
  itkSetObjectMacro( Optimizer, OptimizerType );
  itkGetObjectMacro( Optimizer, OptimizerType );

 /** Set/Get the initial parameters. */
 itkSetMacro( InitialParameters, ParametersType );
 itkGetMacro( InitialParameters, ParametersType );

  /** Set/Get the scaling of the shape prior term. */
  void SetShapePriorScaling( ValueType v )
  {
    if ( v != m_ShapePriorSegmentationFunction->GetShapePriorWeight() )
    {
    m_ShapePriorSegmentationFunction->SetShapePriorWeight( v );
    this->Modified();
    }
  }
  ValueType GetShapePriorScaling() const
  {
    return m_ShapePriorSegmentationFunction->GetShapePriorWeight();
  }
  
  /** Set the shape prior segmentation function. In general, this should only be called
   * by a subclass of this object. It is made public to allow itk::Command objects access. */
  virtual void SetShapePriorSegmentationFunction( ShapePriorSegmentationFunctionType * s );
  virtual ShapePriorSegmentationFunctionType * GetShapePriorSegementationFunction()
    { return m_ShapePriorSegmentationFunction; }

  /** Get the current parameters. */
  itkGetMacro( CurrentParameters, ParametersType );

protected:
  virtual ~ShapePriorSegmentationLevelSetImageFilter() {}
  ShapePriorSegmentationLevelSetImageFilter();

  virtual void PrintSelf(std::ostream& os, Indent indent) const;

  /** Overrides parent implementation. MAP estimates of the shape and pose parameters
   is computed in this method. */
  virtual void InitializeIteration();
  
  /** Overridden from ProcessObject to set certain values before starting the
   * finite difference solver and then create an appropriate output */
  void GenerateData();

  /** Extract node of active region into a NodeContainer */
  void ExtractActiveRegion( NodeContainerType * ptr );
  
private:

   ShapeFunctionPointer     m_ShapeFunction;
   CostFunctionPointer      m_CostFunction;
   OptimizerPointer         m_Optimizer;
   ParametersType           m_InitialParameters;
   ParametersType           m_CurrentParameters;

   ShapePriorSegmentationFunctionType * m_ShapePriorSegmentationFunction;

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkShapePriorSegmentationLevelSetImageFilter.txx"
#endif

#endif

