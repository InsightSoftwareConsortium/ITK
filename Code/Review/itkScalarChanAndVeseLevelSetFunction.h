/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarChanAndVeseLevelSetFunction.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkScalarChanAndVeseLevelSetFunction_h
#define __itkScalarChanAndVeseLevelSetFunction_h

#include "itkScalarRegionBasedLevelSetFunctionBase.h"
#include "itkScalarChanAndVeseLevelSetFunctionSharedData.h"

namespace itk {

/** \class ScalarChanAndVeseLevelSetFunction
 *
 * \brief LevelSet function that computes a speed image based on regional integrals of probabilities
 *
 * This class implements a level set function that computes the speed image by
 * integrating values on the image domain.
 *
 * Based on the paper:
 *
 *        "An active contour model without edges"
 *         T. Chan and L. Vese. 
 *         In Scale-Space Theories in Computer Vision, pages 141–151, 1999.
 * 
 * \author Mosaliganti K., Smith B., Gelas A., Gouaillard A., Megason S.
 *
 *  This code was taken from the Insight Journal paper:
 *
 *      "Cell Tracking using Coupled Active Surfaces for Nuclei and Membranes"
 *      http://www.insight-journal.org/browse/publication/642
 *      http://hdl.handle.net/10380/3055
 *
 *  That is based on the papers:
 *
 *      "Level Set Segmentation: Active Contours without edge"
 *      http://www.insight-journal.org/browse/publication/322
 *      http://hdl.handle.net/1926/1532
 *
 *      and
 *
 *      "Level set segmentation using coupled active surfaces"
 *      http://www.insight-journal.org/browse/publication/323
 *      http://hdl.handle.net/1926/1533
 *
 *
 */
template < class TInputImage,
class TFeatureImage,
class TSharedData = ScalarChanAndVeseLevelSetFunctionSharedData< TInputImage, TFeatureImage > >
class ITK_EXPORT ScalarChanAndVeseLevelSetFunction
: public ScalarRegionBasedLevelSetFunctionBase< TInputImage, TFeatureImage, TSharedData >
{
public:
  typedef ScalarChanAndVeseLevelSetFunction           Self;
  typedef ScalarRegionBasedLevelSetFunctionBase< 
    TInputImage, TFeatureImage, TSharedData >         Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods) */
  itkTypeMacro( ScalarChanAndVeseLevelSetFunction, ScalarLevelSetFunctionBase );

  itkStaticConstMacro( ImageDimension, unsigned int, TFeatureImage::ImageDimension );

  typedef TInputImage                                   InputImageType;
  typedef typename Superclass::InputImageConstPointer   InputImageConstPointer;
  typedef typename Superclass::InputImagePointer        InputImagePointer;
  typedef typename Superclass::InputPixelType           InputPixelType;
  typedef typename Superclass::InputIndexType           InputIndexType;
  typedef typename Superclass::InputIndexValueType      InputIndexValueType;
  typedef typename Superclass::InputSizeType            InputSizeType;
  typedef typename Superclass::InputSizeValueType       InputSizeValueType;
  typedef typename Superclass::InputRegionType          InputRegionType;
  typedef typename Superclass::InputPointType           InputPointType;

  typedef TFeatureImage                                 FeatureImageType;
  typedef typename FeatureImageType::ConstPointer       FeatureImageConstPointer;
  typedef typename Superclass::FeaturePixelType         FeaturePixelType;
  typedef typename Superclass::FeatureIndexType         FeatureIndexType;
  typedef typename Superclass::FeatureOffsetType        FeatureOffsetType;

  typedef typename Superclass::ScalarValueType          ScalarValueType;
  typedef typename Superclass::NeighborhoodType         NeighborhoodType;
  typedef typename Superclass::FloatOffsetType          FloatOffsetType;
  typedef typename Superclass::RadiusType               RadiusType;
  typedef typename Superclass::TimeStepType             TimeStepType;
  typedef typename Superclass::GlobalDataStruct         GlobalDataStruct;
  typedef typename Superclass::PixelType                PixelType;
  typedef typename Superclass::VectorType               VectorType;

  typedef typename Superclass::SharedDataType           SharedDataType;
  typedef typename Superclass::SharedDataPointer        SharedDataPointer;

  typedef typename Superclass::ImageIteratorType        ImageIteratorType;
  typedef typename Superclass::ConstImageIteratorType   ConstImageIteratorType;
  typedef typename Superclass::FeatureImageIteratorType FeatureImageIteratorType;
  typedef typename Superclass::ConstFeatureIteratorType ConstFeatureIteratorType;

  typedef typename Superclass::ListPixelType            ListPixelType;
  typedef typename Superclass::ListPixelConstIterator   ListPixelConstIterator;
  typedef typename Superclass::ListPixelIterator        ListPixelIterator;
  typedef typename Superclass::ListImageType            ListImageType;

  void UpdatePixel( const unsigned int& idx,
    NeighborhoodIterator<TInputImage> & iterator,
    ScalarValueType & newValue,
    bool & status );

protected:
  ScalarChanAndVeseLevelSetFunction() {}
  ~ScalarChanAndVeseLevelSetFunction(){}

  void ComputeParameters();

  ScalarValueType computeInternalTerm( const FeaturePixelType& iValue,
    const FeatureIndexType& iIdx, const unsigned int& fId );

  ScalarValueType computeExternalTerm( const FeaturePixelType& iValue,
    const FeatureIndexType& iIdx, const unsigned int& pr );

private:
  ScalarChanAndVeseLevelSetFunction(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
};

}

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkScalarChanAndVeseLevelSetFunction.txx"
#endif

#endif
