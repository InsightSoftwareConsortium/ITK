/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLevelSetNeighborhoodExtractor.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkLevelSetNeighborhoodExtractor_h
#define _itkLevelSetNeighborhoodExtractor_h

#include "itkLightProcessObject.h"
#include "itkLevelSet.h"
#include "itkIndex.h"

namespace itk
{

/** \class LevelSetNeighborhoodExtractor
 * \brief Locate pixels of a particular level set.
 *
 * LevelSetNeighborhoodExtractor locates a particular level set in the input
 * level set. Specifically, the method Locate() fills two containers: one
 * containing pixels immediately inside the contour defined by the level set
 * and the other containing pixels immediately outside.  For each located
 * pixel, an estimated distance to the particular level set is also
 * calculated.
 *
 * The containers InsidePoints and OutsidePoints can then be used 
 * in FastMarchingImageFilter to produce a signed distance map from 
 * the specified level set.
 *
 * This class is templated over the image type representing
 * the level set.
 *
 * This class supports narrowbanding. If a input narrowband is
 * provided, the alogrithm will only serach of pixels within the
 * narrowband.
 *
 * Implemenation of this class is based on Chapter 11 of
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Second edition, 1999.
 *
 * \ingroup LevelSetSegmentation 
 * 
 */
template <class TLevelSet>
class ITK_EXPORT LevelSetNeighborhoodExtractor :
    public LightProcessObject
{
public:
  /** Standard class typdedefs. */
  typedef LevelSetNeighborhoodExtractor Self;
  typedef LightProcessObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LevelSetNeighborhoodExtractor, Object);

  /** LevelSetType typedef support. */
  typedef LevelSetTypeDefault<TLevelSet>  LevelSetType;
  typedef typename LevelSetType::LevelSetImageType  LevelSetImageType;
  typedef typename LevelSetType::LevelSetPointer  LevelSetPointer;
  typedef typename LevelSetType::LevelSetConstPointer  LevelSetConstPointer;
  typedef typename LevelSetType::PixelType  PixelType;
  typedef typename LevelSetType::NodeType NodeType;
  typedef typename LevelSetType::NodeContainer NodeContainer;
  typedef typename LevelSetType::NodeContainerPointer NodeContainerPointer;

  /** SetDimension enumeration. */
  itkStaticConstMacro(SetDimension, unsigned int,
                      LevelSetType::SetDimension);

  /** Index typedef support. */
  typedef Index<itkGetStaticConstMacro(SetDimension)> IndexType;

  /** Set the input level set. */
  itkSetConstObjectMacro( InputLevelSet, LevelSetImageType );

  /** Get the input level set. */
  itkGetConstObjectMacro( InputLevelSet, LevelSetImageType );

  /** Set the value of the level set to be located. Default is 0. */
  itkSetMacro( LevelSetValue, double );

  /** Get the value of the level set to be located. */
  itkGetMacro( LevelSetValue, double );

  /** Set the narrow band width. Default is 12. */
  itkSetClampMacro( NarrowBandwidth, double, 0.0, 
                    NumericTraits<double>::max());

  /** Get the narrow band width. */
  itkGetMacro( NarrowBandwidth, double );

  /** Set the narrowbanding flag. */
  itkSetMacro( NarrowBanding, bool );

  /** Get the narrowbanding flag. */
  itkGetMacro( NarrowBanding, bool );
  itkBooleanMacro( NarrowBanding );

  /** Set/Get the input narrowband. A narrowband is represented as
   * a VectorContainer of LevelSetNodes. */
  void SetInputNarrowBand( NodeContainer * ptr );
  itkGetObjectMacro( InputNarrowBand, NodeContainer );

  /** Get the container of inside points. The inside points are
   * stored in a VectorContainer of LevelSetNodes. */
  NodeContainerPointer GetInsidePoints()
  { return m_InsidePoints; }

  /** Get the container of outside points. The outside points are
   * stored in a VectorContainer of LevelSetNodes. */
  NodeContainerPointer GetOutsidePoints(void)
  { return m_OutsidePoints; }

  /** Locate the level set. This method evokes the level set
   * location algorithm. */
  void Locate();

protected:
  LevelSetNeighborhoodExtractor();
  ~LevelSetNeighborhoodExtractor(){};
  void PrintSelf( std::ostream& os, Indent indent ) const;

  typename LevelSetImageType::PixelType GetLargeValue() const
  { return m_LargeValue; }

  const NodeType& GetNodeUsedInCalculation(unsigned int idx) const
  { return m_NodesUsed[idx]; }

  virtual void Initialize();
  virtual double CalculateDistance( IndexType& index );

  void GenerateData();

  bool GetLastPointIsInside() const
  { return m_LastPointIsInside; }

private:
  LevelSetNeighborhoodExtractor(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
  void      GenerateDataFull();
  void      GenerateDataNarrowBand();

  double                                        m_LevelSetValue;

  NodeContainerPointer                          m_InsidePoints;
  NodeContainerPointer                          m_OutsidePoints;

  LevelSetConstPointer                          m_InputLevelSet;

  bool                                          m_NarrowBanding;
  double                                        m_NarrowBandwidth;
  NodeContainerPointer                          m_InputNarrowBand;

  signed long                                   m_ImageSize[SetDimension];
  typename LevelSetImageType::PixelType         m_LargeValue;

  std::vector<NodeType>                         m_NodesUsed;

  bool                                          m_LastPointIsInside;

};


} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLevelSetNeighborhoodExtractor.txx"
#endif

#endif
