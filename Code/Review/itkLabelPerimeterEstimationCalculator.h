/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelPerimeterEstimationCalculator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLabelPerimeterEstimationCalculator_h
#define __itkLabelPerimeterEstimationCalculator_h

#include "itkObject.h"

namespace itk
{
/** \class LabelPerimeterEstimationCalculator
 * \brief Estimates the perimeter of a label object.
 *
 * The LabelPerimeterEstimationCalculator takes a label object and calculates
 * an estimated perimeter based on pixels' and their neighbors.
 *
 * This implementation was taken from the Insight Journal paper:
 * http://hdl.handle.net/1926/584  or
 * http://www.insight-journal.org/browse/publication/176
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \sa
 */
template< class TInputImage >
class ITK_EXPORT LabelPerimeterEstimationCalculator:
  public Object
{
public:
  /** Standard class typedefs. */
  typedef LabelPerimeterEstimationCalculator Self;
  typedef Object                             Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  /** Some convenient typedefs. */
  typedef TInputImage                           InputImageType;
  typedef typename InputImageType::Pointer      InputImagePointer;
  typedef typename InputImageType::ConstPointer InputImageConstPointer;
  typedef typename InputImageType::RegionType   InputImageRegionType;
  typedef typename InputImageType::PixelType    InputImagePixelType;

  typedef typename InputImageType::RegionType RegionType;
  typedef typename InputImageType::SizeType   SizeType;
  typedef typename InputImageType::IndexType  IndexType;

  typedef typename std::map< InputImagePixelType, double > PerimetersType;

  /** ImageDimension constants */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(LabelPerimeterEstimationCalculator, Object);

  /**
   * Set/Get whether the connected components are defined strictly by
   * face connectivity or by face+edge+vertex connectivity.  Default is
   * FullyConnectedOff.  For objects that are 1 pixel wide, use
   * FullyConnectedOn.
   */
  itkSetMacro(FullyConnected, bool);
  itkGetConstReferenceMacro(FullyConnected, bool);
  itkBooleanMacro(FullyConnected);

  void SetImage(const InputImageType *img)
  {
    m_Image = img;
  }

  const InputImageType * GetImage() const
  {
    return m_Image;
  }

  void Compute();

  const PerimetersType & GetPerimeters() const
  {
    return m_Perimeters;
  }

  const double & GetPerimeter(const InputImagePixelType & label) const
  {
    if ( m_Perimeters.find(label) != m_Perimeters.end() )
      {
      return m_Perimeters.find(label)->second;
      }
    itkExceptionMacro( << "Unknown label: "
                       << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( label ) );
  }

  bool HasLabel(const InputImagePixelType & label) const
  {
    if ( m_Perimeters.find(label) != m_Perimeters.end() )
      {
      return true;
      }
    return false;
  }

protected:
  LabelPerimeterEstimationCalculator();
  ~LabelPerimeterEstimationCalculator() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

private:
  LabelPerimeterEstimationCalculator(const Self &); //purposely not implemented
  void operator=(const Self &);                     //purposely not implemented

  bool m_FullyConnected;

  const InputImageType *m_Image;

  PerimetersType m_Perimeters;
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLabelPerimeterEstimationCalculator.txx"
#endif

#endif
