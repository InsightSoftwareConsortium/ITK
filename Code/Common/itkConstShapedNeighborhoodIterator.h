/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstShapedNeighborhoodIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConstShapedNeighborhoodIterator_h
#define __itkConstShapedNeighborhoodIterator_h

#include <vector>
#include <list>
#include "itkNeighborhoodIterator.h"

namespace itk {

/** \class ConstShapedNeighborhoodIterator
 *
 * \brief Const version of ShapedNeighborhoodIterator, defining iteration of a local
 * N-dimensional neighborhood of pixels across an itk::Image.
 *
 * ConstShapedNeighborhoodIterator implements the read-only methods of
 * ShapedNeighborhoodIterator.  A "shaped" neighborhood iterator is one that
 * allows you to specify which neighbor elements are active and which are
 * inactive.  See ShapedNeighborhoodIterator for more information.
 *
 * \ingroup ImageIterators
 *
 * \sa Neighborhood \sa ImageIterator \sa NeighborhoodIterator \sa ShapedNeighborhoodIterator
 **/
template<class TImage,  class TBoundaryCondition
                       = ZeroFluxNeumannBoundaryCondition<TImage> >
class ITK_EXPORT ConstShapedNeighborhoodIterator
  :  private NeighborhoodIterator<TImage, TBoundaryCondition>
{
public:
  /** Extract image type information. */
  typedef typename TImage::InternalPixelType InternalPixelType;
  typedef typename TImage::PixelType PixelType;
    
  /** Save the image dimension. */
  itkStaticConstMacro(Dimension, unsigned int, TImage::ImageDimension);
  
  /** Standard class typedefs. */
  typedef ConstShapedNeighborhoodIterator Self;
  typedef NeighborhoodIterator<TImage, TBoundaryCondition> Superclass;

  /** Inherit typedefs from superclass */
  typedef typename Superclass::OffsetType OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;
  typedef typename Superclass::RadiusType RadiusType;  
  typedef typename Superclass::SizeType SizeType;
  typedef typename Superclass::SizeValueType SizeValueType;
  
  /** Typedef support for common objects */
  typedef TImage ImageType;
  typedef typename TImage::RegionType RegionType;
  typedef Index<itkGetStaticConstMacro(Dimension)> IndexType;
  typedef typename IndexType::IndexValueType IndexValueType;
  typedef Neighborhood<PixelType, itkGetStaticConstMacro(Dimension)> NeighborhoodType;

  /** An stl storage container type that can be sorted.  The type used for
      the list of active offsets in the neighborhood.*/
  typedef std::list<unsigned int> IndexListType;

  /** Typedef for boundary condition type. */
  typedef TBoundaryCondition BoundaryConditionType;
  
  /** Typedef for generic boundary condition pointer */
  typedef ImageBoundaryCondition<ImageType> *ImageBoundaryConditionPointerType;

  /** A const iterator for the ShapedNeighborhood classes.*/
  struct ConstIterator
  {
    ConstIterator() { m_NeighborhoodIterator = 0; }
    ConstIterator(Self *s)
    {
      m_NeighborhoodIterator = s;
      this->GoToBegin();
    }
    ~ConstIterator() {}
    const ConstIterator &operator=(const ConstIterator &o)
    {
      m_NeighborhoodIterator = o.m_NeighborhoodIterator;
      m_ListIterator = o.m_ListIterator;
      return *this;
    }
    
    ConstIterator(const ConstIterator &o)
    {
      m_NeighborhoodIterator = o.m_NeighborhoodIterator;
      m_ListIterator = o.m_ListIterator;
    }

    void operator++(int)
    { m_ListIterator++; }

    void operator--(int)
    { m_ListIterator--; }
    
    const ConstIterator &operator++()
    {
      m_ListIterator++;
      return *this;
    }
    const ConstIterator &operator--()
    {
      m_ListIterator--;
      return *this;
    }
    
    bool operator!=(const ConstIterator &o) const
    { return m_ListIterator.operator!=(o.m_ListIterator); }
    bool operator==(const ConstIterator &o) const
    { return m_ListIterator.operator==(o.m_ListIterator); }

    bool IsAtEnd() const
    {
      if (m_ListIterator == m_NeighborhoodIterator->GetActiveIndexList().end())
        { return true; }
      else { return false; }
    }

    void GoToBegin()
    {
      m_ListIterator = m_NeighborhoodIterator->GetActiveIndexList().begin();
    }

    void GoToEnd()
    {
      m_ListIterator = m_NeighborhoodIterator->GetActiveIndexList().end();
    }

    PixelType Get() const
    { return m_NeighborhoodIterator->GetPixel(*m_ListIterator); }

    OffsetType GetNeighborhoodOffset() const
    { return m_NeighborhoodIterator->GetOffset(*m_ListIterator); }

    typename IndexListType::value_type GetNeighborhoodIndex() const
    { return *m_ListIterator; }

  protected:
    Self *m_NeighborhoodIterator;
    typename IndexListType::const_iterator m_ListIterator;

    void ProtectedSet(const PixelType &v) const
    { m_NeighborhoodIterator->SetPixel(*m_ListIterator, v); } 
  };

  /** Returns a const iterator for the neighborhood which points to the first
   * pixel in the neighborhood. */
  const ConstIterator &Begin() const
  {    return m_ConstBeginIterator;  }

  /** Returns a const iterator for the neighborhood which points to the last
   * pixel in the neighborhood. */
  const ConstIterator &End() const
  {    return m_ConstEndIterator;  }
  
  /** Default constructor */
  ConstShapedNeighborhoodIterator()
  {
    m_ConstBeginIterator = ConstIterator(this);
    m_ConstEndIterator = ConstIterator(this);
    m_ConstEndIterator.GoToEnd();
    m_CenterIsActive = false;
  }

  /** Virtual destructor */
  virtual ~ConstShapedNeighborhoodIterator()  { }

   /** Copy constructor */
  ConstShapedNeighborhoodIterator( const ConstShapedNeighborhoodIterator & );

  /** Constructor which establishes the region size, neighborhood, and image
   * over which to walk. */
  ConstShapedNeighborhoodIterator(const SizeType &radius,
                       const ImageType * ptr,
                       const RegionType &region
                                  ) : Superclass (radius, const_cast<ImageType*>(ptr),
                                                  region)
  {
    m_ConstBeginIterator = ConstIterator(this);
    m_ConstEndIterator = ConstIterator(this);
    m_ConstEndIterator.GoToEnd();
    m_CenterIsActive = false;
  }
  
  // Expose the following methods from the superclass.  This is a restricted
  // subset of the methods available for ConstNeighborhoodIterator.
  Superclass::GetImagePointer;
  Superclass::GetRadius;
  Superclass::GetIndex;
  Superclass::GetNeighborhoodIndex;
  Superclass::GetRegion;
  Superclass::GetBeginIndex;
  Superclass::GoToBegin;
  Superclass::GoToEnd;
  Superclass::IsAtBegin;
  Superclass::IsAtEnd;
  Superclass::GetOffset;
  Superclass::operator==;
  Superclass::operator!=;
  Superclass::operator<;
  Superclass::operator>;
  Superclass::operator>=;
  Superclass::operator<=;
  Superclass::operator[];
  Superclass::SetLocation;
  Superclass::OverrideBoundaryCondition;
  Superclass::ResetBoundaryCondition;
  Superclass::GetBoundaryCondition;
  Superclass::Print;

  Superclass::operator-;
 
  Superclass::GetPixel;
  
  /** Assignment operator */
  Self &operator=(const Self& orig)
  { return Superclass::operator=(orig); }

  /** Standard itk print method */
  virtual void PrintSelf(std::ostream &, Indent) const;

  /** Add/Remove a neighborhood offset (from the center of the neighborhood) to/from the
      active list.  Active list offsets are the only locations updated and
      accessible through the iterator.  */
  virtual void ActivateOffset(const OffsetType& off)
  { this->ActivateIndex( Superclass::GetNeighborhoodIndex(off) ); }
  virtual void DeactivateOffset(const OffsetType& off)
  { this->DeactivateIndex( Superclass::GetNeighborhoodIndex(off) ); }

  /** Removes all active pixels from this neighborhood. */
  virtual void ClearActiveList()
  {
    m_ActiveIndexList.clear();
    m_ConstBeginIterator.GoToBegin();
    m_ConstEndIterator.GoToEnd();
    m_CenterIsActive = false;
  }
  
  /** Returns the list of active indicies in the neighborhood */
  const IndexListType &GetActiveIndexList() const
  { return m_ActiveIndexList; }

  /** Returns the size of the list of active neighborhood indicies. */
  typename IndexListType::size_type GetActiveIndexListSize() const
  { return m_ActiveIndexList.size(); }

  /** Reimplements the operator++ method so that only active pixel locations
   * are updataed.*/
  Self &operator++();

  /** Reimplements the operator-- method so that only active pixel locations
   * are updataed.*/
  Self &operator--();

 /** Addition of an itk::Offset.  Note that this method does not do any bounds
   * checking.  Adding an offset that moves the iterator out of its assigned
   * region will produce undefined results. */
  Self &operator+=(const OffsetType &);

  /** Subtraction of an itk::Offset. Note that this method does not do any bounds
   * checking.  Subtracting an offset that moves the iterator out of its
   * assigned region will produce undefined results. */
  Self &operator-=(const OffsetType &);

  // Should be protected, but Borland compiler will not allow it.  A workaround
  // must be found.
  Superclass::SetPixel;
protected:
  friend struct ConstIterator;
  
  /** Class is protected here so that it is not publicly accessible, but can be
   * accessed by subclasses.. */
  //  Superclass::SetPixel;
    
  /** Add/Remove a neighborhood index to/from the active.  Locations in the
      active list are the only accessible elements in the neighborhood. The
      argument is an index location calculated as an offset into a linear
      array which represents the image region defined by the radius of this
      iterator, with the smallest dimension as the fastest increasing index. */
  virtual void ActivateIndex(const unsigned int);
  virtual void DeactivateIndex(const unsigned int);

  bool m_CenterIsActive;
  IndexListType m_ActiveIndexList;
  ConstIterator m_ConstEndIterator;
  ConstIterator m_ConstBeginIterator;
};


} // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConstShapedNeighborhoodIterator.txx"
#endif

#endif

