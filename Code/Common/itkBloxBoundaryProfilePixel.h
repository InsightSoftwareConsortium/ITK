#ifndef __itkBloxBoundaryProfilePixel_h
#define __itkBloxBoundaryProfilePixel_h

#include "itkBloxBoundaryProfileItem.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkPoint.h"
#include "itkBloxPixel.h"

namespace itk
{

template <unsigned int NDimensions>
class ITK_EXPORT BloxBoundaryProfilePixel : public BloxPixel< BloxBoundaryProfileItem<NDimensions> >
{
public:
  /** Run-time type information (and related methods). */
  itkTypeMacro( BloxBoundaryProfilePixel, BloxPixel );

  /** The type of boundary profile item we process. */
  typedef BloxBoundaryProfilePixel<NDimensions> BoundaryProfileItemType;

  /** The type of boundary point item we process. */
  typedef BloxBoundaryPointItem<NDimensions> BPItemType;

  /** The type used to store the position of the boundary point item. */
  typedef Point<double, NDimensions> PositionType;
  
  BloxBoundaryProfilePixel();
  virtual ~BloxBoundaryProfilePixel();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryProfilePixel.txx"
#endif

#endif
