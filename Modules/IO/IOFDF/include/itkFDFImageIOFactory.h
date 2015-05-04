/*  Copyright (C) 2004 Glenn Pierce.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Library General Public License for more details.
 *
 *  You should have received a copy of the GNU Library General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 */
#ifndef itkFDFImageIOFactory_h
#define itkFDFImageIOFactory_h
#include "IOFDFExport.h"
#include "itkObjectFactoryBase.h"
#include "itkImageIOBase.h"

namespace itk
{
/** \class FDFImageIOFactory
 *  \ingroup ITKIOFDF
 * \brief Create instances of FDFImageIO objects using an object factory.
 */
class IOFDF_EXPORT FDFImageIOFactory : public ObjectFactoryBase
{
public:
  /** Standard class typedefs. */
  typedef FDFImageIOFactory        Self;
  typedef ObjectFactoryBase        Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char *
  GetITKSourceVersion(void) const ITK_OVERRIDE;
  virtual const char *
  GetDescription(void) const ITK_OVERRIDE;

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(FDFImageIOFactory, ObjectFactoryBase);

  /** Register one factory of this type  */
  static void
  RegisterOneFactory(void)
  {
    FDFImageIOFactory::Pointer FdfFactory = FDFImageIOFactory::New();
    ObjectFactoryBase::RegisterFactory(FdfFactory);
  }

protected:
  FDFImageIOFactory();
  ~FDFImageIOFactory();

private:
  FDFImageIOFactory(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented
};


} // end namespace itk

#endif
