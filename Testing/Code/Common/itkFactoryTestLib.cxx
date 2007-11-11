/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFactoryTestLib.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkFactoryTestLib.h"
#include "itkImage.h"
#include "itkRGBPixel.h"
#include "itkVersion.h"

static unsigned long itkTotalMemoryUsed = 0;

template<class TElementIdentifier, typename TElement>
class TestImportImageContainer : public itk::ImportImageContainer< TElementIdentifier, TElement >
{
public:
  /** Standard class typedefs.   */
  typedef TestImportImageContainer                             Self;
  typedef itk::ImportImageContainer< TElementIdentifier, TElement > Superclass;
  typedef itk::SmartPointer<Self>               Pointer;
  typedef itk::SmartPointer<const Self>         ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(TestImportImageContainer);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TestImportImageContainer, ImportImageContainer);

  typedef typename Superclass::ElementIdentifier  ElementIdentifier;
  typedef typename Superclass::Element            Element;

  // Methods from itkObject
  virtual ~TestImportImageContainer() 
    {
    itkTotalMemoryUsed -= m_TotalSize;
    m_TotalSize = 0;
    }
  TestImportImageContainer()
    {
    m_TotalSize = 0;
    }
protected:
  TElement* AllocateElements(ElementIdentifier size) const
    { 
    std::cout << "ImportImageContainer: Allocating " << size << " elements of type " << typeid(TElement).name() << " totaling " << sizeof(TElement) * size << " bytes" << std::endl;
    TElement *ptr = Superclass::AllocateElements(size);
    m_TotalSize = size * sizeof(TElement);
    itkTotalMemoryUsed += m_TotalSize;
    std::cout << "ImportImageContainer: Total memory used is "
              << itkTotalMemoryUsed << " bytes" << std::endl;
    return ptr;
    }

private:
  TestImportImageContainer(const TestImportImageContainer&);
  void operator=(const TestImportImageContainer&);
  mutable TElementIdentifier m_TotalSize;
};

class ImportImageContainerFactory : public itk::ObjectFactoryBase
{
public:
  typedef ImportImageContainerFactory   Self;
  typedef itk::ObjectFactoryBase        Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;

  /** Class methods used to interface with the registered factories. */
  virtual const char* GetITKSourceVersion() const { return ITK_SOURCE_VERSION; }
  const char* GetDescription() const { return "A Factory for ImportImageContainer"; }

  /** Method for class instantiation. */
  itkFactorylessNewMacro(Self);
  
  /** Run-time type information (and related methods). */
  itkTypeMacro(ImportImageContainerFactory, itk::ObjectFactoryBase);

  /** Register one factory of this type  */
  static void RegisterOneFactory(void)
  {
    ImportImageContainerFactory::Pointer factory = ImportImageContainerFactory::New();
    itk::ObjectFactoryBase::RegisterFactory(factory);
  }

private:
  ImportImageContainerFactory(const Self&);    //purposely not implemented
  void operator=(const Self&); //purposely not implemented

#define OverrideTypeMacro(t)       this->RegisterOverride(\
        typeid(itk::ImportImageContainer<unsigned long,t >).name(),\
        typeid(TestImportImageContainer<unsigned long,t >).name(),\
        "Test ImportImageContainerOverride",\
        true,\
        itk::CreateObjectFunction<TestImportImageContainer<unsigned long,t > >::New())


  ImportImageContainerFactory()
    {
      OverrideTypeMacro(short);
      OverrideTypeMacro(unsigned char);
      OverrideTypeMacro(float);
      OverrideTypeMacro(int);
      OverrideTypeMacro(double);
      OverrideTypeMacro(itk::RGBPixel<unsigned char>);
      OverrideTypeMacro(itk::RGBPixel<unsigned short>);
    }
};

/**
 * Routine that is called when the shared library is loaded by
 * itk::ObjectFactoryBase::LoadDynamicFactories().
 *
 * itkLoad() is C (not C++) function.
 */
itk::ObjectFactoryBase* itkLoad()
{
    static ImportImageContainerFactory::Pointer f = ImportImageContainerFactory::New();
    return f;
}
