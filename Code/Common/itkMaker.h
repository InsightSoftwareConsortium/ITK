#ifndef __itkMaker_h
#define __itkMaker_h

#include <map>
#include "itkVersion.h"
#include "itkObjectFactoryBase.h"

namespace itk
{

template <class TKey, class TObject>
class Maker : public ObjectFactoryBase {

public:
  /**
   * Standard "Self" typedef.
   */
  typedef Maker   Self;

  /** 
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(Maker, ObjectFactoryBase);

  /**
	 * Default constructor. Not very useful, but needed for ITK
	 */
	Maker();

  /**
	 * Destructor
	 */
	virtual ~Maker();

	/**
	 * Print the registry, one factory per line.
	 */
	void PrintRegistry () const;

	/**
	 * Searches for key in the registry. If the key is found, the corresponding
	 * object factory is used to create an instance of that factory's object
	 * type. This is then returned to the caller
	 */
  static TObject* Create (const TKey& key);

  /**
	 * Get the ITK source version
	 */
	virtual const char* GetITKSourceVersion();

  /**
	 * Get the class's description
	 */
	virtual const char* GetDescription();

protected:
	/**
	 * Useful constructor. Takes in a key and then registers itself with
	 * that key
	 */
  Maker(const TKey& key);

	/**
	 * Manufactures an instance of the factory's object type
	 */
  virtual TObject* MakeObject(const std::string& parameters) const;

private:
	/**
	 * Some typedefs to define the registry
	 */
  typedef Maker<TKey, TObject>* MakerPtr;
  typedef std::map<TKey, MakerPtr> MakerMap;

	/**
	 * The registry itself. Essentially just an STL::map
	 */
  static MakerMap* m_Registry;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMaker.txx"
#endif

#endif // __itkMaker_h
