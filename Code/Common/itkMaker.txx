#include "itkMaker.h"

namespace itk
{

template <class TKey, class TObject>
Maker<TKey, TObject>::Maker() : ObjectFactoryBase() 
{
}

template <class TKey, class TObject>
Maker<TKey, TObject>::Maker(const TKey& key) : ObjectFactoryBase()
{
	static bool registryInitialized = false;

	if (!registryInitialized) {
		m_Registry = new MakerMap;
		registryInitialized = true;
	}
  m_Registry->insert(std::make_pair(key, this));
	std::cout << "Factory for " << key;
	std::cout << " registered " << std::endl;
}

template <class TKey, class TObject>
Maker<TKey, TObject>::~Maker()
{
}

template <class TKey, class TObject>
void Maker<TKey, TObject>::PrintRegistry() const
{
	MakerMap::iterator i;

	if (m_Registry == NULL)
		std::cout << "Error: No factories registered!!" << std::endl;
	else
	{
		std::cout << "Registry start..." << std::endl;
		for (i = m_Registry->begin(); i != m_Registry->end(); i++)
			std::cout << i->first << " " << i->second << std::endl;
		std::cout << "Registry end." << std::endl;
	}
}

template <class TKey, class TObject>
TObject* Maker<TKey, TObject>::Create (const TKey& key)
{
  MakerMap::iterator maker = m_Registry->find(key);

  if (maker == m_Registry->end()) 
    return NULL;
	else
		return maker->second->MakeObject("");
}

template <class TKey, class TObject>
const char* Maker<TKey, TObject>::GetITKSourceVersion()
{
	return ITK_SOURCE_VERSION;
}

template <class TKey, class TObject>
const char* Maker<TKey, TObject>::GetDescription()
{
	return "Maker - TObject factory with registry";
}

template <class TKey, class TObject>
TObject* Maker<TKey, TObject>::MakeObject(const std::string& parameters) const
{
	std::cerr << "Generic MakeObject shouldn't be called" << std::endl;
	return NULL;
};

} // end namespace itk