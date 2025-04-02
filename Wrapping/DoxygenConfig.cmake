# Configure cmake variables used to autogenerate a doxyfile configuration
# https://cmake.org/cmake/help/v3.16/module/FindDoxygen.html
set(DOXYGEN_PROJECT_NAME "ITK")
set(DOXYGEN_PROJECT_NUMBER "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}.${ITK_VERSION_PATCH}")
set(DOXYGEN_PROJECT_BRIEF "Insight Toolkit")
set(DOXYGEN_PROJECT_LOGO "${ITK_SOURCE_DIR}/Documentation/Art/itkLogoSmall.png")
set(DOXYGEN_OUTPUT_DIRECTORY "${ITK_WRAP_DOC_LIBRARY_DIR}")
set(DOXYGEN_BRIEF_MEMBER_DESC "NO")
set(DOXYGEN_FULL_PATH_NAMES "NO")
set(DOXYGEN_STRIP_FROM_PATH "${ITK_BINARY_DIR}/Utilities/")
set(DOXYGEN_TAB_SIZE "2")
set(DOXYGEN_ALIASES
    "starteraliasnotused=@par not used"
    "wiki=\\par Wiki Examples:<br> ^^ \\li <a href=\"https://www.itk.org/Wiki/ITK/Examples\">All Media Wiki Examples</a> ^^"
    "wikiexample{2}= \\li <a href=\"https://www.itk.org/Wiki/ITK/Examples/\\1\">\\2</a> ^^"
    "endwiki=^^ ^^ ^^"
    "sphinx=\\par ITK Sphinx Examples:<br> ^^ \\li <a href=\"https://itk.org/ITKExamples\">All ITK Sphinx Examples</a> ^^"
    "sphinxexample{2}=\\li <a href=\"https://itk.org/ITKExamples/src/\\1/Documentation.html\">\\2</a> ^^"
    "endsphinx=^^ ^^ ^^")
set(DOXYGEN_TOC_INCLUDE_HEADINGS "0")
set(DOXYGEN_BUILTIN_STL_SUPPORT "YES")
set(DOXYGEN_DISTRIBUTE_GROUP_DOC "YES")
set(DOXYGEN_LOOKUP_CACHE_SIZE "2")
set(DOXYGEN_EXTRACT_ALL "YES")
set(DOXYGEN_EXTRACT_PRIVATE "YES")
set(DOXYGEN_EXTRACT_STATIC "YES")
set(DOXYGEN_SORT_BRIEF_DOCS "YES")
set(DOXYGEN_LAYOUT_FILE "${ITK_SOURCE_DIR}/Documentation/Doxygen/DoxygenLayout.xml")
# -- This value should be sent in as first argument of doxygen_add_docs set(DOXYGEN_INPUT "${ITK_WRAP_DOC_DOXYGEN_HEADERS_FORMATED}")
set(DOXYGEN_FILE_PATTERNS "*.h" "*.dox")
set(DOXYGEN_RECURSIVE "YES")
set(DOXYGEN_EXCLUDE
    "${ITK_SOURCE_DIR}/Modules/ThirdParty/"
    "${ITK_SOURCE_DIR}/Modules/Core/Common/itkPixelTraits.h"
    "${ITK_SOURCE_DIR}/Modules/Core/Common/itkNumericTraits.h"
    "${ITK_SOURCE_DIR}/Modules/Core/Common/itkMathDetail.h"
    "${ITK_SOURCE_DIR}/Modules/Core/IO/itkPixelData.h"
    "${ITK_SOURCE_DIR}/Modules/Core/IO/itkAnalyzeDbh.h"
    "${ITK_SOURCE_DIR}/Modules/Remote/")
set(DOXYGEN_EXCLUDE_PATTERNS
    "*/vxl_copyright.h"
    "*/vcl/*"
    "*/dll.h"
    "*/test*"
    "*/example*"
    "*config*"
    "*/contrib/*"
    "*/Templates/*"
    "*_mocced.cxx")
set(DOXYGEN_EXAMPLE_PATH "${DOXYGEN_TEST_DIRS}")
set(DOXYGEN_EXAMPLE_PATTERNS "*.cxx")
set(DOXYGEN_EXAMPLE_RECURSIVE "YES")
set(DOXYGEN_IMAGE_PATH "${ITK_SOURCE_DIR}/Documentation/Art" "${ITK_BINARY_DIR}/Utilities/Doxygen/Modules")
set(DOXYGEN_INPUT_FILTER "${ITK_DOXYGEN_INPUT_FILTER}")
set(DOXYGEN_FILTER_SOURCE_FILES "YES")
set(DOXYGEN_SOURCE_BROWSER "YES")
set(DOXYGEN_REFERENCED_BY_RELATION "YES")
set(DOXYGEN_REFERENCES_RELATION "YES")
set(DOXYGEN_HTML_HEADER "${ITK_SOURCE_DIR}/Documentation/Doxygen/DoxygenHeader.html")
set(DOXYGEN_HTML_FOOTER "${ITK_SOURCE_DIR}/Documentation/Doxygen/DoxygenFooter.html")
set(DOXYGEN_HTML_EXTRA_STYLESHEET "${ITK_SOURCE_DIR}/Documentation/Doxygen/ITKDoxygenStyle.css")
set(DOXYGEN_HTML_EXTRA_FILES "${ITK_SOURCE_DIR}/Utilities/Doxygen/serviceWorker.js"
                             "${ITK_SOURCE_DIR}/Utilities/Doxygen/workbox-sw.prod.v2.0.1.js")
set(DOXYGEN_HTML_TIMESTAMP "YES")
set(DOXYGEN_HTML_DYNAMIC_SECTIONS "YES")
set(DOXYGEN_DOCSET_FEEDNAME "ITK Doxygen generated documentation")
set(DOXYGEN_DOCSET_BUNDLE_ID "org.itk.ITK")
set(DOXYGEN_DOCSET_PUBLISHER_ID "org.itk.InsightConsortium")
set(DOXYGEN_DOCSET_PUBLISHER_NAME "InsightConsortium")
set(DOXYGEN_ECLIPSE_DOC_ID "org.itk.ITK")
set(DOXYGEN_ENUM_VALUES_PER_LINE "1")
set(DOXYGEN_USE_MATHJAX "YES")
set(DOXYGEN_MATHJAX_RELPATH "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/")
set(DOXYGEN_SERVER_BASED_SEARCH "YES")
set(DOXYGEN_LATEX_CMD_NAME "latex")
set(DOXYGEN_PAPER_TYPE "letter")
set(DOXYGEN_EXTRA_PACKAGES "amsmath" "amsfonts")
set(DOXYGEN_LATEX_BATCHMODE "YES")
set(DOXYGEN_MACRO_EXPANSION "YES")
set(DOXYGEN_EXPAND_ONLY_PREDEF "YES")
set(DOXYGEN_SEARCH_INCLUDES "NO")
set(DOXYGEN_PREDEFINED
    "itkNotUsed(x)="
    "itkStaticConstMacro(name,type,value)=static constexpr type name = value"
    "itkSetInputMacro(name, type, number)=                   virtual void Set##name##Input(const type *_arg);                   virtual void SetInput##number(const type *_arg);"
    "itkGetInputMacro(name, type, number)=                   virtual const type * Get##name##Input() const;                   virtual const type * GetInput##number() const;"
    "itkSetMacro(name,type)=                   virtual void Set##name (type _arg);"
    "itkGetMacro(name,type)=                   virtual type Get##name ();"
    "itkGetConstMacro(name,type)=                   virtual type Get##name () const;"
    "itkSetStringMacro(name)=                   virtual void Set##name (const char* _arg);"
    "itkGetStringMacro(name)=                   virtual const char* Get##name () const;"
    "itkSetClampMacro(name,type,min,max)=                   virtual void Set##name (type _arg);"
    "itkSetObjectMacro(name,type)=                   virtual void Set##name (type* _arg);"
    "itkSetConstObjectMacro(name,type)=                   virtual void Set##name ( const type* _arg);"
    "itkGetConstObjectMacro(name,type)=      virtual const type* Get##name () const;"
    "itkGetModifiableObjectMacro(name,type)= virtual type* GetModifiable##name ();  virtual const type* Get##name () const;"
    "itkGetConstReferenceMacro(name,type)=                   virtual const type& Get##name () const;"
    "itkSetEnumMacro(name,type)=                   virtual void Set##name (const type _arg);"
    "itkGetEnumMacro(name,type)=                   virtual type Get##name () const;"
    "itkGetConstReferenceObjectMacro(name,type)=                   virtual const type::Pointer& Get##name () const;"
    "itkSetDecoratedInputMacro(name, type)=                   virtual void Set##name##Input(const SimpleDataObjectDecorator<type> *_arg);                   virtual void Set##name(const type &_arg);              void Set##name(const SimpleDataObjectDecorator< type > *_arg)"
    "itkGetDecoratedInputMacro(name, type)= virtual const SimpleDataObjectDecorator< type > * Get##name##Input() const; virtual const type & Get##name() const;"
    "itkSetGetDecoratedInputMacro(name, type)=                   virtual void Set##name##Input(const SimpleDataObjectDecorator< type > *_arg);         virtual void Set##name(const type &_arg);          virtual void Set##name(const SimpleDataObjectDecorator< type > *_arg);        virtual const SimpleDataObjectDecorator< type > * Get##name##Input() const;              virtual const type & Get##name() const;"
    "itkSetDecoratedObjectInputMacro(name, type, number)=                   virtual void Set##name##Input(const DataObjectDecorator<type> *_arg);                   virtual void SetInput##number(const DataObjectDecorator<type> *_arg);                   virtual const DataObjectDecorator<type> * Get##name##Input() const;                   virtual const DataObjectDecorator<type> * GetInput##number() const;                   virtual void Set##name(const type *_arg);"
    "itkGetDecoratedObjectInputMacro(name, type)= virtual const DataObjectDecorator< type > * Get##name##Input() const; virtual const type * Get##name() const;"
    "itkSetGetDecoratedObjectInputMacro(name, type)= virtual void Set##name##Input(const DataObjectDecorator<type> *_arg);                   virtual void SetInput##number(const DataObjectDecorator<type> *_arg);                   virtual const DataObjectDecorator<type> * Get##name##Input() const;                   virtual const DataObjectDecorator<type> * GetInput##number() const;                   virtual void Set##name(const type *_arg); virtual const DataObjectDecorator< type > * Get##name##Input() const; virtual const type * Get##name() const;"
    "itkSetDecoratedOutputMacro(name, type)=                   virtual void Set##name##Output(const SimpleDataObjectDecorator<type> *_arg);                   virtual void Set##name(const type &_arg);              void Set##name(const SimpleDataObjectDecorator< type > *_arg)"
    "itkGetDecoratedOutputMacro(name, type)= virtual const SimpleDataObjectDecorator< type > * Get##name##Output() const; virtual const type & Get##name() const;"
    "itkBooleanMacro(name)=                   virtual void name##On ();                   virtual void name##Off ();"
    "itkSetVector2Macro(name,type)=                   virtual void Set##name (type _arg1, type _arg2)                   virtual void Set##name (type _arg[2]);"
    "itkGetVector2Macro(name,type)=                   virtual type* Get##name () const;                   virtual void Get##name (type& _arg1, type& _arg2) const;                   virtual void Get##name (type _arg[2]) const;"
    "itkSetVector3Macro(name,type)=                   virtual void Set##name (type _arg1, type _arg2, type _arg3)                   virtual void Set##name (type _arg[3]);"
    "itkGetVector3Macro(name,type)=                   virtual type* Get##name () const;                   virtual void Get##name (type& _arg1, type& _arg2, type& _arg3) const;                   virtual void Get##name (type _arg[3]) const;"
    "itkSetVector4Macro(name,type)=                   virtual void Set##name (type _arg1, type _arg2, type _arg3, type _arg4)                   virtual void Set##name (type _arg[4]);"
    "itkGetVector4Macro(name,type)=                   virtual type* Get##name () const;                   virtual void Get##name (type& _arg1, type& _arg2, type& _arg3, type& _arg4) const;                   virtual void Get##name (type _arg[4]) const;"
    "itkSetVector6Macro(name,type)=                   virtual void Set##name (type _arg1, type _arg2, type _arg3, type _arg4, type _arg5, type _arg6)                   virtual void Set##name (type _arg[6]);"
    "itkGetVector6Macro(name,type)=                   virtual type* Get##name () const;                   virtual void Get##name (type& _arg1, type& _arg2, type& _arg3, type& _arg4, type& _arg5, type& _arg6) const;                   virtual void Get##name (type _arg[6]) const;"
    "itkSetVectorMacro(name,type,count)=                   virtual void Set##name(type data[]);"
    "itkGetVectorMacro(name,type,count)=                   virtual type* Get##name () const;"
    "itkSetInputMacro(name,type)=         virtual void Set##name(const type *input);"
    "itkGetInputMacro(name,type)=         virtual const type * Get##name() const;"
    "itkNewMacro(type)=                   static Pointer New();                                                                           virtual::itk::LightObject::Pointer CreateAnother() const;"
    "itkSimpleNewMacro(type)=             static Pointer New();"
    "itkCreateAnotherMacro(type)=         virtual::itk::LightObject::Pointer CreateAnother() const;"
    "itkFactorylessNewMacro(x)=                   static Pointer New();                   virtual ::itk::LightObject::Pointer CreateAnother() const;"
    "itkTypeMacro(thisClass,superclass)=                   virtual const char *GetNameOfClass() const;"
    "itkEventMacro(thisClass,superclass)=                   class thisClass : public superclass {};"
    "itkDeclareExceptionMacro(newexcp,parentexcp,whatmessage)=         namespace itk {         class newexcp : public parentexcp {        public:       newexcp(const char *file, unsigned int lineNumber) :        parentexcp(file, lineNumber)          { this->SetDescription(whatmessage); }          newexcp(const std::string & file, unsigned int lineNumber) :          parentexcp(file, lineNumber)          { this->SetDescription(whatmessage); }           itkOverrideGetNameOfClassMacro(newexcp, );         };        }"
    "itkConceptMacro(thisName,thisConcept)=                   /* This class requires thisName                       in the form of thisConcept */"
    "std::numeric_limits=                   std::numeric_limits"
    "ITK_TYPENAME=                   typename"
    "itkTemplateFloatingToIntegerMacro(name)=                   template <TReturn, typename TInput> name(TInput x)"
    "FEM_ABSTRACT_CLASS(thisClass,parentClass)=                   public:                                                                   /** Standard  Self typedef.*/                                          typedef thisClass Self;                                                 /** Standard  Superclass typedef. */                                   typedef parentClass Superclass;                                         /** Pointer or SmartPointer to an object. */                            typedef Self* Pointer;                                                  /** Const pointer or SmartPointer to an object. */                      typedef const Self* ConstPointer;                                     private:"
    "FEM_CLASS(thisClass,parentClass)=                   FEM_ABSTRACT_CLASS(thisClass,parentClass)                               public:                                                                   /** Create a new object from the existing one  */                       virtual Baseclass::Pointer Clone() const;                               /** Class ID for FEM object factory */                                  static const int CLID;                                                  /** Virtual function to access the class ID */                          virtual int ClassID() const                                               { return CLID; }                                                      /** Object creation in an itk compatible way */                         static Self::Pointer New()                                                { return new Self(); }                                              private:"
    "ERROR_CHECKING"
    "VCL_USE_NATIVE_STL=1"
    "VCL_USE_NATIVE_COMPLEX=1"
    "VCL_HAS_BOOL=1"
    "VXL_BIG_ENDIAN=1"
    "VXL_LITTLE_ENDIAN=0"
    "VNL_DLL_DATA="
    "size_t=vcl_size_t"
    "ITK_USE_FFTWD"
    "ITK_USE_FFTWF"
    "ITK_USE_CONCEPT_CHECKING"
    "itkMacro_h"
    "ITK_LEGACY_REMOVE"
    "ITK_FUTURE_LEGACY_REMOVE"
    "ITKCommon_EXPORT_EXPLICIT"
    "ITK_OVERRIDE= override"
    "ITK_NULLPTR=  nullptr"
    "ITK_NOEXCEPT= noexcept"
    "ITK_DISALLOW_COPY_AND_MOVE(type)="
    "ITK_DISALLOW_COPY_AND_ASSIGN(type)="
    "ITK_FORCE_EXPORT_MACRO(X)=")
set(DOXYGEN_GENERATE_TAGFILE "${ITK_BINARY_DIR}/Utilities/Doxygen/InsightDoxygen.tag")
set(DOXYGEN_GRAPHICAL_HIERARCHY "NO")
set(DOXYGEN_DOT_IMAGE_FORMAT "svg")
set(DOXYGEN_DOT_GRAPH_MAX_NODES "150")
set(DOXYGEN_DOT_MULTI_TARGETS "YES")
