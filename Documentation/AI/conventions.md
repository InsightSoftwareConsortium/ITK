# ITK C++ Conventions

## Object Creation and Pipeline

```cpp
// Factory method returns SmartPointer — never use new/delete
auto filter = FilterType::New();
filter->SetInput(image);
filter->Update();   // Lazy: nothing runs until here
auto output = filter->GetOutput();

// Parameters changed after Update() require another Update()
filter->SetRadius(3);
filter->Update();
```

## Image Iteration

Use iterators, not raw buffer access:

```cpp
itk::ImageRegionIterator<ImageType> it(image, region);
for (; !it.IsAtEnd(); ++it) {
  it.Set(it.Get() * 2);
}
```

## Class Boilerplate Macros

```cpp
class MyFilter : public BaseFilter {
public:
  using Self       = MyFilter;
  using Superclass = BaseFilter;
  using Pointer    = itk::SmartPointer<Self>;

  itkNewMacro(Self);                          // Provides New()
  itkTypeMacro(MyFilter, BaseFilter);         // RTTI
  itkSetMacro(Radius, unsigned int);          // Generates SetRadius()
  itkGetConstMacro(Radius, unsigned int);     // Generates GetRadius()
  itkBooleanMacro(UseSpacing);                // Generates UseSpacingOn/Off()
```

## Module File Layout

```
ModuleName/
├── itk-module.cmake          # Module metadata and dependency declarations
├── include/
│   ├── itkClassName.h        # Class declaration
│   └── itkClassName.hxx      # Template method implementations (included by .h)
├── src/
│   └── itkClassName.cxx      # Non-template implementations (compiled into lib)
├── test/
│   ├── CMakeLists.txt
│   └── itkClassNameGTest.cxx # Preferred: GoogleTest
└── wrapping/
    └── itkClassName.wrap     # Python wrapping (if applicable)
```

## Module Dependencies

In `itk-module.cmake`:
- `DEPENDS` — public (propagated to consumers)
- `PRIVATE_DEPENDS` — implementation-only
- `TEST_DEPENDS` — test-only

## Third-Party Libraries

`Modules/ThirdParty/` uses `git subtree`, not submodules. Update with:
```bash
git subtree pull --prefix=Modules/ThirdParty/Foo upstream-foo main
```

## Remote Modules

To create an external module:
1. `itk-module.cmake` — declare dependencies
2. `MyModule.remote.cmake` in `Modules/Remote/` — fetch URL + hash
3. `CMakeLists.txt` using `itk_module_impl()`
