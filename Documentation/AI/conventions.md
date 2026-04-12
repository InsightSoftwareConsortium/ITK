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

## SWIG `%pythoncode` Brace Forms

When extending a class with embedded Python in a `.i` file, use the
**verbatim** form `%pythoncode %{ ... %}` for any code that contains
`#` comments:

```swig
// BAD — `# Foo` is parsed as a SWIG preprocessor directive named "Foo":
%pythoncode {
    def __array__(self, dtype=None, copy=None):
        # Explicit copy requested.   <-- SWIG error
        return ...
}

// GOOD — verbatim block is passed through unchanged:
%pythoncode %{
    def __array__(self, dtype=None, copy=None):
        # Explicit copy requested.
        return ...
%}
```

The single-brace form `%pythoncode { ... }` runs its body through
SWIG's preprocessor and emits errors like:

```
Error: Unknown SWIG preprocessor directive: Explicit
```

The verbatim `%{ ... %}` form is the safe default for any non-trivial
Python code block. **References:** PR #6027 commit fix.

## Adding a New Class Checklist

When adding a new ITK class to a wrapped module (anything under
`Modules/` that has a `wrapping/` subdirectory), every step below is
required to keep CI green:

1. `include/itkXxx.h` — class declaration, with `\class Xxx` doxygen
2. `include/itkXxx.hxx` — template implementations (header-only)
3. `src/itkXxx.cxx` — non-template implementations + entry in
   `src/CMakeLists.txt`
4. `wrapping/itkXxx.wrap` — `itk_wrap_simple_class("itk::Xxx" POINTER)`
   (or `itk_wrap_class` + `itk_wrap_image_filter` for templates).
   **Missing this file produces `KeyError: 'Xxx'` in Python tests** —
   the C++ build still passes, but `ARMBUILD-Python`,
   `ITK.Linux.Python`, and `ITK.macOS.Python` will fail.
5. For new `ImageIO` classes: add the factory to `FACTORY_NAMES` in
   `itk-module.cmake` *and* register the factory in
   `ImageIOFactory.cxx`.
6. New module dependencies → `DEPENDS` (public) or `PRIVATE_DEPENDS`
   (implementation-only) in `itk-module.cmake`.
7. `test/itkXxxGTest.cxx` — at least one round-trip / identity test,
   plus an exception-validation test.
8. KWStyle: every header has the doxygen `\class Xxx` tag.
9. Run `pre-commit run --files <changed-files>` before committing.

**References:** PR #6032 (VTI factory wrapping fix), PR #6034 (SSIM
filter add).

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
