# Documenting ITK

The ITK code documentation is available online at https://itk.org/Doxygen/html/.

By default, the latest released documentation version is displayed,
but documentation for previous versions is available at by choosing the
appropriate version at the bottom of the above page, e.g.
https://itk.org/Doxygen53/html/index.html.

## Using Doxygen for C++ code

### Dependencies

Generating a complete Doxygen documentation requires (besides CMake and
the ITK source code):

- [Doxygen](https://www.doxygen.nl/)
- [LaTeX](https://www.latex-project.org/) for formulas

Preferable:

- [Dot](https://graphviz.org/doc/info/lang.html) from [GraphViz](https://graphviz.org/)
  for generating inheritance and dependency graphs.

## Generating the Doxygen documentation

Generating the Doxygen documentation for ITK requires:

- The `BUILD_DOCUMENTATION` CMake flag be turned `ON`.
- Build the project as you would normally do: it will build both the
  ITK libraries/binaries and the Doxygen documentation.

Doxygen will then generate the documentation for enabled modules. So if
you want to generate the complete doxygen documentation you would need
to turn `ON` the `ITK_BUILD_ALL_MODULES` CMake flag.

### How to document ITK

Users are referred to [Doxygen's documentation manual](https://www.doxygen.nl/manual/docblocks.html),
and the [command reference list](https://www.doxygen.nl/manual/commands.html)
for detailed instructions.

Below is a simple example of how to document ITK code.

#### Classes

A minimal example to document an ITK class is the following:

```cpp
/**
 * \class MyAwesomeClass
 * \brief Short Description of MyAwesomeClass.
 *
 * Here you can start writing a more detailed documentation for MyAwesomeClass.
 *
 * To document each template parameters use:
 * \tparam T1 documentation for the first type
 * \tparam T2 documentation for the second type
 * \tparam VDimension documentation about the third template parameter which seems to be related to the Dimension
 *
 * You can make implicit references to any other ITK class, by just writing their names, e.g. Image, ImageRegion...
 *
 * You can also set the group(s) the class belongs to:
 * \ingroup MySpecialGroupOrModule1
 * \ingroup MySpecialGroupOrModule2
 *
 * Or you can create make a dedicated section "see also":
 * \sa Image
 * \sa ImageRegion
 */

template< class T1, typename T2, unsigned int VDimension >
class MyAwesomeClass
{
public:
  /** You can directly write the documentation for PixelType */
  typedef PixelType T1;
};
```

The most common Doxygen commands used to document classes in ITK are:

- [`\class`](https://www.doxygen.nl/manual/commands.html#cmdclass)
- [`\brief`](https://www.doxygen.nl/manual/commands.html#cmdbrief)
- [`\tparam`](http://www.stack.nl/~dimitri/doxygen/commands.html#cmdtparam)
- [`\ingroup`](https://www.doxygen.nl/manual/commands.html#cmdingroup)
- [`\sa`](https://www.doxygen.nl/manual/commands.html#cmdsa)

#### Methods and functions

Similar to classes, methods and functions can be documented using
Doxygen commands.

A minimal example for a method documentation is the following:

```cpp
/** \brief A short description of Method1.
 *
 * A more detailed documentation for Method1.
 *
 * \param[in] iP1 Documentation for the first parameter, an input parameter.
 * \param[in] iValue Documentation for the second parameter, an input parameter.
 * \param[out] oValue Documentation for the third parameter, an output parameter.
 * \param[in,out] ioP2 Documentation for the fourth parameter, an input/output parameter.
 * \return Description about the return value.
 */
int Method1(PixelType iP1, unsigned int iValue, unsigned int& oValue, PixelType& ioP2) const
```

The most common Doxygen commands used to document methods and functions
in ITK are:

- [`\param`](https://www.doxygen.nl/manual/commands.html#cmdparam)
- [`\return`](https://www.doxygen.nl/manual/commands.html#cmdreturn)

### Creating links to ITK examples

Links to ITK examples can be created using the special
`\sphinx/\endphinx` Doxygen span pair and the `\sphinxexample` command
defined by ITK.

It is used as, e.g.

```cpp
/** \class Image
 *  \brief Templated n-dimensional image class.
 *
 *  (...)
 *
 * \sphinx
 * \sphinxexample{Core/Common/SetPixelValueInOneImage,Set Pixel Value In One Image}
 * \sphinxexample{Core/Common/DisplayImage,Display Image}
 * \endsphinx
 */
```

where the first argument to `\sphinxexample` points to the relative
path of the example at the [ITK Examples source code](https://github.com/InsightSoftwareConsortium/ITKSphinxExamples/tree/master/src),
and the second one being the title of the example in the
`Documentation.rst` file of the example at issue.

#### Maintaining the documentation

The documentation is built nightly on the latest ITK commit, and deployed to GitHub pages
at https://insightsoftwareconsortium.github.io/ITKDoxygen/, whose code is maintaned at
https://github.com/InsightSoftwareConsortium/ITKDoxygen.
