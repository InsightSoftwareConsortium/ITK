Download ITK
============

This page documents how to download ITK. See our
[CONTRIBUTING](../CONTRIBUTING.md) guide for more information.

Download ITK Release Assets
---------------------------

ITK release tarballs, including:

- Source code
- Regression test data
- Doxygen documentation
- ITK's Software Guide
- ITK's Examples

can be downloaded from [ITK's GitHub Releases
page](https://github.com/InsightSoftwareConsortium/ITK/releases).

### Linux Package Distributions

ITK packages exist for many Linux distributions. For Debian and Ubuntu, they
can be obtained running

```sh
   $ sudo apt-get install libinsighttoolkit4-dev
```

This will give you the libraries and development headers.

For information on other ITK packages and packages for other distributions, see
this [blog post].

### Python

ITK Python wheels are available through the Python Package Index ([PyPI]).

To install the ITK Python wheel, use

```sh
   $ python -m pip install --upgrade pip
   $ python -m pip install itk
```

Additionally, wheels for external modules may also be available through the
Python Package Index ([PyPI]).

Check the [ITKPythonPackage] website for further information.

Download ITK Sources with Git
-----------------------------

First, [download and install Git](https://git-scm.com/downloads).

Note that the software must be compiled from source using [CMake] and your
favorite C++ compiler. The toolkit can be installed, but it is not necessary;
it can be used directly from its build directory. See the [ITK Software Guide]
for further information on configuring and building ITK.

### Clone

Clone ITK using the commands

```sh
   $ git clone https://github.com/InsightSoftwareConsortium/ITK
   $ cd ITK
```

The latest stable release can be cloned into a local repository using

```sh
   $ git clone -b release https://github.com/InsightSoftwareConsortium/ITK ITKLatestRelease
```
where `ITKLatestRelease` is the name of the local repository created.

Additionally, specific releases can be cloned using the tags, for example:

```sh
   $ git clone -b v4.13.1 https://github.com/InsightSoftwareConsortium/ITK ITK-4.13.1
```
where `v4.13.1` corresponds to ITK 4.13.1, and the `ITK-4.13.1` is the name
of the local repository created.

### Update

Users that have made no local changes and simply want to update a clone with
the latest changes may run

```sh
   $ git checkout master
   $ git pull --rebase upstream master
```

Avoid making local changes unless you have read our [developer
instructions](../CONTRIBUTING.md).

### Checkout

After cloning, your local repository will be configured to follow the upstream
`master` branch by default. This means you will have access to cutting edge
features, but along with these may come cutting edge bugs :grimacing:. One may
create a local branch to track the upstream `release` branch instead, which
should guarantee only bug fixes to the functionality available in the latest
release:

```sh
   $ git checkout --track -b release origin/release
```

This local branch will always follow the latest `release`. Use the [#update]
instructions to update it.

Alternatively, one may checkout a specific release tag using

```sh
   $ git checkout v4.13.1
```

Release tags never move. Repeat the command with a different tag to get a
different release. One may list available tags using:

```sh
   $ git tag
```


[blog post]: https://blog.kitware.com/itk-packages-in-linux-distributions/
[download page]: https://itk.org/ITK/resources/software.html
[ITKPythonPackage]: https://itkpythonpackage.readthedocs.io/en/latest/index.html
[ITK Software Guide]: https://itk.org/ItkSoftwareGuide.pdf

[CMake]: https://cmake.org/

[Git]: https://git-scm.com
[PyPI]: https://pypi.python.org/pypi
