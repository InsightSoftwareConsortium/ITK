Download ITK
============

This page documents how to download ITK. See our
[CONTRIBUTING](../CONTRIBUTING.md) guide for more information.

Download ITK Sources with Git
-----------------------------

Follow our [Git download] instructions to install Git.

Note that the software must be compiled from source using [CMake] and your
favorite C++ compiler. The toolkit can be installed, but it is not necessary;
it can be used directly from its build directory. See the [ITK Software Guide]
for further information on configuring and building ITK.

### Clone

Clone ITK using the commands

```sh
   $ git clone git://itk.org/ITK.git
   $ cd ITK
```

The latest stable release can be cloned into a local repositiory using

```sh
   $ git clone -b release https://itk.org/ITK.git ITKLatestRelease
```
where `ITKLatestRelease` is the name of the local repository created.

Additionally, specific releases can be cloned using the tags, for example:

```sh
   $ git clone -b v4.10.0 https://itk.org/ITK.git ITK-4.10.0
```
where `v4.10.0` corresponds to ITK 4.10.0, and the `ITK-4.10.0` is the name
of the local repository created.

### Update

Users that have made no local changes and simply want to update a clone with
the latest changes may run

```sh
   $ git pull
```

Avoid making local changes unless you have read our developer instructions.

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
   $ git checkout v3.20.0
   $ git submodule update
```

Release tags never move. Repeat the command with a different tag to get a
different release. One may list available tags using:

```sh
   $ git tag
```

Download ITK Releases
---------------------

ITK release tarballs can be downloaded from the ITK [download page].

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



[blog post]: https://blog.kitware.com/itk-packages-in-linux-distributions/
[download page]: https://itk.org/ITK/resources/software.html
[ITKPythonPackage]: https://itkpythonpackage.readthedocs.io/en/latest/index.html
[ITK Software Guide]: https://itk.org/ItkSoftwareGuide.pdf

[CMake]: https://cmake.org/

[Git]: http://git-scm.com
[Git download]: https://itk.org/Wiki/Git/Download
[PyPI]: https://pypi.python.org/pypi