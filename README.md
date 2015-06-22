Previous Version
================

Previous Version is a simple Haskell tool that allows retrieving several versions of the same module from git.
The preprocessor will analyse the imports from your Haskell files and extract the corresponding versions.

Usage
=====

Add the following line in the header of your Haskell file:

```
{-# OPTIONS_GHC -F -pgmF prevproc #-}
```

You can now include previous versions of your imports:

```
import TypesV0
import TypesV1
```

The preprocessor will automatically extract the versions tagged `V0` and `V1` of the file Types.hs from your git repository.
The extracted files will be placed in dist/build/autogen and renamed to match the imports.


Use case
========

Previous Version is used for "continuous data integration".
See the example folder for an exemple repository doing continuous data integration using previous-version.


 
