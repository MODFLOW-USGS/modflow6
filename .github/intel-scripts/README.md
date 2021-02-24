These scripts install the ifort compiler on the CI machines.

The content has been directly copied from the Intel OneAPI-CI examples, found
here:

https://github.com/oneapi-src/oneapi-ci

Note that on Linux and macOS, shell scripts must be marked as executable: not
doing so for these scripts will result in a permission error during the GitHub
Action. Windows does not have such a permission system, but you can mark a 
script nonetheless with git:

```
git update-index --chmod=+x {name_of_script}
```

Then commit and push.
