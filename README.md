# Fortran playground

Something I made over christmas. I want to get used to Fortran, CMake, unit test framework
I use at Deltares. I went through a few days of advent of code, as you can see from the files in the
`src` directory. I was hoping to get further, but unfortunately most of the time was spent on figuring
out how to get the testing framework to work. I'm using this one: https://github.com/loudove/f90tw. Its
Fortran wrappers around `googletest`.

There's `.devcontainer` configuration, which should install the Intel compilers, `CMake`, `googletest`,
some `VSCode` plugins if you're using that and some other things. There's also a `CMakePresets.json`, and
a `.vscode/launch.json` and `.vscode/settings.json`, so you can start building, running tests and debugging
right away.