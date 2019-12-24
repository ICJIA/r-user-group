# Organizing code with R module

This directory contains examples of different code organization strategies, presented to the User Group on December 11, 2019.

The presentation is based on a simplified data analysis scenario consisting of three spearate steps: data exploration, visualiation and statistical modeling. One main goal of this presentation is to illustrate how [R module](https://github.com/bobaekang/r-module) can help code organization.

Each subdirectory contians R files (not fully implemented) exemplifying a different style of code orgnization:

* `/1-single-file/` contains a single R file that contians the entire program;
* `/2-multi-file-1/` contains three R files, each representing a particular aspect of the same program;
* `/3-multi-file-2/` contains three R files, each slightly modified for better organization; and
* `/4-modular/` contains four R files, one of which is a module, that encapsulates shared code.