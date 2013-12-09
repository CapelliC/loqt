loqt
====

Logic Objecs and Qt

By now, just a rendering of Graphviz (via library cgraph,xdot) to Qt Graphic Scene.

Such rendering plays a role in my next project (to be published), showcasing (SWI)Prolog direct control of Qt via reflection.

---------

Some note about components

lqUty
lqXDot
lqXDot_test

==========

+ lqXDot is the actual Graphviz processor.
  /lqXDotScene the main component.
  /lqContextGraph shows a way to encapsulate the Graphviz pointer model in C++.

+ lqXDot_test is a utility UI,
  with basic file/mode maintenance,
  display both lqXDot/SVG rendering,
  and allow editing with syntax coloring.

+ lqUty collects some basic Qt interface helpers.

--------
