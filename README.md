loqt
====

Logic Objecs and Qt

+ spqr
  a basic SWI-Prolog Graphviz interface
  + pqConsole control
  + editing Prolog files via CodeMirror (requires QtWebKit)
  + inline plDoc help (requires QtWebKit)

+ lqXDot
  Just a rendering of Graphviz (via library cgraph,xdot) to Qt Graphic Scene.
  Such rendering plays a role in my next project (to be published), showcasing (SWI)Prolog direct control of Qt via reflection.

---------

Some note about lqXDot components

==========

+ lqXDot is the actual Graphviz processor.
  + lqXDotScene the main component.
  + lqContextGraph shows a way to encapsulate the Graphviz pointer model in C++.

+ lqXDot_test is an utility UI,
  with basic file/mode maintenance,
  display both lqXDot/SVG rendering,
  and allow editing .dot,.gv with syntax coloring.

+ lqUty collects some basic Qt interface helpers.

