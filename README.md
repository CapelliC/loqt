loqt
====

Logic Objecs and Qt

+ pqSource: the main IDE component.
  Integrate the console, editing with semantic highlighting, inline help,
  in an MDI environment, with graphs for inclusions and references analysis.
  The entry point is pqSourceTest, graphs built via pqGraphviz.

+ pqConsole: reusing swipl-console in a component based environment (shared libraries)

+ lqXDot: just a rendering of Graphviz (via library cgraph,xdot) to Qt Graphic Scene.
  Such rendering plays a role in my next project (to be published), showcasing (SWI)Prolog direct control of Qt via reflection.

+ pqGraphviz: a component to bridge Prolog to Graphviz, built upon lqXDot

+ spqr: a basic SWI-Prolog Graphviz interface (WIP).
  Graphviz rendering by Prolog.

+ fdqueens: an example of multithreading GUI interface, with CLP(FD) processing.

==========

lqXDot components:

+ lqXDot is the actual Graphviz processor.
  + lqXDotScene the main component.
  + lqContextGraph shows a way to encapsulate the Graphviz pointer model in C++.

+ lqXDot_test is an utility UI,
  with basic file/mode maintenance,
  display both lqXDot/SVG rendering,
  and allow editing .dot,.gv with syntax coloring.

+ lqUty collects some basic Qt interface helpers.

==========

spqr features:

+ editing Prolog files via CodeMirror (requires QtWebKit)
+ pqConsole control
+ inline plDoc help (requires QtWebKit)
